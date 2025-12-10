# libs ----
pacman::p_load(
    tidyverse,
    furrr,
    tictoc
)

setwd(this.path::here())

# load data ----
bills_party <- read_csv("complete_data.csv")

# information structure ----

# There exists a global state variable
# G(t) in J, which is the party of the president
# at time t (period). Then, the relational agent
# state S_i(t) based on G(t) is:
# officialist if C_j(t) in G(t)
# opposition if C_j(t) not in G(t)
add_S_i_t <- bills_party %>%
    mutate(
        S_i_t = if_else(party == ruling_party,
            "officialist",
            "opposition"
        )
    )
add_S_i_t

# Agent knows the policy space of all proposals, x_p(t)
# and the status quo, x_sq(t)
# for this I make the following assumptions
# 1. x_p(t) is just the ruling party political center.
# 2. x_sq(t) is similar but computed for all the politicians

# get probability yea ----

# this function just implement the computation of the
# probability of yea (voting in favor of proposal)
# for likelihood estimation
get_prob_yea <- function(w, # personal beliefs
                         K, # party loyalty
                         p_i,
                         xp,
                         xsq,
                         role # off/opp
) {
    # ideological payoff
    u_ideo_yea <- -(p_i - xp)^2
    u_ideo_nay <- -(p_i - xsq)^2

    # portfolio payoff
    # init with zeros
    u_port_yea <- numeric(length(p_i))
    u_port_nay <- numeric(length(p_i))

    # officialist logic
    is_off <- (role == "officialist")
    u_port_yea[is_off] <- K
    u_port_nay[is_off] <- -K

    # opposition logic
    u_port_yea[!is_off] <- -K
    u_port_nay[!is_off] <- K

    # total weighted utility
    u_total_yea <- (w * u_ideo_yea) + ((1 - w) * u_port_yea)
    u_total_nay <- (w * u_ideo_nay) + ((1 - w) * u_port_nay)

    # logit probability
    # p(yea) = 1 / (1 + exp(-(U_yea - U_nay)))
    utility_diff <- u_total_yea - u_total_nay
    prob_yea <- 1 / (1 + exp(-utility_diff))

    # with safeties for log(0)
    return(pmax(pmin(prob_yea, 0.999999), 0.000001))
}

# get vote decision ----
# this is the function to get an actual vote prediction
get_vote_decision <- function(w,
                              K,
                              p_i,
                              xp,
                              xsq,
                              role) {
    # compute utilities
    u_ideo_yea <- -(p_i - xp)^2
    u_ideo_nay <- -(p_i - xsq)^2

    if_off <- (role == "officialist")

    u_port_yea <- ifelse(is_off, K, -K)
    u_port_nay <- ifelse(if_off, -K, K)

    u_total_yea <- w * u_ideo_yea + (1 - w) * u_port_yea
    u_total_nay <- w * u_ideo_nay + (1 - w) * u_port_nay

    vote <- ifelse(u_total_yea > u_total_nay, 1, 0)

    return(vote)
}

# likelihood function ----
# here I estimate the model dynamics

# split data by politician and period
# and arrange by bill sequence
# dim ideology is the starting p
# I will sample politician to get faster results
period_data_i <- add_S_i_t %>%
    group_by(period) %>%
    filter(full_name %in% sample(unique(full_name),
        size = min(5, n_distinct(full_name))
    )) %>%
    ungroup() %>%
    group_by(full_name, period) %>%
    arrange(bill_id, .by_group = TRUE) %>%
    group_split()

# estimate party centers per period
party_centers_df <- add_S_i_t %>%
    group_by(period, party) %>%
    reframe(
        party = party[1],
        party_center = party_center[1]
    )
party_centers_df

# function for the force vectors or netwon's force
get_force_scalar <- function(p_i,
                             C_j, # centers vector
                             k, # susceptibility to policy space
                             theta # "mass" of the parties
) {
    centers <- party_centers_df
    # for safety
    if (nrow(centers) == 0) {
        return(0)
    }

    # direction and magnitude
    dist_vec <- C_j - p_i
    abs_dist <- abs(dist_vec)

    # deal with division by zero
    abs_dist_safe <- pmax(abs_dist, 0.001)

    # main logic
    force_j <- (dist_vec * (theta^2)) / (abs_dist_safe^3)

    # prevent extreme optimization solutions
    force_j_safe <- pmax(pmin(force_j, 2.0), -2.0)

    total_force <- force_j_safe

    # apply susceptibility
    return(k * sum(force_j_safe))
}

# dynamics
ll_dynamic_trajectory <- function(params,
                                  agent_data,
                                  centers_vec,
                                  p_initial) {
    w <- params[1]
    K <- params[2]
    alpha_loss <- params[3]
    alpha_win <- params[4]
    k_suscept <- params[5]
    theta <- params[6]

    # # bound checks enforced with penalties
    # if (w < 0 || w > 1 ||
    #     alpha_loss < 0 || alpha_win < 0 ||
    #     k_suscept < 0 ||
    #     theta < 0) {
    #     return(1e10)
    # }

    # init state
    p_current <- p_initial # Fabrega's estimate
    total_neg_ll <- 0

    # make sure data is arrange by bill id
    n_votes <- nrow(agent_data)

    xp_means <- agent_data$xp_mean
    xsq_means <- agent_data$xsq_mean
    roles <- agent_data$S_i_t
    values <- agent_data$value
    outcomes <- agent_data$outcome

    for (t in 1:n_votes) {
        row <- agent_data[t, ]

        # compute prob of observed vote based on curr position
        prob_yea <- get_prob_yea(
            w,
            K,
            p_current,
            xp_means[t],
            xsq_means[t],
            roles[t]
        )

        # add to likelihood
        actual_vote <- row$value
        lik <- if_else(actual_vote == 1, prob_yea, 1 - prob_yea)
        total_neg_ll <- total_neg_ll - log(lik)

        # update position
        did_win <- (values[t] == outcomes[t])
        alpha <- if_else(did_win, alpha_win, alpha_loss)

        x_winner <- if_else(outcomes[t] == 1, xp_means[t], xsq_means[t])
        delta_learn <- alpha * (x_winner - p_current)

        # force field
        delta_force <- get_force_scalar(
            p_current,
            centers_vec,
            k_suscept,
            theta
        )

        # update state
        p_current <- p_current + delta_learn + delta_force

        # bound position within in reason
        p_current <- max(min(p_current, 2.0), -2.0)
    }

    return(total_neg_ll)
}

plan(multisession)
tic()
optim_results <- period_data_i %>%
    future_map(., function(X) {
        dat <- head(X, n = 1000) # set lower number of bills for faster opt, full test is super expensive
        p_init <- dat$dim_ideology[1] # constant by period

        # pre-calc force field
        current_period <- dat$period[1]
        centers_vec <- party_centers_df$party_center[party_centers_df$period == current_period]

        # skip if too few votes
        if (nrow(dat) < 20) {
            return(NULL)
        }

        lower_bounds <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
        upper_bounds <- c(1.0, 10.0, 1.0, 1.0, 10.0, 10.0)

        start_par <- c(
            0.5, # w
            0.5, # K
            0.05, # alpha loss
            0.01, # alpha win
            0.01, # k
            1.0 # theta
        )

        opt <- nlminb(
            start = start_par,
            objective = ll_dynamic_trajectory,
            lower = lower_bounds,
            upper = upper_bounds,
            agent_data = dat,
            centers_vec = centers_vec,
            p_initial = p_init,
            control = list(iter.max = 1000)
        )
        out <- tibble(
            id = dat$DiputadoId[1],
            period = dat$period[1],
            w = opt$par[1],
            K = opt$par[2],
            alpha_loss = opt$par[3],
            alpha_win = opt$par[4],
            k_suscept = opt$par[5],
            theta = opt$par[6],
            log_lik = -opt$objective
        )
        return(out)
    }, .options = furrr_options(seed = TRUE))
toc()

saveRDS(object = optim_results, file = "optim_results.rds")


res_meta <- period_data_i %>%
    map_dfr(., function(X) {
        X %>%
            slice_head(., n = 1) %>%
            select(
                DiputadoId,
                xp_mean,
                xsq_mean,
                S_i_t
            ) %>%
            select(-DiputadoId)
    })
res_meta

res <- bind_rows(optim_results) %>%
    bind_cols(res_meta)
res

# statistical analysis ----

# while its possible that politicians are repeated between periods
# just for simplicity I will assume independence of measurements by period

## hypothesis I: assymetric learning from legislative outcomes ----

# non-parametric, alpha is skewed by construction
h1_mdl <- wilcox.test(res$alpha_loss, res$alpha_win, paired = TRUE, exact = FALSE)
h1_mdl

## hypothesis II: systemic drift ----

# reconstruct agent trajectory

trajectory_retrieval <- function(params_row,
                                 agent_data,
                                 centers_vec,
                                 p_initial) {
    # unpack optimized params
    w <- params_row$w
    K <- params_row$K
    alpha_loss <- params_row$alpha_loss
    alpha_win <- params_row$alpha_win
    k_suscept <- params_row$k_suscept
    theta <- params_row$theta

    # init state
    p_current <- p_initial

    # pre-allocate history vectors
    n_votes <- nrow(agent_data)
    history_p <- numeric(n_votes)
    history_delta_learn <- numeric(n_votes)
    history_delta_force <- numeric(n_votes)
    history_x_winner <- numeric(n_votes)

    # extract vectors
    xp_means <- agent_data$xp_mean
    xsq_means <- agent_data$xsq_mean
    roles <- agent_data$S_i_t
    values <- agent_data$value
    outcomes <- agent_data$outcome

    # replay loop
    for (t in 1:n_votes) {
        # save curr position
        history_p[t] <- p_current

        # learning step
        did_win <- (values[t] == outcomes[t])
        alpha <- if_else(did_win, alpha_win, alpha_loss)
        x_winner <- if_else(outcomes[t] == 1, xp_means[t], xsq_means[t])

        delta_learn <- alpha * (x_winner - p_current)

        # force step
        delta_force <- get_force_scalar(
            p_current,
            centers_vec,
            k_suscept,
            theta
        )

        # get components
        history_delta_learn[t] <- delta_learn
        history_delta_force[t] <- delta_force
        history_x_winner[t] <- x_winner

        # update state
        p_current <- p_current + delta_learn + delta_force
        p_current <- max(min(p_current, 2.0), -2.0)
    }

    tibble(
        bill_seq = 1:n_votes,
        bill_id = agent_data$bill_id,
        p_t = history_p,
        xp_mean = xp_means,
        x_winner = history_x_winner,
        drift_learn = history_delta_learn,
        drift_force = history_delta_force,
        vote_outcome = if_else(values == outcomes, "win", "loss")
    )
}

# reconstruct all p_i's
all_histories <- map2_dfr(
    period_data_i,
    split(res, 1:nrow(res)),
    function(dat, res_row) {
        # get context
        current_period <- dat$period[1]
        p_init <- dat$dim_ideology[1]

        # get centers vector
        centers_vec <- party_centers_df$party_center[party_centers_df$period == current_period]

        # run replay
        traj <- trajectory_retrieval(res_row, dat, centers_vec, p_init)

        # add metadata
        traj %>%
            mutate(
                id = dat$DiputadoId,
                period = current_period,
                full_name = dat$full_name[1],
                party = dat$party[1],
                role = dat$S_i_t[1]
            )
    }
)

# get the drift (slopes)
agent_slopes <- all_histories %>%
    # here we get distance to the ruling party
    mutate(dist_to_rp = abs(p_t - xp_mean)) %>%
    group_by(id, full_name, role, party, period) %>%
    mutate(bill_seq = log(1:length(bill_seq)))
agent_slopes

# test the hypothesis
h2_mdl <- lme4::lmer(
    data = agent_slopes,
    p_t ~ bill_seq * role + period + (1 | id)
)
summary(h2_mdl)
h2_emm <- emmeans::emmeans(
    h2_mdl,
    ~ bill_seq | role + period
) %>% emmeans::test()
h2_emm

h2_emtrend <- emmeans::emtrends(
    h2_mdl,
    specs = ~role, var = "bill_seq"
) %>% emmeans::test()
h2_emtrend

# hypothesis III: agent topologies

w_test <- lm(
    data = res,
    w ~ S_i_t
)
summary(w_test)
w_emm <- emmeans::emmeans(
    w_test,
    pairwise ~ S_i_t
)
w_emm

K_test <- lm(
    data = res,
    K ~ S_i_t
)
summary(K_test)
K_emm <- emmeans::emmeans(
    K_test,
    pairwise ~ S_i_t
)
K_emm

p1 <- res %>%
    ggplot(aes(w, K, color = S_i_t)) +
    geom_hline(yintercept = 1) +
    geom_vline(xintercept = 0.5) +
    geom_point() +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.25)) +
    ggpubr::theme_pubr()
p1
