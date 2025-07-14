# lib load ----
pacman::p_load(
    tidyverse,
    ggplot2,
    patchwork,
    ggthemes
)
setwd(this.path::here())

# load data ----
load(file = "../data/behData.RData")

# rl functions ----
## soft-max action selection function ----
soft_max <- function(Q_vector, tau) {
    # we compute the log of probabilities to get a stable response
    # we do not need to log Q because log(exp(x)) = x
    # so note that that I did not apply exp to Q_exp
    # because later on if would log(exp(Q_scaled))
    # 1/tau because Im multiplying and I want to use temperature not inverse temp
    Q_scaled <- (as.vector(Q_vector) * (1 / tau))
    # use the log rule for division
    # and do the log-sum-exp trick to avoid under and overflows
    soft_max_out <- Q_scaled - matrixStats::logSumExp(Q_scaled)
    return(soft_max_out)
}

## likelihood function ----
likelihood_function <- function(theta, # theta[1] learning rate, theta[2] temperature
                                actions,
                                rewards,
                                ID, game) {
    # parameters
    alpha <- theta[1]
    tau <- theta[2]
    # this vector will store the log-probabilities
    # this is a vector of all 0's
    logp_actions_t <- numeric(length(actions))

    # initialize random Q table
    random_init <- TRUE
    if (random_init == TRUE) {
        Q <- c(
            round(runif(1, min = 3, max = 6), 1),
            round(runif(1, min = 3, max = 6), 1),
            round(runif(1, min = 3, max = 6), 1),
            round(runif(1, min = 3, max = 6), 1),
            round(runif(1, min = 3, max = 6), 1),
            round(runif(1, min = 3, max = 6), 1)
        )
    }

    # do simulations
    for (i in 1:length(actions)) {
        # apply softmax but in log form
        soft_max_out <- soft_max(Q, tau)

        # total of 6 options
        r <- rewards[i]
        a <- actions[i]
        # store log probability of each action
        logp_actions_t[i] <- soft_max_out[a]

        # update Q values
        Q[a] <- Q[a] + (alpha * (r - Q[a]))
    }
    return(-sum(logp_actions_t[-1]))
}

## parameter recovery ----
### data generation ----
data_gen <- function(alpha, tau, n_trials = 100) {
    reward_mean <- c(
        5.55,
        3.93,
        3.37,
        4.73,
        5.09,
        3.40
    )
    reward_sd <- c(
        1.72,
        1.3,
        1.16,
        2.27,
        1.39,
        1.35
    )
    actions <- numeric(length(n_trials))
    rewards <- numeric(length(n_trials))
    Q_s <- tibble(
        trial = numeric(),
        action = numeric(),
        reward = numeric(),
        q1 = numeric(),
        q2 = numeric(),
        q3 = numeric(),
        q4 = numeric(),
        q5 = numeric(),
        q6 = numeric()
    )
    logp_actions_t <- numeric()
    Q <- c(
        round(runif(1, min = 3, max = 6), 1),
        round(runif(1, min = 3, max = 6), 1),
        round(runif(1, min = 3, max = 6), 1),
        round(runif(1, min = 3, max = 6), 1),
        round(runif(1, min = 3, max = 6), 1),
        round(runif(1, min = 3, max = 6), 1)
    )
    # do simulations
    for (i in 1:n_trials) {
        # apply softmax but in log form
        soft_max_out <- soft_max(Q, tau)
        soft_max_prob <- exp(soft_max_out)

        # total of 6 options
        a <- sample(1:6, prob = soft_max_prob, replace = TRUE, size = 1)
        r <- rnorm(n = 1, mean = reward_mean[a], sd = reward_sd[a])
        # store log probability of each action
        logp_actions_t[i] <- soft_max_out[a]

        # update Q values
        Q[a] <- Q[a] + (alpha * (r - Q[a]))
        Q_s[i, ] <- list(
            trial = i,
            action = a,
            reward = r,
            q1 = Q[1],
            q2 = Q[2],
            q3 = Q[3],
            q4 = Q[4],
            q5 = Q[5],
            q6 = Q[6]
        )
    }
    return(Q_s)
}

# test the procedure
dat_sim <- data_gen(0.05, 1.5, n_trials = 100)
lower_bounds <- c(alpha = 0.001, tau = 0.001) # Use small positive values
upper_bounds <- c(alpha = 1, tau = 5) # Reasonable upper bounds

start_par <- c(
    runif(1, min = lower_bounds[1], max = upper_bounds[1]),
    runif(1, min = lower_bounds[2], max = upper_bounds[2])
)
param_optim <- optim(
    par = start_par,
    fn = likelihood_function,
    method = "L-BFGS-B",
    lower = lower_bounds,
    upper = upper_bounds,
    actions = dat_sim$action,
    rewards = dat_sim$reward,
    ID = 1
)
optim_res <- param_optim$par
optim_res


# data ----

choice_data <- behData %>%
    as_tibble()
rm(behData)
choice_data

metadata <- choice_data %>%
    group_by(subjectID, game) %>%
    summarise(
        age = unique(age),
        gender = unique(gender),
        glasses = unique(glasses),
        mean_rt = mean(choiceRT, na.rm = TRUE),
        max_reward = max(rewardTotal)
    )
metadata

write_rds(x = metadata, file = "../data/metadata.rds")

choice_data_filtered <- choice_data %>%
    select(
        subjectID,
        age,
        gender,
        glasses,
        game,
        gameType,
        block,
        trial,
        choiceMade,
        chosenArm,
        reward,
        rewardScaled
    ) %>%
    filter(
        choiceMade == 1
    ) %>%
    rename(
        action = chosenArm
    )
choice_data_filtered

# parameter extraction ----
set.seed(420)
subjects_parameters <- choice_data_filtered %>%
    ungroup() %>%
    group_by(
        subjectID,
        game
    ) %>%
    group_split() %>%
    imap_dfr(
        ., function(subject_data, idx) {
            1:100 %>%
                map_dfr(
                    ., function(iteration) {
                        print(paste(
                            "analysis unit: ", idx, "/28",
                            "iteration number: ", iteration
                        ))
                        lower_bounds <- c(alpha = 0.001, tau = 0.001) # Use small positive values
                        upper_bounds <- c(alpha = 1, tau = 10) # Reasonable upper bounds
                        start_par <- c(
                            runif(1, min = lower_bounds[1], max = upper_bounds[1]),
                            runif(1, min = lower_bounds[2], max = upper_bounds[2])
                        )
                        param_optim <- optim(
                            par = start_par,
                            fn = likelihood_function,
                            method = "L-BFGS-B",
                            lower = lower_bounds,
                            upper = upper_bounds,
                            actions = subject_data$action,
                            rewards = subject_data$reward,
                            ID = unique(subject_data$subjectID),
                            game = unique(subject_data$game)
                        )
                        optim_res <- param_optim$par
                        return(tibble(
                            opt_alpha = optim_res[1],
                            opt_tau = optim_res[2],
                            likelihood = param_optim$value,
                            iteration = iteration,
                            id = unique(subject_data$subjectID),
                            game = unique(subject_data$game)
                        ))
                    }
                )
        }
    )
subjects_parameters


# optimal parameters ----
optimal_parameters <- subjects_parameters %>%
    pivot_longer(cols = c(opt_alpha, opt_tau)) %>%
    group_by(id, game, name) %>%
    slice_min(., n = 1, order_by = likelihood) %>%
    ungroup() %>%
    pivot_wider(
        names_from = name,
        values_from = value
    ) %>%
    mutate(
        scaled_alpha = scales::rescale(opt_alpha, to = c(0, 1)),
        scaled_tau = scales::rescale(opt_tau, to = c(0, 1))
    )
optimal_parameters
write_rds(x = optimal_parameters, file = "../data/optimal_params.rds")

optimal_parameters %>%
    pivot_longer(
        cols = c(scaled_alpha, scaled_tau)
    ) %>%
    ggplot(aes(
        name, value
    )) +
    geom_boxplot(outlier.shape = NA) +
    geom_point() +
    theme_par() +
    facet_wrap(~game)

# q values extraction -----
data_gen_deterministic <- function(alpha, tau, choice_data) {
    Q_s <- tibble(
        trial = numeric(),
        action = numeric(),
        reward = numeric(),
        q1 = numeric(),
        q2 = numeric(),
        q3 = numeric(),
        q4 = numeric(),
        q5 = numeric(),
        q6 = numeric()
    )
    # I assume subjects were told what a good and a bad reward was
    Q <- c(
        round(runif(1, min = 3, max = 6), 1),
        round(runif(1, min = 3, max = 6), 1),
        round(runif(1, min = 3, max = 6), 1),
        round(runif(1, min = 3, max = 6), 1),
        round(runif(1, min = 3, max = 6), 1),
        round(runif(1, min = 3, max = 6), 1)
    )
    # do simulations
    for (i in 1:length(unique(choice_data$trial))) {
        # total of 6 options
        a <- choice_data$action[i]
        r <- choice_data$reward[i]

        # update Q values
        Q[a] <- Q[a] + (alpha * (r - Q[a]))
        Q_s[i, ] <- list(
            trial = i,
            action = a,
            reward = r,
            q1 = Q[1],
            q2 = Q[2],
            q3 = Q[3],
            q4 = Q[4],
            q5 = Q[5],
            q6 = Q[6]
        )
    }
    return(Q_s)
}

# get task option values and sd

q_val_est <- choice_data_filtered %>%
    rename(id = subjectID) %>%
    left_join(
        optimal_parameters,
        by = c("id", "game")
    ) %>%
    ungroup() %>%
    group_by(
        id,
        game
    ) %>%
    group_split() %>%
    imap_dfr(
        ., function(subject_data, idx) {
            1:500 %>%
                map_dfr(
                    ., function(iteration) {
                        out <- data_gen_deterministic(
                            alpha = unique(subject_data$opt_alpha),
                            tau = unique(subject_data$opt_tau),
                            choice_data = subject_data
                        )
                        return(out %>%
                            mutate(
                                id = unique(subject_data$id),
                                game = unique(subject_data$game),
                                alpha = unique(subject_data$opt_alpha),
                                tau = unique(subject_data$opt_tau),
                                iteration = iteration
                            ))
                    }
                )
        }
    )
q_val_est

write_rds(x = q_val_est, file = "../data/q_val_est.rds")
