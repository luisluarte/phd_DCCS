# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    matrixStats,
    patchwork,
    gganimate
)

# hazard rate ----
#' @param n the current block length
#' @param tau the scale parameter of the expo dist
#' @param min_len the minimum block length
#' @param max_len the maximum block length
#'
#' @return the hazard rate h_tau(n) prob between 0 and 1
compute_hazard_rate <- function(n,
                                tau = 60,
                                min_len = 20,
                                max_len = 100) {
    # input validation
    if (!is.numeric(n) || n <= 0 || n != floor(n)) {
        stop("block length 'n' must be a positive integer.")
    }
    # rules
    if (n < min_len) {
        return(0) # cannot end before min length
    } else if (n >= max_len) {
        return(1) # must end at max length
    } else {
        numerator <- exp(-n / tau)
        denominator_range <- n:max_len
        h_values_for_sum <- exp(-denominator_range / tau)
        denominator <- sum(h_values_for_sum)
        # handle division by zero
        if (denominator <= 1e-15) {
            return(1)
        }
    }
    # compute actual hazard rate
    hazard <- numerator / denominator
    return(max(0.0, min(1.0, hazard)))
}

## test hazard function ----
hazard_rate_pd <- tibble(
    trial_length = 1:100,
    p_trial_end = map_vec(1:100, compute_hazard_rate)
)
hazard_rate_pd

p1 <- hazard_rate_pd %>%
    ggplot(aes(
        trial_length, p_trial_end
    )) +
    geom_line(linewidth = 3) +
    theme_classic() +
    theme(
        text = element_text(size = 30),
        plot.margin = margin(1, 1, 1, 1, "cm")
    ) +
    scale_y_continuous(
        breaks = seq(0, 1, 0.25),
        limits = c(0, 1),
        expand = c(0, 0)
    ) +
    scale_x_continuous(
        breaks = seq(0, 100, 10),
        limits = c(0, 100),
        expand = c(0, 0)
    )
p1

# transition matrix ----
#' @param params include:
#'  - n_lengths: maximum block length
#'  - tau, min_block_len, max_block_len: params for hazard rate
#' @param hazard_func the actual function
#' @return a n_length x n_length matrix with log probs of transition
compute_log_length_transition_matrix <- function(params,
                                                 hazard_func = compute_hazard_rate) {
    n_lengths <- params$n_lengths
    log_p_l <- matrix(-Inf, nrow = n_lengths, ncol = n_lengths)
    rownames(log_p_l) <- paste0("from_", 1:n_lengths)
    colnames(log_p_l) <- paste0("to_", 1:n_lengths)

    # compute hazard rate for each transition
    for (n_prev in 1:(n_lengths - 1)) {
        h_n <- compute_hazard_rate(
            n = n_prev,
            tau = params$tau,
            min_len = params$min_block_len,
            max_len = params$max_block_len
        )
        # linear probs
        prob_continue <- 1.0 - h_n
        prob_switch <- h_n
        # go to log space
        log_prob_continue <- if (prob_continue > 1e-15) log(prob_continue) else -Inf
        log_prob_switch <- if (prob_switch > 1e-15) log(prob_switch) else -Inf

        # assign to matrix
        n_next_continue <- n_prev + 1
        # trial continues in same block
        if (log_prob_continue > -Inf) {
            log_p_l[n_prev, n_next_continue] <- log_prob_continue
        }
        # trial goes to next block
        n_next_switch <- 1
        if (log_prob_switch > -Inf) {
            log_p_l[n_prev, n_next_switch] <- log_prob_switch
        }
    }
    # mandatory switch at 100
    log_p_l[n_lengths, 1] <- 0

    return(log_p_l)
}

## test transition matrix ----
transition_matrix_pd <- compute_log_length_transition_matrix(
    list(
        gamma = 0.8,
        tau = 60,
        min_block_len = 20,
        max_block_len = 100,
        n_lengths = 100,
        n_block_types = 3
    ),
    hazard_func = compute_hazard_rate
) %>%
    as_tibble() %>%
    rownames_to_column(var = "from") %>%
    pivot_longer(
        cols = to_1:to_100,
        names_to = "to",
        values_to = "prob"
    ) %>%
    mutate(
        from = as.numeric(from),
        to = as.numeric(
            gsub(pattern = "to_", replacement = "", x = to)
        )
    )
transition_matrix_pd

p2 <- transition_matrix_pd %>%
    ggplot(aes(
        from, to,
        fill = prob
    )) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    coord_fixed() +
    theme_classic() +
    theme(
        text = element_text(size = 30),
        plot.margin = margin(1, 1, 1, 1, "cm")
    ) +
    scale_y_continuous(
        breaks = seq(0, 100, 25),
        expand = c(0, 0)
    ) +
    scale_x_continuous(
        breaks = seq(0, 100, 25),
        expand = c(0, 0)
    )
p2

# block type transitions ----
#' @param params a list with:
#'  - n_block_types
#'  - epsilon_leak: small prob for smoothin
#' @return a list with two matrices:
#'  - log_p_b_continues: log p(b_t | b_{t-1}, continue)
#'  - log_p_b_switches: log p(b_t | b_{t-1}, switch)
compute_log_block_transition_matrices <- function(params) {
    n_types <- params$n_block_types
    # %||% returns a default when value is NULL
    epsilon_leak <- params$epsilon_leak %||% 0
    if (!is.numeric(epsilon_leak) ||
        epsilon_leak < 0 ||
        epsilon_leak >= 1.0 / n_types) {
        if (epsilon_leak != 0) {
            warning("epsilon_leak must be >= 0 and small")
            epsilon_leak <- 0
        }
    }
    log_leak <- if (epsilon_leak > 0) log(epsilon_leak) else -Inf

    log_prob_primary_3_options <- if (epsilon_leak > 0) log(1 - (n_types - 1) * epsilon_leak) else log(1.0) # Used for diagonals if leak applies to others
    log_prob_primary_2_options <- if (epsilon_leak > 0) log(1 - (n_types - 2) * epsilon_leak) else log(1.0) # Used if one option is already -Inf

    # log_p_b continues
    log_p_b_continues <- matrix(log_leak, nrow = n_types, ncol = n_types)
    diag(log_p_b_continues) <- log_prob_primary_3_options

    # log_p_b switches
    log_p_b_switches <- matrix(log_leak, nrow = n_types, ncol = n_types)
    log_p_b_switches[1, 3] <- log_prob_primary_3_options

    # previous was unbiased
    prob_unbiased_split <- (1 - epsilon_leak) / 2
    log_unbiased_split <- if (prob_unbiased_split > 0) log(prob_unbiased_split) else -Inf
    log_p_b_switches[2, 1] <- log_unbiased_split
    log_p_b_switches[2, 3] <- log_unbiased_split

    # previous was right
    log_p_b_switches[3, 1] <- log_prob_primary_3_options

    # add names
    type_names <- c("b_-1", "b_0", "b_1")
    rownames(log_p_b_continues) <- paste0("from_", type_names)
    colnames(log_p_b_continues) <- paste0("to_", type_names)
    rownames(log_p_b_switches) <- paste0("from_", type_names)
    colnames(log_p_b_switches) <- paste0("to_", type_names)

    # return matrices
    return(list(
        log_p_b_continues = log_p_b_continues,
        log_p_b_switches = log_p_b_switches
    ))
}

# helper func
`%||%` <- function(a, b) {
    if (!is.null(a)) a else b
}

## test block type transitions ----
block_type_pd <- compute_log_block_transition_matrices(
    list(
        gamma = 0.8,
        tau = 60,
        min_block_len = 20,
        max_block_len = 100,
        n_lengths = 100,
        n_block_types = 3,
        epsilon_leak = 1e-10
    )
)
block_type_pd

p3 <- block_type_pd[[1]] %>%
    as_tibble() %>%
    set_names(c("LB", "UB", "RB")) %>%
    rownames_to_column(var = "b_t") %>%
    mutate(b_t = c("LB", "UB", "RB")) %>%
    pivot_longer(
        cols = -b_t,
        names_to = "b_t_to",
        values_to = "prob"
    ) %>%
    ggplot(aes(
        b_t, b_t_to,
        fill = prob
    )) +
    geom_tile(
        color = "white",
        lwd = 1.5,
        linetype = 1
    ) +
    coord_fixed() +
    scale_fill_continuous(low = "white", high = "red") +
    theme_classic() +
    theme(
        text = element_text(size = 30)
    )
p3

p4 <- block_type_pd[[2]] %>%
    as_tibble() %>%
    set_names(c("LB", "UB", "RB")) %>%
    rownames_to_column(var = "b_t") %>%
    mutate(b_t = c("LB", "UB", "RB")) %>%
    pivot_longer(
        cols = -b_t,
        names_to = "b_t_to",
        values_to = "prob"
    ) %>%
    ggplot(aes(
        b_t, b_t_to,
        fill = prob
    )) +
    geom_tile(
        color = "white",
        lwd = 1.5,
        linetype = 1
    ) +
    coord_fixed() +
    scale_fill_continuous(low = "white", high = "red") +
    theme_classic() +
    theme(
        text = element_text(size = 30)
    )
p4

(p3 / p4)

# emission matrix ----
#' @param params a list with:
#'  - gamma: base prob of stim in biased side
#'  - n_block_types (3)
#'  - lapse_rate: small probability of random observation
#'
#'  @return a n_block_type x 2 matrix containing log probabilities
#'  log P(s_t | b_t)
compute_log_emission_matrix <- function(params) {
    gamma <- params$gamma
    n_types <- params$n_block_types
    lapse_rate <- params$lapse_rate %||% 0

    # validate lapse_rate
    if (!is.numeric(lapse_rate) ||
        lapse_rate < 0 ||
        lapse_rate >= 0.5) {
        if (lapse_rate != 0) {
            warning("lapse_rate should be >= 0 and < 0.5")
            lapse_rate <- 0
        }
    }
    # compute smoothed probs
    prob_gamma_smoothed <- (1 - lapse_rate) * gamma + lapse_rate * 0.5
    prob_one_minus_gamma_smoothed <- (1 - lapse_rate) * (1 - gamma) + lapse_rate * 0.5
    prob_05_smoothed <- (1 - lapse_rate) * 0.5 + lapse_rate * 0.5

    # convert into log space
    log_prob_gamma_smooth <- if (prob_gamma_smoothed > 0) log(prob_gamma_smoothed) else -Inf
    log_prob_one_minus_gamma_smooth <- if (prob_one_minus_gamma_smoothed > 0) log(prob_one_minus_gamma_smoothed) else -Inf
    log_prob_05 <- log(0.5)

    # create log emission matrix
    log_p_s <- matrix(-Inf, nrow = n_types, ncol = 2)

    # left block
    log_p_s[1, 1] <- log_prob_gamma_smooth
    log_p_s[1, 2] <- log_prob_one_minus_gamma_smooth

    # unbiased block
    log_p_s[2, 1] <- log_prob_05
    log_p_s[2, 2] <- log_prob_05

    # right block
    log_p_s[3, 1] <- log_prob_one_minus_gamma_smooth
    log_p_s[3, 2] <- log_prob_gamma_smooth

    # names
    rownames(log_p_s) <- c("b_-1", "b_0", "b_1")
    colnames(log_p_s) <- c("s_-1", "s_1")

    return(log_p_s)
}

## test emissions ----
emission_pd <- compute_log_emission_matrix(
    list(
        gamma = 0.8,
        tau = 60,
        min_block_len = 20,
        max_block_len = 100,
        n_lengths = 100,
        n_block_types = 3,
        lapse_rate = 0.01
    )
)
emission_pd

p5 <- emission_pd %>%
    as_tibble() %>%
    rownames_to_column() %>%
    mutate(
        rowname = c("LB", "UB", "RB")
    ) %>%
    pivot_longer(cols = -rowname) %>%
    mutate(
        name = c(
            "Left_stim", "Right_stim",
            "Left_stim", "Right_stim",
            "Left_stim", "Right_stim"
        )
    ) %>%
    ggplot(aes(
        rowname, name,
        fill = value
    )) +
    geom_tile(
        color = "white",
        lwd = 1.5,
        linetype = 1
    ) +
    coord_fixed() +
    theme_classic() +
    theme(
        text = element_text(size = 30)
    ) +
    scale_fill_gradient(low = "white", high = "red") +
    xlab("") +
    ylab("")
p5

# propagate belief ----
#' @param log_h_prev normalized log belief matrix from trial t-1
#' @param log_p_l log length transition matrix
#' @param log_p_b_continues log block type transition matrix
#' @param log_p_b_switches log block type transition matrix
#'
#' @return unnormalized log belief matrix log_g_t for the current trial
propagate_belief <- function(log_h_prev,
                             log_p_l,
                             log_p_b_continues,
                             log_p_b_switches,
                             params) {
    n_lengths <- params$n_lengths
    n_types <- params$n_block_types

    log_g_t <- matrix(-Inf, nrow = n_lengths, ncol = n_types)
    colnames(log_g_t) <- colnames(log_h_prev)
    rownames(log_g_t) <- rownames(log_h_prev)

    for (l_prev in 1:n_lengths) {
        for (b_prev_idx in 1:n_types) {
            log_belief_prev <- log_h_prev[l_prev, b_prev_idx]
            if (log_belief_prev > -Inf) {
                # continues
                l_curr_continue <- l_prev + 1
                if (l_curr_continue <= n_lengths) {
                    log_prob_l_continue <- log_p_l[l_prev, l_curr_continue]
                    if (log_prob_l_continue > -Inf) {
                        for (b_curr_idx in 1:n_types) {
                            log_prob_b_continue <- log_p_b_continues[b_prev_idx, b_curr_idx]
                            if (log_prob_b_continue > -Inf) {
                                log_contribution <- log_belief_prev + log_prob_l_continue + log_prob_b_continue
                                log_g_t[l_curr_continue, b_curr_idx] <-
                                    matrixStats::logSumExp(c(log_g_t[l_curr_continue, b_curr_idx], log_contribution))
                            }
                        }
                    }
                }
                # block switches
                l_curr_switch <- 1
                log_prob_l_switch <- log_p_l[l_prev, l_curr_switch]
                if (log_prob_l_switch > -Inf) {
                    for (b_curr_idx in 1:n_types) {
                        log_prob_b_switch <- log_p_b_switches[b_prev_idx, b_curr_idx]
                        if (log_prob_b_switch > -Inf) {
                            log_contribution <- log_belief_prev + log_prob_l_switch + log_prob_b_switch
                            log_g_t[l_curr_switch, b_curr_idx] <-
                                matrixStats::logSumExp(c(log_g_t[l_curr_switch, b_curr_idx], log_contribution))
                        }
                    }
                }
            }
        }
    }
    return(log_g_t)
}

## propagate belief visualization ----
params <- list(
    gamma = 0.8,
    tau = 60,
    min_block_len = 20,
    max_block_len = 100,
    n_lengths = 100,
    n_block_types = 3
)

log_p_l_matrix <- compute_log_length_transition_matrix(params)
block_trans_mats <- compute_log_block_transition_matrices(params)

log_h_initial <- matrix(-Inf, nrow = params$n_lengths, ncol = params$n_block_types)
colnames(log_h_initial) <- c("LB", "UB", "RB")
rownames(log_h_initial) <- paste0("l_", 1:params$n_lengths)
log_h_initial[1, 2] <- 0 # starts in unbiased block

log_g_t2 <- propagate_belief(
    log_h_prev = log_h_initial,
    log_p_l = log_p_l_matrix,
    log_p_b_continues = block_trans_mats$log_p_b_continues,
    log_p_b_switches = block_trans_mats$log_p_b_switches,
    params = params
)
log_g_t2

p6 <- log_g_t2 %>%
    as_tibble() %>%
    rownames_to_column(var = "l_t") %>%
    pivot_longer(cols = -l_t) %>%
    mutate(
        l_t = as.numeric(l_t)
    ) %>%
    filter(l_t <= 10) %>%
    ggplot(aes(
        l_t, name,
        fill = exp(value)
    )) +
    geom_tile(
        color = "white",
        lwd = 1.5,
        linetype = 1
    ) +
    coord_fixed() +
    theme_classic() +
    theme(
        text = element_text(size = 30)
    ) +
    xlab("") +
    ylab("") +
    scale_x_continuous(
        breaks = seq(1, 10, 1)
    )
p6

# log prior ----
logSumExpVector <- function(log_vec) {
    finite_log_vec <- log_vec[is.finite(log_vec)]
    if (length(finite_log_vec) == 0) {
        return(-Inf)
    }
    matrixStats::logSumExp(finite_log_vec)
}

#' @param log_g_t unnormalized predicted log belief matrix for trial t
#' @param log_p_s log emission matrix
#' @param params a list with model parameters
#'
#' @return scalar log prior value for trial t
compute_log_prior <- function(log_g_t, log_p_s, params) {
    n_types <- params$n_block_types

    # aggregate log belief per block type
    log_belief_per_block <- apply(log_g_t, 2, logSumExpVector)

    # compute total log belief for normalization
    log_total_g_t <- logSumExpVector(log_belief_per_block)

    # handle cases where total belief might have collapsed
    if (log_total_g_t == -Inf) {
        warning("total log belief is -Inf during prior calculation")
        return(log(0.5))
    }

    # normalize log beliefs per block type
    norm_log_belief_left <- log_belief_per_block[1] - log_total_g_t
    norm_log_belief_unbiased <- log_belief_per_block[2] - log_total_g_t
    norm_log_belief_right <- log_belief_per_block[3] - log_total_g_t

    # get log emission probs
    log_emission_right_given_left <- log_p_s[1, 2]
    log_emission_right_given_unbiased <- log_p_s[2, 2]
    log_emission_right_given_right <- log_p_s[3, 2]

    # compute final log prior using weighted logsumexp
    log_prior_t <- logSumExpVector(c(
        log_emission_right_given_left + norm_log_belief_left,
        log_emission_right_given_unbiased + norm_log_belief_unbiased,
        log_emission_right_given_right + norm_log_belief_right
    ))

    return(log_prior_t)
}

# muh boy working
compute_log_prior(
    log_g_t = log_g_t2,
    log_p_s = compute_log_emission_matrix(params),
    params = params
) %>%
    {
        exp(.)
    }

# update belief ----
#' @param log_g_t unnormalized predicted log belief matrix for trial t
#' @param stimulus_t observed stimulus on trial t
#' @param log_p_s log emission matrix
#' @param params
#'
#' @return unnormalized updated log belief matrix log_h_t for trial t
update_belief <- function(log_g_t,
                          stimulus_t,
                          log_p_s,
                          params) {
    n_types <- params$n_block_types

    # input validation
    if (!(stimulus_t %in% c(-1, 1))) {
        stop("stimulus_t must be -1 (left) or 1 (right)")
    }
    if (!all(dim(log_g_t) == c(params$n_lengths, params$n_block_types))) {
        stop("dimensions of log_g_t are incorrect")
    }
    if (!all(dim(log_p_s) == c(params$n_block_types, 2))) {
        stop("dimensions of log_p_s are incorrect")
    }

    # determine emission probs for the observed stimulus
    s_t_col_idx <- ifelse(stimulus_t == -1, 1, 2)

    # get the vector of log emission probs x block type
    log_emission_probs_for_s_t <- log_p_s[, s_t_col_idx]

    # update belief
    log_h_t <- log_g_t

    log_h_t[, 1] <- log_g_t[, 1] + log_emission_probs_for_s_t[1]
    log_h_t[, 2] <- log_g_t[, 2] + log_emission_probs_for_s_t[2]
    log_h_t[, 3] <- log_g_t[, 3] + log_emission_probs_for_s_t[3]

    return(log_h_t)
}

normalize_log_belief <- function(log_belief_matrix) {
    log_total_belief <- logSumExpVector(as.vector(log_belief_matrix))
    if (!is.finite(log_total_belief) ||
        log_total_belief == -Inf) {
        warning("total log belief is -Inf during normalization, returning to original matrix")
        return(log_belief_matrix)
    }
    normalized_log_belief_matrix <- log_belief_matrix - log_total_belief
    return(normalized_log_belief_matrix)
}


# simulation loop -----
sim_params <- list(
    gamma = 0.8,
    tau = 60,
    min_block_len = 20,
    max_block_len = 100,
    n_lengths = 100,
    n_block_types = 3,
    epsilon_leak = 1e-12,
    lapse_rate = 0.01
)

# mats
log_p_l_matrix <- compute_log_length_transition_matrix(sim_params)
block_trans_mats <- compute_log_block_transition_matrices(sim_params)
log_p_s_matrix <- compute_log_emission_matrix(sim_params)

# stimulus sequence (all right)
n_sim_trials <- 300
# stimulus_sequence_sim <- rep(1, n_sim_trials)
stimulus_sequence_sim <- c(
    sample(c(1, -1), size = 50, replace = TRUE, prob = c(0.5, 0.5)),
    sample(c(1, -1), size = 100, replace = TRUE, prob = c(0.8, 0.2)),
    sample(c(1, -1), size = 100, replace = TRUE, prob = c(0.2, 0.8)),
    sample(c(1, -1), size = 50, replace = TRUE, prob = c(0.8, 0.2))
)

# inits
log_h_t_minus_1 <- matrix(-Inf, nrow = params$n_lengths, ncol = params$n_block_types)
colnames(log_h_t_minus_1) <- c("LB", "UB", "RB")
rownames(log_h_t_minus_1) <- paste0("l_", 1:params$n_lengths)
log_h_t_minus_1[1, 2] <- 0

priors_sim <- numeric(n_sim_trials)

# capture the belief matrix
belief_mats <- list()

for (t in 1:n_sim_trials) {
    # catch the belief matrix
    belief_mats[[t]] <- log_h_t_minus_1
    # propagate belief
    log_g_t <- propagate_belief(
        log_h_prev = log_h_t_minus_1,
        log_p_l = log_p_l_matrix,
        log_p_b_continues = block_trans_mats$log_p_b_continues,
        log_p_b_switches = block_trans_mats$log_p_b_switches,
        params = sim_params
    )

    # calculate prior
    log_prior_t <- compute_log_prior(
        log_g_t = log_g_t,
        log_p_s = log_p_s_matrix,
        params = sim_params
    )
    priors_sim[t] <- exp(log_prior_t)

    # update belief with stimulus
    stimulus_t <- stimulus_sequence_sim[t]
    log_h_t <- update_belief(
        log_g_t = log_g_t,
        stimulus_t = stimulus_t,
        log_p_s = log_p_s_matrix,
        params = sim_params
    )

    # normalize belief for next iteration
    log_h_t_minus_1 <- normalize_log_belief(log_h_t)

    # print progress
    cat("trial:", t, "prior:", round(priors_sim[t], 3), "\n")
}

# belieft mats proc ----

mats_pd <- belief_mats %>%
    imap_dfr(., function(X, idx) {
        as_tibble(X) %>%
            rownames_to_column(var = "l") %>%
            pivot_longer(cols = -l, names_to = "block") %>%
            mutate(
                l = as.numeric(l),
                block = as.factor(block),
                value = exp(value),
                trial = idx
            )
    })
mats_pd

p7 <- mats_pd %>%
    mutate(
        block_type = c(
            rep("U", 50 * 300),
            rep("R", 100 * 300),
            rep("L", 100 * 300),
            rep("R", 50 * 300)
        ),
        trial_cat = as.factor(paste(trial, block_type, sep = "_"))
    ) %>%
    arrange(trial) %>%
    ggplot(aes(
        block, l,
        fill = value
    )) +
    geom_tile() +
    scale_fill_gradient(
        low = "white",
        high = "red",
        breaks = c(0, 0.5, 1),
        limits = c(0, 1)
    ) +
    theme_minimal() +
    coord_fixed(ratio = 1 / 5) +
    scale_y_continuous(
        breaks = seq(0, 100, 25),
        limits = c(0, 100),
        expand = c(0, 0)
    ) +
    labs(title = "Trial: {closest_state}")
p7

p7_anim <- p7 +
    transition_states(
        states = trial_cat,
        transition_length = 2,
        state_length = 1
    ) +
    ease_aes("linear")
p7_anim
