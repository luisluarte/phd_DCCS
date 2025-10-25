# R Code for Bayes-Optimal Prior Calculation (Log Space, Smoothed, Lapsed, Clamped)

# Libraries
pacman::p_load(
  tidyverse,
  ggplot2,
  matrixStats
)

# --- LogSumExp Helper Functions (With Clamping) ---
max_log_diff <- 700 # Threshold for log difference

logSumExp2 <- function(log_a, log_b) {
  if (log_a == -Inf && log_b == -Inf) return(-Inf)
  if (log_a == -Inf) return(log_b)
  if (log_b == -Inf) return(log_a)
  m <- max(log_a, log_b)
  diff <- abs(log_a - log_b)
  if (diff > max_log_diff) {
    return(m) # Ignore smaller term if difference is too large
  } else {
    return(m + log(exp(min(log_a, log_b) - m) + 1)) # Stable calculation
  }
}
logSumExpVector <- function(log_vec) {
  finite_log_vec <- log_vec[is.finite(log_vec)]
  if (length(finite_log_vec) == 0) return(-Inf)
  matrixStats::logSumExp(finite_log_vec)
}

# --- 1. Define Core Task Parameters ---
gamma <- 0.8
tau <- 60
min_block_len <- 20
max_block_len <- 100
n_lengths <- max_block_len
n_block_types <- 3 # 1(-1 Left), 2(0 Unbiased), 3(1 Right)

# --- 2. Define Stabilization Parameters ---
epsilon_leak <- 1e-5
log_leak <- log(epsilon_leak)
log_one_minus_2_leak <- log(1 - 2 * epsilon_leak)
lapse_rate <- 0.5
log_prob_50 <- log(0.5)
smoothed_gamma <- (1 - lapse_rate) * gamma + lapse_rate * 0.5
smoothed_one_minus_gamma <- (1 - lapse_rate) * (1 - gamma) + lapse_rate * 0.5
log_smoothed_gamma <- log(smoothed_gamma)
log_smoothed_one_minus_gamma <- log(smoothed_one_minus_gamma)

# --- 3. Precise Hazard Rate Calculation ---
h_n_values <- numeric(n_lengths)
valid_indices <- min_block_len:max_block_len
h_n_values[valid_indices] <- exp(-valid_indices / tau)
cumulative_h_sum <- numeric(n_lengths + 1)
for (n in n_lengths:min_block_len) {
  cumulative_h_sum[n] <- cumulative_h_sum[n + 1] + h_n_values[n]
}
hazard_rate <- function(n, min_len = min_block_len, max_len = max_block_len) {
  if (n < min_len) return(0)
  if (n >= max_len) return(1)
  denominator <- cumulative_h_sum[n]
  if (denominator <= 1e-15) return(1)
  return(min(1.0, h_n_values[n] / denominator))
}

# --- 4. Log Transition Probability Matrices ---
log_p_L <- matrix(-Inf, nrow = n_lengths, ncol = n_lengths)
for (n in 1:(n_lengths - 1)) {
  H_n <- hazard_rate(n)
  prob_continue <- 1 - H_n
  prob_switch <- H_n
  log_prob_continue <- if (prob_continue > 0) log(prob_continue) else -Inf
  log_prob_switch   <- if (prob_switch > 0)   log(prob_switch)   else -Inf
  if (log_prob_continue > -Inf) { log_p_L[n, n + 1] <- log_prob_continue }
  if (log_prob_switch > -Inf) { log_p_L[n, 1] <- log_prob_switch }
}
log_p_L[n_lengths, 1] <- 0

log_p_B_continues <- matrix(log_leak, nrow = n_block_types, ncol = n_block_types)
diag(log_p_B_continues) <- log_one_minus_2_leak

log_p_B_switches <- matrix(log_leak, nrow = n_block_types, ncol = n_block_types)
prob_unbiased_split <- (1 - epsilon_leak) / 2
log_unbiased_split <- if(prob_unbiased_split > 0) log(prob_unbiased_split) else -Inf
log_p_B_switches[1, 3] <- log_one_minus_2_leak
log_p_B_switches[2, 1] <- log_unbiased_split
log_p_B_switches[2, 3] <- log_unbiased_split
log_p_B_switches[3, 1] <- log_one_minus_2_leak

# --- 5. Log Emission Probabilities (LAPSED) ---
log_p_S <- matrix(-Inf, nrow = n_block_types, ncol = 2)
log_p_S[1, 1] <- log_smoothed_gamma
log_p_S[1, 2] <- log_smoothed_one_minus_gamma
log_p_S[2, 1] <- log_prob_50
log_p_S[2, 2] <- log_prob_50
log_p_S[3, 1] <- log_smoothed_one_minus_gamma
log_p_S[3, 2] <- log_smoothed_gamma

# --- 6. Simulation Setup ---
set.seed(42)
stimulus_sequence <- sample(c(-1, 1), 600, replace = TRUE)
t_trials <- length(stimulus_sequence)
log_g_t <- matrix(-Inf, nrow = n_lengths, ncol = n_block_types)
colnames(log_g_t) <- c("b_-1", "b_0", "b_1")
log_h_t_minus_1 <- log_g_t
log_priors <- numeric(t_trials)
log_g_t[1, 2] <- 0
log_h_t_minus_1 <- log_g_t

# --- 7. The Recursive Loop (Forward Algorithm) ---
for (t in 1:t_trials) {
  # Step A: Propagation
  log_g_t <- matrix(-Inf, nrow = n_lengths, ncol = n_block_types)
  colnames(log_g_t) <- c("b_-1", "b_0", "b_1")
  for (l_prev in 1:n_lengths) {
    for (b_prev_idx in 1:n_block_types) {
      log_belief_prev <- log_h_t_minus_1[l_prev, b_prev_idx]
      if (log_belief_prev > -Inf) {
        # Path 1: Continue
        l_curr_continue <- l_prev + 1
        log_prob_continue <- log_p_L[l_prev, l_curr_continue]
        if (l_curr_continue <= n_lengths && log_prob_continue > -Inf) {
          for (b_curr_idx in 1:n_block_types) {
            log_prob_b_trans_continue <- log_p_B_continues[b_prev_idx, b_curr_idx]
            if (log_prob_b_trans_continue > -Inf) {
              log_contribution <- log_belief_prev + log_prob_continue + log_prob_b_trans_continue
              log_g_t[l_curr_continue, b_curr_idx] <- logSumExp2(log_g_t[l_curr_continue, b_curr_idx], log_contribution)
            }
          }
        }
        # Path 2: Switch
        l_curr_switch <- 1
        log_prob_switch <- log_p_L[l_prev, l_curr_switch]
        if (log_prob_switch > -Inf) {
          for (b_curr_idx in 1:n_block_types) {
            log_prob_b_trans_switch <- log_p_B_switches[b_prev_idx, b_curr_idx]
            if (log_prob_b_trans_switch > -Inf) {
               log_contribution <- log_belief_prev + log_prob_switch + log_prob_b_trans_switch
               log_g_t[l_curr_switch, b_curr_idx] <- logSumExp2(log_g_t[l_curr_switch, b_curr_idx], log_contribution)
            }
          }
        }
      }
    }
  }

  # Step B: Calculate Log Prior
  log_belief_per_block <- apply(log_g_t, 2, logSumExpVector)
  log_total_g_t <- logSumExpVector(log_belief_per_block)
  if (log_total_g_t > -Inf) {
    norm_log_belief_unbiased <- log_belief_per_block[2] - log_total_g_t
    norm_log_belief_right <- log_belief_per_block[3] - log_total_g_t
    norm_log_belief_left <- log_belief_per_block[1] - log_total_g_t
    log_priors[t] <- logSumExpVector(c(log_prob_50 + norm_log_belief_unbiased,
                                       log_smoothed_gamma + norm_log_belief_right,
                                       log_smoothed_one_minus_gamma + norm_log_belief_left))
  } else {
    log_priors[t] <- log(0.5)
    warning(paste("Log belief log_g_t is -Inf at trial", t))
  }

  # Step C: Update
  stimulus_t <- stimulus_sequence[t]
  s_t_col_idx <- ifelse(stimulus_t == -1, 1, 2)
  log_h_t <- log_g_t
  log_h_t[, 1] <- log_g_t[, 1] + log_p_S[1, s_t_col_idx]
  log_h_t[, 2] <- log_g_t[, 2] + log_p_S[2, s_t_col_idx]
  log_h_t[, 3] <- log_g_t[, 3] + log_p_S[3, s_t_col_idx]

  # Normalize h_t for next iteration
  log_total_h_t <- logSumExpVector(log_h_t)
  if(is.finite(log_total_h_t) && log_total_h_t > -Inf){
    log_h_t_minus_1 <- log_h_t - log_total_h_t
  } else {
     warning(paste("Log updated belief log_h_t is -Inf at trial", t))
     log_h_t_minus_1 <- log_h_t
  }
}

# --- 8. Output and Plotting ---
priors <- exp(log_priors)
print(head(priors)); print(tail(priors))
if(any(!is.finite(priors))) { warning("Non-finite prior values detected.") }

plot(1:t_trials, priors, type = "l", ylim = c(0, 1),
  xlab = "Trial Number", ylab = "Bayes-Optimal Prior (pi_t)",
  main = "Calculated Bayes-Optimal Prior (Log Space, Smoothed, Lapsed, Clamped)"
)
abline(h = 0.5, col = "red", lty = 2)
abline(h = smoothed_gamma, col = "blue", lty = 3)
abline(h = smoothed_one_minus_gamma, col = "blue", lty = 3)
legend("topright", bty = "n", legend = c("Prior", "0.5", "Smoothed Boundary"),
       col = c("black", "red", "blue"), lty = c(1, 2, 3))
