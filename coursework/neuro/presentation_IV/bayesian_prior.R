# R Code for Bayes-Optimal Prior Calculation with Structural Re-Initialization

# Libraries
pacman::p_load(
  tidyverse,
  ggplot2,
  matrixStats 
)

# --- LogSumExp Helper Functions ---
logSumExp2 <- function(log_a, log_b) {
  if (log_a == -Inf) return(log_b)
  if (log_b == -Inf) return(log_a)
  m <- max(log_a, log_b)
  m + log(exp(log_a - m) + exp(log_b - m))
}
logSumExpVector <- function(log_vec) {
  matrixStats::logSumExp(log_vec)
}

# --- 1. Define Core Task Parameters ---
gamma <- 0.8
tau <- 60
min_block_len <- 20
max_block_len <- 100
n_lengths <- max_block_len
n_block_types_3state <- 3 # (-1, 0, 1)
n_block_types_2state <- 2 # (-1, 1)

# Stabilization Parameters
lapse_rate <- 0.05 
epsilon_reentry <- 1e-12 
log_prob_50 <- log(0.5)
log_reentry <- log(epsilon_reentry)
log_one_minus_reentry <- log(1 - epsilon_reentry)
smoothed_gamma <- (1 - lapse_rate) * gamma + lapse_rate * 0.5
smoothed_one_minus_gamma <- (1 - lapse_rate) * (1 - gamma) + lapse_rate * 0.5
log_smoothed_gamma <- log(smoothed_gamma)
log_smoothed_one_minus_gamma <- log(smoothed_one_minus_gamma)

# --- 2. Hazard Rate and Length Transitions (Unchanged) ---
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
  if (denominator <= 0) return(1)
  return(h_n_values[n] / denominator)
}

# --- 3. Length Transition Matrix (log_p_L) ---
# Since this is independent of the number of states (b_t), it's created once.
log_p_L <- matrix(-Inf, nrow = n_lengths, ncol = n_lengths)
for (n in 1:(n_lengths - 1)) {
  H_n <- hazard_rate(n)
  if (1 - H_n > 1e-15) { log_p_L[n, n + 1] <- log(1 - H_n) }
  if (H_n > 1e-15) { log_p_L[n, 1] <- log(H_n) }
}
log_p_L[n_lengths, 1] <- 0 # log(1.0)

# --- 4. Transition Matrices for ALTERNATING (2-STATE) HMM ---

# Block Transitions for Alternating Biased HMM (Left <-> Right)
log_p_B_continues_2s <- matrix(-Inf, nrow = n_block_types_2state, ncol = n_block_types_2state)
log_p_B_continues_2s[1, 1] <- 0 # Left stays Left
log_p_B_continues_2s[2, 2] <- 0 # Right stays Right

log_p_B_switches_2s <- matrix(-Inf, nrow = n_block_types_2state, ncol = n_block_types_2state)
# Row 1 (Left): flips to Right (2)
log_p_B_switches_2s[1, 2] <- log_one_minus_reentry
log_p_B_switches_2s[1, 1] <- log_reentry # Stabilization leak

# Row 2 (Right): flips to Left (1)
log_p_B_switches_2s[2, 1] <- log_one_minus_reentry
log_p_B_switches_2s[2, 2] <- log_reentry # Stabilization leak

# Emission Probabilities for Alternating Biased HMM
# Rows: 1(b=-1 Left), 2(b=1 Right). Columns: 1(s=-1 Left), 2(s=1 Right)
log_p_S_2s <- matrix(-Inf, nrow = n_block_types_2state, ncol = 2)
log_p_S_2s[1, 1] <- log_smoothed_gamma      # P(Left | Left Block)
log_p_S_2s[1, 2] <- log_smoothed_one_minus_gamma # P(Right | Left Block)
log_p_S_2s[2, 1] <- log_smoothed_one_minus_gamma # P(Left | Right Block)
log_p_S_2s[2, 2] <- log_smoothed_gamma      # P(Right | Right Block)

# --- 5. Simulation Setup ---
set.seed(42)
stimulus_sequence <- c(rep(1, 100), sample(c(-1, 1), 500, replace = TRUE)) # Total 600 trials
t_trials <- length(stimulus_sequence)
priors <- numeric(t_trials)

# --- PHASE 1: Structural Start (t=1 to t=20) ---
# We use the 3-state HMM logic, running only until the first switch is possible.
t_phase1_end <- min_block_len

# Only the log_p_S for the b=0 block is needed here.
log_p_S_0 <- c(log_prob_50, log_prob_50) # P(s=-1|b=0), P(s=1|b=0)

log_h_t_minus_1 <- matrix(-Inf, nrow = n_lengths, ncol = n_block_types_3state)
log_h_t_minus_1[1, 2] <- 0 # Start: l=1, b=0 (Unbiased)

for (t in 1:t_phase1_end) {
    # Since H(n) is 0, this only propagates: log_g_t[l+1, 0] <- log_h[l, 0]
    l_prev <- t-1
    l_curr <- t
    
    # Prior Calc (is always 0.5 in this phase as no switch is possible)
    priors[t] <- 0.5 
    
    # Simplified Propagation & Update for constrained phase (saves computation)
    log_h_prev <- log_h_t_minus_1[l_prev, 2] # Belief is only here
    
    if (t < t_phase1_end) {
        # Propagation (only continues): log_g_t[t+1, 0] = log_h[t, 0]
        log_h_t_minus_1[l_prev+1, 2] <- log_h_prev # Move belief to next length
    }
    
    # Update (log-likelihood of s_t, only affects normalization later)
    s_t_col_idx <- ifelse(stimulus_sequence[t] == -1, 1, 2)
    log_likelihood <- log_p_S_0[s_t_col_idx]
    
    log_h_t_minus_1[l_curr, 2] <- log_h_t_minus_1[l_curr, 2] + log_likelihood
}

# --- PHASE 2: Alternating HMM (t=21 onwards) ---

# Re-Initialize the belief state to 2-State (Left/Right) HMM
log_h_t_minus_1_2s <- matrix(-Inf, nrow = n_lengths, ncol = n_block_types_2state)

# Belief Re-initialization: Split mass 50/50 between Left and Right, starting at l=1.
# This assumes the structural Unbiased block ended randomly 50/50.
log_reinit_mass <- log(0.5)
log_h_t_minus_1_2s[1, 1] <- log_reinit_mass # l=1, b=-1 (Left)
log_h_t_minus_1_2s[1, 2] <- log_reinit_mass # l=1, b=1 (Right)

for (t in t_phase1_end:t_trials) {
    # Step A: Propagation (Calculate log_g_t from log_h_{t-1})
    log_g_t_2s <- matrix(-Inf, nrow = n_lengths, ncol = n_block_types_2state)

    for (l_prev in 1:n_lengths) {
        for (b_prev_idx in 1:n_block_types_2state) {
            log_belief_prev <- log_h_t_minus_1_2s[l_prev, b_prev_idx]
            if (log_belief_prev > -Inf) {
                # Path 1: Block Continues
                l_curr_continue <- l_prev + 1
                log_prob_continue <- log_p_L[l_prev, l_curr_continue]
                if (l_curr_continue <= n_lengths && log_prob_continue > -Inf) {
                    log_prob_b_trans_continue <- log_p_B_continues_2s[b_prev_idx, b_prev_idx]
                    log_contribution <- log_belief_prev + log_prob_continue + log_prob_b_trans_continue
                    log_g_t_2s[l_curr_continue, b_prev_idx] <- logSumExp2(log_g_t_2s[l_curr_continue, b_prev_idx], log_contribution)
                }

                # Path 2: Block Switches (Flip Rule)
                l_curr_switch <- 1
                log_prob_switch <- log_p_L[l_prev, l_curr_switch]
                if (log_prob_switch > -Inf) {
                    # This finds the column index of the opposite block (1->2 or 2->1)
                    b_curr_idx_flip <- ifelse(b_prev_idx == 1, 2, 1) 
                    
                    # Flip Path Contribution (P(b_t = -b_t-1 | switch))
                    log_prob_flip <- log_p_B_switches_2s[b_prev_idx, b_curr_idx_flip] 
                    log_contribution_flip <- log_belief_prev + log_prob_switch + log_prob_flip
                    log_g_t_2s[l_curr_switch, b_curr_idx_flip] <- logSumExp2(log_g_t_2s[l_curr_switch, b_curr_idx_flip], log_contribution_flip)
                    
                    # Leak Path Contribution (P(b_t = b_t-1 | switch) - the epsilon leak)
                    log_prob_leak <- log_p_B_switches_2s[b_prev_idx, b_prev_idx]
                    log_contribution_leak <- log_belief_prev + log_prob_switch + log_prob_leak
                    log_g_t_2s[l_curr_switch, b_prev_idx] <- logSumExp2(log_g_t_2s[l_curr_switch, b_prev_idx], log_contribution_leak)
                }
            }
        }
    }

    # Step B: Calculate Log Prior (Read-Out)
    log_belief_per_block_2s <- apply(log_g_t_2s, 2, logSumExpVector)
    log_total_g_t_2s <- logSumExpVector(log_belief_per_block_2s)

    if (log_total_g_t_2s > -Inf) {
        # Normalized Log Beliefs (only two): Left and Right
        norm_log_belief_left <- log_belief_per_block_2s[1] - log_total_g_t_2s
        norm_log_belief_right <- log_belief_per_block_2s[2] - log_total_g_t_2s

        # Final Log Prior: Weights are log(smoothed_gamma) and log(1-smoothed_gamma)
        log_priors[t] <- logSumExpVector(c(
            log_smoothed_gamma + norm_log_belief_right,
            log_smoothed_one_minus_gamma + norm_log_belief_left
        ))
    } else {
        log_priors[t] <- log(0.5)
    }

    # Step C: Update (Observation)
    stimulus_t <- stimulus_sequence[t]
    s_t_col_idx <- ifelse(stimulus_t == -1, 1, 2)

    log_h_t_2s <- log_g_t_2s
    log_h_t_2s[, 1] <- log_g_t_2s[, 1] + log_p_S_2s[1, s_t_col_idx]
    log_h_t_2s[, 2] <- log_g_t_2s[, 2] + log_p_S_2s[2, s_t_col_idx]

    # Normalize h_t for the next iteration
    log_total_h_t_2s <- logSumExpVector(log_h_t_2s)
    if(log_total_h_t_2s > -Inf){
        log_h_t_minus_1_2s <- log_h_t_2s - log_total_h_t_2s
    } else {
        log_h_t_minus_1_2s <- log_h_t_2s
    }
    
    # Store result for Phase 2
    priors[t] <- exp(log_priors[t])
}

# --- 8. Output and Plotting ---
print(priors[1:25])
print(priors[99:105])

plot(1:t_trials, priors,
  type = "l", ylim = c(0, 1),
  xlab = "Trial Number",
  ylab = "Bayes-Optimal Prior (pi_t)",
  main = "Calculated Bayes-Optimal Prior Over Time (Re-initialized HMM)"
)
abline(h = 0.5, col = "red", lty = 2)
abline(h = smoothed_gamma, col = "blue", lty = 3) 
abline(h = smoothed_one_minus_gamma, col = "blue", lty = 3)
legend("topright",
  legend = c("Prior", "0.5", "Boundary"),
  col = c("black", "red", "blue"), lty = c(1, 2, 3)
)
