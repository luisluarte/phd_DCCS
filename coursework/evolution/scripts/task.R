# Load necessary libraries
# install.packages(c("MASS", "ggplot2", "gridExtra", "dplyr"))
library(MASS)
library(ggplot2)
library(gridExtra)
library(dplyr)

dat <- tibble(
    x = 1:1000,
    y = cumsum(rnorm(n = 1000, mean = 0, sd = 1))
)

dat %>%
    ggplot(aes(
        x, y
    )) +
    geom_line(linewidth = 3) +
    geom_vline(
        xintercept = c(200, 350), linetype = "dashed",
        linewidth = 2
    ) +
    ylab("Time") +
    xlab("RPE derivative") +
    theme_minimal() +
    theme(
        text = element_text(size = 30)
    )



#' Generate tree positions using a 2D Vector Autoregressive (VAR) model.
#'
#' @param n_points The total number of trees to generate.
#' @param phi The damping coefficient (0 <= phi < 1) for the VAR process.
#' @param noise_sd The standard deviation of the Gaussian noise for the VAR process.
#' @return A data frame with x and y coordinates normalized to the [0, 1] unit square.

generate_var_walk <- function(n_points, phi, noise_sd) {
    Phi <- matrix(c(phi, 0, 0, phi), nrow = 2)
    noise_cov <- diag(noise_sd^2, 2)

    positions <- matrix(0, nrow = n_points, ncol = 2)
    deltas <- matrix(0, nrow = n_points, ncol = 2)

    positions[1, ] <- runif(2)
    deltas[1, ] <- mvrnorm(n = 1, mu = c(0, 0), Sigma = noise_cov)

    for (i in 2:n_points) {
        epsilon <- mvrnorm(n = 1, mu = c(0, 0), Sigma = noise_cov)
        deltas[i, ] <- (Phi %*% deltas[i - 1, ]) + epsilon
        positions[i, ] <- positions[i - 1, ] + deltas[i, ]
    }

    # --- 3. Normalize positions to the [0, 1] unit square ---
    # This min-max scaling ensures all generated environments are on a
    # comparable coordinate system, preserving the walk's geometric structure.
    normalize <- function(v) {
        # Handle the edge case where all values are the same to avoid division by zero.
        if (max(v) == min(v)) {
            return(rep(0.5, length(v)))
        }
        (v - min(v)) / (max(v) - min(v))
    }

    positions[, 1] <- normalize(positions[, 1]) # Normalize x-coordinates
    positions[, 2] <- normalize(positions[, 2]) # Normalize y-coordinates

    return(as.data.frame(positions))
}


#' Assigns a fixed number of rewards based on spatial correlation.
#'
#' This function first generates a continuous latent field representing reward
#' propensity using a Gaussian Markov Random Field model (simulated via Gibbs
#' sampling). It then thresholds this field to assign a fixed number of rewards,
#' decoupling patchiness from reward abundance.
#'
#' @param positions A data frame with V1 (x) and V2 (y) coordinates for trees.
#' @param n_rewards The exact number of rewards to be assigned.
#' @param beta The spatial dependence parameter (controls patchiness). beta=0 means independent rewards.
#' @param lambda_sq The squared length-scale of the Gaussian kernel (controls patch size).
#' @param iterations The number of Gibbs sampling iterations to run for convergence.
#' @return A numeric vector of rewards (0 or 1).

assign_rewards_fixed_n <- function(positions, n_rewards, beta, lambda_sq, iterations = 100) {
    n_points <- nrow(positions)

    # --- 1. Pre-calculate the weight matrix based on distance ---
    dist_sq <- as.matrix(dist(positions))^2
    weights <- exp(-dist_sq / (2 * lambda_sq))
    diag(weights) <- 0

    # --- 2. Generate a latent continuous field via Gibbs Sampling ---
    # Initialize a latent field of normally distributed random values.
    latent_field <- rnorm(n_points)

    # Iteratively smooth the field based on neighbor values.
    # This converges to a Gaussian Markov Random Field.
    for (iter in 1:iterations) {
        for (i in sample(n_points)) {
            # The conditional mean for a point is beta * the weighted sum of its neighbors.
            conditional_mean <- beta * sum(weights[i, -i] * latent_field[-i])
            # Update the point by drawing from a normal distribution centered on this mean.
            latent_field[i] <- rnorm(1, mean = conditional_mean, sd = 1)
        }
    }

    # --- 3. Assign rewards using rank ordering (robust to ties) ---
    # Initialize all rewards to 0.
    rewards <- rep(0, n_points)

    # Get the indices of the n_rewards largest values in the latent field.
    top_indices <- order(latent_field, decreasing = TRUE)[1:n_rewards]

    # Set the rewards for only those top indices to 1.
    rewards[top_indices] <- 1

    return(rewards)
}


#' Plot the generated foraging environment.
#'
#' @param env_data A data frame with columns x, y, and reward.
#' @param title A title for the plot.
#' @return A ggplot object.
plot_environment <- function(env_data, title) {
    # Calculate the actual number of rewards for the subtitle
    actual_rewards <- sum(env_data$reward)
    subtitle <- paste("(", actual_rewards, "rewards )")

    ggplot(env_data, aes(x = V1, y = V2, color = factor(reward))) +
        geom_point(size = 2.5) +
        scale_color_manual(
            values = c("0" = "darkgrey", "1" = "red"),
            labels = c("No Reward", "Reward"),
            name = "Resource"
        ) +
        labs(title = title, subtitle = subtitle) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 10),
            plot.subtitle = element_text(hjust = 0.5, size = 8),
            axis.title = element_blank(),
            axis.text = element_blank(),
            legend.position = "bottom"
        ) +
        coord_fixed()
}

# --- Main script: Generate and visualize foraging environments ---

num_trees <- 200
num_rewards <- 100 # Fix the total number of rewards for all scenarios

positions_A <- generate_var_walk(n_points = num_trees, phi = 0.0, noise_sd = 0.1)
rewards_A <- assign_rewards_fixed_n(positions_A, n_rewards = num_rewards, beta = 100, lambda_sq = 0.01)
rewards_B <- assign_rewards_fixed_n(positions_A, n_rewards = num_rewards, beta = 0.1, lambda_sq = 0.01)
env_A <- positions_A %>%
    mutate(
        reward_A = rewards_A,
        reward_B = rewards_B
    ) %>%
    pivot_longer(cols = c(reward_A, reward_B))

env_A %>%
    ggplot(aes(V1, V2, fill = as.factor(value))) +
    geom_point(shape = 21, size = 3) +
    coord_fixed(ratio = 1) +
    scale_fill_manual(values = c("gray", "darkred")) +
    facet_wrap(~name) +
    theme_void() +
    theme(legend.position = "none") +
    xlab("") +
    ylab("")



generate_prob_walk_ar1 <- function(n_blocks, block_size, initial_prob, phi, noise_sd) {
    # Calculate the total number of trials
    total_trials <- n_blocks * block_size

    # Pre-allocate a vector to store the probabilities for each trial
    probabilities <- numeric(total_trials)

    # --- 1. Initialization ---
    # Transform the initial probability into the unbounded logit space.
    current_logit <- log(initial_prob / (1 - initial_prob))

    # Initialize the first step (delta) by drawing from the noise distribution.
    current_delta <- rnorm(1, mean = 0, sd = noise_sd)

    # Assign the initial probability to the first block of trials.
    probabilities[1:block_size] <- initial_prob

    # --- 2. Block-wise Evolution using AR(1) process for steps ---
    for (b in 2:n_blocks) {
        # The new step is a blend of the old step and new noise.
        # delta(b) = phi * delta(b-1) + epsilon(b)
        current_delta <- phi * current_delta + rnorm(1, mean = 0, sd = noise_sd)

        # The new logit value is the old value plus the new, correlated step.
        # l(b) = l(b-1) + delta(b)
        current_logit <- current_logit + current_delta

        # Transform the logit value back into the [0, 1] probability space.
        current_prob <- 1 / (1 + exp(-current_logit))

        # Assign this constant probability to all trials within the current block.
        start_trial <- (b - 1) * block_size + 1
        end_trial <- b * block_size
        probabilities[start_trial:end_trial] <- current_prob
    }

    return(probabilities)
}


# --- Example Usage: Simulating a Two-Armed Bandit Task ---

# Set the parameters for the simulation
num_trials <- 100
trials_per_block <- 10
num_blocks <- num_trials / trials_per_block

# Generate the probability evolution for Arm 1
prob_arm1 <- generate_prob_walk_ar1(
    n_blocks = num_blocks,
    block_size = trials_per_block,
    initial_prob = 0.5,
    phi = 0.1, # Positive phi creates momentum
    noise_sd = 0.4
)

# Generate the probability evolution for Arm 2 (independently)
prob_arm2 <- generate_prob_walk_ar1(
    n_blocks = num_blocks,
    block_size = trials_per_block,
    initial_prob = 0.5,
    phi = 0.1, # Positive phi creates momentum
    noise_sd = 0.4
)

# --- Visualization ---

# Combine the data into a data frame for plotting
plot_data <- data.frame(
    Trial = 1:num_trials,
    `Arm 1` = prob_arm1,
    `Arm 2` = prob_arm2
) %>%
    pivot_longer(cols = c(`Arm.1`, `Arm.2`), names_to = "Arm", values_to = "Probability")

# Create the plot using ggplot2
ggplot(plot_data, aes(x = Trial, y = Probability, color = Arm)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = c("Arm.1" = "#0072B2", "Arm.2" = "#D55E00")) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none"
    )
