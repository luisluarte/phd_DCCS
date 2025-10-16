pacman::p_load(
    tidyverse,
    ggplot2
)
setwd(this.path::here())

# this is the fitted model from the data of Stojic
# note that here no EEG data was recorded
# as such is used just as a starting point
# this model was specified by me in a previous
# econometry course
dat <- read_rds(file = "../data/model_data.rds") %>%
    ungroup() %>%
    filter(
        !is.na(saccade_entropy),
        !is.na(trial_tau_mean)
    ) %>%
    mutate(
        trial_tau_mean = scale(trial_tau_mean),
        saccade_entropy = scale(saccade_entropy),
        trial = scale(trial)
    )
mdl <- lmerTest::lmer(
    data = dat,
    trial_tau_mean ~ saccade_entropy * as.factor(game) +
        trial + (1 | id)
)
summary(mdl)

# extract beta
standarized_beta <- lme4::fixef(mdl)["saccade_entropy"]
standarized_beta

# residual standard deviation
measurement_noise_sd <- summary(mdl)$sigma

# power simulation
param_target_rho <- standarized_beta
param_measurement_noise_sd <- measurement_noise_sd

n_simulations <- 10000
alpha_level <- 0.05
sample_sizes <- seq(10, 50, 1)

power_sim_fun <- function(n_subjects,
                          rho,
                          noise_sd,
                          n_sims,
                          alpha) {
    p_values <- numeric(n_sims)
    for (i in 1:n_sims) {
        mu <- c(0, 0)
        sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
        true_taus <- MASS::mvrnorm(n = n_subjects, mu = mu, Sigma = sigma)

        observed_tau_physical <- true_taus[, 1] + rnorm(n_subjects, mean = 0, sd = noise_sd)
        observed_tau_abstract <- true_taus[, 1] + rnorm(n_subjects, mean = 0, sd = noise_sd)

        test_results <- cor.test(observed_tau_physical, observed_tau_abstract)
        p_values[i] <- test_results$p.value
    }
    power <- sum(p_values < alpha) / n_sims
    return(power)
}

power_results <- tibble(
    n_subjects = sample_sizes,
    power = NA_real_
)

for (i in 1:nrow(power_results)) {
    power_results$power[i] <- power_sim_fun(
        n_subjects = power_results$n_subjects[i],
        rho = param_target_rho,
        noise_sd = param_measurement_noise_sd,
        n_sims = n_simulations,
        alpha = alpha_level
    )
}
power_results


# figures ----
p1 <- power_results %>%
    ggplot(aes(
        n_subjects, power
    )) +
    geom_line() +
    geom_point(
        shape = 21,
        fill = "tan3",
        size = 3
    ) +
    geom_hline(
        yintercept = 0.8,
        linetype = "dashed"
    ) +
    ggpubr::theme_pubr() +
    ylab("Power") +
    xlab("Sample size") +
    scale_y_continuous(
        breaks = seq(0.3, 1, 0.1),
        limits = c(0.3, 1),
        expand = c(0, 0)
    ) +
    scale_x_continuous(
        breaks = seq(10, 50, 10),
        limits = c(10, 50),
        expand = c(0.1, 0.1)
    ) +
    theme(
        text = element_text(size = 30)
    )
p1
