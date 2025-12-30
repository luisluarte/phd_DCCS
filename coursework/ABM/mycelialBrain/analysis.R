# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    viridis,
    mgcv
)

setwd(this.path::here())

# load data ---
dat <- read_csv("grid_search_results.csv") %>%
    mutate(
        tick = row_number(),
        date = lubridate::ymd(Date)
    )
dat

# expected return from rational 'buy low/sell high' ----
# Corrected R Strategy Logic
strategy_df <- dat %>%
    group_by(iteration, agent_type) %>%
    mutate(
        # 1. No lead() needed - columns are already aligned in Python
        # 2. Corrected Equity Curve Calculation
        position_raw = sign(predicted_return) * (1 - risk_metric),
        position = ifelse(risk_metric > 0.90, 0, position_raw),
        strategy_log_return = position * log_return, # Use direct log_return
        equity_curve = cumsum(strategy_log_return)
    ) %>%
    summarise(
        # Annualized Mean Log Return
        ann_log_return = mean(strategy_log_return) * 252,

        # Annualized Volatility
        ann_volatility = sd(strategy_log_return) * sqrt(252),

        # Sharpe Ratio (The ratio of the two above)
        sharpe_ratio = ann_log_return / ann_volatility,

        # Optional: Total Geometric Return (The wealth factor)
        total_compounded_return = exp(sum(strategy_log_return)) - 1,

        # parameters
        sigma = sigma[1],
        levy_lr = levy_lr[1],
        decay_rate = decay_rate[1],
        target_success = target_success[1],
        growth_rate = growth_rate[1],
        mu = mu[1]
    )

# hysteresis ----
dat_cont <- dat %>%
    group_by(iteration) %>%
    mutate(
        risk_vel = risk_metric - lag(risk_metric),
        risk_vel = tidyr::replace_na(risk_vel, 0)
    ) %>%
    ungroup()

# 2. Fit a Tensor Product Smooth (te)
# This models the interaction between risk levels and risk velocity
cont_model <- gam(mu ~ te(risk_metric, risk_vel, k = c(10, 10)),
    data = dat_cont
)

# 3. Predict on a Grid for Visualization
grid_cont <- expand.grid(
    risk_metric = seq(0, 1, length.out = 1000),
    risk_vel = seq(min(dat_cont$risk_vel), max(dat_cont$risk_vel), length.out = 50)
)
grid_cont$pred_mu <- predict(cont_model, newdata = grid_cont)

ggplot(grid_cont, aes(x = risk_metric, y = risk_vel, fill = pred_mu)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", name = "Predicted Mu") +
    ggpubr::theme_classic2()

# plots ----

## general performance ----
max_ret <- strategy_df %>%
    group_by(agent_type) %>%
    slice_max(ann_log_return)
max_sharpe <- strategy_df %>%
    group_by(agent_type) %>%
    slice_max(sharpe_ratio)
p1 <- strategy_df %>%
    ggplot(aes(
        agent_type, exp(ann_log_return)
    )) +
    geom_boxplot(outlier.shape = NA, width = 0.5, aes(fill = agent_type)) +
    geom_point(size = 2, shape = 21, position = position_jitter(width = 0.1)) +
    geom_point(
        data = max_ret,
        color = "red",
        size = 3,
        shape = 1,
        stroke = 1.5
    ) +
    ggpubr::theme_classic2() +
    ylab("Annualized returns") +
    xlab("Agent type") +
    scale_fill_viridis_d() +
    theme(legend.position = "none")
p1

p2 <- strategy_df %>%
    ggplot(aes(
        agent_type, sharpe_ratio
    )) +
    geom_boxplot(outlier.shape = NA, width = 0.5, aes(fill = agent_type)) +
    geom_point(size = 2, shape = 21, position = position_jitter(width = 0.1)) +
    geom_point(
        data = max_sharpe,
        color = "red",
        size = 3,
        shape = 1,
        stroke = 1.5
    ) +
    ggpubr::theme_classic2() +
    ylab("Annualized sharpe ratio") +
    xlab("Agent type") +
    scale_fill_viridis_d() +
    theme(legend.position = "none")
p2

## optimal parameters -----
p3 <- strategy_df %>%
    ungroup() %>%
    filter(agent_type == "mycelial-brain") %>%
    ggplot(aes(
        x = target_success, y = levy_lr, z = exp(ann_log_return)
    )) +
    stat_summary_2d(bins = 10) +
    scale_fill_viridis_c() +
    ggpubr::theme_classic2()
p3


# exploration - exploitation ----
p4 <- dat %>%
    filter(agent_type == "mycelial-brain") %>%
    group_by(iteration) %>%
    mutate(
        tick = tick - head(tick, 1)
    ) %>%
    ggplot(aes(
        risk_metric, mu,
        color = tick
    )) +
    geom_point() +
    scale_color_viridis(option = "magma") +
    ggpubr::theme_classic2()
p4

p5 <- dat %>%
    filter(agent_type == "mycelial-brain") %>%
    group_by(iteration) %>%
    mutate(
        tick = tick - head(tick, 1)
    ) %>%
    ggplot(aes(
        risk_metric, mu
    )) +
    stat_bin_2d(aes(
        group = iteration,
        fill = after_stat(density)
    )) +
    scale_fill_viridis_c(transform = "log", option = "magma") +
    ggpubr::theme_classic2()
p5
