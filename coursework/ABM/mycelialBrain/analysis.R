# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    viridis,
    mgcv,
    latex2exp,
    poweRlaw,
    diptest,
    PerformanceAnalytics
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

summary(dat_cont$risk_vel)

# 2. Fit a Tensor Product Smooth (te)
# This models the interaction between risk levels and risk velocity
cont_model <- gam(mu ~ te(risk_metric, risk_vel, k = c(8, 8)),
    data = dat_cont
)
summary(cont_model)

# 3. Predict on a Grid for Visualization
grid_cont <- expand.grid(
    risk_metric = seq(0, 1, length.out = 100),
    risk_vel = seq(min(dat_cont$risk_vel), max(dat_cont$risk_vel), length.out = 100)
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
    filter(
        agent_type == "mycelial-brain"
    ) %>%
    group_by(iteration) %>%
    mutate(
        tick = tick - head(tick, 1)
    ) %>%
    ggplot(aes(
        risk_metric, mu,
        color = tick
    )) +
    geom_point() +
    ylab(TeX(r"(\mu Levy \rightarrow Brownian)")) +
    xlab(TeX(r"(Risk Metric)")) +
    scale_color_viridis(option = "magma") +
    ggpubr::theme_classic2() +
    theme(
        text = element_text(size = 30)
    )
p4

dip_data <- dat %>%
    filter(agent_type == "mycelial-brain")
dip_result <- dip.test(dat %>% filter(agent_type == "mycelial-brain") %>% pull(mu))
print(dip_result)

# Visualizing the density to see the "Dip"
ggplot(dip_data %>% filter(iteration == 1), aes(x = mu)) +
    geom_density(fill = "purple", alpha = 0.4) +
    ggpubr::theme_classic2() +
    xlab(TeX(r"(\mu)")) +
    theme(
        text = element_text(size = 30)
    )

# 1. Compute Biological Commitment (Positioning)
# We use tanh to squash the prediction/risk ratio between -1 and 1
dip_data$commitment <- tanh(dip_data$predicted_return / (dip_data$sigma + 0.001)) # Add epsilon to avoid div by zero

# 2. Compute Periodic Agent Return
# The return at t+1 is the commitment at t multiplied by the market return at t+1
dip_data$agent_return <- dip_data$commitment * dip_data$log_return

# 3. Calculate Cumulative Performance
# Convert to log-space for biomass growth visualization
dip_data$cum_agent_biomass <- cumprod(1 + dip_data$agent_return)
dip_data$cum_market_biomass <- cumprod(1 + dip_data$log_return)

# 4. Statistical Validation: Metabolic Efficiency (eta)
# eta = Mean Return / Volatility of Internal Flux
metabolic_efficiency <- mean(dip_data$agent_return) / sd(dip_data$agent_return)
cat("Metabolic Efficiency (Agent):", metabolic_efficiency)

dip_data$date <- as.Date(dip_data$date)

# 2. Compute Market Returns (Log-Returns represent nutrient density)
# Log-returns are additive, which fits our biomass accumulation logic
dip_data$market_return <- dip_data$log_return

# 3. Compute Agent Returns
# (Based on the commitment logic from the previous step)
dip_data$agent_return <- dip_data$commitment * dip_data$market_return

# 4. Convert to xts object
# We select the return columns and index them by the date
returns_xts <- xts(dip_data[, c("market_return", "agent_return")], order.by = dip_data$date)
returns_xts

# 5. Clean up any NA values that occur at the start of the window
returns_xts <- na.omit(returns_xts)

# Rename for clarity in plots
colnames(returns_xts) <- c("Market", "Mycelial_Agent")

charts.PerformanceSummary(returns_xts,
    main = "Biomass Evolution vs Market Substrate",
    colorset = c("grey", "purple")
)

returns_xts <- xts(dip_data[, c("log_return", "agent_return")], order.by = as.Date(dip_data$date))
table.Drawdowns(returns_xts$agent_return, top = 5)
table.Drawdowns(returns_xts$market_return, top = 5)

# 2. Calculate Cumulative Returns (Biomass)
cum_returns <- cumprod(1 + returns_xts)
cum_df <- data.frame(
    date = index(cum_returns),
    Market = as.numeric(cum_returns[, 1]),
    Agent = as.numeric(cum_returns[, 2])
) %>% tidyr::pivot_longer(-date, names_to = "Strategy", values_to = "Value")

# 3. Create Top Pane: The Equity Curve
p1 <- ggplot(cum_df, aes(x = date, y = Value, color = Strategy)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("Agent" = "#6A1B9A", "Market" = "#9E9E9E")) + # Purple Mycelium vs Grey Market
    theme_minimal() +
    labs(
        title = "Accumulated Biomass: Mycelial Agent vs. S&P 500",
        y = "Cumulative Return", x = ""
    ) +
    theme(legend.position = "top")

# 4. Calculate Drawdown
dd_market <- Drawdowns(returns_xts$market_return)
dd_agent <- Drawdowns(returns_xts$agent_return)
dd_df <- data.frame(
    date = index(dd_market),
    Market = as.numeric(dd_market),
    Agent = as.numeric(dd_agent)
) %>% tidyr::pivot_longer(-date, names_to = "Strategy", values_to = "Drawdown")

# 5. Create Bottom Pane: Drawdown (The "Starvation" Profile)
p2 <- ggplot(dd_df, aes(x = date, y = Drawdown, fill = Strategy)) +
    geom_area(position = "identity", alpha = 0.5) +
    scale_fill_manual(values = c("Agent" = "#CE93D8", "Market" = "#BDBDBD")) +
    theme_minimal() +
    labs(y = "Drawdown (%)", x = "Year") +
    theme(legend.position = "none")



# Load required libraries
library(dplyr)
library(ggplot2)

# 1. Load the substrate data
df <- dip_data

# 2. Construct the Memory Manifold (Lag Transformation)
# In category theory, this is a Delayed Embedding that reveals the
# hidden topology of the return series.
df <- df %>%
    mutate(
        lag1 = lag(log_return, 1),
        lag2 = lag(log_return, 2)
    ) %>%
    # Filter out the initial 'NA' rows where memory is empty
    filter(!is.na(lag1) & !is.na(lag2))

# 3. Isolate the "Phalanx" State
# We filter for mu > 2.5 to see where the agent's turgor pressure
# is balanced and the "vesicle fusion" is most efficient.
phalanx_data <- df %>% filter(mu < 2)

# 4. Generate the "Islands of Intelligence" Heatmap
ggplot(phalanx_data, aes(x = lag1, y = lag2)) +
    # Use stat_density_2d to find the clustering peaks (Attractors)
    stat_density_2d(aes(fill = ..level..), geom = "polygon", bins = 25) +
    scale_fill_viridis_c(option = "magma") +
    theme_minimal() +
    labs(
        title = "Islands of Intelligence: Lag-Space Attractors",
        subtitle = "Topological density of stable exploitation (Phalanx Mode)",
        x = "Lag 1 ($r_{t-1}$)",
        y = "Lag 2 ($r_{t-2}$)",
        fill = "Agent Density"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(size = 12)
    )

# 5. Export for the Presentation
ggsave("lag_space_heatmap.png", width = 10, height = 8)

df$delta_phi <- abs(df$agent_return - df$market_return) / (df$sigma + 0.01)

ggplot(df %>% filter(iteration %in% sample(unique(df$iteration), size = 100, replace = FALSE)), aes(x = delta_phi, y = mu)) +
    geom_point(alpha = 0.2, color = "gray70") +
    geom_smooth(method = "gam", color = "purple") +
    ggpubr::theme_classic2() +
    scale_y_continuous(transform = "log") +
    scale_x_continuous(transform = "log", breaks = 5) +
    ylab(TeX(r"(\mu)")) +
    xlab(TeX(r"(\Delta \phi)")) +
    theme(
        text = element_text(size = 30)
    )
