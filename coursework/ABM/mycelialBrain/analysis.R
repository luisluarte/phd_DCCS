# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    viridis
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
strategy_df <- dat %>%
    group_by(
        iteration, agent_type
    ) %>%
    mutate(
        # A. Define the Position (The Rational Agent)
        #    If Prediction > 0 -> Buy (+1)
        #    If Prediction < 0 -> Sell (-1)
        #    Scale by (1 - Risk): High Risk means size -> 0.
        position_raw = sign(predicted_return) * (1 - risk_metric),

        # B. Hard Risk Cutoff (Optional but Recommended)
        #    If risk is > 90%, the agent is effectively blind. Force Cash (0).
        position = ifelse(risk_metric > 0.90, 0, position_raw),

        # C. Align Future Returns (Crucial Step)
        #    The row at index 't' contains the Prediction made at 't'.
        #    We must multiply this by the return realized at 't+1'.
        #    lead() shifts the column UP by 1.
        next_log_return = lead(log_return)
    ) %>%
    # Remove the last row (NA because of lead)
    drop_na() %>%
    mutate(
        # D. Calculate Strategy PnL
        strategy_log_return = position * next_log_return,

        # E. Cumulative Wealth (Log Space)
        equity_curve = cumsum(strategy_log_return),
        market_curve = cumsum(next_log_return)
    ) %>%
    ungroup() %>%
    group_by(iteration, agent_type) %>%
    summarise(
        total_return = exp(tail(equity_curve, 1)) - 1,
        avg_return = mean(strategy_log_return),
        std_return = sd(strategy_log_return),
        sharpe_ratio = (avg_return / std_return) * sqrt(252)
    )

# 3. Calculate Performance Metrics
total_return <- exp(tail(strategy_df$equity_curve, 1)) - 1
avg_return <- mean(strategy_df$strategy_log_return)
std_return <- sd(strategy_df$strategy_log_return)
sharpe_ratio <- (avg_return / std_return) * sqrt(252) # Annualized

# plots ----
