# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    blocklength,
    boot,
    tictoc,
    PerformanceAnalytics,
    furrr
)

setwd(this.path::here())


# get data ----
d <- read_csv("../../SPX.csv")
d

log_returns <- d %>%
    mutate(
        Date = lubridate::ymd(Date),
        log_returns = c(NA_real_, diff(log(Close)))
    ) %>%
    drop_na() %>%
    filter(Date >= "2000-01-01")
log_returns

# politis and white block size ----

pw <- pwsd(data = (log_returns$log_returns))
pw

b_circular <- pw$BlockLength[2]
b_circular

# synthetic bootstrap ----

# Pure Bull: Linear upward trend (Pt​=Pt−1​+c).
# Pure Bear: Linear downward trend (Pt​=Pt−1​−c).
# Pure Oscillation: Sine wave (Pt​=Asin(wt)+noise).
# Shock/Crash: Stable price followed by a 20% instantaneous drop.
bull_series <- function(sim_size) {
    (rbeta(n = sim_size, shape1 = 2, shape2 = 1) / 500) - (0.5 / 500)
}
bear_series <- function(sim_size) {
    (rbeta(n = sim_size, shape1 = 1, shape2 = 2) / 500) - (0.5 / 500)
}
sideways_series <- function(sim_size) {
    (rbeta(n = sim_size, shape1 = 2, shape2 = 2) / 500) - (0.5 / 500)
}
crash_series <- function(sim_size) {
    pivot_point <- floor(runif(n = 1, min = sim_size * 0.3, max = sim_size * 0.7))
    print(pivot_point)
    c(bull_series(pivot_point), bear_series(sim_size - pivot_point))[1:sim_size]
}
regimes <- function(sim_size, regime_type, n_sims) {
    switch(regime_type,
        "bull" = replicate(n = n_sims, bull_series(sim_size), simplify = FALSE),
        "bear" = replicate(n = n_sims, bear_series(sim_size), simplify = FALSE),
        "sideways_series" = replicate(n = n_sims, sideways_series(sim_size), simplify = FALSE),
        "crash_series" = replicate(n = n_sims, crash_series(sim_size), simplify = FALSE),
        stop("invalid choice")
    )
}
regimes(252, "crash_series", 10)
# bootstrap with spx ----
get_bootstrap <- function(block_size, sim_size, data, sims) {
    f_size <- floor(sim_size / block_size)
    dat <- c(data, data)
    1:sims %>%
        map(., function(X) {
            boots <- 1:f_size %>%
                map(., function(b_s) {
                    first_idx <- floor(runif(min = 1, max = length(data), n = 1))
                    last_idx <- first_idx + block_size - 1
                    return(dat[first_idx:last_idx])
                })
            return(
                c(1, 1 * tail(exp(cumsum(unlist(boots))), -1))
            )
        })
}
ts_length <- floor(252)
ts_boot <- get_bootstrap(round(b_circular), ts_length, log_returns$log_returns, 1)

# haskell interface ----
setwd("C:/Users/DCCS5/Documents/GitHub/phd_DCCS_linux/coursework/ABM/mushroomBrain")
plan(multisession, workers = 4)
results <- ts_boot %>%
    map(
        ., function(time_series) {
            raw_output <- system2(
                command = "cabal",
                args = c("run", "-v0", "mycelial-exe", "--", "--pipeline"),
                input = paste(time_series, collapse = "\n"),
                stdout = TRUE
            )
            equity_curve <- as.numeric(raw_output)
            dates <- seq(as.Date("2000-01-01"), by = "day", length.out = ts_length)
            len <- min(length(equity_curve), length(dates))
            benchmark_xts <- xts(time_series[1:len], order.by = dates[1:len])
            equity_xts <- xts(equity_curve[1:len], order.by = dates[1:len])
            benchmark_returns <- Return.calculate(benchmark_xts)
            sim_returns <- Return.calculate(equity_xts)
            return(list(spx_mb = sim_returns, spx_bh = benchmark_returns))
        }
    )


# process data ----
results[[1]]$spx_mb %>% table.AnnualizedReturns()
