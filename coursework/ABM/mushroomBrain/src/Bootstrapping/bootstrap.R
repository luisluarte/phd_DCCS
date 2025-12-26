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

# generate log return bootstraps ----
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
            ret_sim <- table.DownsideRiskRatio(sim_returns)
            benchmark_sim <- table.DownsideRiskRatio(benchmark_returns)
            out <- tibble(
                metric = rownames(benchmark_sim),
                benchmark = benchmark_sim[, 1],
                mushroomBrain = ret_sim[, 1]
            )
            return(out)
        }
    )


res_bind <- bind_rows(results) %>%
    pivot_longer(cols = -metric)

lm_mdl <- lm(
    data = res_bind %>%
        filter(metric == "Sortino ratio") %>%
        mutate(value = value * sqrt(ts_length)),
    value ~ name
)
summary(lm_mdl)
emmeans::emmeans(
    lm_mdl,
    pairwise ~ name
)

bind_rows(results) %>%
    pivot_longer(cols = -metric) %>%
    ggplot(aes(
        name, value
    )) +
    geom_boxplot(outlier.shape = NA) +
    geom_point() +
    facet_wrap(~metric, scales = "free") +
    ggpubr::theme_classic2()
