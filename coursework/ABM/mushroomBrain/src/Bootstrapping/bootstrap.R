# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    blocklength,
    boot,
    tictoc,
    PerformanceAnalytics,
    furrr,
    jsonlite,
    optimParallel
)
setwd(this.path::here())


# get data ----
d <- read_csv("../../SPX.csv")

log_returns <- d %>%
    mutate(
        Date = lubridate::ymd(Date),
        log_returns = c(NA_real_, diff(log(Close)))
    ) %>%
    drop_na() %>%
    filter(Date >= "2000-01-01")

# bootstrap blocksize ----

pw <- pwsd(data = (log_returns$log_returns))

b_circular <- pw$BlockLength[2]

# parse JSON ----
run_simulation <- function(price_segment, greed, mutation_on) {
    # A. Construct Payload
    payload <- list(
        inputPrices = as.vector(price_segment),
        inputConfig = list(
            # --- Simulation Control ---
            cfgEnableMutation = mutation_on,
            cfgEnableIntelligence = TRUE,
            cfgSporeBatchSize = 3,
            cfgMaintenanceCost = 0.0,
            cfgDispersionRadius = 0.1,
            cfgInitMaturity = 50.0,
            cfgMaxOrders = 10,
            cfgMaxChildren = 5,
            cfgInitGreed = greed,
            cfgDcaOrder = 0.5,
            cfgInitBaseOrder = 0.5,
            cfgInitTurbulence = 0.1, # 10% random motion noise
            cfgInitGrowthRate = 0.01, # 1% growth per trade/tick
            cfgInitPhiCritical = 1.0, # Quorum sensing threshold
            cfgInitReproductiveInvest = 0.9, # Sacrifice 20% mass to reproduce
            cfgInitVacuumCoefficient = 0.0, # Strong suction toward price targets
            cfgInitDevMult = 1.0 # No deviation multiplier initially
        )
    )

    # B. Serialize
    json_input <- toJSON(payload, auto_unbox = TRUE, digits = NA)

    # C. Execute Haskell
    raw_output <- system2(
        command = "cabal",
        args = c("run", "-v0", "mycelial-exe", "--", "--pipeline"),
        input = json_input,
        stdout = TRUE,
        stderr = TRUE # CHANGED to TRUE to see errors if they happen
    )

    # Check if we got an error message from Cabal instead of JSON
    # (JSON usually starts with '{')
    if (length(raw_output) > 0 && !grepl("^\\{", raw_output[1])) {
        # This filters out "Up to date" messages if they appear before JSON
        json_line <- raw_output[grep("^\\{", raw_output)]
        if (length(json_line) == 0) {
            warning("Haskell Error: ", paste(raw_output, collapse = "\n"))
            return(NULL)
        }
        raw_output <- json_line
    }

    # D. Parse Output
    if (length(raw_output) == 0) {
        return(NULL)
    }

    result <- tryCatch(
        fromJSON(paste(raw_output, collapse = "")), # Paste collapses lines just in case
        error = function(e) {
            return(NULL)
        }
    )

    return(result)
}

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

TIME <- 252
boot_data <- get_bootstrap(round(b_circular), TIME, log_returns$log_returns, 1)

# haskell interface ----
setwd("/home/nicoluarte/Documents/repos/phd_DCCS/coursework/ABM/mushroomBrain")
plan(multisession, workers = 4)

results <- boot_data %>%
    map(
        ., function(time_series) {
            raw_output <- run_simulation(time_series, 0.5, mutation_on = FALSE)
            # raw_output <- system2(
            #     command = "cabal",
            #     args = c("run", "-v0", "mycelial-exe", "--", "--pipeline"),
            #     input = paste(time_series, collapse = "\n"),
            #     stdout = TRUE
            # )
            # equity_curve <- as.numeric(raw_output)
            # dates <- seq(as.Date("2000-01-01"), by = "day", length.out = ts_length)
            # len <- min(length(equity_curve), length(dates))
            # benchmark_xts <- xts(time_series[1:len], order.by = dates[1:len])
            # equity_xts <- xts(equity_curve[1:len], order.by = dates[1:len])
            # benchmark_returns <- Return.calculate(benchmark_xts)
            # sim_returns <- Return.calculate(equity_xts)
            # return(list(spx_mb = sim_returns, spx_bh = benchmark_returns))
            return(raw_output)
        }
    )
stats_df <- results[[1]]$outputStats
stats_df <- stats_df %>%
    mutate(
        mean_greed = map_dbl(statGeneGreed, mean),
        mean_turbulence = map_dbl(statGeneTurbulence, mean),
        mean_pop = map_dbl(statPopSize, mean),
        mean_phi = map_dbl(statGenePhiCritical, mean),
        mean_biobank = map_dbl(statBioBank, mean),
        mean_holding = map_dbl(statHoldings, mean),
        mean_vacuum = map_dbl(statGeneVacuumCoefficient, mean),
        mean_drop = map_dbl(statStratDrop, mean),
        mean_tp = map_dbl(statStratProfit, mean),
        sum_biobank = map_dbl(statBioBank, sum),
        sum_holding = map_dbl(statHoldings, sum)
    )
stats_df %>%
    pivot_longer(cols = c(
        mean_tp, mean_drop, mean_biobank,
        mean_holding, sum_biobank, sum_holding, statMktPrice,
        statPopSize, mean_vacuum, mean_phi
    )) %>%
    ggplot(aes(statTick, value, color = name)) +
    geom_line() +
    facet_wrap(~name, scale = "free")
