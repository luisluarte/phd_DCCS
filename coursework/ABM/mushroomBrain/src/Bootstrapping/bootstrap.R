# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    blocklength,
    boot,
    tictoc,
    PerformanceAnalytics,
    furrr,
    jsonlite
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
            cfgSporeBatchSize = 10,
            cfgMaintenanceCost = 0.01,
            cfgDispersionRadius = 0.1,
            cfgInitMaturity = 50.0,
            cfgMaxOrders = 10,
            cfgMaxChildren = 10,
            cfgInitGreed = greed,
            cfgDcaOrder = 0.5,
            cfgInitBaseOrder = 0.5,
            cfgInitTurbulence = 0.1, # 10% random motion noise
            cfgInitGrowthRate = 0.01, # 1% growth per trade/tick
            cfgInitPhiCritical = 1.0, # Quorum sensing threshold
            cfgInitReproductiveInvest = 0.2, # Sacrifice 20% mass to reproduce
            cfgInitVacuumCoefficient = 1.0, # Strong suction toward price targets
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
setwd("C:/Users/DCCS5/Documents/GitHub/phd_DCCS_linux/coursework/ABM/mushroomBrain")
plan(multisession, workers = 4)
results <- boot_data %>%
    map(
        ., function(time_series) {
            raw_output <- run_simulation(time_series, 0.5, mutation_on = TRUE)
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

# optimization ----
objective_fn <- function(par, time_series) {
    # 1. Force conversion of the 'par' vector into a standard list to strip names
    # and ensure types match the Haskell SimConfig exactly.

    # Map by name to local variables first for safety
    p_maint <- as.numeric(par["maint"])
    p_maturity <- as.numeric(par["maturity"])
    p_greed <- as.numeric(par["greed"])
    p_turb <- as.numeric(par["turbulence"])
    p_growth <- as.numeric(par["growth_rate"])
    p_phi <- as.numeric(par["phi_crit"])
    p_inv <- as.numeric(par["repInvest"])
    p_vacuum <- as.numeric(par["vacuum"])
    p_dev <- as.numeric(par["dev_mult"])
    p_disp <- as.numeric(par["dispersionRadius"])
    p_base <- as.numeric(par["baseOrder"])
    p_dca <- as.numeric(par["dcaOrder"])

    # Cast these strictly to Integers
    p_batch <- as.integer(par["sporeBatchSize"])
    p_orders <- as.integer(par["maxOrders"])
    p_children <- as.integer(par["maxChildren"])

    # 2. Build the exact structure Haskell expects
    payload <- list(
        inputPrices = as.numeric(time_series),
        inputConfig = list(
            # A. Simulation Control
            cfgEnableMutation = TRUE,
            cfgEnableIntelligence = TRUE,
            cfgSporeBatchSize = p_batch,

            # B. Fixed System Parameters
            cfgDcaOrder = p_dca,
            cfgMaxOrders = p_orders,
            cfgMaxChildren = p_children,
            cfgDispersionRadius = p_disp,
            cfgMaintenanceCost = p_maint,

            # C. Genesis Genome
            cfgInitGreed = p_greed,
            cfgInitTurbulence = p_turb,
            cfgInitGrowthRate = p_growth,
            cfgInitBaseOrder = p_base,
            cfgInitPhiCritical = p_phi,
            cfgInitReproductiveInvest = p_inv, # Hardcoded if not in par
            cfgInitVacuumCoefficient = p_vacuum,
            cfgInitDevMult = p_dev,
            cfgInitMaturity = p_maturity
        )
    )

    # 3. Serialize with auto_unbox=TRUE to prevent [0.5] instead of 0.5
    json_input <- jsonlite::toJSON(payload, auto_unbox = TRUE, digits = NA)

    # 4. Execute (ensure -v0 to keep stdout clean for JSON)
    raw_output <- system2(
        command = "cabal",
        args = c("run", "-v0", "mycelial-exe", "--", "--pipeline"),
        input = json_input,
        stdout = TRUE,
        stderr = TRUE
    )

    # 5. Noise filtering
    if (length(raw_output) > 0 && !grepl("^\\{", raw_output[1])) {
        json_line <- raw_output[grep("^\\{", raw_output)]
        if (length(json_line) == 0) {
            return(1e10)
        }
        raw_output <- json_line
    }

    # 6. Parse and calculate return
    result <- tryCatch(
        jsonlite::fromJSON(paste(raw_output, collapse = "")),
        error = function(e) {
            return(NULL)
        }
    )

    if (is.null(result)) {
        return(1e10)
    }

    # Metric calculation
    equity <- as.numeric(result$outputStats$statTotalWealth)
    if (length(equity) < 2) {
        return(1e10)
    }

    # Simplified metric if PerformanceAnalytics fails: Log Return of terminal wealth
    # (Optim is much more stable with this during early iterations)
    total_ret <- log(tail(equity, 1) / equity[1])

    if (!is.finite(total_ret)) {
        return(1e10)
    }
    return(-total_ret)
}

# Define initial starting points (Middle-of-the-road values)
init_params <- c(
    sporeBatchSize = 1, # Int
    maxOrders = 3, # Int
    maxChildren = 3, # Int
    maint = 0.0001,
    maturity = 50.0,
    greed = 0.5,
    turbulence = 0.1,
    growth_rate = 0.01,
    phi_crit = 1.0,
    repInvest = 0.2,
    vacuum = 0.1,
    dev_mult = 1.0,
    dispersionRadius = 0.1,
    baseOrder = 0.5,
    dcaOrder = 0.1
)

# Define Lower Bounds (Logical minimums)
lower_bounds <- c(
    sporeBatchSize = 1,
    maxOrders = 1,
    maxChildren = 1,
    maint = 0.0, # Zero maintenance possible
    maturity = 5.0,
    greed = 0.0,
    turbulence = 0.0,
    growth_rate = 0.001,
    phi_crit = 0.1,
    repInvest = 0.05,
    vacuum = 0.0,
    dev_mult = 0.1,
    dispersionRadius = 0.01,
    baseOrder = 0.1,
    dcaOrder = 0.01
)

# Define Upper Bounds (Logical maximums)
upper_bounds <- c(
    sporeBatchSize = 10,
    maxOrders = 10,
    maxChildren = 10,
    maint = 0.01, # Max 1% per tick
    maturity = 500.0,
    greed = 1.0,
    turbulence = 1.0,
    growth_rate = 0.5,
    phi_crit = 10.0,
    repInvest = 0.5,
    vacuum = 1.0,
    dev_mult = 5.0,
    dispersionRadius = 0.5,
    baseOrder = 5.0,
    dcaOrder = 2.0
)

# Execute the optimization
opt_results <- optim(
    par = init_params,
    fn = objective_fn,
    time_series = boot_data[[1]][1:10], # Passing the first bootstrap sample
    method = "L-BFGS-B",
    lower = lower_bounds,
    upper = upper_bounds,
    control = list(
        trace = 1, # Prints progress to console
        maxit = 1, # Maximum iterations
        fnscale = 1, # We are minimizing -Return (maximizing Return)
        parscale = init_params # Normalizes gradients for parameters on different scales
    )
)

# Extract optimized parameters
best_config <- opt_results$par
print(best_config)


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
        statPopSize, mean_vacuum
    )) %>%
    ggplot(aes(statTick, value, color = name)) +
    geom_line() +
    facet_wrap(~name, scale = "free")



# process data ----
results[[1]]$spx_mb %>% table.AnnualizedReturns()
