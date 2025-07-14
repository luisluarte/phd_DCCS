pacman::p_load(
    tidyverse,
    psych
)
setwd(this.path::here())

# raw data ----
data_raw <- read_csv("../data/responses.csv")

# data ----
data <- data_raw %>%
    rename(
        timestamp = Timestamp,
        birth = `Fecha nacimiento`,
        sex = `Sexo biológico`,
        educ = `Maximo nivel educacional completado`
    ) %>%
    mutate(
        timestamp = lubridate::mdy_hms(timestamp),
        birth = lubridate::mdy(birth),
        sex = as.factor(sex),
        educ = as.factor(educ),
        across(c(5:30), ~ as.factor(.x)),
        id = row_number()
    )
data

# factorial analysis ----

fac_an <- data[, 5:11]

fac_an_num <- fac_an %>%
    mutate(
        across(everything(), as.numeric)
    )

# alpha ----

alpha_data <- alpha(fac_an_num, check.keys = TRUE)
alpha_data

# parallel ----

parallel_result <- fa.parallel(fac_an_num, fm = "ml", fa = "fa", n.iter = 1000)

# EFA ----

num_factors <- parallel_result$nfact
efa_result <- fa(fac_an_num, nfactors = num_factors, rotate = "varimax")

print(efa_result)

# weights ----

efa_weights <- efa_result$weights %>%
    as_tibble() %>%
    mutate(
        items = colnames(fac_an_num),
        my_dim = c(
            "belief",
            "belief",
            "belief",
            "belief",
            "stochasticity",
            "stochasticity",
            "stochasticity"
        )
    )
efa_weights
