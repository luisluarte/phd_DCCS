pacman::p_load(
    tidyverse
)
setwd(this.path::here())

# load data ----
rating <- read_csv("../data/inter_rater.csv")

# agreement metric
discrepancy_mat <- rating %>%
    rowwise() %>%
    mutate(
        max_sd = sd(c(4, 1)),
        std_dev = sd(c(rater1, rater2)),
        mean_score = mean(c(rater1, rater2))
    ) %>%
    ungroup() %>%
    mutate(
        agreement = (1 - (std_dev / max_sd))
    )
discrepancy_mat

# mean agreement per item
mean_agreement <- discrepancy_mat %>%
    ungroup() %>%
    group_by(item) %>%
    summarise(
        mean_agreement = mean(agreement),
        mean_score = mean(mean_score)
    ) %>%
    mutate(
        dimension = if_else(item <= 10, "belief", "stochasticity")
    ) %>%
    filter(
        mean_agreement >= 0.8
    )
mean_agreement
