# lib load ----
pacman::p_load(
    tidyverse,
    ggplot2,
    patchwork,
    ggthemes
)
setwd(this.path::here())


# helper functions ----
euc_dist <- function(x2, x1, y2, y1) {
    return(sqrt((x2 - x1)^2 + (y2 - y1)^2))
}

# load data ----
load("../data/fixData.RData")
eye_raw <- fixData
rm(fixData)


# get saccades (same as euc dist)
eye_trial <- eye_raw %>%
    separate_wider_delim(
        text,
        delim = "_",
        names = c(
            "id",
            "task_indicator",
            "task_number",
            "trial_indicator",
            "trial_number",
            "action_indicator",
            "action_type"
        ),
        too_few = "align_start",
        too_many = "merge"
    ) %>%
    filter(
        task_indicator == "game",
        task_number != "instructions",
        trial_indicator == "trial",
        action_indicator == "ITI"
    ) %>%
    mutate(
        task_number = as.numeric(task_number),
        trial_number = as.numeric(trial_number),
        id = id
    ) %>%
    drop_na(gazex, gazey) %>%
    ungroup() %>%
    group_by(
        id,
        task_number,
        trial_number
    ) %>%
    mutate(
        euclidean_distance = euc_dist(
            gazex, lag(gazex), gazey, lag(gazey)
        )
    ) %>%
    drop_na(
        euclidean_distance
    )
eye_trial

(unique(eye_trial$id))

# filters ----

## outlier amplitudes ----
outlier_amplitudes <- quantile(eye_trial$euclidean_distance, probs = 0.75) +
    (1.5 * IQR(eye_trial$euclidean_distance))
outlier_amplitudes

# filter out outliers
eye_trial_filtered <- eye_trial %>%
    filter(euclidean_distance <= outlier_amplitudes) %>%
    select(
        id,
        task_indicator, task_number,
        trial_indicator, trial_number,
        euclidean_distance
    )
eye_trial_filtered

write_rds(x = eye_trial_filtered, file = "../data/saccades.rds")

# figures -----

## saccades distribution ----

f1 <- eye_trial_filtered %>%
    ggplot(aes(
        euclidean_distance,
        group = 1
    )) +
    geom_boxplot(aes(group = 1, y = -0.1),
        width = 0.1,
        outlier.shape = NA
    ) +
    geom_density(aes(group = id)) +
    stat_boxplot(
        geom = "vline", aes(xintercept = ..xlower..),
        linetype = "dashed"
    ) +
    stat_boxplot(
        geom = "vline", aes(xintercept = ..xmiddle..),
        linetype = "dashed"
    ) +
    stat_boxplot(
        geom = "vline", aes(xintercept = ..xupper..),
        linetype = "dashed"
    ) +
    theme_par()
f1

## saccades over trials ----
f2 <- eye_trial_filtered %>%
    ggplot(aes(
        trial_number, euclidean_distance
    )) +
    stat_summary(
        fun.data = "mean_se",
        geom = "pointrange",
        aes(group = trial_number)
    ) +
    stat_summary(
        fun.data = "mean_se",
        geom = "line",
        aes(group = 1)
    ) +
    stat_summary(
        fun.data = "mean_se",
        geom = "line",
        aes(group = id),
        alpha = 0.1
    ) +
    theme_par()
f2
