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
eye_raw <- read_csv("../../data/econometrics/10.1073pnas.1911348117/fixData.csv")

# pre-proc data ----

# action indicator = ITI: fixation cross without any instruction ~5 sec

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
        trial_indicator == "trial"
    ) %>%
    mutate(
        task_number = as.numeric(task_number),
        trial_number = as.numeric(trial_number)
    )
eye_trial


## check single trial
trial_test <- eye_trial %>%
    filter(
        is_trial == 1,
        subjectID == 2354282,
        game_number == 1
    ) %>%
    group_by(text) %>%
    mutate(
        euc_distance = euc_dist(gazex, lag(gazex), gazey, lag(gazey))
    ) %>%
    drop_na() %>%
    arrange(., desc(euc_distance), .by_group = TRUE) %>%
    mutate(
        ccdf = row_number() / nrow(.)
    )

mdls <- trial_test %>%
    ungroup() %>%
    group_by(trial_number, text) %>%
    group_split() %>%
    map_dfr(., function(X) {
        mdl <- lm(log(ccdf) ~ log(euc_distance), data = X)
        return(
            tibble(
                trial = X$trial_number[1],
                text = X$text[1],
                coef = (-coef(mdl)[2]) + 1,
                rsq = summary(mdl)$r.squared
            )
        )
    })

mdls %>%
    filter(
        grepl(pattern = "ITI_start", x = text),
        !grepl(pattern = "practice", x = text)
    ) %>%
    drop_na() %>%
    ggplot(aes(
        trial, coef,
        color = rsq
    )) +
    geom_point()


mdls %>%
    filter(
        grepl(pattern = "ITI_start", x = text),
        !grepl(pattern = "practice", x = text)
    ) %>%
    drop_na() %>%
    ggplot(aes(
        coef
    )) +
    geom_density()

trial_test %>%
    ggplot(aes(
        euc_distance, ccdf
    )) +
    geom_point(aes(color = trial_number)) +
    geom_smooth(aes(group = trial_number, color = trial_number), method = "lm", se = FALSE) +
    scale_x_continuous(transform = "log") +
    scale_y_continuous(transform = "log")

var <- load("../../data/econometrics/10.1073pnas.1911348117/modelResults.RData")
