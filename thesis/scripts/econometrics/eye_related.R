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

## filter trial related data
eye_trial <- eye_raw %>%
    mutate(
        is_trial = replace_na(as.numeric(str_match(text, pattern = "trial") == "trial"), 0),
        game_number = str_extract(text, pattern = "game_[0-9]+") %>%
            str_extract(., pattern = "[0-9]+") %>%
            as.numeric(),
        trial_number = str_extract(text, pattern = "trial_[0-9]+") %>%
            str_extract(., pattern = "[0-9]+") %>%
            as.numeric()
    )

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
