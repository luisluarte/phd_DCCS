# lib load ----
pacman::p_load(
    tidyverse,
    ggplot2,
    patchwork,
    ggthemes,
    gtsummary
)
setwd(this.path::here())

# datasets ----
spontaneous_behavior <- read_rds(file = "../data/saccades.rds")
decision_behavior <- read_rds(file = "../data/q_val_est.rds")
metadata <- read_rds("../data/metadata.rds") %>%
    rename(id = subjectID)

# trial-by-trial tau (likelihood function) ----

trial_by_trial_likelihood <- function(tau, q_values, chosen_action_index) {
    # tau must be positive
    if (tau <= 0) {
        return(Inf)
    }
    q_scaled <- q_values / tau
    max_q_scaled <- max(q_scaled)
    log_sum_exp <- max_q_scaled + log(sum(exp(q_scaled - max_q_scaled)))
    q_chosen_scaled <- q_scaled[chosen_action_index]
    log_prob_of_choice <- q_chosen_scaled - log_sum_exp
    return(-log_prob_of_choice)
}

optimize_tau <- function(q_values,
                         chosen_action_index,
                         start_tau = 1.0) {
    optim_result <- optim(
        par = start_tau,
        fn = trial_by_trial_likelihood,
        q_values = q_values,
        chosen_action_index = chosen_action_index,
        method = "Brent",
        lower = 1e-6,
        upper = 10
    )
    return(list(
        estimated_tau = optim_result$par,
        optimization_details = optim_result
    ))
}


trial_by_trial_tau <- decision_behavior %>%
    filter(iteration <= 100) %>%
    ungroup() %>%
    group_by(id, game, trial, iteration) %>%
    group_split() %>%
    map_dfr(
        ., function(d) {
            q_values <- d %>%
                select(q1:q6) %>%
                as_vector()
            chosen_action_index <- unique(d$action)
            est_tau <- optimize_tau(q_values, chosen_action_index)$estimated_tau
            return(tibble(
                id = unique(d$id),
                game = unique(d$game),
                overall_alpha = unique(d$alpha),
                overall_tau = unique(d$tau),
                trial = unique(d$trial),
                action = unique(d$action),
                reward = unique(d$reward),
                iteration = unique(d$iteration),
                trial_tau = est_tau
            ))
        },
        .progress = TRUE
    )

summary_tau <- trial_by_trial_tau %>%
    ungroup() %>%
    group_by(
        id,
        game,
        trial
    ) %>%
    summarise(
        trial_tau_mean = mean(trial_tau)
    ) %>%
    ungroup()
summary_tau

# trial_by_trial saccades ----

spontaneous_behavior_entropy <- spontaneous_behavior %>%
    group_by(
        id,
        task_number,
        trial_number
    ) %>%
    summarise(
        saccade_entropy = sd(euclidean_distance)
    ) %>%
    drop_na(saccade_entropy) %>%
    rename(
        game = task_number,
        trial = trial_number
    ) %>%
    mutate(id = as.numeric(id))
spontaneous_behavior_entropy

# merge datasets ----

unique(spontaneous_behavior_entropy$id)[unique(spontaneous_behavior_entropy$id) %in% unique(summary_tau$id)]

mdl_data <- summary_tau %>%
    ungroup() %>%
    left_join(
        spontaneous_behavior_entropy
    )
mdl_data

write_rds(x = mdl_data, file = "../data/model_data.rds")

# models ----

mdl_data <- read_rds("../data/model_data.rds") %>%
    left_join(
        metadata,
        by = c("id", "game")
    )
mdl_data

## data description ----

### table 1 ----

table1 <- mdl_data %>%
    drop_na(saccade_entropy) %>%
    select(trial_tau_mean, saccade_entropy, age, gender, mean_rt, max_reward) %>%
    mutate(
        gender = as.factor(gender)
    ) %>%
    tbl_summary(
        label = list(
            trial_tau_mean ~ "Temperature (τ)",
            saccade_entropy ~ "Saccades",
            age ~ "Age",
            gender ~ "Gender (N trials)",
            mean_rt ~ "Mean RT",
            max_reward ~ "Max. Reward"
        ),
        statistic = list(
            all_continuous() ~ "{mean} ({sd})"
        ),
        digits = all_continuous() ~ 3
    ) %>%
    modify_header(
        label = "**Variable**",
        stat_0 = "**Value**"
    ) %>%
    as_gt() %>%
    gt::tab_header(
        title = "Table 1: Descriptive Statistics of variables"
    )

### figure 1 ----

f1 <- mdl_data %>%
    ggplot(aes(
        saccade_entropy,
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
    ylab("Density") +
    xlab("Saccade length (px.)") +
    theme_par()
f1

f2 <- mdl_data %>%
    ggplot(aes(
        trial_tau_mean
    )) +
    geom_boxplot(aes(group = 1, y = -0.1),
        width = 0.1,
        outlier.shape = NA
    ) +
    geom_density() +
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
    ylab("Density") +
    xlab("Temperature (τ)") +
    theme_par()
f2

f3 <- mdl_data %>%
    drop_na(saccade_entropy) %>%
    ggplot(aes(
        trial, saccade_entropy,
        group = id
    )) +
    stat_summary(
        fun.data = "mean_se",
        geom = "ribbon",
        aes(group = 1),
        alpha = 0.25
    ) +
    stat_summary(
        fun.data = "mean_se",
        geom = "line",
        aes(group = 1)
    ) +
    theme_par() +
    xlab("Trial number") +
    ylab("Saccade std.")
f3

f4 <- mdl_data %>%
    group_by(id, game) %>%
    summarise(
        mean_tau = mean(trial_tau_mean)
    ) %>%
    ggplot(aes(
        as.factor(game), mean_tau
    )) +
    geom_boxplot(outlier.shape = NA) +
    geom_point() +
    theme_par() +
    ylab("Mean τ values") +
    xlab("Juego")
f4

## stats ----

# tau related descriptives

tau_games <- lm(
    data = mdl_data,
    trial_tau_mean ~ game
)
summary(tau_games)

tau_games_emm <- emmeans::emmeans(
    tau_games,
    pairwise ~ game,
    type = "response"
)
tau_games_emm

vesp_trial <- lm(
    data = mdl_data,
    saccade_entropy ~ trial
)
summary(vesp_trial)

vesp_trial_emm <- emmeans::emtrends(
    vesp_trial,
    ~trial,
    var = "trial"
)
summary(vesp_trial_emm, infer = TRUE)
vesp_trial

mean_rt <- mdl_data %>%
    group_by(id, game) %>%
    summarise(
        mean_rt = mean(mean_rt)
    )
mean_rt

mean_rt_mdl <- lm(
    data = mean_rt,
    mean_rt ~ id * game
)
summary(mean_rt_mdl)

mean_reward <- mdl_data %>%
    group_by(id, game) %>%
    summarise(
        mean_reward = mean(max_reward)
    )
mean_reward


mean_reward_mdl <- lm(
    data = mean_reward,
    mean_reward ~ id * game
)
summary(mean_reward_mdl)


### main models ----

main_model_1 <- lmerTest::lmer(
    data = mdl_data,
    trial_tau_mean ~ saccade_entropy * as.factor(game) + (1 | id)
)
summary(main_model_1)

main_model_1_emm <- emmeans::emtrends(
    main_model_1,
    pairwise ~ saccade_entropy * game,
    var = "saccade_entropy",
    type = "response"
)
main_model_1_emm

main_model_1_emm %>% emmeans::test()


main_model_2 <- lmerTest::lmer(
    data = mdl_data,
    trial_tau_mean ~ saccade_entropy * as.factor(game) +
        trial +
        age +
        gender +
        mean_rt +
        max_reward + (1 | id)
)
summary(main_model_2)

summary(mdl_data$saccade_entropy)

main_model_2_emm <- emmeans::emmeans(
    main_model_2,
    specs = ~ saccade_entropy * game +
        trial +
        age +
        gender +
        mean_rt +
        max_reward,
    at = list(saccade_entropy = seq(0, 2, by = 0.1))
)
main_model_2_emm

# walkaround for stargazer
class(main_model_1) <- "lmerMod"
class(main_model_2) <- "lmerMod"
stargazer::stargazer(
    main_model_1,
    main_model_2,
    type = "latex"
)



model_2_p <- main_model_2_emm %>%
    broom.mixed::tidy(., conf.int = TRUE) %>%
    ggplot(aes(
        saccade_entropy, estimate,
        fill = as.factor(game)
    )) +
    geom_line() +
    geom_ribbon(aes(
        ymin = conf.low,
        ymax = conf.high
    ), alpha = 0.5) +
    theme_par() +
    ggokabeito::scale_fill_okabe_ito(name = "Juego") +
    facet_wrap(~gender) +
    xlab("Saccade length std.") +
    ylab("τ model estimates")
model_2_p
