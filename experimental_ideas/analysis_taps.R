# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    MSEntropy,
    nonlinearTseries
)
setwd(this.path::here())

# raw data ----
data_raw <- read_csv("data/p01_2025_04_21_23_02_35_943999.csv")

# proc data ----
iti_data <- data_raw %>% 
    mutate(
        ITI = timestamp_abs - lag(timestamp_abs, n = 1)
    ) %>% 
    drop_na()
iti_data

# detrended iti ----

detrend_iti <- iti_data %>% 
    mutate(
        detrended_iti = resid(lm(ITI ~ timestamp_abs, data = iti_data))
    )

# lets explore the linear regression
iti_mdl <- MASS::rlm(
    data = iti_data,
    ITI ~ timestamp_abs
)
summary(iti_mdl)

# lets do the fit in a IQR restricted ITI

iti_data %>% 
    filter(ITI>=0.399-IQR(iti_data$ITI), ITI <=0.43 + IQR(iti_data$ITI)) %>% 
    ggplot(aes(
        ITI
    )) +
    geom_density()

iti_data %>% 
    mutate(
        t_bin = trunc(timestamp_abs/60)
    ) %>% 
    filter(ITI>=0.399-IQR(iti_data$ITI), ITI <=0.43 + IQR(iti_data$ITI)) %>% 
    ggplot(aes(
        as.factor(t_bin), ITI
    )) +
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "pointrange",
        aes(group = t_bin)
    ) +
    stat_summary(
        fun.data = "mean_se",
        geom = "line",
        aes(group = 1)
    )

iti_data %>% 
    filter(ITI>=0.399-IQR(iti_data$ITI), ITI <=0.43 + IQR(iti_data$ITI)) %>% 
    ggplot(aes(
        timestamp_abs, ITI
    )) +
    geom_point() +
    geom_smooth(method = "gam") +
    scale_y_continuous(
        limits = c(0.25, 0.6)
    )

iti_mdl_bnd <- lm(
    data = iti_data %>% 
        filter(ITI>=0.399-IQR(iti_data$ITI), ITI <=0.43 + IQR(iti_data$ITI)),
    ITI ~ smooth.spline(timestamp_abs, ITI)$y
)
summary(iti_mdl_bnd)

plot(resid(iti_mdl_bnd))



# DFA ----

dfa_exp <- dfa(
    iti_data$ITI,
    npoints = 100,
    do.plot = FALSE,
    window.size.range = c(10, 500)
)
estimate(dfa_exp, do.plot = TRUE)

# MSE ----

data_sd <- 0.15 * sd(iti_data$ITI)

data_mse <- MSE(
    iti_data$ITI,
    m = 2,
    r = 0.15,
    scales = 1:20
)

tibble(
    x = 1:20,
    y = data_mse
) %>% 
    ggplot(aes(
        x, y
    )) +
    geom_line() +
    geom_line(aes(x, (1/x) + tail(y, n=1)), color = "red") +
    geom_point()


mse_results <- SampEn(
    iti_data$ITI,
    dim = 2,
    lag = 10,
    r = 0.2 * data_sd
)



# plots ----

p1 <- iti_data %>% 
    ggplot(aes(
        ITI
    )) +
    geom_density() +
    ggpubr::theme_classic2() +
    theme(
        axis.line = element_line(colour = "white", linewidth = 0),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
    )
p1

p2 <- iti_data %>% 
    ggplot(aes(
        timestamp_abs, ITI
    )) +
    geom_point()
p2

p3 <- detrend_iti %>% 
    ggplot(aes(
        timestamp_abs, detrended_iti+1
    )) +
    geom_smooth(method = "gam") +
    scale_y_continuous(transform = "log")
p3

# iti data ----

raw_data <- read_delim("raw_data.csv", delim = ";")
clean_data <- read_delim("cleaned_data.csv", delim = ";")

filtered_p <- unique(clean_data$subj_no)

tempo_data <- raw_data %>% 
    filter(subj_no %in% filtered_p) %>% 
    group_by(subj_no) %>% 
    group_split() %>% 
    map_dfr(., function(X){
        interval_dat <- as.numeric(unlist(str_split(X$intervals, pattern = ";")))
        return(tibble(
            id = X$subj_no[1],
            intervals = interval_dat,
            timestamp = cumsum(intervals) - head(intervals, n = 1)
        ))
    })
tempo_data

mdl0 <- lmerTest::lmer(
    data = tempo_data %>%
        group_by(id) %>% 
        mutate(ts = scales::rescale(timestamp, to = c(0, 1))) %>% 
        ungroup(), 
    log(intervals) ~ ts + (ts|id)
)
summary(mdl0)

plot(resid(mdl0))
qqnorm(resid(mdl0))
qqline(resid(mdl0))
