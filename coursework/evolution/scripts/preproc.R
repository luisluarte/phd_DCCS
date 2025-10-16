pacman::p_load(
    tidyverse,
    psych,
    likert,
    ggthemes
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

# demographics

demographics <- data %>%
    select(
        birth,
        sex,
        educ
    ) %>%
    mutate(
        years_old = trunc(as.numeric(Sys.Date() - birth) / 365),
        sex = if_else(sex == "Femenino", "F", "M")
    )

summary(demographics$years_old)
table(demographics$sex)
table(demographics$educ)

likert_levels <- c(
    "Totalmente en desacuerdo",
    "En desacuerdo", "Neutral", "De acuerdo", "Totalmente de acuerdo"
)

survey_data <- data %>%
    select(-c(id, timestamp, birth, sex, educ)) %>%
    mutate(across(everything(), ~ factor(., levels = likert_levels)))

survey_data %>%
    pivot_longer(
        cols = everything()
    ) %>%
    group_by(name, value) %>%
    summarise(
        cnt = n()
    ) %>%
    ungroup() %>%
    mutate(
        item = data.table::rleid(name)
    ) %>%
    ggplot(aes(
        value, cnt
    )) +
    geom_col() +
    scale_y_continuous(breaks = seq(0, 20, 10)) +
    scale_x_discrete(labels = c("TD", "D", "N", "A", "TA")) +
    theme_par() +
    ylab("Frecuencia") +
    xlab("") +
    facet_wrap(~item)


# factorial analysis ----
big_five <- data[, 12:30]
fac_an <- data[, 5:11]

fac_an_num <- fac_an %>%
    mutate(
        across(everything(), as.numeric)
    )

# alpha ----

alpha_data_belief <- alpha(fac_an_num[, 1:4], check.keys = TRUE)
alpha_data_belief

alpha_data_estoc <- alpha(fac_an_num[, 5:7], check.keys = TRUE)
alpha_data_estoc

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

# correlations
data_num <- data %>%
    mutate(
        across(everything(), as.numeric)
    )

keys.list <- list(
    alpha = c(colnames(data_num[, 5:8])),
    temperature = c(colnames(data_num[, 9:11])),
    Openness = c("Es inventivo", "Es original, se le ocurren ideas nuevas", "Valora lo artístico, lo estético", "Tiene una imaginación activa", "Le gusta reflexionar, jugar con las ideas", "Es educado en arte, música, o literatura", "En ingenioso, analítico", "Tiene intereses muy diversos", "-Prefiere trabajos que son rutinarios", "-Tiene poco intereses artísticos"),
    Conscientiousness = c("Es minucioso en el trabajo", "Hace las cosas de manera eficiente", "Hace planes y los sigue cuidadosamente", "Es un trabajador cumplidor, de confianza", "Persevera hasta terminar el trabajo", "-Se distrae con facilidad", "-Puede a veces ser algo descuidado", "-Tiende a ser flojo, vago", "-Tiende a ser desorganizado")
)


scale_scores <- scoreItems(keys.list, data_num)

final_scores_df <- as_tibble(scale_scores$scores) %>%
    rename(
        Stability = alpha,
        Stochasticity = temperature
    )

correlation_matrix <- corr.test(final_scores_df)

corrplot::corrplot(correlation_matrix$r,
    p.mat = correlation_matrix$p,
    type = "upper",
    method = "number"
)
