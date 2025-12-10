# libs ----
pacman::p_load(
    tidyverse
)

setwd(this.path::here())

# load data ----
ideology_raw <- read_csv("ideologia_congreso_chile_2002_2026_long_format.csv") %>%
    mutate(
        period = case_when(
            Period == "Periodo_1" ~ 2002,
            Period == "Periodo_2" ~ 2006,
            Period == "Periodo_3" ~ 2010,
            Period == "Periodo_4" ~ 2014,
            Period == "Periodo_5" ~ 2018,
            Period == "Periodo_6" ~ 2022
        )
    )

# ruling party
ruling_party <- tibble(
    period = c(2002, 2006, 2010, 2014, 2018, 2022),
    ruling_party = c("PPD", "PS", "RN", "PS", "RN", "IND")
    # Boric choosen as IND because party did not survive
)

# roll
roll_files <- list.files(
    pattern = "matriz_*",
    full.names = TRUE
) %>%
    as.list()

roll_calls <- roll_files %>%
    map_dfr(., function(X) {
        period <- str_extract(X, pattern = "2[0-9]{3}")
        dat <- read_csv(X, show_col_type = FALSE) %>%
            mutate(
                full_name = paste(Nombre, ApellidoPaterno, ApellidoMaterno, sep = " ")
            ) %>%
            select(-1, -c(Nombre, ApellidoPaterno, ApellidoMaterno)) %>%
            pivot_longer(
                cols = -c(full_name, DiputadoId),
                names_to = "bill_id",
                values_to = "value"
            ) %>%
            filter(value %in% c("Afirmativo", "En Contra")) %>%
            mutate(
                value = if_else(value == "Afirmativo", 1, 0) # 1 = yea, 0 = nay
            ) %>%
            ungroup() %>%
            group_by(bill_id) %>%
            mutate(
                # use 50% + 1 to pass for all bills, 1 = pass, 0 = no pass
                outcome = if_else(mean(value) > 0.5, 1, 0),
                period = as.numeric(period)
            ) %>%
            ungroup()
    })
roll_calls

# political party ----
party <- ideology_raw %>%
    ungroup() %>%
    drop_na(party_1) %>%
    group_by(Name) %>%
    select(Name, party_1) %>%
    slice_head(n = 1) %>%
    rename(
        full_name = Name,
        party = party_1
    )
party

# ideology fabregas -----
ideology <- ideology_raw %>%
    ungroup() %>%
    drop_na(dim_ideology) %>%
    group_by(Name, period) %>%
    select(Name, dim_ideology) %>%
    slice_head(n = 1) %>%
    rename(
        full_name = Name,
        dim_ideology = dim_ideology
    ) %>%
    ungroup()
ideology

# merge bills with party ----
bills_party <- roll_calls %>%
    left_join(party, by = c("full_name")) %>%
    left_join(ideology, by = c("full_name", "period")) %>%
    left_join(ruling_party, by = c("period"))
bills_party

xp_xsq <- bills_party %>%
    group_by(period, full_name) %>%
    reframe(
        dim_ideology = dim_ideology[1],
        party = party[1],
        ruling_party = ruling_party[1]
    ) %>%
    ungroup() %>%
    group_by(period) %>%
    summarise(
        xp_mean = mean(dim_ideology[party == ruling_party[1]]), # ruling party mean
        xp_sd = sd(dim_ideology[party == ruling_party[1]]), # ruling party standar deviation
        xsq_mean = mean(dim_ideology), # overall (status quo) mean
        xsq_sd = sd(dim_ideology) # overall (status quo) standard deviation
    ) %>%
    ungroup()
xp_xsq

party_centers <- bills_party %>%
    ungroup() %>%
    group_by(period, full_name, party) %>%
    reframe(
        full_name = full_name[1],
        dim_ideology = dim_ideology[1],
        party = party[1]
    ) %>%
    ungroup() %>%
    group_by(period, party) %>%
    summarise(
        party_center = mean(dim_ideology)
    ) %>%
    ungroup()
party_centers


complete_data <- bills_party %>%
    ungroup() %>%
    left_join(xp_xsq, by = c("period")) %>%
    left_join(party_centers, by = c("period", "party"))
complete_data

write_csv(x = complete_data, file = "complete_data.csv")
