pacman::p_load(
    tidyverse,
    ggplot2,
    ggthemes,
    ggokabeito,
    igraph,
    neuprintr
)
setwd(this.path::here())

# dataset login ----
neuprint_login(
    server = "https://neuprint.janelia.org",
    token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6Im5pY29uaWNvbHVhcnRlQGdtYWlsLmNvbSIsImxldmVsIjoibm9hdXRoIiwiaW1hZ2UtdXJsIjoiaHR0cHM6Ly9saDMuZ29vZ2xldXNlcmNvbnRlbnQuY29tL2EvQUNnOG9jTGZIWWtfbndCdjBfSUdwMWR5d1BBRVZ6LV9jRDgyRTIxMVlMZFpDajdzdEkxN1pBPXM5Ni1jP3N6PTUwP3N6PTUwIiwiZXhwIjoxOTM0OTI1NTgwfQ.R399P52aj7_d6lc6bDkNs6WuTiXf8V3PzL1X5vNYUZ0",
    dataset = "hemibrain:v1.1"
)

# neurontype ----
get_neuron_nt <- function(bodyids) {
    bodyids_str <- paste(bodyids, collapse = ", ")
    # query_cypher <- paste0(
    #    "MATCH (n:Neuron) WHERE n.bodyId IN [", bodyids_str, "] ",
    #    "RETURN n.bodyId AS bodyid, n.type AS type, n.predictedNt AS predictedNt"
    # )
    query_cypher <- paste0(
        "MATCH (n:hemibrain_Neuron) WHERE n.bodyId IN [", bodyids_str, "] ",
        "RETURN n"
    )
    meta <- neuprint_fetch_custom(cypher = query_cypher)
    cols <- unlist(meta$columns)
    dat <- unlist(meta$data)
    return(meta)
}

neuron_list <- unique(data_raw$bodyId_pre)

dat <- get_neuron_nt(neuron_list[100])
dat


# approximate neuron location ----
estimate_neuron_roi <- function(data, pre_post) {
    dat <- data %>%
        select(bodyId_post, roi, weight) %>%
        group_by(bodyId_post, roi) %>%
        summarise(
            synapse_sum = sum(weight)
        ) %>%
        slice_max(order_by = synapse_sum, n = 1) %>%
        rename(
            neuron_id = bodyId_post,
            most_dense_roi = roi
        ) %>%
        select(-synapse_sum)
    return(dat)
}


# read data ----
data_raw <- read_csv("../data/traced-roi-connections.csv") %>%
    mutate(
        roi = str_remove(roi, pattern = "\\([LR]\\)")
    )
data_raw

metadata <- read_csv("../data/metadata.csv")

most_dense_roi <- estimate_neuron_roi(data_raw)
most_dense_roi

merge_data_raw <- data_raw %>%
    left_join(., metadata, by = c("roi")) %>%
    left_join(., most_dense_roi, by = c("bodyId_pre" = "neuron_id")) %>%
    rename(most_dense_roi_pre = most_dense_roi) %>%
    left_join(., most_dense_roi, by = c("bodyId_post" = "neuron_id")) %>%
    rename(most_dense_roi_post = most_dense_roi)
merge_data_raw

sum_conn_mat <- merge_data_raw %>%
    group_by(most_dense_roi_pre, most_dense_roi_post) %>%
    summarise(
        summ_conn = sum(weight)
    )
sum_conn_mat

# n degrees ----

neuron_mat <- graph_from_data_frame(
    sum_conn_mat %>% select(most_dense_roi_pre, most_dense_roi_post),
    directed = TRUE
)
neuron_mat

adj_mat <- as_adjacency_matrix(neuron_mat)


sum_conn_mat %>%
    drop_na() %>%
    ggplot(aes(
        most_dense_roi_pre, most_dense_roi_post
    )) +
    geom_tile(aes(fill = (summ_conn)),
        color = "white"
    ) +
    theme_par() +
    coord_fixed() +
    scale_fill_gradient(
        low = "white",
        high = palette_okabe_ito()[3]
    ) +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)
    ) +
    xlab("") +
    ylab("")
