# load libs ----
pacman::p_load(
    tidyverse
)
setwd(this.path::here())

# load data ----
train_data <- read_csv("train.csv")
test_data <- read_csv("test.csv")

# data prep ----

# model ----

glm_0 <- glm(
    data = train_data %>% select(-id),
    y ~ .,
    family = binomial(link = "logit")
)
summary(glm_0)

## preds ----

pred <- test_data %>%
    mutate(
        y = predict(object = glm_0, newdata = test_data, type = "response")
    )

write_csv(x = pred %>% select(id, y), file = "submission.csv")
