library(tidyverse)
source("get_MSE.R")

experiments <- tibble::tribble(
  ~cm_method, ~dm_norm, ~constant, ~missing_cost, ~min_age, ~max_age,
  "TRATE", "maxlength", NULL, NULL, NULL, NULL,
  "TRATE", "maxlength", 1.9982, NULL, NULL, NULL,
  "TRATE", "gmean", NULL, NULL, NULL, NULL,
  "FUTURE", "maxlength", NULL, NULL, NULL, NULL,
  "INDELS", "maxlength", NULL, NULL, NULL, NULL,
  "INDELSLOG", "maxlength", NULL, NULL, NULL, NULL,
  "FUTURE", "gmean", NULL, NULL, NULL, NULL,
  "FUTURE", "gmean", NULL, 1, NULL, NULL,
  "FUTURE", "gmean", NULL, NULL, 20, 55,
  "FUTURE", "gmean", NULL, NULL, 20, 40,
  "FUTURE", "gmean", NULL, 1, 20, 55,
)

eval_experiments <- experiments[2, ] %>%
  dplyr::rowwise() %>%
  mutate(result = list(get_MSE(
    data = rh_data,
    prediction_data = data_all,
    train_names = train_names,
    test_names = test_names,
    max_neighbors = 80,
    cm_method = cm_method,
    dm_norm = dm_norm,
    constant = constant,
    missing_cost = missing_cost,
    min_age = min_age,
    max_age = max_age
  )))

## Expand the results in a tidy tbl with unnest:
# eval_experiments %>% tidyr::unnest(result)

# example2 <- get_MSE(data = rh_data,
#                     prediction_data = data_all,
#                     tain_names = tain_names,
#                     test_names = test_names,
#                     max_neighbors = 80,
#                     cm_method = "TRATE",
#                     dm_norm = "maxlength",
#                     min_age = 20,
#                     max_age = 40)
#
# rownames_all <- rownames(seq_data)
# rownames_age <- rownames(seq_data)
# issues <- rownames_all[!rownames_all %in% rownames_age]

my_exp <- eval_experiments %>%
  tibble::add_column(Experiment = 1:11) %>%
  select(Experiment, result) %>%
  tidyr::unnest(result) %>%
  group_by(Experiment, Score) %>%
  slice_min(order_by = MSE, n = 1) %>%
  ungroup() %>%
  left_join(trivial_MSE, by = "Score") %>%
  mutate(Improvement = (1 - (MSE.x / MSE.y)) * 100)

ggplot(my_exp, aes(Experiment, Score)) +
  geom_tile(aes(fill = Improvement)) +
  geom_text(aes(label = paste(round(Improvement, 2), paste0("k = ", k), sep = "\n"))) +
  scale_fill_gradient(low = "white", high = "red") +
  scale_x_continuous(breaks = 1:11)

my_exp2 <- eval_experiments %>%
  tibble::add_column(Experiment = 1:10) %>%
  select(Experiment, result) %>%
  tidyr::unnest(result) %>%
  filter(Score == "Neuroticism") %>%
  arrange(MSE)

example2 <- get_MSE(
  data = rh_data,
  prediction_data = data_all,
  tain_names = tain_names,
  test_names = test_names,
  max_neighbors = 80,
  cm_method = "TRATE",
  dm_norm = "maxlength",
  min_age = 20,
  max_age = 40
)
