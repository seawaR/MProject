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
  max_neighbors = 20,
  cm_method = "TRATE",
  dm_norm = "maxlength",
  min_age = 20,
  max_age = 40)

###################

missing_costs <- seq(from = 0, to = 2, by = 0.1)

exp2 <- tibble(cm_method = "FUTURE",
               dm_norm = "gmean",
               missing_cost = missing_costs)

eval_experiments2 <- exp2 %>%
  dplyr::rowwise() %>%
  mutate(result = list(get_MSE(
    data = rh_data,
    prediction_data = data_all,
    train_names = train_names,
    test_names = test_names,
    max_neighbors = 80,
    cm_method = cm_method,
    dm_norm = dm_norm,
    missing_cost = missing_cost,
  )))

my_exp2 <- eval_experiments2 %>%
  tibble::add_column(Experiment = 1:21) %>%
  select(Experiment, result) %>%
  tidyr::unnest(result) %>%
  group_by(Experiment, Score) %>%
  slice_min(order_by = MSE, n = 1) %>%
  ungroup() %>%
  left_join(trivial_MSE, by = "Score") %>%
  mutate(Improvement = (1 - (MSE.x / MSE.y)) * 100)

ggplot(my_exp2, aes(Experiment, Score)) +
  geom_tile(aes(fill = Improvement)) +
  geom_text(aes(label = paste(round(Improvement, 2), paste0("k = ", k), sep = "\n"))) +
  scale_fill_gradient(low = "red", high = "green") +
  scale_x_continuous(breaks = 1:21)

########################

sa<- seq(from = 15, to = 22)

ea<- seq(from = 40, to = 60)

ea2<- c(43:46, 54:60)

combs <- expand.grid(sa, ea)

exp3 <- tibble(cm_method = "FUTURE",
               dm_norm = "gmean",
               max_age = ea)

eval_experiments3 <- exp3 %>%
  dplyr::rowwise() %>%
  mutate(result = list(get_MSE(
    data = rh_data,
    prediction_data = data_all,
    train_names = train_names,
    test_names = test_names,
    max_neighbors = 50,
    cm_method = cm_method,
    dm_norm = dm_norm,
    max_age = max_age
  )))

my_exp3 <- eval_experiments3 %>%
  tibble::add_column(Experiment = 1:21) %>%
  select(Experiment, result) %>%
  tidyr::unnest(result) %>%
  group_by(Experiment, Score) %>%
  slice_min(order_by = MSE, n = 1) %>%
  ungroup() %>%
  left_join(trivial_MSE, by = "Score") %>%
  mutate(Improvement = (1 - (MSE.x / MSE.y)) * 100)

ggplot(my_exp3, aes(Experiment, Score)) +
  geom_tile(aes(fill = Improvement)) +
  geom_text(aes(label = paste(round(Improvement, 2), paste0("k = ", k), sep = "\n"))) +
  scale_fill_gradient(low = "red", high = "green") +
  scale_x_continuous(breaks = 1:21)


######################################


sa<- seq(from = 15, to = 22)

ea<- seq(from = 40, to = 60)

ea2<- c(43:46, 54:60)

combs <- expand.grid(sa, ea)

exp4 <- tibble(cm_method = "FUTURE",
               dm_norm = "gmean",
               min_age = sa)

eval_experiments4 <- exp4 %>%
  dplyr::rowwise() %>%
  mutate(result = list(get_MSE(
    data = rh_data,
    prediction_data = data_all,
    train_names = train_names,
    test_names = test_names,
    max_neighbors = 50,
    cm_method = cm_method,
    dm_norm = dm_norm,
    min_age = min_age
  )))

my_exp4 <- eval_experiments4 %>%
  tibble::add_column(Experiment = 1:8) %>%
  select(Experiment, result) %>%
  tidyr::unnest(result) %>%
  group_by(Experiment, Score) %>%
  slice_min(order_by = MSE, n = 1) %>%
  ungroup() %>%
  left_join(trivial_MSE, by = "Score") %>%
  mutate(Improvement = (1 - (MSE.x / MSE.y)) * 100)

ggplot(my_exp4, aes(Experiment, Score)) +
  geom_tile(aes(fill = Improvement)) +
  geom_text(aes(label = paste(round(Improvement, 2), paste0("k = ", k), sep = "\n"))) +
  scale_fill_gradient(low = "red", high = "green") +
  scale_x_continuous(breaks = 1:8)


######################################


sa <- c(15, 19:21)

ea <- c(43:46, 54:60)

combs <- expand.grid(sa, ea)

exp5 <- tibble(cm_method = "FUTURE",
               dm_norm = "gmean",
               min_age = combs[1:22, 1],
               max_age = combs[1:22, 2])

eval_experiments5 <- exp5 %>%
  dplyr::rowwise() %>%
  mutate(result = list(get_MSE(
    data = rh_data,
    prediction_data = data_all,
    train_names = train_names,
    test_names = test_names,
    max_neighbors = 50,
    cm_method = cm_method,
    dm_norm = dm_norm,
    min_age = min_age,
    max_age = max_age
  )))

my_exp5 <- eval_experiments5 %>%
  tibble::add_column(Experiment = 1:22) %>%
  select(Experiment, result) %>%
  tidyr::unnest(result) %>%
  group_by(Experiment, Score) %>%
  slice_min(order_by = MSE, n = 1) %>%
  ungroup() %>%
  left_join(trivial_MSE, by = "Score") %>%
  mutate(Improvement = (1 - (MSE.x / MSE.y)) * 100)

ggplot(my_exp5, aes(Experiment, Score)) +
  geom_tile(aes(fill = Improvement)) +
  geom_text(aes(label = paste(round(Improvement, 2), paste0("k = ", k), sep = "\n"))) +
  scale_fill_gradient(low = "red", high = "green") +
  scale_x_continuous(breaks = 1:8)

exp6 <- tibble(cm_method = "FUTURE",
               dm_norm = "gmean",
               min_age = combs[23:44, 1],
               max_age = combs[23:44, 2])

eval_experiments6 <- exp6 %>%
  dplyr::rowwise() %>%
  mutate(result = list(get_MSE(
    data = rh_data,
    prediction_data = data_all,
    train_names = train_names,
    test_names = test_names,
    max_neighbors = 50,
    cm_method = cm_method,
    dm_norm = dm_norm,
    min_age = min_age,
    max_age = max_age
  )))

my_exp6 <- eval_experiments6 %>%
  tibble::add_column(Experiment = 1:22) %>%
  select(Experiment, result) %>%
  tidyr::unnest(result) %>%
  group_by(Experiment, Score) %>%
  slice_min(order_by = MSE, n = 1) %>%
  ungroup() %>%
  left_join(trivial_MSE, by = "Score") %>%
  mutate(Improvement = (1 - (MSE.x / MSE.y)) * 100)

ggplot(my_exp6, aes(Experiment, Score)) +
  geom_tile(aes(fill = Improvement)) +
  geom_text(aes(label = paste(round(Improvement, 2), paste0("k = ", k), sep = "\n"))) +
  scale_fill_gradient(low = "red", high = "green") +
  scale_x_continuous(breaks = 1:22)

my_exp5_test <- eval_experiments5 %>%
  tibble::add_column(Experiment = 1:22) %>%
  select(Experiment, result) %>%
  tidyr::unnest(result)

ggplot(my_exp5_test %>% filter(k > 3, Experiment == 12, Score == "Conscientiousness")) +
  geom_line(aes(x = k, y = MSE)) +
  geom_line(
    aes(x = k, y = Trivial, col = "red"),
    alpha = 0.5,
    show.legend = FALSE
  ) 

my_exp6_test <- my_exp6 %>% 
  mutate(Experiment = Experiment+22)

ggplot(rbind(my_exp5, my_exp6_test), aes(Experiment, Score)) +
  geom_tile(aes(fill = Improvement)) +
  geom_text(aes(label = paste(round(Improvement, 2), paste0("k = ", k), sep = "\n"))) +
  scale_fill_gradient(low = "red", high = "green") +
  scale_x_continuous(breaks = 1:44)


######################################


sa <- 20

ea <- 55

mc <- c(0.5, 1)

combs2 <- expand.grid(sa, ea, mc)

exp7 <- tibble(cm_method = "FUTURE",
               dm_norm = "gmean",
               min_age = combs2[, 1],
               max_age = combs2[, 2],
               missing_cost = combs2[, 3])

eval_experiments7 <- exp7 %>%
  dplyr::rowwise() %>%
  mutate(result = list(get_MSE(
    data = rh_data,
    prediction_data = data_all,
    train_names = train_names,
    test_names = test_names,
    max_neighbors = 80,
    cm_method = cm_method,
    dm_norm = dm_norm,
    min_age = min_age,
    max_age = max_age,
    missing_cost = missing_cost
  )))

my_exp7 <- eval_experiments7 %>%
  filter(min_age < 21) %>% 
  arrange(missing_cost, max_age, min_age) %>% 
  tibble::add_column(Experiment = 1:30) %>% 
  select(Experiment, result) %>%
  tidyr::unnest(result) %>%
  group_by(Experiment, Score) %>%
  slice_min(order_by = MSE, n = 1) %>%
  ungroup() %>%
  left_join(trivial_MSE, by = "Score") %>%
  mutate(Improvement = (1 - (MSE.x / MSE.y)) * 100)

ggplot(my_exp7, aes(Experiment, Score)) +
  geom_tile(aes(fill = Improvement)) +
  geom_text(aes(label = paste(round(Improvement, 2), paste0("k = ", k), sep = "\n"))) +
  scale_fill_gradient(low = "red", high = "green") +
  scale_x_continuous(breaks = 1:45)

#######################

exps <- tibble::tribble(
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
  "FUTURE", "gmean", NULL, 0.5, 20, 55,
)

eval_exps <- exps %>%
  dplyr::rowwise() %>%
  mutate(result = list(get_MSE(
    data = rh_data,
    prediction_data = data_all,
    train_names = train_names,
    test_names = test_names,
    max_neighbors = 100,
    cm_method = cm_method,
    dm_norm = dm_norm,
    min_age = min_age,
    max_age = max_age,
    missing_cost = missing_cost
  )))

my_exps <- eval_exps %>%
  tibble::add_column(Experiment = 1:12) %>% 
  select(Experiment, result) %>%
  tidyr::unnest(result) %>%
  group_by(Experiment, Score) %>%
  slice_min(order_by = MSE, n = 1) %>%
  ungroup() %>%
  left_join(trivial_MSE, by = "Score") %>%
  mutate(Improvement = (1 - (MSE.x / MSE.y)) * 100)

ggplot(my_exps, aes(Experiment, Score)) +
  geom_tile(aes(fill = Improvement)) +
  geom_text(aes(label = paste(round(Improvement, 2), paste0("k = ", k), sep = "\n"))) +
  scale_fill_gradient(low = "red", high = "green") +
  scale_x_continuous(breaks = 1:12)


##########################

seqdplot(results$sd, sortv = "from.start", ltext = status_labels, border = NA)

qs <- quantile(temp$BFI_neuroticism_mean, probs = c(0.2, 0.8))
seqplot.tentrop(res, group = temp$Neuroticism_level, ylim = c(0, 1))

temp <- data_all %>% 
  filter(Id %in% rownames(res)) %>% 
  arrange(factor(Id, levels = rownames(res)))

res <- results$sd[rownames(results$sd) %in% data_all$Id,]

temp <- temp %>% 
  mutate(Neuroticism_level = case_when(BFI_neuroticism_mean < qs[1] ~ "Low",
                                       BFI_neuroticism_mean < qs[2] ~ "High",
                                       TRUE ~ "Medium"))
