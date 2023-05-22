library(tidyverse)
library(TraMineR)
source("predict_scores.R")

get_MSE <- function(data,
                    prediction_data,
                    tain_names, 
                    test_names, 
                    max_neighbors, 
                    cm_method, 
                    dm_norm, 
                    constant = NULL, 
                    missing_cost = NULL,
                    min_age = NULL, 
                    max_age = NULL){
  
  if (!is.null(min_age)){
    data <- data %>% 
      filter(Start_age >= min_age)
  }
  
  if (!is.null(max_age)){
    data <- data %>% 
      filter(End_age <= max_age)
  }
  
  seq_data <- seqformat(data, 
                        from = "SPELL", 
                        to = "STS",
                        id = "Id", 
                        begin = "Start_age", 
                        end = "End_age",
                        status = "Status", 
                        covar = "Age", 
                        process = FALSE)
  
  alphabet <- as.character(1:length(unique(data$Status)))
  
  my_seq <- seqdef(data = seq_data, 
                   alphabet = alphabet)
  
  if (!is.null(missing_cost)){
    cost_matrix <- seqcost(seqdata = my_seq, 
                           method = cm_method,
                           with.missing = TRUE,
                           cval = constant,
                           miss.cost.fixed = TRUE,
                           miss.cost = missing_cost)
  } else{
    cost_matrix <- seqcost(seqdata = my_seq, 
                           method = cm_method,
                           with.missing = TRUE,
                           cval = constant)
  }
  
  my_dist <- seqdist(seqdata = my_seq, 
                     method = "OM", 
                     sm = cost_matrix$sm, 
                     indel = cost_matrix$indel,
                     with.missing = TRUE, 
                     norm = dm_norm)
  
  results_test <- list()
  
  for(k in 1:max_neighbors){
    all_neighbors_test <- list()
    
    for(i in seq_along(test_names)){
      idx <- c(test_names[i], train_names)
      neighbors <- k.nearest.neighbors(i = 1, 
                                       distance_matrix = my_dist[idx, idx], 
                                       k = k) - 1
      all_neighbors_test[[i]] <- train_names[neighbors]
    }
    
    names(all_neighbors_test) <- test_names
    
    all_predictions_test <- list()
    
    for(i in seq_along(test_names)){
      all_predictions_test[[i]] <- predict_scores(data_all, neighbors = all_neighbors_test[[i]])
    }
    
    names(all_predictions_test) <- test_names
    
    predictions_test_df <- do.call(rbind.data.frame, all_predictions_test)
    
    colnames(predictions_test_df) <- names(all_predictions_test[[1]])
    
    predictions_test_df[, "Id"] <- test_names
    
    sq_error_test <- predictions_test_df %>% 
      left_join(prediction_data, by = "Id") %>% 
      mutate(sq_diff_Extraversion = (Extraversion - BFI_extraversion_mean)^2,
             sq_diff_Agreeableness = (Agreeableness - BFI_agreeableness_mean)^2,
             sq_diff_Conscientiousness = (Conscientiousness - BFI_conscientiousness_mean)^2,
             sq_diff_Neuroticism = (Neuroticism - BFI_neuroticism_mean)^2,
             sq_diff_Openness = (Openness - BFI_openness_mean)^2) %>% 
      select(Id, Cluster_5, Cluster_2, starts_with("sq_diff")) %>% 
      pivot_longer(starts_with("sq_diff"), names_to = "Score", values_to = "Value") %>% 
      group_by(Score) %>% 
      summarise(MSQ = mean(Value, na.rm = TRUE)) %>% 
      ungroup()
    
    colnames(sq_error_test)[2] <- paste0("MSQ_", k)
    
    sq_error_test[, 1] <- c("Agreeableness", "Conscientiousness",
                            "Extraversion", "Neuroticism", "Openness")
    
    results_test[[k]] <- sq_error_test
  }
  
  MSE_df <- results_test %>% 
    purrr::reduce(left_join, by = "Score") %>% 
    pivot_longer(starts_with("MSQ_"), names_prefix = "MSQ_", names_to = "k", 
                 values_to = "MSE") %>% 
    mutate(k = as.numeric(k))
  
  return(MSE_df)
}

experiments <- tibble::tribble(
   ~cm_method,    ~dm_norm, ~constant, ~missing_cost, ~min_age, ~max_age,
      "TRATE", "maxlength",      NULL,          NULL,     NULL,     NULL,
      "TRATE", "maxlength",    1.9982,          NULL,     NULL,     NULL,
      "TRATE",     "gmean",      NULL,          NULL,     NULL,     NULL,
     "FUTURE", "maxlength",      NULL,          NULL,     NULL,     NULL,
     "INDELS", "maxlength",      NULL,          NULL,     NULL,     NULL,
  "INDELSLOG", "maxlength",      NULL,          NULL,     NULL,     NULL,
     "FUTURE",     "gmean",      NULL,          NULL,     NULL,     NULL,
     "FUTURE",     "gmean",      NULL,             1,     NULL,     NULL,
#     "FUTURE",     "gmean",      NULL,          NULL,       20,       55,
#     "FUTURE",     "gmean",      NULL,          NULL,       20,       40,
)

eval_experiments <- experiments %>%
  dplyr::rowwise() %>%
  mutate(result = list(get_MSE(
    data = rh_data,
    prediction_data = data_all,
    tain_names = tain_names, 
    test_names = test_names,
    max_neighbors = 80,
    cm_method = cm_method, 
    dm_norm = dm_norm,
    constant = constant,
    missing_cost = missing_cost,
    min_age = min_age,
    max_age = max_age)))

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
#                     max_age = 55)
# 
# rownames_all <- rownames(seq_data)
# rownames_age <- rownames(seq_data)
# issues <- rownames_all[!rownames_all %in% rownames_age]

my_exp <- eval_experiments %>% 
  tibble::add_column(Experiment = 1:8) %>% 
  select(Experiment, result) %>% 
  tidyr::unnest(result) %>% 
  group_by(Experiment, Score) %>% 
  summarise(min_MSE = min(MSE)) %>% 
  ungroup() %>% 
  left_join(trivial_MSE, by = "Score") %>% 
  mutate(Improvement = (1-(min_MSE/MSE))*100)

ggplot(my_exp, aes(Experiment, Score)) +
  geom_tile(aes(fill = Improvement)) +
  geom_text(aes(label = round(Improvement, 2))) +
  scale_fill_gradient(low = "white", high = "red") +
  scale_x_continuous(breaks = 1:8)
