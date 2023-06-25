get_MSE <- function(data,
                    prediction_data,
                    train_names,
                    test_names,
                    max_neighbors,
                    cm_method,
                    dm_norm,
                    constant = NULL,
                    missing_cost = NULL,
                    min_age = NULL,
                    max_age = NULL) {
  dist_matrix <- distance_matrix(
    data = data,
    cm_method = cm_method,
    dm_norm = dm_norm,
    constant = constant,
    missing_cost = missing_cost,
    min_age = min_age,
    max_age = max_age
  )

  train_names <- train_names[train_names %in% colnames(dist_matrix)]

  test_names <- test_names[test_names %in% colnames(dist_matrix)]

  results_test <- list()

  for (k in 1:max_neighbors) {
    all_neighbors_test <- list()

    for (i in seq_along(test_names)) {
      idx <- c(test_names[i], train_names)
      neighbors <- FastKNN::k.nearest.neighbors(
        i = 1,
        distance_matrix = dist_matrix[idx, idx],
        k = k
      ) - 1
      all_neighbors_test[[i]] <- train_names[neighbors]
    }

    names(all_neighbors_test) <- test_names

    all_predictions_test <- list()

    for (i in seq_along(test_names)) {
      all_predictions_test[[i]] <- predict_scores(data_all, neighbors = all_neighbors_test[[i]])
    }

    names(all_predictions_test) <- test_names

    predictions_test_df <- do.call(rbind.data.frame, all_predictions_test)

    colnames(predictions_test_df) <- names(all_predictions_test[[1]])

    predictions_test_df[, "Id"] <- test_names

    sq_error_test <- predictions_test_df %>%
      dplyr::left_join(prediction_data, by = "Id") %>%
      dplyr::mutate(
        sq_diff_Extraversion = (Extraversion - BFI_extraversion_mean)^2,
        sq_diff_Agreeableness = (Agreeableness - BFI_agreeableness_mean)^2,
        sq_diff_Conscientiousness = (Conscientiousness - BFI_conscientiousness_mean)^2,
        sq_diff_Neuroticism = (Neuroticism - BFI_neuroticism_mean)^2,
        sq_diff_Openness = (Openness - BFI_openness_mean)^2
      ) %>%
      dplyr::select(Id, Cluster_4, Cluster_2, starts_with("sq_diff")) %>%
      tidyr::pivot_longer(starts_with("sq_diff"), names_to = "Score", values_to = "Value") %>%
      dplyr::group_by(Score) %>%
      dplyr::summarise(MSQ = mean(Value, na.rm = TRUE)) %>%
      dplyr::ungroup()

    colnames(sq_error_test)[2] <- paste0("MSQ_", k)

    sq_error_test[, 1] <- c(
      "Agreeableness", "Conscientiousness",
      "Extraversion", "Neuroticism", "Openness"
    )

    results_test[[k]] <- sq_error_test
  }

  data_long <- data_all %>%
    tidyr::pivot_longer(
      cols = starts_with("BFI"), names_to = "Trait",
      names_prefix = "BFI_", values_to = "Trait_value"
    ) %>%
    dplyr::mutate(
      Trait = case_when(
        Trait == "extraversion_mean" ~ "Extraversion",
        Trait == "agreeableness_mean" ~ "Agreeableness",
        Trait == "conscientiousness_mean" ~ "Conscientiousness",
        Trait == "neuroticism_mean" ~ "Neuroticism",
        Trait == "openness_mean" ~ "Openness",
        TRUE ~ "other"
      ),
      Cluster_4 = sub("Cluster ", "", Cluster_4),
      Cluster_2 = sub("Cluster ", "", Cluster_2)
    )

  train_means <- data_long %>%
    dplyr::filter(Id %in% train_names) %>%
    dplyr::rename(Score = Trait) %>%
    dplyr::group_by(Score) %>%
    dplyr::summarise(Average = mean(Trait_value)) %>%
    dplyr::ungroup()

  trivial_MSE <- data_long %>%
    dplyr::rename(Score = Trait) %>%
    dplyr::filter(Id %in% test_names) %>%
    dplyr::left_join(train_means, by = "Score") %>%
    dplyr::mutate(sq_difference = (Trait_value - Average)^2) %>%
    dplyr::group_by(Score) %>%
    dplyr::summarise(Trivial = mean(sq_difference)) %>%
    dplyr::ungroup()

  MSE_df <- results_test %>%
    purrr::reduce(left_join, by = "Score") %>%
    tidyr::pivot_longer(starts_with("MSQ_"),
      names_prefix = "MSQ_", names_to = "k",
      values_to = "MSE"
    ) %>%
    dplyr::mutate(k = as.numeric(k)) %>%
    dplyr::left_join(trivial_MSE, by = "Score")

  return(MSE_df)
}
