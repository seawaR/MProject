distance_matrix <- function(data,
                            cm_method,
                            dm_norm,
                            constant = NULL,
                            missing_cost = NULL,
                            min_age = NULL,
                            max_age = NULL,
                            extras_out = FALSE) {
  if (!is.null(min_age)) {
    data <- data %>%
      dplyr::filter(Start_age >= min_age)
  }

  if (!is.null(max_age)) {
    data <- data %>%
      dplyr::filter(End_age <= max_age)
  }

  seq_data <- seqformat(data,
    from = "SPELL",
    to = "STS",
    id = "Id",
    begin = "Start_age",
    end = "End_age",
    status = "Status",
    covar = "Age",
    process = FALSE
  )

  alphabet <- as.character(1:length(unique(data$Status)))

  sequences <- TraMineR::seqdef(
    data = seq_data,
    alphabet = alphabet
  )

  if (!is.null(missing_cost)) {
    cost_matrix <- TraMineR::seqcost(
      seqdata = sequences,
      method = cm_method,
      with.missing = TRUE,
      cval = constant,
      miss.cost.fixed = TRUE,
      miss.cost = missing_cost
    )
  } else {
    cost_matrix <- TraMineR::seqcost(
      seqdata = sequences,
      method = cm_method,
      with.missing = TRUE,
      cval = constant
    )
  }

  distance_matrix <- TraMineR::seqdist(
    seqdata = sequences,
    method = "OM",
    sm = cost_matrix$sm,
    indel = cost_matrix$indel,
    with.missing = TRUE,
    norm = dm_norm
  )

  if (extras_out) {
    return(list(cm = cost_matrix$sm, dm = distance_matrix, sd = sequences))
  } else {
    return(distance_matrix)
  }
}
