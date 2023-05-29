library(tidyverse)
library(TraMineR)

distance_matrix <- function(data,
                            cm_method,
                            dm_norm,
                            constant = NULL,
                            missing_cost = NULL,
                            min_age = NULL,
                            max_age = NULL,
                            cm_out = FALSE) {
  if (!is.null(min_age)) {
    data <- data %>%
      filter(Start_age >= min_age)
  }

  if (!is.null(max_age)) {
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
    process = FALSE
  )

  alphabet <- as.character(1:length(unique(data$Status)))

  sequences <- seqdef(
    data = seq_data,
    alphabet = alphabet
  )

  if (!is.null(missing_cost)) {
    cost_matrix <- seqcost(
      seqdata = sequences,
      method = cm_method,
      with.missing = TRUE,
      cval = constant,
      miss.cost.fixed = TRUE,
      miss.cost = missing_cost
    )
  } else {
    cost_matrix <- seqcost(
      seqdata = sequences,
      method = cm_method,
      with.missing = TRUE,
      cval = constant
    )
  }

  distance_matrix <- seqdist(
    seqdata = sequences,
    method = "OM",
    sm = cost_matrix$sm,
    indel = cost_matrix$indel,
    with.missing = TRUE,
    norm = dm_norm
  )

  if (cm_out) {
    return(list(cm = cost_matrix$cm, dm = distance_matrix))
  } else {
    return(distance_matrix)
  }
}
