predict_scores <- function(data, neighbors){
  predictions <- data %>% 
    filter(Id %in% neighbors) %>% 
    summarise(Extraversion = mean(BFI_extraversion_mean, na.rm = TRUE),
              Agreeableness = mean(BFI_agreeableness_mean, na.rm = TRUE),
              Conscientiousness = mean(BFI_conscientiousness_mean, na.rm = TRUE),
              Neuroticism = mean(BFI_neuroticism_mean, na.rm = TRUE),
              Openness = mean(BFI_openness_mean, na.rm = TRUE)) %>% 
    as.vector() %>% 
    unlist()
  return(predictions)
}