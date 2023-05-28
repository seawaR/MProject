## ----setup, include=FALSE------------------------------
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE)


## ----libs, echo = FALSE--------------------------------
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(TraMineR)
library(cluster)
library(FastKNN)
library(TraMineRextras)


## ----data, echo = FALSE, cache=TRUE--------------------
cs_levels <- 1:6
cs_labels <- c("Single", "Married", "Registered partnership", 
               "Same-sex partnership", "Divorced", "Widowed")

rs_levels <- 1:4
rs_labels <- c("No relationship", "Relationship", 
               "Open relationship", "Changing relationships")

liv_levels <- 0:2
liv_labels <- c("No partner", "Yes", "No")

file <- "../Data_original/Tracker_Uni-Zurich_Modified.xlsx"

raw_data <- read_excel(file)

raw_data <- raw_data %>% 
  select_if(~!all(is.na(.)))

new_names <- vector(mode = "character")

for (i in 1:12){
  new_names[2*(i-1)+1] <- paste0("Start-", i)
  new_names[2*(i-1)+2] <- paste0("End-", i)
}

colnames(raw_data)[1] <- "Id"
colnames(raw_data)[58] <- "Age"
colnames(raw_data)[59:82] <- new_names
colnames(raw_data)[83:94] <- paste0("Civil-", 1:12)
colnames(raw_data)[95:106] <- paste0("Relationship-", 1:12)
colnames(raw_data)[119:130] <- paste0("Children-", 1:12)
colnames(raw_data)[131:142] <- paste0("Living-", 1:12)

raw_data <- raw_data %>% 
  select(c(1, 58:106, 119:142)) 

raw_data <- raw_data %>% 
  pivot_longer(cols = starts_with("Start"),
               names_to = "Phase",
               names_prefix = "Start-",
               values_to = "Start_age",
               values_drop_na = TRUE) %>% 
  pivot_longer(cols = starts_with("End"),
               names_to = "Match2",
               names_prefix = "End-",
               values_to = "End_age",
               values_drop_na = TRUE) %>% 
  pivot_longer(cols = starts_with("Civil"),
               names_to = "Match3",
               names_prefix = "Civil-",
               values_to = "Civil_status",
               values_drop_na = TRUE) %>% 
  pivot_longer(cols = starts_with("Relationship"),
               names_to = "Match4",
               names_prefix = "Relationship-",
               values_to = "Relationship_status",
               values_drop_na = TRUE) %>% 
  pivot_longer(cols = starts_with("Children"),
               names_to = "Match5",
               names_prefix = "Children-",
               values_to = "Children",
               values_drop_na = TRUE) %>% 
  pivot_longer(cols = starts_with("Living"),
               names_to = "Match6",
               names_prefix = "Living-",
               values_to = "Living_situation",
               values_drop_na = TRUE) %>% 
  filter(Phase == Match2,
         Phase == Match3,
         Phase == Match4,
         Phase == Match5,
         Phase == Match6,
         Start_age >= 15,
         !Id %in% c("zr34u", "EN61O", "DN15U"),
         !(Id == "GB28U" & Phase == "4")) %>% 
  select(-contains("Match")) %>% 
  mutate(Civil_status = factor(Civil_status, levels = cs_levels, labels = cs_labels),
         Relationship_status = factor(Relationship_status, levels = rs_levels, labels = rs_labels),
         Phase = as.numeric(Phase),
         Living_situation = factor(Living_situation, levels = liv_levels, labels = liv_labels),
         Children = case_when(
           Children == 0 ~ "No",
           is.na(Children) ~ "No",
           Children > 0 ~ "Yes"
         )
         )

tst <- raw_data %>% 
  mutate(
    Status= case_when(
      Relationship_status == "No relationship" & Civil_status == "Single" & Children == "No" ~ 1,
      Relationship_status == "No relationship" & Civil_status == "Single" & Children == "Yes" ~ 2,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Single" & Children == "No" & Living_situation == "No" ~ 5,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Single" & Children == "No" & Living_situation == "Yes"  ~ 6,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Single" & Children == "Yes" & Living_situation == "No" ~ 7,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Single" & Children == "Yes"  & Living_situation == "Yes" ~ 8,
      Relationship_status == "Changing relationships" & Civil_status == "Single" & Children == "No" ~ 3,
      Relationship_status == "Changing relationships" & Civil_status == "Single" & Children == "Yes" ~ 4,
      Civil_status %in% c("Married", "Registered partnership") & Children == "No" ~ 9,
      Civil_status %in% c("Married", "Registered partnership") & Children == "Yes" ~ 10,
      Relationship_status == "No relationship" & Civil_status == "Divorced" & Children == "No" ~ 1,
      Relationship_status == "No relationship" & Civil_status == "Divorced" & Children == "Yes" ~ 5,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Divorced" & Children == "No" & Living_situation == "No" ~ 5,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Divorced" & Children == "No" & Living_situation == "Yes" ~ 6,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Divorced" & Children == "Yes" & Living_situation == "No" ~ 7,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Divorced" & Children == "Yes" & Living_situation == "Yes" ~ 8,
      Relationship_status == "Changing relationships" & Civil_status == "Divorced" & Children == "No" ~ 3,
      Relationship_status == "Changing relationships" & Civil_status == "Divorced" & Children == "Yes" ~ 4,
      Relationship_status == "No relationship" & Civil_status == "Widowed" & Children == "No" ~ 1,
      Relationship_status == "No relationship" & Civil_status == "Widowed" & Children == "Yes" ~ 2,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Widowed" & Children == "No" & Living_situation == "No" ~ 5,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Widowed" & Children == "No" & Living_situation == "Yes" ~ 6,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Widowed" & Children == "Yes" & Living_situation == "No" ~ 7,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Widowed" & Children == "Yes" & Living_situation == "Yes" ~ 8,
      Relationship_status == "Changing relationships" & Civil_status == "Widowed" & Children == "No" ~ 3,
      Relationship_status == "Changing relationships" & Civil_status == "Widowed" & Children == "Yes" ~ 4,
      TRUE ~ 0
      )
    )                

status_levels <- 1:10
status_labels <- c("Single+no ch.",
                   "Single+ch.",
                   "Changing rel.+no ch.",
                   "Changing rel.+ch.",
                   "Rel.+apart+no ch.",
                   "Rel.+together+no ch.",
                   "Rel.+apart+ch.",
                   "Rel.+together+ch.",
                   "Married+no ch.",
                   "Married+ch.")

rh_data <- tst %>% 
  select(-Civil_status, -Relationship_status, -Children) %>% 
  mutate(Status_char = factor(Status, levels = status_levels, labels = status_labels))


## ----cost-matrix, echo=FALSE, message=FALSE------------
test <- seqformat(rh_data, from = "SPELL", to = "STS",
                  id = "Id", begin = "Start_age", end = "End_age", 
                  status = "Status", covar = "Age", process = FALSE)

alphabet <- as.character(1:10)

my_seq <- seqdef(test, alphabet = alphabet)

cost_matrix_1 <- seqsubm(my_seq, method = "TRATE", with.missing = TRUE)

cm <- cbind(1:10, data.frame(cost_matrix_1[1:10,1:10]))
colnames(cm) <- c("Status", 1:10)
knitr::kable(cm, digits = 4)


## ---- echo=FALSE, message=FALSE------------------------
my_dist <- seqdist(my_seq, method = "OM", sm = cost_matrix_1, 
                   with.missing = TRUE, norm = "maxlength")


## ---- echo=FALSE, message=FALSE------------------------
clusterward <- agnes(my_dist, diss = TRUE, method = "ward")


## ---- out.width="400px", fig.align="center"------------
plot(clusterward, which.plots = 2, main = "Dendrogram")
abline(h = 4.3, col = "red")


## ---- echo=FALSE, message=FALSE, cache=TRUE------------
clusters5 <- cutree(clusterward, k = 5)

clusters5_labels <- factor(clusters5, labels = paste("Cluster", 1:5))

counts <- tibble(clusters5) %>% 
  group_by(clusters5) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  rename(Cluster = clusters5)

knitr::kable(counts)


## ---- out.width = "370px", fig.align = "center"--------
#knitr::include_graphics("../Output/cluster2.png")
par(mar = c(2, 1.7, 0.75, 0.5))
seqdplot(my_seq, group = clusters5_labels, border = NA, 
         ltext = status_labels)
invisible(dev.off())


## ---- echo=FALSE, message=FALSE------------------------
BFI_data <- read_excel("../Data_original/Adriana_BFI.xlsx")

BFI_data <- BFI_data %>% 
  select(PersCode,
         starts_with("BFI"))

pers_descrip <- BFI_data %>% 
  select(PersCode,
         ends_with("mean")) %>% 
  pivot_longer(cols = ends_with("mean"), 
               names_to = "Personality trait", 
               values_to = "Value") %>% 
  group_by(`Personality trait`) %>% 
  summarise(Min = min(Value, na.rm = TRUE),
            Max = max(Value, na.rm = TRUE),
            Average = mean(Value, na.rm = TRUE),
            `Std. deviation` = sd(Value, na.rm = TRUE)) %>% 
  ungroup()

pers_descrip[, 1] <- c("Agreeableness", "Conscientiousness",
                       "Extraversion", "Neuroticism", "Openness")

# knitr::kable(pers_descrip, digits = 2) %>% 
#   kableExtra::kable_styling(font_size = 8)


## ---- echo=FALSE, message=FALSE, cache=TRUE------------
clusters2 <- cutree(clusterward, k = 2)

clusters2_labels <- factor(clusters2, labels = paste("Cluster", 1:2))


## ---- echo=FALSE, message=FALSE------------------------
data_all <- tibble(Id = colnames(my_dist), Cluster_5 = clusters5_labels,
                   Cluster_2 = clusters2_labels) %>% 
  left_join(BFI_data, by = c("Id" = "PersCode")) %>% 
  select(Id, Cluster_5, Cluster_2, ends_with("mean")) %>% 
  filter(!is.na(BFI_extraversion_mean))

data_all_long <- data_all %>% 
  pivot_longer(cols = starts_with("BFI"), names_to = "Trait", 
               names_prefix = "BFI_", values_to = "Trait_value") %>% 
  mutate(Trait = case_when(Trait == "extraversion_mean" ~ "Extraversion",
                           Trait == "agreeableness_mean" ~ "Agreeableness",
                           Trait == "conscientiousness_mean" ~ "Conscientiousness",
                           Trait == "neuroticism_mean" ~ "Neuroticism",
                           Trait == "openness_mean" ~ "Openness",
                           TRUE ~ "other"),
         Cluster_5 = sub("Cluster ", "", Cluster_5),
         Cluster_2 = sub("Cluster ", "", Cluster_2))

# by_cluster <- data_all %>% 
#   group_by(Cluster) %>% 
#   summarise(Extraversion = mean(BFI_extraversion_mean, na.rm = TRUE),
#             Agreeableness = mean(BFI_agreeableness_mean, 
#                                          na.rm = TRUE),
#             Conscientiousness = mean(BFI_conscientiousness_mean,
#                                              na.rm = TRUE),
#             Neuroticism = mean(BFI_neuroticism_mean, na.rm = TRUE),
#             Openness = mean(BFI_openness_mean, na.rm = TRUE)) %>% 
#   ungroup()
# 
# knitr::kable(by_cluster, digits = 2) %>% 
#   kableExtra::kable_styling(font_size = 7)


## ---- out.width="350px", fig.align="center"------------
p <- ggplot(data_all_long, aes(x = Trait_value, after_stat(density))) 
p + geom_histogram(bins = 12) + facet_grid(rows = vars(Cluster_5), 
                                           cols = vars(Trait),
                                           scales = "free_x")


## ---- echo=FALSE, message=FALSE, cache=TRUE------------
counts2 <- tibble(clusters2) %>% 
  group_by(clusters2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  rename(Cluster = clusters2)

knitr::kable(counts2)


## ---- out.width = "350px", fig.align = "center"--------
seqdplot(my_seq, group = clusters2_labels, border = NA, 
         ltext = status_labels, with.legend = "right")
invisible(dev.off())


## ---- out.width="350px", fig.align="center"------------
seqplot.tentrop(my_seq, group = clusters2_labels, ylim=c(0,1))


## ---- out.width="350px", fig.align="center"------------
p <- ggplot(data_all_long, aes(x = Trait_value, after_stat(density))) 
p + geom_histogram(bins = 10) + facet_grid(rows = vars(Cluster_2), 
                                           cols = vars(Trait),
                                           scales = "free_x")


## ---- echo=FALSE, message=FALSE, cache=TRUE------------
set.seed(123)
n <- dim(data_all)[1]
size <- round(n*0.3)
max_neighbors <- 80

test_set <- sample.int(n, size)
train_set <- 1:n
train_set <- train_set[-test_set]

test_names <- colnames(my_dist)[test_set]
train_names <- colnames(my_dist)[train_set]

preditc_scores <- function(data, neighbors){
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

results_test <- list()

for(k in 1:max_neighbors){
all_neighbors_test <- list()

for(i in seq_along(test_set)){
  idx <- c(test_set[i], train_set)
  neighbors <- k.nearest.neighbors(i = 1, 
                                   distance_matrix = my_dist[idx, idx], 
                                   k = k) - 1
  all_neighbors_test[[i]] <- train_names[neighbors]
}

names(all_neighbors_test) <- test_names

all_predictions_test <- list()

for(i in seq_along(test_set)){
  all_predictions_test[[i]] <- preditc_scores(data_all, neighbors = all_neighbors_test[[i]])
}

names(all_predictions_test) <- test_names

predictions_test_df <- do.call(rbind.data.frame, all_predictions_test)

colnames(predictions_test_df) <- names(all_predictions_test[[1]])

predictions_test_df[, "Id"] <- test_names

sq_error_test <- predictions_test_df %>% 
  left_join(data_all, by = "Id") %>% 
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

test_MSQ <- results_test %>% 
  purrr::reduce(left_join, by = "Score") %>% 
  pivot_longer(starts_with("MSQ_"), names_prefix = "MSQ_", names_to = "k", 
               values_to = "MSE") %>% 
  mutate(k = as.numeric(k))


## ---- echo=FALSE, message=FALSE, out.width = "400px", fig.align = "center"----
ggplot(data = test_MSQ, aes(x = k, y = MSE, col = Score)) +
  geom_line() + ylim(0.3, 1)


## ------------------------------------------------------
train_means <- data_all_long %>% 
  filter(Id %in% train_names) %>% 
  rename(Score = Trait) %>% 
  group_by(Score) %>% 
  summarise(Average = mean(Trait_value)) %>% 
  ungroup()

trivial_MSE <- data_all_long %>% 
  rename(Score = Trait) %>% 
  filter(Id %in% test_names) %>% 
  left_join(train_means, by = "Score") %>% 
  mutate(sq_difference = (Trait_value - Average)^2) %>%
  group_by(Score) %>% 
  summarise(MSE = mean(sq_difference)) %>% 
  ungroup()
  
ggplot(test_MSQ %>% filter(k>5)) + 
  geom_line(aes(x = k, y = MSE)) +
  geom_hline(data = trivial_MSE, 
             aes(yintercept = MSE, col = "red"), 
             alpha = 0.5,
             show.legend = FALSE) +
  facet_grid(rows = vars(Score), scales = "free")


## ---- out.width = "400px", fig.align = "center"--------
knitr::include_graphics("../Output/exp1a.pdf")


## ---- out.width = "400px", fig.align = "center"--------
knitr::include_graphics("../Output/exp2a.pdf")


## ---- out.width = "400px", fig.align = "center"--------
knitr::include_graphics("../Output/exp3a.pdf")


## ---- out.width = "400px", fig.align = "center"--------
knitr::include_graphics("../Output/exp4a.pdf")


## ---- out.width = "400px", fig.align = "center"--------
knitr::include_graphics("../Output/exp5a.pdf")


## ---- out.width = "400px", fig.align = "center"--------
knitr::include_graphics("../Output/exp6a.pdf")


## ---- out.width = "400px", fig.align = "center"--------
knitr::include_graphics("../Output/exp7a.pdf")

