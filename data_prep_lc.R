library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(TraMineR)

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

# raw_data %>% 
#   rename(Id = 1) %>%
#   mutate(Id = tolower(Id)) %>% 
#   group_by(Id) %>% 
#   summarise(Count = n()) %>% 
#   filter(Count > 1) %>% 
#   View(.)

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

# check children

tst_ch <- raw_data %>% 
  filter(`Children-12` < `Children-11` |
           `Children-11` < `Children-10` |
           `Children-10` < `Children-9` |
           `Children-9` < `Children-8` |
           `Children-8` < `Children-7` |
           `Children-7` < `Children-6` |
           `Children-6` < `Children-5` |
           `Children-5` < `Children-4` |
           `Children-4` < `Children-3` |
           `Children-3` < `Children-2` |
           `Children-2` < `Children-1`)

tst_ch

# check that the start and end ages make sense

tst_se <- raw_data %>% 
  filter(`Start-1` > `End-1` | 
           `Start-2` > `End-2` |
           `Start-3` > `End-3` |
           `Start-4` > `End-4` |
           `Start-5` > `End-5` |
           `Start-6` > `End-6` |
           `Start-7` > `End-7` |
           `Start-8` > `End-8` |
           `Start-9` > `End-9` |
           `Start-10` > `End-10` |
           `Start-11` > `End-11` |
           `Start-12` > `End-12`)

tst_se

# check that the phases don't overlap

tst_over <- raw_data %>% 
  filter(`End-1` > `Start-2` | 
           `End-2` > `Start-3` |
           `End-3` > `Start-4` |
           `End-4` > `Start-5` |
           `End-5` > `Start-6` |
           `End-6` > `Start-7` |
           `End-7` > `Start-8` |
           `End-8` > `Start-9` |
           `End-9` > `Start-10` |
           `End-10` > `Start-11` |
           `End-11` > `Start-12`)

tst_over

# found inconsistencies in:
# - zr34u phase 5 REMOVE
# - GB28U phase 4 REMOVE LAST PHASE

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
  
plot1 <- ggplot(data = raw_data, aes(Civil_status))
plot1 + geom_bar() + geom_text(stat="count", aes(label=..count..), vjust=-1)

ggplot(raw_data, 
       aes(x = Civil_status, 
           y = Start_age)) +
  geom_boxplot()

plot2 <- ggplot(data = raw_data, aes(Relationship_status))
plot2 + geom_bar() + geom_text(stat="count", aes(label=..count..), vjust=-1)

ggplot(raw_data, 
       aes(x = Relationship_status, 
           y = Start_age)) +
  geom_boxplot()

ggplot(raw_data, aes(x = Civil_status))+
  geom_bar(
    aes(fill = Relationship_status), stat = "count", color = "white",
    position = position_dodge(0.9)
  )

plot3 <- ggplot(data = raw_data, aes(Children))
plot3 + geom_bar() + geom_text(stat="count", aes(label=..count..), vjust=-1)

plot4 <- ggplot(data = raw_data, aes(Living_situation))
plot4 + geom_bar() + geom_text(stat="count", aes(label=..count..), vjust=-1)


# register EN61O does not make sense (phase 1: registered partnership + no relationship) REMOVE
# Only two cases of Widowed+Relationship
# Average number of phases 4.168

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

# tst <- raw_data %>% 
#   mutate(
#     Status= case_when(
#       Relationship_status == "No relationship" & Civil_status == "Single" ~ 1,
#       Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Single" ~ 2,
#       Relationship_status == "Changing relationships" & Civil_status == "Single" ~ 3,
#       Civil_status %in% c("Married", "Registered partnership") ~ 4,
#       Relationship_status == "No relationship" & Civil_status == "Divorced" ~ 5,
#       Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Divorced" ~ 6,
#       Relationship_status == "Changing relationships" & Civil_status == "Divorced" ~ 7,
#       Relationship_status == "No relationship" & Civil_status == "Widowed" ~ 8,
#       Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Widowed" ~ 9,
#       Relationship_status == "Changing relationships" & Civil_status == "Widowed" ~ 10,
#       TRUE ~ 0
#     )
#   )                


# check no cases left out of consideration
tst %>% filter(Status == 0)
unique(tst$Status)

# cont

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

# status_levels <- 1:10
# status_labels <- c("Single",
#                    "Relationship",
#                    "Changing relationships",
#                    "Married",
#                    "Divorced",
#                    "Divorced & relationship",
#                    "Divorced & changing relationships",
#                    "Widowed",
#                    "Widowed & relationship",
#                    "Widowed & changing relationships")

rh_data <- tst %>% 
  select(-Civil_status, -Relationship_status, -Children) %>% 
  mutate(Status_char = factor(Status, levels = status_levels, labels = status_labels))

plot5 <- ggplot(data = rh_data, aes(Status_char))
plot5 + geom_bar() + geom_text(stat="count", aes(label=..count..), vjust=-1) + scale_x_discrete(guide = guide_axis(n.dodge=2))




test <- seqformat(rh_data, from = "SPELL", to = "STS",
                  id = "Id", begin = "Start_age", end = "End_age", status = "Status",
                  covar = "Age", process = FALSE)

alphabet <- as.character(1:10)

# alphabet <- as.character(1:10)

my_seq <- seqdef(test, alphabet = alphabet)

cost_matrix_1 <- seqsubm(my_seq, method = "TRATE", with.missing = TRUE)
cost_matrix_2 <- seqsubm(my_seq, method = "FUTURE")
cost_matrix_3 <- seqsubm(my_seq, method = "INDELSLOG")

cm <- cbind(status_labels, data.frame(cost_matrix_1[1:10,1:10]))
colnames(cm) <- c("Status", status_labels)
print(cm, digits = 4)

readr::write_csv(cm, "../Output/cost_matrix_cl.csv")

# generate distances matrix with OM

my_dist <- seqdist(my_seq, method = "OM", sm = cost_matrix_1, with.missing = TRUE)

library(cluster)

clusterward <- agnes(my_dist, diss = TRUE, method = "ward")

clusters <- cutree(clusterward, k = 5)

clusters_labels <- factor(clusters, labels = paste("Cluster", 1:5))

clusters_titles <- c("Married young with children", "Unstable relationships",
                    "Married then separated with children", "Younger with children",
                    "Long relationships w/o children", "Married w/o children")

clusters_labs <- ifelse(clusters_labels == "Cluster 1", clusters_titles[1],
                        ifelse(clusters_labels == "Cluster 2", clusters_titles[2], 
                               ifelse(clusters_labels == "Cluster 3", clusters_titles[3],
                                      ifelse(clusters_labels == "Cluster 4", clusters_titles[4], clusters_titles[5]))))

par(mar = c(0.5, 0.5, 0.5, 0.5))
seqdplot(my_seq, group = clusters_labels, border = NA, 
         ltext = status_labels)

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

pers_descrip[, 1] <- c("Agreeablenes", "Conscientiousness",
                       "Extraversion", "Neuroticism", "Openness")
  

all_data <- tibble(Id = colnames(my_dist), Cluster = clusters_labels) %>% 
  left_join(BFI_data, by = c("Id" = "PersCode")) %>% 
  group_by(Cluster) %>% 
  summarise(Average_extraversion = mean(BFI_extraversion_mean, na.rm = TRUE),
            Average_agreeableness = mean(BFI_agreeableness_mean, na.rm = TRUE),
            Average_conscientiousness = mean(BFI_conscientiousness_mean, na.rm = TRUE),
            Average_neuroticism = mean(BFI_neuroticisms_mean, na.rm = TRUE),
            Average_openness = mean(BFI_openness_mean, na.rm = TRUE)) %>% 
  ungroup()

knitr::kable(all_data)


pam_clusters6 <- pam(x = dist, k =6)
sil <- silhouette(pam_clusters6)