library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(TraMineR)

cs_levels <- 1:6
cs_labels <- c(
  "Single", "Married", "Registered partnership",
  "Same-sex partnership", "Divorced", "Widowed"
)

rs_levels <- 1:4
rs_labels <- c(
  "No relationship", "Relationship",
  "Open relationship", "Changing relationships"
)

liv_levels <- 0:2
liv_labels <- c("No partner", "Yes", "No")

file <- "../Data_original/Tracker_Uni-Zurich_Modified.xlsx"

raw_data <- read_excel(file)

raw_data <- raw_data %>%
  select_if(~ !all(is.na(.)))

# raw_data %>%
#   rename(Id = 1) %>%
#   mutate(Id = tolower(Id)) %>%
#   group_by(Id) %>%
#   summarise(Count = n()) %>%
#   filter(Count > 1) %>%
#   View(.)

new_names <- vector(mode = "character")

for (i in 1:12) {
  new_names[2 * (i - 1) + 1] <- paste0("Start-", i)
  new_names[2 * (i - 1) + 2] <- paste0("End-", i)
}

colnames(raw_data)[1] <- "Id"
colnames(raw_data)[58] <- "Age"
colnames(raw_data)[59:82] <- new_names
colnames(raw_data)[83:94] <- paste0("Civil-", 1:12)
colnames(raw_data)[95:106] <- paste0("Relationship-", 1:12)
colnames(raw_data)[131:142] <- paste0("Living-", 1:12)

raw_data <- raw_data %>%
  select(c(1, 58:106, 131:142))

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
  pivot_longer(
    cols = starts_with("Start"),
    names_to = "Phase",
    names_prefix = "Start-",
    values_to = "Start_age",
    values_drop_na = TRUE
  ) %>%
  pivot_longer(
    cols = starts_with("End"),
    names_to = "Match2",
    names_prefix = "End-",
    values_to = "End_age",
    values_drop_na = TRUE
  ) %>%
  pivot_longer(
    cols = starts_with("Civil"),
    names_to = "Match3",
    names_prefix = "Civil-",
    values_to = "Civil_status",
    values_drop_na = TRUE
  ) %>%
  pivot_longer(
    cols = starts_with("Relationship"),
    names_to = "Match4",
    names_prefix = "Relationship-",
    values_to = "Relationship_status",
    values_drop_na = TRUE
  ) %>%
  pivot_longer(
    cols = starts_with("Living"),
    names_to = "Match5",
    names_prefix = "Living-",
    values_to = "Living_situation",
    values_drop_na = TRUE
  ) %>%
  filter(
    Phase == Match2,
    Phase == Match3,
    Phase == Match4,
    Phase == Match5,
    Start_age >= 15,
    !Id %in% c("zr34u", "EN61O"),
    !(Id == "GB28U" & Phase == "4")
  ) %>%
  select(-contains("Match")) %>%
  mutate(
    Civil_status = factor(Civil_status, levels = cs_levels, labels = cs_labels),
    Relationship_status = factor(Relationship_status, levels = rs_levels, labels = rs_labels),
    Living_situation = factor(Living_situation, levels = liv_levels, labels = liv_labels),
    Phase = as.numeric(Phase)
  )

# checks for living situation

raw_data %>% filter(Relationship_status == "No relationship" & Living_situation == "Yes")

raw_data %>% filter(Civil_status == "Married" & Living_situation != "Yes")



plot1 <- ggplot(data = raw_data, aes(Civil_status))
plot1 + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = -1)

ggplot(
  raw_data,
  aes(
    x = Civil_status,
    y = Start_age
  )
) +
  geom_boxplot()

plot2 <- ggplot(data = raw_data, aes(Relationship_status))
plot2 + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = -1)

ggplot(
  raw_data,
  aes(
    x = Relationship_status,
    y = Start_age
  )
) +
  geom_boxplot()

ggplot(raw_data, aes(x = Civil_status)) +
  geom_bar(
    aes(fill = Relationship_status),
    stat = "count", color = "white",
    position = position_dodge(0.9)
  )

plot3 <- ggplot(data = raw_data, aes(Living_situation))
plot3 + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = -1)

# register EN61O does not make sense (phase 1: registered partnership + no relationship) REMOVE
# Only two cases of Widowed+Relationship
# Average number of phases 4.168

tst <- raw_data %>%
  mutate(
    Status = case_when(
      Relationship_status == "No relationship" & Civil_status == "Single" ~ 1,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Single" & Living_situation == "No" ~ 2,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Single" & Living_situation == "Yes" ~ 3,
      Relationship_status == "Changing relationships" & Civil_status == "Single" ~ 4,
      Civil_status %in% c("Married", "Registered partnership") ~ 5,
      Relationship_status == "No relationship" & Civil_status == "Divorced" ~ 1,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Divorced" & Living_situation == "No" ~ 2,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Divorced" & Living_situation == "Yes" ~ 3,
      Relationship_status == "Changing relationships" & Civil_status == "Divorced" ~ 4,
      Relationship_status == "No relationship" & Civil_status == "Widowed" ~ 1,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Widowed" & Living_situation == "No" ~ 2,
      Relationship_status %in% c("Relationship", "Open relationship") & Civil_status == "Widowed" & Living_situation == "Yes" ~ 3,
      Relationship_status == "Changing relationships" & Civil_status == "Widowed" ~ 4,
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

status_levels <- 1:5
status_labels <- c(
  "Single",
  "Relationship",
  "Rel. + Living together",
  "Changing relationships",
  "Married"
)

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
  select(-Civil_status, -Relationship_status, -Living_situation) %>%
  mutate(Status_char = factor(Status, levels = status_levels, labels = status_labels))

plot4 <- ggplot(data = rh_data, aes(Status_char))
plot4 + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = -1) + scale_x_discrete(guide = guide_axis(n.dodge = 2))




test <- seqformat(rh_data,
  from = "SPELL", to = "STS",
  id = "Id", begin = "Start_age", end = "End_age", status = "Status",
  covar = "Age", process = FALSE
)

alphabet <- as.character(1:5)

# alphabet <- as.character(1:10)

my_seq <- seqdef(test, alphabet = alphabet)

cost_matrix_1 <- seqsubm(my_seq, method = "TRATE")
cost_matrix_2 <- seqsubm(my_seq, method = "FUTURE")
cost_matrix_3 <- seqsubm(my_seq, method = "INDELSLOG")

cm <- cbind(status_labels, data.frame(cost_matrix_1))
colnames(cm) <- c("Status", status_labels)
print(cm, digits = 4)

readr::write_csv(cm, "../Output/cost_matrix_living.csv")

### Examples with TraMineR

data("mvad")

# Alphabet of the states
mvad.alphab <- c(
  "employment", "FE", "HE", "joblessness",
  "school", "training"
)

# Define sequences with the data frame and the alphabet
# xtstep is an argument for plotting
mvad.seq <- seqdef(mvad, 17:86, xtstep = 6, alphabet = mvad.alphab)

# Optimal matching with insertion/deletion cost 1 and replacement costs
# estimated by transition rates
# The result is a (squared) distance matrix
mvad.om <- seqdist(mvad.seq, method = "OM", indel = 1, sm = "TRATE")

## Clustering

library(cluster)

clusterward <- agnes(mvad.om, diss = TRUE, method = "ward")

# Vector containing cluster by individual
mvad.cl4 <- cutree(clusterward, k = 4)

cl4.lab <- factor(mvad.cl4, labels = paste("Cluster", 1:4))

# Plot cluster distributions
seqdplot(mvad.seq, group = cl4.lab, border = NA)
