library(foreign)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

sav_file <- "../Data_original/Datensatz_gesamt.sav"

full_db <- read.spss(sav_file, to.data.frame = TRUE)

xl_file <- "../Data_original/Base_MA_Adriana.xls"

sub_data <- read_excel(xl_file)

data <- full_db %>%
  select(colnames(sub_data)) %>%
  mutate(
    MensMenopaus = ifelse(MensMenopaus == -66, NA, MensMenopaus),
    Pregnancies_N = ifelse(Pregnancies_N == -66, NA, Pregnancies_N),
    Children_N = ifelse(Children_N == -66, NA, Children_N),
    Nationality = case_when(
      Swiss == "quoted" ~ "Swiss",
      G_A_FL_L == "quoted" ~ "GER/AUT/LIE/LUX",
      Europe == "quoted" ~ "Other european country"
    ),
    Country_Residence = case_when(
      str_trim(str_to_lower(Country_Residence), side = "right") == "schweiz" ~ "Switzerland",
      str_trim(Country_Residence, side = "right") == "CH" ~ "Switzerland",
      str_trim(Country_Residence, side = "right") == "Deutschland" ~ "Germany"
    ),
    Education = recode_factor(Education, `Obligatorische Schulzeit: Volksschulabschluss (CH) / Hauptschulabschluss oder Fachoberschulreife (D) / Pflichtschule (A)` = "Obligatorische Schulzeit"),
    Education_Partner = recode_factor(Education, `Obligatorische Schulzeit: Volksschulabschluss (CH) / Hauptschulabschluss oder Fachoberschulreife (D) / Pflichtschule (A)` = "Obligatorische Schulzeit")
  )

summary(data$Menstruation_Letzte12)

ggplot(data, aes(Menstruation_Letzte12)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

summary(data$Menstruation_Letzte3)

ggplot(data, aes(Menstruation_Letzte3)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Nationality)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Country_Residence)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Education)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Work)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Employment)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Income_Enough)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Education_Partner)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Infidelity)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Children)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Children_Nonbiological)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Smoking)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(as.character(Coffee))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Closeness_Contentment)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(Partnership_RAS)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(RAS_contentment)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(RAS_goodness)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(RAS_strikeup)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(RAS_expectations)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(RAS_love)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(RAS_problems)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggplot(data, aes(WorkInShift)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))



ggplot(data, aes(MensMenopaus)) +
  geom_histogram()

ggplot(data, aes(Work_Years)) +
  geom_histogram()

ggplot(data, aes(Income)) +
  geom_histogram()

ggplot(data, aes(Housework_Hours)) +
  geom_histogram()

ggplot(data, aes(Income_Household)) +
  geom_histogram()
