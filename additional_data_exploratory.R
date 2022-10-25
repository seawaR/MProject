library(foreign)
library(readxl)
library(dplyr)
library(ggplot2)

sav_file <- "../Data_original/Datensatz_gesamt.sav"

full_db <- read.spss(sav_file, to.data.frame=TRUE)

xl_file <- "../Data_original/Base_MA_Adriana.xls"

sub_data <- read_excel(xl_file)

data <- full_db %>% 
  select(colnames(sub_data)) %>% 
  mutate(MensMenopaus = ifelse(MensMenopaus == -66, NA, MensMenopaus),
         Nationality = case_when(
           Swiss == "quoted" ~ "Swiss",
           G_A_FL_L == "quoted" ~ "GER/AUT/LIE/LUX",
           Europe == "quoted" ~ "Other european country"
         ))

plot(data)

summary(data$Menstruation_Letzte12)

ggplot(data, aes(Menstruation_Letzte12)) + 
  geom_bar() + geom_text(stat="count", aes(label=..count..), vjust=-1) + scale_x_discrete(guide = guide_axis(n.dodge=2))

summary(data$Menstruation_Letzte3)

ggplot(data, aes(Menstruation_Letzte3)) + 
  geom_bar() + geom_text(stat="count", aes(label=..count..), vjust=-1) + scale_x_discrete(guide = guide_axis(n.dodge=2))

ggplot(data, aes(Nationality)) + 
  geom_bar() + geom_text(stat="count", aes(label=..count..), vjust=-1) + scale_x_discrete(guide = guide_axis(n.dodge=2))

ggplot(data, aes(MensMenopaus)) + 
  geom_histogram()
