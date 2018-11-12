# load libraries
library(tidyverse) 
library(readr) 
library(lubridate)
library(magrittr)
library(ggplot2)
library(car)
library(emmeans)
library(lme4)

##read in data
prelim_tidy <- read_csv("_data/_tidy/prelim_data_tidycols.csv")
View(prelim_tidy)

#Omit any missing values:
summary(is.na(prelim_tidy))

prelim_temp<-na.omit(prelim_tidy)
View(prelim_temp)

#Combine positional behaviors into smaller categories
# suspensory = Cb, Br & QM
#Vertical climb and cling together
prelim_temp$pos_beh[prelim_temp$pos_beh == "Cb"] <- "Su"
prelim_temp$pos_beh[prelim_temp$pos_beh == "Br"] <- "Su"
prelim_temp$pos_beh[prelim_temp$pos_beh == "QM"] <- "Su"
prelim_temp$pos_beh[prelim_temp$pos_beh == "Sq"] <- "St"
prelim_temp$pos_beh[prelim_temp$pos_beh == "Vci"] <- "VC"
prelim_temp$pos_beh[prelim_temp$pos_beh == "Abp"] <- "Bp"
prelim_temp$pos_beh[prelim_temp$pos_beh == "BpS"] <- "Bp"
prelim_temp$pos_beh[prelim_temp$pos_beh == "BpW"] <- "Bp"

############
##re-order levels
prelim_temp$time_od <- factor(prelim_temp$time_od, levels = c("e_morning", "l_morning", "e_afternoon", "l_afternoon", "evening"))
prelim_temp$pos_beh <- factor(prelim_temp$pos_beh, levels = c("Ly", "St", "QS", "QW", "Bp", "Su", "VC"))


##plotting
ggplot(prelim_temp, aes(pos_beh, therm_t)) +
  geom_boxplot() + labs(x="Positional Behavior", y="Body Temperature") +
  theme_classic()

ggplot(prelim_temp, aes(individual, therm_t)) +
  geom_boxplot() + labs(x="Individual", y="Body Temperature") +
  theme_classic()

ggplot(prelim_temp, aes(hab_type, therm_t)) +
  geom_boxplot() + labs(x="Habitat Type", y="Body Temperature") +
  theme_classic()

ggplot(prelim_temp, aes(time_od, therm_t)) +
  geom_boxplot() + labs(x="Time of Day", y="Body Temperature") +
  theme_classic()

##counts
ggplot(prelim_temp, aes(pos_beh))+
  geom_bar(stat="count") + labs(x="Positional Behavior", y="Count") +
  theme_classic()  ###maybe combine squat and sit

ggplot(prelim_temp, aes(hab_type))+
  geom_bar(stat="count") + labs(x="Habitat Type", y="Count")+
  theme_minimal()    ###can I combine bamboo with bamboo woodland?

ggplot(prelim_temp, aes(time_od))+
  geom_bar(stat="count") + labs(x="Time of Day", y="Count")+
  theme_minimal()

ggplot(prelim_temp, aes(individual))+
  geom_bar(stat="count") + labs(x="Individual", y="Count")+
  theme_minimal()

ggplot(prelim_temp, aes(sun))+
  geom_histogram()

###model plotting
m1 <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + 
       (1|individual), data = prelim_temp)
prelim_temp$pred <- predict(m1, type="response")

