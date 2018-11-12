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
prelim_temp$pos_beh[prelim_temp$pos_beh == "Vci"] <- "VC"
prelim_temp$pos_beh[prelim_temp$pos_beh == "Abp"] <- "Bp"
prelim_temp$pos_beh[prelim_temp$pos_beh == "BpS"] <- "Bp"
prelim_temp$pos_beh[prelim_temp$pos_beh == "BpW"] <- "Bp"

############
##re-order levels
prelim_temp$time_od <- factor(prelim_temp$time_od, levels = c("e_morning", "l_morning", "e_afternoon", "l_afternoon", "evening"))
prelim_temp$pos_beh <- factor(prelim_temp$pos_beh, levels = c("Ly", "St", "Sq", "QS", "QW", "Bp", "Su", "VC"))


##plotting
ggplot(prelim_temp, aes(pos_beh, therm_t)) +
  geom_boxplot() + labs(x="Positional Behavior", y="Body Temperature")

##counts
ggplot(prelim_temp, aes(pos_beh))+
  geom_bar(stat="count")  ###maybe combine squat and sit

ggplot(prelim_temp, aes(hab_type))+
  geom_bar(stat="count") ###can I combine bamboo with bamboo woodland?

ggplot(prelim_temp, aes(time_od))+
  geom_bar(stat="count")

ggplot(prelim_temp, aes(sun))+
  geom_histogram()
