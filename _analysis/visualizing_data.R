# load libraries
library(tidyverse)
library(readr)
library(lubridate)
library(magrittr)
library(ggplot2)

#read in data
prelim_tidy <- read_csv("_data/_tidy/prelim_data_tidycols.csv")
View(prelim_tidy)

bipedal_data <- read_csv("_summary/bipedal_table.csv")
View(bipedal_data)

str(prelim_tidy)
str(bipedal_data)

## Change to factor and numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi")

prelim_tidy[factor_cols] <- lapply(prelim_tidy[factor_cols], as.factor)
bipedal_data[factor_cols] <- lapply(bipedal_data[factor_cols], as.factor)

prelim_tidy[numeric_cols] <- lapply(prelim_tidy[numeric_cols], as.numeric)
bipedal_data[numeric_cols] <- lapply(bipedal_data[numeric_cols], as.numeric)

#Summary data
summary(prelim_tidy)
summary(bipedal_data)

#explore using tables
### prelim_tidy
with(prelim_tidy, table(pos_beh, context))
with(prelim_tidy, table(pos_beh, hab_type))
with(prelim_tidy, table(pos_beh, substrate))

with(prelim_tidy, ftable(pos_beh, context, hab_type))
with(prelim_tidy, ftable(pos_beh, context, substrate))

with(prelim_tidy, table(individual, pos_beh))
with(prelim_tidy, ftable(individual, context, pos_beh))

### bipedal_data
with(bipedal_data, table(pos_beh, context))
with(bipedal_data, table(pos_beh, hab_type))
with(bipedal_data, table(pos_beh, substrate))

with(bipedal_data, ftable(pos_beh, context, hab_type))
with(bipedal_data, ftable(pos_beh, context, substrate))

with(bipedal_data, table(individual, pos_beh))
with(bipedal_data, ftable(individual, pos_beh, context))

################################################################################
# Graphics
##boxplot
boxplot(prelim_tidy$therm_t)
boxplot(therm_t ~ pos_beh, data=prelim_tidy)
boxplot(therm_t ~ context, data=prelim_tidy)
boxplot(therm_t ~ hab_type, data=prelim_tidy)
boxplot(therm_t ~ substrate, data=prelim_tidy)

##histogram
hist(prelim_tidy$therm_t)
hist(prelim_tidy$sun)

#coplot
#effect of positional behavior on body temp given context
coplot(therm_t ~ pos_beh | context, data=prelim_tidy)
coplot(therm_t ~ pos_beh | hab_type, data=prelim_tidy)

## plot
plot(prelim_tidy$sun, prelim_tidy$therm_t)
plot(prelim_tidy$time, prelim_tidy$therm_t)

################################################################
## ggplot
## prelim_tidy
ggplot(prelim_tidy, aes(therm_t)) + 
  geom_histogram()

ggplot(prelim_tidy, aes(sun)) + 
  geom_histogram()

ggplot(prelim_tidy, aes(pos_beh, therm_t, color=context)) + 
  geom_boxplot()

## bipedal data
ggplot(bipedal_data, aes(pos_beh, therm_t, color=individual)) + 
  geom_boxplot()

ggplot(bipedal_data, aes(pos_beh, therm_t, color=context)) + 
  geom_boxplot()

ggplot(bipedal_data, aes(context, therm_t, color=pos_beh)) + 
  geom_boxplot()

ggplot(bipedal_data, aes(sun)) + 
  geom_histogram()

