# load libraries
library(scales)
library(readr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(car)
library(emmeans)
library(lme4)
library(ggthemes)
library(tidyverse) 

#read in data
prelim_tidy <- read_csv("_data/_tidy/prelim_data_tidycols.csv")
View(prelim_tidy)

bipedal_data <- read_csv("_summary/bipedal_table.csv")
View(bipedal_data)

#Omit any missing values:
summary(is.na(prelim_tidy))

prelim_temp<-na.omit(prelim_tidy)
View(prelim_temp)

##### OMIT OUTLIERS ###
prelim_temp2 <- prelim_temp
prelim_temp2$therm_t[prelim_temp2$therm_t >= 42] <- NA

View(prelim_temp2)

prelim_temp3<-na.omit(prelim_temp2)
View(prelim_temp3)

#Vertical climb and cling together
prelim_temp3$pos_beh[prelim_temp3$pos_beh == "Cb"] <- "Su"
prelim_temp3$pos_beh[prelim_temp3$pos_beh == "Br"] <- "Su"
prelim_temp3$pos_beh[prelim_temp3$pos_beh == "QM"] <- "Su"
prelim_temp3$pos_beh[prelim_temp3$pos_beh == "Vci"] <- "VC"
prelim_temp3$pos_beh[prelim_temp3$pos_beh == "Abp"] <- "Bp"
prelim_temp3$pos_beh[prelim_temp3$pos_beh == "BpS"] <- "Bp"
prelim_temp3$pos_beh[prelim_temp3$pos_beh == "BpW"] <- "Bp"
prelim_temp3$pos_beh[prelim_temp3$pos_beh == "Sq"] <- "St"

############
##re-order levels
prelim_temp3$time_od <- factor(prelim_temp3$time_od, levels = c("e_morning", "l_morning", "e_afternoon", "l_afternoon", "evening"))
prelim_temp3$pos_beh <- factor(prelim_temp3$pos_beh, levels = c("Bp", "Ly", "St", "QS", "QW", "Su", "VC"))

###change to factor & numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi", "amb_t")

prelim_temp3[factor_cols] <- lapply(prelim_temp3[factor_cols], as.factor)

prelim_temp3[numeric_cols] <- lapply(prelim_temp3[numeric_cols], as.numeric)

str(prelim_temp3)

##model minus outliers
mod_1 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                (1|individual), data = prelim_temp3)
summary(mod_1)
confint(mod_1)

#######get p-values########
# extract coefficients
coefs <- data.frame(coef(summary(mod_1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

##########
#####have Sit/Squat as the outgroup
##re-order levels
prelim_temp3$time_od <- factor(prelim_temp3$time_od, levels = c("e_morning", "l_morning", "e_afternoon", "l_afternoon", "evening"))
prelim_temp3$pos_beh <- factor(prelim_temp3$pos_beh, levels = c("St", "Ly", "Bp", "QS", "QW", "Su", "VC"))

###change to factor & numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi", "amb_t")

prelim_temp3[factor_cols] <- lapply(prelim_temp3[factor_cols], as.factor)

prelim_temp3[numeric_cols] <- lapply(prelim_temp3[numeric_cols], as.numeric)

str(prelim_temp3)

##model minus outliers
mod_2 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                (1|individual), data = prelim_temp3)
summary(mod_2)
confint(mod_2)

#######get p-values########
# extract coefficients
coefs <- data.frame(coef(summary(mod_2)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

