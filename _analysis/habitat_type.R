# load libraries
library(tidyverse) 
library(readr) 
library(lubridate)
library(magrittr)
library(ggplot2)
library(car)
library(emmeans)
library(lme4)

#read in data
prelim_tidy <- read_csv("_data/_tidy/prelim_data_tidycols.csv")
View(prelim_tidy)

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

####combine levels
#Vertical climb and cling together; make Bp 1 category; St & Sq together
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
prelim_temp3$pos_beh <- factor(prelim_temp3$pos_beh, levels = c("Ly", "St", "QS", "QW", "Bp", "Su", "VC"))

###change to factor & numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi", "amb_t")

prelim_temp3[factor_cols] <- lapply(prelim_temp3[factor_cols], as.factor)

prelim_temp3[numeric_cols] <- lapply(prelim_temp3[numeric_cols], as.numeric)

str(prelim_temp3)

####model
mod_2 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                (1|individual), data = prelim_temp3)
summary(mod_2)
confint(mod_2)

###New dataset combining with vs. without shade habitat types
prelim_temp4 <- prelim_temp3
View(prelim_temp4)

#####combine habitat types
prelim_temp4$hab_type[prelim_temp4$hab_type == "WD"] <- "WITH SHADE"
prelim_temp4$hab_type[prelim_temp4$hab_type == "BMWD"] <- "WITH SHADE"
prelim_temp4$hab_type[prelim_temp4$hab_type == "BM"] <- "WITH SHADE"
prelim_temp4$hab_type[prelim_temp4$hab_type == "GA"] <- "WITH SHADE"
prelim_temp4$hab_type[prelim_temp4$hab_type == "GL"] <- "WITHOUT SHADE"

############
##re-order levels
prelim_temp4$time_od <- factor(prelim_temp4$time_od, levels = c("e_morning", "l_morning", "e_afternoon", "l_afternoon", "evening"))
prelim_temp4$pos_beh <- factor(prelim_temp4$pos_beh, levels = c("Ly", "St", "QS", "QW", "Bp", "Su", "VC"))
prelim_temp4$hab_type <- factor(prelim_temp4$hab_type, levels = c("WITH SHADE", "WITHOUT SHADE"))

###change to factor & numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi", "amb_t")

prelim_temp4[factor_cols] <- lapply(prelim_temp4[factor_cols], as.factor)

prelim_temp4[numeric_cols] <- lapply(prelim_temp4[numeric_cols], as.numeric)

str(prelim_temp4)

##model new dataset
mod_3 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                (1|individual), data = prelim_temp4)
summary(mod_3)
confint(mod_3)
