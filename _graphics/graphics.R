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
prelim_temp$pos_beh <- factor(prelim_temp$pos_beh, levels = c("St", "Ly", "Sq", "QS", "QW", "Bp", "Su", "VC"))


##plotting
ggplot(prelim_temp, aes(pos_beh, therm_t)) +
  geom_boxplot() + labs(x="Positional Behavior", y="Body Temperature") +
  theme_classic() + geom_boxplot(fill = "white", colour = "blue")

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
  theme_classic()  

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
library(broom)
m1 <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + 
       (1|individual), data = prelim_temp)

summary(m1)
m1_res <- tidy(coef(summary(m1)) ) 

ggplot(m1_res, aes(pos_beh, therm_t)) +
  geom_point()


ggplot(prelim_temp, aes(pos_beh, therm_t)) + 
  #geom_point(size = 3) +
  geom_point(aes(y=predict(m1)), size = 1)

##attempting to follow tutorial but got too confused....
preddata <- with(prelim_temp, expand.grid(pos_beh = levels(pos_beh), time_od = "l_morning", 
                                          sun = 50, date = "2018-06-28", hab_type = "WD"))
              #time_od = levels(time_od))) ##
str(preddata)
preddata$date <- as.Date(preddata$date)

mm <- model.matrix(~pos_beh + time_od + sun + date + hab_type, data=preddata)

pframe2 <- data.frame(preddata,eta=mm%*%fixef(m1))
pvar1 <- diag(mm %*% tcrossprod(vcov(m1),mm))
tvar1 <- pvar1+VarCorr(m1)$individual
