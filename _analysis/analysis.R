# load libraries
library(tidyverse) 
library(readr) 
library(lubridate)
library(magrittr)
library(ggplot2)
library(car)
library(emmeans)

#read in data
prelim_tidy <- read_csv("_data/_tidy/prelim_data_tidycols.csv")
View(prelim_tidy)

bipedal_data <- read_csv("_summary/bipedal_table.csv")
View(bipedal_data)

#Omit any missing values:
summary(is.na(prelim_tidy))

prelim_temp<-na.omit(prelim_tidy)
View(prelim_temp)

#Combine positional behaviors into smaller categories
# suspensory = Cb, Br & QM
#Vertical climb and cling together
prelim_temp$pos_beh[prelim_temp$pos_beh == "Cb"] <- "Sp"
prelim_temp$pos_beh[prelim_temp$pos_beh == "Br"] <- "Sp"
prelim_temp$pos_beh[prelim_temp$pos_beh == "QM"] <- "Sp"
prelim_temp$pos_beh[prelim_temp$pos_beh == "Vci"] <- "VC"

###
hist(prelim_temp$therm_t)
dotchart(prelim_temp$therm_t)
boxplot(prelim_temp$therm_t ~ prelim_temp$pos_beh,  ylab="body temp")


# i.	Plot response against each predictor and random effect. 
ggplot(prelim_temp, aes(pos_beh, therm_t, color=context))+
  geom_boxplot()

ggplot(prelim_temp, aes(pos_beh, therm_t, color=time_od))+
  geom_boxplot()+
  facet_grid(.~individual)

##
with(prelim_temp, table(pos_beh, hab_type))
with(prelim_temp, table(pos_beh, time_od))
with(prelim_temp, table(pos_beh, hab_type, time_od))

## choosing models
#### positional behavior on body temp
therm_mod1 <- lm(therm_t ~ pos_beh, data = prelim_temp)

model.matrix(therm_mod1)

summary(therm_mod1)

### multiple linear regression
therm_mod2 <- lm(therm_t ~ pos_beh + time_od + sun, data = prelim_temp)

model.matrix(therm_mod2)

summary(therm_mod2)

###interaction between pos_beh and time of day
therm_mod3 <- lm(therm_t ~ pos_beh*time_od + sun, data = prelim_temp)

model.matrix(therm_mod3)

summary(therm_mod3)

## plus habitat type
therm_mod4 <- lm(therm_t ~ pos_beh + time_od + sun + hab_type, data = prelim_temp)

model.matrix(therm_mod4)

summary(therm_mod4)

### interaction of time of day and sun
therm_mod5 <- lm(therm_t ~ pos_beh + time_od*sun, data = prelim_temp)

model.matrix(therm_mod5)

summary(therm_mod5)

#### Model testing
###AIC
AIC(therm_mod1, therm_mod2, therm_mod3, therm_mod4, therm_mod5, therm_mod6, therm_mod7)


### plus individual random effect
## GLMER ##
library(lme4)
therm_mod6 <- lmer(therm_t ~ pos_beh + time_od*sun + (1|individual), data = prelim_temp)

summary(therm_mod6)

#extract residuals
E1 <- resid(therm_mod6, type = "pearson")

#plot fitted vs residuals
F1 <- fitted(therm_mod6, type = "response")

plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)


##plus context random effect
therm_mod7 <- lmer(therm_t ~ pos_beh + time_od*sun + (1|individual) + (1|context), data = prelim_temp)

summary(therm_mod7)

#extract residuals
E2 <- resid(therm_mod7, type = "pearson")

#plot fitted vs residuals
F2 <- fitted(therm_mod7, type = "response")

plot(x = F2, 
     y = E2, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

###with habitat type
therm_mod8 <- lmer(therm_t ~ pos_beh + time_od*sun + hab_type + (1|individual) + (1|context), data = prelim_temp)

summary(therm_mod8)

#extract residuals
E3 <- resid(therm_mod8, type = "pearson")

#plot fitted vs residuals
F3 <- fitted(therm_mod8, type = "response")

plot(x = F3, 
     y = E3, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

#### Model testing
###AIC
AIC(therm_mod1, therm_mod2, therm_mod3, therm_mod4, therm_mod5, therm_mod6, therm_mod7, therm_mod8)
