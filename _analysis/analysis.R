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

bipedal_data <- read_csv("_summary/bipedal_table.csv")
View(bipedal_data)

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

###
hist(prelim_temp$therm_t)
dotchart(prelim_temp$therm_t)
boxplot(prelim_temp$therm_t ~ prelim_temp$pos_beh,  ylab="body temp")

# Plot response against each predictor and random effect. 
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
##GLMER

### full model
therm_mod <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + context + 
                     (1|individual) + pos_beh*hab_type + pos_beh*sun + time_od*sun,
                     data = prelim_temp)

##minus pos_beh/sun interaction
therm_mod2 <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + context + 
                    (1|individual) + pos_beh*hab_type + time_od*sun,
                  data = prelim_temp)

##plus pos_beh*sun, minus pos_beh*hab_type
therm_mod3 <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + context + 
                     (1|individual) + pos_beh*sun + time_od*sun,
                   data = prelim_temp)

##minus interactions with pos_beh
therm_mod4 <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + context + 
                    (1|individual) + time_od*sun,
                  data = prelim_temp)

##minus time_od interaction
therm_mod5 <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + context + 
                     (1|individual) + pos_beh*sun,
                   data = prelim_temp)

##minus all interactions
therm_mod6 <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + context + 
                     (1|individual),
                   data = prelim_temp)

####compare interaction models
AIC(therm_mod3, therm_mod4, therm_mod5, therm_mod6) ###lowest AIC is with no interaction (3226.904; DF: 23)
                                               ###second lowest is time_od*sun (3255.461, DF: 27)
                                               ###continue forward with time_od*sun & no interactions
###############################################################
##time_od*sun models
## minus context 
therm_mod7 <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + 
                     (1|individual) + time_od*sun,
                   data = prelim_temp)

##minus hab_type
therm_mod8 <- lmer(therm_t ~ pos_beh + time_od + sun + date + context + 
                     (1|individual) + time_od*sun,
                   data = prelim_temp)

##minus date
therm_mod9 <- lmer(therm_t ~ pos_beh + time_od + sun + hab_type + context + 
                     (1|individual) + time_od*sun,
                   data = prelim_temp)

##minus hab_type & context
therm_mod10 <- lmer(therm_t ~ pos_beh + time_od + sun + date + 
                     (1|individual) + time_od*sun,
                   data = prelim_temp)

###############################################################################
###no interactions
## minus context 
therm_mod11 <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + 
                     (1|individual), data = prelim_temp)

##minus hab_type
therm_mod12 <- lmer(therm_t ~ pos_beh + time_od + sun + date + context + 
                     (1|individual), data = prelim_temp)

##minus date
therm_mod13 <- lmer(therm_t ~ pos_beh + time_od + sun + hab_type + context + 
                     (1|individual), data = prelim_temp)

##minus hab_type & context
therm_mod14 <- lmer(therm_t ~ pos_beh + time_od + sun + date + 
                      (1|individual), data = prelim_temp)

#####################################################################
####Compare models
###


#####################################################################
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
