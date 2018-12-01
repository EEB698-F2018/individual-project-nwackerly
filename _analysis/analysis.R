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

############
##re-order levels
prelim_temp$time_od <- factor(prelim_temp$time_od, levels = c("e_morning", "l_morning", "e_afternoon", "l_afternoon", "evening"))
prelim_temp$pos_beh <- factor(prelim_temp$pos_beh, levels = c("St","Ly", "Sq", "QS", "QW", "Bp", "Su", "VC"))

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
###Compare models
AIC(therm_mod7, therm_mod8, therm_mod9, therm_mod10) ##model 7 and 10 are lowest (3247.068 & 3248.205, resp.)
                                      ##model 8 isn't far off, though, (3255. 603) 
                                 ##and it may be important to include context

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

###Compare models
AIC(therm_mod11, therm_mod12, therm_mod13, therm_mod14) ##models 11 and 14 are lowest (3218.163 & 3218.819, resp.)

###compare between types of models
AIC(therm_mod4, therm_mod6, therm_mod7, therm_mod10, therm_mod8, therm_mod11, therm_mod14)
        ##best models (11, 14, 6) exclude context and exclude interactions
        ##run a few more tests and include one with the interaction still (model 7)

### more comparison #########
anova(therm_mod6, therm_mod7) ##not nested, so can't run
anova(therm_mod6, therm_mod11) ##no signif
anova(therm_mod6, therm_mod14) ##no signif
anova(therm_mod11, therm_mod14) ##significance, so model 11 better than 14
                          #####think I'll choose model 11 since it includes habitat type
                        ##### and it's AIC value is best (3218.163 vs. 3226.904 (model 6) & 3218.819 (model 14))

#####################################################################
#extract residuals 
###model 6
E1 <- resid(therm_mod6, type = "pearson")

#plot fitted vs residuals
F1 <- fitted(therm_mod6, type = "response")

plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     main = "Model 6",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

### model 11 ###########
E2 <- resid(therm_mod11, type = "pearson")

#plot fitted vs residuals
F2 <- fitted(therm_mod11, type = "response")

plot(x = F2, 
     y = E2, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     main = "Model 11",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

### model 14 ###########
E3 <- resid(therm_mod14, type = "pearson")

#plot fitted vs residuals
F3 <- fitted(therm_mod14, type = "response")

plot(x = F3, 
     y = E3, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     main = "Model 14",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

### model 7 ###########
E4 <- resid(therm_mod7, type = "pearson")

#plot fitted vs residuals
F4 <- fitted(therm_mod7, type = "response")

plot(x = F4, 
     y = E4, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     main = "Model 7",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

### residual plots all seem relatively normal; cloud around horizontal axis, indicating a good fit
###################################################################
##Model 11 - includes pos_beh, time_od, sun, date, hab_type and (1|individual) 
E2 <- resid(therm_mod11, type = "pearson")

#plot fitted vs residuals
F2 <- fitted(therm_mod11, type = "response")

plot(x = F2, 
     y = E2, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     main = "Model 11",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

boxplot(E2 ~ pos_beh, data=prelim_temp) 
boxplot(E2 ~ time_od, data=prelim_temp)
plot(x=prelim_temp$sun, y=E2)
boxplot(E2 ~ hab_type, data=prelim_temp)
plot(x=prelim_temp$date, y=E2)

model.matrix(therm_mod11)

##summary
summary(therm_mod11) 

##confidence intervals 
confint(therm_mod11) ##maybe I don't need habitat in there because none of the levels are significant

##model 14
summary(therm_mod14)
confint(therm_mod14)


##### OMIT OUTLIERS ###
prelim_temp2 <- prelim_temp
prelim_temp2$therm_t[prelim_temp2$therm_t >= 42] <- NA

View(prelim_temp2)

prelim_temp3<-na.omit(prelim_temp2)
View(prelim_temp3)

###check model minus outliers
##model 11 with outliers
therm_mod11 <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + 
                      (1|individual), data = prelim_temp)

##model minus outliers
mod_2 <- lmer(therm_t ~ pos_beh + time_od + sun + date + hab_type + 
                (1|individual), data = prelim_temp3)

confint(mod_2)
