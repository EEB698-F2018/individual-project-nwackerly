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
#Vertical climb and cling together, BpS & BpW together, Squat & Sit together
prelim_temp$pos_beh[prelim_temp$pos_beh == "Cb"] <- "Su"
prelim_temp$pos_beh[prelim_temp$pos_beh == "Br"] <- "Su"
prelim_temp$pos_beh[prelim_temp$pos_beh == "QM"] <- "Su"
prelim_temp$pos_beh[prelim_temp$pos_beh == "Vci"] <- "VC"
prelim_temp$pos_beh[prelim_temp$pos_beh == "Abp"] <- "Bp"
prelim_temp$pos_beh[prelim_temp$pos_beh == "BpS"] <- "Bp"
prelim_temp$pos_beh[prelim_temp$pos_beh == "BpW"] <- "Bp"
prelim_temp$pos_beh[prelim_temp$pos_beh == "Sq"] <- "St"



############
##re-order levels
prelim_temp$time_od <- factor(prelim_temp$time_od, levels = c("e_morning", "l_morning", "e_afternoon", "l_afternoon", "evening"))
prelim_temp$pos_beh <- factor(prelim_temp$pos_beh, levels = c("St","Ly", "Sq", "QS", "QW", "Bp", "Su", "VC"))

##change to factor & numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi", "amb_t")

prelim_temp[factor_cols] <- lapply(prelim_temp[factor_cols], as.factor)

prelim_temp[numeric_cols] <- lapply(prelim_temp[numeric_cols], as.numeric)

#######################################
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

#######################################################
## choosing models
##GLMER

### full model
therm_mod <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + context + 
                     (1|individual) + pos_beh*hab_type + pos_beh*sun + amb_t*sun,
                     data = prelim_temp)

##minus pos_beh/sun interaction
therm_mod2 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + context + 
                    (1|individual) + pos_beh*hab_type + amb_t*sun,
                  data = prelim_temp)

##plus pos_beh*sun, minus pos_beh*hab_type
therm_mod3 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + context + 
                     (1|individual) + pos_beh*sun + amb_t*sun,
                   data = prelim_temp)

##minus interactions with pos_beh
therm_mod4 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + context + 
                    (1|individual) + amb_t*sun,
                  data = prelim_temp)

##minus amb_t interaction
therm_mod5 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + context + 
                     (1|individual) + pos_beh*sun,
                   data = prelim_temp)

##minus all interactions
therm_mod6 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + context + 
                     (1|individual),
                   data = prelim_temp)

####compare interaction models
AIC(therm_mod3, therm_mod4, therm_mod5, therm_mod6) ###lowest AIC is with no interaction (3244.383; DF: 20)
                                               ###second lowest is time_od*sun (3259.170, DF: 21)
                                               ###continue forward with amb_t*sun & no interactions
###############################################################
##amb_t*sun models
## minus context 
therm_mod7 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                     (1|individual) + amb_t*sun,
                   data = prelim_temp)

##minus hab_type
therm_mod8 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + context + 
                     (1|individual) + amb_t*sun,
                   data = prelim_temp)

##minus date
therm_mod9 <- lmer(therm_t ~ pos_beh + amb_t + sun + hab_type + context + 
                     (1|individual) + amb_t*sun,
                   data = prelim_temp)

##minus hab_type & context
therm_mod10 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + 
                     (1|individual) + amb_t*sun,
                   data = prelim_temp)
###Compare models
AIC(therm_mod7, therm_mod8, therm_mod9, therm_mod10) ##model 7 and 10 are lowest (3251.671 & 3246.007, resp.)
                                      ##model 8 isn't far off, though, (3252.598) 
                                 ##and it may be important to include context

###############################################################################
###no interactions
## minus context 
therm_mod11 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                     (1|individual), data = prelim_temp)

##minus hab_type
therm_mod12 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + context + 
                     (1|individual), data = prelim_temp)

##minus date
therm_mod13 <- lmer(therm_t ~ pos_beh + amb_t + sun + hab_type + context + 
                     (1|individual), data = prelim_temp)

##minus hab_type & context
therm_mod14 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + 
                      (1|individual), data = prelim_temp)

###Compare models
AIC(therm_mod11, therm_mod12, therm_mod13, therm_mod14) ##models 11 and 14 are lowest (3236.897 & 3231.295, resp.)

###compare between types of models
AIC(therm_mod4, therm_mod6, therm_mod7, therm_mod10, therm_mod8, therm_mod11, therm_mod14)
        ##best models (11, 14, 6) exclude context and exclude interactions
        ##run a few more tests and include one with the interaction still (model 7)

### more comparison #########
anova(therm_mod6, therm_mod7) ##not nested, so can't run
anova(therm_mod6, therm_mod11) ##no signif
anova(therm_mod6, therm_mod14) ##no signif
anova(therm_mod11, therm_mod14) ##no signif
                          #####think I'll choose model 11 since it includes habitat type
                        ##### and it's AIC value is best (1416.906 vs. 1419.632 (model 14) & 1423.646 (model 6))

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
##Model 11 - includes pos_beh, amb_t, sun, date, hab_type and (1|individual) 
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
plot(E2 ~ amb_t, data=prelim_temp)
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

###############################################
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
prelim_temp3$pos_beh <- factor(prelim_temp3$pos_beh, levels = c("Ly", "St", "QS", "QW", "Bp", "Su", "VC"))

###change to factor & numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi", "amb_t")

prelim_temp3[factor_cols] <- lapply(prelim_temp3[factor_cols], as.factor)

prelim_temp3[numeric_cols] <- lapply(prelim_temp3[numeric_cols], as.numeric)

str(prelim_temp3)

###check model minus outliers
##model 11 with outliers
therm_mod11 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                      (1|individual), data = prelim_temp)
summary(therm_mod11)
confint(therm_mod11)

##model minus outliers
mod_2 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                (1|individual), data = prelim_temp3)
summary(mod_2)
confint(mod_2)


######## plotting residuals ##################
##model 11
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

##mod_2
E5 <- resid(mod_2, type = "pearson")

#plot fitted vs residuals
F5 <- fitted(mod_2, type = "response")

plot(x = F5, 
     y = E5,
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     main = "Model",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

###### HSR: post-hoc test to compare between levels of position behavior. Will want to do this in the analysis file not graphics file.#### 
library(emmeans)
emmeans(mod_2, list(pairwise ~ pos_beh), adjust = "tukey")


####change levels and run mod_2 with QS as the outgroup #####
prelim_temp3$pos_beh <- factor(prelim_temp3$pos_beh, levels = c("QS", "Ly", "St", "QW", "Bp", "Su", "VC"))

mod_2 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                (1|individual), data = prelim_temp3)
summary(mod_2) 
confint(mod_2) ##bipedalism is significant

##change levels with QW as outgroup
prelim_temp3$pos_beh <- factor(prelim_temp3$pos_beh, levels = c("QW", "Ly", "St", "QS", "Bp", "Su", "VC"))

mod_2 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                (1|individual), data = prelim_temp3)
summary(mod_2) 
confint(mod_2) #Bp is not signif

##change levels with St as outgroup
prelim_temp3$pos_beh <- factor(prelim_temp3$pos_beh, levels = c("St", "Ly", "QS", "QW", "Bp", "Su", "VC"))

mod_2 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                (1|individual), data = prelim_temp3)
summary(mod_2) 
confint(mod_2)

##change levels with Bp as the outgroup
prelim_temp3$pos_beh <- factor(prelim_temp3$pos_beh, levels = c("Bp", "Ly", "Sq", "St", "QW", "QS", "Su", "VC"))

mod_2 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                (1|individual), data = prelim_temp3)
summary(mod_2) 
confint(mod_2) ##QW is not signif.

##check counts again to see differences in QW and QS
ggplot(prelim_temp3, aes(pos_beh))+
  geom_bar(stat="count") + 
  labs(x="Positional Behavior", y="Count") +
  theme_classic() 
              ###there's a lot more QW than QS, but still at least 50 QS

