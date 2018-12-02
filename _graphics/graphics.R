# load libraries
library(tidyverse) 
library(readr) 
library(lubridate)
library(magrittr)
library(ggplot2)
library(car)
library(emmeans)
library(lme4)
library(ggthemes)
library(plyr)

##read in data
prelim_tidy <- read_csv("_data/_tidy/prelim_data_tidycols.csv")
View(prelim_tidy)

#Omit any missing values:
summary(is.na(prelim_tidy))

prelim_temp<-na.omit(prelim_tidy)
View(prelim_temp)

prelim_temp$therm_t[prelim_temp$therm_t >= 42] <- NA

View(prelim_temp)

prelim_temp<-na.omit(prelim_temp)
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
prelim_temp$pos_beh <- factor(prelim_temp$pos_beh, levels = c("QS", "Ly", "Sq", "St", "QW", "Bp", "Su", "VC"))

## Change to factor and numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi")

prelim_temp[factor_cols] <- lapply(prelim_temp[factor_cols], as.factor)

prelim_temp[numeric_cols] <- lapply(prelim_temp[numeric_cols], as.numeric)

str(prelim_temp)

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
confint(m1)
#m1_res <- tidy(coef(summary(m1)) ) 

#ggplot(m1_res, aes(pos_beh, therm_t)) +
  #geom_point()


ggplot(prelim_temp, aes(pos_beh, therm_t)) + 
  #geom_point(size = 3) +
  geom_point(aes(y=predict(m1)), size = 1)

#change levels for the plot
prelim_temp$pos_beh <- factor(prelim_temp$pos_beh, levels = c("Ly","St", "Sq", "QS", "QW", "Bp", "Su", "VC"))

ggplot(prelim_temp, aes(pos_beh, therm_t)) + 
  geom_point(size = 3) +
  geom_boxplot(aes(y=predict(m1)), size = 1, colour = "red") +
  scale_x_discrete(name = "Positional Behavior", labels = c("Lie", "Sit", "Squat",
                        "Quad. Stand", "Quad. Walk", "Bipedal", "Suspensory", "Vert. Climb/Cling")) +
  scale_y_continuous(name = "Body Temperature") +
  theme_classic()


##change levels back
prelim_temp$pos_beh <- factor(prelim_temp$pos_beh, levels = c("QS", "Ly", "Sq", "St", "QW", "Bp", "Su", "VC"))


library(nlme)

##tried another thing and it didn't work...
#newdat <- expand.grid(pos_beh=unique(prelim_temp$pos_beh), 
                      #time_od=unique(prelim_temp$time_od),
                      #sun=c(min(prelim_temp$sun),
                       #     max(prelim_temp$sun)),
                      #date=date(prelim_temp$date),
                      #hab_type=unique(prelim_temp$hab_type))
                      
#ggplot(prelim_temp, aes(x=pos_beh, y=therm_t)) +
 # geom_point(size=3) +
  #geom_line(data=newdat, aes(y=predict(m1, level=0, newdata=newdat), size="Positional Behavior")) +
  #scale_size_manual(name="Predictions", values=c("Positional Behavior"=3)) +
  #theme_classic() 


##attempting to follow tutorial but got too confused....
preddata <- with(prelim_temp, expand.grid(pos_beh = levels(pos_beh), time_od = levels(time_od), 
                                          sun = 50, date = "2018-06-28", hab_type = "WD"))

###
factor_cols <- c("pos_beh", "time_od", "hab_type")
numeric_cols <- c("sun")

preddata[factor_cols] <- lapply(preddata[factor_cols], as.factor)

preddata[numeric_cols] <- lapply(preddata[numeric_cols], as.numeric)

preddata$date <- as.Date(preddata$date)

str(preddata)
########
mm <- model.matrix(~pos_beh + time_od + sun + date + hab_type, data=preddata)
###won't work because I don't have 2 or more levels per factor

pframe2 <- data.frame(preddata,eta=mm%*%fixef(m1))
pframe2 <- with(pframe2,data.frame(pframe2,prop=eta))
pvar1 <- diag(mm %*% tcrossprod(vcov(m1),mm))
tvar1 <- pvar1+VarCorr(m1)$individual
pframe2 <- data.frame(
  pframe2
  , plo = pframe2$eta-2*sqrt(pvar1)
  , phi = pframe2$eta+2*sqrt(pvar1)
  , tlo = pframe2$eta-2*sqrt(tvar1)
  , thi = pframe2$eta+2*sqrt(tvar1)
)

