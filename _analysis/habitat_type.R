##clear environment
#rm(list=ls())

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
levels(prelim_temp4$hab_type)[levels(prelim_temp4$hab_type)=="WD"] <- "WITH SHADE"
levels(prelim_temp4$hab_type)[levels(prelim_temp4$hab_type)=="BMWD"] <- "WITH SHADE"
levels(prelim_temp4$hab_type)[levels(prelim_temp4$hab_type)=="BM"] <- "WITH SHADE"
levels(prelim_temp4$hab_type)[levels(prelim_temp4$hab_type)=="GA"] <- "WITH SHADE"
levels(prelim_temp4$hab_type)[levels(prelim_temp4$hab_type)=="GL"] <- "WITHOUT SHADE"


############
##re-order levels
prelim_temp4$time_od <- factor(prelim_temp4$time_od, levels = c("e_morning", "l_morning", "e_afternoon", "l_afternoon", "evening"))
prelim_temp4$pos_beh <- factor(prelim_temp4$pos_beh, levels = c("Bp", "Ly", "St", "QS", "QW", "Su", "VC"))
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
summary(mod_3) ###still no significant difference with habitat type (shaded vs. not)
confint(mod_3)

summary(prelim_temp4$hab_type) ###WITH SHADE: 762; WITHOUT SHADE: 42

###plot habitat type with vs. without shade

counts_shaded <- ggplot(prelim_temp4, aes(hab_type))+
  geom_bar(stat="count", fill = "mediumpurple1", colour = "mediumpurple1") + 
  ggtitle("Counts of Habitat Type") +
  scale_x_discrete(name = "Habitat Type", 
                   labels = c("WITH SHADE", "WITHOUT SHADE")) +
  scale_y_continuous(name = "Counts") +
  theme_minimal()+
  theme(axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14, face="bold"),
        axis.title.x=element_text(size=14, face="bold"),
        plot.title = element_text(hjust = 0.5, size=17, face="bold"))

counts_shaded

##save plot
ggsave("counts_shaded.pdf", width=10, height=6, units="in")

ggsave("counts_shaded.png", width=10, height=6, units="in")

###frequency of shaded vs. not

freq_hab_type <- ggplot(prelim_temp4, aes(hab_type))+
  geom_bar(aes(y = (..count..)/sum(..count..)), bins = 10, 
                 fill = "mediumpurple1", colour = "mediumpurple1")+
  ggtitle("Frequency of Shaded vs. Non-Shaded Habitat Types") +
  scale_y_continuous(name = "Frequency", labels = percent)+
  scale_x_discrete(name = "Habitat Type")+
  theme_minimal()+
  theme(axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14, face="bold"),
        axis.title.x=element_text(size=14, face="bold"),
        plot.title = element_text(hjust = 0.5, size=17, face="bold"))

freq_hab_type  

##save plot
ggsave("freq_hab_type.pdf", width=10, height=6, units="in")

ggsave("freq_hab_type.png", width=10, height=6, units="in")


###new dataset for open vs. closed habitats
prelim_temp5 <- prelim_temp3
View(prelim_temp5)

#####combine habitat types
levels(prelim_temp5$hab_type)[levels(prelim_temp5$hab_type)=="WD"] <- "OPEN"
levels(prelim_temp5$hab_type)[levels(prelim_temp5$hab_type)=="BMWD"] <- "OPEN"
levels(prelim_temp5$hab_type)[levels(prelim_temp5$hab_type)=="BM"] <- "OPEN"
levels(prelim_temp5$hab_type)[levels(prelim_temp5$hab_type)=="GA"] <- "CLOSED"
levels(prelim_temp5$hab_type)[levels(prelim_temp5$hab_type)=="GL"] <- "OPEN"


############
##re-order levels
prelim_temp5$time_od <- factor(prelim_temp5$time_od, levels = c("e_morning", "l_morning", "e_afternoon", "l_afternoon", "evening"))
prelim_temp5$pos_beh <- factor(prelim_temp5$pos_beh, levels = c("Bp", "Ly", "St", "QS", "QW", "Su", "VC"))
prelim_temp5$hab_type <- factor(prelim_temp5$hab_type, levels = c("OPEN", "CLOSED"))

###change to factor & numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi", "amb_t")

prelim_temp5[factor_cols] <- lapply(prelim_temp5[factor_cols], as.factor)

prelim_temp5[numeric_cols] <- lapply(prelim_temp5[numeric_cols], as.numeric)

str(prelim_temp5)

##model new dataset
mod_4 <- lmer(therm_t ~ pos_beh + amb_t + sun + date + hab_type + 
                (1|individual), data = prelim_temp5)
summary(mod_4) ###still no signif diffs between habitat types; & now QS & Bp are no longer signif
confint(mod_4)

summary(prelim_temp5$hab_type) ##OPEN: 738; CLOSED: 66

###plot habitat type open vs. closed

counts_open <- ggplot(prelim_temp5, aes(hab_type))+
  geom_bar(stat="count", fill = "mediumpurple1", colour = "mediumpurple1") + 
  ggtitle("Counts of Habitat Type") +
  scale_x_discrete(name = "Habitat Type", 
                   labels = c("OPEN", "CLOSED")) +
  scale_y_continuous(name = "Counts") +
  theme_minimal()+
  theme(axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14, face="bold"),
        axis.title.x=element_text(size=14, face="bold"),
        plot.title = element_text(hjust = 0.5, size=17, face="bold"))

counts_open

##save plot
ggsave("counts_open.pdf", width=10, height=6, units="in")

ggsave("counts_open.png", width=10, height=6, units="in")

###frequency of open vs. closed

freq_open <- ggplot(prelim_temp5, aes(hab_type))+
  geom_bar(aes(y = (..count..)/sum(..count..)), bins = 10, 
           fill = "mediumpurple1", colour = "mediumpurple1")+
  ggtitle("Frequency of Open vs. Closed Habitat Types") +
  scale_y_continuous(name = "Percent", labels = percent)+
  scale_x_discrete(name = "Habitat Type")+
  theme_minimal()+
  theme(axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14, face="bold"),
        axis.title.x=element_text(size=14, face="bold"),
        plot.title = element_text(hjust = 0.5, size=17, face="bold"))

freq_open  

##save plot
ggsave("freq_open.pdf", width=10, height=6, units="in")

ggsave("freq_open.png", width=10, height=6, units="in")
