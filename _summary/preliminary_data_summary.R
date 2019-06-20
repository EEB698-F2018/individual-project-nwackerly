##summary data

# load libraries
library(tidyverse)
library(readr)
library(lubridate)
library(magrittr)

#read in preliminary data
prelim_tidy <- read_csv("_data/_tidy/prelim_data_tidycols.csv")
View(prelim_tidy)


## Change to factor and numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual", "time_od")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi", "amb_t")
prelim_tidy[factor_cols] <- lapply(prelim_tidy[factor_cols], as.factor)
prelim_tidy[numeric_cols] <- lapply(prelim_tidy[numeric_cols], as.numeric)
str(prelim_tidy)
dim(prelim_tidy)

## summarize
summary(prelim_tidy)
summary(prelim_tidy$pos_beh)
summary(prelim_tidy$context)

#remove all NA of thermo-temp
therm_t2 <- na.omit(prelim_tidy$therm_t) 

meantherm_t <- prelim_tidy %>%
  summarize(mean(therm_t2))
  
meantherm_t
summary(therm_t2)

#remove all NA of context
context_2 <- na.omit(prelim_tidy$context)

summary(context_2)

#####filter by date ######
June_6 <- prelim_tidy %>%
  filter(date == "2018-06-06")
June_6 <- na.omit(June_6$therm_t)
View(June_6) ##60 - number of thermo-images taken on this date

June_8 <- prelim_tidy %>%
  filter(date == "2018-06-08")
June_8 <- na.omit(June_8$therm_t)
View(June_8) ##54

June_12 <- prelim_tidy %>%
  filter(date == "2018-06-12")
June_12 <- na.omit(June_12$therm_t)
View(June_12) ##49

June_14 <- prelim_tidy %>%
  filter(date == "2018-06-14")
June_14 <- na.omit(June_14$therm_t)
View(June_14) ##27, rainy day

June_17 <- prelim_tidy %>%
  filter(date == "2018-06-17")
June_17 <- na.omit(June_17$therm_t)
View(June_17) ##40

June_19 <- prelim_tidy %>%
  filter(date == "2018-06-19")
June_19 <- na.omit(June_19$therm_t)
View(June_19) ##67

June_21 <- prelim_tidy %>%
  filter(date == "2018-06-21")
June_21 <- na.omit(June_21$therm_t)
View(June_21) ##35

June_23 <- prelim_tidy %>%
  filter(date == "2018-06-23")
June_23 <- na.omit(June_23$therm_t)
View(June_23) ##40

June_26 <- prelim_tidy %>%
  filter(date == "2018-06-26")
June_26 <- na.omit(June_26$therm_t)
View(June_26) ##28, rested most of the day; it was a hot one!

June_28 <- prelim_tidy %>%
  filter(date == "2018-06-28")
June_28 <- na.omit(June_28$therm_t)
View(June_28) ##64

July_1 <- prelim_tidy %>%
  filter(date == "2018-07-01")
July_1 <- na.omit(July_1$therm_t)
View(July_1) ##23, female (LU) with large swelling, males were all over her all day; hard to get images

July_3 <- prelim_tidy %>%
  filter(date == "2018-07-03")
July_3 <- na.omit(July_3$therm_t)
View(July_3) ##39

July_5 <- prelim_tidy %>%
  filter(date == "2018-07-05")
July_5 <- na.omit(July_5$therm_t)
View(July_5) ##39

July_7 <- prelim_tidy %>%
  filter(date == "2018-07-07")
July_7 <- na.omit(July_7$therm_t)
View(July_7) ##zero -- day I forgot to put memory card in camera, so zero images taken
##this day won't count in min/max analysis

July_8 <- prelim_tidy %>%
  filter(date == "2018-07-08")
July_8 <- na.omit(July_8$therm_t)
View(July_8) ##49

July_10 <- prelim_tidy %>%
  filter(date == "2018-07-10")
July_10 <- na.omit(July_10$therm_t)
View(July_10) ##36

July_12 <- prelim_tidy %>%
  filter(date == "2018-07-12")
July_12 <- na.omit(July_12$therm_t)
View(July_12) ##48

July_14 <- prelim_tidy %>%
  filter(date == "2018-07-14")
July_14 <- na.omit(July_14$therm_t)
View(July_14) ##65

July_16 <- prelim_tidy %>%
  filter(date == "2018-07-16")
July_16 <- na.omit(July_16$therm_t)
View(July_16) ##44

#creating a matrix of these data
date_Y <- c("2018-06-06", "2018-06-08", "2018-06-12", "2018-06-14", "2018-06-17", "2018-06-19", 
          "2018-06-21", "2018-06-23", "2018-06-26", "2018-06-28", "2018-07-01", "2018-07-03", 
          "2018-07-05", "2018-07-08", "2018-07-10", "2018-07-12", "2018-07-14", "2018-07-16")
therm_num_X <- c(60, 54, 49, 27, 40, 67, 35, 40, 28, 64, 23, 39, 39, 49, 36, 48, 65, 44)

#check length of vectors
length(date_Y); length(therm_num_X)

#view each vector
date_Y
therm_num_X

#bind as a matrix
d_matrix <- cbind(date_Y, therm_num_X)
d_matrix

#get results
min(therm_num_X) #23
mean(therm_num_X) #44.83
max(therm_num_X) #67
median(therm_num_X) #42

##### filter by bipedalism ######
data_A_bipedal <- prelim_tidy %>%
  filter(pos_beh == "Abp")

data_bipedal_S <- prelim_tidy %>%
  filter(pos_beh == "BpS")

data_bipedal_w <- prelim_tidy %>%
  filter(pos_beh == "BpW")

### Assisted bipedalism filtered by context
data_A_bipedal_Fo <- prelim_tidy %>%
  filter(pos_beh == "Abp", context == "Fo")

data_A_bipedal_Tv <- prelim_tidy %>%
  filter(pos_beh == "Abp", context == "Tv")

data_A_bipedal_So <- prelim_tidy %>%
  filter(pos_beh == "Abp", context == "So")

### Unassisted bipedalism filtered by context
data_bipedal_S_Fo <- prelim_tidy %>%
  filter(pos_beh == "BpS", context == "Fo")

data_bipedal_S_Tv <- prelim_tidy %>%
  filter(pos_beh == "BpS", context == "Tv")

data_bipedal_S_So <- prelim_tidy %>%
  filter(pos_beh == "BpS", context == "So")

### Bipedal walk filtered by context
data_bipedal_w_Fo <- prelim_tidy %>%
  filter(pos_beh == "BpW", context == "Fo")

data_bipedal_w_Tv <- prelim_tidy %>%
  filter(pos_beh == "BpW", context == "Tv")

data_bipedal_w_So <- prelim_tidy %>%
  filter(pos_beh == "BpW", context == "So")

### Join bipedal data #####
join_AS_bipedal_data <- full_join(data_A_bipedal, data_bipedal_S, c("date", "individual", "time", "pos_beh", "context", "substrate", "hab_type", "sun", "therm_t", "t_lo", "t_hi"))
View(join_AS_bipedal_data)

full_join_bipedal <- full_join(join_AS_bipedal_data, data_bipedal_w, c("date", "individual", "time", "pos_beh", "context", "substrate", "hab_type", "sun", "therm_t", "t_lo", "t_hi"))
View(full_join_bipedal)

write.csv(full_join_bipedal, "C:/Users/nw185_000/Documents/Iowa/Dissertation/Data/individual-project-nwackerly/_summary/bipedal_table.csv", row.names=F)
##################################################################

## Summarize bipedal data #####
summary(data_A_bipedal)
  summary(data_A_bipedal_Fo)  ### assisted bipedalism by context (Fo = forage; So = Social; Tv = travel)
  summary(data_A_bipedal_So)
  summary(data_A_bipedal_Tv)
summary(data_bipedal_S)
  summary(data_bipedal_S_Fo)  ### unassisted bipedalism by context (Fo = forage; So = Social; Tv = travel)
  summary(data_bipedal_S_So)
  summary(data_bipedal_S_Tv)
summary(data_bipedal_w)
  summary(data_bipedal_w_Fo)  ### Bipedal Walk by context (Fo = forage; So = Social; Tv = travel)
  summary(data_bipedal_w_So)
  summary(data_bipedal_w_Tv)
summary(full_join_bipedal)


## summary of bipedalism by individual #####
summary(data_A_bipedal$individual)
summary(data_bipedal_S$individual)
summary(data_bipedal_w$individual)
summary(full_join_bipedal$individual) 

################################################################################################

### Separate data by individual #####
## Bandit
data_bandit <- prelim_tidy %>%
  filter(individual == "BN")

## Bilbo
data_bilbo <- prelim_tidy %>%
  filter(individual == "BI")

## BO
data_bo <- prelim_tidy %>%
  filter(individual == "BO")

## Diouf
data_diouf <- prelim_tidy %>%
  filter(individual == "DF")

## Dawson
data_dawson <- prelim_tidy %>%
  filter(individual == "DW")

## Jumkin
data_jumkin <- prelim_tidy %>%
  filter(individual == "JM")

## KL
data_kl <- prelim_tidy %>%
  filter(individual == "KL")

## Lupin
data_lupin <- prelim_tidy %>%
  filter(individual == "LP")

## Luthor
data_luthor <- prelim_tidy %>%
  filter(individual == "LT")

## Lex
data_lex <- prelim_tidy %>%
  filter(individual == "LX")

## Mike
data_mike <- prelim_tidy %>%
  filter(individual == "MI")

## Siberut
data_siberut <- prelim_tidy %>%
  filter(individual == "SI")

##################################################################

## Thermo-temp by individual
summary(data_bilbo$therm_t)
summary(data_bandit$therm_t)
summary(data_bo$therm_t)
summary(data_diouf$therm_t)
summary(data_dawson$therm_t)
summary(data_jumkin$therm_t)
summary(data_kl$therm_t)
summary(data_lex$therm_t)
summary(data_lupin$therm_t)
summary(data_luthor$therm_t)
summary(data_mike$therm_t)
summary(data_siberut$therm_t)

####################################################################
#Omit any missing values:
summary(is.na(prelim_tidy)) #looks like missing lots of therm data

prelim_temp<-na.omit(prelim_tidy)
View(prelim_temp)
nrow(prelim_temp) #810 rows; most rows were removed bc therm data missing. 

prelim_temp$therm_t[prelim_temp$therm_t >= 42] <- NA #remove therm values >42 because likely a temp sensor problem? 6 rows removed.  

prelim_temp<-na.omit(prelim_temp) #remove those rows with NA's
View(prelim_temp)
nrow(prelim_temp) #now 804 rows

#Combine positional behaviors into smaller categories
# suspensory = Cb, Br & QM
#bipedalism = BpS, BpW & Abp
#Sit = St & Sq
#Vertical climb and cling together
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
prelim_temp$pos_beh <- factor(prelim_temp$pos_beh, levels = c("QS", "Ly", "Sq", "St", "QW", "Bp", "Su", "VC"))

## Change to factor and numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi", "amb_t")

prelim_temp[factor_cols] <- lapply(prelim_temp[factor_cols], as.factor)

prelim_temp[numeric_cols] <- lapply(prelim_temp[numeric_cols], as.numeric)

str(prelim_temp)

##########################################################
## summary stats ######
summary(prelim_temp) 
summary(prelim_temp$pos_beh) ## QS: 48, QW: 167, Bp:160, St:326, Ly:68
summary(prelim_temp$hab_type) ##hab_type: BM: 5, BMWD: 56, GA: 66, GL: 42, WD: 635

#thermo-temp summary stats
meantherm_t <- prelim_temp %>%
  summarize(mean(therm_t))

meantherm_t ##mean: 34.1
summary(prelim_temp$therm_t) ##mean: 34.14, median: 34; Max: 41.6, Min: 26.1

max(prelim_temp$therm_t) - min(prelim_temp$therm_t)  ##Range: 15.5; measure of how far apart the entire data spreads in value
IQR(prelim_temp$therm_t) ###interquartile range: 3.7; measure of how far apart the middle portion of data spreads in value

##ambient temp summary stats
meanamb_t <- prelim_temp %>%
  summarize(mean(amb_t))

meanamb_t ##mean: 29.8
summary(prelim_temp$amb_t) #mean: 29.81, median: 30.10; Max: 36.5, Min: 19.4

max(prelim_temp$amb_t) - min(prelim_temp$amb_t)  ##Range: 17.1
IQR(prelim_temp$amb_t) ##5.9

#################################################################
#Time of Day summary
summary(prelim_temp$time_od) ##e_morn: 198; l_morn: 192; e_after: 93; l_after:188; evening:133

counts_time_od <- ggplot(prelim_temp, aes(time_od))+
  geom_bar(stat="count", fill = "mediumpurple1", colour = "mediumpurple1") + 
  ggtitle("Amount in each Time of Day") +
  scale_x_discrete(name = "Time of Day", 
                   labels = c("Early Morning", "Late Morning", "Early Afternoon", "Late Afternoon", 
                              "Evening")) +
  scale_y_continuous(name = "Counts") +
  theme_minimal()+
  theme(axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14, face="bold"),
        axis.title.x=element_text(size=14, face="bold"),
        plot.title = element_text(hjust = 0.5, size=17, face="bold"))
counts_time_od
##doesn't look too skewed; just a little less in the early afternoon, but by less than 100

##### filter time of day ######
data_e_morning <- prelim_temp %>%
  filter(time_od == "e_morning")

data_l_morning <- prelim_temp %>%
  filter(time_od == "l_morning")

data_e_afternoon <- prelim_temp %>%
  filter(time_od == "e_afternoon")

data_l_afternoon <- prelim_temp %>%
  filter(time_od == "l_afternoon")

data_evening <- prelim_temp %>%
  filter(time_od == "evening")

summary(data_e_morning$amb_t)
summary(data_l_morning$amb_t)
summary(data_e_afternoon$amb_t)
summary(data_l_afternoon$amb_t)
summary(data_evening$amb_t)

##plot of time of day against ambient temp
ggplot(prelim_temp, aes(time_od, amb_t)) +
  geom_boxplot() + 
  labs(x="Time of Day", y="Ambient Temperature") +
  theme_classic()

#######################################################
#####Add time of day column
prelim_temp$time_od2 <- NA

prelim_temp$time_od2[prelim_temp$time <= 43200] <- "morning"
prelim_temp$time_od2[prelim_temp$time > 43200 & prelim_temp$time < 61200] <- "afternoon"
prelim_temp$time_od2[prelim_temp$time >= 61200] <- "evening"
prelim_temp$time_od2 <- factor(prelim_temp$time_od2, levels = c("morning", "afternoon", "evening"))
levels(prelim_temp$time_od2)
View(prelim_temp)

##filter time_od2
data_morning <- prelim_temp %>%
  filter(time_od2 == "morning")

data_afternoon <- prelim_temp %>%
  filter(time_od2 == "afternoon")

data_evening <- prelim_temp %>%
  filter(time_od2 == "evening")

summary(data_morning$amb_t)
summary(data_afternoon$amb_t)
summary(data_evening$amb_t)


### time of day & therm_t