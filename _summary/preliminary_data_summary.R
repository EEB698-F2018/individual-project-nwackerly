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
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi")
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

## filter by bipedalism 
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

### Join bipedal data
join_AS_bipedal_data <- full_join(data_A_bipedal, data_bipedal_S, c("date", "individual", "time", "pos_beh", "context", "substrate", "hab_type", "sun", "therm_t", "t_lo", "t_hi"))
View(join_AS_bipedal_data)

full_join_bipedal <- full_join(join_AS_bipedal_data, data_bipedal_w, c("date", "individual", "time", "pos_beh", "context", "substrate", "hab_type", "sun", "therm_t", "t_lo", "t_hi"))
View(full_join_bipedal)

write.csv(full_join_bipedal, "C:/Users/nw185_000/Documents/Iowa/Dissertation/Data/individual-project-nwackerly/_summary/bipedal_table.csv", row.names=F)
##################################################################

## Summarize bipedal data
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


## summary of bipedalism by individual
summary(data_A_bipedal$individual)
summary(data_bipedal_S$individual)
summary(data_bipedal_w$individual)
summary(full_join_bipedal$individual) 

################################################################################################

### Separate data by individual
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

