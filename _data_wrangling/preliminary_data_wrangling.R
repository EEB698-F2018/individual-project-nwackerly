
### instal packages (if needed) ##
install.packages("tidyverse")
install.packages("lubridate")
install.packages("readr")

# load libraries
library(tidyverse)
library(readr)
library(lubridate)

#read in preliminary data
preliminary_data <- read_csv("_data/preliminary_data.csv")
View(preliminary_data)


##############################################################################################

## Explore data

# show structure of data set
str(preliminary_data)

# Check what variables are included in the dataset:
names(preliminary_data)

# Sumarize all variables:
summary(preliminary_data)

# Look at the first 6 rows of the dataset:
head(preliminary_data)

# Check if there are any missing values: 
summary(is.na(preliminary_data))

#rename thermo-temperature column
names(preliminary_data) <- c("date", "individual","time","pos_beh","context","substrate","hab_type","sun","therm_t","t_lo","t_hi")
names(preliminary_data)

##############################################################################################

# Change "Os" (out of sight) to "NA"
preliminary_data$pos_beh[preliminary_data$pos_beh == "Os"] <- "NA"
preliminary_data$context[preliminary_data$context == "Os"] <- "NA"
preliminary_data$substrate[preliminary_data$substrate == "Os"] <- "NA"
preliminary_data$hab_type[preliminary_data$hab_type == "Os"] <- "NA"
preliminary_data$sun[preliminary_data$sun == "Os"] <- "NA"
preliminary_data$therm_t[preliminary_data$therm_t == "Os"] <- "NA"
preliminary_data$t_lo[preliminary_data$t_lo == "Os"] <- "NA"
preliminary_data$t_hi[preliminary_data$t_hi == "Os"] <- "NA"
preliminary_data


#### Fix capitalization ####

## positional behavior
preliminary_data$pos_beh[preliminary_data$pos_beh == "st"] <- "St"
preliminary_data$pos_beh[preliminary_data$pos_beh == "ST"] <- "St"
preliminary_data$pos_beh[preliminary_data$pos_beh == "ABp"] <- "Abp"
preliminary_data$pos_beh[preliminary_data$pos_beh == "ABpW"] <- "Abp"
preliminary_data$pos_beh[preliminary_data$pos_beh == "Bp"] <- "BpS"
preliminary_data$pos_beh[preliminary_data$pos_beh == "SBp"] <- "BpS"
preliminary_data$pos_beh[preliminary_data$pos_beh == "qw"] <- "QW"
preliminary_data$pos_beh[preliminary_data$pos_beh == "Qw"] <- "QW"

## context
preliminary_data$context[preliminary_data$context == "FO"] <- "Fo"
preliminary_data$context[preliminary_data$context == "fo"] <- "Fo"
preliminary_data$context[preliminary_data$context == "TV"] <- "Tv"
preliminary_data$context[preliminary_data$context == "tv"] <- "Tv"
preliminary_data$context[preliminary_data$context == "SO"] <- "So"
preliminary_data$context[preliminary_data$context == "so"] <- "So"
preliminary_data$context[preliminary_data$context == "re"] <- "RE"
preliminary_data$context[preliminary_data$context == "Re"] <- "RE"

## substrate
preliminary_data$substrate[preliminary_data$substrate == "t"] <- "T"
preliminary_data$substrate[preliminary_data$substrate == "a"] <- "A"
preliminary_data$substrate[preliminary_data$substrate == "W/T"] <- "W"
preliminary_data$substrate[preliminary_data$substrate == "T/W"] <- "W"

## habitat type
preliminary_data$hab_type[preliminary_data$hab_type == "wd"] <- "WD"
preliminary_data$hab_type[preliminary_data$hab_type == "Wd"] <- "WD"
preliminary_data$hab_type[preliminary_data$hab_type == "Gl"] <- "GL"
preliminary_data$hab_type[preliminary_data$hab_type == "gl"] <- "GL"
preliminary_data$hab_type[preliminary_data$hab_type == "Bm"] <- "BM"
preliminary_data$hab_type[preliminary_data$hab_type == "bm"] <- "BM"
preliminary_data$hab_type[preliminary_data$hab_type == "ga"] <- "GA"
preliminary_data$hab_type[preliminary_data$hab_type == "Ga"] <- "GA"

## Individual Names
preliminary_data$individual <- as.character(preliminary_data$individual)
preliminary_data$individual[preliminary_data$individual == "Lp"] <- "LP"
preliminary_data$individual[preliminary_data$individual == "Bi"] <- "BI"
preliminary_data$individual[preliminary_data$individual == "Si"] <- "SI"
preliminary_data$individual[preliminary_data$individual == "Kl"] <- "KL"
preliminary_data$individual[preliminary_data$individual == "Jm"] <- "JM"
preliminary_data$individual[preliminary_data$individual == "Dw"] <- "DW"
preliminary_data$individual[preliminary_data$individual == "Bo"] <- "BO"
preliminary_data$individual[preliminary_data$individual == "Lx"] <- "LX"
preliminary_data$individual[preliminary_data$individual == "Lu"] <- "LU"
preliminary_data$individual[preliminary_data$individual == "Mi"] <- "MI"
preliminary_data$individual[preliminary_data$individual == "Df"] <- "DF"

###################################################################################

## Change to factor and numeric
factor_cols <- c("pos_beh","context", "substrate", "hab_type", "individual")
numeric_cols <- c("sun", "therm_t", "t_lo", "t_hi")
preliminary_data[factor_cols] <- lapply(preliminary_data[factor_cols], as.factor)
preliminary_data[numeric_cols] <- lapply(preliminary_data[numeric_cols], as.numeric)
str(preliminary_data)

#Re-name
preliminary_data2 <- preliminary_data
View(preliminary_data2)

###################################################################################

#Add time of day column
preliminary_data2$time_od <- NA

preliminary_data2$time_od[preliminary_data2$time <= 32400] <- "e_morning"
preliminary_data2$time_od[preliminary_data2$time > 32400 & preliminary_data2$time <= 43200] <- "l_morning"
preliminary_data2$time_od[preliminary_data2$time > 43200 & preliminary_data2$time <= 50400] <- "e_afternoon"
preliminary_data2$time_od[preliminary_data2$time > 50400 & preliminary_data2$time < 61200] <- "l_afternoon"
preliminary_data2$time_od[preliminary_data2$time >= 61200] <- "evening"

#create tidy database for analysis
write.csv(preliminary_data2, "C:/Users/nw185_000/Documents/Iowa/Dissertation/Data/individual-project-nwackerly/_data/_tidy/prelim_data_tidycols.csv", row.names=F)

