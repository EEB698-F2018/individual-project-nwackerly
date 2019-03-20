# load libraries
library(tidyverse)
library(readr)
library(lubridate)
library(magrittr)
library(ggplot2)

## simulate response first
Bp_therm_t <- rnorm(n=1500, mean=c(), sd=c()) #bipedalism on temp
AH_therm_t <- rnorm(n=1500, mean=c(), sd=c()) #arm-hang positions on temp
QM_therm_t <- rnorm(n=1500, mean=c(), sd=c()) #quadrumanous positions on temp
QW_therm_t <- rnorm(n=1500, mean=c(), sd=c()) #quadrupedal locomotion on temp
QP_therm_t <- rnorm(n=1500, mean=c(), sd=c()) #quadrupedal posture on temp
IP_therm_t <- rnorm(n=1500, mean=c(), sd=c()) #inactive postures on temp
therm_t_response <- c(Bp_therm_t, AH_therm_t, QM_therm_t, QW_therm_t, QP_therm_t, IP_therm_t)

## simulate predictors
hab_type <- rep(c("WD", "GL", "GA", "BM", "BMWD"), each = 300)
a_temp 


## simstudy
def <- defData(varname = "sit", dist = "nonrandom", formula = 97, id = "idnum")
def <- defData(def, varname = "a_temp", dist = "uniform", formula = "70;110")
def <- defData(def, varname = "therm_t", formula = "sit + a_temp", variance = 8)

dt <- genData(1000, def)
dt

addef <- defDataAdd(varname = "a_temp", dist = "normal", formula = "3 + therm_t", 
                    variance = 2)

dt <- addColumns(addef, dt)
dt

#####
def <- defData(varname = "male", dist = "binary", formula = 0.5, id = "cid")
def <- defData(def, varname = "over65", dist = "binary", formula = "-1.7 + .8*male", 
               link = "logit")
def <- defData(def, varname = "baseDBP", dist = "normal", formula = 70, variance = 40)

study1 <- trtAssign(dtstudy, n = 3, balanced = TRUE, strata = c("male", "over65"), 
                    grpName = "rxGrp")

study1
dtstudy <- genData(330, def)
formula1 <- c("-2 + 2*male - .5*over65", "-1 + 2*male + .5*over65")
dtExp <- trtObserve(dtstudy, formulas = formula1, logit.link = TRUE, grpName = "exposure")
dtExp

formula2 <- c(0.35, 0.45)

dtExp2 <- trtObserve(dtstudy, formulas = formula2, logit.link = FALSE, grpName = "exposure")
dtExp2
