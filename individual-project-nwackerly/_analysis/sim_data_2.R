## simulate! simulate!

# load libraries
library(tidyverse)
library(readr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(dplyr)
library(broom)

##set seed
set.seed(22)

#simulate predictors
npos = 6
nrep = 500
b0 = 37.25
b1 = 0.5
b2 = 1
b3 = -0.5
b4 = -1
b5 = 1.5
sd = 1.5

pos_beh = rep( c("pos_beh1", "pos_beh2" , "pos_beh3", "pos_beh4", "pos_beh5", "pos_beh6"), each = nrep)

#errors
eps = rnorm(npos*nrep, 0, sd)

#simulate response
therm_t = b0 + b1*(pos_beh == "pos_beh2") + b2*(pos_beh == "pos_beh3") + b3*(pos_beh == "pos_beh4") + 
  b4*(pos_beh == "pos_beh5") + b5*(pos_beh == "pos_beh6") + eps 

#combine into a data frame
sim_data <- as.data.frame(cbind(pos_beh, eps, therm_t))
write.csv(sim_data, "_data/_tidy/sim_data.csv")

#fitting the model
simtherm = lm(therm_t ~ pos_beh)
summary(simtherm)

#make it a function

sim_fun = function(nrep = 500, b0 = 37.25, b1 = 0.5, b2 = 1, b3 = -0.5, b4 = -1, b5 = 1.5, sigma = 1.5) {
  npos = 6
  pos_beh = rep( c("pos_beh1", "pos_beh2", "pos_beh3", "pos_beh4", "pos_beh5", "pos_beh6"), each = nrep)
  eps = rnorm(npos*nrep, 0, sigma)
  therm_t = b0 + b1*(pos_beh == "pos_beh2") + b2*(pos_beh == "pos_beh3") + b3*(pos_beh == "pos_beh4") + 
    b4*(pos_beh == "pos_beh5") + b5*(pos_beh == "pos_beh6") + eps
  therm_t_fit = lm(therm_t ~ pos_beh)
  therm_t_fit
}

set.seed(22)
sim_fun()

#repeat simulation
library(purrr)

sims = rerun(1000, sim_fun() )

#look at response
sims %>%
  map_df(tidy) %>%
  filter(term == "pos_behpos_beh2") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) +
  geom_vline( xintercept = 0.5)


sims %>%
  map_df(tidy) %>%
  filter(term == "pos_behpos_beh3") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) +
  geom_vline( xintercept = 1)

sims %>%
  map_df(tidy) %>%
  filter(term == "pos_behpos_beh4") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) +
  geom_vline( xintercept = -0.5)

sims %>%
  map_df(tidy) %>%
  filter(term == "pos_behpos_beh5") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) +
  geom_vline( xintercept = -1)

sims %>%
  map_df(tidy) %>%
  filter(term == "pos_behpos_beh6") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) +
  geom_vline( xintercept = 1.5)
