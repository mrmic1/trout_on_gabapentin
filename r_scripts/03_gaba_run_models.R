#----------------------------------#
# MODELLING
#----------------------------------#

#In this script we run the univariate Gaussian mixed-effects models using brms
#We create run two models
#First model is for distance moved
#Second model is for time spent in shelter


### LOAD LIBRARIES ###
library(tidyverse)
library(brms)
library(performance)
library(rethinking)
library(rstan)

### LOAD DATA ####
gaba_data <- readRDS("./data/gaba_data.rds")

#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#

#Model sample sizes
n_individuals <- length(unique(gaba_data$ID))
sample_sizes <- gaba_data %>% 
  group_by(Treatment, Temp) %>% 
  summarise(trials = n(),
            individuals = length(unique(ID)))
  
print(sample_sizes)


#### > 1.1. Distance moved model ####

#Stage is a predictor. Univariate. Includes three-way interaction

#Define model
dist_moved_mod <- bf(scale(sqrt(DistMove)) ~ Treatment*Temp*Stage + trial_stage_cent + Weight_z + 
                       (0+Stage||gr(ID, by = Treatment:Temp)) + (0 + Stage||gr(Tank, by = Treatment:Temp)), 
                     sigma ~ 0 + Treatment:Temp:Stage, family = gaussian)

#Check prior requirements
get_prior(dist_moved_mod, data = gaba_data)

# Model priors
dist_moved_prior <- c(
  #intercept prior
  set_prior("normal(0,10)", class = "Intercept"),
  #random intercept prior
  set_prior("exponential(1)", class = "sd"),
  #coefficient priors for population-level effects
  set_prior("normal(0,10)", class = "b"))

#Run model
dist_moved_mod_brms<- brm(dist_moved_mod,
                          data = gaba_data,
                          prior = dist_moved_prior,
                          cores = 4,
                          chains = 4,
                          warmup = 1000,
                          iter = 5000,
                          thin = 2,
                          seed = 12345,
                          control = list(max_treedepth = 15, adapt_delta = 0.999),
                          sample_prior = TRUE,
                          save_all_pars = TRUE)


#save and load if required
saveRDS(dist_moved_mod_brms, './models/distance_moved_model.rds')
dist_moved_mod_brms <- readRDS('./models/distance_moved_model.rds')

#Checking mixing of chains
plot(dist_moved_mod_brms, ask = F) 

#Posterior predictive checks.
brms::pp_check(dist_moved_mod_brms) 

#Model fit
performance::r2_bayes(dist_moved_mod_brms, robust = FALSE, ci = 0.89)

#Model summary
print(summary(dist_moved_mod_brms, prob = 0.89), digits = 3) #All Rhat = 1 (good).

#--------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------#

#### > 1.2. Time spent in shelter ####

#Define model
shelter_time_mod <- bf(TimeInShelter_z ~ Treatment*Temp*Stage + trial_stage_cent + Weight_z + 
                       (0+Stage||gr(ID, by = Treatment:Temp)) + (0 + Stage||gr(Tank, by = Treatment:Temp)), 
                     sigma ~ 0 + Treatment:Temp:Stage, family = gaussian)


# Model priors
shelter_time_prior <- c(
  #intercept prior
  set_prior("normal(0,10)", class = "Intercept"),
  #random intercept prior
  set_prior("exponential(1)", class = "sd"),
  #coefficient priors for population-level effects
  set_prior("normal(0,10)", class = "b"))


#Run model
shelter_time_mod_brms<- brm(shelter_time_mod,
                          data = gaba_data,
                          prior = shelter_time_prior,
                          cores = 4,
                          chains = 4,
                          warmup = 1000,
                          iter = 5000,
                          thin = 2,
                          seed = 12345,
                          control = list(max_treedepth = 15, adapt_delta = 0.999),
                          sample_prior = TRUE,
                          save_all_pars = TRUE)


#save or load model if required
saveRDS(shelter_time_mod_brms, './models/shelter_time_mod_brms.rds')
shelter_time_mod_brms <- readRDS('./models/shelter_time_mod_brms.rds')


#Checking mixing of chains
plot(shelter_time_mod_brms, ask = F) #Looks fine

#Posterior predictive checks.
brms::pp_check(shelter_time_mod_brms) #Looks fine

#Model fit
performance::r2_bayes(shelter_time_mod_brms, robust = FALSE, ci = 0.89)

#Model summary
print(summary(shelter_time_mod_brms, prob = 0.89), digits = 3) #All Rhat = 1 (good).

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#

#END OF SCRIPT