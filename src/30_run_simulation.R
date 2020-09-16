# The aim of this code is to run the simulation of 
# an intervention that adjusts the probability of quitting for some individuals

library(data.table)
library(stapmr)
library(tobalcepi)

# Load data ----------------

# Load the prepared data on tobacco consumption
survey_data <- readRDS("intermediate_data/HSE_2001_to_2016_tobacco_imputed.rds")

# Transition probabilities
init_data <- readRDS("intermediate_data/init_data.rds")
quit_data <- readRDS("intermediate_data/quit_data.rds")
relapse_data <- readRDS("intermediate_data/relapse_data.rds")

# Mortality data
mort_data <- readRDS("intermediate_data/tob_mort_data_cause.rds")

# Morbidity data
morb_data <- readRDS("intermediate_data/morb_rates.rds")


##############################################
#In 2015 Public Health England published an evidence review which found that "e-cigarettes
#are significantly less harmful to health than tobacco and have the potential to help
#smokers quit smoking"

#Our intervention is to assume that all NHS Stop Smoking Services therefore started to offer e-cigarettes 
#instead of Nicotine Replacement Therapy between 2016-2020

#Data on use of Stop Smoking Services (SSS) is taken from Webster 2018 Table 7 (p24)
#We assume all smokers using SSS are offered an e-cigarette and that 75% accept this offer
#We assume e-cigarettes are 1.75 times as effective as NRT (from Hajek et al.)

#Adjust quit probabilities accordingly
quit_data_adj1 <- copy(quit_data)

quit_data_adj1[age < 18 & year %in% 2016:2020 & sex=="Male", 
               p_quit := (0.017 * 1.75 * p_quit) + (1-0.017)*p_quit]
quit_data_adj1[age < 35 & year %in% 2016:2020 & sex=="Male", 
               p_quit := (0.016 * 1.75 * p_quit) + (1-0.016)*p_quit]
quit_data_adj1[age < 45 & year %in% 2016:2020 & sex=="Male", 
               p_quit := (0.033 * 1.75 * p_quit) + (1-0.033)*p_quit]
quit_data_adj1[age < 60 & year %in% 2016:2020 & sex=="Male", 
               p_quit := (0.033 * 1.75 * p_quit) + (1-0.033)*p_quit]
quit_data_adj1[age >= 60 & year %in% 2016:2020 & sex=="Male", 
               p_quit := (0.05 * 1.75 * p_quit) + (1-0.05)*p_quit]
quit_data_adj1[age < 18 & year %in% 2016:2020 & sex=="Female", 
               p_quit := (0.039 * 1.75 * p_quit) + (1-0.039)*p_quit]
quit_data_adj1[age < 35 & year %in% 2016:2020 & sex=="Female", 
               p_quit := (0.03 * 1.75 * p_quit) + (1-0.03)*p_quit]
quit_data_adj1[age < 45 & year %in% 2016:2020 & sex=="Female", 
               p_quit := (0.048 * 1.75 * p_quit) + (1-0.048)*p_quit]
quit_data_adj1[age < 60 & year %in% 2016:2020 & sex=="Female", 
               p_quit := (0.048 * 1.75 * p_quit) + (1-0.048)*p_quit]
quit_data_adj1[age >= 60 & year %in% 2016:2020 & sex=="Female", 
               p_quit := (0.046 * 1.75 * p_quit) + (1-0.046)*p_quit]

#Cap quit probabilities at 1
quit_data_adj1[p_quit>1, p_quit:=1]

#Introduce a second scenario where SSS funding is reallocated from more affluent to more deprived areas, 
#halving SSS coverage in IMD quintiles 1 and 2 and increasing it by 50% in quintiles 4 and 5 
#(assuming no change in quintile 3). This incurs a one-off additional cost in 2016 of £20million,
#Which we will assume can be divided equally between imd quintiles.

#Adjust quit probabilities accordingly
quit_data_adj2 <- copy(quit_data)

quit_data_adj2[age < 18 & year %in% 2016:2020 & imd_quintile>=4 & sex=="Male", 
               p_quit := (0.017 * 1.5 * 1.75 * p_quit) + (1-0.017 * 1.5)*p_quit]
quit_data_adj2[age < 18 & year %in% 2016:2020 & imd_quintile==3 & sex=="Male", 
               p_quit := (0.017 * 1.75 * p_quit) + (1-0.017)*p_quit]
quit_data_adj2[age < 18 & year %in% 2016:2020 & imd_quintile<=2 & sex=="Male", 
               p_quit := (0.017 * 0.5 * 1.75 * p_quit) + (1-0.017 * 0.5)*p_quit]
quit_data_adj2[age < 35 & year %in% 2016:2020 & imd_quintile>=4 & sex=="Male", 
               p_quit := (0.016 * 1.5 * 1.75 * p_quit) + (1-0.016 * 1.5)*p_quit]
quit_data_adj2[age < 35 & year %in% 2016:2020 & imd_quintile==3 & sex=="Male", 
               p_quit := (0.016 * 1.75 * p_quit) + (1-0.016)*p_quit]
quit_data_adj2[age < 35 & year %in% 2016:2020 & imd_quintile<=2 & sex=="Male", 
               p_quit := (0.016 * 0.5 * 1.75 * p_quit) + (1-0.016 * 0.5)*p_quit]
quit_data_adj2[age < 45 & year %in% 2016:2020 & imd_quintile>=4 & sex=="Male", 
               p_quit := (0.033 * 1.5 * 1.75 * p_quit) + (1-0.033 * 1.5)*p_quit]
quit_data_adj2[age < 45 & year %in% 2016:2020 & imd_quintile==3 & sex=="Male", 
               p_quit := (0.033 * 1.75 * p_quit) + (1-0.0335)*p_quit]
quit_data_adj2[age < 45 & year %in% 2016:2020 & imd_quintile<=2 & sex=="Male", 
               p_quit := (0.033 * 0.5 * 1.75 * p_quit) + (1-0.033 * 0.5)*p_quit]
quit_data_adj2[age < 60 & year %in% 2016:2020 & imd_quintile>=4 & sex=="Male", 
               p_quit := (0.033 * 1.5 * 1.75 * p_quit) + (1-0.033 * 1.5)*p_quit]
quit_data_adj2[age < 60 & year %in% 2016:2020 & imd_quintile==3 & sex=="Male", 
               p_quit := (0.033 * 1.75 * p_quit) + (1-0.033)*p_quit]
quit_data_adj2[age < 60 & year %in% 2016:2020 & imd_quintile<=2 & sex=="Male", 
               p_quit := (0.033 * 0.5 * 1.75 * p_quit) + (1-0.033 * 0.5)*p_quit]
quit_data_adj2[age >= 60 & year %in% 2016:2020 & imd_quintile>=4 & sex=="Male", 
               p_quit := (0.05 * 1.5 * 1.75 * p_quit) + (1-0.05 * 1.5)*p_quit]
quit_data_adj2[age >= 60 & year %in% 2016:2020 & imd_quintile==3 & sex=="Male", 
               p_quit := (0.05 * 1.75 * p_quit) + (1-0.05)*p_quit]
quit_data_adj2[age >= 60 & year %in% 2016:2020 & imd_quintile<=2 & sex=="Male", 
               p_quit := (0.05 * 0.5 * 1.75 * p_quit) + (1-0.05 * 0.5)*p_quit]
quit_data_adj2[age < 18 & year %in% 2016:2020 & imd_quintile>=4 & sex=="Female", 
               p_quit := (0.039 * 1.5 * 1.75 * p_quit) + (1-0.039 * 1.5)*p_quit]
quit_data_adj2[age < 18 & year %in% 2016:2020 & imd_quintile==3 & sex=="Female", 
               p_quit := (0.039 * 1.75 * p_quit) + (1-0.039)*p_quit]
quit_data_adj2[age < 18 & year %in% 2016:2020 & imd_quintile<=2 & sex=="Female", 
               p_quit := (0.039 * 0.5 * 1.75 * p_quit) + (1-0.039 * 0.5)*p_quit]
quit_data_adj2[age < 35 & year %in% 2016:2020 & imd_quintile>=4 & sex=="Female", 
               p_quit := (0.03 * 1.5 * 1.75 * p_quit) + (1-0.03 * 1.5)*p_quit]
quit_data_adj2[age < 35 & year %in% 2016:2020 & imd_quintile==3 & sex=="Female", 
               p_quit := (0.03 * 1.75 * p_quit) + (1-0.03)*p_quit]
quit_data_adj2[age < 35 & year %in% 2016:2020 & imd_quintile<=2 & sex=="Female", 
               p_quit := (0.03 * 0.5 * 1.75 * p_quit) + (1-0.03 * 0.5)*p_quit]
quit_data_adj2[age < 45 & year %in% 2016:2020 & imd_quintile>=4 & sex=="Female", 
               p_quit := (0.048 * 1.5 * 1.75 * p_quit) + (1-0.048 * 1.5)*p_quit]
quit_data_adj2[age < 45 & year %in% 2016:2020 & imd_quintile==3 & sex=="Female", 
               p_quit := (0.048 * 1.75 * p_quit) + (1-0.048)*p_quit]
quit_data_adj2[age < 45 & year %in% 2016:2020 & imd_quintile<=2 & sex=="Female", 
               p_quit := (0.048 * 0.5 * 1.75 * p_quit) + (1-0.048 * 0.5)*p_quit]
quit_data_adj2[age < 60 & year %in% 2016:2020 & imd_quintile>=4 & sex=="Female", 
               p_quit := (0.048 * 1.5 * 1.75 * p_quit) + (1-0.048 * 1.5)*p_quit]
quit_data_adj2[age < 60 & year %in% 2016:2020 & imd_quintile==3 & sex=="Female", 
               p_quit := (0.048 * 1.75 * p_quit) + (1-0.048)*p_quit]
quit_data_adj2[age < 60 & year %in% 2016:2020 & imd_quintile<=2 & sex=="Female", 
               p_quit := (0.048 * 0.5 * 1.75 * p_quit) + (1-0.048 * 0.5)*p_quit]
quit_data_adj2[age >= 60 & year %in% 2016:2020 & imd_quintile>=4 & sex=="Female", 
               p_quit := (0.046 * 1.5 * 1.75 * p_quit) + (1-0.046 * 1.5)*p_quit]
quit_data_adj2[age >= 60 & year %in% 2016:2020 & imd_quintile==3 & sex=="Female", 
               p_quit := (0.046 * 1.75 * p_quit) + (1-0.046)*p_quit]
quit_data_adj2[age >= 60 & year %in% 2016:2020 & imd_quintile<=2 & sex=="Female", 
               p_quit := (0.046 * 0.5 * 1.75 * p_quit) + (1-0.046 * 0.5)*p_quit]

quit_data_adj2[p_quit>1, p_quit:=1]

# Run simulation ----------------

#######################

scenario1 <- SmokeSim(
  survey_data = survey_data,
  init_data = init_data,
  quit_data = quit_data,
  relapse_data = relapse_data,
  mort_data = mort_data,
  morb_data = morb_data,
  baseline_year = 2002,
  baseline_sample_years = 2001:2003,
  time_horizon = 2050,
  trend_limit_morb = 2016,
  trend_limit_mort = 2016,
  trend_limit_smoke = 2016,
  pop_size = 2e5, # 200,000 people is about the minimum to reduce noise for a single run
  pop_data = stapmr::pop_counts,
  two_arms = TRUE,
  quit_data_adj = quit_data_adj1,
  write_outputs = "output",
  label = "scenario_1"
)

# Check the "output" folder for the saved model outputs
# these are forecast individual-level data on smoking
# and forecast mortality and morbidity rates


scenario2 <- SmokeSim(
  survey_data = survey_data,
  init_data = init_data,
  quit_data = quit_data,
  relapse_data = relapse_data,
  mort_data = mort_data,
  morb_data = morb_data,
  baseline_year = 2002,
  baseline_sample_years = 2001:2003,
  time_horizon = 2050,
  trend_limit_morb = 2016,
  trend_limit_mort = 2016,
  trend_limit_smoke = 2016,
  pop_size = 2e5, # 200,000 people is about the minimum to reduce noise for a single run
  pop_data = stapmr::pop_counts,
  two_arms = TRUE,
  quit_data_adj = quit_data_adj2,
  write_outputs = "output",
  label = "scenario_2"
)







