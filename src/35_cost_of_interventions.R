
# The aim of this code is to calculate the number of interventions given
# and the cumulative cost of interventions
# and to stratify this number by age, sex and IMD quintile

library(stapmr)
library(tobalcepi)
library(ggplot2)

# Load the simulated population
smoke_data1 <- ReadSim(root = "output/smk_data_", label = "scenario_1", two_arms = TRUE)
smoke_data2 <- ReadSim(root = "output/smk_data_", label = "scenario_2", two_arms = TRUE)

# Summarise the smoking outcomes by population strata
smoke_stats1 <- SmkEffects(
  data = smoke_data1,
  strat_vars = c("year", "age", "sex", "imd_quintile"),
  two_arms = TRUE)

smoke_stats2 <- SmkEffects(
  data = smoke_data2,
  strat_vars = c("year", "age", "sex", "imd_quintile"),
  two_arms = TRUE)

# To calculate the number of interventions given

#In this scenario, no interventions (i.e. e-cigarettes) were given to the control arm,
#so we only need to calculate intervention delivery in the intervention arms

# Select just the control arm
smoke_stats_scenario_1 <- smoke_stats1[arm == "treatment"]
smoke_stats_scenario_2 <- smoke_stats2[arm == "treatment"]

# The characteristics of the intervention are described in the code file '30_run_simulation.R'

# All smokers attending SSS between 2016 and 2020 were offered the intervention, 
#SSS attendance varies by age and sex in scenario 1 and also by IMD in scenario 2
smoke_stats_scenario_1[ , offered_intervention := 0]
smoke_stats_scenario_1[year %in% 2016:2020 & age <18 & sex=="Male", offered_intervention := n_smokers * 0.017]
smoke_stats_scenario_1[year %in% 2016:2020 & age %in% 18:34 & sex=="Male", offered_intervention := n_smokers * 0.016]
smoke_stats_scenario_1[year %in% 2016:2020 & age %in% 35:44 & sex=="Male", offered_intervention := n_smokers * 0.033]
smoke_stats_scenario_1[year %in% 2016:2020 & age %in% 45:59 & sex=="Male", offered_intervention := n_smokers * 0.033]
smoke_stats_scenario_1[year %in% 2016:2020 & age >=60 & sex=="Male", offered_intervention := n_smokers * 0.05]
smoke_stats_scenario_1[year %in% 2016:2020 & age <18 & sex=="Female", offered_intervention := n_smokers * 0.039]
smoke_stats_scenario_1[year %in% 2016:2020 & age %in% 18:34 & sex=="Female", offered_intervention := n_smokers * 0.03]
smoke_stats_scenario_1[year %in% 2016:2020 & age %in% 35:44 & sex=="Female", offered_intervention := n_smokers * 0.048]
smoke_stats_scenario_1[year %in% 2016:2020 & age %in% 45:59 & sex=="Female", offered_intervention := n_smokers * 0.043]
smoke_stats_scenario_1[year %in% 2016:2020 & age >=60 & sex=="Female", offered_intervention := n_smokers * 0.046]

smoke_stats_scenario_2[ , offered_intervention := 0]
smoke_stats_scenario_2[year %in% 2016:2020 & age <18 & sex=="Male" & imd_quintile>=4, offered_intervention := n_smokers * 0.017 * 1.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 18:34 & sex=="Male" & imd_quintile>=4, offered_intervention := n_smokers * 0.016 * 1.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 35:44 & sex=="Male" & imd_quintile>=4, offered_intervention := n_smokers * 0.033 * 1.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 45:59 & sex=="Male" & imd_quintile>=4, offered_intervention := n_smokers * 0.033 * 1.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age >=60 & sex=="Male" & imd_quintile>=4, offered_intervention := n_smokers * 0.05 * 1.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age <18 & sex=="Female" & imd_quintile>=4, offered_intervention := n_smokers * 0.039 * 1.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 18:34 & sex=="Female" & imd_quintile>=4, offered_intervention := n_smokers * 0.03 * 1.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 35:44 & sex=="Female" & imd_quintile>=4, offered_intervention := n_smokers * 0.048 * 1.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 45:59 & sex=="Female" & imd_quintile>=4, offered_intervention := n_smokers * 0.043 * 1.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age >=60 & sex=="Female" & imd_quintile>=4, offered_intervention := n_smokers * 0.046 * 1.5]

smoke_stats_scenario_2[year %in% 2016:2020 & age <18 & sex=="Male" & imd_quintile==3, offered_intervention := n_smokers * 0.017]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 18:34 & sex=="Male" & imd_quintile==3, offered_intervention := n_smokers * 0.016]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 35:44 & sex=="Male" & imd_quintile==3, offered_intervention := n_smokers * 0.033]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 45:59 & sex=="Male" & imd_quintile==3, offered_intervention := n_smokers * 0.033]
smoke_stats_scenario_2[year %in% 2016:2020 & age >=60 & sex=="Male" & imd_quintile==3, offered_intervention := n_smokers * 0.0]
smoke_stats_scenario_2[year %in% 2016:2020 & age <18 & sex=="Female" & imd_quintile==3, offered_intervention := n_smokers * 0.039]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 18:34 & sex=="Female" & imd_quintile==3, offered_intervention := n_smokers * 0.03]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 35:44 & sex=="Female" & imd_quintile==3, offered_intervention := n_smokers * 0.048]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 45:59 & sex=="Female" & imd_quintile==3, offered_intervention := n_smokers * 0.043]
smoke_stats_scenario_2[year %in% 2016:2020 & age >=60 & sex=="Female" & imd_quintile==3, offered_intervention := n_smokers * 0.0465]

smoke_stats_scenario_2[year %in% 2016:2020 & age <18 & sex=="Male" & imd_quintile<=2, offered_intervention := n_smokers * 0.017 * 0.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 18:34 & sex=="Male" & imd_quintile<=2, offered_intervention := n_smokers * 0.016 * 0.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 35:44 & sex=="Male" & imd_quintile<=2, offered_intervention := n_smokers * 0.033 * 0.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 45:59 & sex=="Male" & imd_quintile<=2, offered_intervention := n_smokers * 0.033 * 0.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age >=60 & sex=="Male" & imd_quintile<=2, offered_intervention := n_smokers * 0.05 * 0.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age <18 & sex=="Female" & imd_quintile<=2, offered_intervention := n_smokers * 0.039 * 0.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 18:34 & sex=="Female" & imd_quintile<=2, offered_intervention := n_smokers * 0.03 * 0.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 35:44 & sex=="Female" & imd_quintile<=2, offered_intervention := n_smokers * 0.048 * 0.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age %in% 45:59 & sex=="Female" & imd_quintile<=2, offered_intervention := n_smokers * 0.043 * 0.5]
smoke_stats_scenario_2[year %in% 2016:2020 & age >=60 & sex=="Female" & imd_quintile<=2, offered_intervention := n_smokers * 0.046 * 0.5]

# Total number of interventions given
sum(smoke_stats_scenario_1$offered_intervention)
sum(smoke_stats_scenario_2$offered_intervention)

# By IMD quintile
smoke_stats_scenario_1[ , .(n_offered = sum(offered_intervention)), by = c("imd_quintile")]
smoke_stats_scenario_2[ , .(n_offered = sum(offered_intervention)), by = c("imd_quintile")]

# Note that in this simple intervention scenario,
# any individual smoker could have been offered the intervention once in each year

#We assume that all smokers offered the intervention accept the offer

# By IMD quintile and year
n_offered_year_imd1 <- smoke_stats_scenario_1[ , .(n_offered = sum(offered_intervention)), by = c("imd_quintile", "year")]
n_offered_year_imd2 <- smoke_stats_scenario_2[ , .(n_offered = sum(offered_intervention)), by = c("imd_quintile", "year")]

# Plot interventions accepted by scenario
n_offered_year_imd1[,arm:="Scenario 1"]
n_offered_year_imd2[,arm:="Scenario 2"]

n_offered_year_imd <- rbind(n_offered_year_imd1, n_offered_year_imd2)

png("output/interventions_accepted_year_imd.png", units="in", width=7, height=7, res=300)
ggplot(n_offered_year_imd) +
  geom_col(aes(x = year, y = n_offered, fill = imd_quintile), size = .4) +
  scale_fill_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177")) +
  ylab("Number of interventions") +
  theme_minimal() +
  facet_wrap(~arm)+
  labs(title = "Number of interventions delivered",
       #subtitle = "", 
       caption = "")
dev.off()


############################################################################
# Cost of intervention

# Assume that each intervention cost £30.25 based on Hajek et al.
#(n.b. we are assuming this is the marginal additional cost of e-cigarette over
#and above the support they would othewise have received from SSS in the control arm)
n_offered_year_imd[,raw_cost:=n_offered*30.25]

#Add in the £20million cost for scenario 2 in 2016 only, which equates to £4m per IMD quintile
n_offered_year_imd[year==2016 & arm=="Scenario 2", raw_cost:=raw_cost+4000000]

saveRDS(n_offered_year_imd, "output/intervention_delivery_costs_by_imd.rds")

#Calculate cumulative (undiscounted) intervention delivery costs
n_offered_year_imd[ , cum_intervention_cost := cumsum(raw_cost), by = c("imd_quintile", "arm")]

# Plot cumulative intervention cost
png("output/intervention_cost_year_imd.png", units="in", width=7, height=7, res=300)
ggplot(n_offered_year_imd) +
  geom_line(aes(x = year, y = cum_intervention_cost/1000000, colour = imd_quintile), size = .4) +
  scale_colour_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177")) +
  ylab("Cost of interventions (£m)") +
  facet_wrap(~arm)+
  theme_minimal() +
  labs(title = "Cumulative cost of interventions",
       #subtitle = "", 
       caption = "")
dev.off()