
# The aim of this code is to make some basic plots to 
# view the effects of the intervention on smoking trajectories

library(data.table)
library(stapmr)
library(ggplot2)


# Policy effects on smoking prevalence by imd quintile and sex

smk_data1 <- ReadSim(root = "output/smk_data_", two_arms = TRUE, label = "scenario_1")
smk_data2 <- ReadSim(root = "output/smk_data_", two_arms = TRUE, label = "scenario_2")

smoke_stats1 <- SmkEffects(
  data = smk_data1,
  strat_vars = c("year", "imd_quintile", "sex"),
  two_arms = TRUE)

smoke_stats2 <- SmkEffects(
  data = smk_data2,
  strat_vars = c("year", "imd_quintile", "sex"),
  two_arms = TRUE)

saveRDS(smoke_stats1, "output/smoke_prev_by_year_imd_sex_1.rds")
saveRDS(smoke_stats2, "output/smoke_prev_by_year_imd_sex_2.rds")

#Combine policies into a single data table (control is the same for both arms)
smoke_stats1[arm=="treatment", arm:="scenario_1"]
smoke_stats2[arm=="treatment", arm:="scenario_2"]

smoke_stats <- rbind(smoke_stats1, smoke_stats2[arm=="scenario_2"])

#This plot illustrates that Scenario 2 shows bigger effects in IMD quiniles 4 and 5 compared 
#to Scenario 1, the same effect in quintile 3 and smaller effects in quintiles 1 and 2,
#as we would expect from the design of the scenarios.

png("output/intervention_effects_by_imd_and_sex.png", units="in", width=12, height=4, res=300)
ggplot(smoke_stats) +
  geom_line(aes(x = year, y = 100 * smk_prev, linetype = arm, colour = sex), size = .4) +
  scale_colour_manual(name = "Sex", values = c('#6600cc','#00cc99')) +
  facet_wrap(~ imd_quintile, nrow = 1) +
  ylim(0, 30) + ylab("percentage smokers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Intervention effects on smoking prevalence by IMD quintile and sex",
       #subtitle = "", 
       caption = "")
dev.off()

#Look at differences by age and sex
smk_data1[ , ageband := c("11-29", "30-54", "55-74", "75-89")[findInterval(age, c(-10, 30, 55, 75, 1000))]]
smk_data2[ , ageband := c("11-29", "30-54", "55-74", "75-89")[findInterval(age, c(-10, 30, 55, 75, 1000))]]

smoke_stats1a <- SmkEffects(
  data = smk_data1,
  strat_vars = c("year", "ageband", "sex"),
  two_arms = TRUE)

smoke_stats2a <- SmkEffects(
  data = smk_data2,
  strat_vars = c("year", "ageband", "sex"),
  two_arms = TRUE)

#Combine policies into a single data table (control is the same for both arms)
smoke_stats1a[arm=="treatment", arm:="scenario_1"]
smoke_stats2a[arm=="treatment", arm:="scenario_2"]

smoke_stats.a <- rbind(smoke_stats1a, smoke_stats2a[arm=="scenario_2"])

#This plot illustrates that the differences between the two scenarios by age are small,
#as the differences across IMD quintiles largely cancel out. Both scenarios reduce smoking
#prevalence at younger ages significantly though.

png("output/intervention_effects_by_age_and_sex.png", units="in", width=12, height=4, res=300)
ggplot(smoke_stats.a) +
  geom_line(aes(x = year, y = 100 * smk_prev, linetype = arm, colour = sex), size = .4) +
  scale_colour_manual(name = "Sex", values = c('#6600cc','#00cc99')) +
  facet_wrap(~ ageband, nrow = 1) +
  ylim(0, 30) + ylab("percentage smokers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Intervention effects on smoking prevalence by age and sex",
       #subtitle = "", 
       caption = "")
dev.off()