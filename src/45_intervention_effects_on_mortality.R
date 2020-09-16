
# The aim of this code is to make some basic plots to 
# view the effects of the intervention on mortality

library(data.table)
library(stapmr)
library(ggplot2)
library(mort.tools)

# Distribution effects by year and IMD quintile

mort_data1 <- MortCalc(
  path = "output/",
  label = "scenario_1",
  two_arms = TRUE,
  baseline_year = 2002,
  baseline_population_size = 2e5,
  strat_vars = c("year", "age", "imd_quintile"))

mort_data2 <- MortCalc(
  path = "output/",
  label = "scenario_2",
  two_arms = TRUE,
  baseline_year = 2002,
  baseline_population_size = 2e5,
  strat_vars = c("year", "age", "imd_quintile"))

mort_data1[ , `:=`(n_deaths_diff = n_deaths_treatment - n_deaths_control,
                  yll_diff = yll_treatment - yll_control)]

mort_data2[ , `:=`(n_deaths_diff = n_deaths_treatment - n_deaths_control,
                   yll_diff = yll_treatment - yll_control)]

saveRDS(mort_data1, "output/mort_data_year_age_imd_scenario1.rds")
saveRDS(mort_data2, "output/mort_data_year_age_imd_scenario2.rds")

# Years of life lost to smoking related diseases compared to control

yll_temp1 <- mort_data1[ , .(yll_diff = sum(yll_diff, na.rm = T)), by = c("year", "imd_quintile")]
yll_temp2 <- mort_data2[ , .(yll_diff = sum(yll_diff, na.rm = T)), by = c("year", "imd_quintile")]

#Combine policies into a single data table (control is the same for both arms)
yll_temp1[,arm:="Scenario 1"]
yll_temp2[,arm:="Scenario 2"]

yll_temp <- rbind(yll_temp1, yll_temp2)

# Plot intervention effects on the years of life lost compared to control

png("output/yll_year_imd.png", units="in", width=7, height=7, res=300)
ggplot(yll_temp) +
  geom_line(aes(x = year, y = yll_diff, colour = imd_quintile), size = .4) +
  scale_colour_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177")) +
  ylab("Years of life lost") +
  facet_wrap(~arm)+
  theme_minimal() +
  labs(title = "Intervention effect on years of life lost",
       #subtitle = "", 
       caption = "")
dev.off()

