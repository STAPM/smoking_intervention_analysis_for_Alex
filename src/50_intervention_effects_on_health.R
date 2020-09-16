
# The aim of the code is to estimate the effect of the intervention 
# on health costs and QALYs

library(data.table)
library(stapmr)
library(ggplot2)

# Utility data
utility_data <- readRDS("intermediate_data/utility_data.rds")

# Hospital care unit cost data
unit_cost_data <- readRDS("intermediate_data/unit_cost_data.rds")

# Hospital care multiplier data
multiplier_data <- readRDS("intermediate_data/multiplier_data.rds")

# Calculate health outcomes
# Match baseline year and population size to the values in the code that runs the simulation
health_data1 <- HealthCalc(path = "output/",
                          label = "scenario_1",
                          two_arms = TRUE,
                          baseline_year = 2002,
                          baseline_population_size = 2e5,
                          multiplier_data = multiplier_data,
                          unit_cost_data = unit_cost_data,
                          utility_data = utility_data)

saveRDS(health_data1, "output/health_data1.rds")

health_data2 <- HealthCalc(path = "output/",
                           label = "scenario_2",
                           two_arms = TRUE,
                           baseline_year = 2002,
                           baseline_population_size = 2e5,
                           multiplier_data = multiplier_data,
                           unit_cost_data = unit_cost_data,
                           utility_data = utility_data)

saveRDS(health_data2, "output/health_data1.rds")

# QALY calc
qaly_year1 <- health_data1$qaly_data[ , .(
  qaly_total = sum(qaly_total)
), by = c("year", "imd_quintile", "arm")]

qaly_year2 <- health_data2$qaly_data[ , .(
  qaly_total = sum(qaly_total)
), by = c("year", "imd_quintile", "arm")]

# Reshape data
qaly_year1 <- dcast(qaly_year1, year + imd_quintile ~ arm, value.var = "qaly_total")
qaly_year2 <- dcast(qaly_year2, year + imd_quintile ~ arm, value.var = "qaly_total")

# Calculate QALY difference between treatment and control arms
qaly_year1[ , qaly_difference := (treatment - control)]
qaly_year2[ , qaly_difference := (treatment - control)]

# Calculate cumulative discounted effect on QALYs

# Set a years-since-intervention counter
# based on the first year of the intervention being 2016
qaly_year1 <- qaly_year1[year >= 2016, Year_since_int := year - 2016,]
qaly_year2 <- qaly_year2[year >= 2016, Year_since_int := year - 2016,]

# Calculate discounted QALY difference
qaly_year1[year >= 2016 , Dis_qaly_difference := (treatment - control)*(1/((1+0.035)^Year_since_int)), ]
qaly_year2[year >= 2016 , Dis_qaly_difference := (treatment - control)*(1/((1+0.035)^Year_since_int)), ]

qaly_year1[year >= 2016, Dis_Cum_qaly_difference := cumsum(Dis_qaly_difference), by = "imd_quintile"]
qaly_year2[year >= 2016, Dis_Cum_qaly_difference := cumsum(Dis_qaly_difference), by = "imd_quintile"]

write.csv(qaly_year1, "output/discounted_QALY_gain_scenario1.csv", row.names = F)
write.csv(qaly_year2, "output/discounted_QALY_gain_scenario2.csv", row.names = F)

#Combine policies into a single data table (control is the same for both arms)
qaly_year1[,arm:="Scenario 1"]
qaly_year2[,arm:="Scenario 2"]

qaly_year <- rbind(qaly_year1, qaly_year2)

# Plot intervention effects on QALYs
png("output/qaly_year_imd.png", units="in", width=7, height=7, res=300)
ggplot(qaly_year) +
  geom_line(aes(x = year, y = Dis_Cum_qaly_difference, colour = imd_quintile), size = .4) +
  scale_colour_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177")) +
  ylab("QALYs") +
  facet_wrap(~arm)+
  theme_minimal() +
  labs(title = "Intervention effect on Quality-Adjusted Life Years",
       subtitle = "Calculated cumulatively, after discounting", 
       caption = "")
dev.off()


############################################################################
# Effect on the cost of hospital admissions

# Summarise outcomes for cost of hospital admissions
cost_year1 <- health_data1$hosp_data[ , .(
  admission_cost = sum(admission_cost)
), by = c("year", "imd_quintile", "arm")]

cost_year2 <- health_data2$hosp_data[ , .(
  admission_cost = sum(admission_cost)
), by = c("year", "imd_quintile", "arm")]

# Reshape data
cost_year1 <- dcast(cost_year1, year + imd_quintile ~ arm, value.var = "admission_cost")
cost_year2 <- dcast(cost_year2, year + imd_quintile ~ arm, value.var = "admission_cost")

# Calculate intervention effect on costs
cost_year1[ , Cost_difference := (treatment - control)]
cost_year2[ , Cost_difference := (treatment - control)]

# Discounted cost savings
cost_year1 <- cost_year1[year >= 2016, Year_since_int := year - 2016,]
cost_year2 <- cost_year2[year >= 2016, Year_since_int := year - 2016,]

cost_year1[year >= 2016 , Dis_Cost_difference := (treatment - control)*(1/((1+0.035)^Year_since_int)), ]
cost_year2[year >= 2016 , Dis_Cost_difference := (treatment - control)*(1/((1+0.035)^Year_since_int)), ]

cost_year1[year >= 2016 , Dis_Cum_Cost_difference := cumsum(Dis_Cost_difference), by = "imd_quintile"]
cost_year2[year >= 2016 , Dis_Cum_Cost_difference := cumsum(Dis_Cost_difference), by = "imd_quintile"]

write.csv(cost_year1, "output/discounted_cost_difference_scenario1.csv", row.names = F)
write.csv(cost_year2, "output/discounted_cost_difference_scenario2.csv", row.names = F)

cost_year1[,arm:="Scenario 1"]
cost_year2[,arm:="Scenario 2"]

cost_year <- rbind(cost_year1, cost_year2)

# Plot intervention effects on the costs of hospital admissions
png("output/hosp_costs_year_imd.png", units="in", width=7, height=7, res=300)
ggplot(cost_year) +
  geom_line(aes(x = year, y = Dis_Cum_Cost_difference / 1e6, colour = imd_quintile), size = .4) +
  scale_colour_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177")) +
  ylab("Cost /£Million") +
  facet_wrap(~arm)+
  theme_minimal() +
  labs(title = "Intervention effect on cost of hospital admissions",
       subtitle = "Calculated cumulatively, after discounting", 
       caption = "")
dev.off()

#########################################################################
#Bring in intervention costs to calculate net costs of each Scenario

# Hospital care multiplier data
intervention_costs <- readRDS("output/intervention_delivery_costs_by_imd.rds")

#Discount costs
intervention_costs <- intervention_costs[year >= 2016, Year_since_int := year - 2016,]
intervention_costs[year >= 2016 , Dis_int_costs := raw_cost*(1/((1+0.035)^Year_since_int)),]

#Calculate overall net costs
net_costs <- merge(cost_year, intervention_costs, by=c("year", "imd_quintile", "arm"))

net_costs[,net_cost_raw:=Cost_difference+raw_cost]
net_costs[,net_cost_disc:=Dis_Cost_difference+Dis_int_costs]

#Cumulative net costs over time by IMD quintile
net_costs[year >= 2016 , cum_net_cost_raw := cumsum(net_cost_raw), by = c("imd_quintile", "arm")]
net_costs[year >= 2016 , cum_net_cost_disc := cumsum(net_cost_disc), by = c("imd_quintile", "arm")]

#Plot net costs
ggplot(net_costs)+
  geom_line(aes(x=year, y=cum_net_cost_raw/ 1e6, colour=imd_quintile))+
  facet_wrap(~arm)+
  ylab("Net cost vs. control (£Million)") +
  scale_colour_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177")) +
  theme_minimal()

ggplot(net_costs)+
  geom_line(aes(x=year, y=cum_net_cost_disc/ 1e6, colour=imd_quintile))+
  facet_wrap(~arm)+
  ylab("Discounted net cost vs. control (£Million)") +
  scale_colour_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177")) +
  theme_minimal()

#Conventional cost-effectiveness calculations assuming time horizon ends at 2050
CE_calc <- net_costs[year>=2016, .(cum_net_cost_disc=sum(net_cost_disc)), by="arm"]
temp <- qaly_year[year>=2016, .(cum_net_QALY_disc=sum(Dis_qaly_difference)), by="arm"]
CE_calc <- merge(CE_calc, temp)

CE_calc[,ICER_vs_control:=cum_net_cost_disc/cum_net_QALY_disc]

#Plot Cost-Effectiveness plane
ggplot(CE_calc)+
  geom_point(aes(x=cum_net_QALY_disc, y=cum_net_cost_disc/1000000, colour=arm))+
  xlab("Discounted QALYs vs. control")+
  ylab("Discounted costs (£m) vs. control")+
  expand_limits(x = 0, y = 0)+
  theme_minimal()


