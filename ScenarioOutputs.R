# Scenario Outputs
# Script 10/10
# Run this script to create the following figures:
# 1. plots of the EUI for a representative building to illustrate the three scenarios
# 2. one figure for each scenario that combines energy cost savings, avoided penalties, and total cost savings in one figure

# To see the 4 figures, uncomment the lines below and run (after running the entire script)
# scenarioexample_plot
# Scenario1_combinedplot
# Scenario2_combinedplot
# Scenario3_combinedplot


# Scenario Plots for Representative Building ------------------------------

# Identify Representative building
# largest cateogry is Pre-War, Low-Rise -> filter for this category
# Mean EUI is 85.1 -> filter for EUI close to mean
RepBldg <- Baseline_Housing %>% 
  filter(AgeandSizeCategory == "Pre-War, Low-Rise") %>% 
  filter(BaselineEUI < 85.2 & BaselineEUI >85.0)

# For illustrative purposes, choose building with as many fuel types
#  This building uses fuel oil 2, natural gas, and electricity
RepBldg <- RepBldg[2,]

# Identify BBL of representative building
RepBldg_BBL <- RepBldg$BBL

# Identify row # of this BBL
RedBldg_loc <- which(Baseline_Housing$BBL == RepBldg_BBL)

# Scenario 1

scenario1 <- rbind(columns,EUIFuelOil2_Post1[RedBldg_loc,3:length(EUIFuelOil2_Post1)],EUIFuelOil4_Post1[RedBldg_loc,3:length(EUIFuelOil4_Post1)],
                   EUIFuelOil56_Post1[RedBldg_loc,3:length(EUIFuelOil56_Post1)],EUIDistrictSteam_Post1[RedBldg_loc,3:length(EUIDistrictSteam_Post1)],
                   EUINaturalGas_Post1_kBtu[RedBldg_loc,3:length(EUINaturalGas_Post1_kBtu)],EUIElectricity_Post1_kBtu[RedBldg_loc,3:length(EUIElectricity_Post1_kBtu)],
                   EUIFuelOil2_Post1[RedBldg_loc,3:length(EUIFuelOil2_Post1)]+EUIFuelOil4_Post1[RedBldg_loc,3:length(EUIFuelOil4_Post1)]+
                     EUIFuelOil56_Post1[RedBldg_loc,3:length(EUIFuelOil56_Post1)]+EUIDistrictSteam_Post1[RedBldg_loc,3:length(EUIDistrictSteam_Post1)]+
                     EUINaturalGas_Post1_kBtu[RedBldg_loc,3:length(EUINaturalGas_Post1_kBtu)]+EUIElectricity_Post1_kBtu[RedBldg_loc,3:length(EUIElectricity_Post1_kBtu)] )
scenario1 <- as.data.frame(t(scenario1))
scenario1 <- scenario1 %>% 
  rename("Year" = "1") %>% 
  rename("FuelOil2" = "5331") %>% 
  rename("FuelOil4" = "53311") %>% 
  rename("FuelOil56" = "53312") %>% 
  rename("DistrictSteam" = "53313") %>% 
  rename("NaturalGas" = "53314") %>% 
  rename("Electricity" = "53315") %>% 
  rename("Total" = "53316")

scenario1$FuelOil2 <- as.numeric(scenario1$FuelOil2)
scenario1$FuelOil4 <- as.numeric(scenario1$FuelOil4)
scenario1$FuelOil56 <- as.numeric(scenario1$FuelOil56)
scenario1$DistrictSteam <- as.numeric(scenario1$DistrictSteam)
scenario1$NaturalGas <- as.numeric(scenario1$NaturalGas)
scenario1$Electricity <- as.numeric(scenario1$Electricity)
scenario1$Total <- as.numeric(scenario1$Total)


pscenario1 <- ggplot(scenario1) +
  geom_line(aes(x = Year, y = FuelOil2, group = 1, color = "Fuel Oil 2")) +
  geom_line(aes(x = Year, y = FuelOil4, group = 1, color = "Fuel Oil 4")) +
  geom_line(aes(x = Year, y = FuelOil56, group = 1, color = "Fuel Oil 5/6")) +
  geom_line(aes(x = Year, y = DistrictSteam, group = 1, color = "District Steam")) +
  geom_line(aes(x = Year, y = NaturalGas, group = 1, color = "Natural Gas")) + 
  geom_line(aes(x = Year, y = Electricity, group = 1, color = "Electricity")) + 
  geom_line(aes(x = Year, y = Total, group =1, color = "Total")) +
  scale_color_manual(name = "Fuel Type",
                     values = c("Fuel Oil 2" = "#fee5d9", "Fuel Oil 4" = "#fcae91",
                                "Fuel Oil 5/6" = "#fb9a99", "District Steam" = "#ff7f00",
                                "Natural Gas" = "#a6cee3", "Electricity" =  "#33a02c", "Total" = "black")) +
  theme_classic() +
  ggtitle("Scenario 1: Gradual EUI Reductions") +
  scale_y_continuous(name = "EUI (kBtu/sqft/yr)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1))   

# Scenario 2

scenario2 <- rbind(columns,EUIFuelOil2_Post2[RedBldg_loc,3:length(EUIFuelOil2_Post2)],EUIFuelOil4_Post2[RedBldg_loc,3:length(EUIFuelOil4_Post2)],
                   EUIFuelOil56_Post2[RedBldg_loc,3:length(EUIFuelOil56_Post2)],EUIDistrictSteam_Post2[RedBldg_loc,3:length(EUIDistrictSteam_Post2)],
                   EUINaturalGas_Post2_kBtu[RedBldg_loc,3:length(EUINaturalGas_Post2_kBtu)],EUIElectricity_Post2_kBtu[RedBldg_loc,3:length(EUIElectricity_Post2_kBtu)],
                   EUIFuelOil2_Post2[RedBldg_loc,3:length(EUIFuelOil2_Post2)]+EUIFuelOil4_Post2[RedBldg_loc,3:length(EUIFuelOil4_Post2)]+
                     EUIFuelOil56_Post2[RedBldg_loc,3:length(EUIFuelOil56_Post2)]+EUIDistrictSteam_Post2[RedBldg_loc,3:length(EUIDistrictSteam_Post2)]+
                     EUINaturalGas_Post2_kBtu[RedBldg_loc,3:length(EUINaturalGas_Post2_kBtu)]+EUIElectricity_Post2_kBtu[RedBldg_loc,3:length(EUIElectricity_Post2_kBtu)] )
scenario2 <- as.data.frame(t(scenario2))
scenario2 <- scenario2 %>% 
  rename("Year" = "1") %>% 
  rename("FuelOil2" = "2") %>% 
  rename("FuelOil4" = "3") %>% 
  rename("FuelOil56" = "4") %>% 
  rename("DistrictSteam" = "5") %>% 
  rename("NaturalGas" = "5331") %>% 
  rename("Electricity" = "53311") %>% 
  rename("Total" = "11")

scenario2$FuelOil2 <- as.numeric(scenario2$FuelOil2)
scenario2$FuelOil4 <- as.numeric(scenario2$FuelOil4)
scenario2$FuelOil56 <- as.numeric(scenario2$FuelOil56)
scenario2$DistrictSteam <- as.numeric(scenario2$DistrictSteam)
scenario2$NaturalGas <- as.numeric(scenario2$NaturalGas)
scenario2$Electricity <- as.numeric(scenario2$Electricity)
scenario2$Total <- as.numeric(scenario2$Total)


pscenario2 <-ggplot(scenario2) +
  geom_line(aes(x = Year, y = FuelOil2, group = 1, color = "Fuel Oil 2")) +
  geom_line(aes(x = Year, y = FuelOil4, group = 1, color = "Fuel Oil 4")) +
  geom_line(aes(x = Year, y = FuelOil56, group = 1, color = "Fuel Oil 5/6")) +
  geom_line(aes(x = Year, y = DistrictSteam, group = 1, color = "District Steam")) +
  geom_line(aes(x = Year, y = NaturalGas, group = 1, color = "Natural Gas")) + 
  geom_line(aes(x = Year, y = Electricity, group = 1, color = "Electricity")) + 
  geom_line(aes(x = Year, y = Total, group =1, color = "Total")) +
  scale_color_manual(name = "Fuel Type",
                     values = c("Fuel Oil 2" = "#fee5d9", "Fuel Oil 4" = "#fcae91",
                                "Fuel Oil 5/6" = "#fb9a99", "District Steam" = "#ff7f00",
                                "Natural Gas" = "#a6cee3", "Electricity" =  "#33a02c", "Total" = "black")) +
  theme_classic() +
  ggtitle("Scenario 2: Fully Electric in 2024") +
  scale_y_continuous(name = "EUI (kBtu/sqft/yr)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1))

# Scenario 3 EUI example

scenario3 <- rbind(columns,EUIFuelOil2_Post3[RedBldg_loc,3:length(EUIFuelOil2_Post3)],EUIFuelOil4_Post3[RedBldg_loc,3:length(EUIFuelOil4_Post3)],
                   EUIFuelOil56_Post3[RedBldg_loc,3:length(EUIFuelOil56_Post3)],EUIDistrictSteam_Post3[RedBldg_loc,3:length(EUIDistrictSteam_Post3)],
                   EUINaturalGas_Post3_kBtu[RedBldg_loc,3:length(EUINaturalGas_Post3_kBtu)],EUIElectricity_Post3_kBtu[RedBldg_loc,3:length(EUIElectricity_Post3_kBtu)],
                   EUIFuelOil2_Post3[RedBldg_loc,3:length(EUIFuelOil2_Post3)]+EUIFuelOil4_Post3[RedBldg_loc,3:length(EUIFuelOil4_Post3)]+
                     EUIFuelOil56_Post3[RedBldg_loc,3:length(EUIFuelOil56_Post3)]+EUIDistrictSteam_Post3[RedBldg_loc,3:length(EUIDistrictSteam_Post3)]+
                     EUINaturalGas_Post3_kBtu[RedBldg_loc,3:length(EUINaturalGas_Post3_kBtu)]+EUIElectricity_Post3_kBtu[RedBldg_loc,3:length(EUIElectricity_Post3_kBtu)] )
scenario3 <- as.data.frame(t(scenario3))
scenario3 <- scenario3 %>% 
  rename("Year" = "1") %>% 
  rename("FuelOil2" = "2") %>% 
  rename("FuelOil4" = "3") %>% 
  rename("FuelOil56" = "4") %>% 
  rename("DistrictSteam" = "5") %>% 
  rename("NaturalGas" = "5331") %>% 
  rename("Electricity" = "53311") %>% 
  rename("Total" = "11")

scenario3$FuelOil2 <- as.numeric(scenario3$FuelOil2)
scenario3$FuelOil4 <- as.numeric(scenario3$FuelOil4)
scenario3$FuelOil56 <- as.numeric(scenario3$FuelOil56)
scenario3$DistrictSteam <- as.numeric(scenario3$DistrictSteam)
scenario3$NaturalGas <- as.numeric(scenario3$NaturalGas)
scenario3$Electricity <- as.numeric(scenario3$Electricity)
scenario3$Total <- as.numeric(scenario3$Total)


pscenario3 <- ggplot(scenario3) +
  geom_line(aes(x = Year, y = FuelOil2, group = 1, color = "Fuel Oil 2")) +
  geom_line(aes(x = Year, y = FuelOil4, group = 1, color = "Fuel Oil 4")) +
  geom_line(aes(x = Year, y = FuelOil56, group = 1, color = "Fuel Oil 5/6")) +
  geom_line(aes(x = Year, y = DistrictSteam, group = 1, color = "District Steam")) +
  geom_line(aes(x = Year, y = NaturalGas, group = 1, color = "Natural Gas")) + 
  geom_line(aes(x = Year, y = Electricity, group = 1, color = "Electricity")) + 
  geom_line(aes(x = Year, y = Total, group =1, color = "Total")) +
  scale_color_manual(name = "Fuel Type",
                     values = c("Fuel Oil 2" = "#fee5d9", "Fuel Oil 4" = "#fcae91",
                                "Fuel Oil 5/6" = "#fb9a99", "District Steam" = "#ff7f00",
                                "Natural Gas" = "#a6cee3", "Electricity" =  "#33a02c", "Total" = "black")) +
  theme_classic() +
  ggtitle("Scenario 3: Fully Electric Passive House in 2024") +
  scale_y_continuous(name = "EUI (kBtu/sqft/yr)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1))

# Plot scenarios together
scenarioexample_plot <- pscenario1 / pscenario2 / pscenario3 + plot_layout(guides = "collect") #& theme(legend.position = "bottom")


# Scenario 1 --------------------------------------------------------------

# Energy cost savings figures - per apartment

# Create table of Mean NPV, Standard Deviation NPV, and % of NPV < 0 for each energy price scenario and discount rate
# Plot histogram of undiscounted energy cost savings with table with NPVs for different discount rate scenarios and price scenarios


# Create table with Mean, Std Dev, and % <0 NPVs for discount rates and energy prices scenarios

Scenario1_Apt_NPVtable <- cbind(" " = c("0% DR","Mean", "Std Dev","% <0",
                                        "2% DR","Mean", "Std Dev","% <0",
                                        "5% DR","Mean", "Std Dev","% <0",
                                        "8% DR","Mean", "Std Dev","% <0"), 
                                Low = c("",
                                        format(round(Delta1EnergyCostsApt_Low_NPV_DRzero_Mean,0),big.mark = ","),
                                        format(round(Delta1EnergyCostsApt_Low_NPV_DRzero_StDev,0), big.mark = ","),
                                        paste0(format(round(Delta1EnergyCostsApt_Low_NPV_DRzero_PctNeg,1),nsmall = 1),"%"),
                                        "",
                                        format(round(Delta1EnergyCostsApt_Low_NPV_DRlow_Mean,0),big.mark = ","),
                                        format(round(Delta1EnergyCostsApt_Low_NPV_DRlow_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta1EnergyCostsApt_Low_NPV_DRlow_PctNeg,1),nsmall = 1),"%"),
                                        "",
                                        format(round(Delta1EnergyCostsApt_Low_NPV_DRmid_Mean,0),big.mark = ","),
                                        format(round(Delta1EnergyCostsApt_Low_NPV_DRmid_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta1EnergyCostsApt_Low_NPV_DRmid_PctNeg,1),nsmall = 1),"%"),
                                        "",
                                        format(round(Delta1EnergyCostsApt_Low_NPV_DRhigh_Mean,0),big.mark = ","),
                                        format(round(Delta1EnergyCostsApt_Low_NPV_DRhigh_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta1EnergyCostsApt_Low_NPV_DRhigh_PctNeg,1), nsmall = 1),"%")),
                                Mid = c("",
                                        format(round(Delta1EnergyCostsApt_Mid_NPV_DRzero_Mean,0),big.mark = ","),
                                        format(round(Delta1EnergyCostsApt_Mid_NPV_DRzero_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta1EnergyCostsApt_Mid_NPV_DRzero_PctNeg,1), nsmall = 1),"%"),
                                        "",
                                        format(round(Delta1EnergyCostsApt_Mid_NPV_DRlow_Mean,0),big.mark = ","),
                                        format(round(Delta1EnergyCostsApt_Mid_NPV_DRlow_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta1EnergyCostsApt_Mid_NPV_DRlow_PctNeg,1), nsmall = 1), "%"),
                                        "",
                                        format(round(Delta1EnergyCostsApt_Mid_NPV_DRmid_Mean,0),big.mark = ","),
                                        format(round(Delta1EnergyCostsApt_Mid_NPV_DRmid_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta1EnergyCostsApt_Mid_NPV_DRmid_PctNeg,1), nsmall = 1), "%"),
                                        "",
                                        format(round(Delta1EnergyCostsApt_Mid_NPV_DRhigh_Mean,0),big.mark = ","),
                                        format(round(Delta1EnergyCostsApt_Mid_NPV_DRhigh_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta1EnergyCostsApt_Mid_NPV_DRhigh_PctNeg,1), nsmall = 1), "%")),
                                High = c("",
                                         format(round(Delta1EnergyCostsApt_High_NPV_DRzero_Mean,0),big.mark = ","),
                                         format(round(Delta1EnergyCostsApt_High_NPV_DRzero_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta1EnergyCostsApt_High_NPV_DRzero_PctNeg,1), nsmall = 1), "%"),
                                         "",
                                         format(round(Delta1EnergyCostsApt_High_NPV_DRlow_Mean,0),big.mark = ","),
                                         format(round(Delta1EnergyCostsApt_High_NPV_DRlow_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta1EnergyCostsApt_High_NPV_DRlow_PctNeg,1), nsmall = 1), "%"),
                                         "",
                                         format(round(Delta1EnergyCostsApt_High_NPV_DRmid_Mean,0),big.mark = ","),
                                         format(round(Delta1EnergyCostsApt_High_NPV_DRmid_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta1EnergyCostsApt_High_NPV_DRmid_PctNeg,1), nsmall = 1), "%"),
                                         "",
                                         format(round(Delta1EnergyCostsApt_High_NPV_DRhigh_Mean,0),big.mark = ","),
                                         format(round(Delta1EnergyCostsApt_High_NPV_DRhigh_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta1EnergyCostsApt_High_NPV_DRhigh_PctNeg,1), nsmall = 1), "%")))

# Convert to data frame
Scenario1_Apt_NPVtable <- as.data.frame(Scenario1_Apt_NPVtable)

# Use ggtexttable to format table
Scenario1_Apt_NPVtable <- ggtexttable(Scenario1_Apt_NPVtable, rows = NULL,
                                      theme = ttheme("blank"))  %>%
  tab_add_hline(at.row = c(1:2,2:3,6:7,10:11,14:15), row.side = "top", linewidth = 2)


# graph undiscounted energy cost savings with NPV table in top right corner

Scenario1_Apt_plot <- ggplot() +
  geom_histogram(aes(x = Delta1EnergyCostsApt_Low$TotalCosts_2024to2050,
                     y = (..count..)/sum(..count..), fill = "Low"), 
                 binwidth = 500, alpha = 1) +
  geom_histogram(aes(x = Delta1EnergyCostsApt_Mid$TotalCosts_2024to2050, 
                     y = (..count..)/sum(..count..), fill = "Mid"),
                 binwidth = 500, alpha = 0.8) +
  geom_histogram(aes(x = Delta1EnergyCostsApt_High$TotalCosts_2024to2050, 
                     y = (..count..)/sum(..count..), fill = "High"),
                 binwidth = 500, alpha = 0.6) +
  scale_x_continuous(labels = scales::dollar_format(), limits = c(0,150000),
                     name = "Undiscounted Cumulative Energy Cost Savings, 2024-2050 ($/Apt)") +
  scale_y_continuous(labels = percent, name = "% of Apartments") + 
  ggtitle("Scenario 1: Undiscounted Cumulative\nEnergy Cost Savings, 2024-2050") +
  theme_classic() +
  scale_fill_manual(values = c("Low" = "#bdd7e7","Mid" = "#6baed6", "High" = "#08519c"), 
                    breaks = c("Low", "Mid", "High"),
                    name = "Price Scenario") +
  theme(legend.position = "bottom") +
  annotation_custom(ggplotGrob(Scenario1_Apt_NPVtable),
                    xmin = 100000, xmax = 150000, ymin = 0.005, ymax = 0.03)


# Avoided Penalties

# Create table with NPVs
Scenario1_Penalty_Apt_NPVtable <- cbind("Discount Rate" = c("0%", "2%", "5%", "8%"),
                                        "Mean" = c(format(round(Post1AvoidedPenaltiesApt_NPV_DRzero_Mean,0),big.mark = ","),
                                                   format(round(Post1AvoidedPenaltiesApt_NPV_DRlow_Mean,0),big.mark = ","),
                                                   format(round(Post1AvoidedPenaltiesApt_NPV_DRmid_Mean,0),big.mark = ","),
                                                   format(round(Post1AvoidedPenaltiesApt_NPV_DRhigh_Mean,0),big.mark = ",")),
                                        "St Dev" = c(format(round(Post1AvoidedPenaltiesApt_NPV_DRzero_StDev,0),big.mark = ","),
                                                     format(round(Post1AvoidedPenaltiesApt_NPV_DRlow_StDev,0),big.mark = ","),
                                                     format(round(Post1AvoidedPenaltiesApt_NPV_DRmid_StDev,0),big.mark = ","),
                                                     format(round(Post1AvoidedPenaltiesApt_NPV_DRhigh_StDev,0),big.mark = ",")))

# Convert to data frame
Scenario1_Penalty_Apt_NPVtable <- as.data.frame(Scenario1_Penalty_Apt_NPVtable)
# Use ggtexttable to format table
Scenario1_Penalty_Apt_NPVtable <- ggtexttable(Scenario1_Penalty_Apt_NPVtable, rows = NULL,
                                              theme = ttheme("blank"))  %>%
  tab_add_hline(at.row = c(1:2), row.side = "top", linewidth = 2)


# Plot
Scenario1_Apt_penalty_plot <- ggplot(Post1AvoidedPenaltiesApt, aes(x = UndiscountedTotal, y = ..count../sum(..count..))) + 
  geom_histogram(fill = "#74c476", binwidth = 100) +
  scale_x_continuous(name = "Total Avoided Penalties, 2024-2050 ($/Apt)",
                     labels = scales::dollar_format(),
                     limits = c(NA,70000)) +
  scale_y_continuous(name = "% of Apartments", label = percent) +
  ggtitle("Scenario 1: Undiscounted Cumulative\nAvoided Penalties, 2024-2050") +
  theme_classic() +
  annotation_custom(ggplotGrob(Scenario1_Penalty_Apt_NPVtable),
                    xmin = 40000, xmax = 60000, ymin = 0.007, ymax = 0.017)


# Total Cost Savings

# Create table with NPVs
Scenario1_Total_Apt_NPVtable <- cbind("Discount Rate" = c("0%","2%", "5%", "8%"),
                                      "Mean" = c(format(round(Post1CostSavingsApt_NPV_DRzero_Mean,0), big.mark = ","),
                                                 format(round(Post1CostSavingsApt_NPV_DRlow_Mean,0), big.mark = ","),
                                                 format(round(Post1CostSavingsApt_NPV_DRmid_Mean,0), big.mark = ","),
                                                 format(round(Post1CostSavingsApt_NPV_DRhigh_Mean,0), big.mark = ",")),
                                      "St Dev" = c(format(round(Post1CostSavingsApt_NPV_DRzero_StDev,0), big.mark = ","),
                                                   format(round(Post1CostSavingsApt_NPV_DRlow_StDev,0), big.mark = ","),
                                                   format(round(Post1CostSavingsApt_NPV_DRmid_StDev,0), big.mark = ","),
                                                   format(round(Post1CostSavingsApt_NPV_DRhigh_StDev,0), big.mark = ",")),
                                      "% <0" = c(paste0(format(round(Post1CostSavingsApt_NPV_DRzero_PctNeg,1),nsmall = 1),"%"),
                                                 paste0(format(round(Post1CostSavingsApt_NPV_DRlow_PctNeg,1),nsmall = 1),"%"),
                                                 paste0(format(round(Post1CostSavingsApt_NPV_DRmid_PctNeg,1),nsmall = 1),"%"),
                                                 paste0(format(round(Post1CostSavingsApt_NPV_DRhigh_PctNeg,1),nsmall = 1),"%")))

# Convert to data frame
Scenario1_Total_Apt_NPVtable <- as.data.frame(Scenario1_Total_Apt_NPVtable)
# Use ggtexttable to format table
Scenario1_Total_Apt_NPVtable <- ggtexttable(Scenario1_Total_Apt_NPVtable, rows = NULL,
                                            theme = ttheme("blank"))  %>%
  tab_add_hline(at.row = c(1:2), row.side = "top", linewidth = 2)


# Plot
Scenario1_total_Apt_plot <- ggplot(Post1CostSavingsApt, aes(x = UndiscountedTotal, y = ..count../sum(..count..))) + 
  geom_histogram(fill = "#7fcdbb", binwidth = 400) +
  scale_x_continuous(name = "Total Cost Savings, 2024-2050 ($/Apt)",
                     labels = scales::dollar_format(),
                     limits = c(NA,250000)) +
  scale_y_continuous(name = "% of Apartments", label = percent) +
  ggtitle("Scenario 1: Undiscounted Cumulative\nCost Savings, 2024-2050") +
  theme_classic() +
  annotation_custom(ggplotGrob(Scenario1_Total_Apt_NPVtable),
                    xmin = 130000, xmax = 220000, ymin = 0.007, ymax = 0.012)


# Plot energy cost savings, avoided penalties, and total cost savings together
Scenario1_combinedplot <- Scenario1_Apt_plot + (Scenario1_Apt_penalty_plot / Scenario1_total_Apt_plot)



# Scenario 2 --------------------------------------------------------------

# Energy cost savings figures - per apartment

# Create table of Mean NPV, Standard Deviation NPV, and % of NPV < 0 for each energy price scenario and discount rate
# Plot histogram of undiscounted energy cost savings with table with NPVs for different discount rate scenarios and price scenarios

# Create table with Mean, Std Dev, and % <0 NPVs for discount rates and energy prices scenarios

Scenario2_Apt_NPVtable <- cbind(" " = c("0% DR","Mean", "Std Dev","% <0",
                                        "2% DR","Mean", "Std Dev","% <0",
                                        "5% DR","Mean", "Std Dev","% <0",
                                        "8% DR","Mean", "Std Dev","% <0"), 
                                Low = c("",
                                        format(round(Delta2EnergyCostsApt_Low_NPV_DRzero_Mean,0),big.mark = ","),
                                        format(round(Delta2EnergyCostsApt_Low_NPV_DRzero_StDev,0), big.mark = ","),
                                        paste0(format(round(Delta2EnergyCostsApt_Low_NPV_DRzero_PctNeg,1),nsmall = 1),"%"),
                                        "",
                                        format(round(Delta2EnergyCostsApt_Low_NPV_DRlow_Mean,0),big.mark = ","),
                                        format(round(Delta2EnergyCostsApt_Low_NPV_DRlow_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta2EnergyCostsApt_Low_NPV_DRlow_PctNeg,1),nsmall = 1),"%"),
                                        "",
                                        format(round(Delta2EnergyCostsApt_Low_NPV_DRmid_Mean,0),big.mark = ","),
                                        format(round(Delta2EnergyCostsApt_Low_NPV_DRmid_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta2EnergyCostsApt_Low_NPV_DRmid_PctNeg,1),nsmall = 1),"%"),
                                        "",
                                        format(round(Delta2EnergyCostsApt_Low_NPV_DRhigh_Mean,0),big.mark = ","),
                                        format(round(Delta2EnergyCostsApt_Low_NPV_DRhigh_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta2EnergyCostsApt_Low_NPV_DRhigh_PctNeg,1), nsmall = 1),"%")),
                                Mid = c("",
                                        format(round(Delta2EnergyCostsApt_Mid_NPV_DRzero_Mean,0),big.mark = ","),
                                        format(round(Delta2EnergyCostsApt_Mid_NPV_DRzero_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta2EnergyCostsApt_Mid_NPV_DRzero_PctNeg,1), nsmall = 1),"%"),
                                        "",
                                        format(round(Delta2EnergyCostsApt_Mid_NPV_DRlow_Mean,0),big.mark = ","),
                                        format(round(Delta2EnergyCostsApt_Mid_NPV_DRlow_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta2EnergyCostsApt_Mid_NPV_DRlow_PctNeg,1), nsmall = 1), "%"),
                                        "",
                                        format(round(Delta2EnergyCostsApt_Mid_NPV_DRmid_Mean,0),big.mark = ","),
                                        format(round(Delta2EnergyCostsApt_Mid_NPV_DRmid_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta2EnergyCostsApt_Mid_NPV_DRmid_PctNeg,1), nsmall = 1), "%"),
                                        "",
                                        format(round(Delta2EnergyCostsApt_Mid_NPV_DRhigh_Mean,0),big.mark = ","),
                                        format(round(Delta2EnergyCostsApt_Mid_NPV_DRhigh_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta2EnergyCostsApt_Mid_NPV_DRhigh_PctNeg,1), nsmall = 1), "%")),
                                High = c("",
                                         format(round(Delta2EnergyCostsApt_High_NPV_DRzero_Mean,0),big.mark = ","),
                                         format(round(Delta2EnergyCostsApt_High_NPV_DRzero_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta2EnergyCostsApt_High_NPV_DRzero_PctNeg,1), nsmall = 1), "%"),
                                         "",
                                         format(round(Delta2EnergyCostsApt_High_NPV_DRlow_Mean,0),big.mark = ","),
                                         format(round(Delta2EnergyCostsApt_High_NPV_DRlow_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta2EnergyCostsApt_High_NPV_DRlow_PctNeg,1), nsmall = 1), "%"),
                                         "",
                                         format(round(Delta2EnergyCostsApt_High_NPV_DRmid_Mean,0),big.mark = ","),
                                         format(round(Delta2EnergyCostsApt_High_NPV_DRmid_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta2EnergyCostsApt_High_NPV_DRmid_PctNeg,1), nsmall = 1), "%"),
                                         "",
                                         format(round(Delta2EnergyCostsApt_High_NPV_DRhigh_Mean,0),big.mark = ","),
                                         format(round(Delta2EnergyCostsApt_High_NPV_DRhigh_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta2EnergyCostsApt_High_NPV_DRhigh_PctNeg,1), nsmall = 1), "%")))

# Convert to data frame
Scenario2_Apt_NPVtable <- as.data.frame(Scenario2_Apt_NPVtable)

# Use ggtexttable to format table
Scenario2_Apt_NPVtable <- ggtexttable(Scenario2_Apt_NPVtable, rows = NULL,
                                      theme = ttheme("blank"))  %>%
  tab_add_hline(at.row = c(1:2,2:3,6:7,10:11,14:15), row.side = "top", linewidth = 2)


# graph undiscounted energy cost savings with NPV table in top right corner

Scenario2_Apt_plot <- ggplot() +
  geom_histogram(aes(x = Delta2EnergyCostsApt_Low$TotalCosts_2024to2050,
                     y = (..count..)/sum(..count..), fill = "Low"), 
                 binwidth = 700, alpha = 1) +
  geom_histogram(aes(x = Delta2EnergyCostsApt_Mid$TotalCosts_2024to2050, 
                     y = (..count..)/sum(..count..), fill = "Mid"),
                 binwidth = 700, alpha = 0.8) +
  geom_histogram(aes(x = Delta2EnergyCostsApt_High$TotalCosts_2024to2050, 
                     y = (..count..)/sum(..count..), fill = "High"),
                 binwidth = 700, alpha = 0.6) +
  scale_x_continuous(labels = scales::dollar_format(), limits = c(-500000,100),
                     name = "Undiscounted Cumulative Energy Cost Savings, 2024-2050 ($/Apt)") +
  scale_y_continuous(labels = percent, name = "% of Apartments", position = "right") + 
  ggtitle("Scenario 2: Undiscounted Cumulative\nEnergy Cost Savings, 2024-2050") +
  theme_classic() +
  scale_fill_manual(values = c("Low" = "#bdd7e7","Mid" = "#6baed6", "High" = "#08519c"), 
                    breaks = c("Low", "Mid", "High"),
                    name = "Price Scenario") +
  theme(legend.position = "bottom") +
  annotation_custom(ggplotGrob(Scenario2_Apt_NPVtable),
                    xmin = -450000, xmax = -250000, ymin = 0.004, ymax = 0.011)

# Avoided Penalties

# Create table with NPVs
Scenario2_Penalty_Apt_NPVtable <- cbind("Discount Rate" = c("0%", "2%", "5%", "8%"),
                                        "Mean" = c(format(round(Post2AvoidedPenaltiesApt_NPV_DRzero_Mean,0),big.mark = ","),
                                                   format(round(Post2AvoidedPenaltiesApt_NPV_DRlow_Mean,0),big.mark = ","),
                                                   format(round(Post2AvoidedPenaltiesApt_NPV_DRmid_Mean,0),big.mark = ","),
                                                   format(round(Post2AvoidedPenaltiesApt_NPV_DRhigh_Mean,0),big.mark = ",")),
                                        "St Dev" = c(format(round(Post2AvoidedPenaltiesApt_NPV_DRzero_StDev,0),big.mark = ","),
                                                     format(round(Post2AvoidedPenaltiesApt_NPV_DRlow_StDev,0),big.mark = ","),
                                                     format(round(Post2AvoidedPenaltiesApt_NPV_DRmid_StDev,0),big.mark = ","),
                                                     format(round(Post2AvoidedPenaltiesApt_NPV_DRhigh_StDev,0),big.mark = ",")))

# Convert to data frame
Scenario2_Penalty_Apt_NPVtable <- as.data.frame(Scenario2_Penalty_Apt_NPVtable)
# Use ggtexttable to format table
Scenario2_Penalty_Apt_NPVtable <- ggtexttable(Scenario2_Penalty_Apt_NPVtable, rows = NULL,
                                              theme = ttheme("blank"))  %>%
  tab_add_hline(at.row = c(1:2), row.side = "top", linewidth = 2)


# Plot
Scenario2_Apt_penalty_plot <- ggplot(Post2AvoidedPenaltiesApt, aes(x = UndiscountedTotal, y = ..count../sum(..count..))) + 
  geom_histogram(fill = "#74c476", binwidth = 100) +
  scale_x_continuous(name = "Total Avoided Penalties, 2024-2050 ($/Apt)",
                     labels = scales::dollar_format(),
                     limits = c(NA,70000)) +
  scale_y_continuous(name = "% of Apartments", label = percent) +
  ggtitle("Scenario 2: Undiscounted Cumulative\nAvoided Penalties, 2024-2050") +
  theme_classic() +
  annotation_custom(ggplotGrob(Scenario2_Penalty_Apt_NPVtable),
                    xmin = 40000, xmax = 60000, ymin = 0.01, ymax = 0.017)

# Total Cost Savings

# Create table with NPVs
Scenario2_Total_Apt_NPVtable <- cbind("Discount Rate" = c("0%","2%", "5%", "8%"),
                                      "Mean" = c(format(round(Post2CostSavingsApt_NPV_DRzero_Mean,0), big.mark = ","),
                                                 format(round(Post2CostSavingsApt_NPV_DRlow_Mean,0), big.mark = ","),
                                                 format(round(Post2CostSavingsApt_NPV_DRmid_Mean,0), big.mark = ","),
                                                 format(round(Post2CostSavingsApt_NPV_DRhigh_Mean,0), big.mark = ",")),
                                      "St Dev" = c(format(round(Post2CostSavingsApt_NPV_DRzero_StDev,0), big.mark = ","),
                                                   format(round(Post2CostSavingsApt_NPV_DRlow_StDev,0), big.mark = ","),
                                                   format(round(Post2CostSavingsApt_NPV_DRmid_StDev,0), big.mark = ","),
                                                   format(round(Post2CostSavingsApt_NPV_DRhigh_StDev,0), big.mark = ",")),
                                      "% <0" = c(paste0(format(round(Post2CostSavingsApt_NPV_DRzero_PctNeg,1),nsmall = 1),"%"),
                                                 paste0(format(round(Post2CostSavingsApt_NPV_DRlow_PctNeg,1),nsmall = 1),"%"),
                                                 paste0(format(round(Post2CostSavingsApt_NPV_DRmid_PctNeg,1),nsmall = 1),"%"),
                                                 paste0(format(round(Post2CostSavingsApt_NPV_DRhigh_PctNeg,1),nsmall = 1),"%")))

# Convert to data frame
Scenario2_Total_Apt_NPVtable <- as.data.frame(Scenario2_Total_Apt_NPVtable)
# Use ggtexttable to format table
Scenario2_Total_Apt_NPVtable <- ggtexttable(Scenario2_Total_Apt_NPVtable, rows = NULL,
                                            theme = ttheme("blank"))  %>%
  tab_add_hline(at.row = c(1:2), row.side = "top", linewidth = 2)


# Plot
Scenario2_total_Apt_plot <- ggplot(Post2CostSavingsApt, aes(x = UndiscountedTotal, y = ..count../sum(..count..))) + 
  geom_histogram(fill = "#7fcdbb", binwidth = 400) +
  scale_x_continuous(name = "Total Cost Savings, 2024-2050 ($/Apt)",
                     labels = scales::dollar_format(),
                     limits = c(-500000,1000)) +
  scale_y_continuous(name = "% of Apartments", label = percent, position = "right") +
  ggtitle("Scenario 2: Undiscounted Cumulative\nCost Savings, 2024-2050") +
  theme_classic() +
  annotation_custom(ggplotGrob(Scenario2_Total_Apt_NPVtable),
                    xmin = -450000, xmax = -300000, ymin = 0.003, ymax = 0.005)

# Plot energy cost savings, avoided penalties, and total cost savings together
Scenario2_combinedplot <- Scenario2_Apt_plot + (Scenario2_Apt_penalty_plot / Scenario2_total_Apt_plot)


# Scenario 3 --------------------------------------------------------------

# Energy cost savings figures - per apartment

# Create table of Mean NPV, Standard Deviation NPV, and % of NPV < 0 for each energy price scenario and discount rate
# Plot histogram of undiscounted energy cost savings with table with NPVs for different discount rate scenarios and price scenarios


# Create table with Mean, Std Dev, and % <0 NPVs for discount rates and energy prices scenarios

Scenario3_Apt_NPVtable <- cbind(" " = c("0% DR","Mean", "Std Dev","% <0",
                                        "2% DR","Mean", "Std Dev","% <0",
                                        "5% DR","Mean", "Std Dev","% <0",
                                        "8% DR","Mean", "Std Dev","% <0"), 
                                Low = c("",
                                        format(round(Delta3EnergyCostsApt_Low_NPV_DRzero_Mean,0),big.mark = ","),
                                        format(round(Delta3EnergyCostsApt_Low_NPV_DRzero_StDev,0), big.mark = ","),
                                        paste0(format(round(Delta3EnergyCostsApt_Low_NPV_DRzero_PctNeg,1),nsmall = 1),"%"),
                                        "",
                                        format(round(Delta3EnergyCostsApt_Low_NPV_DRlow_Mean,0),big.mark = ","),
                                        format(round(Delta3EnergyCostsApt_Low_NPV_DRlow_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta3EnergyCostsApt_Low_NPV_DRlow_PctNeg,1),nsmall = 1),"%"),
                                        "",
                                        format(round(Delta3EnergyCostsApt_Low_NPV_DRmid_Mean,0),big.mark = ","),
                                        format(round(Delta3EnergyCostsApt_Low_NPV_DRmid_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta3EnergyCostsApt_Low_NPV_DRmid_PctNeg,1),nsmall = 1),"%"),
                                        "",
                                        format(round(Delta3EnergyCostsApt_Low_NPV_DRhigh_Mean,0),big.mark = ","),
                                        format(round(Delta3EnergyCostsApt_Low_NPV_DRhigh_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta3EnergyCostsApt_Low_NPV_DRhigh_PctNeg,1), nsmall = 1),"%")),
                                Mid = c("",
                                        format(round(Delta3EnergyCostsApt_Mid_NPV_DRzero_Mean,0),big.mark = ","),
                                        format(round(Delta3EnergyCostsApt_Mid_NPV_DRzero_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta3EnergyCostsApt_Mid_NPV_DRzero_PctNeg,1), nsmall = 1),"%"),
                                        "",
                                        format(round(Delta3EnergyCostsApt_Mid_NPV_DRlow_Mean,0),big.mark = ","),
                                        format(round(Delta3EnergyCostsApt_Mid_NPV_DRlow_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta3EnergyCostsApt_Mid_NPV_DRlow_PctNeg,1), nsmall = 1), "%"),
                                        "",
                                        format(round(Delta3EnergyCostsApt_Mid_NPV_DRmid_Mean,0),big.mark = ","),
                                        format(round(Delta3EnergyCostsApt_Mid_NPV_DRmid_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta3EnergyCostsApt_Mid_NPV_DRmid_PctNeg,1), nsmall = 1), "%"),
                                        "",
                                        format(round(Delta3EnergyCostsApt_Mid_NPV_DRhigh_Mean,0),big.mark = ","),
                                        format(round(Delta3EnergyCostsApt_Mid_NPV_DRhigh_StDev,0),big.mark = ","),
                                        paste0(format(round(Delta3EnergyCostsApt_Mid_NPV_DRhigh_PctNeg,1), nsmall = 1), "%")),
                                High = c("",
                                         format(round(Delta3EnergyCostsApt_High_NPV_DRzero_Mean,0),big.mark = ","),
                                         format(round(Delta3EnergyCostsApt_High_NPV_DRzero_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta3EnergyCostsApt_High_NPV_DRzero_PctNeg,1), nsmall = 1), "%"),
                                         "",
                                         format(round(Delta3EnergyCostsApt_High_NPV_DRlow_Mean,0),big.mark = ","),
                                         format(round(Delta3EnergyCostsApt_High_NPV_DRlow_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta3EnergyCostsApt_High_NPV_DRlow_PctNeg,1), nsmall = 1), "%"),
                                         "",
                                         format(round(Delta3EnergyCostsApt_High_NPV_DRmid_Mean,0),big.mark = ","),
                                         format(round(Delta3EnergyCostsApt_High_NPV_DRmid_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta3EnergyCostsApt_High_NPV_DRmid_PctNeg,1), nsmall = 1), "%"),
                                         "",
                                         format(round(Delta3EnergyCostsApt_High_NPV_DRhigh_Mean,0),big.mark = ","),
                                         format(round(Delta3EnergyCostsApt_High_NPV_DRhigh_StDev,0),big.mark = ","),
                                         paste0(format(round(Delta3EnergyCostsApt_High_NPV_DRhigh_PctNeg,1), nsmall = 1), "%")))

# Convert to data frame
Scenario3_Apt_NPVtable <- as.data.frame(Scenario3_Apt_NPVtable)

# Use ggtexttable to format table
Scenario3_Apt_NPVtable <- ggtexttable(Scenario3_Apt_NPVtable, rows = NULL,
                                      theme = ttheme("blank"))  %>%
  tab_add_hline(at.row = c(1:2,2:3,6:7,10:11,14:15), row.side = "top", linewidth = 2)


# graph undiscounted energy cost savings with NPV table in top right corner

Scenario3_Apt_plot <- ggplot() +
  geom_histogram(aes(x = Delta3EnergyCostsApt_Low$TotalCosts_2024to2050,
                     y = (..count..)/sum(..count..), fill = "Low"), 
                 binwidth = 700, alpha = 1) +
  geom_histogram(aes(x = Delta3EnergyCostsApt_Mid$TotalCosts_2024to2050, 
                     y = (..count..)/sum(..count..), fill = "Mid"),
                 binwidth = 700, alpha = 0.8) +
  geom_histogram(aes(x = Delta3EnergyCostsApt_High$TotalCosts_2024to2050, 
                     y = (..count..)/sum(..count..), fill = "High"),
                 binwidth = 700, alpha = 0.6) +
  scale_x_continuous(labels = scales::dollar_format(), limits = c(-100000,500000),
                     name = "Undiscounted Cumulative Energy Cost Savings, 2024-2050 ($/Apt)") +
  scale_y_continuous(labels = percent, name = "% of Apartments") + 
  ggtitle("Scenario 3: Undiscounted Cumulative\nEnergy Cost Savings, 2024-2050") +
  theme_classic() +
  scale_fill_manual(values = c("Low" = "#bdd7e7","Mid" = "#6baed6", "High" = "#08519c"), 
                    breaks = c("Low", "Mid", "High"),
                    name = "Price Scenario") +
  theme(legend.position = "bottom") +
  annotation_custom(ggplotGrob(Scenario3_Apt_NPVtable),
                    xmin = 220000, xmax = 420000, ymin = 0.003, ymax = 0.020)

# Avoided Penalties

# Create table with NPVs
Scenario3_Penalty_Apt_NPVtable <- cbind("Discount Rate" = c("0%", "2%", "5%", "8%"),
                                        "Mean" = c(format(round(Post3AvoidedPenaltiesApt_NPV_DRzero_Mean,0),big.mark = ","),
                                                   format(round(Post3AvoidedPenaltiesApt_NPV_DRlow_Mean,0),big.mark = ","),
                                                   format(round(Post3AvoidedPenaltiesApt_NPV_DRmid_Mean,0),big.mark = ","),
                                                   format(round(Post3AvoidedPenaltiesApt_NPV_DRhigh_Mean,0),big.mark = ",")),
                                        "St Dev" = c(format(round(Post3AvoidedPenaltiesApt_NPV_DRzero_StDev,0),big.mark = ","),
                                                     format(round(Post3AvoidedPenaltiesApt_NPV_DRlow_StDev,0),big.mark = ","),
                                                     format(round(Post3AvoidedPenaltiesApt_NPV_DRmid_StDev,0),big.mark = ","),
                                                     format(round(Post3AvoidedPenaltiesApt_NPV_DRhigh_StDev,0),big.mark = ",")))

# Convert to data frame
Scenario3_Penalty_Apt_NPVtable <- as.data.frame(Scenario3_Penalty_Apt_NPVtable)
# Use ggtexttable to format table
Scenario3_Penalty_Apt_NPVtable <- ggtexttable(Scenario3_Penalty_Apt_NPVtable, rows = NULL,
                                              theme = ttheme("blank"))  %>%
  tab_add_hline(at.row = c(1:2), row.side = "top", linewidth = 2)


# Plot
Scenario3_Apt_penalty_plot <- ggplot(Post3AvoidedPenaltiesApt, aes(x = UndiscountedTotal, y = ..count../sum(..count..))) + 
  geom_histogram(fill = "#74c476", binwidth = 100) +
  scale_x_continuous(name = "Total Avoided Penalties, 2024-2050 ($/Apt)",
                     labels = scales::dollar_format(),
                     limits = c(NA,70000)) +
  scale_y_continuous(name = "% of Apartments", label = percent) +
  ggtitle("Scenario 3: Undiscounted Cumulative\nAvoided Penalties, 2024-2050") +
  theme_classic() +
  annotation_custom(ggplotGrob(Scenario3_Penalty_Apt_NPVtable),
                    xmin = 40000, xmax = 60000, ymin = 0.007, ymax = 0.017)

# Total Cost Savings

# Create table with NPVs
Scenario3_Total_Apt_NPVtable <- cbind("Discount Rate" = c("0%","2%", "5%", "8%"),
                                      "Mean" = c(format(round(Post3CostSavingsApt_NPV_DRzero_Mean,0), big.mark = ","),
                                                 format(round(Post3CostSavingsApt_NPV_DRlow_Mean,0), big.mark = ","),
                                                 format(round(Post3CostSavingsApt_NPV_DRmid_Mean,0), big.mark = ","),
                                                 format(round(Post3CostSavingsApt_NPV_DRhigh_Mean,0), big.mark = ",")),
                                      "St Dev" = c(format(round(Post3CostSavingsApt_NPV_DRzero_StDev,0), big.mark = ","),
                                                   format(round(Post3CostSavingsApt_NPV_DRlow_StDev,0), big.mark = ","),
                                                   format(round(Post3CostSavingsApt_NPV_DRmid_StDev,0), big.mark = ","),
                                                   format(round(Post3CostSavingsApt_NPV_DRhigh_StDev,0), big.mark = ",")),
                                      "% <0" = c(paste0(format(round(Post3CostSavingsApt_NPV_DRzero_PctNeg,1),nsmall = 1),"%"),
                                                 paste0(format(round(Post3CostSavingsApt_NPV_DRlow_PctNeg,1),nsmall = 1),"%"),
                                                 paste0(format(round(Post3CostSavingsApt_NPV_DRmid_PctNeg,1),nsmall = 1),"%"),
                                                 paste0(format(round(Post3CostSavingsApt_NPV_DRhigh_PctNeg,1),nsmall = 1),"%")))

# Convert to data frame
Scenario3_Total_Apt_NPVtable <- as.data.frame(Scenario3_Total_Apt_NPVtable)
# Use ggtexttable to format table
Scenario3_Total_Apt_NPVtable <- ggtexttable(Scenario3_Total_Apt_NPVtable, rows = NULL,
                                            theme = ttheme("blank"))  %>%
  tab_add_hline(at.row = c(1:2), row.side = "top", linewidth = 2)


# Plot
Scenario3_total_Apt_plot <- ggplot(Post3CostSavingsApt, aes(x = UndiscountedTotal, y = ..count../sum(..count..))) + 
  geom_histogram(fill = "#7fcdbb", binwidth = 400) +
  scale_x_continuous(name = "Total Cost Savings, 2024-2050 ($/Apt)",
                     labels = scales::dollar_format(),
                     limits = c(-120000,250000)) +
  scale_y_continuous(name = "% of Apartments", label = percent) +
  ggtitle("Scenario 3: Undiscounted Cumulative\nCost Savings, 2024-2050") +
  theme_classic() +
  annotation_custom(ggplotGrob(Scenario3_Total_Apt_NPVtable),
                    xmin = 110000, xmax = 200000, ymin = 0.005, ymax = 0.009)

# Plot energy cost savings, avoided penalties, and total cost savings together
Scenario3_combinedplot <- Scenario3_Apt_plot + (Scenario3_Apt_penalty_plot / Scenario3_total_Apt_plot)
