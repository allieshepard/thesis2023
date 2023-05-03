# Scenario 1
# Script 7/10
# Run this script to model Scenario 1 - Gradual EUI Reductions - by
# calculating the EUI, energy costs, emissions, and penalties for all buildings through 2050,
# and comparing these to the BAU to calculate the energy cost savings, avoided penalties, and total cost savings.
# This script also calculates the Net Present Value (NPV) of each of these metrics
# under 3 discount rate scenarios (2%, 5%, 8%)


# EUI Calculations --------------------------------------------------------

# Calculate the % reduction in emissions required to meet LL97 limits
PercentEmissionsOverLL97Limits <- cbind(BAUEmissionsOverLimit[1:2],BAUEmissionsOverLimit[3:length(BAUEmissionsOverLimit)]/
                                          BAUEmissionsSqft[3:length(BAUEmissionsSqft)])

PercentEmissionsOverLL97Limits$`2050` <- 1

#replace NAs with zero (NAs occurred when 0/0)
PercentEmissionsOverLL97Limits[is.na(PercentEmissionsOverLL97Limits)] <- 0


# Calculate the Post-retrofit EUI for each fuel type 
# (assuming emissions exactly meet LL97 requirements) 

# Fuel Oil 2
EUIFuelOil2_Post1 <- cbind(EUIFuelOil2_BAU[1:2],EUIFuelOil2_BAU[3:length(EUIFuelOil2_BAU)]*
                             (1-PercentEmissionsOverLL97Limits[3:length(PercentEmissionsOverLL97Limits)]))

# Fuel Oil 4
EUIFuelOil4_Post1 <- cbind(EUIFuelOil4_BAU[1:2],EUIFuelOil4_BAU[3:length(EUIFuelOil4_BAU)]*
                             (1-PercentEmissionsOverLL97Limits[3:length(PercentEmissionsOverLL97Limits)]))

# Fuel Oil 5/6
EUIFuelOil56_Post1 <- cbind(EUIFuelOil56_BAU[1:2],EUIFuelOil56_BAU[3:length(EUIFuelOil56_BAU)]*
                              (1-PercentEmissionsOverLL97Limits[3:length(PercentEmissionsOverLL97Limits)]))

# District Steam
EUIDistrictSteam_Post1 <- cbind(EUIDistrictSteam_BAU[1:2],EUIDistrictSteam_BAU[3:length(EUIDistrictSteam_BAU)]*
                                  (1-PercentEmissionsOverLL97Limits[3:length(PercentEmissionsOverLL97Limits)]))

# Natural Gas
EUINaturalGas_Post1 <- cbind(EUINaturalGas_BAU[1:2],EUINaturalGas_BAU[3:length(EUINaturalGas_BAU)]*
                               (1-PercentEmissionsOverLL97Limits[3:length(PercentEmissionsOverLL97Limits)]))

# convert therms to kBtu

EUINaturalGas_Post1_kBtu <- cbind(EUINaturalGas_Post1[1:2],
                                  EUINaturalGas_Post1[3:length(EUINaturalGas_Post1)]*thermstokBtu)

# Electricity
EUIElectricity_Post1 <- cbind(EUIElectricity_BAU[1:2],EUIElectricity_BAU[3:length(EUIElectricity_BAU)]*
                                (1-PercentEmissionsOverLL97Limits[3:length(PercentEmissionsOverLL97Limits)]))

# convert kWh to kBtu

EUIElectricity_Post1_kBtu <- cbind(EUIElectricity_Post1[1:2],
                                   EUIElectricity_Post1[3:length(EUIElectricity_Post1)]*kWhtokBtu)

# Calculate total building-level EUI

TotalEUI_Post1 <- cbind(EUIFuelOil2_Post1[1:2],EUIFuelOil2_Post1[,columns]+EUIFuelOil4_Post1[,columns]+
                          EUIFuelOil56_Post1[,columns]+EUIDistrictSteam_Post1[,columns]+
                          EUINaturalGas_Post1_kBtu[,columns]+EUIElectricity_Post1_kBtu[,columns])


# Energy Cost Calculations ------------------------------------------------

# Calculate the Post1-retrofit costs for different price scenarios

# Fuel Oil 2

FuelOil2Costs_Post1_Low <- EUIFuelOil2_Post1
FuelOil2Costs_Post1_Low[,3:length(FuelOil2Costs_Post1_Low)] <- FuelOil2Costs_Post1_Low[,3:length(FuelOil2Costs_Post1_Low)]*FuelOil2Price_Low

FuelOil2Costs_Post1_Mid <- EUIFuelOil2_Post1
FuelOil2Costs_Post1_Mid[,3:length(FuelOil2Costs_Post1_Mid)] <- FuelOil2Costs_Post1_Mid[,3:length(FuelOil2Costs_Post1_Mid)]*FuelOil2Price_Mid

FuelOil2Costs_Post1_High <- EUIFuelOil2_Post1
FuelOil2Costs_Post1_High[,3:length(FuelOil2Costs_Post1_High)] <- FuelOil2Costs_Post1_High[,3:length(FuelOil2Costs_Post1_High)]*FuelOil2Price_High


# Fuel Oil 4

FuelOil4Costs_Post1_Low <- EUIFuelOil4_Post1
FuelOil4Costs_Post1_Low[,3:length(FuelOil4Costs_Post1_Low)] <- FuelOil4Costs_Post1_Low[,3:length(FuelOil4Costs_Post1_Low)]*FuelOil4Price_Low

FuelOil4Costs_Post1_Mid <- EUIFuelOil4_Post1
FuelOil4Costs_Post1_Mid[,3:length(FuelOil4Costs_Post1_Mid)] <- FuelOil4Costs_Post1_Mid[,3:length(FuelOil4Costs_Post1_Mid)]*FuelOil4Price_Mid

FuelOil4Costs_Post1_High <- EUIFuelOil4_Post1
FuelOil4Costs_Post1_High[,3:length(FuelOil4Costs_Post1_High)] <- FuelOil4Costs_Post1_High[,3:length(FuelOil4Costs_Post1_High)]*FuelOil4Price_High


# Fuel Oil 56

FuelOil56Costs_Post1_Low <- EUIFuelOil56_Post1
FuelOil56Costs_Post1_Low[,3:length(FuelOil56Costs_Post1_Low)] <- FuelOil56Costs_Post1_Low[,3:length(FuelOil56Costs_Post1_Low)]*FuelOil56Price_Low

FuelOil56Costs_Post1_Mid <- EUIFuelOil56_Post1
FuelOil56Costs_Post1_Mid[,3:length(FuelOil56Costs_Post1_Mid)] <- FuelOil56Costs_Post1_Mid[,3:length(FuelOil56Costs_Post1_Mid)]*FuelOil56Price_Mid

FuelOil56Costs_Post1_High <- EUIFuelOil56_Post1
FuelOil56Costs_Post1_High[,3:length(FuelOil56Costs_Post1_High)] <- FuelOil56Costs_Post1_High[,3:length(FuelOil56Costs_Post1_High)]*FuelOil56Price_High


# District Steam

DistrictSteamCosts_Post1_Low <- EUIDistrictSteam_Post1
DistrictSteamCosts_Post1_Low[,3:length(DistrictSteamCosts_Post1_Low)] <- DistrictSteamCosts_Post1_Low[,3:length(DistrictSteamCosts_Post1_Low)]*DistrictSteamPrice_Low

DistrictSteamCosts_Post1_Mid <- EUIDistrictSteam_Post1
DistrictSteamCosts_Post1_Mid[,3:length(DistrictSteamCosts_Post1_Mid)] <- DistrictSteamCosts_Post1_Mid[,3:length(DistrictSteamCosts_Post1_Mid)]*DistrictSteamPrice_Mid

DistrictSteamCosts_Post1_High <- EUIDistrictSteam_Post1
DistrictSteamCosts_Post1_High[,3:length(DistrictSteamCosts_Post1_High)] <- DistrictSteamCosts_Post1_High[,3:length(DistrictSteamCosts_Post1_High)]*DistrictSteamPrice_High


# Natural Gas

NaturalGasCosts_Post1_Low <- EUINaturalGas_Post1
NaturalGasCosts_Post1_Low[,3:length(NaturalGasCosts_Post1_Low)] <- NaturalGasCosts_Post1_Low[,3:length(NaturalGasCosts_Post1_Low)]*NaturalGasPrice_Low

NaturalGasCosts_Post1_Mid <- EUINaturalGas_Post1
NaturalGasCosts_Post1_Mid[,3:length(NaturalGasCosts_Post1_Mid)] <- NaturalGasCosts_Post1_Mid[,3:length(NaturalGasCosts_Post1_Mid)]*NaturalGasPrice_Mid

NaturalGasCosts_Post1_High <- EUINaturalGas_Post1
NaturalGasCosts_Post1_High[,3:length(NaturalGasCosts_Post1_High)] <- NaturalGasCosts_Post1_High[,3:length(NaturalGasCosts_Post1_High)]*NaturalGasPrice_High


# Electricity

ElectricityCosts_Post1_Low <- EUIElectricity_Post1
ElectricityCosts_Post1_Low[,3:length(ElectricityCosts_Post1_Low)] <- ElectricityCosts_Post1_Low[,3:length(ElectricityCosts_Post1_Low)]*ElectricityPrice_Low

ElectricityCosts_Post1_Mid <- EUIElectricity_Post1
ElectricityCosts_Post1_Mid[,3:length(ElectricityCosts_Post1_Mid)] <- ElectricityCosts_Post1_Mid[,3:length(ElectricityCosts_Post1_Mid)]*ElectricityPrice_Mid

ElectricityCosts_Post1_High <- EUIElectricity_Post1
ElectricityCosts_Post1_High[,3:length(ElectricityCosts_Post1_High)] <- ElectricityCosts_Post1_High[,3:length(ElectricityCosts_Post1_High)]*ElectricityPrice_High


# Add all costs together
# UNITS: $/sqft/yr
# help from: https://stackoverflow.com/questions/54078513/how-to-sum-same-column-of-different-data-frames-in-r

columns <- as.character(2019:2050)

# Low, mid, high scenarios
Post1EnergyCostsSqft_Low <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

Post1EnergyCostsSqft_Low <- FuelOil2Costs_Post1_Low[, columns] + FuelOil4Costs_Post1_Low[, columns] + FuelOil56Costs_Post1_Low[, columns] +
  DistrictSteamCosts_Post1_Low[, columns] + NaturalGasCosts_Post1_Low[, columns] + ElectricityCosts_Post1_Low[, columns]

Post1EnergyCostsSqft_Low <- Post1EnergyCostsSqft_Low %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

Post1EnergyCostsSqft_Mid <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

Post1EnergyCostsSqft_Mid <- FuelOil2Costs_Post1_Mid[, columns] + FuelOil4Costs_Post1_Mid[, columns] + FuelOil56Costs_Post1_Mid[, columns] +
  DistrictSteamCosts_Post1_Mid[, columns] + NaturalGasCosts_Post1_Mid[, columns] + ElectricityCosts_Post1_Mid[, columns]

Post1EnergyCostsSqft_Mid <- Post1EnergyCostsSqft_Mid %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

Post1EnergyCostsSqft_High <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

Post1EnergyCostsSqft_High <- FuelOil2Costs_Post1_High[, columns] + FuelOil4Costs_Post1_High[, columns] + FuelOil56Costs_Post1_High[, columns] +
  DistrictSteamCosts_Post1_High[, columns] + NaturalGasCosts_Post1_High[, columns] + ElectricityCosts_Post1_High[, columns]

Post1EnergyCostsSqft_High <- Post1EnergyCostsSqft_High %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

# Add columns for total, undiscounted costs, 2024-2050
Post1EnergyCostsSqft_Low <- Post1EnergyCostsSqft_Low %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

Post1EnergyCostsSqft_Mid <- Post1EnergyCostsSqft_Mid %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

Post1EnergyCostsSqft_High <- Post1EnergyCostsSqft_High %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))


# Scenario 1 energy cost savings
Delta1EnergyCostsSqft_Low <- cbind(Post1EnergyCostsSqft_Low[1:2],BAUEnergyCostsSqft_Low[3:length(BAUEnergyCostsSqft_Low)]-
                                     Post1EnergyCostsSqft_Low[3:length(Post1EnergyCostsSqft_Low)])

Delta1EnergyCostsSqft_Mid <- cbind(Post1EnergyCostsSqft_Mid[1:2],BAUEnergyCostsSqft_Mid[3:length(BAUEnergyCostsSqft_Mid)]-
                                     Post1EnergyCostsSqft_Mid[3:length(Post1EnergyCostsSqft_Mid)])

Delta1EnergyCostsSqft_High <- cbind(Post1EnergyCostsSqft_High[1:2],BAUEnergyCostsSqft_High[3:length(BAUEnergyCostsSqft_High)]-
                                      Post1EnergyCostsSqft_High[3:length(Post1EnergyCostsSqft_High)])



# Penalty Calculations ----------------------------------------------------

# Fuel Oil 2
FuelOil2Emissions_Post1 <- mapply("*", EUIFuelOil2_Post1[intersect(names(EUIFuelOil2_Post1), names(FuelOil2EF))],
                                  FuelOil2EF[intersect(names(EUIFuelOil2_Post1), names(FuelOil2EF))]) %>% 
  as.data.frame()

FuelOil2Emissions_Post1 <- FuelOil2Emissions_Post1 %>% 
  mutate("HousingType" = EUIFuelOil2_Post1$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil2_Post1$BBL) %>% 
  relocate(BBL)

# Fuel Oil 4
FuelOil4Emissions_Post1 <- mapply("*", EUIFuelOil4_Post1[intersect(names(EUIFuelOil4_Post1), names(FuelOil4EF))],
                                  FuelOil4EF[intersect(names(EUIFuelOil4_Post1), names(FuelOil4EF))]) %>% 
  as.data.frame()

FuelOil4Emissions_Post1 <- FuelOil4Emissions_Post1 %>% 
  mutate("HousingType" = EUIFuelOil4_Post1$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil4_Post1$BBL) %>% 
  relocate(BBL)

# Fuel Oil 56
FuelOil56Emissions_Post1 <- mapply("*", EUIFuelOil56_Post1[intersect(names(EUIFuelOil56_Post1), names(FuelOil56EF))],
                                   FuelOil56EF[intersect(names(EUIFuelOil56_Post1), names(FuelOil56EF))]) %>% 
  as.data.frame()

FuelOil56Emissions_Post1 <- FuelOil56Emissions_Post1 %>% 
  mutate("HousingType" = EUIFuelOil56_Post1$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil56_Post1$BBL) %>% 
  relocate(BBL)

# District Steam
DistrictSteamEmissions_Post1 <- mapply("*", EUIDistrictSteam_Post1[intersect(names(EUIDistrictSteam_Post1), names(DistrictSteamEF))],
                                       DistrictSteamEF[intersect(names(EUIDistrictSteam_Post1), names(DistrictSteamEF))]) %>% 
  as.data.frame()

DistrictSteamEmissions_Post1 <- DistrictSteamEmissions_Post1 %>% 
  mutate("HousingType" = EUIDistrictSteam_Post1$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIDistrictSteam_Post1$BBL) %>% 
  relocate(BBL)

# Natural Gas
NaturalGasEmissions_Post1 <- mapply("*", EUINaturalGas_Post1[intersect(names(EUINaturalGas_Post1), names(NaturalGasEF))],
                                    NaturalGasEF[intersect(names(EUINaturalGas_Post1), names(NaturalGasEF))]) %>% 
  as.data.frame()

NaturalGasEmissions_Post1 <- NaturalGasEmissions_Post1 %>% 
  mutate("HousingType" = EUINaturalGas_Post1$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUINaturalGas_Post1$BBL) %>% 
  relocate(BBL)

# Electricity
ElectricityEmissions_Post1 <- mapply("*", EUIElectricity_Post1[intersect(names(EUIElectricity_Post1), names(ElectricityEF))],
                                     ElectricityEF[intersect(names(EUIElectricity_Post1), names(ElectricityEF))]) %>% 
  as.data.frame()

ElectricityEmissions_Post1 <- ElectricityEmissions_Post1 %>% 
  mutate("HousingType" = EUIElectricity_Post1$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIElectricity_Post1$BBL) %>% 
  relocate(BBL)

# Add all emissions together
# UNITS: MTCO2/sqft/yr

Post1EmissionsSqft <- cbind(FuelOil2Emissions_Post1[1:2], 
                            FuelOil2Emissions_Post1[, columns] + FuelOil4Emissions_Post1[, columns] + 
                              FuelOil56Emissions_Post1[, columns] + DistrictSteamEmissions_Post1[, columns] + 
                              NaturalGasEmissions_Post1[, columns] + ElectricityEmissions_Post1[, columns])

# Calculate difference between Post 1 emissions intensity and LL97 limits

# subtract LL97 limits from BAU emissions
Post1EmissionsOverLimit <- mapply("-", Post1EmissionsSqft[intersect(names(Post1EmissionsSqft), names(LL97limits))],
                                  LL97limits[intersect(names(Post1EmissionsSqft), names(LL97limits))]) %>% 
  as.data.frame()

# replace negatives with zero
Post1EmissionsOverLimit[Post1EmissionsOverLimit<0] <- 0

Post1EmissionsOverLimit <- Post1EmissionsOverLimit %>% 
  mutate("HousingType" = Post1EmissionsSqft$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = Post1EmissionsSqft$BBL) %>% 
  relocate(BBL)

# Calculate penalties for each building 2019-2050

Post1LL97PenaltiesBldg <- Post1EmissionsOverLimit %>% 
  left_join(BuildingSize, by = "BBL") %>% 
  rename(BldgArea = `Multifamily Housing - Gross Floor Area (ft²)`) %>% 
  relocate(c("BBL", "HousingType", "BldgArea", "Apartments"))

Post1LL97PenaltiesBldg <- Post1LL97PenaltiesBldg %>% 
  mutate(across(starts_with("2"),~.*BldgArea*LL97penalty/1000))

Post1LL97PenaltiesBldg <- Post1LL97PenaltiesBldg %>% 
  mutate("TotalPenalties_2024to2050" = rowSums(across(`2019`:`2050`), na.rm = TRUE))

# Calculate penalties per sqft

Post1LL97PenaltiesSqft <- Post1LL97PenaltiesBldg %>% 
  mutate(across(starts_with("2"),~./BldgArea))

# add column for undiscounted penalty total (2019-2050)
Post1LL97PenaltiesSqft <- Post1LL97PenaltiesSqft %>% 
  mutate("UndiscountedPenalties" = rowSums(across(starts_with("2")), na.rm = TRUE)) 

# Calculate avoided penalties

Post1AvoidedPenaltiesSqft <- as.data.frame(BAULL97PenaltiesSqft$UndiscountedPenalties - Post1LL97PenaltiesSqft$UndiscountedPenalties) %>% 
  rename("AvoidedPenalties" = "BAULL97PenaltiesSqft$UndiscountedPenalties - Post1LL97PenaltiesSqft$UndiscountedPenalties")

Post1AvoidedPenaltiesSqft <- cbind(Post1LL97PenaltiesSqft[1:4],BAULL97PenaltiesSqft[5:(length(BAULL97PenaltiesSqft)-2)]-
                                     Post1LL97PenaltiesSqft[5:(length(Post1LL97PenaltiesSqft)-2)])


# Total Cost Savings Calculation ------------------------------------------

Post1CostSavingsSqft <- as.data.frame(Post1AvoidedPenaltiesSqft$AvoidedPenalties + Delta1EnergyCostsSqft_Mid$TotalCosts_2024to2050) %>% 
  rename("TotalCostSavings" = "Post1AvoidedPenaltiesSqft$AvoidedPenalties + Delta1EnergyCostsSqft_Mid$TotalCosts_2024to2050")


# NPV Calculations --------------------------------------------------------

# time period
timeperiod <- 1:32
# discount rates
dr_low <- 0.02
dr_mid <- 0.05
dr_high <- 0.08
dr <- c(dr_low, dr_mid, dr_high)

# Energy Cost Savings NPVs

Delta1EnergyCostsSqft_Low_NPV <- cbind(Delta1EnergyCostsSqft_Low[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

Delta1EnergyCostsSqft_Mid_NPV <- cbind(Delta1EnergyCostsSqft_Mid[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

Delta1EnergyCostsSqft_High_NPV <- cbind(Delta1EnergyCostsSqft_High[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")


for (i in 1:length(dr)) {
  for (t in 1:nrow(Delta1EnergyCostsSqft_Low)) {
    
    Delta1EnergyCostsSqft_Low_NPV[t,i+2] <- NPV(cf0 = Delta1EnergyCostsSqft_Low[t,3],
                                                cf = as.numeric(Delta1EnergyCostsSqft_Low[t,3:(length(Delta1EnergyCostsSqft_Low)-1)],
                                                                na.rm = TRUE),
                                                times = timeperiod,
                                                i = dr[i])
    
    
    Delta1EnergyCostsSqft_Mid_NPV[t,i+2] <- NPV(cf0 = Delta1EnergyCostsSqft_Mid[t,3],
                                                cf = as.numeric(Delta1EnergyCostsSqft_Mid[t,3:(length(Delta1EnergyCostsSqft_Mid)-1)]),
                                                times = timeperiod,
                                                i = dr[i])
    
    Delta1EnergyCostsSqft_High_NPV[t,i+2] <- NPV(cf0 = Delta1EnergyCostsSqft_High[t,3],
                                                 cf = as.numeric(Delta1EnergyCostsSqft_High[t,3:(length(Delta1EnergyCostsSqft_High)-1)]),
                                                 times = timeperiod,
                                                 i = dr[i])
  }
}


# Create new dataframes for costs by apartment
Delta1EnergyCostsApt_Low <- Delta1EnergyCostsSqft_Low %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("T"),~.*AptSize$AptSqft))

Delta1EnergyCostsApt_Mid <- Delta1EnergyCostsSqft_Mid %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("T"),~.*AptSize$AptSqft))

Delta1EnergyCostsApt_High <- Delta1EnergyCostsSqft_High %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("T"),~.*AptSize$AptSqft))

Delta1EnergyCostsApt_Low_NPV <- Delta1EnergyCostsSqft_Low_NPV %>% 
  mutate(across(starts_with("NPV"),~.*AptSize$AptSqft))

Delta1EnergyCostsApt_Mid_NPV <- Delta1EnergyCostsSqft_Mid_NPV %>% 
  mutate(across(starts_with("NPV"),~.*AptSize$AptSqft))

Delta1EnergyCostsApt_High_NPV <- Delta1EnergyCostsSqft_High_NPV %>% 
  mutate(across(starts_with("NPV"),~.*AptSize$AptSqft))


# Zero Discount Rate
Delta1EnergyCostsApt_Low_NPV_DRzero_Mean <- mean(Delta1EnergyCostsApt_Low$TotalCosts_2024to2050)
Delta1EnergyCostsApt_Mid_NPV_DRzero_Mean <- mean(Delta1EnergyCostsApt_Mid$TotalCosts_2024to2050)
Delta1EnergyCostsApt_High_NPV_DRzero_Mean <- mean(Delta1EnergyCostsApt_High$TotalCosts_2024to2050)

Delta1EnergyCostsApt_Low_NPV_DRzero_StDev <- sd(Delta1EnergyCostsApt_Low$TotalCosts_2024to2050)
Delta1EnergyCostsApt_Mid_NPV_DRzero_StDev <- sd(Delta1EnergyCostsApt_Mid$TotalCosts_2024to2050)
Delta1EnergyCostsApt_High_NPV_DRzero_StDev <- sd(Delta1EnergyCostsApt_High$TotalCosts_2024to2050)

Delta1EnergyCostsApt_Low_NPV_DRzero_PctNeg <- sum(Delta1EnergyCostsApt_Low$TotalCosts_2024to2050 < 0)/nrow(Delta1EnergyCostsApt_Low)*100
Delta1EnergyCostsApt_Mid_NPV_DRzero_PctNeg <- sum(Delta1EnergyCostsApt_Mid$TotalCosts_2024to2050 < 0)/nrow(Delta1EnergyCostsApt_Mid)*100
Delta1EnergyCostsApt_High_NPV_DRzero_PctNeg <- sum(Delta1EnergyCostsApt_High$TotalCosts_2024to2050 < 0)/nrow(Delta1EnergyCostsApt_High)*100

# Low Discount Rate
Delta1EnergyCostsApt_Low_NPV_DRlow_Median <- median(Delta1EnergyCostsApt_Low_NPV$NPV_DRlow)
Delta1EnergyCostsApt_Mid_NPV_DRlow_Median <- median(Delta1EnergyCostsApt_Mid_NPV$NPV_DRlow)
Delta1EnergyCostsApt_High_NPV_DRlow_Median <- median(Delta1EnergyCostsApt_High_NPV$NPV_DRlow)

Delta1EnergyCostsApt_Low_NPV_DRlow_Mean <- mean(Delta1EnergyCostsApt_Low_NPV$NPV_DRlow)
Delta1EnergyCostsApt_Mid_NPV_DRlow_Mean <- mean(Delta1EnergyCostsApt_Mid_NPV$NPV_DRlow)
Delta1EnergyCostsApt_High_NPV_DRlow_Mean <- mean(Delta1EnergyCostsApt_High_NPV$NPV_DRlow)

Delta1EnergyCostsApt_Low_NPV_DRlow_StDev <- sd(Delta1EnergyCostsApt_Low_NPV$NPV_DRlow)
Delta1EnergyCostsApt_Mid_NPV_DRlow_StDev <- sd(Delta1EnergyCostsApt_Mid_NPV$NPV_DRlow)
Delta1EnergyCostsApt_High_NPV_DRlow_StDev <- sd(Delta1EnergyCostsApt_High_NPV$NPV_DRlow)

Delta1EnergyCostsApt_Low_NPV_DRlow_PctNeg <- sum(Delta1EnergyCostsApt_Low_NPV$NPV_DRlow < 0)/nrow(Delta1EnergyCostsApt_Low_NPV)*100
Delta1EnergyCostsApt_Mid_NPV_DRlow_PctNeg <- sum(Delta1EnergyCostsApt_Mid_NPV$NPV_DRlow < 0)/nrow(Delta1EnergyCostsApt_Mid_NPV)*100
Delta1EnergyCostsApt_High_NPV_DRlow_PctNeg <- sum(Delta1EnergyCostsApt_High_NPV$NPV_DRlow < 0)/nrow(Delta1EnergyCostsApt_High_NPV)*100

# Mid Discount Rate
Delta1EnergyCostsApt_Low_NPV_DRmid_Median <- median(Delta1EnergyCostsApt_Low_NPV$NPV_DRmid)
Delta1EnergyCostsApt_Mid_NPV_DRmid_Median <- median(Delta1EnergyCostsApt_Mid_NPV$NPV_DRmid)
Delta1EnergyCostsApt_High_NPV_DRmid_Median <- median(Delta1EnergyCostsApt_High_NPV$NPV_DRmid)

Delta1EnergyCostsApt_Low_NPV_DRmid_Mean <- mean(Delta1EnergyCostsApt_Low_NPV$NPV_DRmid)
Delta1EnergyCostsApt_Mid_NPV_DRmid_Mean <- mean(Delta1EnergyCostsApt_Mid_NPV$NPV_DRmid)
Delta1EnergyCostsApt_High_NPV_DRmid_Mean <- mean(Delta1EnergyCostsApt_High_NPV$NPV_DRmid)

Delta1EnergyCostsApt_Low_NPV_DRmid_StDev <- sd(Delta1EnergyCostsApt_Low_NPV$NPV_DRmid)
Delta1EnergyCostsApt_Mid_NPV_DRmid_StDev <- sd(Delta1EnergyCostsApt_Mid_NPV$NPV_DRmid)
Delta1EnergyCostsApt_High_NPV_DRmid_StDev <- sd(Delta1EnergyCostsApt_High_NPV$NPV_DRmid)

Delta1EnergyCostsApt_Low_NPV_DRmid_PctNeg <- sum(Delta1EnergyCostsApt_Low_NPV$NPV_DRmid < 0)/nrow(Delta1EnergyCostsApt_Low_NPV)*100
Delta1EnergyCostsApt_Mid_NPV_DRmid_PctNeg <- sum(Delta1EnergyCostsApt_Mid_NPV$NPV_DRmid < 0)/nrow(Delta1EnergyCostsApt_Mid_NPV)*100
Delta1EnergyCostsApt_High_NPV_DRmid_PctNeg <- sum(Delta1EnergyCostsApt_High_NPV$NPV_DRmid < 0)/nrow(Delta1EnergyCostsApt_High_NPV)*100


# High Discount Rate
Delta1EnergyCostsApt_Low_NPV_DRhigh_Median <- median(Delta1EnergyCostsApt_Low_NPV$NPV_DRhigh)
Delta1EnergyCostsApt_Mid_NPV_DRhigh_Median <- median(Delta1EnergyCostsApt_Mid_NPV$NPV_DRhigh)
Delta1EnergyCostsApt_High_NPV_DRhigh_Median <- median(Delta1EnergyCostsApt_High_NPV$NPV_DRhigh)

Delta1EnergyCostsApt_Low_NPV_DRhigh_Mean <- mean(Delta1EnergyCostsApt_Low_NPV$NPV_DRhigh)
Delta1EnergyCostsApt_Mid_NPV_DRhigh_Mean <- mean(Delta1EnergyCostsApt_Mid_NPV$NPV_DRhigh)
Delta1EnergyCostsApt_High_NPV_DRhigh_Mean <- mean(Delta1EnergyCostsApt_High_NPV$NPV_DRhigh)

Delta1EnergyCostsApt_Low_NPV_DRhigh_StDev <- sd(Delta1EnergyCostsApt_Low_NPV$NPV_DRhigh)
Delta1EnergyCostsApt_Mid_NPV_DRhigh_StDev <- sd(Delta1EnergyCostsApt_Mid_NPV$NPV_DRhigh)
Delta1EnergyCostsApt_High_NPV_DRhigh_StDev <- sd(Delta1EnergyCostsApt_High_NPV$NPV_DRhigh)

Delta1EnergyCostsApt_Low_NPV_DRhigh_PctNeg <- sum(Delta1EnergyCostsApt_Low_NPV$NPV_DRhigh < 0)/nrow(Delta1EnergyCostsApt_Low_NPV)*100
Delta1EnergyCostsApt_Mid_NPV_DRhigh_PctNeg <- sum(Delta1EnergyCostsApt_Mid_NPV$NPV_DRhigh < 0)/nrow(Delta1EnergyCostsApt_Mid_NPV)*100
Delta1EnergyCostsApt_High_NPV_DRhigh_PctNeg <- sum(Delta1EnergyCostsApt_High_NPV$NPV_DRhigh < 0)/nrow(Delta1EnergyCostsApt_High_NPV)*100


# Avoided Penalties NPVs

# Per SQFT

Post1AvoidedPenaltiesSqft_NPV <- cbind(Post1AvoidedPenaltiesSqft[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

for (i in 1:length(dr)) {
  for (t in 1:nrow(Post1AvoidedPenaltiesSqft_NPV)) {
    
    Post1AvoidedPenaltiesSqft_NPV[t,i+2] <- NPV(cf0 = Post1AvoidedPenaltiesSqft[t,5],
                                                cf = as.numeric(Post1AvoidedPenaltiesSqft[t,5:length(Post1AvoidedPenaltiesSqft)],
                                                                na.rm = TRUE),
                                                times = timeperiod,
                                                i = dr[i])
  }
}

Post1AvoidedPenaltiesSqft_NPV_DRlow_Mean <- mean(Post1AvoidedPenaltiesSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post1AvoidedPenaltiesSqft_NPV_DRlow_StDev <- sd(Post1AvoidedPenaltiesSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post1AvoidedPenaltiesSqft_NPV_DRmid_Mean <- mean(Post1AvoidedPenaltiesSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post1AvoidedPenaltiesSqft_NPV_DRmid_StDev <- sd(Post1AvoidedPenaltiesSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post1AvoidedPenaltiesSqft_NPV_DRhigh_Mean <- mean(Post1AvoidedPenaltiesSqft_NPV$NPV_DRhigh, na.rm = TRUE)
Post1AvoidedPenaltiesSqft_NPV_DRhigh_StDev <- sd(Post1AvoidedPenaltiesSqft_NPV$NPV_DRhigh, na.rm = TRUE)

# Add total undiscounted column
Post1AvoidedPenaltiesSqft <- Post1AvoidedPenaltiesSqft %>% 
  mutate("UndiscountedTotal" = rowSums(across(`2024`:`2050`), na.rm = TRUE))
# Calculate Mean, St Dev for undiscounted avoided penalties
Post1AvoidedPenaltiesSqft_NPV_DRzero_Mean <- mean(Post1AvoidedPenaltiesSqft$UndiscountedTotal, na.rm = TRUE)
Post1AvoidedPenaltiesSqft_NPV_DRzero_StDev <- sd(Post1AvoidedPenaltiesSqft$UndiscountedTotal, na.rm = TRUE)

# Per APT

# Create new dataframes for avoided penalties by apartment
Post1AvoidedPenaltiesApt <- Post1AvoidedPenaltiesSqft %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("U"),~.*AptSize$AptSqft))

Post1AvoidedPenaltiesApt_NPV <- Post1AvoidedPenaltiesSqft_NPV %>% 
  mutate(across(starts_with("N"),~.*AptSize$AptSqft))

Post1AvoidedPenaltiesApt_NPV_DRlow_Mean <- mean(Post1AvoidedPenaltiesApt_NPV$NPV_DRlow, na.rm = TRUE)
Post1AvoidedPenaltiesApt_NPV_DRlow_StDev <- sd(Post1AvoidedPenaltiesApt_NPV$NPV_DRlow, na.rm = TRUE)
Post1AvoidedPenaltiesApt_NPV_DRmid_Mean <- mean(Post1AvoidedPenaltiesApt_NPV$NPV_DRmid, na.rm = TRUE)
Post1AvoidedPenaltiesApt_NPV_DRmid_StDev <- sd(Post1AvoidedPenaltiesApt_NPV$NPV_DRmid, na.rm = TRUE)
Post1AvoidedPenaltiesApt_NPV_DRhigh_Mean <- mean(Post1AvoidedPenaltiesApt_NPV$NPV_DRhigh, na.rm = TRUE)
Post1AvoidedPenaltiesApt_NPV_DRhigh_StDev <- sd(Post1AvoidedPenaltiesApt_NPV$NPV_DRhigh, na.rm = TRUE)


# Calculate Mean, St Dev for undiscounted avoided penalties
Post1AvoidedPenaltiesApt_NPV_DRzero_Mean <- mean(Post1AvoidedPenaltiesApt$UndiscountedTotal, na.rm = TRUE)
Post1AvoidedPenaltiesApt_NPV_DRzero_StDev <- sd(Post1AvoidedPenaltiesApt$UndiscountedTotal, na.rm = TRUE)


# Total Cost Savings

# per SQFT
Post1CostSavingsSqft <- cbind(Post1AvoidedPenaltiesSqft[1:4],
                              Delta1EnergyCostsSqft_Mid[3:(length(Delta1EnergyCostsSqft_Mid)-1)] + 
                                Post1AvoidedPenaltiesSqft[5:(length(Post1AvoidedPenaltiesSqft)-1)])

Post1CostSavingsSqft <- Post1CostSavingsSqft %>% 
  mutate("UndiscountedTotal" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

# Calculate NPV
Post1CostSavingsSqft_NPV <- cbind(Post1CostSavingsSqft[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

for (i in 1:length(dr)) {
  for (t in 1:nrow(Post1CostSavingsSqft_NPV)) {
    
    Post1CostSavingsSqft_NPV[t,i+2] <- NPV(cf0 = Post1CostSavingsSqft[t,5],
                                           cf = as.numeric(Post1CostSavingsSqft[t,5:(length(Post1CostSavingsSqft)-1)],
                                                           na.rm = TRUE),
                                           times = timeperiod,
                                           i = dr[i])
  }
}

Post1CostSavingsSqft_NPV_DRzero_Mean <- mean(Post1CostSavingsSqft$UndiscountedTotal, na.rm = TRUE)
Post1CostSavingsSqft_NPV_DRzero_StDev <- sd(Post1CostSavingsSqft$UndiscountedTotal, na.rm = TRUE)
Post1CostSavingsSqft_NPV_DRzero_PctNeg <- sum(Post1CostSavingsSqft$UndiscountedTotal < 0)/nrow(Post1CostSavingsSqft)*100
Post1CostSavingsSqft_NPV_DRlow_Mean <- mean(Post1CostSavingsSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post1CostSavingsSqft_NPV_DRlow_StDev <- sd(Post1CostSavingsSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post1CostSavingsSqft_NPV_DRlow_PctNeg <- sum(Post1CostSavingsSqft_NPV$NPV_DRlow < 0)/nrow(Post1CostSavingsSqft_NPV)*100
Post1CostSavingsSqft_NPV_DRmid_Mean <- mean(Post1CostSavingsSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post1CostSavingsSqft_NPV_DRmid_StDev <- sd(Post1CostSavingsSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post1CostSavingsSqft_NPV_DRmid_PctNeg <- sum(Post1CostSavingsSqft_NPV$NPV_DRmid < 0)/nrow(Post1CostSavingsSqft_NPV)*100
Post1CostSavingsSqft_NPV_DRhigh_Mean <- mean(Post1CostSavingsSqft_NPV$NPV_DRhigh, na.rm = TRUE)
Post1CostSavingsSqft_NPV_DRhigh_StDev <- sd(Post1CostSavingsSqft_NPV$NPV_DRhigh, na.rm = TRUE)
Post1CostSavingsSqft_NPV_DRhigh_PctNeg <- sum(Post1CostSavingsSqft_NPV$NPV_DRhigh < 0)/nrow(Post1CostSavingsSqft_NPV)*100

# per APT

# Create new NPV dataframes
Post1CostSavingsApt <- Post1CostSavingsSqft %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("U"),~.*AptSize$AptSqft))

Post1CostSavingsApt_NPV <- Post1CostSavingsSqft_NPV %>% 
  mutate(across(starts_with("N"),~.*AptSize$AptSqft))

# Calculate Mean, SD, % <0
Post1CostSavingsApt_NPV_DRzero_Mean <- mean(Post1CostSavingsApt$UndiscountedTotal, na.rm = TRUE)
Post1CostSavingsApt_NPV_DRzero_StDev <- sd(Post1CostSavingsApt$UndiscountedTotal, na.rm = TRUE)
Post1CostSavingsApt_NPV_DRzero_PctNeg <- sum(Post1CostSavingsApt$UndiscountedTotal < 0)/nrow(Post1CostSavingsApt)*100
Post1CostSavingsApt_NPV_DRlow_Mean <- mean(Post1CostSavingsApt_NPV$NPV_DRlow, na.rm = TRUE)
Post1CostSavingsApt_NPV_DRlow_StDev <- sd(Post1CostSavingsApt_NPV$NPV_DRlow, na.rm = TRUE)
Post1CostSavingsApt_NPV_DRlow_PctNeg <- sum(Post1CostSavingsApt_NPV$NPV_DRlow < 0)/nrow(Post1CostSavingsApt_NPV)*100
Post1CostSavingsApt_NPV_DRmid_Mean <- mean(Post1CostSavingsApt_NPV$NPV_DRmid, na.rm = TRUE)
Post1CostSavingsApt_NPV_DRmid_StDev <- sd(Post1CostSavingsApt_NPV$NPV_DRmid, na.rm = TRUE)
Post1CostSavingsApt_NPV_DRmid_PctNeg <- sum(Post1CostSavingsApt_NPV$NPV_DRmid < 0)/nrow(Post1CostSavingsApt_NPV)*100
Post1CostSavingsApt_NPV_DRhigh_Mean <- mean(Post1CostSavingsApt_NPV$NPV_DRhigh, na.rm = TRUE)
Post1CostSavingsApt_NPV_DRhigh_StDev <- sd(Post1CostSavingsApt_NPV$NPV_DRhigh, na.rm = TRUE)
Post1CostSavingsApt_NPV_DRhigh_PctNeg <- sum(Post1CostSavingsApt_NPV$NPV_DRhigh < 0)/nrow(Post1CostSavingsApt_NPV)*100

