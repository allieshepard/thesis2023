# Scenario 3
# Script 9/10
# Run this script to model Scenario 3 - Fully Electric Passive House in 2024 - by
# calculating the EUI, energy costs, emissions, and penalties for all buildings through 2050,
# and comparing these to the BAU to calculate the energy cost savings, avoided penalties, and total cost savings.
# This script also calculates the Net Present Value (NPV) of each of these metrics
# under 3 discount rate scenarios (2%, 5%, 8%)

# EUI Calculations --------------------------------------------------------

# Fuel Oils, District Steam, Natural Gas same as Scenario 2

EUIFuelOil2_Post3 <- EUIFuelOil2_Post2
EUIFuelOil4_Post3 <- EUIFuelOil4_Post2
EUIFuelOil56_Post3 <- EUIFuelOil56_Post2
EUIDistrictSteam_Post3 <- EUIDistrictSteam_Post2
EUINaturalGas_Post3 <- EUINaturalGas_Post2
EUINaturalGas_Post3_kBtu <- EUINaturalGas_Post2_kBtu

# Electricity EUI is same as Scenario 2 for 2019-2023, then drops to Passive House standard EUI in 2024
# Passive House Standard EUI = 38.0 kBtu/sqft/yr
# Source: https://www.aceee.org/files/proceedings/2016/data/papers/5_742.pdf
PassiveHouseEUI <- 38

EUIElectricity_Post3_kBtu <- EUIElectricity_Post2_kBtu

for (i in 8:ncol(EUIElectricity_Post3_kBtu)) {
  for (j in 1:nrow(EUIElectricity_Post3_kBtu)) {
    EUIElectricity_Post3_kBtu[j,i] <- min(EUIElectricity_Post3_kBtu[j,i],PassiveHouseEUI)
  }
}

EUIElectricity_Post3 <- cbind(EUIElectricity_Post3_kBtu[1:2],
                              EUIElectricity_Post3_kBtu[3:length(EUIElectricity_Post3_kBtu)]/kWhtokBtu)

# Calculate total building-level EUI

TotalEUI_Post3 <- cbind(EUIFuelOil2_Post3[1:2],EUIFuelOil2_Post3[,columns]+EUIFuelOil4_Post3[,columns]+
                          EUIFuelOil56_Post3[,columns]+EUIDistrictSteam_Post3[,columns]+
                          EUINaturalGas_Post3_kBtu[,columns]+EUIElectricity_Post3_kBtu[,columns])


# Energy Cost Calculations ------------------------------------------------

# Calculate the Post3-retrofit costs for different price scenarios

# Fuel Oil 2

FuelOil2Costs_Post3_Low <- EUIFuelOil2_Post3
FuelOil2Costs_Post3_Low[,3:length(FuelOil2Costs_Post3_Low)] <- FuelOil2Costs_Post3_Low[,3:length(FuelOil2Costs_Post3_Low)]*FuelOil2Price_Low

FuelOil2Costs_Post3_Mid <- EUIFuelOil2_Post3
FuelOil2Costs_Post3_Mid[,3:length(FuelOil2Costs_Post3_Mid)] <- FuelOil2Costs_Post3_Mid[,3:length(FuelOil2Costs_Post3_Mid)]*FuelOil2Price_Mid

FuelOil2Costs_Post3_High <- EUIFuelOil2_Post3
FuelOil2Costs_Post3_High[,3:length(FuelOil2Costs_Post3_High)] <- FuelOil2Costs_Post3_High[,3:length(FuelOil2Costs_Post3_High)]*FuelOil2Price_High


# Fuel Oil 4

FuelOil4Costs_Post3_Low <- EUIFuelOil4_Post3
FuelOil4Costs_Post3_Low[,3:length(FuelOil4Costs_Post3_Low)] <- FuelOil4Costs_Post3_Low[,3:length(FuelOil4Costs_Post3_Low)]*FuelOil4Price_Low

FuelOil4Costs_Post3_Mid <- EUIFuelOil4_Post3
FuelOil4Costs_Post3_Mid[,3:length(FuelOil4Costs_Post3_Mid)] <- FuelOil4Costs_Post3_Mid[,3:length(FuelOil4Costs_Post3_Mid)]*FuelOil4Price_Mid

FuelOil4Costs_Post3_High <- EUIFuelOil4_Post3
FuelOil4Costs_Post3_High[,3:length(FuelOil4Costs_Post3_High)] <- FuelOil4Costs_Post3_High[,3:length(FuelOil4Costs_Post3_High)]*FuelOil4Price_High


# Fuel Oil 56

FuelOil56Costs_Post3_Low <- EUIFuelOil56_Post3
FuelOil56Costs_Post3_Low[,3:length(FuelOil56Costs_Post3_Low)] <- FuelOil56Costs_Post3_Low[,3:length(FuelOil56Costs_Post3_Low)]*FuelOil56Price_Low

FuelOil56Costs_Post3_Mid <- EUIFuelOil56_Post3
FuelOil56Costs_Post3_Mid[,3:length(FuelOil56Costs_Post3_Mid)] <- FuelOil56Costs_Post3_Mid[,3:length(FuelOil56Costs_Post3_Mid)]*FuelOil56Price_Mid

FuelOil56Costs_Post3_High <- EUIFuelOil56_Post3
FuelOil56Costs_Post3_High[,3:length(FuelOil56Costs_Post3_High)] <- FuelOil56Costs_Post3_High[,3:length(FuelOil56Costs_Post3_High)]*FuelOil56Price_High


# District Steam

DistrictSteamCosts_Post3_Low <- EUIDistrictSteam_Post3
DistrictSteamCosts_Post3_Low[,3:length(DistrictSteamCosts_Post3_Low)] <- DistrictSteamCosts_Post3_Low[,3:length(DistrictSteamCosts_Post3_Low)]*DistrictSteamPrice_Low

DistrictSteamCosts_Post3_Mid <- EUIDistrictSteam_Post3
DistrictSteamCosts_Post3_Mid[,3:length(DistrictSteamCosts_Post3_Mid)] <- DistrictSteamCosts_Post3_Mid[,3:length(DistrictSteamCosts_Post3_Mid)]*DistrictSteamPrice_Mid

DistrictSteamCosts_Post3_High <- EUIDistrictSteam_Post3
DistrictSteamCosts_Post3_High[,3:length(DistrictSteamCosts_Post3_High)] <- DistrictSteamCosts_Post3_High[,3:length(DistrictSteamCosts_Post3_High)]*DistrictSteamPrice_High


# Natural Gas

NaturalGasCosts_Post3_Low <- EUINaturalGas_Post3
NaturalGasCosts_Post3_Low[,3:length(NaturalGasCosts_Post3_Low)] <- NaturalGasCosts_Post3_Low[,3:length(NaturalGasCosts_Post3_Low)]*NaturalGasPrice_Low

NaturalGasCosts_Post3_Mid <- EUINaturalGas_Post3
NaturalGasCosts_Post3_Mid[,3:length(NaturalGasCosts_Post3_Mid)] <- NaturalGasCosts_Post3_Mid[,3:length(NaturalGasCosts_Post3_Mid)]*NaturalGasPrice_Mid

NaturalGasCosts_Post3_High <- EUINaturalGas_Post3
NaturalGasCosts_Post3_High[,3:length(NaturalGasCosts_Post3_High)] <- NaturalGasCosts_Post3_High[,3:length(NaturalGasCosts_Post3_High)]*NaturalGasPrice_High


# Electricity

ElectricityCosts_Post3_Low <- EUIElectricity_Post3
ElectricityCosts_Post3_Low[,3:length(ElectricityCosts_Post3_Low)] <- ElectricityCosts_Post3_Low[,3:length(ElectricityCosts_Post3_Low)]*ElectricityPrice_Low

ElectricityCosts_Post3_Mid <- EUIElectricity_Post3
ElectricityCosts_Post3_Mid[,3:length(ElectricityCosts_Post3_Mid)] <- ElectricityCosts_Post3_Mid[,3:length(ElectricityCosts_Post3_Mid)]*ElectricityPrice_Mid

ElectricityCosts_Post3_High <- EUIElectricity_Post3
ElectricityCosts_Post3_High[,3:length(ElectricityCosts_Post3_High)] <- ElectricityCosts_Post3_High[,3:length(ElectricityCosts_Post3_High)]*ElectricityPrice_High


# Add all costs together
# UNITS: $/sqft/yr
# help from: https://stackoverflow.com/questions/54078513/how-to-sum-same-column-of-different-data-frames-in-r


# Low, mid, high scenarios
Post3EnergyCostsSqft_Low <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

Post3EnergyCostsSqft_Low <- FuelOil2Costs_Post3_Low[, columns] + FuelOil4Costs_Post3_Low[, columns] + FuelOil56Costs_Post3_Low[, columns] +
  DistrictSteamCosts_Post3_Low[, columns] + NaturalGasCosts_Post3_Low[, columns] + ElectricityCosts_Post3_Low[, columns]

Post3EnergyCostsSqft_Low <- Post3EnergyCostsSqft_Low %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

Post3EnergyCostsApt_Low <- Post3EnergyCostsSqft_Low %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft))

Post3EnergyCostsSqft_Mid <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

Post3EnergyCostsSqft_Mid <- FuelOil2Costs_Post3_Mid[, columns] + FuelOil4Costs_Post3_Mid[, columns] + FuelOil56Costs_Post3_Mid[, columns] +
  DistrictSteamCosts_Post3_Mid[, columns] + NaturalGasCosts_Post3_Mid[, columns] + ElectricityCosts_Post3_Mid[, columns]

Post3EnergyCostsSqft_Mid <- Post3EnergyCostsSqft_Mid %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

Post3EnergyCostsApt_Mid <- Post3EnergyCostsSqft_Mid %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft))

Post3EnergyCostsSqft_High <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

Post3EnergyCostsSqft_High <- FuelOil2Costs_Post3_High[, columns] + FuelOil4Costs_Post3_High[, columns] + FuelOil56Costs_Post3_High[, columns] +
  DistrictSteamCosts_Post3_High[, columns] + NaturalGasCosts_Post3_High[, columns] + ElectricityCosts_Post3_High[, columns]

Post3EnergyCostsSqft_High <- Post3EnergyCostsSqft_High %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

Post3EnergyCostsApt_High <- Post3EnergyCostsSqft_High %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft))

# Add columns for total, undiscounted costs, 2024-2050
Post3EnergyCostsSqft_Low <- Post3EnergyCostsSqft_Low %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

Post3EnergyCostsSqft_Mid <- Post3EnergyCostsSqft_Mid %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

Post3EnergyCostsSqft_High <- Post3EnergyCostsSqft_High %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))


# Scenario 3 energy cost savings
Delta3EnergyCostsSqft_Low <- cbind(Post3EnergyCostsSqft_Low[1:2],BAUEnergyCostsSqft_Low[3:length(BAUEnergyCostsSqft_Low)]-
                                     Post3EnergyCostsSqft_Low[3:length(Post3EnergyCostsSqft_Low)])

Delta3EnergyCostsSqft_Mid <- cbind(Post3EnergyCostsSqft_Mid[1:2],BAUEnergyCostsSqft_Mid[3:length(BAUEnergyCostsSqft_Mid)]-
                                     Post3EnergyCostsSqft_Mid[3:length(Post3EnergyCostsSqft_Mid)])

Delta3EnergyCostsSqft_High <- cbind(Post3EnergyCostsSqft_High[1:2],BAUEnergyCostsSqft_High[3:length(BAUEnergyCostsSqft_High)]-
                                      Post3EnergyCostsSqft_High[3:length(Post3EnergyCostsSqft_High)])


# Penalty Calculations ----------------------------------------------------

# Fuel Oil 2
FuelOil2Emissions_Post3 <- mapply("*", EUIFuelOil2_Post3[intersect(names(EUIFuelOil2_Post3), names(FuelOil2EF))],
                                  FuelOil2EF[intersect(names(EUIFuelOil2_Post3), names(FuelOil2EF))]) %>% 
  as.data.frame()

FuelOil2Emissions_Post3 <- FuelOil2Emissions_Post3 %>% 
  mutate("HousingType" = EUIFuelOil2_Post3$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil2_Post3$BBL) %>% 
  relocate(BBL)

# Fuel Oil 4
FuelOil4Emissions_Post3 <- mapply("*", EUIFuelOil4_Post3[intersect(names(EUIFuelOil4_Post3), names(FuelOil4EF))],
                                  FuelOil4EF[intersect(names(EUIFuelOil4_Post3), names(FuelOil4EF))]) %>% 
  as.data.frame()

FuelOil4Emissions_Post3 <- FuelOil4Emissions_Post3 %>% 
  mutate("HousingType" = EUIFuelOil4_Post3$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil4_Post3$BBL) %>% 
  relocate(BBL)

# Fuel Oil 56
FuelOil56Emissions_Post3 <- mapply("*", EUIFuelOil56_Post3[intersect(names(EUIFuelOil56_Post3), names(FuelOil56EF))],
                                   FuelOil56EF[intersect(names(EUIFuelOil56_Post3), names(FuelOil56EF))]) %>% 
  as.data.frame()

FuelOil56Emissions_Post3 <- FuelOil56Emissions_Post3 %>% 
  mutate("HousingType" = EUIFuelOil56_Post3$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil56_Post3$BBL) %>% 
  relocate(BBL)

# District Steam
DistrictSteamEmissions_Post3 <- mapply("*", EUIDistrictSteam_Post3[intersect(names(EUIDistrictSteam_Post3), names(DistrictSteamEF))],
                                       DistrictSteamEF[intersect(names(EUIDistrictSteam_Post3), names(DistrictSteamEF))]) %>% 
  as.data.frame()

DistrictSteamEmissions_Post3 <- DistrictSteamEmissions_Post3 %>% 
  mutate("HousingType" = EUIDistrictSteam_Post3$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIDistrictSteam_Post3$BBL) %>% 
  relocate(BBL)

# Natural Gas
NaturalGasEmissions_Post3 <- mapply("*", EUINaturalGas_Post3[intersect(names(EUINaturalGas_Post3), names(NaturalGasEF))],
                                    NaturalGasEF[intersect(names(EUINaturalGas_Post3), names(NaturalGasEF))]) %>% 
  as.data.frame()

NaturalGasEmissions_Post3 <- NaturalGasEmissions_Post3 %>% 
  mutate("HousingType" = EUINaturalGas_Post3$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUINaturalGas_Post3$BBL) %>% 
  relocate(BBL)

# Electricity
ElectricityEmissions_Post3 <- mapply("*", EUIElectricity_Post3[intersect(names(EUIElectricity_Post3), names(ElectricityEF))],
                                     ElectricityEF[intersect(names(EUIElectricity_Post3), names(ElectricityEF))]) %>% 
  as.data.frame()

ElectricityEmissions_Post3 <- ElectricityEmissions_Post3 %>% 
  mutate("HousingType" = EUIElectricity_Post3$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIElectricity_Post3$BBL) %>% 
  relocate(BBL)


# Add all emissions together
# UNITS: MTCO2/sqft/yr


Post3EmissionsSqft <- cbind(FuelOil2Emissions_Post3[1:2], 
                            FuelOil2Emissions_Post3[, columns] + FuelOil4Emissions_Post3[, columns] + 
                              FuelOil56Emissions_Post3[, columns] + DistrictSteamEmissions_Post3[, columns] + 
                              NaturalGasEmissions_Post3[, columns] + ElectricityEmissions_Post3[, columns])

# Calculate difference between Post 3 emissions intensity and LL97 limits

# subtract LL97 limits from BAU emissions
Post3EmissionsOverLimit <- mapply("-", Post3EmissionsSqft[intersect(names(Post3EmissionsSqft), names(LL97limits))],
                                  LL97limits[intersect(names(Post3EmissionsSqft), names(LL97limits))]) %>% 
  as.data.frame()

# replace negatives with zero
Post3EmissionsOverLimit[Post3EmissionsOverLimit<0] <- 0

Post3EmissionsOverLimit <- Post3EmissionsOverLimit %>% 
  mutate("HousingType" = Post3EmissionsSqft$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = Post3EmissionsSqft$BBL) %>% 
  relocate(BBL)

# Calculate penalties for each building 2019-2050

Post3LL97PenaltiesBldg <- Post3EmissionsOverLimit %>% 
  left_join(BuildingSize, by = "BBL") %>% 
  rename(BldgArea = `Multifamily Housing - Gross Floor Area (ft²)`) %>% 
  relocate(c("BBL", "HousingType", "BldgArea", "Apartments"))

Post3LL97PenaltiesBldg <- Post3LL97PenaltiesBldg %>% 
  mutate(across(starts_with("2"),~.*BldgArea*LL97penalty/1000))

Post3LL97PenaltiesBldg <- Post3LL97PenaltiesBldg %>% 
  mutate("TotalPenalties_2024to2050" = rowSums(across(`2019`:`2050`), na.rm = TRUE))

# Calculate penalties per sqft

Post3LL97PenaltiesSqft <- Post3LL97PenaltiesBldg %>% 
  mutate(across(starts_with("2"),~./BldgArea))

# add column for undiscounted penalty total (2019-2050)
Post3LL97PenaltiesSqft <- Post3LL97PenaltiesSqft %>% 
  mutate("UndiscountedPenalties" = rowSums(across(starts_with("2")), na.rm = TRUE)) 

# Calculate avoided penalties

Post3AvoidedPenaltiesSqft <- cbind(Post3LL97PenaltiesSqft[1:4],BAULL97PenaltiesSqft[5:(length(BAULL97PenaltiesSqft)-2)]-
                                     Post3LL97PenaltiesSqft[5:(length(Post3LL97PenaltiesSqft)-2)])

Post3AvoidedPenaltiesSqft[Post3AvoidedPenaltiesSqft < 0] <- 0


# Total Cost Savings Calculation ------------------------------------------

# Combine avoided penalties and avoided energy costs
# Use Mid price scenario
Post3CostSavingsSqft <- as.data.frame(Post3AvoidedPenaltiesSqft$AvoidedPenalties + Delta3EnergyCostsSqft_Mid$TotalCosts_2024to2050) %>% 
  rename("TotalCostSavings" = "Post3AvoidedPenaltiesSqft$AvoidedPenalties + Delta3EnergyCostsSqft_Mid$TotalCosts_2024to2050")

# NPV Calculations --------------------------------------------------------

# Energy Cost Savings NPVs

Delta3EnergyCostsSqft_Low_NPV <- cbind(Delta3EnergyCostsSqft_Low[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

Delta3EnergyCostsSqft_Mid_NPV <- cbind(Delta3EnergyCostsSqft_Mid[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

Delta3EnergyCostsSqft_High_NPV <- cbind(Delta3EnergyCostsSqft_High[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")


for (i in 1:length(dr)) {
  for (t in 1:nrow(Delta3EnergyCostsSqft_Low)) {
    
    Delta3EnergyCostsSqft_Low_NPV[t,i+2] <- NPV(cf0 = Delta3EnergyCostsSqft_Low[t,3],
                                                cf = as.numeric(Delta3EnergyCostsSqft_Low[t,3:(length(Delta3EnergyCostsSqft_Low)-1)],
                                                                na.rm = TRUE),
                                                times = timeperiod,
                                                i = dr[i])
    
    
    Delta3EnergyCostsSqft_Mid_NPV[t,i+2] <- NPV(cf0 = Delta3EnergyCostsSqft_Mid[t,3],
                                                cf = as.numeric(Delta3EnergyCostsSqft_Mid[t,3:(length(Delta3EnergyCostsSqft_Mid)-1)]),
                                                times = timeperiod,
                                                i = dr[i])
    
    Delta3EnergyCostsSqft_High_NPV[t,i+2] <- NPV(cf0 = Delta3EnergyCostsSqft_High[t,3],
                                                 cf = as.numeric(Delta3EnergyCostsSqft_High[t,3:(length(Delta3EnergyCostsSqft_High)-1)]),
                                                 times = timeperiod,
                                                 i = dr[i])
  }
}

# Create new dataframes for costs by apartment
Delta3EnergyCostsApt_Low <- Delta3EnergyCostsSqft_Low %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("T"),~.*AptSize$AptSqft))

Delta3EnergyCostsApt_Mid <- Delta3EnergyCostsSqft_Mid %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("T"),~.*AptSize$AptSqft))

Delta3EnergyCostsApt_High <- Delta3EnergyCostsSqft_High %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("T"),~.*AptSize$AptSqft))

Delta3EnergyCostsApt_Low_NPV <- Delta3EnergyCostsSqft_Low_NPV %>% 
  mutate(across(starts_with("NPV"),~.*AptSize$AptSqft))

Delta3EnergyCostsApt_Mid_NPV <- Delta3EnergyCostsSqft_Mid_NPV %>% 
  mutate(across(starts_with("NPV"),~.*AptSize$AptSqft))

Delta3EnergyCostsApt_High_NPV <- Delta3EnergyCostsSqft_High_NPV %>% 
  mutate(across(starts_with("NPV"),~.*AptSize$AptSqft))


# Zero Discount Rate
Delta3EnergyCostsApt_Low_NPV_DRzero_Mean <- mean(Delta3EnergyCostsApt_Low$TotalCosts_2024to2050)
Delta3EnergyCostsApt_Mid_NPV_DRzero_Mean <- mean(Delta3EnergyCostsApt_Mid$TotalCosts_2024to2050)
Delta3EnergyCostsApt_High_NPV_DRzero_Mean <- mean(Delta3EnergyCostsApt_High$TotalCosts_2024to2050)

Delta3EnergyCostsApt_Low_NPV_DRzero_StDev <- sd(Delta3EnergyCostsApt_Low$TotalCosts_2024to2050)
Delta3EnergyCostsApt_Mid_NPV_DRzero_StDev <- sd(Delta3EnergyCostsApt_Mid$TotalCosts_2024to2050)
Delta3EnergyCostsApt_High_NPV_DRzero_StDev <- sd(Delta3EnergyCostsApt_High$TotalCosts_2024to2050)

Delta3EnergyCostsApt_Low_NPV_DRzero_PctNeg <- sum(Delta3EnergyCostsApt_Low$TotalCosts_2024to2050 < 0)/nrow(Delta3EnergyCostsApt_Low)*100
Delta3EnergyCostsApt_Mid_NPV_DRzero_PctNeg <- sum(Delta3EnergyCostsApt_Mid$TotalCosts_2024to2050 < 0)/nrow(Delta3EnergyCostsApt_Mid)*100
Delta3EnergyCostsApt_High_NPV_DRzero_PctNeg <- sum(Delta3EnergyCostsApt_High$TotalCosts_2024to2050 < 0)/nrow(Delta3EnergyCostsApt_High)*100

# Low Discount Rate
Delta3EnergyCostsApt_Low_NPV_DRlow_Median <- median(Delta3EnergyCostsApt_Low_NPV$NPV_DRlow)
Delta3EnergyCostsApt_Mid_NPV_DRlow_Median <- median(Delta3EnergyCostsApt_Mid_NPV$NPV_DRlow)
Delta3EnergyCostsApt_High_NPV_DRlow_Median <- median(Delta3EnergyCostsApt_High_NPV$NPV_DRlow)

Delta3EnergyCostsApt_Low_NPV_DRlow_Mean <- mean(Delta3EnergyCostsApt_Low_NPV$NPV_DRlow)
Delta3EnergyCostsApt_Mid_NPV_DRlow_Mean <- mean(Delta3EnergyCostsApt_Mid_NPV$NPV_DRlow)
Delta3EnergyCostsApt_High_NPV_DRlow_Mean <- mean(Delta3EnergyCostsApt_High_NPV$NPV_DRlow)

Delta3EnergyCostsApt_Low_NPV_DRlow_StDev <- sd(Delta3EnergyCostsApt_Low_NPV$NPV_DRlow)
Delta3EnergyCostsApt_Mid_NPV_DRlow_StDev <- sd(Delta3EnergyCostsApt_Mid_NPV$NPV_DRlow)
Delta3EnergyCostsApt_High_NPV_DRlow_StDev <- sd(Delta3EnergyCostsApt_High_NPV$NPV_DRlow)

Delta3EnergyCostsApt_Low_NPV_DRlow_PctNeg <- sum(Delta3EnergyCostsApt_Low_NPV$NPV_DRlow < 0)/nrow(Delta3EnergyCostsApt_Low_NPV)*100
Delta3EnergyCostsApt_Mid_NPV_DRlow_PctNeg <- sum(Delta3EnergyCostsApt_Mid_NPV$NPV_DRlow < 0)/nrow(Delta3EnergyCostsApt_Mid_NPV)*100
Delta3EnergyCostsApt_High_NPV_DRlow_PctNeg <- sum(Delta3EnergyCostsApt_High_NPV$NPV_DRlow < 0)/nrow(Delta3EnergyCostsApt_High_NPV)*100

# Mid Discount Rate
Delta3EnergyCostsApt_Low_NPV_DRmid_Median <- median(Delta3EnergyCostsApt_Low_NPV$NPV_DRmid)
Delta3EnergyCostsApt_Mid_NPV_DRmid_Median <- median(Delta3EnergyCostsApt_Mid_NPV$NPV_DRmid)
Delta3EnergyCostsApt_High_NPV_DRmid_Median <- median(Delta3EnergyCostsApt_High_NPV$NPV_DRmid)

Delta3EnergyCostsApt_Low_NPV_DRmid_Mean <- mean(Delta3EnergyCostsApt_Low_NPV$NPV_DRmid)
Delta3EnergyCostsApt_Mid_NPV_DRmid_Mean <- mean(Delta3EnergyCostsApt_Mid_NPV$NPV_DRmid)
Delta3EnergyCostsApt_High_NPV_DRmid_Mean <- mean(Delta3EnergyCostsApt_High_NPV$NPV_DRmid)

Delta3EnergyCostsApt_Low_NPV_DRmid_StDev <- sd(Delta3EnergyCostsApt_Low_NPV$NPV_DRmid)
Delta3EnergyCostsApt_Mid_NPV_DRmid_StDev <- sd(Delta3EnergyCostsApt_Mid_NPV$NPV_DRmid)
Delta3EnergyCostsApt_High_NPV_DRmid_StDev <- sd(Delta3EnergyCostsApt_High_NPV$NPV_DRmid)

Delta3EnergyCostsApt_Low_NPV_DRmid_PctNeg <- sum(Delta3EnergyCostsApt_Low_NPV$NPV_DRmid < 0)/nrow(Delta3EnergyCostsApt_Low_NPV)*100
Delta3EnergyCostsApt_Mid_NPV_DRmid_PctNeg <- sum(Delta3EnergyCostsApt_Mid_NPV$NPV_DRmid < 0)/nrow(Delta3EnergyCostsApt_Mid_NPV)*100
Delta3EnergyCostsApt_High_NPV_DRmid_PctNeg <- sum(Delta3EnergyCostsApt_High_NPV$NPV_DRmid < 0)/nrow(Delta3EnergyCostsApt_High_NPV)*100


# High Discount Rate
Delta3EnergyCostsApt_Low_NPV_DRhigh_Median <- median(Delta3EnergyCostsApt_Low_NPV$NPV_DRhigh)
Delta3EnergyCostsApt_Mid_NPV_DRhigh_Median <- median(Delta3EnergyCostsApt_Mid_NPV$NPV_DRhigh)
Delta3EnergyCostsApt_High_NPV_DRhigh_Median <- median(Delta3EnergyCostsApt_High_NPV$NPV_DRhigh)

Delta3EnergyCostsApt_Low_NPV_DRhigh_Mean <- mean(Delta3EnergyCostsApt_Low_NPV$NPV_DRhigh)
Delta3EnergyCostsApt_Mid_NPV_DRhigh_Mean <- mean(Delta3EnergyCostsApt_Mid_NPV$NPV_DRhigh)
Delta3EnergyCostsApt_High_NPV_DRhigh_Mean <- mean(Delta3EnergyCostsApt_High_NPV$NPV_DRhigh)

Delta3EnergyCostsApt_Low_NPV_DRhigh_StDev <- sd(Delta3EnergyCostsApt_Low_NPV$NPV_DRhigh)
Delta3EnergyCostsApt_Mid_NPV_DRhigh_StDev <- sd(Delta3EnergyCostsApt_Mid_NPV$NPV_DRhigh)
Delta3EnergyCostsApt_High_NPV_DRhigh_StDev <- sd(Delta3EnergyCostsApt_High_NPV$NPV_DRhigh)

Delta3EnergyCostsApt_Low_NPV_DRhigh_PctNeg <- sum(Delta3EnergyCostsApt_Low_NPV$NPV_DRhigh < 0)/nrow(Delta3EnergyCostsApt_Low_NPV)*100
Delta3EnergyCostsApt_Mid_NPV_DRhigh_PctNeg <- sum(Delta3EnergyCostsApt_Mid_NPV$NPV_DRhigh < 0)/nrow(Delta3EnergyCostsApt_Mid_NPV)*100
Delta3EnergyCostsApt_High_NPV_DRhigh_PctNeg <- sum(Delta3EnergyCostsApt_High_NPV$NPV_DRhigh < 0)/nrow(Delta3EnergyCostsApt_High_NPV)*100


# Avoided Penalties NPVs

# Per SQFT

Post3AvoidedPenaltiesSqft_NPV <- cbind(Post3AvoidedPenaltiesSqft[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

for (i in 1:length(dr)) {
  for (t in 1:nrow(Post3AvoidedPenaltiesSqft_NPV)) {
    
    Post3AvoidedPenaltiesSqft_NPV[t,i+2] <- NPV(cf0 = Post3AvoidedPenaltiesSqft[t,5],
                                                cf = as.numeric(Post3AvoidedPenaltiesSqft[t,5:length(Post3AvoidedPenaltiesSqft)],
                                                                na.rm = TRUE),
                                                times = timeperiod,
                                                i = dr[i])
  }
}

Post3AvoidedPenaltiesSqft_NPV_DRlow_Mean <- mean(Post3AvoidedPenaltiesSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post3AvoidedPenaltiesSqft_NPV_DRlow_StDev <- sd(Post3AvoidedPenaltiesSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post3AvoidedPenaltiesSqft_NPV_DRmid_Mean <- mean(Post3AvoidedPenaltiesSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post3AvoidedPenaltiesSqft_NPV_DRmid_StDev <- sd(Post3AvoidedPenaltiesSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post3AvoidedPenaltiesSqft_NPV_DRhigh_Mean <- mean(Post3AvoidedPenaltiesSqft_NPV$NPV_DRhigh, na.rm = TRUE)
Post3AvoidedPenaltiesSqft_NPV_DRhigh_StDev <- sd(Post3AvoidedPenaltiesSqft_NPV$NPV_DRhigh, na.rm = TRUE)

# Add total undiscounted column
Post3AvoidedPenaltiesSqft <- Post3AvoidedPenaltiesSqft %>% 
  mutate("UndiscountedTotal" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

# PER APT

# Create new dataframes for avoided penalties by apartment
Post3AvoidedPenaltiesApt <- Post3AvoidedPenaltiesSqft %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("U"),~.*AptSize$AptSqft))

Post3AvoidedPenaltiesApt_NPV <- Post3AvoidedPenaltiesSqft_NPV %>% 
  mutate(across(starts_with("N"),~.*AptSize$AptSqft))

Post3AvoidedPenaltiesApt_NPV_DRlow_Mean <- mean(Post3AvoidedPenaltiesApt_NPV$NPV_DRlow, na.rm = TRUE)
Post3AvoidedPenaltiesApt_NPV_DRlow_StDev <- sd(Post3AvoidedPenaltiesApt_NPV$NPV_DRlow, na.rm = TRUE)
Post3AvoidedPenaltiesApt_NPV_DRmid_Mean <- mean(Post3AvoidedPenaltiesApt_NPV$NPV_DRmid, na.rm = TRUE)
Post3AvoidedPenaltiesApt_NPV_DRmid_StDev <- sd(Post3AvoidedPenaltiesApt_NPV$NPV_DRmid, na.rm = TRUE)
Post3AvoidedPenaltiesApt_NPV_DRhigh_Mean <- mean(Post3AvoidedPenaltiesApt_NPV$NPV_DRhigh, na.rm = TRUE)
Post3AvoidedPenaltiesApt_NPV_DRhigh_StDev <- sd(Post3AvoidedPenaltiesApt_NPV$NPV_DRhigh, na.rm = TRUE)


# Calculate Mean, St Dev for undiscounted avoided penalties
Post3AvoidedPenaltiesApt_NPV_DRzero_Mean <- mean(Post3AvoidedPenaltiesApt$UndiscountedTotal, na.rm = TRUE)
Post3AvoidedPenaltiesApt_NPV_DRzero_StDev <- sd(Post3AvoidedPenaltiesApt$UndiscountedTotal, na.rm = TRUE)

# Total Cost Savings

# PER SQFT

Post3CostSavingsSqft <- cbind(Post3AvoidedPenaltiesSqft[1:4],
                              Delta3EnergyCostsSqft_Mid[3:(length(Delta3EnergyCostsSqft_Mid)-1)] + 
                                Post3AvoidedPenaltiesSqft[5:(length(Post3AvoidedPenaltiesSqft)-1)])

Post3CostSavingsSqft <- Post3CostSavingsSqft %>% 
  mutate("UndiscountedTotal" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

# Calculate NPV
Post3CostSavingsSqft_NPV <- cbind(Post3CostSavingsSqft[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

for (i in 1:length(dr)) {
  for (t in 1:nrow(Post3CostSavingsSqft_NPV)) {
    
    Post3CostSavingsSqft_NPV[t,i+2] <- NPV(cf0 = Post3CostSavingsSqft[t,5],
                                           cf = as.numeric(Post3CostSavingsSqft[t,5:(length(Post3CostSavingsSqft)-1)],
                                                           na.rm = TRUE),
                                           times = timeperiod,
                                           i = dr[i])
  }
}

Post3CostSavingsSqft_NPV_DRlow_Mean <- mean(Post3CostSavingsSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post3CostSavingsSqft_NPV_DRlow_StDev <- sd(Post3CostSavingsSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post3CostSavingsSqft_NPV_DRlow_PctNeg <- sum(Post3CostSavingsSqft_NPV$NPV_DRlow < 0)/nrow(Post3CostSavingsSqft_NPV)*100
Post3CostSavingsSqft_NPV_DRmid_Mean <- mean(Post3CostSavingsSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post3CostSavingsSqft_NPV_DRmid_StDev <- sd(Post3CostSavingsSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post3CostSavingsSqft_NPV_DRmid_PctNeg <- sum(Post3CostSavingsSqft_NPV$NPV_DRmid < 0)/nrow(Post3CostSavingsSqft_NPV)*100
Post3CostSavingsSqft_NPV_DRhigh_Mean <- mean(Post3CostSavingsSqft_NPV$NPV_DRhigh, na.rm = TRUE)
Post3CostSavingsSqft_NPV_DRhigh_StDev <- sd(Post3CostSavingsSqft_NPV$NPV_DRhigh, na.rm = TRUE)
Post3CostSavingsSqft_NPV_DRhigh_PctNeg <- sum(Post3CostSavingsSqft_NPV$NPV_DRhigh < 0)/nrow(Post3CostSavingsSqft_NPV)*100

# PER APT

# Create new NPV dataframes
Post3CostSavingsApt <- Post3CostSavingsSqft %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("U"),~.*AptSize$AptSqft))

Post3CostSavingsApt_NPV <- Post3CostSavingsSqft_NPV %>% 
  mutate(across(starts_with("N"),~.*AptSize$AptSqft))

# Calculate Mean, SD, % <0
Post3CostSavingsApt_NPV_DRzero_Mean <- mean(Post3CostSavingsApt$UndiscountedTotal, na.rm = TRUE)
Post3CostSavingsApt_NPV_DRzero_StDev <- sd(Post3CostSavingsApt$UndiscountedTotal, na.rm = TRUE)
Post3CostSavingsApt_NPV_DRzero_PctNeg <- sum(Post3CostSavingsApt$UndiscountedTotal < 0)/nrow(Post3CostSavingsApt)*100
Post3CostSavingsApt_NPV_DRlow_Mean <- mean(Post3CostSavingsApt_NPV$NPV_DRlow, na.rm = TRUE)
Post3CostSavingsApt_NPV_DRlow_StDev <- sd(Post3CostSavingsApt_NPV$NPV_DRlow, na.rm = TRUE)
Post3CostSavingsApt_NPV_DRlow_PctNeg <- sum(Post3CostSavingsApt_NPV$NPV_DRlow < 0)/nrow(Post3CostSavingsApt_NPV)*100
Post3CostSavingsApt_NPV_DRmid_Mean <- mean(Post3CostSavingsApt_NPV$NPV_DRmid, na.rm = TRUE)
Post3CostSavingsApt_NPV_DRmid_StDev <- sd(Post3CostSavingsApt_NPV$NPV_DRmid, na.rm = TRUE)
Post3CostSavingsApt_NPV_DRmid_PctNeg <- sum(Post3CostSavingsApt_NPV$NPV_DRmid < 0)/nrow(Post3CostSavingsApt_NPV)*100
Post3CostSavingsApt_NPV_DRhigh_Mean <- mean(Post3CostSavingsApt_NPV$NPV_DRhigh, na.rm = TRUE)
Post3CostSavingsApt_NPV_DRhigh_StDev <- sd(Post3CostSavingsApt_NPV$NPV_DRhigh, na.rm = TRUE)
Post3CostSavingsApt_NPV_DRhigh_PctNeg <- sum(Post3CostSavingsApt_NPV$NPV_DRhigh < 0)/nrow(Post3CostSavingsApt_NPV)*100



