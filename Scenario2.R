# Scenario 2
# Script 8/10
# Run this script to model Scenario 2 - Fully Electric in 2024 - by
# calculating the EUI, energy costs, emissions, and penalties for all buildings through 2050,
# and comparing these to the BAU to calculate the energy cost savings, avoided penalties, and total cost savings.
# This script also calculates the Net Present Value (NPV) of each of these metrics
# under 3 discount rate scenarios (2%, 5%, 8%)

# EUI Calculations --------------------------------------------------------

# Calculate the total EUI, BAU
TotalEUI_BAU <- cbind(EUIbySource_BAU[1:2],EUIFuelOil2_BAU[3:length(EUIFuelOil2_BAU)]+
                        EUIFuelOil4_BAU[3:length(EUIFuelOil4_BAU)]+
                        EUIFuelOil56_BAU[3:length(EUIFuelOil56_BAU)]+
                        EUIDistrictSteam_BAU[3:length(EUIDistrictSteam_BAU)]+
                        EUINaturalGas_BAU[3:length(EUINaturalGas_BAU)]*thermstokBtu+
                        EUIElectricity_BAU[3:length(EUIElectricity_BAU)]*kWhtokBtu)


# In 2024, all EUI is shifted to electricity

EUIElectricity_Post2_kBtu <- cbind(TotalEUI_BAU[1:2], EUIElectricity_BAU[c("2019","2020","2021","2022","2023")]*kWhtokBtu,
                                   TotalEUI_BAU[c("2024","2025","2026","2027","2028","2029","2030",
                                                  "2031","2032","2033","2034","2035","2036","2037",
                                                  "2038","2039","2040","2041","2042","2043","2044",
                                                  "2045","2046","2047","2048","2049","2050")])
EUIElectricity_Post2 <- cbind(EUIElectricity_Post2_kBtu[1:2],
                              EUIElectricity_Post2_kBtu[3:length(EUIElectricity_Post2_kBtu)]/kWhtokBtu)

EUIFuelOil2_Post2 <- EUIFuelOil2_BAU 
EUIFuelOil2_Post2[c("2024","2025","2026","2027","2028","2029","2030",
                    "2031","2032","2033","2034","2035","2036","2037",
                    "2038","2039","2040","2041","2042","2043","2044",
                    "2045","2046","2047","2048","2049","2050")] <- 0

EUIFuelOil4_Post2 <- EUIFuelOil4_BAU 
EUIFuelOil4_Post2[c("2024","2025","2026","2027","2028","2029","2030",
                    "2031","2032","2033","2034","2035","2036","2037",
                    "2038","2039","2040","2041","2042","2043","2044",
                    "2045","2046","2047","2048","2049","2050")] <- 0

EUIFuelOil56_Post2 <- EUIFuelOil56_BAU 
EUIFuelOil56_Post2[c("2024","2025","2026","2027","2028","2029","2030",
                     "2031","2032","2033","2034","2035","2036","2037",
                     "2038","2039","2040","2041","2042","2043","2044",
                     "2045","2046","2047","2048","2049","2050")] <- 0

EUIDistrictSteam_Post2 <- EUIDistrictSteam_BAU 
EUIDistrictSteam_Post2[c("2024","2025","2026","2027","2028","2029","2030",
                         "2031","2032","2033","2034","2035","2036","2037",
                         "2038","2039","2040","2041","2042","2043","2044",
                         "2045","2046","2047","2048","2049","2050")] <- 0

EUINaturalGas_Post2  <- EUINaturalGas_BAU 
EUINaturalGas_Post2[c("2024","2025","2026","2027","2028","2029","2030",
                      "2031","2032","2033","2034","2035","2036","2037",
                      "2038","2039","2040","2041","2042","2043","2044",
                      "2045","2046","2047","2048","2049","2050")] <- 0

EUINaturalGas_Post2_kBtu <- cbind(EUINaturalGas_Post2[1:2],
                                  EUINaturalGas_Post2[3:length(EUINaturalGas_Post2)]*thermstokBtu)

# Calculate total building-level EUI

TotalEUI_Post2 <- cbind(EUIFuelOil2_Post2[1:2],EUIFuelOil2_Post2[,columns]+EUIFuelOil4_Post2[,columns]+
                          EUIFuelOil56_Post2[,columns]+EUIDistrictSteam_Post2[,columns]+
                          EUINaturalGas_Post2_kBtu[,columns]+EUIElectricity_Post2_kBtu[,columns])


# Energy Cost Calculations ------------------------------------------------

# Calculate the Post1-retrofit costs for different price scenarios

# Fuel Oil 2

FuelOil2Costs_Post2_Low <- EUIFuelOil2_Post2
FuelOil2Costs_Post2_Low[,3:length(FuelOil2Costs_Post2_Low)] <- FuelOil2Costs_Post2_Low[,3:length(FuelOil2Costs_Post2_Low)]*FuelOil2Price_Low

FuelOil2Costs_Post2_Mid <- EUIFuelOil2_Post2
FuelOil2Costs_Post2_Mid[,3:length(FuelOil2Costs_Post2_Mid)] <- FuelOil2Costs_Post2_Mid[,3:length(FuelOil2Costs_Post2_Mid)]*FuelOil2Price_Mid

FuelOil2Costs_Post2_High <- EUIFuelOil2_Post2
FuelOil2Costs_Post2_High[,3:length(FuelOil2Costs_Post2_High)] <- FuelOil2Costs_Post2_High[,3:length(FuelOil2Costs_Post2_High)]*FuelOil2Price_High


# Fuel Oil 4

FuelOil4Costs_Post2_Low <- EUIFuelOil4_Post2
FuelOil4Costs_Post2_Low[,3:length(FuelOil4Costs_Post2_Low)] <- FuelOil4Costs_Post2_Low[,3:length(FuelOil4Costs_Post2_Low)]*FuelOil4Price_Low

FuelOil4Costs_Post2_Mid <- EUIFuelOil4_Post2
FuelOil4Costs_Post2_Mid[,3:length(FuelOil4Costs_Post2_Mid)] <- FuelOil4Costs_Post2_Mid[,3:length(FuelOil4Costs_Post2_Mid)]*FuelOil4Price_Mid

FuelOil4Costs_Post2_High <- EUIFuelOil4_Post2
FuelOil4Costs_Post2_High[,3:length(FuelOil4Costs_Post2_High)] <- FuelOil4Costs_Post2_High[,3:length(FuelOil4Costs_Post2_High)]*FuelOil4Price_High


# Fuel Oil 56

FuelOil56Costs_Post2_Low <- EUIFuelOil56_Post2
FuelOil56Costs_Post2_Low[,3:length(FuelOil56Costs_Post2_Low)] <- FuelOil56Costs_Post2_Low[,3:length(FuelOil56Costs_Post2_Low)]*FuelOil56Price_Low

FuelOil56Costs_Post2_Mid <- EUIFuelOil56_Post2
FuelOil56Costs_Post2_Mid[,3:length(FuelOil56Costs_Post2_Mid)] <- FuelOil56Costs_Post2_Mid[,3:length(FuelOil56Costs_Post2_Mid)]*FuelOil56Price_Mid

FuelOil56Costs_Post2_High <- EUIFuelOil56_Post2
FuelOil56Costs_Post2_High[,3:length(FuelOil56Costs_Post2_High)] <- FuelOil56Costs_Post2_High[,3:length(FuelOil56Costs_Post2_High)]*FuelOil56Price_High


# District Steam

DistrictSteamCosts_Post2_Low <- EUIDistrictSteam_Post2
DistrictSteamCosts_Post2_Low[,3:length(DistrictSteamCosts_Post2_Low)] <- DistrictSteamCosts_Post2_Low[,3:length(DistrictSteamCosts_Post2_Low)]*DistrictSteamPrice_Low

DistrictSteamCosts_Post2_Mid <- EUIDistrictSteam_Post2
DistrictSteamCosts_Post2_Mid[,3:length(DistrictSteamCosts_Post2_Mid)] <- DistrictSteamCosts_Post2_Mid[,3:length(DistrictSteamCosts_Post2_Mid)]*DistrictSteamPrice_Mid

DistrictSteamCosts_Post2_High <- EUIDistrictSteam_Post2
DistrictSteamCosts_Post2_High[,3:length(DistrictSteamCosts_Post2_High)] <- DistrictSteamCosts_Post2_High[,3:length(DistrictSteamCosts_Post2_High)]*DistrictSteamPrice_High


# Natural Gas

NaturalGasCosts_Post2_Low <- EUINaturalGas_Post2
NaturalGasCosts_Post2_Low[,3:length(NaturalGasCosts_Post2_Low)] <- NaturalGasCosts_Post2_Low[,3:length(NaturalGasCosts_Post2_Low)]*NaturalGasPrice_Low

NaturalGasCosts_Post2_Mid <- EUINaturalGas_Post2
NaturalGasCosts_Post2_Mid[,3:length(NaturalGasCosts_Post2_Mid)] <- NaturalGasCosts_Post2_Mid[,3:length(NaturalGasCosts_Post2_Mid)]*NaturalGasPrice_Mid

NaturalGasCosts_Post2_High <- EUINaturalGas_Post2
NaturalGasCosts_Post2_High[,3:length(NaturalGasCosts_Post2_High)] <- NaturalGasCosts_Post2_High[,3:length(NaturalGasCosts_Post2_High)]*NaturalGasPrice_High


# Electricity

ElectricityCosts_Post2_Low <- EUIElectricity_Post2
ElectricityCosts_Post2_Low[,3:length(ElectricityCosts_Post2_Low)] <- ElectricityCosts_Post2_Low[,3:length(ElectricityCosts_Post2_Low)]*ElectricityPrice_Low

ElectricityCosts_Post2_Mid <- EUIElectricity_Post2
ElectricityCosts_Post2_Mid[,3:length(ElectricityCosts_Post2_Mid)] <- ElectricityCosts_Post2_Mid[,3:length(ElectricityCosts_Post2_Mid)]*ElectricityPrice_Mid

ElectricityCosts_Post2_High <- EUIElectricity_Post2
ElectricityCosts_Post2_High[,3:length(ElectricityCosts_Post2_High)] <- ElectricityCosts_Post2_High[,3:length(ElectricityCosts_Post2_High)]*ElectricityPrice_High


# Add all costs together
# UNITS: $/sqft/yr
# help from: https://stackoverflow.com/questions/54078513/how-to-sum-same-column-of-different-data-frames-in-r


# Low, mid, high scenarios
Post2EnergyCostsSqft_Low <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

Post2EnergyCostsSqft_Low <- FuelOil2Costs_Post2_Low[, columns] + FuelOil4Costs_Post2_Low[, columns] + FuelOil56Costs_Post2_Low[, columns] +
  DistrictSteamCosts_Post2_Low[, columns] + NaturalGasCosts_Post2_Low[, columns] + ElectricityCosts_Post2_Low[, columns]

Post2EnergyCostsSqft_Low <- Post2EnergyCostsSqft_Low %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

Post2EnergyCostsSqft_Mid <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

Post2EnergyCostsSqft_Mid <- FuelOil2Costs_Post2_Mid[, columns] + FuelOil4Costs_Post2_Mid[, columns] + FuelOil56Costs_Post2_Mid[, columns] +
  DistrictSteamCosts_Post2_Mid[, columns] + NaturalGasCosts_Post2_Mid[, columns] + ElectricityCosts_Post2_Mid[, columns]

Post2EnergyCostsSqft_Mid <- Post2EnergyCostsSqft_Mid %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

Post2EnergyCostsSqft_High <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

Post2EnergyCostsSqft_High <- FuelOil2Costs_Post2_High[, columns] + FuelOil4Costs_Post2_High[, columns] + FuelOil56Costs_Post2_High[, columns] +
  DistrictSteamCosts_Post2_High[, columns] + NaturalGasCosts_Post2_High[, columns] + ElectricityCosts_Post2_High[, columns]

Post2EnergyCostsSqft_High <- Post2EnergyCostsSqft_High %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

# Add columns for total, undiscounted costs, 2024-2050
Post2EnergyCostsSqft_Low <- Post2EnergyCostsSqft_Low %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

Post2EnergyCostsSqft_Mid <- Post2EnergyCostsSqft_Mid %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

Post2EnergyCostsSqft_High <- Post2EnergyCostsSqft_High %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))


# Scenario 2 energy cost savings

Delta2EnergyCostsSqft_Low <- cbind(Post2EnergyCostsSqft_Low[1:2],BAUEnergyCostsSqft_Low[3:length(BAUEnergyCostsSqft_Low)]-
                                     Post2EnergyCostsSqft_Low[3:length(Post2EnergyCostsSqft_Low)])

Delta2EnergyCostsSqft_Mid <- cbind(Post2EnergyCostsSqft_Mid[1:2],BAUEnergyCostsSqft_Mid[3:length(BAUEnergyCostsSqft_Mid)]-
                                     Post2EnergyCostsSqft_Mid[3:length(Post2EnergyCostsSqft_Mid)])

Delta2EnergyCostsSqft_High <- cbind(Post2EnergyCostsSqft_High[1:2],BAUEnergyCostsSqft_High[3:length(BAUEnergyCostsSqft_High)]-
                                      Post2EnergyCostsSqft_High[3:length(Post2EnergyCostsSqft_High)])


# Penalty Calculations ----------------------------------------------------

# Fuel Oil 2
FuelOil2Emissions_Post2 <- mapply("*", EUIFuelOil2_Post2[intersect(names(EUIFuelOil2_Post2), names(FuelOil2EF))],
                                  FuelOil2EF[intersect(names(EUIFuelOil2_Post2), names(FuelOil2EF))]) %>% 
  as.data.frame()

FuelOil2Emissions_Post2 <- FuelOil2Emissions_Post2 %>% 
  mutate("HousingType" = EUIFuelOil2_Post2$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil2_Post2$BBL) %>% 
  relocate(BBL)

# Fuel Oil 4
FuelOil4Emissions_Post2 <- mapply("*", EUIFuelOil4_Post2[intersect(names(EUIFuelOil4_Post2), names(FuelOil4EF))],
                                  FuelOil4EF[intersect(names(EUIFuelOil4_Post2), names(FuelOil4EF))]) %>% 
  as.data.frame()

FuelOil4Emissions_Post2 <- FuelOil4Emissions_Post2 %>% 
  mutate("HousingType" = EUIFuelOil4_Post2$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil4_Post2$BBL) %>% 
  relocate(BBL)

# Fuel Oil 56
FuelOil56Emissions_Post2 <- mapply("*", EUIFuelOil56_Post2[intersect(names(EUIFuelOil56_Post2), names(FuelOil56EF))],
                                   FuelOil56EF[intersect(names(EUIFuelOil56_Post2), names(FuelOil56EF))]) %>% 
  as.data.frame()

FuelOil56Emissions_Post2 <- FuelOil56Emissions_Post2 %>% 
  mutate("HousingType" = EUIFuelOil56_Post2$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil56_Post2$BBL) %>% 
  relocate(BBL)

# District Steam
DistrictSteamEmissions_Post2 <- mapply("*", EUIDistrictSteam_Post2[intersect(names(EUIDistrictSteam_Post2), names(DistrictSteamEF))],
                                       DistrictSteamEF[intersect(names(EUIDistrictSteam_Post2), names(DistrictSteamEF))]) %>% 
  as.data.frame()

DistrictSteamEmissions_Post2 <- DistrictSteamEmissions_Post2 %>% 
  mutate("HousingType" = EUIDistrictSteam_Post2$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIDistrictSteam_Post2$BBL) %>% 
  relocate(BBL)

# Natural Gas
NaturalGasEmissions_Post2 <- mapply("*", EUINaturalGas_Post2[intersect(names(EUINaturalGas_Post2), names(NaturalGasEF))],
                                    NaturalGasEF[intersect(names(EUINaturalGas_Post2), names(NaturalGasEF))]) %>% 
  as.data.frame()

NaturalGasEmissions_Post2 <- NaturalGasEmissions_Post2 %>% 
  mutate("HousingType" = EUINaturalGas_Post2$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUINaturalGas_Post2$BBL) %>% 
  relocate(BBL)

# Electricity
ElectricityEmissions_Post2 <- mapply("*", EUIElectricity_Post2[intersect(names(EUIElectricity_Post2), names(ElectricityEF))],
                                     ElectricityEF[intersect(names(EUIElectricity_Post2), names(ElectricityEF))]) %>% 
  as.data.frame()

ElectricityEmissions_Post2 <- ElectricityEmissions_Post2 %>% 
  mutate("HousingType" = EUIElectricity_Post2$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIElectricity_Post2$BBL) %>% 
  relocate(BBL)


# Add all emissions together
# UNITS: MTCO2/sqft/yr


Post2EmissionsSqft <- cbind(FuelOil2Emissions_Post2[1:2], 
                            FuelOil2Emissions_Post2[, columns] + FuelOil4Emissions_Post2[, columns] + 
                              FuelOil56Emissions_Post2[, columns] + DistrictSteamEmissions_Post2[, columns] + 
                              NaturalGasEmissions_Post2[, columns] + ElectricityEmissions_Post2[, columns])

# Calculate difference between Post 2 emissions intensity and LL97 limits

# subtract LL97 limits from BAU emissions
Post2EmissionsOverLimit <- mapply("-", Post2EmissionsSqft[intersect(names(Post2EmissionsSqft), names(LL97limits))],
                                  LL97limits[intersect(names(Post2EmissionsSqft), names(LL97limits))]) %>% 
  as.data.frame()

# replace negatives with zero
Post2EmissionsOverLimit[Post2EmissionsOverLimit<0] <- 0

Post2EmissionsOverLimit <- Post2EmissionsOverLimit %>% 
  mutate("HousingType" = Post2EmissionsSqft$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = Post2EmissionsSqft$BBL) %>% 
  relocate(BBL)

# Calculate penalties for each building 2019-2050

Post2LL97PenaltiesBldg <- Post2EmissionsOverLimit %>% 
  left_join(BuildingSize, by = "BBL") %>% 
  rename(BldgArea = `Multifamily Housing - Gross Floor Area (ft²)`) %>% 
  relocate(c("BBL", "HousingType", "BldgArea", "Apartments"))

Post2LL97PenaltiesBldg <- Post2LL97PenaltiesBldg %>% 
  mutate(across(starts_with("2"),~.*BldgArea*LL97penalty/1000))

Post2LL97PenaltiesBldg <- Post2LL97PenaltiesBldg %>% 
  mutate("TotalPenalties_2024to2050" = rowSums(across(`2019`:`2050`), na.rm = TRUE))


# Calculate penalties per sqft

Post2LL97PenaltiesSqft <- Post2LL97PenaltiesBldg %>% 
  mutate(across(starts_with("2"),~./BldgArea))

# add column for undiscounted penalty total (2019-2050)
Post2LL97PenaltiesSqft <- Post2LL97PenaltiesSqft %>% 
  mutate("UndiscountedPenalties" = rowSums(across(starts_with("2")), na.rm = TRUE)) 

# Calculate avoided penalties

Post2AvoidedPenaltiesSqft <- cbind(Post2LL97PenaltiesSqft[1:4],BAULL97PenaltiesSqft[5:(length(BAULL97PenaltiesSqft)-2)]-
                                     Post2LL97PenaltiesSqft[5:(length(Post2LL97PenaltiesSqft)-2)])

Post2AvoidedPenaltiesSqft[Post2AvoidedPenaltiesSqft < 0] <- 0


# Total Cost Savings Calculation ------------------------------------------

Post2CostSavingsSqft <- as.data.frame(Post2AvoidedPenaltiesSqft$AvoidedPenalties + Delta2EnergyCostsSqft_Mid$TotalCosts_2024to2050) %>% 
  rename("TotalCostSavings" = "Post2AvoidedPenaltiesSqft$AvoidedPenalties + Delta2EnergyCostsSqft_Mid$TotalCosts_2024to2050")

# NPV Calculations --------------------------------------------------------

# Energy Cost Savings NPVs

Delta2EnergyCostsSqft_Low_NPV <- cbind(Delta2EnergyCostsSqft_Low[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

Delta2EnergyCostsSqft_Mid_NPV <- cbind(Delta2EnergyCostsSqft_Mid[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

Delta2EnergyCostsSqft_High_NPV <- cbind(Delta2EnergyCostsSqft_High[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")


for (i in 1:length(dr)) {
  for (t in 1:nrow(Delta2EnergyCostsSqft_Low)) {
    
    Delta2EnergyCostsSqft_Low_NPV[t,i+2] <- NPV(cf0 = Delta2EnergyCostsSqft_Low[t,3],
                                                cf = as.numeric(Delta2EnergyCostsSqft_Low[t,3:(length(Delta2EnergyCostsSqft_Low)-1)],
                                                                na.rm = TRUE),
                                                times = timeperiod,
                                                i = dr[i])
    
    
    Delta2EnergyCostsSqft_Mid_NPV[t,i+2] <- NPV(cf0 = Delta2EnergyCostsSqft_Mid[t,3],
                                                cf = as.numeric(Delta2EnergyCostsSqft_Mid[t,3:(length(Delta2EnergyCostsSqft_Mid)-1)]),
                                                times = timeperiod,
                                                i = dr[i])
    
    Delta2EnergyCostsSqft_High_NPV[t,i+2] <- NPV(cf0 = Delta2EnergyCostsSqft_High[t,3],
                                                 cf = as.numeric(Delta2EnergyCostsSqft_High[t,3:(length(Delta2EnergyCostsSqft_High)-1)]),
                                                 times = timeperiod,
                                                 i = dr[i])
  }
}

# Create new dataframes for costs by apartment
Delta2EnergyCostsApt_Low <- Delta2EnergyCostsSqft_Low %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("T"),~.*AptSize$AptSqft))

Delta2EnergyCostsApt_Mid <- Delta2EnergyCostsSqft_Mid %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("T"),~.*AptSize$AptSqft))

Delta2EnergyCostsApt_High <- Delta2EnergyCostsSqft_High %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("T"),~.*AptSize$AptSqft))

Delta2EnergyCostsApt_Low_NPV <- Delta2EnergyCostsSqft_Low_NPV %>% 
  mutate(across(starts_with("NPV"),~.*AptSize$AptSqft))

Delta2EnergyCostsApt_Mid_NPV <- Delta2EnergyCostsSqft_Mid_NPV %>% 
  mutate(across(starts_with("NPV"),~.*AptSize$AptSqft))

Delta2EnergyCostsApt_High_NPV <- Delta2EnergyCostsSqft_High_NPV %>% 
  mutate(across(starts_with("NPV"),~.*AptSize$AptSqft))


# Zero Discount Rate
Delta2EnergyCostsApt_Low_NPV_DRzero_Mean <- mean(Delta2EnergyCostsApt_Low$TotalCosts_2024to2050)
Delta2EnergyCostsApt_Mid_NPV_DRzero_Mean <- mean(Delta2EnergyCostsApt_Mid$TotalCosts_2024to2050)
Delta2EnergyCostsApt_High_NPV_DRzero_Mean <- mean(Delta2EnergyCostsApt_High$TotalCosts_2024to2050)

Delta2EnergyCostsApt_Low_NPV_DRzero_StDev <- sd(Delta2EnergyCostsApt_Low$TotalCosts_2024to2050)
Delta2EnergyCostsApt_Mid_NPV_DRzero_StDev <- sd(Delta2EnergyCostsApt_Mid$TotalCosts_2024to2050)
Delta2EnergyCostsApt_High_NPV_DRzero_StDev <- sd(Delta2EnergyCostsApt_High$TotalCosts_2024to2050)

Delta2EnergyCostsApt_Low_NPV_DRzero_PctNeg <- sum(Delta2EnergyCostsApt_Low$TotalCosts_2024to2050 < 0)/nrow(Delta2EnergyCostsApt_Low)*100
Delta2EnergyCostsApt_Mid_NPV_DRzero_PctNeg <- sum(Delta2EnergyCostsApt_Mid$TotalCosts_2024to2050 < 0)/nrow(Delta2EnergyCostsApt_Mid)*100
Delta2EnergyCostsApt_High_NPV_DRzero_PctNeg <- sum(Delta2EnergyCostsApt_High$TotalCosts_2024to2050 < 0)/nrow(Delta2EnergyCostsApt_High)*100

# Low Discount Rate
Delta2EnergyCostsApt_Low_NPV_DRlow_Median <- median(Delta2EnergyCostsApt_Low_NPV$NPV_DRlow)
Delta2EnergyCostsApt_Mid_NPV_DRlow_Median <- median(Delta2EnergyCostsApt_Mid_NPV$NPV_DRlow)
Delta2EnergyCostsApt_High_NPV_DRlow_Median <- median(Delta2EnergyCostsApt_High_NPV$NPV_DRlow)

Delta2EnergyCostsApt_Low_NPV_DRlow_Mean <- mean(Delta2EnergyCostsApt_Low_NPV$NPV_DRlow)
Delta2EnergyCostsApt_Mid_NPV_DRlow_Mean <- mean(Delta2EnergyCostsApt_Mid_NPV$NPV_DRlow)
Delta2EnergyCostsApt_High_NPV_DRlow_Mean <- mean(Delta2EnergyCostsApt_High_NPV$NPV_DRlow)

Delta2EnergyCostsApt_Low_NPV_DRlow_StDev <- sd(Delta2EnergyCostsApt_Low_NPV$NPV_DRlow)
Delta2EnergyCostsApt_Mid_NPV_DRlow_StDev <- sd(Delta2EnergyCostsApt_Mid_NPV$NPV_DRlow)
Delta2EnergyCostsApt_High_NPV_DRlow_StDev <- sd(Delta2EnergyCostsApt_High_NPV$NPV_DRlow)

Delta2EnergyCostsApt_Low_NPV_DRlow_PctNeg <- sum(Delta2EnergyCostsApt_Low_NPV$NPV_DRlow < 0)/nrow(Delta2EnergyCostsApt_Low_NPV)*100
Delta2EnergyCostsApt_Mid_NPV_DRlow_PctNeg <- sum(Delta2EnergyCostsApt_Mid_NPV$NPV_DRlow < 0)/nrow(Delta2EnergyCostsApt_Mid_NPV)*100
Delta2EnergyCostsApt_High_NPV_DRlow_PctNeg <- sum(Delta2EnergyCostsApt_High_NPV$NPV_DRlow < 0)/nrow(Delta2EnergyCostsApt_High_NPV)*100

# Mid Discount Rate
Delta2EnergyCostsApt_Low_NPV_DRmid_Median <- median(Delta2EnergyCostsApt_Low_NPV$NPV_DRmid)
Delta2EnergyCostsApt_Mid_NPV_DRmid_Median <- median(Delta2EnergyCostsApt_Mid_NPV$NPV_DRmid)
Delta2EnergyCostsApt_High_NPV_DRmid_Median <- median(Delta2EnergyCostsApt_High_NPV$NPV_DRmid)

Delta2EnergyCostsApt_Low_NPV_DRmid_Mean <- mean(Delta2EnergyCostsApt_Low_NPV$NPV_DRmid)
Delta2EnergyCostsApt_Mid_NPV_DRmid_Mean <- mean(Delta2EnergyCostsApt_Mid_NPV$NPV_DRmid)
Delta2EnergyCostsApt_High_NPV_DRmid_Mean <- mean(Delta2EnergyCostsApt_High_NPV$NPV_DRmid)

Delta2EnergyCostsApt_Low_NPV_DRmid_StDev <- sd(Delta2EnergyCostsApt_Low_NPV$NPV_DRmid)
Delta2EnergyCostsApt_Mid_NPV_DRmid_StDev <- sd(Delta2EnergyCostsApt_Mid_NPV$NPV_DRmid)
Delta2EnergyCostsApt_High_NPV_DRmid_StDev <- sd(Delta2EnergyCostsApt_High_NPV$NPV_DRmid)

Delta2EnergyCostsApt_Low_NPV_DRmid_PctNeg <- sum(Delta2EnergyCostsApt_Low_NPV$NPV_DRmid < 0)/nrow(Delta2EnergyCostsApt_Low_NPV)*100
Delta2EnergyCostsApt_Mid_NPV_DRmid_PctNeg <- sum(Delta2EnergyCostsApt_Mid_NPV$NPV_DRmid < 0)/nrow(Delta2EnergyCostsApt_Mid_NPV)*100
Delta2EnergyCostsApt_High_NPV_DRmid_PctNeg <- sum(Delta2EnergyCostsApt_High_NPV$NPV_DRmid < 0)/nrow(Delta2EnergyCostsApt_High_NPV)*100


# High Discount Rate
Delta2EnergyCostsApt_Low_NPV_DRhigh_Median <- median(Delta2EnergyCostsApt_Low_NPV$NPV_DRhigh)
Delta2EnergyCostsApt_Mid_NPV_DRhigh_Median <- median(Delta2EnergyCostsApt_Mid_NPV$NPV_DRhigh)
Delta2EnergyCostsApt_High_NPV_DRhigh_Median <- median(Delta2EnergyCostsApt_High_NPV$NPV_DRhigh)

Delta2EnergyCostsApt_Low_NPV_DRhigh_Mean <- mean(Delta2EnergyCostsApt_Low_NPV$NPV_DRhigh)
Delta2EnergyCostsApt_Mid_NPV_DRhigh_Mean <- mean(Delta2EnergyCostsApt_Mid_NPV$NPV_DRhigh)
Delta2EnergyCostsApt_High_NPV_DRhigh_Mean <- mean(Delta2EnergyCostsApt_High_NPV$NPV_DRhigh)

Delta2EnergyCostsApt_Low_NPV_DRhigh_StDev <- sd(Delta2EnergyCostsApt_Low_NPV$NPV_DRhigh)
Delta2EnergyCostsApt_Mid_NPV_DRhigh_StDev <- sd(Delta2EnergyCostsApt_Mid_NPV$NPV_DRhigh)
Delta2EnergyCostsApt_High_NPV_DRhigh_StDev <- sd(Delta2EnergyCostsApt_High_NPV$NPV_DRhigh)

Delta2EnergyCostsApt_Low_NPV_DRhigh_PctNeg <- sum(Delta2EnergyCostsApt_Low_NPV$NPV_DRhigh < 0)/nrow(Delta2EnergyCostsApt_Low_NPV)*100
Delta2EnergyCostsApt_Mid_NPV_DRhigh_PctNeg <- sum(Delta2EnergyCostsApt_Mid_NPV$NPV_DRhigh < 0)/nrow(Delta2EnergyCostsApt_Mid_NPV)*100
Delta2EnergyCostsApt_High_NPV_DRhigh_PctNeg <- sum(Delta2EnergyCostsApt_High_NPV$NPV_DRhigh < 0)/nrow(Delta2EnergyCostsApt_High_NPV)*100


# Avoided Penalties NPVs

# Per SQFT

Post2AvoidedPenaltiesSqft_NPV <- cbind(Post2AvoidedPenaltiesSqft[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

for (i in 1:length(dr)) {
  for (t in 1:nrow(Post2AvoidedPenaltiesSqft_NPV)) {
    
    Post2AvoidedPenaltiesSqft_NPV[t,i+2] <- NPV(cf0 = Post2AvoidedPenaltiesSqft[t,5],
                                                cf = as.numeric(Post2AvoidedPenaltiesSqft[t,5:length(Post2AvoidedPenaltiesSqft)],
                                                                na.rm = TRUE),
                                                times = timeperiod,
                                                i = dr[i])
  }
}

Post2AvoidedPenaltiesSqft_NPV_DRlow_Mean <- mean(Post2AvoidedPenaltiesSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post2AvoidedPenaltiesSqft_NPV_DRlow_StDev <- sd(Post2AvoidedPenaltiesSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post2AvoidedPenaltiesSqft_NPV_DRmid_Mean <- mean(Post2AvoidedPenaltiesSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post2AvoidedPenaltiesSqft_NPV_DRmid_StDev <- sd(Post2AvoidedPenaltiesSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post2AvoidedPenaltiesSqft_NPV_DRhigh_Mean <- mean(Post2AvoidedPenaltiesSqft_NPV$NPV_DRhigh, na.rm = TRUE)
Post2AvoidedPenaltiesSqft_NPV_DRhigh_StDev <- sd(Post2AvoidedPenaltiesSqft_NPV$NPV_DRhigh, na.rm = TRUE)

# Add total undiscounted column
Post2AvoidedPenaltiesSqft <- Post2AvoidedPenaltiesSqft %>% 
  mutate("UndiscountedTotal" = rowSums(across(`2024`:`2050`), na.rm = TRUE))
# Calculate Mean, St Dev for undiscounted avoided penalties
Post2AvoidedPenaltiesSqft_NPV_DRzero_Mean <- mean(Post2AvoidedPenaltiesSqft$UndiscountedTotal, na.rm = TRUE)
Post2AvoidedPenaltiesSqft_NPV_DRzero_StDev <- sd(Post2AvoidedPenaltiesSqft$UndiscountedTotal, na.rm = TRUE)


# PER APT

# Create new dataframes for avoided penalties by apartment
Post2AvoidedPenaltiesApt <- Post2AvoidedPenaltiesSqft %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("U"),~.*AptSize$AptSqft))

Post2AvoidedPenaltiesApt_NPV <- Post2AvoidedPenaltiesSqft_NPV %>% 
  mutate(across(starts_with("N"),~.*AptSize$AptSqft))

Post2AvoidedPenaltiesApt_NPV_DRlow_Mean <- mean(Post2AvoidedPenaltiesApt_NPV$NPV_DRlow, na.rm = TRUE)
Post2AvoidedPenaltiesApt_NPV_DRlow_StDev <- sd(Post2AvoidedPenaltiesApt_NPV$NPV_DRlow, na.rm = TRUE)
Post2AvoidedPenaltiesApt_NPV_DRmid_Mean <- mean(Post2AvoidedPenaltiesApt_NPV$NPV_DRmid, na.rm = TRUE)
Post2AvoidedPenaltiesApt_NPV_DRmid_StDev <- sd(Post2AvoidedPenaltiesApt_NPV$NPV_DRmid, na.rm = TRUE)
Post2AvoidedPenaltiesApt_NPV_DRhigh_Mean <- mean(Post2AvoidedPenaltiesApt_NPV$NPV_DRhigh, na.rm = TRUE)
Post2AvoidedPenaltiesApt_NPV_DRhigh_StDev <- sd(Post2AvoidedPenaltiesApt_NPV$NPV_DRhigh, na.rm = TRUE)


# Calculate Mean, St Dev for undiscounted avoided penalties
Post2AvoidedPenaltiesApt_NPV_DRzero_Mean <- mean(Post2AvoidedPenaltiesApt$UndiscountedTotal, na.rm = TRUE)
Post2AvoidedPenaltiesApt_NPV_DRzero_StDev <- sd(Post2AvoidedPenaltiesApt$UndiscountedTotal, na.rm = TRUE)


# Total Cost Savings

# PER SQFT

Post2CostSavingsSqft <- cbind(Post2AvoidedPenaltiesSqft[1:4],
                              Delta2EnergyCostsSqft_Mid[3:(length(Delta2EnergyCostsSqft_Mid)-1)] + 
                                Post2AvoidedPenaltiesSqft[5:(length(Post2AvoidedPenaltiesSqft)-1)])

Post2CostSavingsSqft <- Post2CostSavingsSqft %>% 
  mutate("UndiscountedTotal" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

# Calculate NPV
Post2CostSavingsSqft_NPV <- cbind(Post2CostSavingsSqft[1:2],dr_low,dr_mid,dr_high) %>% 
  rename("NPV_DRlow" = "dr_low") %>% 
  rename("NPV_DRmid" = "dr_mid") %>%
  rename("NPV_DRhigh" = "dr_high")

for (i in 1:length(dr)) {
  for (t in 1:nrow(Post2CostSavingsSqft_NPV)) {
    
    Post2CostSavingsSqft_NPV[t,i+2] <- NPV(cf0 = Post2CostSavingsSqft[t,5],
                                           cf = as.numeric(Post2CostSavingsSqft[t,5:(length(Post2CostSavingsSqft)-1)],
                                                           na.rm = TRUE),
                                           times = timeperiod,
                                           i = dr[i])
  }
}

Post2CostSavingsSqft_NPV_DRlow_Mean <- mean(Post2CostSavingsSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post2CostSavingsSqft_NPV_DRlow_StDev <- sd(Post2CostSavingsSqft_NPV$NPV_DRlow, na.rm = TRUE)
Post2CostSavingsSqft_NPV_DRlow_PctNeg <- sum(Post2CostSavingsSqft_NPV$NPV_DRlow < 0)/nrow(Post2CostSavingsSqft_NPV)*100
Post2CostSavingsSqft_NPV_DRmid_Mean <- mean(Post2CostSavingsSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post2CostSavingsSqft_NPV_DRmid_StDev <- sd(Post2CostSavingsSqft_NPV$NPV_DRmid, na.rm = TRUE)
Post2CostSavingsSqft_NPV_DRmid_PctNeg <- sum(Post2CostSavingsSqft_NPV$NPV_DRmid < 0)/nrow(Post2CostSavingsSqft_NPV)*100
Post2CostSavingsSqft_NPV_DRhigh_Mean <- mean(Post2CostSavingsSqft_NPV$NPV_DRhigh, na.rm = TRUE)
Post2CostSavingsSqft_NPV_DRhigh_StDev <- sd(Post2CostSavingsSqft_NPV$NPV_DRhigh, na.rm = TRUE)
Post2CostSavingsSqft_NPV_DRhigh_PctNeg <- sum(Post2CostSavingsSqft_NPV$NPV_DRhigh < 0)/nrow(Post2CostSavingsSqft_NPV)*100

# PER APT

# Create new NPV dataframes
Post2CostSavingsApt <- Post2CostSavingsSqft %>% 
  mutate(across(starts_with("2"),~.*AptSize$AptSqft)) %>% 
  mutate(across(starts_with("U"),~.*AptSize$AptSqft))

Post2CostSavingsApt_NPV <- Post2CostSavingsSqft_NPV %>% 
  mutate(across(starts_with("N"),~.*AptSize$AptSqft))

# Calculate Mean, SD, % <0
Post2CostSavingsApt_NPV_DRzero_Mean <- mean(Post2CostSavingsApt$UndiscountedTotal, na.rm = TRUE)
Post2CostSavingsApt_NPV_DRzero_StDev <- sd(Post2CostSavingsApt$UndiscountedTotal, na.rm = TRUE)
Post2CostSavingsApt_NPV_DRzero_PctNeg <- sum(Post2CostSavingsApt$UndiscountedTotal < 0)/nrow(Post2CostSavingsApt)*100
Post2CostSavingsApt_NPV_DRlow_Mean <- mean(Post2CostSavingsApt_NPV$NPV_DRlow, na.rm = TRUE)
Post2CostSavingsApt_NPV_DRlow_StDev <- sd(Post2CostSavingsApt_NPV$NPV_DRlow, na.rm = TRUE)
Post2CostSavingsApt_NPV_DRlow_PctNeg <- sum(Post2CostSavingsApt_NPV$NPV_DRlow < 0)/nrow(Post2CostSavingsApt_NPV)*100
Post2CostSavingsApt_NPV_DRmid_Mean <- mean(Post2CostSavingsApt_NPV$NPV_DRmid, na.rm = TRUE)
Post2CostSavingsApt_NPV_DRmid_StDev <- sd(Post2CostSavingsApt_NPV$NPV_DRmid, na.rm = TRUE)
Post2CostSavingsApt_NPV_DRmid_PctNeg <- sum(Post2CostSavingsApt_NPV$NPV_DRmid < 0)/nrow(Post2CostSavingsApt_NPV)*100
Post2CostSavingsApt_NPV_DRhigh_Mean <- mean(Post2CostSavingsApt_NPV$NPV_DRhigh, na.rm = TRUE)
Post2CostSavingsApt_NPV_DRhigh_StDev <- sd(Post2CostSavingsApt_NPV$NPV_DRhigh, na.rm = TRUE)
Post2CostSavingsApt_NPV_DRhigh_PctNeg <- sum(Post2CostSavingsApt_NPV$NPV_DRhigh < 0)/nrow(Post2CostSavingsApt_NPV)*100



