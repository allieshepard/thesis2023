# BAU Energy Costs and Emissions
# Script 5/10
# Run this script to calculate the energy costs for each building in the Business as Usual (BAU) scenario
# and calculate the BAU emissions for each building

# Calculate BAU Energy Costs -----------------------------------------

# Energy Prices

# Electricity
# New York State Monthly Average Retail Price of Electricity - Residential
# Source: https://www.nyserda.ny.gov/Researchers-and-Policymakers/Energy-Prices/Electricity/Monthly-Avg-Electricity-Residential
# UNITS: $/kWh
ElectricityPrice_Mid <- 0.22125
ElectricityPrice_Low <- ElectricityPrice_Mid * (1 - 0.25)
ElectricityPrice_High <- ElectricityPrice_Mid * (1 + 0.25)

# Natural Gas
# New York State Monthly Average Price of Natural Gas - Residential
# Source: https://www.nyserda.ny.gov/Researchers-and-Policymakers/Energy-Prices/Natural-Gas/Monthly-Average-Price-of-Natural-Gas-Residential
# UNITS: $/therm
NaturalGasPrice_Mid <- 1.922131148
NaturalGasPrice_Low <- NaturalGasPrice_Mid * (1 - 0.25)
NaturalGasPrice_High <- NaturalGasPrice_Mid * (1 + 0.25)  

# District Steam
# NYC Steam Case study, uses $33/Mlb and 1,194 kBtu/Mlb
# Source: https://assets.ctfassets.net/ntcn17ss1ow9/73gDFE9yMk45H4mEezo18y/f2532fe00d1ae79e1b68d5d6de4c7524/EEFA-Upgrading_NYC_Steam_Systems.pdf
# UNITS: $/kBtu
DistrictSteamPrice_Mid <- 0.0276381909547739
DistrictSteamPrice_Low <- DistrictSteamPrice_Mid * (1 - 0.25) 
DistrictSteamPrice_High <- DistrictSteamPrice_Mid * (1 + 0.25)  

# Fuel Oil 2
# Average Residential Heating Oil price in 2022 for New York City region
# source: https://www.nyserda.ny.gov/researchers-and-policymakers/energy-prices/home-heating-oil/average-home-heating-oil-prices#nygov-header
# UNITS: $/kBtu
FuelOil2Price_Mid <- 0.0367965 
FuelOil2Price_Low <- FuelOil2Price_Mid * (1 - 0.25)
FuelOil2Price_High <- FuelOil2Price_Mid * (1 + 0.25)   


# Fuel Oil 4
# Average Residential Heating Oil price in 2022 for New York City region
# source: https://www.nyserda.ny.gov/researchers-and-policymakers/energy-prices/home-heating-oil/average-home-heating-oil-prices#nygov-header
# UNITS: $/kBtu
FuelOil4Price_Mid <- 0.0367965
FuelOil4Price_Low <- FuelOil4Price_Mid * (1 - 0.25)
FuelOil4Price_High <- FuelOil4Price_Mid * (1 + 0.25)

# Fuel Oil 5/6
# Average Residential Heating Oil price in 2022 for New York City region
# source: https://www.nyserda.ny.gov/researchers-and-policymakers/energy-prices/home-heating-oil/average-home-heating-oil-prices#nygov-header
# UNITS: $/kBtu
FuelOil56Price_Mid <- 0.0367965
FuelOil56Price_Low <- FuelOil56Price_Mid * (1 - 0.25)
FuelOil56Price_High <- FuelOil56Price_Mid * (1 + 0.25)


# Create new dataframe for EUI by source, from Baseline (2017-2019) to 2050 under
#  Business As Usual (BAU) case, which assumes no change in energy use
EUIbySource_BAU <- Baseline_Housing %>% 
  select(c("BBL", "Year Built", "Multifamily Housing - Gross Floor Area (ft²)",
           "Multifamily Unit Density (sqft/unit)", "Subsidized", "NOAH", "RentRegulated",
           "BaselineFuelOil2_kBtupersqft", "BaselineFuelOil4_kBtupersqft", 
           "BaselineFuelOil56_kBtupersqft", "BaselineDistrictSteam_kBtupersqft", 
           "BaselineNaturalGas_WeatherNorm_thermpersqft", "BaselineElectricity_WeatherNorm_kWhpersqft"))

years <- 2020:2050

for (i in years) {
  newFuelOil2_kBtupersqft <- rep(EUIbySource_BAU$BaselineFuelOil2_kBtupersqft) 
  EUIbySource_BAU[ , ncol(EUIbySource_BAU) + 1] <- newFuelOil2_kBtupersqft
  colnames(EUIbySource_BAU)[ncol(EUIbySource_BAU)] <- paste0(i,"FuelOil2_kBtupersqft")
  
  newFuelOil4_kBtupersqft <- rep(EUIbySource_BAU$BaselineFuelOil4_kBtupersqft)
  EUIbySource_BAU[ , ncol(EUIbySource_BAU) + 1] <- newFuelOil4_kBtupersqft
  colnames(EUIbySource_BAU)[ncol(EUIbySource_BAU)] <- paste0(i,"FuelOil4_kBtupersqft")
  
  newFuelOil56_kBtupersqft <- rep(EUIbySource_BAU$BaselineFuelOil56_kBtupersqft)
  EUIbySource_BAU[ , ncol(EUIbySource_BAU) + 1] <- newFuelOil56_kBtupersqft
  colnames(EUIbySource_BAU)[ncol(EUIbySource_BAU)] <- paste0(i,"FuelOil56_kBtupersqft")
  
  newDistrictSteam_kBtupersqft <- rep(EUIbySource_BAU$BaselineDistrictSteam_kBtupersqft)
  EUIbySource_BAU[ , ncol(EUIbySource_BAU) + 1] <- newDistrictSteam_kBtupersqft
  colnames(EUIbySource_BAU)[ncol(EUIbySource_BAU)] <- paste0(i,"DistrictSteam_kBtupersqft")
  
  newNaturalGas_thermpersqft <- rep(EUIbySource_BAU$BaselineNaturalGas_WeatherNorm_thermpersqft)
  EUIbySource_BAU[ , ncol(EUIbySource_BAU) + 1] <- newNaturalGas_thermpersqft
  colnames(EUIbySource_BAU)[ncol(EUIbySource_BAU)] <- paste0(i,"NaturalGas_thermpersqft")
  
  newElectricity_kWhpersqft <- rep(EUIbySource_BAU$BaselineElectricity_WeatherNorm_kWhpersqft)
  EUIbySource_BAU[ , ncol(EUIbySource_BAU) + 1] <- newElectricity_kWhpersqft
  colnames(EUIbySource_BAU)[ncol(EUIbySource_BAU)] <- paste0(i,"Electricity_kWhpersqft")
}

# Rename Baseline to 2019, remove "WeatherNorm_
names(EUIbySource_BAU) <- gsub(x = names(EUIbySource_BAU), pattern = "Baseline", replacement = "2019")
names(EUIbySource_BAU) <- gsub(x = names(EUIbySource_BAU), pattern = "WeatherNorm_", replacement = "")

# Add column for MarketRate housing  identifier (if Subsidized, NOAH, RentRegulated all zero, MarketRate=1)
EUIbySource_BAU <- EUIbySource_BAU %>% 
  mutate("MarketRate" = if_else(Subsidized == 1 | NOAH == 1 | RentRegulated ==1, 0, 1)) %>% 
  relocate(MarketRate, .after = RentRegulated)

# Add column to identify housing type (Subsidized, NOAH, RentRegulated, MarketRate)
EUIbySource_BAU <- EUIbySource_BAU %>% 
  mutate("HousingType" = if_else(Subsidized ==1, "Subsidized",
                                 if_else(NOAH ==1, "NOAH", if_else(RentRegulated ==1, "RentRegulated",
                                                                   if_else(MarketRate == 1,
                                                                           "MarketRate","Error"))))) %>% 
  relocate(HousingType) %>% 
  select(contains(c("BBL", "HousingType", "2")))

# Calculate the mean fuel usage by year & fuel, grouped by housing type
EUIbySource_BAU_means <- EUIbySource_BAU %>% 
  group_by(HousingType) %>% 
  summarise(across(everything(),mean),
            .groups = 'drop') %>% 
  as.data.frame()


# Create cost dataframes for each fuel type 
# UNITS for XXXCost = $/sqft/yr

# Fuel Oil 2
EUIFuelOil2_BAU <-  EUIbySource_BAU %>%
  select(contains(c("BBL", "HousingType", "FuelOil2")))

names(EUIFuelOil2_BAU) <- gsub(x = names(EUIFuelOil2_BAU),
                               pattern = "FuelOil2_kBtupersqft", replacement = "")


# Calculate costs for low, med, high scenarios
FuelOil2Costs_BAU_Low <- EUIFuelOil2_BAU
FuelOil2Costs_BAU_Low[,3:length(FuelOil2Costs_BAU_Low)] <- FuelOil2Costs_BAU_Low[,3:length(FuelOil2Costs_BAU_Low)]*FuelOil2Price_Low

FuelOil2Costs_BAU_Mid <- EUIFuelOil2_BAU
FuelOil2Costs_BAU_Mid[,3:length(FuelOil2Costs_BAU_Mid)] <- FuelOil2Costs_BAU_Mid[,3:length(FuelOil2Costs_BAU_Mid)]*FuelOil2Price_Mid

FuelOil2Costs_BAU_High <- EUIFuelOil2_BAU
FuelOil2Costs_BAU_High[,3:length(FuelOil2Costs_BAU_High)] <- FuelOil2Costs_BAU_High[,3:length(FuelOil2Costs_BAU_High)]*FuelOil2Price_High



# Fuel Oil 4
EUIFuelOil4_BAU <-  EUIbySource_BAU %>%
  select(contains(c("BBL", "HousingType", "FuelOil4")))

names(EUIFuelOil4_BAU) <- gsub(x = names(EUIFuelOil4_BAU),
                               pattern = "FuelOil4_kBtupersqft", replacement = "")

# Calculate costs for low, med, high scenarios
FuelOil4Costs_BAU_Low <- EUIFuelOil4_BAU
FuelOil4Costs_BAU_Low[,3:length(FuelOil4Costs_BAU_Low)] <- FuelOil4Costs_BAU_Low[,3:length(FuelOil4Costs_BAU_Low)]*FuelOil4Price_Low

FuelOil4Costs_BAU_Mid <- EUIFuelOil4_BAU
FuelOil4Costs_BAU_Mid[,3:length(FuelOil4Costs_BAU_Mid)] <- FuelOil4Costs_BAU_Mid[,3:length(FuelOil4Costs_BAU_Mid)]*FuelOil4Price_Mid

FuelOil4Costs_BAU_High <- EUIFuelOil4_BAU
FuelOil4Costs_BAU_High[,3:length(FuelOil4Costs_BAU_High)] <- FuelOil4Costs_BAU_High[,3:length(FuelOil4Costs_BAU_High)]*FuelOil4Price_High


# Fuel Oil 56
EUIFuelOil56_BAU <-  EUIbySource_BAU %>%
  select(contains(c("BBL", "HousingType", "FuelOil56")))

names(EUIFuelOil56_BAU) <- gsub(x = names(EUIFuelOil56_BAU),
                                pattern = "FuelOil56_kBtupersqft", replacement = "")

# Calculate costs for low, med, high scenarios
FuelOil56Costs_BAU_Low <- EUIFuelOil56_BAU
FuelOil56Costs_BAU_Low[,3:length(FuelOil56Costs_BAU_Low)] <- FuelOil56Costs_BAU_Low[,3:length(FuelOil56Costs_BAU_Low)]*FuelOil56Price_Low

FuelOil56Costs_BAU_Mid <- EUIFuelOil56_BAU
FuelOil56Costs_BAU_Mid[,3:length(FuelOil56Costs_BAU_Mid)] <- FuelOil56Costs_BAU_Mid[,3:length(FuelOil56Costs_BAU_Mid)]*FuelOil56Price_Mid

FuelOil56Costs_BAU_High <- EUIFuelOil56_BAU
FuelOil56Costs_BAU_High[,3:length(FuelOil56Costs_BAU_High)] <- FuelOil56Costs_BAU_High[,3:length(FuelOil56Costs_BAU_High)]*FuelOil56Price_High


# District Steam
EUIDistrictSteam_BAU <-  EUIbySource_BAU %>%
  select(contains(c("BBL", "HousingType", "DistrictSteam")))

names(EUIDistrictSteam_BAU) <- gsub(x = names(EUIDistrictSteam_BAU),
                                    pattern = "DistrictSteam_kBtupersqft", replacement = "")

# Calculate costs for low, med, high scenarios
DistrictSteamCosts_BAU_Low <- EUIDistrictSteam_BAU
DistrictSteamCosts_BAU_Low[,3:length(DistrictSteamCosts_BAU_Low)] <- DistrictSteamCosts_BAU_Low[,3:length(DistrictSteamCosts_BAU_Low)]*DistrictSteamPrice_Low

DistrictSteamCosts_BAU_Mid <- EUIDistrictSteam_BAU
DistrictSteamCosts_BAU_Mid[,3:length(DistrictSteamCosts_BAU_Mid)] <- DistrictSteamCosts_BAU_Mid[,3:length(DistrictSteamCosts_BAU_Mid)]*DistrictSteamPrice_Mid

DistrictSteamCosts_BAU_High <- EUIDistrictSteam_BAU
DistrictSteamCosts_BAU_High[,3:length(DistrictSteamCosts_BAU_High)] <- DistrictSteamCosts_BAU_High[,3:length(DistrictSteamCosts_BAU_High)]*DistrictSteamPrice_High


# Natural Gas
EUINaturalGas_BAU <-  EUIbySource_BAU %>%
  select(contains(c("BBL", "HousingType", "NaturalGas")))

names(EUINaturalGas_BAU) <- gsub(x = names(EUINaturalGas_BAU),
                                 pattern = "NaturalGas_thermpersqft", replacement = "")

# Calculate costs for low, med, high scenarios
NaturalGasCosts_BAU_Low <- EUINaturalGas_BAU
NaturalGasCosts_BAU_Low[,3:length(NaturalGasCosts_BAU_Low)] <- NaturalGasCosts_BAU_Low[,3:length(NaturalGasCosts_BAU_Low)]*NaturalGasPrice_Low

NaturalGasCosts_BAU_Mid <- EUINaturalGas_BAU
NaturalGasCosts_BAU_Mid[,3:length(NaturalGasCosts_BAU_Mid)] <- NaturalGasCosts_BAU_Mid[,3:length(NaturalGasCosts_BAU_Mid)]*NaturalGasPrice_Mid

NaturalGasCosts_BAU_High <- EUINaturalGas_BAU
NaturalGasCosts_BAU_High[,3:length(NaturalGasCosts_BAU_High)] <- NaturalGasCosts_BAU_High[,3:length(NaturalGasCosts_BAU_High)]*NaturalGasPrice_High


# Electricity
EUIElectricity_BAU <-  EUIbySource_BAU %>%
  select(contains(c("BBL", "HousingType", "Electricity")))

names(EUIElectricity_BAU) <- gsub(x = names(EUIElectricity_BAU),
                                  pattern = "Electricity_kWhpersqft", replacement = "")

# Calculate costs for low, med, high scenarios
ElectricityCosts_BAU_Low <- EUIElectricity_BAU
ElectricityCosts_BAU_Low[,3:length(ElectricityCosts_BAU_Low)] <- ElectricityCosts_BAU_Low[,3:length(ElectricityCosts_BAU_Low)]*ElectricityPrice_Low

ElectricityCosts_BAU_Mid <- EUIElectricity_BAU
ElectricityCosts_BAU_Mid[,3:length(ElectricityCosts_BAU_Mid)] <- ElectricityCosts_BAU_Mid[,3:length(ElectricityCosts_BAU_Mid)]*ElectricityPrice_Mid

ElectricityCosts_BAU_High <- EUIElectricity_BAU
ElectricityCosts_BAU_High[,3:length(ElectricityCosts_BAU_High)] <- ElectricityCosts_BAU_High[,3:length(ElectricityCosts_BAU_High)]*ElectricityPrice_High


# Add all costs together
# UNITS: $/sqft/yr
# help from: https://stackoverflow.com/questions/54078513/how-to-sum-same-column-of-different-data-frames-in-r

columns <- as.character(2019:2050)

# Low, mid, high scenarios
BAUEnergyCostsSqft_Low <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

BAUEnergyCostsSqft_Low <- FuelOil2Costs_BAU_Low[, columns] + FuelOil4Costs_BAU_Low[, columns] + FuelOil56Costs_BAU_Low[, columns] +
  DistrictSteamCosts_BAU_Low[, columns] + NaturalGasCosts_BAU_Low[, columns] + ElectricityCosts_BAU_Low[, columns]

BAUEnergyCostsSqft_Low <- BAUEnergyCostsSqft_Low %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

BAUEnergyCostsSqft_Mid <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

BAUEnergyCostsSqft_Mid <- FuelOil2Costs_BAU_Mid[, columns] + FuelOil4Costs_BAU_Mid[, columns] + FuelOil56Costs_BAU_Mid[, columns] +
  DistrictSteamCosts_BAU_Mid[, columns] + NaturalGasCosts_BAU_Mid[, columns] + ElectricityCosts_BAU_Mid[, columns]

BAUEnergyCostsSqft_Mid <- BAUEnergyCostsSqft_Mid %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

BAUEnergyCostsSqft_High <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

BAUEnergyCostsSqft_High <- FuelOil2Costs_BAU_High[, columns] + FuelOil4Costs_BAU_High[, columns] + FuelOil56Costs_BAU_High[, columns] +
  DistrictSteamCosts_BAU_High[, columns] + NaturalGasCosts_BAU_High[, columns] + ElectricityCosts_BAU_High[, columns]

BAUEnergyCostsSqft_High <- BAUEnergyCostsSqft_High %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)

# Add columns for total, undiscounted costs, 2024-2050
BAUEnergyCostsSqft_Low <- BAUEnergyCostsSqft_Low %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

BAUEnergyCostsSqft_Mid <- BAUEnergyCostsSqft_Mid %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))

BAUEnergyCostsSqft_High <- BAUEnergyCostsSqft_High %>% 
  mutate("TotalCosts_2024to2050" = rowSums(across(`2024`:`2050`), na.rm = TRUE))


# Calculate yearly energy costs for whole building and for apartment

# Add column for number of apartments in building
# number of apts per building = total sqft * apartments/1,000sqft / 1,000
Baseline_Housing <- Baseline_Housing %>% 
  mutate("Apartments" = `Multifamily Housing - Gross Floor Area (ft²)` * 
           `Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 ft²)` /
           1000)

# Add columns for building sqft and apartment density
BuildingSize <- Baseline_Housing %>% 
  select("BBL", "Multifamily Housing - Gross Floor Area (ft²)", "Apartments")

# Low
BAUEnergyCostsBldg_Low <- BAUEnergyCostsSqft_Low %>% 
  left_join(BuildingSize, by = "BBL") %>% 
  rename(BldgArea = `Multifamily Housing - Gross Floor Area (ft²)`) %>% 
  relocate(c("BBL", "HousingType", "BldgArea", "Apartments"))

BAUEnergyCostsBldg_Low <- BAUEnergyCostsBldg_Low %>% 
  mutate(across(starts_with("2"),~.*BldgArea)) %>% 
  mutate(across(starts_with("T"),~.*BldgArea))

BAUEnergyCostsApt_Low <- BAUEnergyCostsBldg_Low %>% 
  mutate(across(starts_with("2"),~./Apartments)) %>% 
  mutate(across(starts_with("T"),~./Apartments))

# Mid
BAUEnergyCostsBldg_Mid <- BAUEnergyCostsSqft_Mid %>% 
  left_join(BuildingSize, by = "BBL") %>% 
  rename(BldgArea = `Multifamily Housing - Gross Floor Area (ft²)`) %>% 
  relocate(c("BBL", "HousingType", "BldgArea", "Apartments"))

BAUEnergyCostsBldg_Mid <- BAUEnergyCostsBldg_Mid %>% 
  mutate(across(starts_with("2"),~.*BldgArea)) %>% 
  mutate(across(starts_with("T"),~.*BldgArea))

BAUEnergyCostsApt_Mid <- BAUEnergyCostsBldg_Mid %>% 
  mutate(across(starts_with("2"),~./Apartments)) %>% 
  mutate(across(starts_with("T"),~./Apartments))

# High

BAUEnergyCostsBldg_High <- BAUEnergyCostsSqft_High %>% 
  left_join(BuildingSize, by = "BBL") %>% 
  rename(BldgArea = `Multifamily Housing - Gross Floor Area (ft²)`) %>% 
  relocate(c("BBL", "HousingType", "BldgArea", "Apartments"))

BAUEnergyCostsBldg_High <- BAUEnergyCostsBldg_High %>% 
  mutate(across(starts_with("2"),~.*BldgArea)) %>% 
  mutate(across(starts_with("T"),~.*BldgArea))

BAUEnergyCostsApt_High <- BAUEnergyCostsBldg_High %>% 
  mutate(across(starts_with("2"),~./Apartments)) %>% 
  mutate(across(starts_with("T"),~./Apartments))


# Calculate BAU Emissions -------------------------------------------------

# Load emissions factors for each fuel
# Import CSVs with emissions factors for 2017-2050 for each fuel type
# UNITS: fuel oil, district steam - kgCO2/kBtu
#        natural gas - kgCO2/therm
#        electricity - kgCO2/kWh

FuelOil2EF <- read_csv('EmissionsFactors/FuelOil2EF.csv')
FuelOil4EF <- read_csv('EmissionsFactors/FuelOil4EF.csv')
FuelOil56EF <- read_csv('EmissionsFactors/FuelOil56EF.csv')
DistrictSteamEF <- read_csv('EmissionsFactors/DistrictSteamEF.csv')
NaturalGasEF <- read_csv('EmissionsFactors/NaturalGasEF.csv')
ElectricityEF <- read_csv('EmissionsFactors/ElectricityEF.csv')

# Create emissions dataframes for each fuel type 

# Fuel Oil 2
FuelOil2Emissions <- mapply("*", EUIFuelOil2_BAU[intersect(names(EUIFuelOil2_BAU), names(FuelOil2EF))],
                            FuelOil2EF[intersect(names(EUIFuelOil2_BAU), names(FuelOil2EF))]) %>% 
  as.data.frame()

FuelOil2Emissions <- FuelOil2Emissions %>% 
  mutate("HousingType" = EUIFuelOil2_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil2_BAU$BBL) %>% 
  relocate(BBL)

# Fuel Oil 4
FuelOil4Emissions <- mapply("*", EUIFuelOil4_BAU[intersect(names(EUIFuelOil4_BAU), names(FuelOil4EF))],
                            FuelOil4EF[intersect(names(EUIFuelOil4_BAU), names(FuelOil4EF))]) %>% 
  as.data.frame()

FuelOil4Emissions <- FuelOil4Emissions %>% 
  mutate("HousingType" = EUIFuelOil4_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil4_BAU$BBL) %>% 
  relocate(BBL)

# Fuel Oil 56
FuelOil56Emissions <- mapply("*", EUIFuelOil56_BAU[intersect(names(EUIFuelOil56_BAU), names(FuelOil56EF))],
                             FuelOil56EF[intersect(names(EUIFuelOil56_BAU), names(FuelOil56EF))]) %>% 
  as.data.frame()

FuelOil56Emissions <- FuelOil56Emissions %>% 
  mutate("HousingType" = EUIFuelOil56_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIFuelOil56_BAU$BBL) %>% 
  relocate(BBL)

# District Steam
DistrictSteamEmissions <- mapply("*", EUIDistrictSteam_BAU[intersect(names(EUIDistrictSteam_BAU), names(DistrictSteamEF))],
                                 DistrictSteamEF[intersect(names(EUIDistrictSteam_BAU), names(DistrictSteamEF))]) %>% 
  as.data.frame()

DistrictSteamEmissions <- DistrictSteamEmissions %>% 
  mutate("HousingType" = EUIDistrictSteam_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIDistrictSteam_BAU$BBL) %>% 
  relocate(BBL)

# Natural Gas
NaturalGasEmissions <- mapply("*", EUINaturalGas_BAU[intersect(names(EUINaturalGas_BAU), names(NaturalGasEF))],
                              NaturalGasEF[intersect(names(EUINaturalGas_BAU), names(NaturalGasEF))]) %>% 
  as.data.frame()

NaturalGasEmissions <- NaturalGasEmissions %>% 
  mutate("HousingType" = EUINaturalGas_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUINaturalGas_BAU$BBL) %>% 
  relocate(BBL)

# Electricity
ElectricityEmissions <- mapply("*", EUIElectricity_BAU[intersect(names(EUIElectricity_BAU), names(ElectricityEF))],
                               ElectricityEF[intersect(names(EUIElectricity_BAU), names(ElectricityEF))]) %>% 
  as.data.frame()

ElectricityEmissions <- ElectricityEmissions %>% 
  mutate("HousingType" = EUIElectricity_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIElectricity_BAU$BBL) %>% 
  relocate(BBL)


# Add all emissions together
# UNITS: MTCO2/sqft/yr

BAUEmissionsSqft <- EUIbySource_BAU %>% 
  select("BBL", "HousingType")

BAUEmissionsSqft <- FuelOil2Emissions[, columns] + FuelOil4Emissions[, columns] + FuelOil56Emissions[, columns] +
  DistrictSteamEmissions[, columns] + NaturalGasEmissions[, columns] + ElectricityEmissions[, columns]

BAUEmissionsSqft <- BAUEmissionsSqft %>% 
  mutate("HousingType" = EUIbySource_BAU$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = EUIbySource_BAU$BBL) %>% 
  relocate(BBL)
