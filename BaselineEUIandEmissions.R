# Baseline EUI and Emissions
# Script 3/10
# Run this script to calculate the baseline EUI and emissions intensity for each building, 
# remove outliers, run regressions, and analyze the fuel types used in buildings

# Calculate Baseline Avg EUI and Emissions --------------------------------

# Create columns with Average EUI, Emissions Intensity, and Total Emissions
Baseline_Housing <- Baseline_Housing %>% 
  mutate(BaselineEUI = (`2017-Weather Normalized Site EUI (kBtu/ft²)`+`2018-Weather Normalized Site EUI (kBtu/ft²)`+ `2019-Weather Normalized Site EUI (kBtu/ft²)`)/3) %>% 
  mutate(BaselineEmissionsIntensity = (`2017-Total GHG Emissions Intensity (kgCO2e/ft²)`+`2018-Total GHG Emissions Intensity (kgCO2e/ft²)`+`2019-Total GHG Emissions Intensity (kgCO2e/ft²)`)/3) %>% 
  mutate(BaselineEmissions = BaselineEmissionsIntensity * `Multifamily Housing - Gross Floor Area (ft²)`)

# Remove outliers

#find Q1, Q3, and interquartile range for EUI
Q1_EUI <- quantile(Baseline_Housing$BaselineEUI, .25)
Q3_EUI <- quantile(Baseline_Housing$BaselineEUI, .75)
IQR_EUI <- IQR(Baseline_Housing$BaselineEUI)
#only keep EUI rows that have values within 1.5*IQR of Q1 and Q3
Baseline_Housing <- Baseline_Housing %>% 
  filter(BaselineEUI > (Q1_EUI - 1.5*IQR_EUI) & BaselineEUI < (Q3_EUI + 1.5*IQR_EUI))

#find Q1, Q3, and interquartile range for Emissions
Q1_EmissionsIntensity <- quantile(Baseline_Housing$BaselineEmissionsIntensity, .25)
Q3_EmissionsIntensity <- quantile(Baseline_Housing$BaselineEmissionsIntensity, .75)
IQR_EmissionsIntensity <- IQR(Baseline_Housing$BaselineEmissionsIntensity)
#only keep EUI rows that have values within 1.5*IQR of Q1 and Q3
Baseline_Housing <- Baseline_Housing %>% 
  filter(BaselineEmissionsIntensity > (Q1_EmissionsIntensity - 1.5*IQR_EmissionsIntensity) & BaselineEmissionsIntensity < (Q3_EmissionsIntensity + 1.5*IQR_EmissionsIntensity))



# Baseline EUI and Emissions Regressions ----------------------------------

# EUI Regressions
R1 <- lm(Baseline_Housing$BaselineEUI~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated, data = Baseline_Housing)
R2 <- lm(Baseline_Housing$BaselineEUI~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built`, data = Baseline_Housing)  
R3 <- lm(Baseline_Housing$BaselineEUI~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)`, data = Baseline_Housing)
R4 <- lm(Baseline_Housing$BaselineEUI~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)
R5 <- lm(Baseline_Housing$BaselineEUI~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built` + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)` + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)

# create regression tables
stargazer(R1, R2, R3, R4, R5, type = "text")

stargazer(R1, R2, R3, R4, R5, 
          type = "html",
          digits = 2, 
          star.char = c("*","**","***"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Energy Use Intensity (kBtu/sqft)"),
          covariate.labels = c("Subsidized", "NOAH","Rent Regulated", "Year Built", "Gross Floor Area (1,000 sqft)", "Residential Units Density (sqft/unit)"),
          out = "BaselineEUI_regression.htm")

# Emissions Regressions
R6 <- lm(Baseline_Housing$BaselineEmissionsIntensity~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated, data = Baseline_Housing)
R7 <- lm(Baseline_Housing$BaselineEmissionsIntensity~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built`, data = Baseline_Housing)  
R8 <- lm(Baseline_Housing$BaselineEmissionsIntensity~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)`, data = Baseline_Housing)
R9 <- lm(Baseline_Housing$BaselineEmissionsIntensity~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)
R10 <- lm(Baseline_Housing$BaselineEmissionsIntensity~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built` + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)` + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)

# create regression tables
stargazer(R6, R7, R8, R9, R10, type = "text")

stargazer(R6, R7, R8, R9, R10, 
          type = "html",
          digits = 2,
          star.char = c("*","**","***"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Emissions (kgCO2e/sqft)"),
          covariate.labels = c("Subsidized", "NOAH", "Rent Regulated", "Year Built", "Gross Floor Area (1,000 sqft)", "Residential Units Density (sqft/unit)"),
          out = "BaselineEmissions_regression.htm")

# regression table with EUI and Emissions together
stargazer(R5, R10, type = "text") 

stargazer(R5, R10,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Energy Use Intensity (kBtu/sqft/yr)", "Emissions Intensity (kgCO2e/sqft/yr)"),
          covariate.labels = c("Subsidized", "NOAH", "Rent Regulated", "Year Built", "Gross Floor Area (1,000 sqft)", "Residential Units Density (sqft/unit)"),
          model.numbers = FALSE,
          out = "BaselineEUIandEmissions_regression.htm")

# Create columns for average baseline energy use by fuel type -------------

# Replace NAs with 0
Baseline_Housing <- Baseline_Housing %>% 
  replace(is.na(.),0)

# Create new vector for each energy type, calculate baseline fuel use intensity as average of 2017-2019 fuel usage divided by gross floor area
Baseline_Housing <- Baseline_Housing %>% 
  mutate(BaselineFuelOil1_kBtupersqft = (`2017-Fuel Oil #1 Use (kBtu)` + `2018-Fuel Oil #1 Use (kBtu)` + `2019-Fuel Oil #1 Use (kBtu)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`)) %>% 
  mutate(BaselineFuelOil2_kBtupersqft = (`2017-Fuel Oil #2 Use (kBtu)` + `2018-Fuel Oil #2 Use (kBtu)`  + `2019-Fuel Oil #2 Use (kBtu)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`)) %>% 
  mutate(BaselineFuelOil4_kBtupersqft = (`2017-Fuel Oil #4 Use (kBtu)` + `2018-Fuel Oil #4 Use (kBtu)` + `2019-Fuel Oil #4 Use (kBtu)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`)) %>% 
  mutate(BaselineFuelOil56_kBtupersqft = (`2017-Fuel Oil #5 & 6 Use (kBtu)` + `2018-Fuel Oil #5 & 6 Use (kBtu)` + `2019-Fuel Oil #5 & 6 Use (kBtu)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`)) %>%
  mutate(BaselineDiesel2_kBtupersqft = (`2017-Diesel #2 Use (kBtu)` + `2018-Diesel #2 Use (kBtu)` + `2019-Diesel #2 Use (kBtu)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`)) %>% 
  mutate(BaselineKerosene_kBtupersqft = (`2017-Kerosene Use (kBtu)` + `2018-Kerosene Use (kBtu)` + `2019-Kerosene Use (kBtu)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`)) %>% 
  mutate(BaselinePropane_kBtupersqft = (`2017-Propane Use (kBtu)` + `2018-Propane Use (kBtu)` + `2019-Propane Use (kBtu)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`)) %>% 
  mutate(BaselineDistrictSteam_kBtupersqft = (`2017-District Steam Use (kBtu)` + `2018-District Steam Use (kBtu)` + `2019-District Steam Use (kBtu)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`)) %>% 
  mutate(BaselineDistrictHotWater_kBtupersqft = (`2017-District Hot Water Use (kBtu)` + `2018-District Hot Water Use (kBtu)` + `2019-District Hot Water Use (kBtu)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`)) %>% 
  mutate(BaselineDistrictChilledWater_kBtupersqft = (`2017-District Chilled Water Use (kBtu)` + `2018-District Chilled Water Use (kBtu)` + `2019-District Chilled Water Use (kBtu)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`)) %>% 
  mutate(BaselineNaturalGas_WeatherNorm_thermpersqft = (`2017-Weather Normalized Site Natural Gas Use (therms)` + `2018-Weather Normalized Site Natural Gas Use (therms)` + `2019-Weather Normalized Site Natural Gas Use (therms)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`)) %>% 
  mutate(BaselineElectricity_WeatherNorm_kWhpersqft = (`2017-Weather Normalized Site Electricity (kWh)` + `2018-Weather Normalized Site Electricity (kWh)` + `2019-Weather Normalized Site Electricity (kWh)`)/(3 * `Multifamily Housing - Gross Floor Area (ft²)`))

# Convert Natural Gas and Electricity to kBtu
# Conversions
thermstokBtu <- 100
kWhtokBtu <- 3.412

Baseline_Housing <- Baseline_Housing %>% 
  mutate(BaselineNaturalGas_kBtupersqft = BaselineNaturalGas_WeatherNorm_thermpersqft * thermstokBtu) %>% 
  mutate(BaselineElectricity_kBtupersqft = BaselineElectricity_WeatherNorm_kWhpersqft * kWhtokBtu)

# Regressions by fuel type for fuel oil 2, natural gas, electricity

#Fuel Oil 2
R11 <- lm(Baseline_Housing$BaselineFuelOil2_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated, data = Baseline_Housing)
R12 <- lm(Baseline_Housing$BaselineFuelOil2_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built`, data = Baseline_Housing)  
R13 <- lm(Baseline_Housing$BaselineFuelOil2_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)`, data = Baseline_Housing)
R14 <- lm(Baseline_Housing$BaselineFuelOil2_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)
R15 <- lm(Baseline_Housing$BaselineFuelOil2_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built` + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)` + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)

# create regression tables
stargazer(R11, R12, R13, R14, R15, type = "text")

stargazer(R11, R12, R13, R14, R15, 
          type = "html",
          digits = 2, 
          star.char = c("*","**","***"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Fuel Oil 2 Use Intensity (kBtu/sqft)"),
          covariate.labels = c("Subsidized", "NOAH", "Rent Regulated", "Year Built", "Gross Floor Area (1,000 sqft)", "Residential Units Density (sqft/unit)"),
          out = "BaselineFuelOil2_regression.htm")

# Natural Gas
R16 <- lm(Baseline_Housing$BaselineNaturalGas_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated, data = Baseline_Housing)
R17 <- lm(Baseline_Housing$BaselineNaturalGas_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built`, data = Baseline_Housing)  
R18 <- lm(Baseline_Housing$BaselineNaturalGas_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)`, data = Baseline_Housing)
R19 <- lm(Baseline_Housing$BaselineNaturalGas_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)
R20 <- lm(Baseline_Housing$BaselineNaturalGas_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built` + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)` + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)

# create regression tables
stargazer(R16, R17, R18, R19, R20, type = "text")

stargazer(R16, R17, R18, R19, R20, 
          type = "html",
          digits = 2, 
          star.char = c("*","**","***"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Natural Gas Use Intensity (kBtu/sqft)"),
          covariate.labels = c("Subsidized", "NOAH", "Rent Regulated", "Year Built", "Gross Floor Area (1,000 sqft)", "Residential Units Density (sqft/unit)"),
          out = "BaselineNaturalGas_regression.htm")

#Electricity
R21 <- lm(Baseline_Housing$BaselineElectricity_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated, data = Baseline_Housing)
R22 <- lm(Baseline_Housing$BaselineElectricity_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built`, data = Baseline_Housing)  
R23 <- lm(Baseline_Housing$BaselineElectricity_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)`, data = Baseline_Housing)
R24 <- lm(Baseline_Housing$BaselineElectricity_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)
R25 <- lm(Baseline_Housing$BaselineElectricity_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built` + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)` + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)

# create regression tables
stargazer(R21, R22, R23, R24, R25, type = "text")

stargazer(R21, R22, R23, R24, R25, 
          type = "html",
          digits = 2, 
          star.char = c("*","**","***"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Electricity Use Intensity (kBtu/sqft)"),
          covariate.labels = c("Subsidized", "NOAH", "Rent Regulated", "Year Built", "Gross Floor Area (1,000 sqft)", "Residential Units Density (sqft/unit)"),
          out = "BaselineElectricity_regression.htm")


# District Steam
R26 <- lm(Baseline_Housing$BaselineDistrictSteam_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated, data = Baseline_Housing)
R27 <- lm(Baseline_Housing$BaselineDistrictSteam_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built`, data = Baseline_Housing)  
R28 <- lm(Baseline_Housing$BaselineDistrictSteam_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)`, data = Baseline_Housing)
R29 <- lm(Baseline_Housing$BaselineDistrictSteam_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)
R30 <- lm(Baseline_Housing$BaselineDistrictSteam_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built` + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)` + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)

# create regression tables
stargazer(R26, R27, R28, R29, R30, type = "text")

stargazer(R26, R27, R28, R29, R30, 
          type = "html",
          digits = 2, 
          star.char = c("*","**","***"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("District Steam Use Intensity (kWh/sqft)"),
          covariate.labels = c("Subsidized", "NOAH", "Rent Regulated", "Year Built", "Gross Floor Area (1,000 sqft)", "Residential Units Density (sqft/unit)"),
          out = "BaselineDistrictSteam_regression.htm")

#Fuel Oil 4
R31 <- lm(Baseline_Housing$BaselineFuelOil4_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated, data = Baseline_Housing)
R32 <- lm(Baseline_Housing$BaselineFuelOil4_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built`, data = Baseline_Housing)  
R33 <- lm(Baseline_Housing$BaselineFuelOil4_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)`, data = Baseline_Housing)
R34 <- lm(Baseline_Housing$BaselineFuelOil4_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)
R35 <- lm(Baseline_Housing$BaselineFuelOil4_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built` + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)` + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)

# create regression tables
stargazer(R31, R32, R33, R34, R35, type = "text")

stargazer(R11, R12, R13, R14, R15, 
          type = "html",
          digits = 2, 
          star.char = c("*","**","***"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Fuel Oil 4 Use Intensity (kBtu/sqft)"),
          covariate.labels = c("Subsidized", "NOAH", "Rent Regulated", "Year Built", "Gross Floor Area (1,000 sqft)", "Residential Units Density (sqft/unit)"),
          out = "BaselineFuelOil4_regression.htm")

#Fuel Oil 5/6
R36 <- lm(Baseline_Housing$BaselineFuelOil56_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated, data = Baseline_Housing)
R37 <- lm(Baseline_Housing$BaselineFuelOil56_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built`, data = Baseline_Housing)  
R38 <- lm(Baseline_Housing$BaselineFuelOil56_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)`, data = Baseline_Housing)
R39 <- lm(Baseline_Housing$BaselineFuelOil56_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)
R40 <- lm(Baseline_Housing$BaselineFuelOil56_kBtupersqft~Baseline_Housing$Subsidized + Baseline_Housing$NOAH + Baseline_Housing$RentRegulated + Baseline_Housing$`Year Built` + Baseline_Housing$`Multifamily Housing - Gross Floor Area (1,000 ft²)` + Baseline_Housing$`Multifamily Unit Density (sqft/unit)`, data = Baseline_Housing)

# create regression tables
stargazer(R36, R37, R38, R39, R40, type = "text")

stargazer(R36, R37, R38, R39, R40, 
          type = "html",
          digits = 2, 
          star.char = c("*","**","***"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Fuel Oil 5/6 Use Intensity (kBtu/sqft)"),
          covariate.labels = c("Subsidized", "NOAH", "Rent Regulated", "Year Built", "Gross Floor Area (1,000 sqft)", "Residential Units Density (sqft/unit)"),
          out = "BaselineFuelOil56_regression.htm")


# Create table with dependent variables as fuel types, only include regression with all factors as controls 
stargazer(R15, R35, R40, R20, R25, R30)

stargazer(R15, R35, R40, R20, R25, R30, 
          type = "html",
          digits = 2, 
          star.char = c("*","**","***"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Fuel Oil 2","Fuel Oil 4", "Fuel Oil 5/6", "Natural Gas", "Electricity", "District Steam"),
          covariate.labels = c("Subsidized", "NOAH", "Rent Regulated", "Year Built", "Gross Floor Area (1,000 sqft)", "Residential Units Density (sqft/unit)"),
          title = "Regressions: Fuel Type Usage (kBtu/sqft/yr) by Housing Type",
          model.numbers = FALSE,
          out = "BaselineFuelType_regression.htm")

# Fuel Type Analysis ------------------------------------------------------

# create data frame for total energy use by fuel type
BaselineFuelTypes <- Baseline_Housing %>% 
  select(c(AgeandSizeCategory, `Multifamily Housing - Gross Floor Area (ft²)`, BaselineFuelOil2_kBtupersqft,
           BaselineFuelOil4_kBtupersqft, BaselineFuelOil56_kBtupersqft, BaselineDistrictSteam_kBtupersqft,
           BaselineNaturalGas_kBtupersqft, BaselineElectricity_kBtupersqft)) %>% 
  mutate(across(contains("kBtu"),~.*`Multifamily Housing - Gross Floor Area (ft²)`)) %>% 
  select(c(AgeandSizeCategory, BaselineFuelOil2_kBtupersqft,
           BaselineFuelOil4_kBtupersqft, BaselineFuelOil56_kBtupersqft, BaselineDistrictSteam_kBtupersqft,
           BaselineNaturalGas_kBtupersqft, BaselineElectricity_kBtupersqft)) %>% 
  rename("Fuel Oil 2" = "BaselineFuelOil2_kBtupersqft") %>% 
  rename("Fuel Oil 4" = "BaselineFuelOil4_kBtupersqft") %>% 
  rename("Fuel Oil 5/6" = "BaselineFuelOil56_kBtupersqft") %>% 
  rename("District Steam" = "BaselineDistrictSteam_kBtupersqft") %>% 
  rename("Natural Gas" = "BaselineNaturalGas_kBtupersqft") %>% 
  rename("Electricity" = "BaselineElectricity_kBtupersqft")

# convert to long form
BaselineFuelTypes <- melt(BaselineFuelTypes, id = "AgeandSizeCategory")
BaselineFuelTypes <- BaselineFuelTypes %>% 
  rename("kBtu" = "value") %>% 
  rename("Fuel Type" = "variable")

# calculate total energy use
BaselineEnergy_total <- sum(BaselineFuelTypes$kBtu)


# plot bar graph of energy use by building type w/ stacks by fuel type
fuelbycat_plot <- ggplot(BaselineFuelTypes, aes(x = AgeandSizeCategory, 
                                                y = (kBtu)/(BaselineEnergy_total),
                                                fill = `Fuel Type`)) +
  geom_bar(stat = "sum", position = "fill") +
  scale_x_discrete(labels = wrap_format(8), limits = buildingtypepositions,
                   name = "Buiding Age and Size Category") +
  scale_y_continuous(labels = percent, name = "Percent of Total Site Energy Use") +
  scale_fill_manual(values = c("Fuel Oil 2" = "#fee5d9", "Fuel Oil 4" = "#fcae91",
                               "Fuel Oil 5/6" = "#fb9a99", "District Steam" = "#ff7f00",
                               "Natural Gas" = "#a6cee3", "Electricity" =  "#33a02c")) +
  ggtitle("Fuel Type by Building Age and Size Categories") +
  guides(size = FALSE) +
  theme_classic() 

# Pie chart of fuel type for all buildings
BaselineFuelTypes_total <- BaselineFuelTypes %>% 
  group_by(`Fuel Type`) %>% 
  summarise(totalkBtu = sum(kBtu))

fuelbycat_pie <- ggplot(BaselineFuelTypes_total, aes(x = "", y = totalkBtu, 
                                                     fill = `Fuel Type`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Fuel Type") +
  scale_fill_manual(values = c("Fuel Oil 2" = "#fee5d9", "Fuel Oil 4" = "#fcae91",
                               "Fuel Oil 5/6" = "#fb9a99", "District Steam" = "#ff7f00",
                               "Natural Gas" = "#a6cee3", "Electricity" =  "#33a02c")) +
  theme_void() +
  ggtitle("Total Site Energy Use by Fuel Type")

