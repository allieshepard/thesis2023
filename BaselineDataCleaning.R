# Baseline Data Cleaning
# Script 1/10
# Run this script to load the necessary packages, import the baseline energy & emissions data,
# and clean and combine the data into one "Baseline" dataframe 

# Load packages ----------------------------------------------------------

library(tidyverse)
library(readr)
library(dplyr)
library(stargazer)
library(tidycensus)
library(tigris)
library(ggplot2)
library(tmap)
library(sf)
library(EnvStats)
library(reshape2)
library(gridExtra)
library(patchwork)
library(FinancialMath)
library(scales)
library(ggpubr)
library(gt)
library(coin)
library(colorspace)


# Load & Combine Baseline Energy and Emissions Data from Local Law 84 Reports -------

# Load LL84 Data 2017-2019
Baseline2017 <- read_csv('LL84Data/Energy_and_Water_Data_Disclosure_for_Local_Law_84_2018__Data_for_Calendar_Year_2017_.csv')
Baseline2018 <- read_csv('LL84Data/Energy_and_Water_Data_Disclosure_for_Local_Law_84_2019__Data_for_Calendar_Year_2018_.csv')
Baseline2019 <- read_csv('LL84Data/Energy_and_Water_Data_Disclosure_for_Local_Law_84_2020__Data_for_Calendar_Year_2019_.csv')

# Add year before column names
colnames(Baseline2017) <- paste("2017", colnames(Baseline2017), sep = "-")
colnames(Baseline2018) <- paste("2018", colnames(Baseline2018), sep = "-")
colnames(Baseline2019) <- paste("2019", colnames(Baseline2019), sep = "-")

# Create variable for emissions intensity (kgCO2e/sqft)
Baseline2017 <- Baseline2017 %>% 
  mutate(`2017-Total GHG Emissions Intensity (kgCO2e/ft²)` = 1000 * `2017-Total GHG Emissions (Metric Tons CO2e)` / `2017-Multifamily Housing - Gross Floor Area (ft²)`)
Baseline2018 <- Baseline2018 %>% 
  mutate(`2018-Total GHG Emissions Intensity (kgCO2e/ft²)` = 1000 * as.numeric(`2018-Total GHG Emissions (Metric Tons CO2e)`) / as.numeric(`2018-Multifamily Housing - Gross Floor Area (ft²)`))


# Combine 2017-2019 Data into one Data Frame
Baseline <- left_join(Baseline2017, Baseline2018, by = c("2017-Property Id" = "2018-Property Id"))
Baseline <- left_join(Baseline, Baseline2019, by = c("2017-Property Id" = "2019-Property Id"))

# Select columns to include in dataset
vars1 <- c("2017-Property Id","2019-NYC Borough, Block and Lot (BBL)", "2019-Primary Property Type - Self Selected", 
           "2019-Year Built","2019-Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 sq ft)" , 
           "2019-Multifamily Housing - Gross Floor Area (ft²)", "2019-Multifamily Housing - Number of Residential Living Units in a High-Rise Building (10 or more Stories)",
           "2019-Multifamily Housing - Number of Residential Living Units in a Mid-Rise Building (5-9 Stories)")
vars2 <- c("2017-Weather Normalized Site EUI (kBtu/ft²)","2017-Weather Normalized Site Energy Use (kBtu)","2017-Fuel Oil #1 Use (kBtu)","2017-Fuel Oil #2 Use (kBtu)","2017-Fuel Oil #4 Use (kBtu)","2017-Fuel Oil #5 & 6 Use (kBtu)","2017-Diesel #2 Use (kBtu)","2017-Kerosene Use (kBtu)","2017-Propane Use (kBtu)","2017-District Steam Use (kBtu)","2017-District Hot Water Use (kBtu)","2017-District Chilled Water Use (kBtu)","2017-Natural Gas Use (kBtu)","2017-Weather Normalized Site Natural Gas Use (therms)","2017-Electricity Use - Grid Purchase (kBtu)","2017-Electricity Use - Grid Purchase (kWh)","2017-Weather Normalized Site Electricity (kWh)","2017-Total GHG Emissions (Metric Tons CO2e)", "2017-Total GHG Emissions Intensity (kgCO2e/ft²)")
vars3 <- c("2018-Weather Normalized Site EUI (kBtu/ft²)","2018-Weather Normalized Site Energy Use (kBtu)","2018-Fuel Oil #1 Use (kBtu)","2018-Fuel Oil #2 Use (kBtu)","2018-Fuel Oil #4 Use (kBtu)","2018-Fuel Oil #5 & 6 Use (kBtu)","2018-Diesel #2 Use (kBtu)","2018-Kerosene Use (kBtu)","2018-Propane Use (kBtu)","2018-District Steam Use (kBtu)","2018-District Hot Water Use (kBtu)","2018-District Chilled Water Use (kBtu)","2018-Natural Gas Use (kBtu)","2018-Weather Normalized Site Natural Gas Use (therms)","2018-Electricity Use - Grid Purchase (kBtu)","2018-Electricity Use - Grid Purchase (kWh)","2018-Weather Normalized Site Electricity (kWh)","2018-Total GHG Emissions (Metric Tons CO2e)", "2018-Total GHG Emissions Intensity (kgCO2e/ft²)")
vars4 <- c("2019-Weather Normalized Site EUI (kBtu/ft²)","2019-Weather Normalized Site Energy Use (kBtu)","2019-Fuel Oil #1 Use (kBtu)","2019-Fuel Oil #2 Use (kBtu)","2019-Fuel Oil #4 Use (kBtu)","2019-Fuel Oil #5 & 6 Use (kBtu)","2019-Diesel #2 Use (kBtu)","2019-Kerosene Use (kBtu)","2019-Propane Use (kBtu)","2019-District Steam Use (kBtu)","2019-District Hot Water Use (kBtu)","2019-District Chilled Water Use (kBtu)","2019-Natural Gas Use (kBtu)","2019-Weather Normalized Site Natural Gas Use (therms)","2019-Electricity Use - Grid Purchase (kBtu)","2019-Electricity Use - Grid Purchase (kWh)","2019-Weather Normalized Site Electricity (kWh)","2019-Total GHG Emissions (Metric Tons CO2e)", "2019-Total GHG Emissions Intensity (kgCO2e/ft²)")

vars <- c(vars1,vars2,vars3,vars4)

Baseline <- Baseline %>% 
  select(all_of(vars))

# Rename variables that are not dependent on the year (Property Id, Units Density, BBL, Year Built, Property Type)
Baseline <- Baseline %>% 
  rename('Property Id' = '2017-Property Id')  %>% 
  rename("Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 ft²)" = "2019-Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 sq ft)") %>% 
  rename("BBL" = "2019-NYC Borough, Block and Lot (BBL)") %>% 
  rename("Year Built" = "2019-Year Built") %>% 
  rename("Primary Property Type - Self Selected" = "2019-Primary Property Type - Self Selected") %>% 
  rename("Multifamily Housing - Gross Floor Area (ft²)" = "2019-Multifamily Housing - Gross Floor Area (ft²)") %>% 
  rename("High Rise Units" = "2019-Multifamily Housing - Number of Residential Living Units in a High-Rise Building (10 or more Stories)") %>% 
  rename("Mid Rise Units" = "2019-Multifamily Housing - Number of Residential Living Units in a Mid-Rise Building (5-9 Stories)")

# Filter Property Type by "Multifamily Housing"
Baseline <- Baseline %>% 
  filter(`Primary Property Type - Self Selected` == "Multifamily Housing")

# Convert 2019 values from character to numeric
Baseline <- Baseline %>% 
  mutate_at(vars(contains("2019")), as.numeric)

# Create new density unit to be sqft per unit
Baseline$`Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 ft²)` <- 
  as.numeric(Baseline$`Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 ft²)`)

Baseline <- Baseline %>% 
  mutate("Multifamily Unit Density (sqft/unit)" = (1/`Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 ft²)`)*1000) %>% 
  relocate(`Multifamily Unit Density (sqft/unit)`, .after = `Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 ft²)`)

# Convert gross area metric to 1,000 sqft from sqft
Baseline$`Multifamily Housing - Gross Floor Area (ft²)` <- 
  as.numeric(Baseline$`Multifamily Housing - Gross Floor Area (ft²)`)

Baseline <- Baseline %>% 
  mutate(`Multifamily Housing - Gross Floor Area (1,000 ft²)` = `Multifamily Housing - Gross Floor Area (ft²)`/1000)


# Add census tract for each building --------------------------------------

# Import Census Tract data matched to BBLs
#  (from Census geocoder, which converts addresses to Census Tract)
BBLtoCensusTract <- read_csv('CensusBlockID/BBLtoCensusTract.csv')
BBLtoCensusTract <- BBLtoCensusTract %>% 
  select(c("Census Tract", "BBL"))

#BBLtoCensusTract$BBL <- as.numeric(BBLtoCensusTract$BBL)

# Convert BBLs to numeric
#BBLtoCensusTract$BBL <- as.numeric(BBLtoCensusTract$BBL)
#Baseline$BBL <- as.numeric(Baseline$BBL)

# Add census tract to baseline data, matched by Borough-Block-Lot (BBL) ID
Baseline <- Baseline %>% 
  left_join(BBLtoCensusTract, by = "BBL")


# Baseline Data Cleaning/Formatting ---------------------------------------

# Remove duplicate rows
Baseline <- Baseline %>% 
  distinct(BBL, .keep_all = TRUE)

# Filter out rows with missing data in any of the following columns:
# BBL
# 2017-Weather Normalized Site EUI (kBtu/ft²)
# 2017-Total GHG Emissions (Metric Tons CO2e)
# 2017-Total GHG Emissions Intensity (kgCO2e/ft²)
# 2018-Weather Normalized Site EUI (kBtu/ft²)
# 2018-Total GHG Emissions (Metric Tons CO2e)
# 2018-Total GHG Emissions Intensity (kgCO2e/ft²)
# 2019-Weather Normalized Site EUI (kBtu/ft²)
# 2019-Total GHG Emissions (Metric Tons CO2e)
# 2019-Total GHG Emissions Intensity (kgCO2e/ft²)
# Multifamily Housing - Gross Floor Area (ft2)
# Multifamily Housing - Gross Floor Area (1,000 ft²)
# Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 ft2)
# Multifamily Unit Density (sqft/unit)
Baseline <- Baseline %>% 
  filter_at(vars(BBL,`Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 ft²)`, `Multifamily Unit Density (sqft/unit)`, `Multifamily Housing - Gross Floor Area (ft²)`, `Multifamily Housing - Gross Floor Area (1,000 ft²)`, `2017-Weather Normalized Site EUI (kBtu/ft²)`,`2017-Total GHG Emissions (Metric Tons CO2e)`,`2018-Weather Normalized Site EUI (kBtu/ft²)`,`2018-Total GHG Emissions (Metric Tons CO2e)`,`2019-Weather Normalized Site EUI (kBtu/ft²)`,`2019-Total GHG Emissions (Metric Tons CO2e)`), all_vars(!is.na(.)))  %>% 
  filter(!grepl("Not Available",`2017-Weather Normalized Site EUI (kBtu/ft²)`)) %>% 
  filter(!grepl("Not Available",`2018-Weather Normalized Site EUI (kBtu/ft²)`)) %>% 
  filter(!grepl("Not Available",`2019-Weather Normalized Site EUI (kBtu/ft²)`)) %>% 
  filter(!grepl("Not Available",`2017-Total GHG Emissions (Metric Tons CO2e)`)) %>% 
  filter(!grepl("Not Available",`2018-Total GHG Emissions (Metric Tons CO2e)`)) %>% 
  filter(!grepl("Not Available",`2019-Total GHG Emissions (Metric Tons CO2e)`)) %>%
  filter(!grepl("Not Available",`2017-Total GHG Emissions Intensity (kgCO2e/ft²)`)) %>%
  filter(!grepl("Not Available",`2018-Total GHG Emissions Intensity (kgCO2e/ft²)`)) %>%
  filter(!grepl("Not Available",`2019-Total GHG Emissions Intensity (kgCO2e/ft²)`)) %>%
  filter(!grepl("Not Available",BBL))

# Filter out zeroes from the following columns: 
# 2017-Weather Normalized Site EUI (kBtu/ft²)
# 2017-Total GHG Emissions (Metric Tons CO2e)
# 2017-Total GHG Emissions Intensity (kgCO2e/ft²)
# 2018-Weather Normalized Site EUI (kBtu/ft²)
# 2018-Total GHG Emissions (Metric Tons CO2e)
# 2018-Total GHG Emissions Intensity (kgCO2e/ft²)
# 2019-Weather Normalized Site EUI (kBtu/ft²)
# 2019-Total GHG Emissions (Metric Tons CO2e)
# 2019-Total GHG Emissions Intensity (kgCO2e/ft²)
# Multifamily Housing - Gross Floor Area (ft2)
# Multifamily Housing - Gross Floor Area (1,000 ft²)
# Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 ft2)
# Multifamily Unit Density (sqft/unit)
Baseline <- Baseline %>% 
  filter_at(vars(`2017-Weather Normalized Site EUI (kBtu/ft²)`, `2017-Total GHG Emissions (Metric Tons CO2e)`, `2017-Total GHG Emissions Intensity (kgCO2e/ft²)`, `2018-Weather Normalized Site EUI (kBtu/ft²)`, `2018-Total GHG Emissions (Metric Tons CO2e)`, `2018-Total GHG Emissions Intensity (kgCO2e/ft²)`, `2019-Weather Normalized Site EUI (kBtu/ft²)`, `2019-Total GHG Emissions (Metric Tons CO2e)`, `2019-Total GHG Emissions Intensity (kgCO2e/ft²)`, `Multifamily Housing - Gross Floor Area (ft²)`, `Multifamily Housing - Gross Floor Area (1,000 ft²)`, `Multifamily Housing - Total Number of Residential Living Units Density (Number per 1,000 ft²)`, `Multifamily Unit Density (sqft/unit)`), all_vars(.>=0))

# Remove dashes from BBL in Baseline data
Baseline$BBL <- gsub("-", "", Baseline$BBL)

# Filter out Properties with multiple BBLs
Baseline <- Baseline %>% 
  filter(!grepl(";",BBL)) %>% 
  filter(!grepl(",",BBL)) %>% 
  filter(!grepl("/",BBL))

# Filter out buildings smaller than LL97 cutoff (25,000 sqft)
BuildingSizeCutoff <- 25000
Baseline <- Baseline %>% 
  filter(`Multifamily Housing - Gross Floor Area (ft²)` >= 25000)

