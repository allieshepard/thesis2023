# Building Categories
# Script 2/10
# Run this script to label each building in the baseline data set as:
# 1. Covered by Local Law 97 (1 - yes, 0 - no)
# 2. Housing Type (Subsidized, Naturally Occurring Affordable Housing (NOAH),
                  # Rent Regulated, or Market Rate)
# 3. Age & Size (Pre-war, post-war, post-1980, low-rise, high-rise, very large)


# Add LL97 Covered Buildings List Variable --------------------------------

# Import list of covered buildings
LL97CBL <- read_csv('LL97/LL97_CoveredBuildingsList.csv')

# Create indicator variable of 1s to signify the building is covered under LL97
LL97CBL <- LL97CBL %>% 
  mutate(CoveredBuilding = 1) %>% 
  select(c("10 Digit BBL", "CoveredBuilding"))

# Convert Baseline BBL to numeric
Baseline$BBL <- as.numeric(Baseline$BBL)

# Add Covered Buildings data to Baseline
Baseline <- Baseline %>% 
  left_join(LL97CBL, by = c("BBL"="10 Digit BBL"))

# Convert NAs to 0s
Baseline$CoveredBuilding <- Baseline$CoveredBuilding %>%
  replace_na(0)


# Add Subsidized Housing Data to Baseline Data ----------------------------

# Import subsidized housing dataset
subsidized_housing_data <- read_csv('FC_Subsidized_Housing_Database_2022-06-29/FC_SHD_subsidy_analysis_2022-06-29.csv')

# Select relevant columns
subsidies <- subsidized_housing_data %>% 
  select(ref_bbl, subsidy_name, sub_subsidy_name)

# Combine Baseline & Housing data sets, by BBL
Baseline_Housing <- left_join(Baseline,subsidies, by = c("BBL"="ref_bbl"))

# Convert data from character type to double type (excluding property id, property type, bbl (converted above))
i <- c(4:length(vars))
Baseline_Housing[ ,i] <- apply(Baseline_Housing[ ,i],2,function(x)as.numeric(x))

# Replace NA in subsidy with "No Subsidy"
Baseline_Housing$subsidy_name <- replace_na(Baseline_Housing$subsidy_name, "No Subsidy")
Baseline_Housing$sub_subsidy_name <- replace_na(Baseline_Housing$sub_subsidy_name, "No Subsidy")

# Create new column for general Subsidized/Not Subsidized classification
#   (0 if Not Subidized, 1 if Subsidized)
Baseline_Housing <- Baseline_Housing %>% 
  mutate(Subsidized = if_else(grepl("No Subsidy",subsidy_name),0,1))

# Identify Naturally Occurring Affordable Housing and add to Baseline Data -------------------------------------------

# Import ACS data: median gross rent by bedrooms - ACS code B25031
census_api_key("e6d87bc0016ad148328a76dc50f4c9b9521503c1")
NYC_medianrent0Br_tract <- get_acs(geography = "tract",
                                   state = 36,
                                   county = c(081, 061, 005, 047, 085),
                                   variables = "B25031_002E",
                                   year = 2019,
                                   geometry = TRUE)
NYC_medianrent1Br_tract <- get_acs(geography = "tract",
                                   state = 36,
                                   county = c(081, 061, 005, 047, 085),
                                   variables = "B25031_003E",
                                   year = 2019,
                                   geometry = TRUE)
NYC_medianrent2Br_tract <- get_acs(geography = "tract",
                                   state = 36,
                                   county = c(081, 061, 005, 047, 085),
                                   variables = "B25031_004E",
                                   year = 2019,
                                   geometry = TRUE)
NYC_medianrent3Br_tract <- get_acs(geography = "tract",
                                   state = 36,
                                   county = c(081, 061, 005, 047, 085),
                                   variables = "B25031_005E",
                                   year = 2019,
                                   geometry = TRUE)
NYC_medianrent4Br_tract <- get_acs(geography = "tract",
                                   state = 36,
                                   county = c(081, 061, 005, 047, 085),
                                   variables = "B25031_006E",
                                   year = 2019,
                                   geometry = TRUE)

# Rename columns to identify # of bedrooms
NYC_medianrent0Br_tract <- NYC_medianrent0Br_tract %>% 
  rename("0BrMedianRent" = "estimate")
NYC_medianrent1Br_tract <- NYC_medianrent1Br_tract %>% 
  rename("1BrMedianRent" = "estimate")
NYC_medianrent2Br_tract <- NYC_medianrent2Br_tract %>% 
  rename("2BrMedianRent" = "estimate")
NYC_medianrent3Br_tract <- NYC_medianrent3Br_tract %>% 
  rename("3BrMedianRent" = "estimate")
NYC_medianrent4Br_tract <- NYC_medianrent4Br_tract %>% 
  rename("4BrMedianRent" = "estimate")

# Convert to data frame
NYC_medianrent0Br_tract <- as.data.frame(NYC_medianrent0Br_tract)
NYC_medianrent1Br_tract <- as.data.frame(NYC_medianrent1Br_tract)
NYC_medianrent2Br_tract <- as.data.frame(NYC_medianrent2Br_tract)
NYC_medianrent3Br_tract <- as.data.frame(NYC_medianrent3Br_tract)
NYC_medianrent4Br_tract <- as.data.frame(NYC_medianrent4Br_tract)

# Merge all median rent data into one data frame
NYC_medianrent_tract <- full_join(x = NYC_medianrent0Br_tract, y = NYC_medianrent1Br_tract, by = "GEOID") %>% 
  full_join(NYC_medianrent2Br_tract, by = "GEOID") %>% 
  full_join(NYC_medianrent3Br_tract, by = "GEOID") %>% 
  full_join(NYC_medianrent4Br_tract, by = "GEOID") %>% 
  select(c("GEOID","NAME.x", "0BrMedianRent", 
           "1BrMedianRent", "2BrMedianRent",
           "3BrMedianRent", "4BrMedianRent","geometry.x")) %>% 
  rename("TractName" = "NAME.x", "geometry" = "geometry.x")

# NYC 2019 Fair Market Rents: 
# https://www.huduser.gov/portal/datasets/fmr/fmrs/FY2019_code/2019summary.odn
FMR0Br <- 1559
FMR1Br <- 1599
FMR2Br <- 1831
FMR3Br <- 2324
FMR4Br <- 2475

# Create new variables to identify if median rent is less than FMR by # of bedrooms
#   if value is N/A, assume BelowFMR = 0
NYC_medianrent_tract <- NYC_medianrent_tract %>% 
  mutate("BelowFMR0Br" = if_else(`0BrMedianRent` < FMR0Br, 1, 0, missing = 0)) %>% 
  mutate("BelowFMR1Br" = if_else(`1BrMedianRent` < FMR1Br, 1, 0, missing = 0)) %>% 
  mutate("BelowFMR2Br" = if_else(`2BrMedianRent` < FMR2Br, 1, 0, missing = 0)) %>% 
  mutate("BelowFMR3Br" = if_else(`3BrMedianRent` < FMR3Br, 1, 0, missing = 0)) %>% 
  mutate("BelowFMR4Br" = if_else(`4BrMedianRent` < FMR4Br, 1, 0, missing = 0))

# Create new variable to count how many of the BelowFMR metrics are true (max value is 5)
NYC_medianrent_tract <- NYC_medianrent_tract %>% 
  mutate("BelowFMRCount" = BelowFMR0Br + BelowFMR1Br + BelowFMR2Br + BelowFMR3Br + BelowFMR4Br)

# Move geometry to end
NYC_medianrent_tract <- NYC_medianrent_tract %>% 
  relocate(geometry, .after = last_col()) %>% 
  filter(!grepl("36085990100", GEOID)) #filter out empty geometry

# Convert to sf
NYC_medianrent_tract <-  st_as_sf(NYC_medianrent_tract)

# Plot Below FMR Counts
plot(NYC_medianrent_tract["BelowFMRCount"])

# Classify tract as NOAH if BelowFMRCount >= 4
NOAH_classification <- 4
NYC_medianrent_tract <- NYC_medianrent_tract %>% 
  mutate("NOAH_Identifier" = ifelse(BelowFMRCount >= NOAH_classification, 1, 0))

# Convert GEOID from char to numeric
NYC_medianrent_tract$GEOID <- as.numeric(NYC_medianrent_tract$GEOID)


tm_shape(NYC_medianrent_tract) +
  tm_polygons(col = "NOAH_Identifier", palette = "Greens") +
  tm_layout(title = "NOAH Census Tracts", legend.show = FALSE, frame = FALSE)

# Select only Census Tract (GEOID), NOAH_Identifier (and geometry will remain)
NYC_medianrent_tract <- NYC_medianrent_tract %>% 
  select(c("GEOID","NOAH_Identifier"))

# Add NOAH data to Baseline Data, match by Census Tract
Baseline_Housing <- Baseline_Housing %>% 
  left_join(NYC_medianrent_tract, by = c("Census Tract" = "GEOID"))

# Replace and NA with 0 for NOAH_Identifier
Baseline_Housing$NOAH_Identifier <- replace(Baseline_Housing$NOAH_Identifier, is.na(Baseline_Housing$NOAH_Identifier), 0)

# Change NOAH_Identifier to 0 if Subsidized = 1
Baseline_Housing <- Baseline_Housing %>% 
  mutate("NOAH" = if_else(Baseline_Housing$Subsidized == 1, 0, NOAH_Identifier))



# Add Rent Regulated Data -------------------------------------------------

# Load rent regulated data from DHCR
RentReg <- read_csv('RentRegulated/RentRegulatedBuildings_DHCR2020.csv')

# Add rent regulated column to Baseline data
Baseline_Housing <-  Baseline_Housing %>% 
  left_join(RentReg, by = "BBL")

# Convert NAs to 0s
Baseline_Housing$RentRegulated <- Baseline_Housing$RentRegulated %>%
  replace_na(0)

# If building is NOAH or subsidized, rent regulated = 0
Baseline_Housing$RentRegulated <- if_else(Baseline_Housing$NOAH == 1 | 
                                            Baseline_Housing$Subsidized == 1,
                                          0,Baseline_Housing$RentRegulated)


# Add columns to identify whether <=35% or >35% of units in building are rent regulated
# Per LL97, buildings with >35% rent regulated units must follow prescriptive path
# If a building is covered under LL97 and rent regulated, then <=35% of units are rent regulated
# If a building is not covered under LL97 and rent regulated, then >35% of units are rent regulated
Baseline_Housing <- Baseline_Housing %>% 
  mutate("RentRegUnder35Pct" = if_else(CoveredBuilding == 1 & RentRegulated == 1, 1, 0)) %>% 
  mutate("RentRegover35Pct" = if_else(CoveredBuilding == 0 & RentRegulated == 1, 1, 0))


# Add Housing Type Labels ----------------------------------------------

# Create Market Rate label
Baseline_Housing <- Baseline_Housing %>% 
  mutate("MarketRate" = if_else(Subsidized == 1 | NOAH == 1 | RentRegulated ==1, 0, 1)) %>% 
  relocate(MarketRate, .after = RentRegulated)

# Create Housing Type Label
Baseline_Housing <- Baseline_Housing %>% 
  mutate("HousingType" = if_else(Subsidized ==1, "Subsidized",
                                 if_else(NOAH ==1, "NOAH", if_else(RentRegulated ==1, "RentRegulated",
                                                                   if_else(MarketRate == 1,
                                                                           "MarketRate","Error"))))) %>% 
  relocate(HousingType, .after = MarketRate)

# Data Cleaning -----------------------------------------------------------

# Remove duplicates from Baseline_Housing
# Remove duplicate rows
Baseline_Housing <- Baseline_Housing %>% 
  distinct(BBL, .keep_all = TRUE)


# Add Building Age & Size labels ------------------------------------------

# Building age categories:
# Pre-War: pre-1946
# Post-War: 1946-1980
# Post-1980: 1981-present

pre_war_low <- 0
pre_war_high <- 1946
post_war_low <- pre_war_high
post_war_high <- 1980
post_1980_low <- post_war_high
post_1980_high <- 2023

Baseline_Housing <- Baseline_Housing %>% 
  mutate("AgeCategory" = if_else(`Year Built` >= pre_war_low & `Year Built` < pre_war_high, "Pre-War",
                                 if_else(`Year Built` >= post_war_low & `Year Built` <= post_war_high, "Post-War",
                                         if_else(`Year Built` > post_1980_low & `Year Built` <= post_1980_high, "Post-1980",
                                                 "Error")))) %>% 
  relocate(AgeCategory, .before = `Year Built`)

# Building size categories
# Low-rise: <500k sqft & <10 floors
# High-rise: <500k sqft & >10 floors
# Very large: >500k sqft

sizelimit <- 500000

# If buildings are smaller than 500k sqft but do not have any data in Mid Rise Units and High Rise Units columns, classify as Low-Rise
Baseline_Housing <- Baseline_Housing %>% 
  mutate("SizeCategory" = if_else(`Multifamily Housing - Gross Floor Area (ft²)` >= sizelimit, "Very Large",
                                  if_else(`Multifamily Housing - Gross Floor Area (ft²)` < sizelimit & `High Rise Units` >0, "High-Rise",
                                          if_else(`Multifamily Housing - Gross Floor Area (ft²)` < sizelimit & `Mid Rise Units` >= 0, "Low-Rise",
                                                  "Error")))) %>% 
  relocate(SizeCategory, .after = AgeCategory)

# Create combined label for age+size
Baseline_Housing$AgeandSizeCategory <- paste(Baseline_Housing$AgeCategory, Baseline_Housing$SizeCategory, sep = ", ")
Baseline_Housing <- Baseline_Housing %>% 
  relocate(AgeandSizeCategory, .after = SizeCategory)

#specify order of building types in charts
buildingtypepositions <- c("Pre-War, Low-Rise", "Pre-War, High-Rise", "Pre-War, Very Large",
                           "Post-War, Low-Rise", "Post-War, High-Rise", "Post-War, Very Large",
                           "Post-1980, Low-Rise", "Post-1980, High-Rise", "Post-1980, Very Large")

