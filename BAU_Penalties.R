# BAU Penalties
# Script 6/10
# Run this script to calculate the penalties each building (and apartment) would pay under the BAU scenario and
# calculate the 'penalty burden' (median penalty divided by median rent at the census tract level)

# To see the penalty burden figures, uncomment the line below and run (after running the entire script)
# penaltyburdenplot

# LL97 penalty = $268/ton
LL97penalty <- 268

# 2019-2050 LL97 limits:
#   2019-2023 - none (approx as 100000)
#   2024-2029 - 6.75 MTCO2/sqft
#   2030-2034 - 3.34664 MTCO2/sqft
#   2035-2039 - 2.692183 MTCO2/sqft
#   2040-2050 - 2.052731 MTCO2/sqft
#   2050 - 0

LL97limits <- data.frame(100000,100000,100000,100000,100000,6.75,6.75,6.75,6.75,6.75,6.75,3.34664,3.34664,3.34664,3.34664,3.34664,
                         2.692183,2.692183,2.692183,2.692183,2.692183,2.052731,2.052731,2.052731,2.052731,
                         2.052731,2.052731,2.052731,2.052731,2.052731,2.052731,0)
colnames(LL97limits) = columns


# Calculate difference betweeen BAU emissions intensity and LL97 limits
# subtract LL97 limits from BAU emissions
BAUEmissionsOverLimit <- mapply("-", BAUEmissionsSqft[intersect(names(BAUEmissionsSqft), names(LL97limits))],
                                LL97limits[intersect(names(BAUEmissionsSqft), names(LL97limits))]) %>% 
  as.data.frame()

# replace negatives with zero
BAUEmissionsOverLimit[BAUEmissionsOverLimit<0] <- 0

BAUEmissionsOverLimit <- BAUEmissionsOverLimit %>% 
  mutate("HousingType" = BAUEmissionsSqft$HousingType) %>% 
  relocate(HousingType) %>% 
  mutate("BBL" = BAUEmissionsSqft$BBL) %>% 
  relocate(BBL)


# Calculate and plot penalties for each building 2019-2050 - BAU -------------------------

BAULL97PenaltiesBldg <- BAUEmissionsOverLimit %>% 
  left_join(BuildingSize, by = "BBL") %>% 
  rename(BldgArea = `Multifamily Housing - Gross Floor Area (ft²)`) %>% 
  relocate(c("BBL", "HousingType", "BldgArea", "Apartments"))

BAULL97PenaltiesBldg <- BAULL97PenaltiesBldg %>% 
  mutate(across(starts_with("2"),~.*BldgArea*LL97penalty/1000))

BAULL97PenaltiesBldg <- BAULL97PenaltiesBldg %>% 
  mutate("TotalPenalties_2024to2050" = rowSums(across(`2019`:`2050`), na.rm = TRUE))

# Calculate city-wide penalties
BAULL97Penalties <- BAULL97PenaltiesBldg %>%
  select(c("BBL",starts_with("2")))

BAULL97Penalties$BBL <- as.character(BAULL97Penalties$BBL)

BAULL97Penalties <- BAULL97Penalties %>% 
  mutate("TotalPenalties_2024to2050" = rowSums(across(`2019`:`2050`), na.rm = TRUE)) %>% 
  bind_rows(summarise(.,across(where(is.numeric),sum),
                      across(where(is.character), ~'Total'))) %>% 
  filter(BBL == "Total") %>% 
  select(starts_with("2"))

BAULL97Penalties <- BAULL97Penalties %>% 
  pivot_longer(`2019`:`2050`)

# Plot annual total penalties
pTotalPenalties <- ggplot(BAULL97Penalties, aes(x = name, y = value)) +
  geom_bar(stat = "identity", fill = "tomato1", alpha = 0.6) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(name = "Penalties Collected", 
                     labels = scales::dollar_format()) +
  xlab("Year") +
  ggtitle("Total Annual LL97 Penalties Collected by the City, Multifamily Buildings") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1))

# Calculate penalties per sqft

BAULL97PenaltiesSqft <- BAULL97PenaltiesBldg %>% 
  mutate(across(starts_with("2"),~./BldgArea))

# add column for undiscounted penalty total (2019-2050)
BAULL97PenaltiesSqft <- BAULL97PenaltiesSqft %>% 
  mutate("UndiscountedPenalties" = rowSums(across(starts_with("2")), na.rm = TRUE)) 


# Plot 2025 penalty histogram
p2025_BAU <- ggplot(BAULL97PenaltiesSqft, aes(x = `2025`)) + 
  geom_histogram(fill = "lightblue", binwidth = 0.02) +
  xlim(NA,2.5) +
  ylim(0,250) +
  xlab("2025 Penalty ($/sqft/yr)") +
  ggtitle("2025") +
  theme_classic()

# Plot 2030 penalty histogram
p2030_BAU <- ggplot(BAULL97PenaltiesSqft, aes(x = `2030`)) + 
  geom_histogram(fill = "lightgreen", binwidth = 0.02) +
  xlim(NA,2.5) +
  ylim(0,250) +
  xlab("2030 Penalty ($/sqft/yr)") +
  ggtitle("2030") +
  theme_classic()

# Plot 2035 penalty histogram
p2035_BAU <- ggplot(BAULL97PenaltiesSqft, aes(x = `2035`)) + 
  geom_histogram(fill = "sandybrown", binwidth = 0.02) +
  xlim(NA,2.5) +
  ylim(0,250) +
  xlab("2035 Penalty ($/sqft/yr)") +
  ggtitle("2035") +
  theme_classic()

# Plot 2040 penalty histogram
p2040_BAU <- ggplot(BAULL97PenaltiesSqft, aes(x = `2040`)) + 
  geom_histogram(fill = "plum", binwidth = 0.02) +
  xlim(0,2.5) +
  ylim(0,250) +
  xlab("2040 Penalty ($/sqft/yr)") +
  ggtitle("2040") +
  theme_classic()

# Plot 2045 penalty histogram
p2045_BAU <- ggplot(BAULL97PenaltiesSqft, aes(x = `2045`)) + 
  geom_histogram(fill = "pink", binwidth = 0.02) +
  xlim(0,2.5) +
  ylim(0,250) +
  xlab("2045 Penalty ($/sqft/yr)") +
  ggtitle("2045") +
  theme_classic()

# Plot 2050 penalty histogram
p2050_BAU <- ggplot(BAULL97PenaltiesSqft, aes(x = `2050`)) + 
  geom_histogram(fill = "khaki2", binwidth = 0.02) +
  xlim(0,2.5) +
  ylim(0,250) +
  xlab("2050 Penalty ($/sqft/yr)") +
  ggtitle("2050") +
  theme_classic()

BAUpenaltiesplot <- grid.arrange(p2025_BAU, p2030_BAU, p2035_BAU, p2040_BAU, p2045_BAU, p2050_BAU, nrow = 2,
             top = "Local Law 97 Penalties, BAU")

# Calculate penalties per apartment
BAULL97PenaltiesApt <- BAULL97PenaltiesBldg %>% 
  mutate(across(starts_with("2"),~./Apartments)) %>% 
  mutate(across(starts_with("T"),~./Apartments))

# Calculate average penalty by housing type
BAUPenaltiesApt_HousingType <- BAULL97PenaltiesApt %>%
  group_by(BAULL97PenaltiesApt$HousingType) %>%
  summarise(across(`2019`:`2050`, ~ mean(.x, na.rm = TRUE)))

# Calculate average apartment and building size
AptSize <- BAULL97PenaltiesBldg %>% 
  select("BldgArea", "Apartments") %>% 
  mutate("AptSqft" = BldgArea / Apartments)

AvgAptSize <- mean(AptSize$AptSqft)
AvgBldgSize <- mean(AptSize$BldgArea)

# Calculate number of buildings that will pay penalties in each year
PortionofPenalties <- as.data.frame(colSums(BAULL97PenaltiesBldg[5:(length(BAULL97PenaltiesBldg)-1)] != 0))
PortionofPenalties <- PortionofPenalties %>% 
  rename("Count" = "colSums(BAULL97PenaltiesBldg[5:(length(BAULL97PenaltiesBldg) - 1)] != 0)") %>% 
  mutate("Percent" = Count / nrow(BAULL97PenaltiesBldg)) %>% 
  mutate("Year" = rownames(PortionofPenalties)) %>% 
  relocate(Year, .before = Count)

# add average penalty per sqft (excluding zeros)
AvgPenalty <- BAULL97PenaltiesSqft[5:(length(BAULL97PenaltiesSqft)-2)] %>% 
  summarise_all(~mean(.[. != 0], na.rm = TRUE))

AvgPenalty <- t(AvgPenalty)

PortionofPenalties <- PortionofPenalties %>% 
  mutate("AvgPenalty" = AvgPenalty) %>% 
  mutate("AvgPenalty_Apt" = AvgPenalty * AvgAptSize) %>% 
  mutate("AvgPenalty_Bldg" = AvgPenalty * AvgBldgSize)

# Table of values
PortionofPenalties_table <- PortionofPenalties %>% 
  filter(Year == 2025 | Year == 2030 | Year == 2035 | Year == 2040 |
           Year == 2045 | Year == 2050) %>% 
  select("Year", "Percent", "AvgPenalty","AvgPenalty_Apt", "AvgPenalty_Bldg")

# Use ggtexttable to format table
PortionofPenalties_table <- ggtexttable(PortionofPenalties_table,
                                        rows = NULL,
                                        theme = ttheme("blank"))  %>%
  tab_add_hline(at.row = c(1:2), row.side = "top", linewidth = 2)


# Calculate penalties as % of energy costs, by sqft

BAUPenaltiestoEnergyCosts_Sqft <- cbind(BAULL97PenaltiesSqft[1:4],BAULL97PenaltiesSqft[,c("UndiscountedPenalties")]/
                                          BAUEnergyCostsSqft_Mid[,c("TotalCosts_2024to2050")])

BAUPenaltiestoEnergyCosts_Sqft <- BAUPenaltiestoEnergyCosts_Sqft %>% 
  rename("PenaltytoEnergyRatio" = "BAULL97PenaltiesSqft[, c(\"UndiscountedPenalties\")]/BAUEnergyCostsSqft_Mid[, ")


# Calculate Penalty Burden (penalties as fraction of rent)  --------

# add median household income & census tract GEOID to penalties dataframe
BBLtoCensusTract <- Baseline_Housing %>% 
  select(c("BBL","Census Tract"))

BAULL97PenaltiesApt <- BAULL97PenaltiesApt %>% 
  left_join(BBLtoCensusTract, by = "BBL") %>% 
  relocate(`Census Tract`, .after = BBL)

BAULL97PenaltiesApt$`Census Tract` <- as.character(BAULL97PenaltiesApt$`Census Tract`)

# Calculate median LL97 penalty by census tract

BAULL97PenaltiesApt_Tract <- BAULL97PenaltiesApt %>% 
  group_by(`Census Tract`) %>% 
  summarise(across(`2019`:`2050`, ~median(.x, na.rm = TRUE)))


# Import ACS data: median rent - ACS code B25031_001E
census_api_key("e6d87bc0016ad148328a76dc50f4c9b9521503c1")
NYC_medianrentall_tract <- get_acs(geography = "tract",
                                   state = 36,
                                   county = c(081, 061, 005, 047, 085),
                                   variables = "B25031_001E",
                                   year = 2019,
                                   geometry = TRUE)

NYC_medianrentall_tract <- NYC_medianrentall_tract %>% 
  rename("MedianRent" = "estimate")


BAULL97PenaltiesApt_Tract$`Census Tract` <- as.character(BAULL97PenaltiesApt_Tract$`Census Tract`)

BAULL97PenaltiesApt_Tract <- NYC_medianrentall_tract %>% 
  left_join(BAULL97PenaltiesApt_Tract, by = c("GEOID" = "Census Tract"))

# Calculate penalties as % of rent for each year, by census tract
MedianPenaltyBurden <- BAULL97PenaltiesApt_Tract %>% 
  mutate(across(starts_with("2"),~./(MedianRent*12)))

# Add NOAH designation to tract

NOAH_tract <- as.data.frame(NYC_medianrent_tract)

NOAH_tract <- NOAH_tract %>% 
  select(GEOID, NOAH_Identifier)

NOAH_tract$GEOID <- as.character(NOAH_tract$GEOID)

MedianPenaltyBurden <- MedianPenaltyBurden %>% 
  left_join(NOAH_tract, by = "GEOID")

MedianPenaltyBurden$NOAH_Identifier <- as.character(MedianPenaltyBurden$NOAH_Identifier)


# Plot 2030 penalty burden histogram
penaltyburden2030_plot <- ggplot(MedianPenaltyBurden, aes(x = `2030`,fill = NOAH_Identifier)) + 
  geom_density(alpha = 0.5, aes(y = ..scaled..), color = NA) +
  scale_fill_manual(values=c("#fb9a99", "#33a02c"),
                    name = "Tract Type", 
                    labels = c("Market Rate", "NOAH")) +
  scale_x_continuous(limits = c(0,0.15),
                     name = "Tract-level Median Penalty Burden",
                     labels = percent) +
  scale_y_continuous(name = "Density (scaled)") +
  ggtitle("2030 Penalty Burden") +
  theme_classic() +
  labs(fill = "")

# Plot 2040 penalty burden histogram
penaltyburden2040_plot <- ggplot(MedianPenaltyBurden, aes(x = `2040`,fill = NOAH_Identifier)) + 
  geom_density(alpha = 0.5, aes(y = ..scaled..), color = NA) +
  scale_fill_manual(values=c("#fb9a99", "#33a02c"),
                    name = "Tract Type", 
                    labels = c("Market Rate", "NOAH")) +
  scale_x_continuous(limits = c(0,0.15),
                     name = "Tract-level Median Penalty Burden",
                     labels = percent) +
  scale_y_continuous(name = "Density (scaled)") +
  ggtitle("2040 Penalty Burden") +
  theme_classic() +
  labs(fill = "")

# Plot 2050 penalty burden histogram
penaltyburden2050_plot <- ggplot(MedianPenaltyBurden, aes(x = `2050`,fill = NOAH_Identifier)) + 
  geom_density(alpha = 0.5, aes(y = ..scaled..), color = NA) +
  scale_fill_manual(values=c("#fb9a99", "#33a02c"),
                    name = "Tract Type", 
                    labels = c("Market Rate", "NOAH")) +
  scale_x_continuous(limits = c(0,0.15),
                     name = "Tract-level Median Penalty Burden",
                     labels = percent) +
  scale_y_continuous(name = "Density (scaled)") +
  ggtitle("2050 Penalty Burden") +
  theme_classic() +
  labs(fill = "")

# Plot 2030, 2040, and 2050 penalty burden curves together
penaltyburdenplot <- ggarrange(penaltyburden2030_plot, penaltyburden2040_plot, penaltyburden2050_plot, ncol = 3, common.legend = TRUE, legend = "bottom")


