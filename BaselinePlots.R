# Baseline Plots
# Script 4/10
# Run this script to create boxplots of EUI and emissions intensities by housing and age+size categories,
# bar chart of baseline EUI and emissions by age+size category,
# and bar chart of housing type by age+size category

# To see the 3 figures, uncomment the lines below and run (after running the entire script)
# boxplot_eui_emissions
# percent_bycat_plot
# Housingtype_pct_plot


# Plot box plots of EUI and emissions intensity by building age/size category
EUI_agesize_boxplot <- ggplot(Baseline_Housing, aes(x = AgeandSizeCategory, y = BaselineEUI)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_format(8), limits = buildingtypepositions,
                   name = "Buiding Age and Size Category") +
  ggtitle("EUI by Building Age & Size Categories") +
  ylab("Baseline EUI (kBtu/sqft/yr)") +
  theme_classic()

Emissions_agesize_boxplot <- ggplot(Baseline_Housing, aes(x = AgeandSizeCategory, y = BaselineEmissionsIntensity)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_format(8), limits = buildingtypepositions,
                   name = "Buiding Age and Size Category") +
  ggtitle("Emissions Intensity by Building Age & Size Categories") +
  ylab("Baseline Emissions Intensity (kgCO2/sqft/yr)") +
  theme_classic()

# Plot box plots of EUI and emissions intensity by Housing Type
EUI_housing_boxplot <- ggplot(Baseline_Housing, aes(x = HousingType, y = BaselineEUI)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_format(8)) +
  ggtitle("EUI by Housing Type") +
  xlab("Housing Type") +
  ylab("Baseline EUI (kBtu/sqft/yr)") +
  theme_classic()

Emissions_housing_boxplot <- ggplot(Baseline_Housing, aes(x = HousingType, y = BaselineEmissionsIntensity)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_format(8)) +
  ggtitle("Emissions Intensity by Housing Type") +
  xlab("Housing Type") +
  ylab("Baseline Emissions Intensity (kgCO2/sqft/yr)") +
  theme_classic()

# all box plots together
boxplot_eui_emissions <- ggarrange(EUI_agesize_boxplot, Emissions_agesize_boxplot, 
          EUI_housing_boxplot, Emissions_housing_boxplot, 
          ncol = 2, nrow = 2)


# Dataframe summary of percent of buildings and total emissions by age & size category

NumBldgs <- nrow(Baseline_Housing)

percent_bycat <- Baseline_Housing %>% 
  group_by(AgeandSizeCategory) %>% 
  count() %>%
  rename("BldgCount" = "n") %>% 
  mutate("Percent of Total Buildings" = BldgCount/NumBldgs)

# Calculate total baseline emissions for all buildings
BaselineEmissions_total <- sum(Baseline_Housing$BaselineEmissions)

percent_emissions_bycat <- Baseline_Housing %>% 
  group_by(AgeandSizeCategory) %>% 
  summarise(Emissions = sum(BaselineEmissions)) %>% 
  mutate("Percent of Total Emissions" = Emissions / BaselineEmissions_total)

percent_bycat <- percent_bycat %>% 
  left_join(percent_emissions_bycat, by = "AgeandSizeCategory") %>% 
  select(c(AgeandSizeCategory,`Percent of Total Buildings`, `Percent of Total Emissions`))

percent_bycat <- melt(percent_bycat)

# Grouped bar chart of % of Bldgs and % of Emissions by Age & Size Category
percent_bycat_plot <- ggplot(percent_bycat, aes(x = AgeandSizeCategory, y = value,
                                                fill = variable)) +
  geom_bar(width = 0.7, position=position_dodge(width = 0.8), stat="identity") +
  scale_x_discrete(labels = wrap_format(8), limits = buildingtypepositions,
                   name = "Buiding Age and Size Category") +
  scale_y_continuous(labels = percent, name = "Percent",
                     limits = c(0,0.5)) +
  ggtitle("Percent of Total Buildings and Total Emissions by Age and Size Category") +
  scale_fill_manual(values = c("Percent of Total Buildings" = "#cccccc",
                               "Percent of Total Emissions" = "#636363"),
                    name = "") +
  theme_classic() +
  theme(legend.position = "bottom")

# Plot the number of buildings in each category, with stacked bars for Housing Type
Housingtype_pct_plot <- ggplot(Baseline_Housing, aes(x = AgeandSizeCategory, 
                                                     y = (..count..)/sum(..count..),
                                                     fill = HousingType)) +
  geom_bar(position = "fill") +
  scale_x_discrete(labels = wrap_format(8), limits = buildingtypepositions,
                   name = "Buiding Age and Size Category") +
  scale_y_continuous(labels = percent, name = "Percent of Buildings") +
  ggtitle("Housing Type Makeup of Age & Size Categories") +
  scale_fill_manual(values = c("MarketRate" = "#fb9a99", "NOAH" = "#33a02c",
                               "RentRegulated" = "#a6cee3", "Subsidized" = "#ff7f00"),
                    name = "Housing Type") +
  theme_classic()

