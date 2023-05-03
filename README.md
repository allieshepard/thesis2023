# thesis2023
Data analysis &amp; figures for TPP master's thesis, titled: XXXXX

The analysis occurs sequentially through 10 scripts, described below. 6 folders contain the data inputs for the analysis.

1. BaselineDataCleaning.R
This script loads the necessary packages, imports the baseline energy & emissions data, and cleans and combines the data into one "Baseline" dataframe 

2. BuildingCategories.R
Thus script creates the following labels for each building in the dataset:
  1. Covered by Local Law 97 (1 - yes, 0 - no)
  2. Housing Type (Subsidized, Naturally Occurring Affordable Housing (NOAH), Rent Regulated, or Market Rate)
  3. Age & Size (Pre-war, post-war, post-1980, low-rise, high-rise, very large)

3. BaselineEUIandEmissions.R
This script calculates the baseline EUI and emissions intensity for each building, removes outliers, runs regressions, and analyzes the fuel types used in buildings

4. BaselinePlots.R
This script creates boxplots of EUI and emissions intensities by housing and age+size categories, a bar chart of baseline EUI and emissions by age+size category, and a bar chart of housing type by age+size category

5. BAU_EnergyCosts_Emissions.R
This script calculates the energy costs for each building in the Business as Usual (BAU) scenario and calculates the BAU emissions for each building

6. BAU_Penalties.R
This script calculates the penalties each building (and apartment) would pay under the BAU scenario and calculates the 'penalty burden' (median penalty divided by median rent at the census tract level)

7. Scenario1.R
This script models Scenario 1 - Gradual EUI Reductions - by calculating the EUI, energy costs, emissions, and penalties for all buildings through 2050, and comparing these to the BAU to calculate the energy cost savings, avoided penalties, and total cost savings. This script also calculates the Net Present Value (NPV) of each of these metrics under 3 discount rate scenarios (2%, 5%, 8%)

8. Scenario2.R
This script models Scenario 2 - Fully Electric in 2024 - by calculating the EUI, energy costs, emissions, and penalties for all buildings through 2050, and comparing these to the BAU to calculate the energy cost savings, avoided penalties, and total cost savings. This script also calculates the Net Present Value (NPV) of each of these metrics under 3 discount rate scenarios (2%, 5%, 8%)

9. Scenario3.R
This script models Scenario 3 - Fully Electric Passive House in 2024 - by calculating the EUI, energy costs, emissions, and penalties for all buildings through 2050, and comparing these to the BAU to calculate the energy cost savings, avoided penalties, and total cost savings. This script also calculates the Net Present Value (NPV) of each of these metrics under 3 discount rate scenarios (2%, 5%, 8%)

10. ScenarioOutputs.R
This script creates the following figures:
  1. plots of the EUI for a representative building to illustrate the three scenarios
  2. one figure for each scenario that combines energy cost savings, avoided penalties, and total cost savings in one figure
