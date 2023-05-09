# thesis2023
Data analysis &amp; figures for TPP master's thesis, titled: Understanding the Equity and Affordability Impacts of Building Performance Standards

I performed this data analysis in R (Version 4.1.1, 2021-08-10)/RStudio (Version 2023.03.0+336) on a MacBook Pro (Operating system macOS Monterey Version 12.6.2)

## Scrips & Data Inputs
The analysis occurs sequentially through 10 scripts, described below. 6 folders contain the data inputs for the analysis.

1. BaselineDataCleaning.R
This script loads the necessary packages, imports the baseline energy & emissions data, and cleans and combines the data into one "Baseline" dataframe 

2. BuildingCategories.R
This script creates the following labels for each building in the dataset:
  Covered by Local Law 97 (1 - yes, 0 - no)
  Housing Type (Subsidized, Naturally Occurring Affordable Housing (NOAH), Rent Regulated, or Market Rate)
  Age & Size (Pre-war, post-war, post-1980, low-rise, high-rise, very large)

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
  Plots of the EUI for a representative building to illustrate the three scenarios
  One figure for each scenario that combines energy cost savings, avoided penalties, and total cost savings in one figure
  
## Package Citations 
**dplyr** 
  Hadley Wickham, Romain François, Lionel Henry, Kirill Müller and Davis Vaughan (2023). dplyr: A
  Grammar of Data Manipulation. R package version 1.1.0. https://CRAN.R-project.org/package=dplyr
  
**tidyverse**
  Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L,
  Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu
  V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal
  of Open Source Software_, *4*(43), 1686. doi: 10.21105/joss.01686 (URL:
  https://doi.org/10.21105/joss.01686).
  
**ggplot2**
  H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
  
**readr**
  Hadley Wickham, Jim Hester and Jennifer Bryan (2023). readr: Read Rectangular Text Data. R
  package version 2.1.4. https://CRAN.R-project.org/package=readr
  
**stargazer**
  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables. R
  package version 5.2.3. https://CRAN.R-project.org/package=stargazer
  
**tidycensus**
  Kyle Walker and Matt Herman (2023). tidycensus: Load US Census Boundary and Attribute Data as
  'tidyverse' and 'sf'-Ready Data Frames. R package version 1.3.2.
  https://CRAN.R-project.org/package=tidycensus
  
**tigris**
  Kyle Walker (2023). tigris: Load Census TIGER/Line Shapefiles. R package version 2.0.1.
  https://CRAN.R-project.org/package=tigris
  
**sf**
  Pebesma, E., & Bivand, R. (2023). Spatial Data Science: With Applications in R (1st ed.).
  Chapman and Hall/CRC. https://doi.org/10.1201/9780429459016
  
**tmap**
  Tennekes M (2018). “tmap: Thematic Maps in R.” _Journal of Statistical Software_, *84*(6), 1-39.
  doi: 10.18637/jss.v084.i06 (URL: https://doi.org/10.18637/jss.v084.i06).
  
**EnvStats**
  Millard SP (2013). _EnvStats: An R Package for Environmental Statistics_. Springer, New York. ISBN
  978-1-4614-8455-4, <URL: https://www.springer.com>.
  
**reshape2**
  Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software,
  21(12), 1-20. URL http://www.jstatsoft.org/v21/i12/.
  
**gridExtra**
  Baptiste Auguie (2017). gridExtra: Miscellaneous Functions for "Grid" Graphics. R package
  version 2.3. https://CRAN.R-project.org/package=gridExtra
  
**patchwork**
  Thomas Lin Pedersen (2022). patchwork: The Composer of Plots. R package version 1.1.2.
  https://CRAN.R-project.org/package=patchwork
  
**FinancialMath**
  Kameron Penn and Jack Schmidt (2016). FinancialMath: Financial Mathematics for Actuaries. R
  package version 0.1.1. https://CRAN.R-project.org/package=FinancialMath
  
**scales**
  Hadley Wickham and Dana Seidel (2022). scales: Scale Functions for Visualization. R package
  version 1.2.1. https://CRAN.R-project.org/package=scales
  
**ggpubr**
  Alboukadel Kassambara (2023). ggpubr: 'ggplot2' Based Publication Ready Plots. R package version
  0.6.0. https://CRAN.R-project.org/package=ggpubr
  
**gt**
  Richard Iannone, Joe Cheng, Barret Schloerke, Ellis Hughes and JooYoung Seo (2022). gt: Easily
  Create Presentation-Ready Display Tables. R package version 0.8.0.
  https://CRAN.R-project.org/package=gt
  
**coin**
  Hothorn T, Hornik K, van de Wiel MA, Zeileis A (2006). “A Lego system for conditional inference.”
  _The American Statistician_, *60*(3), 257-263. doi: 10.1198/000313006X118430 (URL:
  https://doi.org/10.1198/000313006X118430).
**colorspace**
  Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2020).
  “colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.” _Journal of
  Statistical Software_, *96*(1), 1-49. doi: 10.18637/jss.v096.i01 (URL:
  https://doi.org/10.18637/jss.v096.i01).
