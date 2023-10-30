### Modeled Flow Data for the Reorienting to Recovery DSM models 

*This data package contains modeled flow and diversion data for each of the watersheds within the R2R Salmon Life Cycle Model. The R2R Salmon Life Cycle Models are derived and adapted from the CVPIA SIT DSMs.*

#### Installation

``` r
# install.packages("remotes")
remotes::install_github("Reorienting-to-Recovery/DSMFlow")
```

#### Usage

This package provides flow related datasets to the [`fallRunDSM,`](https://github.com/Reorienting-to-Recovery/fallRunDSM) [`springRunDSM,`](https://github.com/Reorienting-to-Recovery/springRunDSM) [`winterRunDSM,`](https://github.com/Reorienting-to-Recovery/winterRunDSM).

``` r
# datasets within the package
data(package = 'DSMflow')

# explore CALSIM II modeled flow mapped to tributaries metadata
?DSMflow::flows_cfs
```

#### About the Models

Output from the CALSIM II operational model is used to generate hydrologic inputs for the CVPIA Decision Support Model (DSM). CALSIM II is the model used to simulate California State Water Project (SWP) / Central Valley Project (CVP) operations. CALSIM II was developed in the Water Resource Integrated Modeling System (WRIMS model engine or WRIMS), a generalized water resources modeling system for evaluating operational alternatives of large, complex river basins.

Data objects in this package contain information from a CalSim II run from 2009 and a CalSim II 
run from 2019, and an exploratory Run of River CalSim run. All datasets produced using CalSim outputs are filtered to the years 1980-2000 to match the timeserries of the Life Cycle Models. See documentation for more information how to 
access data objects from each run.

This CalSim II run was used as the basis of comparison for other potential operations that could offset impacts to listed species.

[More information on CALSIM II](https://water.ca.gov/Library/Modeling-and-Analysis/Central-Valley-models-and-tools/CalSim-II)\
[CALSIM II Schematic](https://s3-us-west-2.amazonaws.com/cvpiaflow-r-package/BST_CALSIMII_schematic_040110.jpg)

### Dependencies

The `DSMFlow` package provides data for several other packages within the [Reorienting to Recovery GitHub Organization](https://github.com/Reorienting-to-Recovery). These relationships are visualized in the dependency graph below.

<img src="man/figures/dependencyChain.svg" width="100%"/>

::: {style="margin-top: 40px;"}
Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="man/figures/TransLogoTreb.png" width="150px"/>
:::
