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

Output from the CalSim II & CalSim 3 operational models are used to generate hydrologic inputs for the R2R Salmon Life Cycle Model. CalSim 3 is the most up to date model used to simulate California State Water Project (SWP) / Central Valley Project (CVP) operations. CalSim 3 was developed in the Water Resource Integrated Modeling System (WRIMS model engine or WRIMS), a generalized water resources modeling system for evaluating operational alternatives of large, complex river basins.


[More information on CalSim 3 II](https://water.ca.gov/Library/Modeling-and-Analysis/Central-Valley-models-and-tools/CalSim-3)

[CalSim 3 Schematic](https://data.cnra.ca.gov/dataset/2395530a-5421-487e-921e-d6e594f23ac6/resource/3ec8338e-31d9-4dd3-93df-8d60ee6ec3d2/download/cs3_networkschematic_integrated_10_11_2022.pdf)

[More information on CalSim II](https://water.ca.gov/Library/Modeling-and-Analysis/Central-Valley-models-and-tools/CalSim-II)

[CalSim II Schematic](https://s3-us-west-2.amazonaws.com/cvpiaflow-r-package/BST_CALSIMII_schematic_040110.jpg)

### Dependencies

The `DSMFlow` package provides data for several other packages within the [Reorienting to Recovery GitHub Organization](https://github.com/Reorienting-to-Recovery). These relationships are visualized in the dependency graph below.

<img src="man/figures/dependencyChain.svg" width="100%"/>

::: {style="margin-top: 40px;"}
Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> 
<img src="man/figures/TransLogoTreb.png" width="150px"/>
:::
