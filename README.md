<img src="man/figures/cvpia_logo.jpg" align="right" width="40%"/>

### Modeled Flow Data for the CVPIA SIT Model

*This data package contains modeled flow and diversion data for each of the watersheds within the CVPIA SIT Salmon Life Cycle Model.*

#### Installation

``` r
# install.packages("remotes")
remotes::install_github("CVPIA-OSC/DSMFlow")
```

#### Usage

This package provides flow related datasets to the [`fallRunDSM,`](https://github.com/CVPIA-OSC/fallRunDSM) [`springRunDSM,`](https://github.com/CVPIA-OSC/springRunDSM) [`winterRunDSM,`](https://github.com/CVPIA-OSC/winterRunDSM) and [`latefallRunDSM`](https://github.com/CVPIA-OSC/latefallRunDSM) packages.

``` r
# datasets within the package
data(package = 'DSMflow')

# explore CALSIM II modeled flow mapped to CVPIA tributaries metadata
?DSMflow::flows_cfs
```

#### About the Models

Output from the CALSIM II operational model is used to generate hydrologic inputs for the CVPIA Decision Support Model (DSM). CALSIM II is the model used to simulate California State Water Project (SWP) / Central Valley Project (CVP) operations. CALSIM II was developed in the Water Resource Integrated Modeling System (WRIMS model engine or WRIMS), a generalized water resources modeling system for evaluating operational alternatives of large, complex river basins.

The 2017 DSM uses existing conditions simulated from CalSim-II Draft SWP Delivery Reliability Studies. There are two versions of the simulated conditions that can be used: the 2009 CalSim-II Draft SWP Delivery Reliability Studies and the 2019 CalSim-II Draft SWP Delivery Reliability Studies. Results of these simulations are stored in data objects with both versions named by the year of the CalSim-II run (2008-2009 or 2018-2019). The CalSim-II run is a Reclamation product used to replicate current operations for comparison with proposed adjustments under an ongoing Endangered Species Act consultation with the National Marine Fisheries Service.

A current NMFS Biological Opinion concluded that, as proposed, CVP and SWP operations were likely to jeopardize the continued existence of four federally- listed anadromous fish species:\
- Sacramento River winter-run Chinook salmon\
- Central Valley spring-run Chinook salmon\
- California Central Valley steelhead\
- Southern distinct population segment of the North American green sturgeon

Data objects in this package contain information from a CalSim II run from 2009 and a CalSim II 
run from 2019, both spanning the years 1980-2000. See documentation for more information how to 
access data objects from each run.

This CalSim II run was used as the basis of comparison for other potential operations that could offset impacts to listed species.

[More information on CALSIM II](https://water.ca.gov/Library/Modeling-and-Analysis/Central-Valley-models-and-tools/CalSim-II)\
[CALSIM II Schematic](https://s3-us-west-2.amazonaws.com/cvpiaflow-r-package/BST_CALSIMII_schematic_040110.jpg)

### Dependencies

The `DSMFlow` package provides data for several other packages within the [CVPIA Open Science Collaborative](https://github.com/CVPIA-OSC). These relationships are visualized in the dependency graph below.

<img src="man/figures/dependencyChain.svg" width="100%"/>

::: {style="margin-top: 40px;"}
Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="man/figures/TransLogoTreb.png" width="150px"/>
:::
