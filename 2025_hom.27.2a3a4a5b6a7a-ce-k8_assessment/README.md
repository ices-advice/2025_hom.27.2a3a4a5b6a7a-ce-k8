2025_hom.27.2a4a5b6a7a-ce-k8_assessment
================

## Western horse mackerel

This repository recreates the stock assessment for western horse mackerel (*Trachurus trachurus*) in Divisions 2a, 3a, 4a, 5b, 6a,7a-c,e-k and Subarea 8 in SS3 from WGWIDE 2025


## Running the assessment

The easiest way to run the assessment is to clone or download this
repository, navigate into the repository with R and run:

``` r
### load the icesTAF package
library(TAF)
### load data and install R packages
taf.boot()
### run all scripts
sourceAll()
```

This code snippet runs the data compilation and assessment and creates
the tables and figures presented in the WG report.
