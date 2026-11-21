# Description

This repository contains the code and data supporting the manuscript "Modeling Rare Fisheries Bycatch using Bayesian Time-Series and Concurrent Fishery-Dependent Sampling Frames".

# Set up

In addition to the code and data in this repository, you will need to install the R package `bycatch`. You can install it using the R package `pak` by running:

``` r
library(pak)
pak::pak("ericward-noaa/bycatch")
```

# Data

The haul-level data used for this manuscript are confidential, so the observer and EM data in this repository were obscured using R package `synthpop`. These data are used in case study 1.

The annual takes and landings data are not confidential, so the data in this repository are equivalent to those used in the manuscript. These data are used in case studies 1 and 2.

# NOAA Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
