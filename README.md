
<!-- README.md is generated from README.Rmd. Please edit that file -->

# soilscaler

<!-- badges: start -->
<!-- badges: end -->

The R package soilscaler contains functions to downscale
coarse-resolution soil maps to higher resolutions by combining
high-resolution covariates with soil observations from other sites. The
goal of the package is to provide a workflow which is userfriendly and
straightforward, while being flexible enough to allow experimentation.

## Installation

You can install the development version of soilscaler from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("anbm-dk/soilscaler")
```

## Input data

The main function for the package is `make_downscaler`, which fits
prediction models for downscaling and tests their accuracies using
leave-site-out cross-validation.

The required inputs for the function include:

1.  A list with SpatVector point datasets containing soil observations
    for each site. The datasets can contain different numbers of points,
    but the column names and formats must match.
2.  A list of SpatRaster datasets with high-resolution covariates for
    each site. The number of layers and the layer names must match.
3.  A SpatRaster containing the coarse-resolution input soil map for
    downscaling. Technically, it is possible to use the function without
    an input map. However, some of the options in the function will
    become unavailable, as they depend on the input map.

The package contains data examples for the inputs. Firstly,
`DK_observations` are soil observations from five research fields in
Denmark. The function `list_unwrap` unwraps the data.

``` r
library(terra)
#> terra 1.6.47
library(magrittr)
#> 
#> Vedhæfter pakke: 'magrittr'
#> De følgende objekter er maskerede fra 'package:terra':
#> 
#>     extract, inset
library(soilscaler)

my_obs <- list_unwrap(DK_observations, "EPSG:25832")

par(mfrow = c(2, 3))

for (i in 1:length(my_obs)) {
  plot(my_obs[[i]], "clay", main = names(my_obs)[i])
}
```

<img src="man/figures/README-input-1.png" width="100%" />
