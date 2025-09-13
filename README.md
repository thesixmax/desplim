# desplim: Implementation of the DESPLIM algorithm
<!-- badges: start -->
  [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
  [![CodeFactor](https://www.codefactor.io/repository/github/thesixmax/desplim/badge)](https://www.codefactor.io/repository/github/thesixmax/desplim)
  [![R-CMD-check](https://github.com/thesixmax/desplim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thesixmax/desplim/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package is the R implementation of the DESPLIM (deterministic split-merge) algorithm for geospatial districting.

Aside from the main algorithm, the package contains several helper functions, which can be utilised when working with `sf` LINESTRING and POLYGON data.

## Installation

This package depends heavily on the spatial package [`sf`](https://github.com/r-spatial/sf). It is recommended to install this package first.

Execute the following to install the latest version of `desplim`:

``` R
library(devtools)
install_github("thesixmax/desplim")
```
In addition, the core functions can utilise parallel processing through the [`future`](https://future.futureverse.org/) framework, which is highly recommended.

``` R
install.packages("future")
```

## The DESPLIM algorithm

**An associated paper is forthcoming.**

**Extended documentation, including examples, is work in progress.**
