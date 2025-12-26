# desplim: Implementation of the DESPLIM algorithm

This package is the R implementation of the DESPLIM (deterministic
split-merge) algorithm for geospatial districting.

Aside from the main algorithm, the package contains several helper
functions, which can be utilised when working with `sf` LINESTRING and
POLYGON data.

## Installation

This package depends heavily on the spatial package
[`sf`](https://github.com/r-spatial/sf). It is recommended to install
this package first.

Execute the following to install the latest version of `desplim`:

``` r
library(devtools)
install_github("thesixmax/desplim")
```

In addition, the core functions can utilise parallel processing through
the [`future`](https://future.futureverse.org/) framework, which is
highly recommended.

``` r
install.packages("future")
```

## The DESPLIM algorithm

**An associated paper is forthcoming.**

**Extended documentation, including examples, is work in progress.**
