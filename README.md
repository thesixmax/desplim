# desplim: Implementation of the DESPLIM algorithm in R

This package is the implementation of the DESPLIM (deterministic split-merge) algorithm in R. An associated paper is forthcoming.

Aside from the main algorithm, the package contains several helper functions, which can be utilised when working with `sf` LINESTRING and POLYGON data.

## Installation

This package depends on the spatial package [`sf`](https://github.com/r-spatial/sf). It is recommended to install this package first.

Execute the following to install the latest version of `desplim`:

``` R
library(devtools)
install_github("thesixmax/desplim")
```

## List of functions

The following is the up to date list of functions available in the package. Please refer to the individual man pages for more information. **Extended documentation, including examples, is work in progress.**

| Fuction | Function type | Description |
|-----------------|----------|---------------------------------------------|
| desplim_split_merge() | Main | Apply the DESPLIM algorithm to a set of input polygons and lines. |
| desplim_split() | Main | Split a polygon based on a `sf` LINESTRING object and optional border connections. |
| desplim_merge() | Main | Deterministically merge a set of polygons with possible parameters defined by the user. |
| desplim_all_nodes() | Helper | Compute all nodes of an `sf` LINESTRING object |
| desplim_angles() | Helper | Calculate the minimum and maximum angles between two `sf` LINESTRING objects. |
| desplim_cast_substring() | Helper | Cast an `sf` LINESTRING or MULTILINESTRING object to substrings. |
| desplim_compactness() | Helper | Calculate the 'desplim' compactness of an `sf` POLYGON object. |
| desplim_connect_border() | Helper | Connect a `sf` LINESTRING object to border nodes of an `sf` POLYGON object. |
| desplim_leaf_nodes() | Helper | Identify leaf nodes of an `sf` LINESTRING object. |
| desplim_line_nearest_node() | Helper | Calculate the minimum distance linestring from an `sf` POINT object to nodes of a `sf` LINESTRING object. |