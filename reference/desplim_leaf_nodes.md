# Identify leaf nodes of an `sf` LINESTRING object

Function for computing leaf nodes of an `sf` MULTILINESTRING or
LINESTRING object.

## Usage

``` r
desplim_leaf_nodes(input_lines)
```

## Arguments

- input_lines:

  object of class sf of type LINESTRING or MULTILINESTRING.

## Value

An sf object of type POINT containing all leaf nodes of `input_lines`.

## Details

Function to compute leaf nodes, i.e. nodes which only intersect with one
linestring in the `sf` LINESTRING object. If `input_lines` contains
geometries of type MULTILINESTRING, they are cast to LINESTRING before
computing.

## Examples

``` r
# Generate lines
line1 <- sf::st_linestring(rbind(c(0, 0), c(1, 1)))
line2 <- sf::st_linestring(rbind(c(1, 1), c(2, 2)))
line3 <- sf::st_linestring(rbind(c(1, 1), c(1, 0)))
combined_sf <- sf::st_as_sf(sf::st_sfc(line1, line2, line3, crs = 4326))

# Compute leaf nodes
leaf_nodes <- desplim_leaf_nodes(combined_sf)
print(leaf_nodes)
#> Simple feature collection with 3 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 2 ymax: 2
#> Geodetic CRS:  WGS 84
#>      geometry
#> 1 POINT (0 0)
#> 2 POINT (2 2)
#> 3 POINT (1 0)

# Visualise
plot(sf::st_geometry(combined_sf), col = "slateblue3", lwd = 2)
plot(sf::st_geometry(leaf_nodes), col = "tomato3", pch = 16, cex = 2, add = TRUE)
```
