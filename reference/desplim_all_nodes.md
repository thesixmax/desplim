# Compute all nodes of an `sf` LINESTRING object

Function for computing all nodes of an `sf` MULTILINESTRING or
LINESTRING object.

## Usage

``` r
desplim_all_nodes(input_lines, cast_substring = FALSE)
```

## Arguments

- input_lines:

  object of class sf of type LINESTRING or MULTILINESTRING.

- cast_substring:

  logical; should the input linestrings be cast to substring? Default is
  `FALSE`.

## Value

An sf object of type POINT containing all nodes of `input_lines`.

## Details

Function to compute all nodes of an `sf` LINESTRING object, after
optionally casting `input_lines` to substrings. If `input_lines`
contains geometries of type MULTILINESTRING, they are cast to LINESTRING
before computing.

## Examples

``` r
# Generate lines
line1 <- sf::st_linestring(rbind(c(0, 0), c(1, 1)))
line2 <- sf::st_linestring(rbind(c(1, 1), c(2, 0)))
combined_sf <- sf::st_as_sf(sf::st_sfc(line1, line2, crs = 4326))

# Compute all nodes
all_nodes <- desplim_all_nodes(combined_sf)
print(all_nodes)
#> Simple feature collection with 3 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 2 ymax: 1
#> Geodetic CRS:  WGS 84
#>      geometry
#> 1 POINT (0 0)
#> 2 POINT (1 1)
#> 3 POINT (2 0)

# Visualise
plot(sf::st_geometry(combined_sf), col = "slateblue3", lwd = 2)
plot(sf::st_geometry(all_nodes), col = "tomato3", pch = 16, cex = 2, add = TRUE)
```
