# Cast an `sf` LINESTRING or MULTILINESTRING object to substrings

Function for computing the substrings of an `sf` MULTILINESTRING or
LINESTRING object.

## Usage

``` r
desplim_cast_substring(input_lines)
```

## Arguments

- input_lines:

  object of class sf with geometry type MULTILINESTRING or LINESTRING.

## Value

An sf object of type LINESTRING containing the substrings of
`input_lines`.

## Details

To avoid issues of connecting points from spatially separate
linestrings, the function converts all MULTILINESTRING to LINESTRING.
The output is always LINESTRING of class matching `input_lines`. Inputs
of type sfc are not supported.

## Examples

``` r
# Create input line
input_line <- sf::st_linestring(matrix(
  c(-1, 0, 0, 0, 1, 0, 2, 0),
  ncol = 2,
  byrow = TRUE
))
input_sf <- sf::st_sf(geom = sf::st_sfc(input_line), crs = 32613)
print(input_sf)
#> Simple feature collection with 1 feature and 0 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -1 ymin: 0 xmax: 2 ymax: 0
#> Projected CRS: WGS 84 / UTM zone 13N
#>                             geom
#> 1 LINESTRING (-1 0, 0 0, 1 0,...
# Cast to substring
substring_sf <- desplim_cast_substring(input_sf)
print(substring_sf)
#> Simple feature collection with 3 features and 0 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -1 ymin: 0 xmax: 2 ymax: 0
#> Projected CRS: WGS 84 / UTM zone 13N
#>                 geometry
#> 1 LINESTRING (-1 0, 0 0)
#> 2  LINESTRING (0 0, 1 0)
#> 3  LINESTRING (1 0, 2 0)
```
