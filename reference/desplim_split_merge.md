# Apply the DESPLIM algorithm to a set of input polygons and lines.

Apply the DESPLIM algorithm to a set of input polygons and lines.

## Usage

``` r
desplim_split_merge(
  input_polygon,
  input_lines,
  input_buildings = NULL,
  line_type_identifier = NULL,
  line_type_hierarchy = NULL,
  parallel = FALSE,
  ...
)
```

## Arguments

- input_polygon:

  object of class sf of type POLYGON to be merged.

- input_lines:

  object of class sf of type LINESTRING or MULTILINESTRING to be used
  for splitting.

- input_buildings:

  object of class sf of type POLYGON or MULTIPOLYGON. An optional sf
  object representing buildings which should be considered when
  splitting and merging. Default is `NULL`.

- line_type_identifier:

  string; the name of the column in `input_lines` which contains the
  line type. Default is `NULL`.

- line_type_hierarchy:

  list; hierarchical structure of line types used for splitting. Default
  is `NULL`. All types can be accessed with `"all"`.

- parallel:

  logical; whether to run the split and merging steps in parallel. If
  `TRUE`, uses the `future` package, where the number of workers should
  be set using the `plan` argument. Default is `FALSE`.

- ...:

  additional arguments passed to `desplim_split`, `desplim_merge` and
  `desplim_connect_border`.

## Value

An sf object of POLYGONs resulting from the merge.

## Details

The function applies the DESPLIM algorithm to a set of input polygons
and lines. The splitting and merging steps are performed in line with
the `desplim_split` and `desplim_merge` functions for each iteration as
defined by `line_type_hierarchy`.
