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
  max_iter = Inf,
  compact_allow_subsequent = 1,
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

- max_iter:

  integer; maximum number of split-merge iterations to perform per
  hierarchy level. The algorithm repeats split-merge until the polygon
  set no longer changes (convergence) or `max_iter` is reached. Default
  is `Inf`.

- compact_allow_subsequent:

  numerical; value of `compact_allow` passed to `desplim_merge` from the
  second iteration onward. A higher value is more lenient, reducing the
  risk of re-merging polygons created in earlier iterations. Only
  applied when `max_iter > 1`. Default is `1` (compactness does not
  trigger merges in subsequent iterations).

- ...:

  additional arguments passed to `desplim_split`, `desplim_merge` and
  `desplim_connect_border`.

## Value

An sf object of POLYGONs resulting from the merge.

## Details

The function applies the DESPLIM algorithm to a set of input polygons
and lines. For each hierarchy level, split-merge is repeated until
convergence (the polygon set no longer changes between iterations) or
`max_iter` is reached. Convergence is determined by the number of output
polygons being equal to the previous iteration.

## Note

When using `parallel = TRUE`, each worker session will print the `sf`
startup message on first load. To suppress this, configure your `future`
plan with an initializer before calling this function:

    future::plan(future::multisession, workers = n,
      initializer = function() suppressPackageStartupMessages(library(sf)))
