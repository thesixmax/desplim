# Data used to model the DESPLIM compactness metric

Clean data used to model the DESPLIM compactness metric, based on the
data provided by Kaufman et al. (2021). The metrics have been computed
using the `redistmetrics` package.

## Usage

``` r
compact_train
```

## Format

A data frame with 547 rows and 9 columns:

- compact:

  Human-assigned compactness score, rescaled to 0 (least compact) to 1
  (most compact)

- boyce:

  Boyce-Clark index: mean deviation of 16 evenly spaced radials from the
  interior centre (0-Inf, 0 = most compact)

- box_reock:

  Bounding Box Reock: ratio of district area to its minimum bounding
  rectangle area (0-1)

- hull:

  Convex Hull: ratio of district area to its convex hull area (0-1)

- len_wid:

  Length-Width: ratio of district width to length via bounding box (0-1,
  1 = equal dimensions)

- polsby:

  Polsby-Popper: proportional to the ratio of district area to its
  perimeter squared (0-1)

- skew:

  Skew: ratio of maximum inscribed circle area to minimum bounding
  circle area (0-1)

- sym_x:

  X-symmetry: intersection of district with its X-axis reflection,
  relative to total area (0-1)

- sym_y:

  Y-symmetry: intersection of district with its Y-axis reflection,
  relative to total area (0-1)

## Source

<https://dataverse.harvard.edu/file.xhtml?fileId=4143644&version=1.1>

<https://github.com/aaronrkaufman/compactness/tree/e91519e15fd3ca55f57fc21323bb12ed5a66305b/data>

<https://alarm-redist.org/redistmetrics/>
