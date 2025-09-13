#' Data used to model the DESPLIM compactness metric
#'
#' Clean data used to model the DESPLIM compactness metric, based on
#' the data provided by Kaufman et al. (2021). The metrics have been computed
#' using the `redistmetrics` package.
#'
#' @format A data frame with 547 rows and 11 columns:
#' \describe{
#'   \item{compact}{Kaufman compactness}
#'   \item{boyce}{Boyce Clark ratio}
#'   \item{box_reock}{Box Reock compactness}
#'   \item{hull}{Convex Hull compactness}
#'   \item{len_wid}{Length-Width compactness}
#'   \item{polsby}{Polsby Popper compactness}
#'   \item{reock}{Reock compactness}
#'   \item{schwartz}{Schwartzberg compactness}
#'   \item{skew}{Skew compactness}
#'   \item{sym_x}{X symmetry}
#'   \item{sym_y}{Y symmetry}
#' }
#'
#' @source <https://dataverse.harvard.edu/file.xhtml?fileId=4143644&version=1.1>
#' @source <https://github.com/aaronrkaufman/compactness/tree/e91519e15fd3ca55f57fc21323bb12ed5a66305b/data>
#' @source <https://alarm-redist.org/redistmetrics/>
"compact_train"
