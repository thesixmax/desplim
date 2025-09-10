#' DESPLIM compactness data
#'
#' Clean data used to model the DESPLIM compactness metric, based on
#' the data provided by Kaufmann et al. (2021). The metrics have been computed
#' using the `redistmetrics` package.
#'
#' @format ## `desplim_compact_data`
#' A data frame with 558 rows and 11 columns:
#' \describe{
#'   \item{compact}{District compactness}
#'   \item{boyce}{Boyce Clark ratio}
#'   \item{box_reock}{Box Reock compactness}
#'   \item{hull}{Convex Hull compactness}
#'   \item{len_wid}{Length Width compactness}
#'   \item{polsby}{Polsby Popper compactness}
#'   \item{reock}{Reock compactness}
#'   \item{schwartz}{Schwartzberg compactness}
#'   \item{skew}{Skew compactness}
#'   \item{sym_x}{X symmetry compactness}
#'   \item{sym_y}{Y symmetry compactness}
#' }
#'
#' @source <https://dataverse.harvard.edu/file.xhtml?fileId=4143644&version=1.1>
#' @source <https://github.com/aaronrkaufman/compactness/tree/e91519e15fd3ca55f57fc21323bb12ed5a66305b/data>
#' @source <https://alarm-redist.org/redistmetrics/>
"desplim_compact_data"

#' DESPLIM sf data
#'
#' Example districts with compactness score from the data provided by 
#' Kaufmann et al. (2021).
#'
#' @format ## `desplim_sf_data`
#' A sf data frame with 25 rows and 2 columns:
#' \describe{
#'   \item{compact}{District compactness}
#'   \item{geometry}{District shape}
#' }
#'
#' @source <https://dataverse.harvard.edu/file.xhtml?fileId=4143644&version=1.1>
"desplim_sf_data"
