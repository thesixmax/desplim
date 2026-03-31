#' Calculate the 'desplim' compactness of an `sf` POLYGON object
#' @description Based on the approach described in Kaufmann et al. (2021), this
#' function computes the compactness of an `sf` POLYGON object.
#' @param input object of class `sf` of type POLYGON or MULTIPOLYGON.
#' @param keep_metrics logical; should all the compactness metrics be kept?
#' Default is `FALSE`.
#' @return An sf object of type POLYGON matching `input`, with the compactness
#' metric(s) added.
#' @details Please see the dedicated vignette for information about the
#' compactness metric. If `input` contains geometries of type MULTIPOLYGON, they
#' are cast to POLYGON before computing.
#' @examples
#' # Create simple polygon
#' polygon_coords <- list(matrix(
#'  c(0, 0, 0, 1, 1, 3, -1, 2, -3, 1, -2, 0, 0, 0),
#'  ncol = 2,
#'  byrow = TRUE
#' ))
#' simple_polygon <- sf::st_sfc(sf::st_polygon(polygon_coords), crs = 4326)
#' input_sf <- sf::st_sf(geometry = simple_polygon)
#' # Calculate compactness
#' compactness_result <- desplim_compactness(input_sf, keep_metrics = TRUE)
#' plot(input_sf)
#' print(sf::st_drop_geometry(compactness_result))
#' @export
desplim_compactness <- function(input, keep_metrics = FALSE) {
  if (!inherits(input, "sf")) {
    stop("Input polygons have to be of class sf")
  }
  input_geom_type <- unique(sf::st_geometry_type(input))
  if (!all(input_geom_type %in% c("MULTIPOLYGON", "POLYGON"))) {
    stop("Input should be MULTIPOLYGON or POLYGON")
  }
  if ("MULTIPOLYGON" %in% input_geom_type) {
    input <- sf::st_cast(input, "POLYGON", warn = FALSE)
  }
  districts <- input
  xgb_model <- xgboost::xgb.load(system.file(
    "extdata",
    "xgb_model.json",
    package = "desplim"
  ))
  districts$id <- seq_len(nrow(districts))
  metric_fns <- list(
    boyce     = redistmetrics::comp_bc,
    box_reock = redistmetrics::comp_box_reock,
    hull      = redistmetrics::comp_ch,
    len_wid   = redistmetrics::comp_lw,
    polsby    = redistmetrics::comp_polsby,
    skew      = redistmetrics::comp_skew,
    sym_x     = redistmetrics::comp_x_sym,
    sym_y     = redistmetrics::comp_y_sym
  )
  for (nm in names(metric_fns)) {
    districts[[nm]] <- metric_fns[[nm]](plans = districts$id, shp = districts)
  }
  districts_nogeom <- sf::st_drop_geometry(districts)[, names(metric_fns)]
  districts_nogeom$compactness <- stats::predict(
    xgb_model,
    newdata = as.matrix(districts_nogeom)
  )
  if (keep_metrics) {
    input <- cbind(input, districts_nogeom)
  } else {
    input <- cbind(input, "compactness" = districts_nogeom[, "compactness"])
  }
  input
}
