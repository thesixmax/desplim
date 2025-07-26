#' Internal data functions
#' @keywords internal
#' @noRd
.desplim_rename_geom <- function(input, geom_name = "geometry") {
  if (!inherits(input, "sf")) {
    stop("Input has to be of class sf")
  }
  current <- attr(input, "sf_column")
  names(input)[names(input) == current] <- geom_name
  sf::st_geometry(input) <- geom_name
  return(input)
}
.desplim_rm_duplicate_geoms <- function(input) {
  if (!inherits(input, "sf")) {
    stop("Input has to be of class sf")
  }
  if (nrow(input) == 0) {
    return(input)
  }
  equals_list <- sf::st_equals(input)
  indices_to_remove <- unique(unlist(lapply(equals_list, function(x) {
    if (length(x) > 1) {
      x[x != min(x)]
    } else {
      NULL
    }
  })))
  if (length(indices_to_remove) > 0) {
    output <- input[-indices_to_remove, , drop = FALSE]
  } else {
    output <- input
  }
  return(output)
}
