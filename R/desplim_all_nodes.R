#' Compute all nodes of an `sf` LINESTRING object
#' @description Function for computing all nodes of an `sf`
#' MULTILINESTRING or LINESTRING object.
#' @param input_lines object of class sf of type LINESTRING or MULTILINESTRING.
#' @param cast_substring logical; should the input linestrings be cast to
#' substring? Default is `FALSE`.
#' @return An sf object of type POINT containing all nodes of `input_lines`.
#' @details Function to compute all nodes of an `sf` LINESTRING object, after
#' optionally casting `input_lines` to substrings. If `input_lines` contains
#' geometries of type MULTILINESTRING, they are cast to LINESTRING before
#' computing.
#' @examples
#' # Generate lines
#' line1 <- sf::st_linestring(rbind(c(0, 0), c(1, 1)))
#' line2 <- sf::st_linestring(rbind(c(1, 1), c(2, 0)))
#' combined_sf <- sf::st_as_sf(sf::st_sfc(line1, line2, crs = 4326))
#'
#' # Compute all nodes
#' all_nodes <- desplim_all_nodes(combined_sf)
#' print(all_nodes)
#'
#' # Visualise
#' plot(sf::st_geometry(combined_sf), col = "slateblue3", lwd = 2)
#' plot(sf::st_geometry(all_nodes), col = "tomato3", pch = 16, cex = 2, add = TRUE)
#' @export
desplim_all_nodes <- function(input_lines, cast_substring = FALSE) {
  if (!inherits(input_lines, "sf")) {
    stop("Input lines have to be of class sf")
  }
  output_crs <- sf::st_crs(input_lines)
  if (is.na(output_crs)) {
    warning("Input lines have no CRS")
    output_crs <- sf::st_crs(NA)
  }
  empty_sf <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  if (nrow(input_lines) == 0) {
    warning("Input is empty, no nodes identified")
    return(empty_sf)
  }
  input_geom_types <- unique(sf::st_geometry_type(input_lines))
  if (!all(input_geom_types %in% c("LINESTRING", "MULTILINESTRING"))) {
    if (
      length(input_geom_types) == 1 &&
        input_geom_types == "GEOMETRYCOLLECTION" &&
        all(sf::st_is_empty(input_lines))
    ) {
      warning("Input is empty, no nodes identified")
      return(empty_sf)
    }
    stop("Input should be LINESTRING or MULTILINESTRING")
  }
  if (any(input_geom_types == "MULTILINESTRING")) {
    input_lines <- sf::st_cast(input_lines, "LINESTRING", warn = FALSE)
  }
  if (attr(input_lines, "sf_column") != "geometry") {
    input_lines <- .desplim_rename_geom(input_lines)
  }
  if (cast_substring) {
    input_lines <- desplim_cast_substring(input_lines)
  }
  coords_matrix <- sf::st_coordinates(input_lines)
  unique_coords_matrix <- unique(coords_matrix[, c("X", "Y"), drop = FALSE])
  all_nodes <- sf::st_as_sf(
    as.data.frame(unique_coords_matrix),
    coords = c("X", "Y"),
    crs = output_crs
  )
  return(all_nodes)
}
