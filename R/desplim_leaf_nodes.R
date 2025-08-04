#' Identify leaf nodes of an `sf` LINESTRING object
#' @description Function for computing leaf nodes of an `sf` MULTILINESTRING or
#' LINESTRING object.
#' @param input_lines object of class sf of type LINESTRING or MULTILINESTRING.
#' @return An sf object of type POINT containing all leaf nodes of
#' `input_lines`.
#' @details Function to compute leaf nodes, i.e. nodes which only intersect with
#' one linestring in the `sf` LINESTRING object. If `input_lines` contains
#' geometries of type MULTILINESTRING, they are cast to LINESTRING before
#' computing.
#' @examples
#' # Generate lines
#' line1 <- sf::st_linestring(rbind(c(0, 0), c(1, 1)))
#' line2 <- sf::st_linestring(rbind(c(1, 1), c(2, 2)))
#' line3 <- sf::st_linestring(rbind(c(1, 1), c(1, 0)))
#' combined_sf <- sf::st_as_sf(sf::st_sfc(line1, line2, line3, crs = 4326))
#'
#' # Compute leaf nodes
#' leaf_nodes <- desplim_leaf_nodes(combined_sf)
#' print(leaf_nodes)
#'
#' # Visualise
#' plot(sf::st_geometry(combined_sf), col = "slateblue3", lwd = 2)
#' plot(sf::st_geometry(leaf_nodes), col = "tomato3", pch = 16, cex = 2, add = TRUE)
#' @export
desplim_leaf_nodes <- function(input_lines) {
  if (!inherits(input_lines, "sf")) {
    stop("Input lines have to be of class sf")
  }
  output_crs <- sf::st_crs(input_lines)
  if (is.na(output_crs)) {
    warning("Input lines have no CRS")
    output_crs <- sf::st_crs(NA)
  }
  empty_sfc_points <- sf::st_sfc(sf::st_point(), crs = output_crs)[0]
  empty_leaf_nodes <- sf::st_sf(geometry = empty_sfc_points)
  if (nrow(input_lines) == 0) {
    return(empty_leaf_nodes)
  }
  input_geom_types <- unique(sf::st_geometry_type(input_lines))
  if (attr(input_lines, "sf_column") != "geometry") {
    input_lines <- .desplim_rename_geom(input_lines)
  }
  if (!all(input_geom_types %in% c("LINESTRING", "MULTILINESTRING"))) {
    is_valid_type <- sapply(
      input_geom_types,
      function(type) type %in% c("LINESTRING", "MULTILINESTRING")
    )
    if (!all(is_valid_type)) {
      if (
        length(input_geom_types) == 1 &&
          input_geom_types == "GEOMETRYCOLLECTION" &&
          all(sf::st_is_empty(input_lines))
      ) {
        return(empty_leaf_nodes)
      }
      stop("Input should be LINESTRING or MULTILINESTRING")
    }
  }
  if (any(input_geom_types == "MULTILINESTRING")) {
    input_lines <- sf::st_cast(input_lines, "LINESTRING", warn = FALSE)
  }
  lines_substring <- desplim_cast_substring(input_lines)
  all_cords <- sf::st_coordinates(lines_substring)[, c("X", "Y"), drop = FALSE]
  unique_endpoint_flags <- !(duplicated(all_cords) |
    duplicated(all_cords, fromLast = TRUE))
  leaf_coords_matrix <- all_cords[unique_endpoint_flags, , drop = FALSE]
  leaf_nodes <- sf::st_as_sf(
    as.data.frame(leaf_coords_matrix),
    coords = c("X", "Y"),
    crs = output_crs
  )
  return(leaf_nodes)
}
