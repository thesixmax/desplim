#' Internal geometry functions
#' @keywords internal
#' @noRd
.sym_x_fun <- function(polygon) {
  geom <- sf::st_geometry(polygon)
  crs_original <- sf::st_crs(polygon)
  centroids <- sf::st_centroid(geom)
  centroid_coords <- sf::st_coordinates(centroids)
  flipped_geometries <- lapply(seq_along(geom), function(i) {
    single_geom <- sf::st_cast(geom[i], "POLYGON")
    reflected_parts <- lapply(single_geom, function(part) {
      coords <- sf::st_coordinates(part)
      centroid <- centroid_coords[i, ]
      reflected_y <- centroid[2] - (coords[, 2] - centroid[2])
      reflected_polygon <- sf::st_polygon(lapply(
        split(
          as.data.frame(cbind(coords[, 1], reflected_y)),
          factor(coords[, 3])
        ),
        as.matrix
      ))
      return(reflected_polygon)
    })
    sf::st_sfc(reflected_parts, crs = crs_original)
  })
  sym_x_val <- mapply(
    function(orig, flipped_sfc) {
      overlap <- sf::st_intersection(
        orig,
        sf::st_set_crs(flipped_sfc, sf::st_crs(orig))
      )
      original_area <- sf::st_area(orig)
      if (original_area == 0) {
        return(0)
      }
      as.numeric(sf::st_area(overlap) / original_area)
    },
    geom,
    flipped_geometries
  )
  return(sym_x_val)
}
.sym_y_fun <- function(polygon) {
  geom <- sf::st_geometry(polygon)
  crs_original <- sf::st_crs(polygon)
  centroids <- sf::st_centroid(geom)
  centroid_coords <- sf::st_coordinates(centroids)
  flipped_geometries <- lapply(seq_along(geom), function(i) {
    single_geom <- sf::st_cast(geom[i], "POLYGON")
    reflected_parts <- lapply(single_geom, function(part) {
      coords <- sf::st_coordinates(part)
      centroid <- centroid_coords[i, ]
      reflected_x <- centroid[1] - (coords[, 1] - centroid[1])
      reflected_polygon <- sf::st_polygon(lapply(
        split(
          as.data.frame(cbind(reflected_x, coords[, 2])),
          factor(coords[, 3])
        ),
        as.matrix
      ))
      return(reflected_polygon)
    })
    sf::st_sfc(reflected_parts, crs = crs_original)
  })
  sym_y_val <- mapply(
    function(orig, flipped_sfc) {
      overlap <- sf::st_intersection(
        orig,
        sf::st_set_crs(flipped_sfc, sf::st_crs(orig))
      )
      original_area <- sf::st_area(orig)
      if (original_area == 0) {
        return(0)
      }
      as.numeric(sf::st_area(overlap) / original_area)
    },
    geom,
    flipped_geometries
  )
  return(sym_y_val)
}
