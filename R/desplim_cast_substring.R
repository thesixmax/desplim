#' Cast an `sf` LINESTRING or MULTILINESTRING object to substrings
#' @description Function for computing the substrings of an `sf` MULTILINESTRING
#' or LINESTRING object.
#' @param input_lines object of class sf with geometry type MULTILINESTRING or
#' LINESTRING.
#' @return An sf object of type LINESTRING containing the substrings of
#' `input_lines`.
#' @details To avoid issues of connecting points from spatially separate
#' linestrings, the function converts all MULTILINESTRING to LINESTRING. The
#' output is always LINESTRING of class matching `input_lines`. Inputs of type
#' sfc are not supported.
#' @examples
#' # Create input line
#' input_line <- sf::st_linestring(matrix(
#'   c(-1, 0, 0, 0, 1, 0, 2, 0),
#'   ncol = 2,
#'   byrow = TRUE
#' ))
#' input_sf <- sf::st_sf(geom = sf::st_sfc(input_line), crs = 32613)
#' print(input_sf)
#' # Cast to substring
#' substring_sf <- desplim_cast_substring(input_sf)
#' print(substring_sf)
#' @export
desplim_cast_substring <- function(input_lines) {
  if (!inherits(input_lines, "sf")) {
    stop("Input lines have to be of class sf")
  }
  output_crs <- sf::st_crs(input_lines)
  if (is.na(output_crs)) {
    warning("Input lines have no CRS")
  }
  if (nrow(input_lines) == 0) {
    warning("Input lines is empty")
    return(sf::st_sf(geometry = sf::st_sfc(crs = output_crs)))
  }
  input_geom_type <- unique(sf::st_geometry_type(input_lines))
  if (!all(input_geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("Input should be LINESTRING or MULTILINESTRING")
  }
  if (attr(input_lines, "sf_column") != "geometry") {
    input_lines <- .desplim_rename_geom(input_lines)
  }
  if ("MULTILINESTRING" %in% input_geom_type) {
    input_lines <- sf::st_cast(input_lines, "LINESTRING", warn = FALSE)
  }
  input_attributes <- sf::st_drop_geometry(input_lines)
  n_lines <- nrow(input_lines)
  segment_lists <- vector("list", n_lines)
  parent_lists <- vector("list", n_lines)
  if (n_lines < 1000) {
    for (i in seq_len(n_lines)) {
      coords <- sf::st_coordinates(input_lines[i, ])
      if (nrow(coords) < 2) next
      n_segs <- nrow(coords) - 1L
      segment_lists[[i]] <- lapply(seq_len(n_segs), function(k) {
        sf::st_linestring(coords[k:(k + 1), 1:2])
      })
      parent_lists[[i]] <- rep(i, n_segs)
    }
  } else {
    all_coords <- sf::st_coordinates(input_lines)
    if (nrow(all_coords) == 0) {
      out_sf <- input_lines[0, , drop = FALSE]
      sf::st_geometry(out_sf) <- sf::st_sfc(crs = output_crs)
      return(out_sf)
    }
    line_id_coords <- all_coords[, "L1"]
    is_new_line <- c(TRUE, line_id_coords[-1] != line_id_coords[-length(line_id_coords)])
    line_starts <- which(is_new_line)
    line_ends <- c(line_starts[-1] - 1L, nrow(all_coords))
    original_line_indices <- line_id_coords[line_starts]
    segment_lists <- vector("list", length(line_starts))
    parent_lists <- vector("list", length(line_starts))
    for (j in seq_along(line_starts)) {
      coords_one_line <- all_coords[line_starts[j]:line_ends[j], c("X", "Y")]
      if (nrow(coords_one_line) < 2) next
      n_segs <- nrow(coords_one_line) - 1L
      segment_lists[[j]] <- lapply(seq_len(n_segs), function(k) {
        sf::st_linestring(coords_one_line[k:(k + 1), ])
      })
      parent_lists[[j]] <- rep(original_line_indices[j], n_segs)
    }
  }
  all_substring_list <- do.call(c, segment_lists)
  parent_line_indices <- unlist(parent_lists)
  if (length(all_substring_list) == 0) {
    out_sf <- input_lines[0, , drop = FALSE]
    sf::st_geometry(out_sf) <- sf::st_sfc(crs = output_crs)
    return(out_sf)
  }
  lines_sfc <- sf::st_sfc(all_substring_list, crs = output_crs)
  attributes_expanded <- input_attributes[parent_line_indices, , drop = FALSE]
  sf::st_set_geometry(attributes_expanded, lines_sfc)
}
