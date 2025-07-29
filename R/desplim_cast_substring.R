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
    output_crs <- sf::st_crs(NA)
  }
  empty_sf <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  if (nrow(input_lines) == 0) {
    warning("Input lines is empty")
    return(empty_sf)
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
  all_substring_list <- list()
  parent_line_indices <- integer(0)
  if (nrow(input_lines) < 1000) {
    for (i in seq_len(nrow(input_lines))) {
      coords <- sf::st_coordinates(input_lines[i, ])
      if (is.null(coords) || nrow(coords) < 2) {
        next
      }
      current_line_segments <- lapply(
        1:(nrow(coords) - 1),
        function(k) {
          sf::st_linestring(coords[k:(k + 1), 1:2, drop = FALSE])
        }
      )
      all_substring_list <- c(all_substring_list, current_line_segments)
      parent_line_indices <- c(
        parent_line_indices,
        rep(i, length(current_line_segments))
      )
    }
  } else {
    all_coords <- sf::st_coordinates(input_lines)
    if (is.null(all_coords) || nrow(all_coords) == 0) {
      out_sf <- input_lines[0, , drop = FALSE]
      sf::st_geometry(out_sf) <- sf::st_sfc(crs = sf::st_crs(input_lines))
      return(out_sf)
    }
    line_id_coords <- all_coords[, "L1"]
    is_new_line <- c(
      TRUE,
      line_id_coords[-1] != line_id_coords[-length(line_id_coords)]
    )
    line_starts <- which(is_new_line)
    line_ends <- c(
      line_starts[-1] - 1,
      nrow(all_coords)
    )
    original_line_indices <- line_id_coords[
      line_starts
    ]
    for (j in seq_along(line_starts)) {
      start_row <- line_starts[j]
      end_row <- line_ends[j]
      original_line_idx <- original_line_indices[j]
      coords_one_line <- all_coords[
        start_row:end_row,
        c("X", "Y"),
        drop = FALSE
      ]
      if (is.null(coords_one_line) || nrow(coords_one_line) < 2) {
        next
      }
      segments_current_line <- lapply(
        1:(nrow(coords_one_line) - 1),
        function(k) {
          sf::st_linestring(coords_one_line[
            k:(k + 1),
            ,
            drop = FALSE
          ])
        }
      )
      all_substring_list <- c(
        all_substring_list,
        segments_current_line
      )
      parent_line_indices <- c(
        parent_line_indices,
        rep(original_line_idx, length(segments_current_line))
      )
    }
  }
  if (length(all_substring_list) == 0) {
    out_sf <- input_lines[0, , drop = FALSE]
    sf::st_geometry(out_sf) <- sf::st_sfc(crs = output_crs)
    return(out_sf)
  }
  lines_sfc <- sf::st_sfc(all_substring_list, crs = output_crs)
  attributes_expanded <- input_attributes[parent_line_indices, , drop = FALSE]
  lines_sf_out <- sf::st_set_geometry(attributes_expanded, lines_sfc)
  return(lines_sf_out)
}
