#' Calculate the minimum and maximum angles between two `sf` LINESTRING objects
#' @description Function for calculating the minimum and maximum angles
#' between two intersecting `sf` MULTILINESTRING or LINESTRING objects.
#' @param input_lines object of class sf of type LINESTRING or MULTILINESTRING.
#' @param reference_lines object of class sf of type LINESTRING or
#' MULTILINESTRING.
#' @param cast_substring logical; should the input linestrings be cast to
#' substring? Default is `TRUE`.
#' @return An sf object with the geometries from `input_lines` with added
#' columns containing the values of the minimum and maximum angle.
#' @details If the input and reference lines are already cast to substring,
#' the function can be sped up, notably by setting `cast_substring` to `FALSE`
#' for larger geometries.
#' @examples
#' # Create input and reference lines
#' input_line <- sf::st_linestring(matrix(
#'   c(-1, 0, 0, 0, 1, 0),
#'   ncol = 2,
#'   byrow = TRUE
#' ))
#' ref_line <- sf::st_linestring(matrix(
#'   c(0, -1, 0, 0, -0.5, 1),
#'   ncol = 2,
#'   byrow = TRUE
#' ))
#' input_sf <- sf::st_sf(geom = sf::st_sfc(input_line), crs = 32613)
#' ref_sf <- sf::st_sf(geom = sf::st_sfc(ref_line), crs = 32613)
#'
#' # Calculate angles
#' angles_sf <- desplim_angles(input_lines = input_sf, reference_lines = ref_sf)
#' print(angles_sf)
#' 
#' # Plot
#' plot(sf::st_geometry(input_sf), col = "tomato3", lwd = 2)
#' plot(sf::st_geometry(ref_sf), add = TRUE, col = "slateblue3", lwd = 2)
#' text(-0.1, 0.1, paste0(round(angles_sf$min_angle[[1]], 1), "°"), pos = 2)
#' text(-0.1, -0.1, paste0(round(angles_sf$max_angle[[1]], 1), "°"), pos = 2)
#' text(0.1, 0.1, paste0(round(angles_sf$max_angle[[2]], 1), "°"), pos = 4)
#' text(0.1, -0.1, paste0(round(angles_sf$min_angle[[2]], 1), "°"), pos = 4)
#' @export
desplim_angles <- function(
  input_lines,
  reference_lines,
  cast_substring = TRUE
) {
  if (!inherits(input_lines, "sf")) {
    stop("Input lines have to be of class sf")
  }
  if (!inherits(reference_lines, "sf")) {
    stop("Reference lines have to be of class sf")
  }
  if (is.na(sf::st_crs(input_lines))) {
    warning("Input lines have no CRS")
  }
  if (nrow(input_lines) == 0 || nrow(reference_lines) == 0) {
    out_empty <- input_lines[0, , drop = FALSE]
    out_empty$min_angle <- NA_real_
    out_empty$max_angle <- NA_real_
    message("Input is empty, no angles calculated")
    return(out_empty)
  }
  if (sf::st_crs(input_lines) != sf::st_crs(reference_lines)) {
    stop("Input and reference lines should be in the same CRS")
  }
  valid_geom_types <- c("LINESTRING", "MULTILINESTRING")
  input_geom_types <- unique(sf::st_geometry_type(input_lines))
  ref_geom_types   <- unique(sf::st_geometry_type(reference_lines))
  if (!all(input_geom_types %in% valid_geom_types)) {
    stop("Input lines should be LINESTRING or MULTILINESTRING")
  }
  if (!all(ref_geom_types %in% valid_geom_types)) {
    stop("Reference lines should be LINESTRING or MULTILINESTRING")
  }
  if (attr(input_lines, "sf_column") != "geometry") {
    input_lines <- .desplim_rename_geom(input_lines)
  }
  if (attr(reference_lines, "sf_column") != "geometry") {
    reference_lines <- .desplim_rename_geom(reference_lines)
  }
  if (any(input_geom_types == "MULTILINESTRING")) {
    input_lines <- sf::st_cast(input_lines, "LINESTRING", warn = FALSE)
  }
  if (any(ref_geom_types == "MULTILINESTRING")) {
    reference_lines <- sf::st_cast(reference_lines, "LINESTRING", warn = FALSE)
  }
  if (cast_substring) {
    input_lines <- desplim_cast_substring(input_lines)
    reference_lines <- desplim_cast_substring(reference_lines)
  }
  input_geom_sfc <- sf::st_geometry(input_lines)
  reference_geom_sfc <- sf::st_geometry(reference_lines)
  intersects_list <- sf::st_intersects(input_geom_sfc, reference_geom_sfc)
  angle_results_list <- vector("list", length(input_geom_sfc))
  for (i in seq_along(input_geom_sfc)) {
    current_input_sfc <- input_geom_sfc[[i]]
    inter_ref_idx <- intersects_list[[i]]
    if (
      length(inter_ref_idx) == 0 ||
        sf::st_is_empty(current_input_sfc)
    ) {
      angle_results_list[[i]] <- list(
        min_angle = NA_real_,
        max_angle = NA_real_
      )
      next
    }
    coords_input <- sf::st_coordinates(current_input_sfc)
    n_input <- nrow(coords_input)
    xy_input <- coords_input[, c("X", "Y"), drop = FALSE]
    current_input_angle <- unlist(lapply(
      inter_ref_idx,
      function(k_idx) {
        current_ref_sfc <- reference_geom_sfc[[k_idx]]
        if (sf::st_is_empty(current_ref_sfc)) {
          return(NA_real_)
        }
        coords_ref <- sf::st_coordinates(current_ref_sfc)
        n_ref <- nrow(coords_ref)
        xy_ref <- coords_ref[, c("X", "Y"), drop = FALSE]
        current_ref_angles <- numeric(0)
        for (v_input_idx in seq_len(n_input)) {
          pt_b_input <- xy_input[v_input_idx, , drop = FALSE]
          for (v_ref_idx in seq_len(n_ref)) {
            pt_b_ref <- xy_ref[v_ref_idx, , drop = FALSE]
            if (all(abs(pt_b_input - pt_b_ref) < 1e-9)) {
              point_b <- t(pt_b_input)
              if (n_input < 2L) next
              other_v_input_idx <- if (v_input_idx == 1L) 2L else 1L
              point_a <- t(xy_input[other_v_input_idx, , drop = FALSE])
              if (n_ref < 2L) next
              other_v_ref_idx <- if (v_ref_idx == 1L) 2L else 1L
              point_c <- t(xy_ref[other_v_ref_idx, , drop = FALSE])
              current_angle <- .angle_fun(point_a, point_b, point_c)
              if (!is.na(current_angle)) {
                current_ref_angles <- c(current_ref_angles, current_angle)
              }
              return(current_ref_angles)
            }
          }
        }
      }
    ))
    valid_angles <- current_input_angle[!is.na(current_input_angle)]
    if (length(valid_angles) == 0) {
      angle_results_list[[i]] <- list(
        min_angle = NA_real_,
        max_angle = NA_real_
      )
    } else {
      angle_results_list[[i]] <- list(
        min_angle = min(valid_angles),
        max_angle = max(valid_angles)
      )
    }
  }
  final_out_sf <- input_lines
  final_out_sf$min_angle <- vapply(angle_results_list, `[[`, numeric(1), "min_angle")
  final_out_sf$max_angle <- vapply(angle_results_list, `[[`, numeric(1), "max_angle")
  final_out_sf
}
