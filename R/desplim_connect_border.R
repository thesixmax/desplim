#' Connect a `sf` LINESTRING object to border nodes of an `sf` POLYGON object
#' @description Compute the optimal connections between nodes of a `sf`
#' LINESTRING object and border nodes of a `sf` POLYGON object based on user
#' input.
#' @param input_linestring object of class sf of type LINESTRING or
#' MULTILINESTRING.
#' @param input_polygon object of class sf of type POLYGON.
#' @param input_buildings object of class sf of type POLYGON or MULTIPOLYGON.
#' Contains elements which should not overlap with output border connetions.
#' Default is `NULL`.
#' @param distance_intersect numerical; the minimum distance from exsting
#' intersections of `input_linestring` and the border of `input_polygon` for a
#' border node of `input_polygon` to be considered for a new connection. Default
#' is 500 units.
#' @param distance_nodes numerical; maximum distance from the border nodes of
#' `input_polygon` to for a node of `input_linestring` to be considered for a
#' new connection. Default is 250 units.
#' @param minimum_angle numerical; minimum angle between a new connection and
#' the border of `input_polygon` required for the new connection to be c
#' onsidered. Default is 30 degrees.
#' @param num_candidates numerical; number of connections to attempt for each
#' border node. Default is 3.
#' @return An sf object of type LINESTRING connecting nodes in
#' `input_linestring` to border nodes of `input_polygon`.
#' @details For each candidate border node, `num_candidates` candidate
#' connections are generated. Only candidate connections which meet the
#' requirements of `minimum_angle`, are fully covered by the input polygon and
#' do not cross any existing linestrings are selected. The border node closest
#' to an existing intersection is considered first. If more than one valid
#' connection for the border node is found, the shortest connection is selected
#' and the set of other candidate border nodes is updated to exclude those who
#' are within `distance_intersect` of the new connection. If no valid connection
#' for the border node exist, the border node is removed from the set of
#' candidate border nodes.
#' @examples
#' # Create input polygon
#' poly_coords <- list(matrix(
#'   c(0, 0, 10, 0, 16, 3, 10, 10, 0, 10, -4, 5, 0, 0),
#'   ncol = 2,
#'   byrow = TRUE
#' ))
#' input_poly <- sf::st_sf(
#'   geometry = sf::st_sfc(sf::st_polygon(poly_coords), crs = 32612)
#' )
#' 
#' # Create input linestring
#' line_coords <- matrix(
#'   c(-4, 5, -2, 5, 7, 8, 9, 6, 9, 1),
#'   ncol = 2,
#'   byrow = TRUE
#' )
#' input_line <- sf::st_sf(
#'   geometry = sf::st_sfc(sf::st_linestring(line_coords), crs = 32612)
#' )
#' 
#' # Create input building(s)
#' building_coords <- list(matrix(
#'   c(3, 9, 9, 9, 9, 8.5, 3, 8.5, 3, 9),
#'   ncol = 2,
#'   byrow = TRUE
#' ))
#' input_build <- sf::st_sf(
#'   geometry = sf::st_sfc(sf::st_polygon(building_coords), crs = 32612)
#' )
#' 
#' # Function call
#' border_connections <- desplim_connect_border(
#'   input_linestring = input_line,
#'   input_polygon = input_poly,
#'   input_buildings = input_build,
#'   distance_intersect = 3, # Small for example
#'   distance_nodes = 5, # Small for example
#'   minimum_angle = 30,
#'   num_candidates = 3
#' )
#' 
#' # Visualisation
#' plot(sf::st_geometry(input_poly), border = 'black', lwd = 3)
#' plot(sf::st_geometry(input_build), col = 'tomato3', add = TRUE)
#' plot(sf::st_geometry(input_line), col = 'olivedrab3', lwd = 4, add = TRUE)
#' plot(
#'   sf::st_geometry(border_connections),
#'   col = 'royalblue4',
#'   lwd = 3,
#'   lty = 2,
#'   add = TRUE
#' )
#' @export
desplim_connect_border <- function(
  input_linestring,
  input_polygon,
  input_buildings = NULL,
  distance_intersect = 500,
  distance_nodes = 250,
  minimum_angle = 30,
  num_candidates = 3
) {
  if (!inherits(input_linestring, "sf")) {
    stop("Input linestring must be an sf object")
  }
  if (!inherits(input_polygon, "sf")) {
    stop("Input polygon must be an sf object")
  }
  output_crs <- sf::st_crs(input_linestring)
  if (is.na(output_crs) && inherits(input_polygon, "sf")) {
    output_crs <- sf::st_crs(input_polygon)
  }
  if (is.na(output_crs)) {
    warning("Input linestring and polygon have no CRS")
    output_crs <- sf::st_crs(NA)
  }
  empty_lines_sf <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  if (nrow(input_linestring) == 0) {
    ("Input linestring is empty.")
    return(empty_lines_sf)
  }
  if (nrow(input_polygon) == 0) {
    stop("Input polygon is empty.")
    return(empty_lines_sf)
  }
  if (sf::st_crs(input_linestring) != sf::st_crs(input_polygon)) {
    stop("Input linestring and polygon must have the same CRS.")
  }
  geom_type_lines <- unique(sf::st_geometry_type(input_linestring))
  if (!all(geom_type_lines %in% c("MULTILINESTRING", "LINESTRING"))) {
    stop(
      "Input linestring should be of type MULTILINESTRING or LINESTRING"
    )
  }
  if (attr(input_linestring, "sf_column") != "geometry") {
    input_linestring <- .desplim_rename_geom(input_linestring)
  }
  if (any(geom_type_lines == "MULTILINESTRING")) {
    input_linestring <- sf::st_cast(
      input_linestring,
      "LINESTRING",
      warn = FALSE
    )
  }
  geom_type_polygon <- unique(sf::st_geometry_type(input_polygon))
  if (!all(geom_type_polygon == "POLYGON")) {
    stop(
      "Input polygon should be of type POLYGON. Found: ",
      paste(geom_type_polygon, collapse = ", ")
    )
  }
  if (attr(input_polygon, "sf_column") != "geometry") {
    input_polygon <- .desplim_rename_geom(input_polygon)
  }
  processed_buildings_geom <- NULL
  if (!is.null(input_buildings)) {
    if (!inherits(input_buildings, "sf")) {
      stop("Input buildings must be an sf object if provided.")
    }
    if (nrow(input_buildings) > 0) {
      if (sf::st_crs(input_linestring) != sf::st_crs(input_buildings)) {
        stop("Input linestring and input buildings must have the same CRS.")
      }
      geom_type_buildings <- unique(sf::st_geometry_type(input_buildings))
      if (!all(geom_type_buildings %in% c("POLYGON", "MULTIPOLYGON"))) {
        stop(
          "Input buildings should be of type POLYGON or MULTIPOLYGON"
        )
      }
      if (attr(input_buildings, "sf_column") != "geometry") {
        input_buildings <- .desplim_rename_geom(input_buildings)
      }
      processed_buildings_geom <- sf::st_geometry(input_buildings)
    } else {
      input_buildings <- NULL
    }
  }
  border_polygon <- sf::st_cast(input_polygon, "LINESTRING", warn = FALSE)
  border_substring <- desplim_cast_substring(border_polygon)
  border_nodes <- desplim_all_nodes(border_substring, cast_substring = FALSE)
  if (nrow(border_nodes) == 0) {
    return(empty_lines_sf)
  }
  border_int_geom <- sf::st_intersection(
    sf::st_geometry(input_linestring),
    sf::st_geometry(border_polygon)
  )
  if (!any(sf::st_geometry_type(border_int_geom) == "POINT")) {
    return(empty_lines_sf)
  }
  border_int_sf <- NULL
  if (!is.null(border_int_geom) && length(border_int_geom) > 0) {
    border_int_points <- suppressWarnings(sf::st_collection_extract(
      border_int_geom,
      "POINT"
    ))
    if (
      length(border_int_points) > 0 && !sf::st_is_empty(border_int_points[[1]])
    ) {
      border_int_sf <- .desplim_rename_geom(
        sf::st_as_sf(sf::st_cast(border_int_points, "POINT", warn = FALSE))
      )
    }
  }
  if (is.null(border_int_sf) || nrow(border_int_sf) == 0) {
    empty_sfc_for_int <- sf::st_sfc(sf::st_point(), crs = output_crs)[0]
    border_int_sf <- sf::st_sf(geometry = empty_sfc_for_int)
  }
  if (nrow(border_int_sf) > 0 && nrow(border_nodes) > 0) {
    indices_to_remove_bn <- unique(unlist(sf::st_is_within_distance(
      border_int_sf,
      border_nodes,
      dist = distance_intersect
    )))
    if (length(indices_to_remove_bn) > 0) {
      border_nodes <- border_nodes[-indices_to_remove_bn, , drop = FALSE]
    }
  }
  if (nrow(border_nodes) == 0) {
    ("No candidate border nodes found after filtering by distance_intersect.")
    return(empty_lines_sf)
  }
  border_nodes_order <- seq_len(nrow(border_nodes))
  if (nrow(border_int_sf) > 0) {
    dist_bn_to_int <- sf::st_distance(border_nodes, border_int_sf)
    if (ncol(dist_bn_to_int) > 0) {
      min_distances_to_int <- suppressWarnings(apply(
        dist_bn_to_int,
        1,
        min,
        na.rm = TRUE
      ))
      min_distances_to_int[
        is.infinite(min_distances_to_int) & min_distances_to_int < 0
      ] <- Inf
      border_nodes_order <- order(min_distances_to_int)
    }
  }
  lines_nodes <- desplim_all_nodes(input_linestring, cast_substring = TRUE)
  if (nrow(lines_nodes) == 0) {
    return(empty_lines_sf)
  }
  if (nrow(border_int_sf) > 0 && nrow(lines_nodes) > 0) {
    indices_to_remove_ln1 <- unique(unlist(sf::st_is_within_distance(
      border_int_sf,
      lines_nodes,
      dist = distance_intersect
    )))
    if (length(indices_to_remove_ln1) > 0) {
      lines_nodes <- lines_nodes[-indices_to_remove_ln1, , drop = FALSE]
    }
  }
  if (nrow(lines_nodes) == 0) {
    warning(
      "No lines_nodes remain after filtering near existing 
    intersections."
    )
    return(empty_lines_sf)
  }
  if (nrow(border_nodes) > 0 && nrow(lines_nodes) > 0) {
    keep_ln_logical <- lengths(sf::st_is_within_distance(
      lines_nodes,
      border_nodes,
      dist = distance_nodes
    )) >
      0
    lines_nodes <- lines_nodes[keep_ln_logical, , drop = FALSE]
  } else {
    lines_nodes <- lines_nodes[0, , drop = FALSE]
  }
  if (nrow(lines_nodes) == 0) {
    return(empty_lines_sf)
  }
  node_match <- matrix(
    NA_integer_,
    nrow = nrow(border_nodes),
    ncol = num_candidates
  )
  if (nrow(lines_nodes) > 0) {
    dist_matrix_bn_ln <- sf::st_distance(border_nodes, lines_nodes)

    for (i_bn in seq_len(nrow(border_nodes))) {
      row_distances <- dist_matrix_bn_ln[i_bn, ]
      k_to_select <- min(num_candidates, length(row_distances))
      if (k_to_select > 0) {
        ordered_indices_for_row <- order(row_distances)[1:k_to_select]
        node_match[i_bn, 1:k_to_select] <- ordered_indices_for_row
      }
    }
  }
  dist_mat_bn_bn <- sf::st_is_within_distance(
    border_nodes,
    dist = distance_intersect
  )
  ls_list_sfg <- list()
  candidate_line_attributes_list <- list()
  coords_border_nodes <- sf::st_coordinates(border_nodes)
  coords_lines_nodes <- sf::st_coordinates(lines_nodes)
  if (nrow(lines_nodes) > 0) {
    for (i in seq_len(nrow(border_nodes))) {
      current_border_node_coord <- coords_border_nodes[
        i,
        c("X", "Y"),
        drop = FALSE
      ]
      for (j_cand in 1:num_candidates) {
        ln_idx_in_lines_nodes <- node_match[i, j_cand]
        if (is.na(ln_idx_in_lines_nodes)) {
          next
        }
        current_line_node_coord <- coords_lines_nodes[
          ln_idx_in_lines_nodes,
          c("X", "Y"),
          drop = FALSE
        ]
        segment_coords <- rbind(
          current_border_node_coord,
          current_line_node_coord
        )
        ls_list_sfg[[length(ls_list_sfg) + 1]] <- sf::st_linestring(
          segment_coords
        )
        candidate_line_attributes_list[[
          length(candidate_line_attributes_list) + 1
        ]] <-
          data.frame(border_node_row_idx = i)
      }
    }
  }
  if (length(ls_list_sfg) == 0) {
    warning("No candidate lines could be initially formed.")
    return(empty_lines_sf)
  }
  border_connect_filter <- sf::st_sf(
    do.call(rbind, candidate_line_attributes_list),
    geometry = sf::st_sfc(ls_list_sfg, crs = output_crs)
  )
  border_connect_filter$internal <- sf::st_covered_by(
    sf::st_geometry(border_connect_filter),
    input_polygon,
    sparse = FALSE
  )[, 1]
  border_connect_filter$crosses <- lengths(sf::st_crosses(
    sf::st_geometry(border_connect_filter),
    sf::st_geometry(input_linestring),
    sparse = TRUE
  )) >
    0
  if (
    !is.null(input_buildings) &&
      nrow(input_buildings) > 0 &&
      nrow(border_connect_filter) > 0
  ) {
    border_connect_filter$crosses_buildings <- lengths(sf::st_crosses(
      sf::st_geometry(border_connect_filter),
      processed_buildings_geom,
      sparse = TRUE
    )) >
      0
  } else {
    border_connect_filter$crosses_buildings <- FALSE
  }
  candidate_lines <- split(
    border_connect_filter,
    factor(
      border_connect_filter$border_node_row_idx,
      levels = seq_len(nrow(border_nodes))
    )
  )
  candidate_lines <- lapply(
    candidate_lines,
    function(x_df) {
      subset(
        x_df,
        x_df$internal == TRUE &
          x_df$crosses == FALSE &
          x_df$crosses_buildings == FALSE
      )
    }
  )
  border_connect_out_list <- list()
  current_processing_order <- border_nodes_order
  max_angle <- min_angle <- NULL
  while (length(current_processing_order) > 0) {
    current_bn_processing_idx <- current_processing_order[1]
    candidate_lines_bn_list <-
      candidate_lines[[as.character(
        current_bn_processing_idx
      )]]
    selected_connection_sf <- NULL
    if (
      !is.null(candidate_lines_bn_list) &&
        nrow(candidate_lines_bn_list) > 0
    ) {
      connections_with_angles_sf <- desplim_angles(
        candidate_lines_bn_list,
        border_substring,
        cast_substring = FALSE
      )
      connections_with_angles_sf$length <- as.numeric(sf::st_length(
        connections_with_angles_sf
      ))
      valid_connections_sf <- subset(
        connections_with_angles_sf,
        max_angle > minimum_angle &
          max_angle < (180.0 - minimum_angle) &
          min_angle > minimum_angle &
          min_angle < (180.0 - minimum_angle) &
          length < distance_nodes &
          length > 1e-9
      )
      if (nrow(valid_connections_sf) > 0) {
        if (nrow(valid_connections_sf) > 1) {
          min_len <- min(valid_connections_sf$length, na.rm = TRUE)
          selected_connection_sf <- valid_connections_sf[
            which(abs(valid_connections_sf$length - min_len) < 1e-9)[1],
          ]
        } else {
          selected_connection_sf <- valid_connections_sf
        }
      }
    }
    if (!is.null(selected_connection_sf) && nrow(selected_connection_sf) == 1) {
      border_connect_out_list[[
        length(border_connect_out_list) + 1
      ]] <- selected_connection_sf
      nodes_rm_from_order <- dist_mat_bn_bn[[current_bn_processing_idx]]
      nodes_rm_from_order <- unique(c(
        nodes_rm_from_order,
        current_bn_processing_idx
      ))
      current_processing_order <- current_processing_order[
        !(current_processing_order %in% nodes_rm_from_order)
      ]
    } else {
      current_processing_order <- current_processing_order[
        current_processing_order != current_bn_processing_idx
      ]
    }
  }
  final_output_sf <- NULL
  if (length(border_connect_out_list) > 0) {
    final_output_sf <- do.call(rbind, border_connect_out_list)
    if (!inherits(final_output_sf, "sf")) {
      final_output_sf <- sf::st_as_sf(final_output_sf)
    }
    sf::st_crs(final_output_sf) <- output_crs
    final_output_sf <- .desplim_rename_geom(final_output_sf)
    final_output_sf <- final_output_sf[, "geometry", drop = FALSE]
  } else {
    final_output_sf <- empty_lines_sf
  }
  return(final_output_sf)
}
