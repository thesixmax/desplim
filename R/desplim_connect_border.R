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
#' plot(sf::st_geometry(input_poly), border = 'black', lwd = 2)
#' plot(sf::st_geometry(input_build), col = 'tomato3', add = TRUE)
#' plot(sf::st_geometry(input_line), col = 'olivedrab', lwd = 4, add = TRUE)
#' plot(
#'   sf::st_geometry(border_connections),
#'   col = 'slateblue3',
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
  if (is.na(output_crs)) {
    output_crs <- sf::st_crs(input_polygon)
  }
  if (is.na(output_crs)) {
    warning("Input linestring and polygon have no CRS")
  }
  empty_lines_sf <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  if (nrow(input_linestring) == 0) {
    warning("Input linestring is empty.")
    return(empty_lines_sf)
  }
  if (nrow(input_polygon) == 0) {
    stop("Input polygon is empty.")
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
    input_linestring <- suppressWarnings(sf::st_cast(
      input_linestring,
      "LINESTRING"
    ))
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
  buildings_geom <- NULL
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
      buildings_geom <- sf::st_geometry(input_buildings)
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
  if (length(border_int_geom) > 0) {
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
    rm_bn <- unique(unlist(sf::st_is_within_distance(
      border_int_sf,
      border_nodes,
      dist = distance_intersect
    )))
    if (length(rm_bn) > 0) {
      border_nodes <- border_nodes[-rm_bn, , drop = FALSE]
    }
  }
  if (nrow(border_nodes) == 0) {
    warning("No candidate border nodes found after filtering by distance_intersect.")
    return(empty_lines_sf)
  }
  border_nodes_order <- seq_len(nrow(border_nodes))
  if (nrow(border_int_sf) > 0) {
    dist_bn_to_int <- sf::st_distance(border_nodes, border_int_sf)
    if (ncol(dist_bn_to_int) > 0) {
      min_dist <- suppressWarnings(apply(
        dist_bn_to_int,
        1,
        min,
        na.rm = TRUE
      ))
      min_dist[
        is.infinite(min_dist) & min_dist < 0
      ] <- Inf
      border_nodes_order <- order(min_dist)
    }
  }
  lines_nodes <- desplim_all_nodes(input_linestring, cast_substring = TRUE)
  if (nrow(lines_nodes) == 0) {
    return(empty_lines_sf)
  }
  if (nrow(border_int_sf) > 0 && nrow(lines_nodes) > 0) {
    rm_ln <- unique(unlist(sf::st_is_within_distance(
      border_int_sf,
      lines_nodes,
      dist = distance_intersect
    )))
    if (length(rm_ln) > 0) {
      lines_nodes <- lines_nodes[-rm_ln, , drop = FALSE]
    }
  }
  if (nrow(lines_nodes) == 0) {
    warning(
      "No lines_nodes remain after filtering near existing intersections."
    )
    return(empty_lines_sf)
  }
  if (nrow(border_nodes) > 0 && nrow(lines_nodes) > 0) {
    keep_ln <- lengths(sf::st_is_within_distance(
      lines_nodes,
      border_nodes,
      dist = distance_nodes
    )) >
      0
    lines_nodes <- lines_nodes[keep_ln, , drop = FALSE]
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
  dist_bn_ln <- sf::st_distance(border_nodes, lines_nodes)
  for (i_bn in seq_len(nrow(border_nodes))) {
    row_dist <- dist_bn_ln[i_bn, ]
    k <- min(num_candidates, length(row_dist))
    if (k > 0) {
      top_idx <- order(row_dist)[seq_len(k)]
      node_match[i_bn, seq_len(k)] <- top_idx
    }
  }
  dist_bn_bn <- sf::st_is_within_distance(
    border_nodes,
    dist = distance_intersect
  )
  n_max_segs <- nrow(border_nodes) * num_candidates
  seg_list <- vector("list", n_max_segs)
  border_node_idx_vec <- integer(n_max_segs)
  seg_count <- 0L
  coords_border_nodes <- sf::st_coordinates(border_nodes)
  coords_lines_nodes <- sf::st_coordinates(lines_nodes)
  for (i in seq_len(nrow(border_nodes))) {
    bn_coord <- coords_border_nodes[i, c("X", "Y")]
    for (j_cand in seq_len(num_candidates)) {
      ln_idx <- node_match[i, j_cand]
      if (is.na(ln_idx)) next
      ln_coord <- coords_lines_nodes[ln_idx, c("X", "Y")]
      seg_count <- seg_count + 1L
      seg_list[[seg_count]] <- sf::st_linestring(rbind(bn_coord, ln_coord))
      border_node_idx_vec[seg_count] <- i
    }
  }
  if (seg_count == 0L) {
    warning("No candidate lines could be initially formed.")
    return(empty_lines_sf)
  }
  border_connect_filter <- sf::st_sf(
    data.frame(border_node_row_idx = border_node_idx_vec[seq_len(seg_count)]),
    geometry = sf::st_sfc(seg_list[seq_len(seg_count)], crs = output_crs)
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
      buildings_geom,
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
  all_valid_cands <- do.call(
    rbind,
    Filter(function(x) nrow(x) > 0, candidate_lines)
  )
  if (!is.null(all_valid_cands) && nrow(all_valid_cands) > 0) {
    all_valid_cands <- desplim_angles(
      all_valid_cands,
      border_substring,
      cast_substring = FALSE
    )
    all_valid_cands$length <- as.numeric(sf::st_length(all_valid_cands))
    all_valid_split <- split(
      all_valid_cands,
      all_valid_cands$border_node_row_idx
    )
    for (nm in names(all_valid_split)) {
      candidate_lines[[nm]] <- all_valid_split[[nm]]
    }
  }
  out_list <- list()
  proc_order <- border_nodes_order
  max_angle <- min_angle <- NULL # suppress R CMD check NOTE for subset() NSE
  while (length(proc_order) > 0) {
    bn_idx <- proc_order[1]
    bn_cands <- candidate_lines[[as.character(bn_idx)]]
    selected <- NULL
    if (
      !is.null(bn_cands) &&
        nrow(bn_cands) > 0
    ) {
      valid_cands <- subset(
        bn_cands,
        max_angle > minimum_angle &
          max_angle < (180.0 - minimum_angle) &
          min_angle > minimum_angle &
          min_angle < (180.0 - minimum_angle) &
          length < distance_nodes &
          length > 1e-9
      )
      if (nrow(valid_cands) > 0) {
        selected <- valid_cands[which.min(valid_cands$length), ]
      }
    }
    if (!is.null(selected) && nrow(selected) == 1) {
      out_list[[
        length(out_list) + 1
      ]] <- selected
      rm_nodes <- dist_bn_bn[[bn_idx]]
      rm_nodes <- unique(c(
        rm_nodes,
        bn_idx
      ))
      proc_order <- proc_order[
        !(proc_order %in% rm_nodes)
      ]
    } else {
      proc_order <- proc_order[
        proc_order != bn_idx
      ]
    }
  }
  out <- NULL
  if (length(out_list) > 0) {
    out <- do.call(rbind, out_list)
    if (!inherits(out, "sf")) {
      out <- sf::st_as_sf(out)
    }
    sf::st_crs(out) <- output_crs
    out <- .desplim_rename_geom(out)
    out <- out[, "geometry", drop = FALSE]
  } else {
    out <- empty_lines_sf
  }
  out
}
