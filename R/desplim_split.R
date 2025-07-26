#' Perform a detailed split of a polygon based on a sf LINESTRING object and
#' optional border connections.
#' @param input_polygon object of class sf of type POLYGON to be split.
#' @param input_lines object of class sf of type LINESTRING or MULTILINESTRING
#' to be used for splitting.
#' @param input_buildings object of class sf of type POLYGON or MULTIPOLYGON.
#' An optional sf object representing buildings which cannot be intersected
#' during the splitting procedure. Default is `NULL`.
#' @param enable_line_nearest_node logical; whether to connect leaf nodes of
#' `input_lines` to the nearest node using `desplim_line_nearest_node`. Default
#' is `TRUE`.
#' @param enable_border_connect logical; whether to connect `input_lines` to the
#' border of `input_polygon` using `desplim_connect_border`. Default is `FALSE`.
#' @param ... additional arguments passed to `desplim_connect_border` if set to
#' `TRUE`.
#' @return An sf object of POLYGONs resulting from the split.
#' @details The function splits an input polygon based on an initial set of
#' linestrings. If `enable_line_nearest_node` is `TRUE`, leaf nodes in the input
#' linestrings are connected to the nearest node as laid out in
#' `desplim_line_nearest_node`. If `enable_border_connect` is `TRUE`, the input
#' linestrings are connected to the border of the input polygon using
#' `desplim_connect_border`.
#' @export
desplim_split <- function(
  input_polygon,
  input_lines,
  input_buildings = NULL,
  enable_line_nearest_node = TRUE,
  enable_border_connect = FALSE,
  ...
) {
  if (!inherits(input_polygon, "sf")) {
    stop("Input polygon must be an sf object")
  }
  if (!inherits(input_lines, "sf")) {
    stop("input_lines must be an sf object")
  }
  output_crs <- sf::st_crs(input_polygon)
  if (is.na(output_crs) && !is.null(input_lines)) {
    output_crs <- sf::st_crs(input_lines)
  }
  if (is.na(output_crs) && !is.null(input_buildings)) {
    output_crs <- sf::st_crs(input_buildings)
  }
  if (is.na(output_crs)) {
    warning("Input have no CRS")
    output_crs <- sf::st_crs(NA)
  }
  empty_sf <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  if (nrow(input_polygon) == 0) {
    warning("Input is empty")
    return(empty_sf)
  }
  if (
    !is.na(sf::st_crs(input_lines)) && sf::st_crs(input_lines) != output_crs
  ) {
    input_lines <- sf::st_transform(input_lines, output_crs)
  }
  if (!is.null(input_buildings)) {
    if (!inherits(input_buildings, "sf")) {
      stop("Input buildings must be an sf object.")
    }
    if (
      !is.na(sf::st_crs(input_buildings)) &&
        sf::st_crs(input_buildings) != output_crs
    ) {
      input_buildings <- sf::st_transform(input_buildings, output_crs)
    }
    if (nrow(input_buildings) == 0) input_buildings <- NULL
  }
  if (attr(input_polygon, "sf_column") != "geometry") {
    input_polygon <- .desplim_rename_geom(input_polygon)
  }
  if (nrow(input_lines) > 0 && attr(input_lines, "sf_column") != "geometry") {
    input_lines <- .desplim_rename_geom(input_lines)
  }
  if (
    !is.null(input_buildings) &&
      attr(input_buildings, "sf_column") != "geometry"
  ) {
    input_buildings <- .desplim_rename_geom(input_buildings)
  }
  args <- list(...)
  if (!all(names(args) %in% names(formals(desplim_connect_border)))) {
    stop("Invalid parameters passed to desplim_connect_border")
  }
  geom_polygon <- sf::st_geometry(input_polygon)
  lines_network <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  if (nrow(input_lines) > 0) {
    # Select only lines that intersect the buffered polygon
    filter_buffer <- sf::st_buffer(geom_polygon, 1000)
    lines_subset <- sf::st_filter(
      input_lines,
      filter_buffer,
      .predicate = sf::st_intersects
    )
    if (nrow(lines_subset) > 0) {
      polygon_intersect_buffer <- sf::st_buffer(geom_polygon, 1)
      raw_intersection <- sf::st_intersection(
        sf::st_geometry(lines_subset),
        polygon_intersect_buffer
      )
      if (
        length(raw_intersection) > 0 && !all(sf::st_is_empty(raw_intersection))
      ) {
        line_geoms <- suppressWarnings(sf::st_collection_extract(
          raw_intersection,
          "LINESTRING"
        ))
        if (length(line_geoms) > 0 && !all(sf::st_is_empty(line_geoms))) {
          lines_network <- .desplim_rename_geom(sf::st_as_sf(line_geoms))
        } else {
          potential_lines <- sf::st_cast(
            sf::st_cast(raw_intersection, "MULTILINESTRING"),
            "LINESTRING"
          )
          if (
            length(potential_lines) > 0 &&
              !all(sf::st_is_empty(potential_lines))
          ) {
            lines_network <- .desplim_rename_geom(sf::st_as_sf(potential_lines))
          }
        }
      }
    }
  }
  polygon_border <- .desplim_rename_geom(sf::st_as_sf(
    sf::st_cast(sf::st_cast(geom_polygon, "MULTILINESTRING"), "LINESTRING")
  ))
  lines_substring <- desplim_cast_substring(lines_network)
  polygon_border_substring <- desplim_cast_substring(polygon_border)
  network_list <- list()
  if (nrow(lines_substring) > 0) {
    network_list$lines <- lines_substring
  }
  if (nrow(polygon_border_substring) > 0) {
    network_list$border <- polygon_border_substring
  }
  lines_combined <- NULL
  if (length(network_list) == 2) {
    lines_combined <- sfnetworks::st_network_join(
      sfnetworks::as_sfnetwork(network_list$lines, directed = FALSE),
      sfnetworks::as_sfnetwork(network_list$border, directed = FALSE)
    )
  } else if (length(network_list) == 1) {
    lines_combined <- sfnetworks::as_sfnetwork(
      network_list[[1]],
      directed = FALSE
    )
  }
  lines_combined_sf <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  if (!is.null(lines_combined)) {
    if (nrow(lines_network) > 0 && nrow(polygon_border) > 0) {
      road_polygon_intersect_geom <- sf::st_intersection(
        sf::st_geometry(lines_network),
        sf::st_geometry(polygon_border)
      )
      road_polygon_intersect_sf <- NULL
      if (
        !is.null(road_polygon_intersect_geom) &&
          length(road_polygon_intersect_geom) > 0 &&
          !all(sf::st_is_empty(road_polygon_intersect_geom)) &&
          any(sf::st_geometry_type(road_polygon_intersect_geom) == "POINT")
      ) {
        road_polygon_intersect_points <- suppressWarnings(
          sf::st_collection_extract(
            road_polygon_intersect_geom,
            "POINT"
          )
        )
        if (
          length(road_polygon_intersect_points) > 0 &&
            !all(sf::st_is_empty(road_polygon_intersect_points))
        ) {
          road_polygon_intersect_sf <- sf::st_as_sf(sf::st_cast(
            road_polygon_intersect_points,
            "POINT"
          ))
        }
      }
      if (
        !is.null(road_polygon_intersect_sf) &&
          nrow(road_polygon_intersect_sf) > 0
      ) {
        lines_combined <- sfnetworks::st_network_blend(
          lines_combined,
          road_polygon_intersect_sf,
          tolerance = 1
        )
      }
    }
    lines_combined_sf <- sf::st_as_sf(sfnetworks::activate(
      lines_combined,
      "edges"
    ))
  }
  if (enable_line_nearest_node) {
    all_leaf_nodes <- desplim_leaf_nodes(lines_combined_sf)
    all_leaf_nodes_int <- sf::st_filter(
      all_leaf_nodes,
      input_polygon,
      .predicate = sf::st_intersects
    )
  } else {
    all_leaf_nodes_int <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  }
  all_leaf_nearest_nodes <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  if (nrow(all_leaf_nodes_int) > 0 && nrow(lines_combined_sf) > 0) {
    all_leaf_nearest_nodes <- desplim_line_nearest_node(
      input_nodes = all_leaf_nodes_int,
      input_lines = lines_combined_sf,
      input_buildings = input_buildings,
      cast_substring = FALSE,
      combine_nodes = TRUE,
      ignore_equal = TRUE
    )
  }
  if (is.null(all_leaf_nearest_nodes) || nrow(all_leaf_nearest_nodes) == 0) {
    all_leaf_nearest_nodes <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  }
  sfc_lines_combined <- if (nrow(lines_combined_sf) > 0) {
    sf::st_geometry(lines_combined_sf)
  } else {
    sf::st_sfc(crs = output_crs)
  }
  sfc_leaf_nearest <- if (nrow(all_leaf_nearest_nodes) > 0) {
    sf::st_geometry(all_leaf_nearest_nodes)
  } else {
    sf::st_sfc(crs = output_crs)
  }
  sfc_lines_substring <- if (nrow(lines_substring) > 0) {
    sf::st_geometry(lines_substring)
  } else {
    sf::st_sfc(crs = output_crs)
  }
  updated_network <- .desplim_rename_geom(sf::st_sf(
    geometry = c(sfc_lines_combined, sfc_leaf_nearest)
  ))
  updated_lines <- .desplim_rename_geom(sf::st_sf(
    geometry = c(sfc_lines_substring, sfc_leaf_nearest)
  ))
  border_connect_lines <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  if (enable_border_connect) {
    if (nrow(updated_lines) > 0 && nrow(input_polygon) > 0) {
      temp_border_connect <- desplim_connect_border(
        input_linestring = updated_lines,
        input_polygon = input_polygon,
        input_buildings = input_buildings,
        ...
      )
      if (!is.null(temp_border_connect) && nrow(temp_border_connect) > 0) {
        border_connect_lines <- temp_border_connect
      }
    }
  }
  final_updated_network_sfc_list <- list()
  if (nrow(updated_network) > 0 && !all(sf::st_is_empty(updated_network))) {
    final_updated_network_sfc_list[[
      length(final_updated_network_sfc_list) + 1
    ]] <- sf::st_geometry(updated_network)
  }
  if (
    nrow(border_connect_lines) > 0 &&
      !all(sf::st_is_empty(border_connect_lines))
  ) {
    final_updated_network_sfc_list[[
      length(final_updated_network_sfc_list) + 1
    ]] <- sf::st_geometry(border_connect_lines)
  }
  final_splitting_lines_sfc <- NULL
  if (length(final_updated_network_sfc_list) > 0) {
    final_splitting_lines_sfc <- do.call(c, final_updated_network_sfc_list)
  } else {
    final_splitting_lines_sfc <- sf::st_sfc(crs = output_crs)
  }
  final_splitting_lines_sf <- .desplim_rename_geom(sf::st_sf(
    geometry = final_splitting_lines_sfc
  ))
  split_result_geom <- lwgeom::st_split(
    sf::st_geometry(input_polygon),
    sf::st_geometry(final_splitting_lines_sf)
  )
  polygonize <- .desplim_rename_geom(sf::st_as_sf(
    suppressWarnings(sf::st_collection_extract(split_result_geom, "POLYGON"))
  ))
  if (
    !is.na(sf::st_crs(polygonize)) &&
      !is.na(output_crs) &&
      sf::st_crs(polygonize) != output_crs
  ) {
    polygonize <- sf::st_transform(polygonize, output_crs)
  } else if (is.na(sf::st_crs(polygonize)) && !is.na(output_crs)) {
    sf::st_crs(polygonize) <- output_crs
  }
  return(polygonize)
}
