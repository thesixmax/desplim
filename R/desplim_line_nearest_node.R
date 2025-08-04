#' Calculate the minimum distance linestring from an `sf` POINT object to nodes
#' of a `sf` LINESTRING object
#' @description Calculate the minimum distance linestring connection from an
#' `sf` POINT object to nodes of an `sf` LINESTRING object
#' @param input_nodes object of class sf with geometry type POINT or MULTIPOINT.
#' @param input_lines object of class sf with geometry type LINESTRING or
#' MULTILINESTRING.
#' @param input_buildings optional object of class sf with geometry type POLYGON
#' or MULTIPOLYGON to not be crossed.
#' @param cast_substring logical; should the input linestring be cast to
#' substring? Default is `TRUE`.
#' @param combine_nodes logical; should the input nodes and nodes of the input
#' lines be combined? Default is `TRUE`.
#' @param ignore_equal logical; should output linestrings which are exactly
#' equal to one or more elements in `input_lines` be ignored? Default is `TRUE`.
#' @return An `sf` object of type LINESTRING containing the minimum distance
#' linestrings from each node in `input_nodes` to nodes in `input_lines` or the
#' combined set of `input_nodes` and nodes in `input_lines`.
#' @details For each input node, the function computes the minimum distance to
#' the chosen set of nodes and outputs the minimum distance linestrings. In
#' cases where the minimum distance is zero, due to overlap of the input node
#' and one or more nodes in `input_lines`, the second minimum distance
#' linestring is computed. If `input_nodes` or `input_lines` contain geometries
#' of type MULTIPOINT or MULTILINESTRING, they are converted to POINT and
#' LINESTRING respectively before computing.
#' @examples
#' # Create nodes, lines and a building
#' crs <- 32632
#' line1 <- sf::st_linestring(rbind(c(0, 10), c(10, 10)))
#' line2 <- sf::st_linestring(rbind(c(10, 10), c(10, 0)))
#' lines_sf <- sf::st_as_sf(sf::st_sfc(line1, line2, crs = crs))
#' 
#' node1 <- sf::st_point(c(3, 6))
#' node2 <- sf::st_point(c(8, 4))
#' points_sf <- sf::st_as_sf(sf::st_sfc(node1, node2, crs = crs))
#' 
#' building_poly <- sf::st_polygon(list(rbind(
#'   c(1, 7),
#'   c(1, 9),
#'   c(4, 9),
#'   c(4, 7),
#'   c(1, 7)
#' )))
#' building_sf <- sf::st_as_sf(sf::st_sfc(building_poly, crs = crs))
#' 
#' # Compute connections with no building
#' connections_no_building <- desplim_line_nearest_node(points_sf, lines_sf)
#' print(connections_no_building)
#' 
#' # Visualise
#' plot(sf::st_geometry(lines_sf), col = "black", lwd = 2)
#' plot(sf::st_geometry(points_sf), col = "slateblue3", pch = 16, cex = 2, add = TRUE)
#' plot(sf::st_geometry(connections_no_building), col = "olivedrab", lwd = 2, add = TRUE)
#' 
#' # Compute connections with building
#' connections_with_building <- desplim_line_nearest_node(
#'   input_nodes = points_sf,
#'   input_lines = lines_sf,
#'   input_buildings = building_sf
#' )
#' print(connections_with_building)
#' 
#' # Visualise
#' plot(sf::st_geometry(lines_sf), col = "black", lwd = 2)
#' plot(sf::st_geometry(building_sf), col = "tomato3", border = NA, add = TRUE)
#' plot(sf::st_geometry(points_sf), col = "blue", pch = 16, cex = 2, add = TRUE)
#' plot(sf::st_geometry(connections_with_building), col = "green", lwd = 2, add = TRUE)
#' @export
desplim_line_nearest_node <- function(
  input_nodes,
  input_lines,
  input_buildings = NULL,
  cast_substring = TRUE,
  combine_nodes = TRUE,
  ignore_equal = TRUE
) {
  if (!inherits(input_nodes, "sf")) {
    stop("Input nodes have to be of class sf")
  }
  if (!inherits(input_lines, "sf")) {
    stop("Input lines have to be of class sf")
  }
  if (!is.null(input_buildings) && !inherits(input_buildings, "sf")) {
    stop("Input buildings have to be of class sf")
  }
  if (sf::st_crs(input_nodes) != sf::st_crs(input_lines)) {
    stop("Input nodes and lines should be in the same CRS")
  }
  if (
    any(
      !unique(sf::st_geometry_type(input_nodes)) %in% c("POINT", "MULTIPOINT")
    )
  ) {
    stop("Point input should be POINT or MULTIPOINT")
  }
  if (
    any(
      !unique(sf::st_geometry_type(input_lines)) %in%
        c("LINESTRING", "MULTILINESTRING")
    )
  ) {
    stop("Lines input should be LINESTRING or MULTILINESTRING")
  }
  if (
    !is.null(input_buildings) &&
      any(
        !unique(sf::st_geometry_type(input_buildings)) %in%
          c("POLYGON", "MULTIPOLYGON")
      )
  ) {
    stop("Input buildings should be POLYGON or MULTIPOLYGON")
  }
  if (any(unique(sf::st_geometry_type(input_lines)) == "MULTILINESTRING")) {
    input_lines <- sf::st_cast(
      input_lines,
      "LINESTRING",
      warn = FALSE
    )
  }
  if (any(unique(sf::st_geometry_type(input_nodes)) == "MULTIPOINT")) {
    input_nodes <- sf::st_cast(
      input_nodes,
      "POINT",
      warn = FALSE
    )
  }
  if (nrow(input_nodes) == 0) {
    stop("Input nodes is empty.")
  }
  if (cast_substring) {
    input_lines <- desplim_cast_substring(input_lines)
  }
  if (attr(input_nodes, "sf_column") != "geometry") {
    input_nodes <- .desplim_rename_geom(input_nodes)
  }
  if (attr(input_lines, "sf_column") != "geometry") {
    input_lines <- .desplim_rename_geom(input_lines)
  }
  if (!is.null(input_buildings)) {
    if (attr(input_buildings, "sf_column") != "geometry") {
      input_buildings <- .desplim_rename_geom(input_buildings)
    }
  }
  all_nodes <- desplim_all_nodes(input_lines)
  nodes_inter_list <- lengths(sf::st_intersects(all_nodes, input_nodes)) > 0
  subset_nodes <- all_nodes[!nodes_inter_list, ]
  if (combine_nodes) {
    subset_nearest_node <- subset_nodes[
      sf::st_nearest_feature(input_nodes, subset_nodes),
    ]
    input_nearest_node <- input_nodes[sf::st_nearest_feature(input_nodes), ]
    subset_distances <- sf::st_distance(
      input_nodes,
      subset_nearest_node,
      by_element = TRUE
    )
    input_distances <- sf::st_distance(
      input_nodes,
      input_nearest_node,
      by_element = TRUE
    )
    indices <- ifelse(
      subset_distances < input_distances,
      seq_along(subset_distances),
      (length(subset_distances) + 1):(2 * length(subset_distances))
    )
    temp_subset <- subset_nearest_node["geometry"]
    temp_input <- input_nearest_node["geometry"]
    combined_nearest_node <- rbind(temp_subset, temp_input)[indices, ]
  } else {
    combined_nearest_node <- subset_nodes[
      sf::st_nearest_feature(input_nodes, subset_nodes),
    ]
  }
  line_nearest_node <- sf::st_sfc(mapply(
    function(a, b) {
      union <- sf::st_union(a, b)
      if (sf::st_geometry_type(union) %in% "MULTIPOINT") {
        lnn <- sf::st_cast(union, "LINESTRING", warn = FALSE)
      } else {
        lnn <- NULL
      }
      return(lnn)
    },
    sf::st_geometry(input_nodes),
    sf::st_geometry(combined_nearest_node),
    SIMPLIFY = FALSE
  ))
  sf::st_crs(line_nearest_node) <- sf::st_crs(input_nodes)
  if (ignore_equal) {
    line_nearest_node_equal <- lengths(sf::st_equals(
      line_nearest_node,
      input_lines
    ))
    line_nearest_node <- line_nearest_node[which(line_nearest_node_equal == 0)]
  }
  line_nearest_node <- sf::st_as_sf(line_nearest_node[which(
    !sf::st_is_empty(line_nearest_node)
  )])
  if (!is.null(input_buildings)) {
    lines_buildings_intersect <- lengths(sf::st_intersects(
      line_nearest_node,
      input_buildings
    ))
    line_nearest_node <- line_nearest_node[
      which(lines_buildings_intersect == 0),
    ]
  }
  line_nearest_node <- .desplim_rm_duplicate_geoms(line_nearest_node)
  line_nearest_node <- .desplim_rename_geom(line_nearest_node)
  return(line_nearest_node)
}
