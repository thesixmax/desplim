#' Deterministically merge a set of polygons with possible parameters defined by
#' the user.
#' @description Function for merging a set of polygons, with optinal rules
#' defined by the user. The merging process is deterministic, leading to
#' reproducible results on subsequent runs with the same input data and rules.
#' @param input_polygons object of class sf of type POLYGON to be merged.
#' @param input_buildings object of class sf of type POLYGON or MULTIPOLYGON.
#' An optional sf object representing buildings which should be considered when
#' merging. Default is `NULL`.
#' @param compact_method string; chosen method for calculating compactness of
#' polygons. Default is `"desplim"`, using the internal `desplim_compactness`
#' function. Other options include `"polsby"`, `"schwartz"` and `"convex_hull"`
#' from the `redistmetrics` package.
#' @param compact_threshold numerical; the baseline expected compactness of the
#' output polygons. Default is 0.7.
#' @param compact_allow numerical; the fraction of polygons allowed to fall
#' below `compact_tolerance`. Default is 0.2.
#' @param area_threshold numerical; the minimum required area of merged
#' polygons. Default is 10000 units.
#' @param enclosed_threshold numerical; the fraction of a polygon border shared
#' with a single neighbour for it to be considered enclosed (and flagged for
#' merging). Default is 0.8.
#' @param building_threshold integer; the minimum number of required buildings
#' within merged polygons if `input_buildings` is provided. Default is 5, but
#' sis set to 0 if `input_buildings` is not provided.
#' @return An sf object of POLYGONs resulting from the deterministic merge.
#' @details There are no requirements for all polygons being adjacent to each
#' other. Although named `input_buildings`, any sf object of type POLYGON or
#' MULTIPOLYGON can be provided for this argument. For details about the
#' compactness metric, please see the dedicated vignette. It is not recommended
#' to set `compact_allow` too close to 1 and `compact_tolerance` too low.
#' This is especially true if `compact_method` is set to `"polsby"`.
#' @examples
#' # Create grid
#' crs <- 32632
#' grid_area <- sf::st_bbox(
#'   c(xmin = 0, ymin = 0, xmax = 1200, ymax = 1000),
#'   crs = crs
#' )
#' grid_poly <- sf::st_as_sf(sf::st_make_grid(
#'   grid_area,
#'   cellsize = 100,
#'   square = TRUE
#' ))
#' plot(sf::st_geometry(grid_poly), border = 'grey')
#'
#' # Create buildings
#' n_buildings <- 100
#' set.seed(420)
#' building_centers <- sf::st_sample(sf::st_as_sfc(grid_area), size = n_buildings)
#' list_of_buildings <- lapply(sf::st_geometry(building_centers), function(point) {
#'   x <- point[1]
#'   y <- point[2]
#'   width <- runif(1, min = 5, max = 20)
#'   height <- runif(1, min = 5, max = 20)
#'   corners <- rbind(
#'     c(x - width / 2, y - height / 2),
#'     c(x + width / 2, y - height / 2),
#'     c(x + width / 2, y + height / 2),
#'     c(x - width / 2, y + height / 2),
#'     c(x - width / 2, y - height / 2)
#'   )
#'   sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(corners))), crs = crs)
#' })
#' buildings_sf <- do.call(rbind, list_of_buildings)
#' plot(sf::st_geometry(buildings_sf), col = 'slateblue3', border = NA, add = TRUE)
#'
#' # Merge with default settings
#' grid_merged <- desplim_merge(
#'   input_polygon = grid_poly,
#'   input_buildings = buildings_sf
#' )
#' print(grid_merged, n = nrow(grid_merged))
#' plot(sf::st_geometry(grid_merged), lwd = 4, border = 'tomato3', add = TRUE)
#' @export
desplim_merge <- function(
  input_polygons,
  input_buildings = NULL,
  compact_method = "desplim",
  compact_threshold = 0.7,
  compact_allow = 0.2,
  area_threshold = 10000,
  enclosed_threshold = 0.8,
  building_threshold = 5
) {
  if (!inherits(input_polygons, "sf")) {
    stop("Input polygons must be an sf object")
  }
  output_crs <- sf::st_crs(input_polygons)
  if (is.na(output_crs) && !is.null(input_buildings)) {
    output_crs <- sf::st_crs(input_buildings)
  }
  if (is.na(output_crs)) {
    warning("Neither of the input has a CRS")
  }
  empty_sf <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
  if (nrow(input_polygons) == 0) {
    warning("Input is empty")
    return(empty_sf)
  }
  if (!is.null(input_buildings)) {
    if (!inherits(input_buildings, "sf")) {
      stop("Input buildings must be an sf object.")
    }
    if (
      !is.na(sf::st_crs(input_buildings)) &&
        sf::st_crs(input_buildings) != output_crs
    ) {
      warning(
        "Input buildings has been transformed to the CRS of the input polygons"
      )
      input_buildings <- sf::st_transform(input_buildings, output_crs)
    }
    if (nrow(input_buildings) == 0) input_buildings <- NULL
  }
  if (attr(input_polygons, "sf_column") != "geometry") {
    input_polygons <- .desplim_rename_geom(input_polygons)
  }
  if (
    !is.null(input_buildings) &&
      attr(input_buildings, "sf_column") != "geometry"
  ) {
    input_buildings <- .desplim_rename_geom(input_buildings)
  }
  if (is.null(input_buildings)) {
    input_buildings <- sf::st_sf(geometry = sf::st_sfc(crs = output_crs))
    building_threshold <- 0
  }
  if (!compact_method %in% c("desplim", "polsby", "schwartz", "convex_hull")) {
    stop(
      "compact_method must be one of 'desplim', 'polsby', 'schwartz' or 'convex_hull'"
    )
  }
  N <- nrow(input_polygons)
  id_seq <- seq_len(N)
  id_chr_init <- as.character(id_seq)
  next_id <- N + 1L
  # Adjacency list keyed by stable character ID
  relate_raw <- sf::st_relate(input_polygons, pattern = "F***1****")
  relate <- stats::setNames(relate_raw, id_chr_init)
  relate_vec <- lengths(relate)
  borders <- stats::setNames(vector("list", N), id_chr_init)
  for (i in id_seq) {
    i_chr <- id_chr_init[i]
    neighbors <- relate[[i_chr]]
    if (length(neighbors) == 0) next
    neighbors_chr <- as.character(neighbors)
    new_neigh_mask <- !neighbors_chr %in% names(borders[[i_chr]])
    new_neigh <- neighbors[new_neigh_mask]
    new_neigh_chr <- neighbors_chr[new_neigh_mask]
    if (length(new_neigh) == 0) next
    computed <- suppressWarnings(as.numeric(sf::st_length(sf::st_intersection(
      input_polygons[i, ],
      input_polygons[new_neigh, ]
    ))))
    names(computed) <- new_neigh_chr
    borders[[i_chr]] <- c(borders[[i_chr]], computed)
    for (j in seq_along(new_neigh)) {
      j_chr <- new_neigh_chr[j]
      borders[[j_chr]] <- c(borders[[j_chr]], stats::setNames(computed[j], i_chr))
    }
  }
  # Named scalar vectors
  perimeter_vec <- stats::setNames(as.numeric(sf::st_perimeter(input_polygons)), id_chr_init)
  area_vec <- stats::setNames(as.numeric(sf::st_area(input_polygons)), id_chr_init)
  # Building counts
  input_polygons$temp_poly_id <- id_seq
  input_buildings$temp_bldg_id <- seq_len(nrow(input_buildings))
  building_vec_raw <- rep(0L, N)
  if (nrow(input_buildings) > 0) {
    overlaps_sf <- suppressWarnings(sf::st_intersection(input_buildings, input_polygons))
    if (nrow(overlaps_sf) > 0) {
      overlaps_sf$overlap_area <- as.numeric(sf::st_area(overlaps_sf))
      overlaps_df <- sf::st_drop_geometry(overlaps_sf)
      overlaps_df_ordered <- overlaps_df[
        order(overlaps_df$temp_bldg_id, -overlaps_df$overlap_area),
      ]
      building_assignments <- overlaps_df_ordered[
        !duplicated(overlaps_df_ordered$temp_bldg_id),
      ]
      if (nrow(building_assignments) > 0) {
        polygon_counts_table <- table(building_assignments$temp_poly_id)
        polygon_indices_with_buildings <- as.integer(names(polygon_counts_table))
        building_vec_raw[polygon_indices_with_buildings] <- as.vector(polygon_counts_table)
      }
    }
  }
  building_vec <- stats::setNames(as.numeric(building_vec_raw), id_chr_init)
  # Enclosed flags
  enclosed_vec <- stats::setNames(rep(FALSE, N), id_chr_init)
  for (i_chr in id_chr_init) {
    if (relate_vec[i_chr] == 1L) {
      neigh_chr <- as.character(relate[[i_chr]][1])
      blen <- borders[[i_chr]][neigh_chr]
      if (!is.na(blen) && blen > enclosed_threshold * perimeter_vec[i_chr]) {
        enclosed_vec[i_chr] <- TRUE
      }
    }
  }
  # Union list and compactness, keyed by stable character ID
  union_list <- stats::setNames(as.list(id_seq), id_chr_init)
  compactness_active <- FALSE
  compactness_iter <- 0L
  compactness_vec <- stats::setNames(rep(NA_real_, N), id_chr_init)
  compactness_merge <- stats::setNames(rep(FALSE, N), id_chr_init)
  # Local helpers (closures capture relate_vec from enclosing environment)
  compute_compactness <- function(poly_sf) {
    switch(compact_method,
      "desplim" = desplim_compactness(poly_sf)$compactness,
      "polsby" = redistmetrics::comp_polsby(plans = 1, shp = poly_sf),
      "schwartz" = redistmetrics::comp_schwartz(plans = 1, shp = poly_sf),
      "convex_hull" = redistmetrics::comp_ch(plans = 1, shp = poly_sf),
      1
    )
  }
  filter_compactness <- function(cm, cv) {
    n_allow <- floor(compact_allow * (length(cm) + sum(relate_vec == 0)))
    if (sum(cm, na.rm = TRUE) > n_allow && n_allow > 0) {
      scores_filter <- cv[cm & !is.na(cm)]
      if (length(scores_filter) > n_allow) {
        cutoff_score <- sort(scores_filter, decreasing = TRUE)[n_allow]
        cm[cm & !is.na(cv) & cv >= cutoff_score] <- FALSE
      }
    } else {
      cm[] <- FALSE
    }
    cm
  }
  check_enclosed <- function(id_c) {
    if (relate_vec[id_c] == 1L) {
      sole_chr <- as.character(relate[[id_c]][1])
      blen <- borders[[id_c]][sole_chr]
      !is.na(blen) && blen > enclosed_threshold * perimeter_vec[id_c]
    } else {
      FALSE
    }
  }
  # This loop uses a two-phase approach. Phase 1 merges polygons based on
  # area, building count, and enclosure. Only when no more of these merges
  # are possible is phase 2 activated to calculate and use compactness.
  while (TRUE) {
    active_ids_chr <- names(relate)
    area_merge <- area_vec[active_ids_chr] < area_threshold
    building_merge <- building_vec[active_ids_chr] < building_threshold
    # Initial merging conditions
    merge_conditions <- enclosed_vec[active_ids_chr] | area_merge | building_merge
    # Initial compactness activator
    if (!compactness_active && !any(relate_vec > 0 & merge_conditions)) {
      compactness_active <- TRUE
      compactness_iter <- 1L
      if (length(union_list) > 0) {
        compactness_vec[active_ids_chr] <- unlist(lapply(
          active_ids_chr,
          function(id_c) {
            compute_compactness(
              sf::st_sf(sf::st_union(input_polygons[union_list[[id_c]], ]))
            )
          }
        ))
        compactness_merge[active_ids_chr] <- compactness_vec[active_ids_chr] < compact_threshold
        compactness_merge <- filter_compactness(compactness_merge, compactness_vec)
      }
    }
    # Updated merging conditions in phase 2
    if (compactness_active) {
      merge_conditions <- compactness_merge[active_ids_chr] |
        enclosed_vec[active_ids_chr] |
        area_merge |
        building_merge
    }
    merge_vec <- (relate_vec > 0) & merge_conditions
    if (sum(merge_vec, na.rm = TRUE) == 0 || length(union_list) <= 1) break
    # Deterministically select polygon to merge
    select_ids_chr <- names(which(merge_vec))
    order_df <- data.frame(
      id = select_ids_chr,
      rel_val = relate_vec[select_ids_chr],
      enc_val = as.integer(enclosed_vec[select_ids_chr]),
      area_val = area_vec[select_ids_chr],
      stringsAsFactors = FALSE
    )
    ordered_candidates <- order_df[
      order(order_df$rel_val, -order_df$enc_val, order_df$area_val),
    ]
    uid_chr <- ordered_candidates$id[1]
    # Deterministically select polygon to merge with (largest shared border)
    uid_neighbors_int <- relate[[uid_chr]]
    uid_neighbors_chr <- as.character(uid_neighbors_int)
    nid_chr <- names(which.max(borders[[uid_chr]][uid_neighbors_chr]))
    new_id <- next_id
    new_chr <- as.character(new_id)
    next_id <- next_id + 1L
    # Compute new neighbors before modifying relate
    nid_neighbors_int <- relate[[nid_chr]]
    new_neighbors_int <- setdiff(
      union(uid_neighbors_int, nid_neighbors_int),
      c(as.integer(uid_chr), as.integer(nid_chr))
    )
    new_neighbors_chr <- as.character(new_neighbors_int)
    # union_list
    union_list[[new_chr]] <- sort(unique(c(union_list[[uid_chr]], union_list[[nid_chr]])))
    union_list[[uid_chr]] <- NULL
    union_list[[nid_chr]] <- NULL
    # area, building
    area_vec[new_chr] <- area_vec[uid_chr] + area_vec[nid_chr]
    area_vec <- area_vec[!names(area_vec) %in% c(uid_chr, nid_chr)]
    building_vec[new_chr] <- building_vec[uid_chr] + building_vec[nid_chr]
    building_vec <- building_vec[!names(building_vec) %in% c(uid_chr, nid_chr)]
    # perimeter
    shared_border_len <- borders[[uid_chr]][nid_chr]
    if (is.na(shared_border_len)) shared_border_len <- 0
    perimeter_vec[new_chr] <- perimeter_vec[uid_chr] + perimeter_vec[nid_chr] -
      2 * shared_border_len
    perimeter_vec <- perimeter_vec[!names(perimeter_vec) %in% c(uid_chr, nid_chr)]
    # borders
    uid_borders <- borders[[uid_chr]]
    nid_borders <- borders[[nid_chr]]
    combined_neigh_chr <- setdiff(
      union(names(uid_borders), names(nid_borders)),
      c(uid_chr, nid_chr)
    )
    new_borders_vec <- vapply(combined_neigh_chr, function(k_chr) {
      v_uid <- if (k_chr %in% names(uid_borders)) uid_borders[k_chr] else 0
      v_nid <- if (k_chr %in% names(nid_borders)) nid_borders[k_chr] else 0
      v_uid + v_nid
    }, numeric(1))
    names(new_borders_vec) <- combined_neigh_chr
    for (k_chr in combined_neigh_chr) {
      old_k <- borders[[k_chr]]
      borders[[k_chr]] <- c(
        old_k[!names(old_k) %in% c(uid_chr, nid_chr)],
        stats::setNames(new_borders_vec[k_chr], new_chr)
      )
    }
    borders[[new_chr]] <- new_borders_vec
    borders[[uid_chr]] <- NULL
    borders[[nid_chr]] <- NULL
    # relate
    relate[[new_chr]] <- new_neighbors_int
    for (k_chr in new_neighbors_chr) {
      old_k <- relate[[k_chr]]
      relate[[k_chr]] <- c(
        setdiff(old_k, c(as.integer(uid_chr), as.integer(nid_chr))),
        new_id
      )
    }
    relate[[uid_chr]] <- NULL
    relate[[nid_chr]] <- NULL
    relate_vec <- lengths(relate)
    # enclosed_vec
    enclosed_vec <- enclosed_vec[!names(enclosed_vec) %in% c(uid_chr, nid_chr)]
    enclosed_vec[new_chr] <- check_enclosed(new_chr)
    for (k_chr in new_neighbors_chr) {
      enclosed_vec[k_chr] <- check_enclosed(k_chr)
    }
    # compactness
    compactness_vec <- compactness_vec[!names(compactness_vec) %in% c(uid_chr, nid_chr)]
    compactness_merge <- compactness_merge[!names(compactness_merge) %in% c(uid_chr, nid_chr)]
    if (compactness_active) {
      compactness_iter <- compactness_iter + 1L
      new_poly_sf <- sf::st_sf(sf::st_union(input_polygons[union_list[[new_chr]], ]))
      compactness_vec[new_chr] <- compute_compactness(new_poly_sf)
      compactness_merge <- !is.na(compactness_vec) & compactness_vec < compact_threshold
      compactness_merge <- filter_compactness(compactness_merge, compactness_vec)
    } else {
      compactness_vec[new_chr] <- NA_real_
      compactness_merge[new_chr] <- FALSE
    }
  }
  final_ids <- names(union_list)
  polygon_union <- do.call(rbind, lapply(final_ids, function(id_c) {
    sf::st_as_sf(sf::st_union(input_polygons[union_list[[id_c]], ]))
  }))
  polygon_union <- .desplim_rename_geom(polygon_union)
  polygon_union$area <- area_vec[final_ids]
  polygon_union$compactness <- compactness_vec[final_ids]
  if (building_threshold > 0) {
    polygon_union$building <- building_vec[final_ids]
  }
  polygon_union
}