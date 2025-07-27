#' Deterministically merge a set of polygons with possible paramters defined by
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
    output_crs <- sf::st_crs(NA)
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
        "Input buildings has been transformed to a the CRS of the input 
      polygons"
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
      "compact_method must be one of 'desplim', 'polsby', 'schwartz' or 
      'convex_hull'"
    )
  }
  # Calculate initial relationships and values
  poly_count <- seq_len(nrow(input_polygons))
  relate <- sf::st_relate(input_polygons, pattern = "F***1****")
  relate_vec <- lengths(relate)
  border_mat <- matrix(nrow = length(poly_count), ncol = length(poly_count))
  for (i in seq_along(poly_count)) {
    border_mat[i, relate[[i]]] <- ifelse(
      is.na(border_mat[i, relate[[i]]]),
      suppressWarnings(as.numeric(sf::st_length(sf::st_intersection(
        input_polygons[i, ],
        input_polygons[relate[[i]], ]
      )))),
      border_mat[i, relate[[i]]]
    )
    border_mat[relate[[i]], i] <- border_mat[i, relate[[i]]]
  }
  border_mat[is.na(border_mat)] <- 0
  perimeter_vec <- as.numeric(sf::st_perimeter(input_polygons))
  enclosed_vec <- rep(0, times = length(poly_count))
  for (i in seq_along(poly_count)) {
    if (lengths(relate)[i] == 1) {
      if (
        border_mat[i, relate[[i]][1]] > (enclosed_threshold * perimeter_vec[i])
      ) {
        enclosed_vec[i] <- 1
      }
    }
  }
  area_vec <- as.numeric(sf::st_area(input_polygons))
  input_polygons$temp_poly_id <- seq_len(nrow(input_polygons))
  input_buildings$temp_bldg_id <- seq_len(nrow(input_buildings))
  building_vec <- rep(0, times = nrow(input_polygons))
  if (nrow(input_buildings) > 0) {
    overlaps_sf <- suppressWarnings(sf::st_intersection(
      input_buildings,
      input_polygons
    ))
    if (nrow(overlaps_sf) > 0) {
      # Step 2: Calculate the area of each overlap
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
        polygon_indices_with_buildings <- as.numeric(names(
          polygon_counts_table
        ))
        building_vec[polygon_indices_with_buildings] <- as.vector(
          polygon_counts_table
        )
      }
    }
  }
  union_list <- as.list(poly_count)
  # Initial compactness
  compactness_active <- FALSE
  compactness_vec <- rep(NA_real_, length(poly_count))
  compactness_merge <- rep(0, length(poly_count))
  compactness_iter <- 0
  # This loop uses a two-phase approach. Phase 1 merges polygons based on
  # area, building count, and enclosure. Only when no more of these merges
  # are possible is phase 2 activated to calculate and use compactness.
  while (TRUE) {
    area_merge <- ifelse(area_vec < area_threshold, 1, 0)
    building_merge <- ifelse(building_vec < building_threshold, 1, 0)
    # Initial merging conditions
    merge_conditions <- (enclosed_vec == 1 |
      area_merge == 1 |
      building_merge == 1)
    # Initial compactness activator
    if (!compactness_active && !any(lengths(relate) > 0 & merge_conditions)) {
      compactness_active <- TRUE
      compactness_iter <- 1
      if (length(union_list) > 0) {
        compactness_vec <- unlist(lapply(union_list, function(x) {
          current_poly_sf <- sf::st_sf(sf::st_union(input_polygons[x, ]))
          switch(
            compact_method,
            "desplim" = desplim_compactness(current_poly_sf)$compactness,
            "polsby" = redistmetrics::comp_polsby(
              plans = 1,
              shp = current_poly_sf
            ),
            "schwartz" = redistmetrics::comp_schwartz(
              plans = 1,
              shp = current_poly_sf
            ),
            "convex_hull" = redistmetrics::comp_ch(
              plans = 1,
              shp = current_poly_sf
            ),
            1
          )
        }))
        compactness_merge <- ifelse(compactness_vec < compact_threshold, 1, 0)
        n_allow <- floor(
          compact_allow * (length(compactness_merge) + sum(relate_vec == 0))
        )
        if (sum(compactness_merge) > n_allow && n_allow > 0) {
          scores_filter <- compactness_vec[which(compactness_merge == 1)]
          if (length(scores_filter) > n_allow) {
            cutoff_score <- sort(scores_filter, decreasing = TRUE)[n_allow]
            compactness_merge[which(
              compactness_merge == 1 & compactness_vec >= cutoff_score
            )] <- 0
          }
        } else {
          compactness_merge <- rep(0, length(compactness_merge))
        }
      }
    }
    # Updated merging conditions in phase 2
    if (compactness_active) {
      merge_conditions <- (compactness_merge == 1) |
        (enclosed_vec == 1) |
        area_merge == 1 |
        building_merge == 1
    }
    merge_vec <- (lengths(relate) > 0) & merge_conditions
    merge_count <- sum(merge_vec)
    if (merge_count == 0 || length(union_list) <= 1) {
      break
    }
    # Deterministically select polygon to merge
    select_idx <- which(merge_vec == 1)
    order_df <- data.frame(
      idx = select_idx,
      rel_val = relate_vec[select_idx],
      enc_val = enclosed_vec[select_idx],
      area_val = area_vec[select_idx]
    )
    ordered_candidates <- order_df[
      order(order_df$rel_val, -order_df$enc_val, order_df$area_val),
    ]
    union_id <- ordered_candidates$idx[1]
    # Deterministically select polygon to merge with
    neigh_union_id <- relate[[union_id]]
    neigh_id <- neigh_union_id[which.max(border_mat[union_id, neigh_union_id])]
    # Update relationships
    union_list[[length(union_list) + 1]] <- sort(unique(c(
      union_list[[union_id]],
      union_list[[neigh_id]]
    )))
    union_list <- union_list[-c(neigh_id, union_id)]
    area_vec <- c(
      area_vec[-c(neigh_id, union_id)],
      sum(area_vec[c(neigh_id, union_id)])
    )
    building_vec <- c(
      building_vec[-c(neigh_id, union_id)],
      sum(building_vec[c(neigh_id, union_id)])
    )
    n_relate <- length(relate)
    relate[[n_relate + 1]] <- sort(unique(c(
      relate[[union_id]],
      relate[[neigh_id]]
    )))
    relate[[n_relate + 1]] <- relate[[n_relate + 1]][
      -which(relate[[n_relate + 1]] %in% c(union_id, neigh_id))
    ]
    relate <- lapply(relate, function(x) {
      vec <- replace(x, which(x %in% c(union_id, neigh_id)), n_relate + 1)
      vec <- ifelse(
        vec > union_id & vec > neigh_id,
        vec - 2,
        ifelse(vec > union_id | vec > neigh_id, vec - 1, vec)
      )
      sort(unique(vec))
    })
    relate <- relate[-c(neigh_id, union_id)]
    relate_vec <- lengths(relate)
    shared_border_len <- border_mat[union_id, neigh_id]
    new_perimeter <- perimeter_vec[union_id] +
      perimeter_vec[neigh_id] -
      (2 * shared_border_len)
    perimeter_vec <- c(perimeter_vec[-c(neigh_id, union_id)], new_perimeter)
    # Critical to evaluate border matrix before updating enclosed
    if (length(union_list) > 1) {
      border_mat_tmp <- rbind(
        border_mat[-c(neigh_id, union_id), ],
        colSums(border_mat[c(neigh_id, union_id), ])
      )
      border_mat <- cbind(
        border_mat_tmp[, -c(neigh_id, union_id)],
        rowSums(border_mat_tmp[, c(neigh_id, union_id)])
      )
      border_mat[nrow(border_mat), ncol(border_mat)] <- 0
    }
    enclosed_vec <- c(enclosed_vec[-c(neigh_id, union_id)], 0)
    if (relate_vec[length(relate_vec)] == 1) {
      if (
        border_mat[nrow(border_mat), relate[[length(relate)]][1]] >
          enclosed_threshold * perimeter_vec[length(perimeter_vec)]
      ) {
        enclosed_vec[length(enclosed_vec)] <- 1
      }
    }
    # Evaluate neighbours of the merged polygon
    if (length(relate[[length(relate)]]) > 0) {
      relate_tmp <- relate[[length(relate)]]
      for (i in seq_along(relate_tmp)) {
        enclosed_vec[relate_tmp[i]] <- 0
        if (length(relate[[relate_tmp[i]]]) == 1) {
          if (
            border_mat[relate_tmp[i], relate[[relate_tmp[i]]][1]] >
              enclosed_threshold * perimeter_vec[relate_tmp[i]]
          ) {
            enclosed_vec[relate_tmp[i]] <- 1
          }
        }
      }
    }
    compactness_vec <- compactness_vec[-c(neigh_id, union_id)]
    compactness_merge <- compactness_merge[-c(neigh_id, union_id)]
    # Update compactness for the merged polygon when compactness is active
    if (compactness_active) {
      compactness_iter <- compactness_iter + 1
      new_poly_sf <- sf::st_sf(sf::st_union(input_polygons[
        union_list[[length(union_list)]],
      ]))
      new_compactness_val <- switch(
        compact_method,
        "desplim" = desplim_compactness(new_poly_sf)$compactness,
        "polsby" = redistmetrics::comp_polsby(plans = 1, shp = new_poly_sf),
        "schwartz" = redistmetrics::comp_schwartz(plans = 1, shp = new_poly_sf),
        "convex_hull" = redistmetrics::comp_ch(plans = 1, shp = new_poly_sf),
        1
      )
      compactness_vec <- c(compactness_vec, new_compactness_val)
      compactness_merge <- ifelse(
        !is.na(compactness_vec) & compactness_vec < compact_threshold,
        1,
        0
      )
      n_allow <- floor(
        compact_allow * (length(compactness_merge) + sum(relate_vec == 0))
      )
      if (sum(compactness_merge == 1) > n_allow && n_allow > 0) {
        scores_filter <- compactness_vec[which(compactness_merge == 1)]
        if (length(scores_filter) > n_allow) {
          cutoff_score <- sort(scores_filter, decreasing = TRUE)[n_allow]
          compactness_merge[which(
            compactness_merge == 1 & compactness_vec >= cutoff_score
          )] <- 0
        }
      } else {
        compactness_merge <- rep(0, length(compactness_merge))
      }
    } else {
      compactness_vec <- c(compactness_vec, NA_real_)
      compactness_merge <- c(compactness_merge, 0)
    }
  }
  polygon_union <- lapply(union_list, function(x) {
    sf::st_as_sf(sf::st_union(input_polygons[x, ]))
  })
  polygon_union <- do.call(rbind, polygon_union)
  polygon_union <- .desplim_rename_geom(polygon_union)
  polygon_union$area <- area_vec
  polygon_union$compactness <- compactness_vec
  if (building_threshold > 0) {
    polygon_union$building <- building_vec
  }
  return(polygon_union)
}
