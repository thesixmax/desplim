#' Apply the DESPLIM algorithm to a set of input polygons and lines.
#' @param input_polygon object of class sf of type POLYGON to be merged.
#' @param input_lines object of class sf of type LINESTRING or MULTILINESTRING
#' to be used for splitting.
#' @param input_buildings object of class sf of type POLYGON or MULTIPOLYGON.
#' An optional sf object representing buildings which should be considered when
#' splitting and merging. Default is `NULL`.
#' @param line_type_identifier string; the name of the column in `input_lines`
#' which contains the line type. Default is `NULL`.
#' @param line_type_hierarchy list; hierarchical structure of line types used
#' for splitting. Default is `NULL`. All types can be accessed with `"all"`.
#' @param parallel logical; whether to run the split and merging steps in
#' parallel. If `TRUE`, uses the `future` package, where the number of workers
#' should be set using the `plan` argument. Default is `FALSE`.
#' @param max_iter integer; maximum number of split-merge iterations to perform
#' per hierarchy level. The algorithm repeats split-merge until the polygon set
#' no longer changes (convergence) or `max_iter` is reached. Default is `Inf`.
#' @param compact_allow_subsequent numerical; value of `compact_allow` passed to
#' `desplim_merge` from the second iteration onward. A higher value is more
#' lenient, reducing the risk of re-merging polygons created in earlier
#' iterations. Only applied when `max_iter > 1`. Default is `1` (compactness
#' does not trigger merges in subsequent iterations).
#' @param ... additional arguments passed to `desplim_split`, `desplim_merge`
#' and `desplim_connect_border`.
#' @return An sf object of POLYGONs resulting from the merge.
#' @details The function applies the DESPLIM algorithm to a set of input
#' polygons and lines. For each hierarchy level, split-merge is repeated until
#' convergence (the polygon set no longer changes between iterations) or
#' `max_iter` is reached. Convergence is determined by the number of output
#' polygons being equal to the previous iteration.
#' @note When using `parallel = TRUE`, each worker session will print the `sf`
#' startup message on first load. To suppress this, configure your `future`
#' plan with an initializer before calling this function:
#' ```r
#' future::plan(future::multisession, workers = n,
#'   initializer = function() suppressPackageStartupMessages(library(sf)))
#' ```
#' @export
desplim_split_merge <- function(
  input_polygon,
  input_lines,
  input_buildings = NULL,
  line_type_identifier = NULL,
  line_type_hierarchy = NULL,
  parallel = FALSE,
  max_iter = Inf,
  compact_allow_subsequent = 1,
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
    warning("Inputs have no CRS")
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
  if (!is.null(line_type_identifier)) {
    if (!is.character(line_type_identifier)) {
      stop("line_type_identifier must be a character")
    }
    if (length(line_type_identifier) != 1) {
      stop("line_type_identifier must be a single character entry")
    }
    if (!line_type_identifier %in% names(input_lines)) {
      stop("line_type_identifier must be a column name in input_lines")
    }
    if (is.null(line_type_hierarchy)) {
      warning("line_type_hierarchy is NULL, using 'all' as default")
    }
  }
  if (!is.null(line_type_hierarchy)) {
    if (is.null(line_type_identifier)) {
      stop("line_type_identifier must be set if line_type_hierarchy is set")
    }
    if (!is.list(line_type_hierarchy)) {
      stop("line_type_hierarchy must be a list")
    }
    if (!all(unlist(lapply(line_type_hierarchy, is.character)))) {
      stop("line_type_hierarchy must be a list of character entries")
    }
    if (
      !all(unlist(lapply(line_type_hierarchy, function(x) {
        x %in% unique(c(input_lines[[line_type_identifier]]), "all")
      })))
    ) {
      warning(
        "Some entries in line_type_hierarchy are not present in line_type_identifier"
      )
    }
    if (
      !any(unlist(lapply(line_type_hierarchy, function(x) {
        x %in% unique(c(input_lines[[line_type_identifier]]), "all")
      })))
    ) {
      stop(
        "At least one entry in line_type_hierarchy must be present in line_type_identifier"
      )
    }
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter < 1) {
    stop("max_iter must be a positive number")
  }
  if (
    !is.numeric(compact_allow_subsequent) ||
      length(compact_allow_subsequent) != 1 ||
      compact_allow_subsequent < 0 ||
      compact_allow_subsequent > 1
  ) {
    stop("compact_allow_subsequent must be a number between 0 and 1")
  }
  if (parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop("Package `future.apply` must be installed to use `parallel = TRUE`.")
    }
    if ("sequential" %in% class(future::plan())) {
      warning("`parallel` set to TRUE, but no `future` plan set. Defaulting to `parallel = FALSE`")
      parallel <- FALSE
    }
  }
  if (is.null(line_type_identifier)) {
    hierarchy <- list("all")
  } else {
    if (is.null(line_type_hierarchy)) {
      hierarchy <- list("all")
    } else {
      hierarchy <- line_type_hierarchy
    }
  }
  # Pre-compute argument lists once before the loop
  args <- list(...)
  split_arg_names <- names(formals(desplim_split))
  merge_arg_names <- names(formals(desplim_merge))
  border_arg_names <- names(formals(desplim_connect_border))
  valid_arg_names <- unique(c(split_arg_names, merge_arg_names, border_arg_names))
  unknown_args <- names(args)[!names(args) %in% valid_arg_names]
  if (length(unknown_args) > 0) {
    stop(paste0(
      "Unknown additional argument(s) provided: '",
      paste(unknown_args, collapse = "', '"),
      "'"
    ))
  }
  args_for_split <- args[names(args) %in% c(split_arg_names, border_arg_names)]
  args_for_merge <- args[names(args) %in% merge_arg_names]
  poly_proc <- input_polygon
  for (i in seq_along(hierarchy)) {
    if (is.null(line_type_identifier)) {
      lines_for_splitting <- input_lines
    } else {
      current_line_types <- unlist(hierarchy[1:i])
      if ("all" %in% current_line_types) {
        current_line_types <- unique(input_lines[[line_type_identifier]])
      }
      lines_for_splitting <- input_lines[
        input_lines[[line_type_identifier]] %in% current_line_types,
      ]
    }
    if (nrow(lines_for_splitting) == 0) {
      next
    }
    .process_chunk <- function(poly_chunk) {
      poly_split <- do.call(desplim_split, c(
        list(
          input_polygon = poly_chunk,
          input_lines = lines_for_splitting,
          input_buildings = input_buildings
        ),
        args_for_split
      ))
      .desplim_rename_geom(do.call(desplim_merge, c(
        list(
          input_polygons = poly_split,
          input_buildings = input_buildings
        ),
        args_for_merge
      )))
    }
    iter <- 0L
    repeat {
      prev_nrow <- nrow(poly_proc)
      if (iter > 0L) {
        args_for_merge[["compact_allow"]] <- compact_allow_subsequent
      }
      polygons_list <- split(poly_proc, seq_len(nrow(poly_proc)))
      if (parallel) {
        processed_list <- future.apply::future_lapply(
          polygons_list,
          .process_chunk,
          future.seed = TRUE,
          future.chunk.size = 1L
        )
      } else {
        processed_list <- lapply(polygons_list, .process_chunk)
      }
      valid_results <- processed_list[!vapply(processed_list, is.null, logical(1))]
      poly_proc <- do.call(rbind, valid_results)
      iter <- iter + 1L
      if (nrow(poly_proc) == prev_nrow || iter >= max_iter) break
    }
  }
  poly_proc
}