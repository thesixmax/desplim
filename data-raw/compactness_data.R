# Filename: data-raw/compactness_data.R
#
# This script creates the data used to model the DESPLIM compactness metric and
# vignettes-specific data. It should be run manually by the developer whenever
# the data needs to be regenerated.

# Load all required packages
require(sf)
require(redistmetrics)
require(dataverse)
require(archive)

# Path
path <- "./data-raw/"

# Get shapefiles
if (!file.exists(paste0(path, "both.shp"))) {
  file <- get_file(
    file = 4143644,
    server = "dataverse.harvard.edu"
  )
  writeBin(file, paste0(path, "both.7z"))
  archive_extract(paste0(path, "both.7z"), dir = path)
}

# Get training data and training labels
if (!file.exists(paste0(path, "training_data.RData"))) {
  download.file(
    "https://raw.githubusercontent.com/aaronrkaufman/compactness/master/data/training_data.RData",
    paste(path, "training_data.RData", sep = ""),
    mode = "wb"
  )
}
if (!file.exists(paste0(path, "training_labels.RData"))) {
  download.file(
    "https://raw.githubusercontent.com/aaronrkaufman/compactness/master/data/training_labels.RData",
    paste(path, "training_labels.RData", sep = ""),
    mode = "wb"
  )
}

# Load data
load(paste0(path, "training_data.RData"))
load(paste0(path, "training_labels.RData"))
districts_shape <- read_sf(paste0(path, "both.shp"))
districts_data <- (do.call(rbind, mylist))[do.call(rbind, mylist)$parts == 1, ]

# Filter districts
districts_filtered <- districts_shape[
  districts_shape$NAME %in% districts_data$district,
]
districts_merged <- merge(
  districts_filtered,
  train_labels,
  by.x = "NAME",
  by.y = "district",
  all.x = TRUE
)
districts_merged <- merge(
  districts_merged,
  districts_data,
  by.x = "NAME",
  by.y = "district",
  all.x = TRUE
)
districts_merged <- st_cast(districts_merged, "POLYGON", warn = FALSE)
districts_merged <- districts_merged[
  !duplicated(sf::st_geometry(districts_merged)),
]

# Rescale compactness and generate example district geometries
districts_merged$compact <- 1 - (districts_merged$compactness / 100)
kaufman_25 <- districts_merged[51:75, c("compact", "geometry")]

# Calculate metrics
districts_merged$id <- seq_len(nrow(districts_merged))
districts_merged$boyce <- comp_bc(
  plans = districts_merged$id,
  shp = districts_merged
)
districts_merged$box_reock <- comp_box_reock(
  plans = districts_merged$id,
  shp = districts_merged
)
districts_merged$hull <- comp_ch(
  plans = districts_merged$id,
  shp = districts_merged
)
districts_merged$len_wid <- comp_lw(
  plans = districts_merged$id,
  shp = districts_merged
)
districts_merged$polsby <- comp_polsby(
  plans = districts_merged$id,
  shp = districts_merged
)
districts_merged$reock <- comp_reock(
  plans = districts_merged$id,
  shp = districts_merged
)
districts_merged$schwartz <- comp_schwartz(
  plans = districts_merged$id,
  shp = districts_merged
)
districts_merged$skew <- comp_skew(
  plans = districts_merged$id,
  shp = districts_merged
)
districts_merged$sym_x <- comp_x_sym(
  plans = districts_merged$id,
  shp = districts_merged
)
districts_merged$sym_y <- comp_y_sym(
  plans = districts_merged$id,
  shp = districts_merged
)

# Create model data
districts_nogeom <- st_drop_geometry(districts_merged)
compact_train <- districts_nogeom[, c(
  "compact",
  "boyce",
  "box_reock",
  "hull",
  "len_wid",
  "polsby",
  "reock",
  "schwartz",
  "skew",
  "sym_x",
  "sym_y"
)]

# Write data
usethis::use_data(compact_train, overwrite = TRUE)
save(kaufman_25, file = "vignettes/kaufman_25.rda")
