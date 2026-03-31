# Filename: data-raw/compactness_data.R
#
# This script creates the data used to model the DESPLIM compactness metric and
# vignettes-specific data. It should be run manually by the developer whenever
# the data needs to be regenerated.

library(sf)
library(redistmetrics)
library(dataverse)
library(archive)

path <- "./data-raw/"

# Download shapefiles
if (!file.exists(paste0(path, "both.shp"))) {
  file <- get_file(file = 4143644, server = "dataverse.harvard.edu")
  writeBin(file, paste0(path, "both.7z"))
  archive_extract(paste0(path, "both.7z"), dir = path)
}

# Download training data and labels
base_url <- "https://raw.githubusercontent.com/aaronrkaufman/compactness/master/data/"
for (fname in c("training_data.RData", "training_labels.RData")) {
  dest <- paste0(path, fname)
  if (!file.exists(dest))
    download.file(paste0(base_url, fname), dest, mode = "wb")
}

# Load source data
load(paste0(path, "training_data.RData"))
load(paste0(path, "training_labels.RData"))
districts_shape <- read_sf(paste0(path, "both.shp"))
districts_data  <- do.call(rbind, mylist)
districts_data  <- districts_data[districts_data$parts == 1, ]

# Filter and merge
districts_merged <- districts_shape[districts_shape$NAME %in% districts_data$district, ]
districts_merged <- merge(districts_merged, train_labels,   by.x = "NAME", by.y = "district", all.x = TRUE)
districts_merged <- merge(districts_merged, districts_data, by.x = "NAME", by.y = "district", all.x = TRUE)
districts_merged <- st_cast(districts_merged, "POLYGON", warn = FALSE)
districts_merged <- districts_merged[!duplicated(sf::st_geometry(districts_merged)), ]

# Rescale compactness and extract example geometries
districts_merged$compact <- 1 - (districts_merged$compactness / 100)
kaufman_25 <- districts_merged[51:75, c("compact", "geometry")]

# Calculate compactness metrics
districts_merged$id <- seq_len(nrow(districts_merged))
metrics <- list(
  boyce     = comp_bc,
  box_reock = comp_box_reock,
  hull      = comp_ch,
  len_wid   = comp_lw,
  polsby    = comp_polsby,
  skew      = comp_skew,
  sym_x     = comp_x_sym,
  sym_y     = comp_y_sym
)
for (col in names(metrics)) {
  districts_merged[[col]] <- metrics[[col]](
    plans = districts_merged$id,
    shp   = districts_merged
  )
}

# Create and save model data
compact_train <- st_drop_geometry(districts_merged)[, c("compact", names(metrics))]

usethis::use_data(compact_train, overwrite = TRUE)
save(kaufman_25, file = "vignettes/kaufman_25.rda")
