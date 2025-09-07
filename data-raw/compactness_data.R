### Required packages
require(sf)
require(redistmetrics)
require(dataverse)
require(archive)

### Path
path <- "./data-raw/"

### Get data
# Shapefiles
if (!file.exists(paste0(path, "both.shp"))) {
  file <- get_file(
    file = 4143644,
    server = "dataverse.harvard.edu"
  )
  writeBin(file, paste0(path, "both.7z"))
  archive_extract(paste0(path, "both.7z"), dir = path)
}

# Training data and training labels
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

### Load data
load(paste0(path, "training_data.RData"))
load(paste0(path, "training_labels.RData"))
districts_shape <- read_sf(paste0(path, "both.shp"))
districts_data <- (do.call(rbind, mylist))[do.call(rbind, mylist)$parts == 1, ]

### Filter districts
districts_filtered <- districts_shape[districts_shape$NAME %in% districts_data$district, ]
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
districts <- districts_merged[, c("NAME", "compactness", "geometry")]

### Calculate metrics
districts$id <- seq(1:nrow(districts))
districts$boyce <- comp_bc(plans = districts$id, shp = districts)
districts$box_reock <- comp_box_reock(plans = districts$id, shp = districts)
districts$hull <- comp_ch(plans = districts$id, shp = districts)
districts$len_wid <- comp_lw(plans = districts$id, shp = districts)
districts$polsby <- comp_polsby(plans = districts$id, shp = districts)
districts$reock <- comp_reock(plans = districts$id, shp = districts)
districts$schwartz <- comp_schwartz(plans = districts$id, shp = districts)
districts$skew <- comp_skew(plans = districts$id, shp = districts)
districts$sym_x <- comp_x_sym(plans = districts$id, shp = districts)
districts$sym_y <- comp_y_sym(plans = districts$id, shp = districts)

### Create model data and rescale compactness
districts_nogeom <- st_drop_geometry(districts)
districts_nogeom$compact <- 1 - (districts_nogeom$compactness / 100)
desplim_compactness_data <- districts_nogeom[, c(
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

### Write data
usethis::use_data(desplim_compactness_data, overwrite = TRUE)
