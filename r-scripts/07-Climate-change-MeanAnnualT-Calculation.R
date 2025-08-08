links <- readLines("envidatS3paths.txt")

# direction
# dir.create("./data/chelsa_monthly", showWarnings = FALSE)
# Remove leading/trailing whitespace from all links
# links <- trimws(links)
# # Download-loop
# for (link in links) {
#   dest <- file.path("./data/chelsa_monthly", basename(link))
#   download.file(link, destfile = dest, mode = "wb", quiet = FALSE)
# }
# # Download nur, wenn Datei fehlt oder zu klein ist
# for (link in links) {
#   dest <- file.path("data/chelsa_monthly", basename(link))
#   
#   # Datei existiert, aber ist unvollständig (<90 MB)
#   if (!file.exists(dest) || file.info(dest)$size < 90 * 1024^2) {
#     message("Downloading: ", basename(link))
#     tryCatch({
#       download.file(link, destfile = dest, mode = "wb", quiet = FALSE)
#     }, error = function(e) {
#       message("❌ Fehler bei ", basename(link), ": ", e$message)
#     })
#   } else {
#     message("✔ Already exists: ", basename(link))
#   }
# }
# CHELSA Climate Change Processing for Germany (Grid-based)

# Load libraries and setup
source("./r-scripts/00-preamble.R")

# Load boundaries and target grid (5×5 km)
de_states <- st_read("https://raw.githubusercontent.com/isellsoap/deutschlandGeoJSON/main/2_bundeslaender/2_hoch.geo.json")
grid_sf_coverage_all <- st_read("./data/Protected-areas/grid_sf_protectionstatus.gpkg")

# Project grid and states
grid_plot_proj <- st_transform(grid_sf_coverage_all, crs = 25832)
de_states_proj <- st_transform(de_states, crs = st_crs(grid_plot_proj))

# Use the grid itself as clipping mask
grid_vect <- vect(grid_plot_proj)

# Check CRS of CHELSA tmin file
r <- rast("./data/chelsa_monthly/tmin/CHELSAcruts_tmin_4_1960_V.1.0.tif")
grid_vect <- project(grid_vect, crs(r))  # Reproject grid to match CHELSA (EPSG:4326)
rm(r)

# List tmin and tmax CHELSA files
files_tmin <- list.files("./data/chelsa_monthly/tmin", pattern = "\\.tif$", full.names = TRUE)
files_tmax <- list.files("./data/chelsa_monthly/tmax", pattern = "\\.tif$", full.names = TRUE)

# Define time periods
years_T1 <- "196[0-9]|197[0-9]|198[0-7]"   # 1960–1987
years_T3 <- "199[7-9]|200[0-9]|201[0-6]"   # 1997–2016

# Filter for relevant growing-season (April–September) files
files_tmin_T1 <- files_tmin[grepl(years_T1, files_tmin)]
files_tmax_T1 <- files_tmax[grepl(years_T1, files_tmax)]
files_tmin_T3 <- files_tmin[grepl(years_T3, files_tmin)]
files_tmax_T3 <- files_tmax[grepl(years_T3, files_tmax)]

# Function to load, crop and mask rasters to the grid
load_crop_mask <- function(filelist, mask) {
  rasters <- lapply(filelist, function(f) {
    message("⏳ Loading: ", f)
    tryCatch({
      r <- rast(f)
      r_crop <- crop(r, mask)
      mask(r_crop, mask)
    }, error = function(e) {
      message("❌ Error in file: ", f)
      message("   ", e$message)
      return(NULL)
    })
  })
  valid_rasters <- Filter(Negate(is.null), rasters)
  if (length(valid_rasters) == 0) stop("No valid rasters found.")
  rast(valid_rasters)
}

# Load, crop, and mask raster stacks
r_stack_tmin_T1 <- load_crop_mask(files_tmin_T1, grid_vect)
r_stack_tmin_T3 <- load_crop_mask(files_tmin_T3, grid_vect)
r_stack_tmax_T1 <- load_crop_mask(files_tmax_T1, grid_vect)
r_stack_tmax_T3 <- load_crop_mask(files_tmax_T3, grid_vect)

# Manually set -32768 as NA in all temperature stacks
r_stack_tmin_T1[r_stack_tmin_T1 == -32768] <- NA
r_stack_tmax_T1[r_stack_tmax_T1 == -32768] <- NA
r_stack_tmin_T3[r_stack_tmin_T3 == -32768] <- NA
r_stack_tmax_T3[r_stack_tmax_T3 == -32768] <- NA


# Convert CHELSAcruts temperature values from °C * 10 to °C
r_stack_tmin_T1 <- r_stack_tmin_T1 / 10
r_stack_tmax_T1 <- r_stack_tmax_T1 / 10
r_stack_tmin_T3 <- r_stack_tmin_T3 / 10
r_stack_tmax_T3 <- r_stack_tmax_T3 / 10

# Calculate mean monthly temperature
r_stack_tmean_T1 <- (r_stack_tmin_T1 + r_stack_tmax_T1) / 2
r_stack_tmean_T3 <- (r_stack_tmin_T3 + r_stack_tmax_T3) / 2

# Calculate growing season mean for each period
mean_tmean_T1 <- app(r_stack_tmean_T1, mean, na.rm = TRUE)
mean_tmean_T3 <- app(r_stack_tmean_T3, mean, na.rm = TRUE)

# Temperature change raster (T3 - T1)
delta_tmean <- mean_tmean_T3 - mean_tmean_T1

# Save to file (optional)
writeRaster(delta_tmean, "./data/processed/delta_tmean_DE.tif", overwrite = TRUE)
delta_tmean <- rast("./data/processed/delta_tmean_DE.tif")


# Reproject to match analysis grid (5x5 km)
delta_tmean_proj <- project(delta_tmean, st_crs(grid_plot_proj)$wkt)

# Extract mean temperature change per cell
grid_vect <- vect(grid_plot_proj)
temp_change_vals <- terra::extract(delta_tmean_proj, grid_vect, fun = mean, na.rm = TRUE)

# Join result to grid
grid_plot_proj$delta_tmean <- temp_change_vals[, 2]

# save
st_write(grid_plot_proj, "./data/processed/grid_with_climatechange.gpkg", delete_dsn = TRUE)

grid_plot_proj %>%
  st_drop_geometry() %>%
  write.csv("./data/processed/grid_with_climatechange.csv", row.names = FALSE)

# Visualization

tempchange <- ggplot() +
  geom_sf(data = grid_plot_proj, aes(fill = delta_tmean), color = "grey80", size = 0.1) +
  scale_fill_gradientn(
    colours = c("lightyellow", "orange", "darkred"),
    name = "Mean ΔT [°C]",
    limits = c(min(grid_plot_proj$delta_tmean, na.rm = TRUE), max(grid_plot_proj$delta_tmean, na.rm = TRUE)),
    oob = scales::squish,
    guide = guide_colorbar(
      direction = "vertical",
      barheight = unit(10, "cm"),
      barwidth = unit(1, "cm"),
      title.position = "top",
      title.vjust = 3
    )
  ) +
  geom_sf(data = de_states_proj, fill = NA, color = "grey10", linewidth=1.1, alpha= 0.85) +
  theme_minimal(base_family = "roboto_condensed") +
  # Coordinate settings (no datum displayed)
  coord_sf(datum = NA, expand = FALSE) +
  
  # Base theme settings
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    text = element_text(family = "roboto_condensed"),
    
    legend.title = element_text(size = 44),
    legend.text = element_text(size = 40),
    plot.margin = margin(5, 5, 5, 5)
  )

tempchange
ggsave("./figures/temperaturechange.png", plot = tempchange,
       width = 11.5, height = 11.5, dpi = 200, units = "in", bg="white")

# # Debug summary
# summary(grid_plot_proj$delta_tmean)
# hist(grid_plot_proj$delta_tmean)
# 
# 
# summary(grid_plot_proj$delta_tmean)
# hist(grid_plot_proj$delta_tmean)
# summary(mean_tmean_T1)
# summary(mean_tmean_T3)
# 
# range(values(r_stack_tmin_T1), na.rm = TRUE)
# hist(values(r_stack_tmin_T1), breaks = 10000)  # Auffällige Peaks?
# r <- rast("./data/chelsa_monthly/tmin/CHELSAcruts_tmin_4_1960_V.1.0.tif")
# names(r)
# terra::sources(r)
# r <- rast(f)[[1]]  # wenn nur Layer 1 Temperatur ist
# values(r)[1:10]  # Nur zur Probe
# summary(r)
