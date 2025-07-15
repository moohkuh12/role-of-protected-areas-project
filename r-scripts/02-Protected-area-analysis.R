# =============================================================
# 03_protected_area_analysis.R
# =============================================================
# Purpose:
#   - Remove protected areas outside Germany and marine-only areas
#   - Intersect grid cells with protected areas
#   - Calculate protection coverage per grid cell and classify protection status
#   - Generate visual summaries (bar charts, pie charts)
#
# Author: Marie Englert
# Date: 02.04.2025
# =============================================================

# SETUP ----
source("./r-scripts/00-preamble.R")

# PART 1: Remove marine protected areas and those outside Germany ----
# Load country geometry from natural earth dataset and simplify its geometry for faster processing.
country <- ne_countries(scale = "medium", country = "Germany", returnclass = "sf")
country <- st_simplify(country, dTolerance = 0.01)

# Load the shapefiles for protected areas (WDPA) with different processing levels (0, 1, 2).
pa_shp1 <- st_read("./data/WDPA_WDOECM_Jan2025_Public_DEU_shp/WDPA_WDOECM_Jan2025_Public_DEU_shp_0/WDPA_WDOECM_Jan2025_Public_DEU_shp-polygons.shp")
pa_shp2 <- st_read("./data/WDPA_WDOECM_Jan2025_Public_DEU_shp/WDPA_WDOECM_Jan2025_Public_DEU_shp_1/WDPA_WDOECM_Jan2025_Public_DEU_shp-polygons.shp")
pa_shp3 <- st_read("./data/WDPA_WDOECM_Jan2025_Public_DEU_shp/WDPA_WDOECM_Jan2025_Public_DEU_shp_2/WDPA_WDOECM_Jan2025_Public_DEU_shp-polygons.shp")

# Simplify the geometries for all three shapefiles (increases performance).
#pa_shp1 <- st_simplify(pa_shp1, dTolerance = 0.1)
#pa_shp2 <- st_simplify(pa_shp2, dTolerance = 0.1)
#pa_shp3 <- st_simplify(pa_shp3, dTolerance = 0.1)

# Filter out marine protected areas (MARINE == "2") and one World Heritage site (WDPAID 903141).
# Marine only = 2, Terrestrial only = 0, Both = 1
pa1_terrestrial <- pa_shp1 %>% filter(MARINE != "2")
pa2_terrestrial <- pa_shp2 %>% filter(MARINE != "2" & WDPAID != 903141)  # This is a single WH site
pa3_terrestrial <- pa_shp3 %>% filter(MARINE != "2")

rm(pa_shp1, pa_shp2, pa_shp3)  # Remove original shapefiles to free up memory

# Combine all cleaned terrestrial shapefiles into one dataset.
PAs <- bind_rows(pa1_terrestrial, pa2_terrestrial, pa3_terrestrial)
rm(pa1_terrestrial, pa2_terrestrial, pa3_terrestrial)  # Remove intermediate variables

# Optional: save the final shapefile or CSV.
# write.csv(PAs, "./data/protected_areas_130325.csv", row.names = FALSE)
# st_write(PAs, "./data/Protected-areas/protected_areas-raw_020425.shp")
PAs <- st_read("./data/Protected-areas/protected_areas-raw_020425.shp")

# PART 2: Load and prepare harmonized red-listed species data (sMon) ----
# These steps are necessary as we want to create unique ids for the grid cells where red list species had occurence records

# Load the harmonized red list species (sMon) dataset (created in r-script "taxonomic harmonization")
red_listed_smon <- read.csv("./data/TaxonHarm/harmonized_redlist_smon_data_020425.csv")

# Load and filter the whole sMon occurrence probability (OP) data
# - Remove intermediate time step 2 (only keep baseline and latest resurvey)

op_data <- bind_rows(
  read_csv("./data/RawData/1875_9_1875_2_Modelled_OPs_incl_sd_pt_1.csv") %>%
    filter(Timestep %in% c(1,3)),
  read_csv("./data/RawData/1875_9_1875_2_Modelled_OPs_incl_sd_pt_2.csv") %>%
    filter(Timestep %in% c(1,3)),
  read_csv("./data/RawData/1875_9_1875_2_Modelled_OPs_incl_sd_pt_3.csv") %>%
    filter(Timestep %in% c(1,3)),
  read_csv("./data/RawData/1875_9_1875_2_Modelled_OPs_incl_sd_pt_4.csv") %>%
    filter(Timestep %in% c(1,3))
)

duplikate2 <- op_data %>%
  count(MTB_Q, TaxonName, Period) %>%
  filter(n > 1)

# Check structure of the loaded OP dataset
str(op_data)

# Combine the occurrence probability data with the harmonized red list dataset 
# - Keep only threatened species
merged_data <- op_data %>%
  inner_join(red_listed_smon, by = "TaxonName")

# inner join with taxon name, because original op_data still has unharmonized species names 
# - get rid of the unharmonized species names in the next step
merged_sMon <- merged_data %>%
  select(TaxonName = TaxonName_cleaned, everything(), -TaxonName, -wcvp_status, wcvp_accepted_id)

# Save the merged dataset
# write_csv(merged_sMon, "./data/sMon/red_listed_sMon_occprobs020425.csv") 


# Load this File to avoid running the previous code again
merged_sMon <- read.csv("./data/sMon/red_listed_sMon_occprobs020425.csv")

unique_species_count <- n_distinct(merged_sMon$TaxonName) #863 species

# PART 3: Define grid cells and match spatially ----
# Create unique IDs for each latitude/longitude pair
# They need to be rounded, because otherwise they are counted double
lat_long_ids <- merged_sMon %>%
  mutate(
    Latitude = round(Latitude, 5),
    Longitude = round(Longitude, 5)
  ) %>%
  select(Latitude, Longitude, MTB_Q) %>%
  distinct() %>%
  mutate(id = row_number())

lat_long_ids_sf <- st_as_sf(lat_long_ids, coords = c("Longitude", "Latitude"), crs = 4326)
# Save the lat_long_ids_sf shapefile
# st_write(lat_long_ids_sf, "./data/landcover_analysis/lat_long_ids_sf.shp")

# Load the MTBQ grid shapefile
grid_sf <- st_read("./data/landcover_analysis/MTBQ/MTBQ_31467.shp")
#grid_sf_WGS84 <- st_transform(grid_sf, crs = 4326)
grid_proj <- st_transform(grid_sf, crs = 25832)


# PART 4: Calculate area and intersect grid with PAs ----

# 1.1) Calculate the area of each grid cell (store it in a new column)
grid_proj <- grid_proj %>%
  mutate(cell_area = st_area(.))

# Join the grid with latitude/longitude IDs using spatial join (st_contains)
lat_long_ids_proj <- st_transform(lat_long_ids_sf, crs = st_crs(grid_proj))

grid_joined <- st_join(grid_proj, lat_long_ids_proj, join = st_contains)


# Select only the required columns: id, MTB_Q, cell_area, and geometry
grid_joined <- grid_joined %>% select(id, MTB_Q, cell_area, geometry)

# Optionally, write the joined grid to a shapefile:
# st_write(grid_joined, "./data/landcover_analysis/grid_joined.shp")

# Load grid
grid_joined <- st_read("./data/landcover_analysis/grid_joined.shp")

# Transform PAs to same CRS
PAs <- st_transform(PAs, st_crs(grid_joined))

# Calculate PA areas
PA_areas <- PAs %>%
  mutate(pa_area = st_area(.),
         GIS_T_AREA = GIS_AREA - GIS_M_AREA)

# Intersect grid with protected areas (this takes time)
intersected <- st_intersection(grid_joined, PA_areas)

# Save intermediate file
st_write(intersected, "./data/Protected-areas/Intermediate/intersected_protected_areas.shp")

# Reload intersected data
intersected <- st_read("./data/Protected-areas/Intermediate/intersected_protected_areas.shp")

intersected %>%
  st_set_geometry(NULL) %>%
  filter(IUCN_CAT == "VI") %>%
  count(DESIG, sort = TRUE)

# STEP 1: Assign numeric protection categories and flag zones with active management
# ------------------------------------------------------------------------------
# Each protected area fragment is assigned a numeric code that reflects its legal protection level.
# Categories Ia–V follow the official IUCN classification (1–5).
# Natura 2000 areas are split into Habitats Directive (6) and Birds Directive (7).
# UNESCO biosphere reserves receive category 8.
# National biosphere zones (core, buffer, transition) receive category 9.
# Not Assigned or Not Applicable categories are grouped as 10.
# All other or unexpected types are assigned category 11 (fallback).
# Additionally, a binary flag 'has_management' indicates whether the area is under some form
# of active or regulated management (e.g. Natura 2000, biosphere buffer zones, IUCN IV–V).

# Large placeholder polygons for entire biosphere reserves (DESIG == "Biosphärenreservat")
# are excluded from the analysis to avoid overweighting uninformative spatial hulls.

intersected_filtered <- intersected %>%
  mutate(
    IUCN_CAT_numeric = case_when(
      IUCN_CAT %in% c("Ia", "Ib") ~ 1,
      IUCN_CAT == "II" ~ 2,
      IUCN_CAT == "III" ~ 3,
      IUCN_CAT == "IV" ~ 4,
      IUCN_CAT == "V" ~ 5,
      IUCN_CAT == "Not Reported" & DESIG == "Special Areas of Conservation (Habitats Directive)" ~ 6,
      IUCN_CAT == "Not Reported" & DESIG == "Special Protection Area (Birds Directive)" ~ 7,
      DESIG == "UNESCO-MAB Biosphere Reserve" ~ 8,
      grepl("Biosphärenreservat - (Pflegezone|Entwicklungszone|Kernzone)", DESIG) ~ 9,
      IUCN_CAT %in% c("Not Applicable", "Not Assigned") ~ 10,
      TRUE ~ 11  # fallback for unclassified/mixed
    ),
    
    # Flag for management
    has_management = case_when(
      IUCN_CAT_numeric %in% c(4, 5, 6, 7) ~ TRUE,
      grepl("Pflegezone|Entwicklungszone", DESIG, ignore.case = TRUE) ~ TRUE,
      DESIG == "UNESCO-MAB Biosphere Reserve" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  
  # Exclude large uninformative parent Biosphärenreservat hulls
  filter(!(DESIG == "Biosphärenreservat"))

# Create area-weighted management score per grid cell
management_weighted <- intersected_filtered %>%
  group_by(id) %>%
  summarise(
    mean_mgmt = sum(overlap_area * has_management, na.rm = TRUE) / sum(overlap_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dominant_management = mean_mgmt > 0.5
  )


# STEP 2: Split by grid cell
intersected_split <- split(intersected_filtered, intersected_filtered$id)

# STEP 3: Remove internal overlaps per cell
intersected_union <- map_dfr(intersected_split, function(df) {
  union_geom <- st_union(df)
  tibble(id = unique(df$id), geometry = union_geom)
}) %>% st_as_sf(crs = st_crs(intersected))

# STEP 4: Clean coverage fraction using dissolved geometries
intersected_union_df <- intersected_union %>%
  mutate(geometry_pa = geometry) %>%
  st_set_geometry(NULL) %>%
  select(id, geometry_pa)

grid_cov_area <- grid_joined %>%
  left_join(intersected_union_df, by = "id") %>%
  mutate(
    cell_area = st_area(geometry),
    pa_area_clean = if_else(
      !map_lgl(geometry_pa, st_is_empty),
      st_area(st_sfc(geometry_pa, crs = st_crs(grid_joined))),
      units::set_units(0, m^2)
    ),
    cov_frac = as.numeric(pa_area_clean / cell_area)
  )

# STEP 5: Add area of each PA fragment
intersected_filtered <- intersected_filtered %>%
  mutate(overlap_area = as.numeric(st_area(.)))

# Optional: Save for checking
st_write(intersected_filtered, "./data/Protected-areas/intersected_filtered.gpkg")

# STEP 6: Calculate area-weighted IUCN protection class per grid cell
#
# For each grid cell (id), an area-weighted average of the assigned protection category
# is calculated across all overlapping protected area fragments.
# The resulting mean score is not simply rounded, but classified using defined intervals:
# e.g., [4,5) = IUCN IV, [8,9) = UNESCO Biosphere, [9,10) = Biosphere Reserve (Zoned).
# This avoids misclassification in mixed-protection cases (e.g. 90% IUCN 10 + 10% IUCN 4)
# and allows separating truly unprotected cells from those with undefined or unclear categories.
# The final categorical label 'IUCN_CAT_final' reflects the dominant protection type in a cell.

iucn_weighted <- intersected_filtered %>%
  group_by(id) %>%
  summarise(
    mean_weighted_iucn = sum(IUCN_CAT_numeric * overlap_area, na.rm = TRUE) / sum(overlap_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    IUCN_CAT_numeric_rounded = floor(mean_weighted_iucn),
    IUCN_CAT_final = case_when(
      mean_weighted_iucn >= 1 & mean_weighted_iucn < 2 ~ "Ia/Ib",
      mean_weighted_iucn >= 2 & mean_weighted_iucn < 3 ~ "II",
      mean_weighted_iucn >= 3 & mean_weighted_iucn < 4 ~ "III",
      mean_weighted_iucn >= 4 & mean_weighted_iucn < 5 ~ "IV",
      mean_weighted_iucn >= 5 & mean_weighted_iucn < 6 ~ "V",
      mean_weighted_iucn >= 6 & mean_weighted_iucn < 7 ~ "Habitats Directive (Natura 2000)",
      mean_weighted_iucn >= 7 & mean_weighted_iucn < 8 ~ "Birds Directive (Natura 2000)",
      mean_weighted_iucn >= 8 & mean_weighted_iucn < 9 ~ "Biosphere Reserve (UNESCO)",
      mean_weighted_iucn >= 9 & mean_weighted_iucn < 10 ~ "Biosphere Reserve (Zoned)",
      mean_weighted_iucn >= 10 & mean_weighted_iucn < 11 ~ "Not Assigned/Applicable",
      mean_weighted_iucn >= 11 ~ "Mixed/Undefined Protected",
      TRUE ~ "Unprotected"
    )
  )
# For each grid cell (id), we calculate an area-weighted protection value that reflects the 
# average level of legal protection based on the overlapping protected area fragments.
# Each protected area is first translated into a numeric IUCN score (1 = strictest, 9 = least defined).
# The protection score is weighted by the area of overlap within the grid cell to reflect 
# the spatial dominance of each category.
# Instead of rounding the weighted value (which could artificially favor high categories due to small fragments),
# we classify the result using defined intervals (e.g. [4,5) = IUCN IV, [8,9) = Birds Directive).
# This avoids overinterpreting mixed or uncertain cases and allows us to assign a final protection class
# to each grid cell, including categories such as "Not Assigned" or "Mixed/Undefined Protected"
# for cells where no clear classification is possible.



# determine dominant designation year of largest PA in cell
dominant_year <- intersected_filtered %>%
  group_by(id, STATUS_YR) %>%
  summarise(area_sum = sum(overlap_area, na.rm = TRUE), .groups = "drop") %>%
  group_by(id) %>%
  slice_max(order_by = area_sum, n = 1, with_ties = FALSE)


# ===========================
# STEP 7: Final Join + Output
# ===========================
iucn_weighted_df_clean <- iucn_weighted %>%
  st_set_geometry(NULL) %>%
  left_join(
    dominant_year %>% st_set_geometry(NULL) %>% select(id, dominant_status_yr = STATUS_YR),
    by = "id"
  ) %>%
  left_join(management_weighted, by = "id")


grid_sf_coverage <- grid_cov_area %>%
  left_join(iucn_weighted_df_clean, by = "id") %>%
  mutate(
    cell_area = as.numeric(cell_area),
    cov_frac = if_else(is.na(cov_frac), 0, cov_frac)
  ) %>%
  st_as_sf()

# Grid with cell geometries
st_write(grid_sf_coverage, "./data/Protected-areas/final_output.gpkg", layer = "grid_cells", delete_layer = TRUE)

# Protected area geometries per cell
st_write(
  intersected_union %>% select(id, geometry_pa = geometry),
  "./data/Protected-areas/final_output.gpkg",
  layer = "protected_area_geoms",
  append = TRUE
)








intersected_filtered_clean %>%
  filter(IUCN_CAT_numeric == 8) %>%
  select(NAME, DESIG, IUCN_CAT_numeric, overlap_area) %>%
  distinct()

# Check example: 
intersected_filtered %>%
  filter(id == 10154) %>%
  select(NAME, WDPAID, DESIG,IUCN_CAT, IUCN_CAT_numeric, overlap_area) %>%
  arrange(desc(overlap_area)) %>%
  View()

intersected %>%
  st_set_geometry(NULL) %>%
  count(DESIG, sort = TRUE)

summary(grid_sf_coverage$cov_frac)
st_crs(grid_sf_coverage) # Check the CRS of the resulting grid
# NOTE: The CRS is still 25832, because of the spatial calcuations

# 1. Explizit benennen und Geometrie korrekt sichern
grid_sf_coverage_clean <- grid_sf_coverage

# 2. Umbenennen, solange Geometrie aktiv ist
names(grid_sf_coverage_clean)[which(names(grid_sf_coverage_clean) == "geometry.x")] <- "geometry_grid"

# 3. Entferne unerwünschte Geometrie (geometry.y)
grid_sf_coverage_clean$geometry.y <- NULL

# 4. Setze die aktive Geometriespalte korrekt
st_geometry(grid_sf_coverage_clean) <- "geometry_grid"


# 2. Geometrie neu setzen (aus geometry_grid)
grid_sf_coverage_clean <- st_as_sf(grid_sf_coverage_clean, sf_column_name = "geometry_grid", crs = st_crs(grid_sf_coverage))


# Save the result
st_write(grid_sf_coverage_clean, "./data/Protected-areas/grid_sf_coverage_clean.gpkg", delete_dsn = TRUE)

grid_df_csv <- grid_sf_coverage_clean %>%
  st_set_geometry(NULL)

write.csv(grid_df_csv, "./data/Protected-areas/grid_sf_coverage_clean.csv", row.names = FALSE)


# PART 6: Sensitivity Analysis: Classify grid cells based on protected area coverage ----

# Protection90:
# - Cells with a coverage fraction (cov_frac) of 0.9 or higher are considered "protected".
# - Cells with a coverage fraction between 0.1 (inclusive) and 0.9 (exclusive) are considered "not fully protected".
# - Cells with a coverage fraction below 0.1 are considered "not protected".
# Same logic applies for 80%, 70%, 60%, and 50% thresholds.

# Make a dataset with all sensitivities
grid_sf_coverage_all <- grid_sf_coverage_clean %>%
  mutate(
    protection90 = case_when(
      cov_frac >= 0.9 ~ "protected",
      cov_frac >= 0.1 & cov_frac < 0.9 ~ "part protected",
      cov_frac < 0.1 ~ "not protected"
    ),
    protection80 = case_when(
      cov_frac >= 0.8 ~ "protected",
      cov_frac >= 0.1 & cov_frac < 0.8 ~ "part protected",
      cov_frac < 0.1 ~ "not protected"
    ),
    protection70 = case_when(
      cov_frac >= 0.7 ~ "protected",
      cov_frac >= 0.1 & cov_frac < 0.7 ~ "part protected",
      cov_frac < 0.1 ~ "not protected"
    ),
    protection60 = case_when(
      cov_frac >= 0.6 ~ "protected",
      cov_frac >= 0.1 & cov_frac < 0.6 ~ "part protected",
      cov_frac < 0.1 ~ "not protected"
    ),
    protection50 = case_when(
      cov_frac >= 0.5 ~ "protected",
      cov_frac >= 0.1 & cov_frac < 0.5 ~ "part protected",
      cov_frac < 0.1 ~ "not protected"
    )
  )

grid_sf_coverage %>% 
  filter(id == 1237) %>% 
  View()


# Save the protection status result
st_write(grid_sf_coverage_all, "./data/Protected-areas/grid_sf_protectionstatus.gpkg")
grid_sf_coverage_all<- st_read("./data/Protected-areas/grid_sf_protectionstatus.gpkg")
# Count how many grid cells are "protected", "part protected", or "not protected" per threshold
list(
  p90 = grid_sf_coverage_all %>% count(protection90), # np=6943; pp=4603; p=478
  p80 = grid_sf_coverage_all %>% count(protection80), # np= 6943; pp=4284; p=797
  p70 = grid_sf_coverage_all %>% count(protection70), # np= 6943; pp=3954; p=1127
  p60 = grid_sf_coverage_all %>% count(protection60), # np= 6943; pp=3578; p=1503
  p50 = grid_sf_coverage_all %>% count(protection50)  # np= 6943; pp=3134; p=1947
)


# PART 7: Create bar plot for grid cell protection status (90% threshold shown as example) ----

protection_hist <- ggplot(grid_sf_coverage_all, aes(x = protection90, fill = protection90)) +
  geom_bar(color = "black", width = 0.7) +
  labs(
    #title = "Distribution of Protection Status in Grid Cells across Germany",
    #subtitle = "Total grid cells: 12,024\nThreshold: ≥90% coverage = 'protected'",
    x = "Protection Status",
    y = "Number of Grid Cells"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 50),
    plot.title = element_text(face = "bold", size = 56, hjust = 0.5),
    plot.subtitle = element_text(size = 50, hjust = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c(
    "protected" = "#006837",
    "part protected" = "#74c476",
    "not protected" = "#d9f0d3"
  ))

print(protection_hist)

ggsave("./figures/protection_status_histogram.png", 
       plot = protection_hist, 
       width = 10, height = 8, dpi = 200)

# PART 8: Create pie chart for protected area size categories ----

# Step 1: Categorize protected areas based on a pre-existing GIS_AREA field
# Create a new column 'area_category' based on the computed area
PA_areas <- st_read("./data/Protected-areas/protected_areas_020425.shp")
PA_areas <- PA_areas %>%
  mutate(area_category = if_else(GIS_AREA >= 25, ">= 25 km²", "< 25 km²"))

# Step 2: Remove geometry and compute numeric areas
PA_areas_numeric <- st_set_geometry(PA_areas, NULL) %>%
  mutate(pa_area_num = as.numeric(GIS_AREA))

# Step 3: Summarize the total area per category
area_summary <- PA_areas_numeric %>%
  group_by(area_category) %>%
  summarise(total_area = sum(pa_area_num, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percentage = total_area / sum(total_area) * 100)

total_pa_area_km2 <- sum(PA_areas$GIS_AREA, na.rm = TRUE)
total_terrestrial_area <- sum(PA_areas$GIS_T_AREA, na.rm = TRUE) # 223.555,54 km²



# Pie chart for size distribution
protected_area_pie_chart <- ggplot(area_summary, aes(x = "", y = total_area, fill = area_category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 20) +
  labs(
    #title = "Distribution of Terrestrial Protected Area Sizes in Germany",
    #subtitle = "Total Protected Area (mainly terrestrial): 240,924 km²",
    fill = "Area Category"
  ) +
  theme_void() +
  scale_fill_manual(values = c(
    ">= 25 km²" = "#006837",
    "< 25 km²" = "#a1d99b"
  )) +
  theme(
    plot.title = element_text(size = 56, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 50, hjust = 0.5),
    legend.title = element_text(size = 50),
    legend.text = element_text(size = 50)
  )

ggsave("./figures/protected_area_pie_chart.png", 
       plot = protected_area_pie_chart, 
       width = 10, height = 8, dpi = 200)

