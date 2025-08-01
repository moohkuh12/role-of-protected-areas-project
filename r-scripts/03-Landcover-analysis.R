# 03-Landcover_analysis.R
# -----------------------------------------------------------------------------
# This Script excludes the predominantly urban gird cells using the difference between 
# urban area-polygons and the 25 km2 grid cells over germany.
# It marks those grid_cells where over 50% of the area is 
# urban and also includes information about the forest cover per grid cell. The script uses the HILDA+ 1960 (baseline timeperiod) land cover data (urban code = 11)
# and subtracts urban areas and forest areas (using a union/difference approach) from the grid raster of germany.
# 
# Author: Marie Englert
# Date: 18.07.2025
# =============================================================


# Load libraries and input data ----

# Load preamble (setup and libraries) 
source("./r-scripts/00-preamble.R")

# Read in the grid raster shapefile 
grid_sf_coverage <- st_read("./data/Protected-areas/grid_sf_protectionstatus.gpkg")

# Load and reproject HILDA+ raster (baseline 1960) ----

# Load the HILDA+ raster for the baseline year 1960 (in WGS84: EPSG 4326)
hilda_raster <- rast("./data/hildap_vGLOB-1.0_geotiff/hildap_GLOB-v1.0_lulc-states/hilda_plus_1960_states_GLOB-v1-0_wgs84-nn.tif")

# Check CRS of HILDA+ raster and the grid
st_crs(hilda_raster)
st_crs(grid_sf_coverage)

# Check raster resolution (in degrees)
res(hilda_raster)

# Reproject HILDA+ raster to match grid CRS (ETRS89 / UTM zone 32N)
# Use nearest neighbor interpolation (important for land cover categories)
# Target resolution is set to 1 km² to approximate original 0.01° cell size
hilda_utm <- terra::project(
  hilda_raster,
  "EPSG:25832",
  method = "near",          
  res = c(1000, 1000)       
)

# Crop the reprojected raster to the extent of the grid
grid_extent <- ext(grid_sf_coverage)
hilda_crop <- crop(hilda_utm, grid_extent)
rm(hilda_utm)  # Clean up memory
rm(hilda_raster)
# Extract urban and forest raster layers ----

# Set all cells not equal to 11 (urban) to NA so only urban areas remain
urban_raster <- hilda_crop
urban_raster[urban_raster != 11] <- NA

# Same for forest: code 44 in HILDA+ 1960 dataset
forest_raster <- hilda_crop
forest_raster[forest_raster != 44] <- NA


# Convert raster to polygons ----

# Convert urban raster to polygons and merge adjacent urban cells
urban_polygons <- as.polygons(urban_raster, dissolve = TRUE)
urban_sf <- st_as_sf(urban_polygons)

# Convert forest raster to polygons and merge adjacent forest cells
forest_polygons <- as.polygons(forest_raster, dissolve = TRUE)
forest_sf <- st_as_sf(forest_polygons)


# Harmonize coordinate reference systems ----

# Reproject urban and forest polygons to match the CRS of the grid
st_crs(urban_sf) <- st_crs(grid_sf_coverage)
urban_sf <- st_transform(urban_sf, crs = st_crs(grid_sf_coverage))
plot(urban_sf)

st_crs(forest_sf) <- st_crs(grid_sf_coverage)
forest_sf <- st_transform(forest_sf, crs = st_crs(grid_sf_coverage))
plot(forest_sf)


# Intersect with grid and calculate area coverage ----

# Calculate total area for each grid cell
grid_sf_coverage <- grid_sf_coverage %>%
  mutate(cell_area = st_area(.))

# Intersect grid with urban and forest polygons
grid_urban_intersection <- st_intersection(grid_sf_coverage, urban_sf)
grid_forest_intersection <- st_intersection(grid_sf_coverage, forest_sf)

# Calculate urban area within each grid cell (in m²)
urban_area_per_cell <- grid_urban_intersection %>%
  mutate(urban_area = st_area(.)) %>%
  st_set_geometry(NULL) %>%
  group_by(id) %>%
  summarise(urban_area = sum(urban_area, na.rm = TRUE))

# Calculate forest area within each grid cell (in m²)
forest_area_per_cell <- grid_forest_intersection %>%
  mutate(forest_area = st_area(.)) %>%
  st_set_geometry(NULL) %>%
  group_by(id) %>%
  summarise(forest_area = sum(forest_area, na.rm = TRUE))


# Join urban and forest metrics back to full grid ----

# Join urban area and classify each cell as "urban" or "non-urban"
grid_sf_classified <- grid_sf_coverage %>%
  left_join(urban_area_per_cell, by = "id") %>%
  mutate(
    urban_area = if_else(is.na(urban_area), 0, as.numeric(urban_area)),  # set to 0 if missing
    cell_area = as.numeric(cell_area),
    urban_prop = urban_area / cell_area,                                # proportion of urban area
    urban_class = if_else(urban_prop > 0.5, "urban", "non-urban")       # classify based on >50% threshold
  )

# Join forest area as additional variable
grid_sf_classified <- grid_sf_classified %>%
  left_join(forest_area_per_cell, by = "id") %>%
  mutate(
    forest_area = if_else(is.na(forest_area), 0, as.numeric(forest_area)),
    cell_area = as.numeric(cell_area),
    forest_prop = forest_area / cell_area
  )


# Quick checks and validation ----

# How many grid cells are completely non-urban?
grid_sf_classified %>% filter(urban_area == 0) %>% nrow()

# How many are classified as "urban" (>50% urban area)?
grid_sf_classified %>% filter(urban_class == "urban") %>% nrow()


# Export results ----

# Save the resulting dataset as a shapefile
st_write(grid_sf_classified, "./data/landcover_analysis/grid_sf_landcover.gpkg")
grid_sf_classified <- st_read("./data/landcover_analysis/grid_sf_landcover.gpkg")
# Optional plot for preview
# plot(st_geometry(intersected_union)) + theme_minimal() + theme(legend.position = "none")

# Prepare EuForPlants data ----

# Read in EuForPlants data
euforplants <- read_excel("./data/landcover_analysis/EuForPlantsGermany.xlsx")
euforplants_selected <- euforplants %>%
  select(`Scientific name`, 
         `Germany, NW lowlands`, 
         `Germany, NE lowlands`, 
         `Germany, uplands and mountains`, 
         `Germany, Alps`)
# Rename columns for clarity
colnames(euforplants_selected) <- c("species", 
                                      "NW_lowlands", 
                                      "NE_lowlands", 
                                      "Uplands_and_mountains", 
                                      "Alps")
head(euforplants_selected)
duplicates_by_species <- euforplants %>%
  group_by(`Scientific name`) %>%
  filter(n() > 1)

duplicates_by_species %>%
  arrange(`Scientific name`) %>%
  distinct(`Scientific name`)
# seems that there are already 3 duplicated species in the EuForPlants dataset (Clematis viticella L., Epipactis bugacensis Robatsch, Silene nemoralis Waldst. & Kit.)

# Remove author names from the species names using the flora package.
# Note: flora::remove.authors() may not work perfectly for every species.
species_without <- vector("character", nrow(euforplants_selected))
for (i in 1:nrow(euforplants_selected)) {
  species_without[i] <- flora::remove.authors(euforplants_selected$species[i])
}
# Add the cleaned species names as a new column.
euforplants_selected$species_without <- species_without
euforplants_selected <- euforplants_selected %>%
  mutate(species_without = str_replace_all(species_without, 
                                             c("agg\\." = "",        # Remove "agg."
                                               "s\\. l\\." = "",      # Remove "s. l."
                                               "subg\\." = "",         # Remove "subg."
                                               "subsp\\." = " subsp. "         # finds the matches better with the subsp.
                                             ))) %>%
  mutate(species_without = str_trim(species_without)) %>%  # Remove leading/trailing whitespace.
  mutate(species_without = str_squish(species_without))      # Remove extra spaces within the name.
# Check the cleaned species names
head(euforplants_selected$species_without)
# Check the unique species names
unique_species <- unique(euforplants_selected$species_without)

# Check for duplicates in the cleaned species names because the number of unique species is lower that the species_without vektor
dupes <- euforplants_selected %>%
  group_by(species_without) %>%
  filter(n() > 1) %>%
  arrange(species_without)

dupes %>% select(species, species_without)

# Match EuForPlants species names with the WCVP database using fuzzy matching.
euforplants_wcvp <- wcvp_match_names(
  euforplants_selected, 
  name_col = "species_without",
  fuzzy = TRUE
)


# check unmatched species
unmatched <- euforplants_wcvp %>%
  filter(is.na(match_type))
unmatched$species_without
# "Hieracium laevigatum borealis" wasnt matched, but is no red list species anyway

euforplants_wcvp <- euforplants_wcvp %>% 
  arrange(
    desc(wcvp_status %in% c("Accepted", "Synonym")),  # Prioritize "Accepted" or "Synonym".
    desc(wcvp_status == "Accepted")                   # Further prioritize if status is "Accepted".
  ) %>%
  distinct(species_without, .keep_all = TRUE)


euforplants_selected <- euforplants_wcvp %>%
  select("species","wcvp_name", 
         "NW_lowlands", 
         "NE_lowlands", 
         "Uplands_and_mountains", 
         "Alps")

# Now we assign different numerical values depending on the dominant classification across Germany. 
# Main_group will be a variable that ignores purely montane species, while main_group2 will keep the montane differentiation.
# Prepare the EuForPlants dataset for classification ----


euforplants <- euforplants_selected %>%
  # A: Kodierung aufbereiten
  mutate(across(
    c(NW_lowlands, NE_lowlands, Uplands_and_mountains, Alps),
    ~ case_when(
      . == "/" ~ NA_character_,
      . == "O" ~ "3",
      TRUE ~ .
    )
  )) %>%
  mutate(across(
    c(NW_lowlands, NE_lowlands, Uplands_and_mountains, Alps),
    ~ round(as.numeric(.), 1)
  )) %>%
  
  # B, C, D: Zeilenweise Klassifikation
  rowwise() %>%
  
  # B: Hauptkategorie bestimmen
  mutate(main_category = {
    vals <- na.omit(c_across(c(NW_lowlands, NE_lowlands, Uplands_and_mountains, Alps)))
    if (length(vals) == 0) {
      NA_character_
    } else {
      freq <- sort(table(vals), decreasing = TRUE)
      top_vals <- as.numeric(names(freq)[freq == max(freq)])
      if (length(top_vals) == 1) {
        format(top_vals[1], nsmall = 1)
      } else {
        paste(format(sort(top_vals), nsmall = 1), collapse = " / ")
      }
    }
  }) %>%
  
  # C: Hauptklassifikation ohne montane Differenzierung
  mutate(main_group = {
    raw_parts <- if (!is.na(main_category)) strsplit(main_category, " / ")[[1]] else NULL
    parts <- suppressWarnings(as.numeric(raw_parts))
    
    if (length(parts) > 0 && all(!is.na(parts))) {
      if (all(parts %in% c(1.1, 1.2))) {
        "forest specialist"
      } else if (all(parts %in% c(2.1, 2.2))) {
        "generalist"
      } else if (all(parts == 3)) {
        "open specialist"
      } else if (any(parts %in% c(1.1, 1.2)) & any(parts %in% c(2.1, 2.2, 3))) {
        "generalist"
      } else if (any(parts %in% c(2.1, 2.2)) & any(parts == 3) & all(parts %in% c(2.1, 2.2, 3))) {
        "generalist"
      } else {
        "mixed"
      }
    } else {
      NA_character_
    }
  }) %>%
  
  # D: Kontrollklassifikation mit montaner Ausdifferenzierung
  mutate(main_group2 = {
    vals <- c_across(c(NW_lowlands, NE_lowlands, Uplands_and_mountains, Alps))
    region_names <- c("NW_lowlands", "NE_lowlands", "Uplands_and_mountains", "Alps")
    present_regions <- region_names[!is.na(vals)]
    lowland_regions <- c("NW_lowlands", "NE_lowlands")
    montane_regions <- c("Uplands_and_mountains", "Alps")
    
    raw_parts <- if (!is.na(main_category)) strsplit(main_category, " / ")[[1]] else NULL
    parts <- suppressWarnings(as.numeric(raw_parts))
    
    if (length(parts) > 0 && all(!is.na(parts))) {
      func_group <- if (all(parts %in% c(1.1, 1.2))) {
        "forest specialist"
      } else if (all(parts %in% c(2.1, 2.2))) {
        "generalist"
      } else if (all(parts == 3)) {
        "open specialist"
      } else if (any(parts %in% c(1.1, 1.2)) & any(parts %in% c(2.1, 2.2, 3))) {
        "generalist"
      } else if (any(parts %in% c(2.1, 2.2)) & any(parts == 3) & all(parts %in% c(2.1, 2.2, 3))) {
        "generalist"
      } else {
        "mixed"
      }
      
      if (all(present_regions %in% montane_regions) && !any(present_regions %in% lowland_regions)) {
        if (func_group == "forest specialist") {
          "montane forest"
        } else if (func_group == "generalist") {
          "montane generalist"
        } else if (func_group == "open specialist") {
          "montane generalist"
        } else if (func_group == "mixed") {
          "montane mixed"
        } else {
          NA_character_
        }
      } else {
        func_group
      }
    } else {
      NA_character_
    }
  }) %>%
  
  # Alles zurück in den normalen (ungeordneten) Datenframe-Modus
  ungroup()

euforplants %>%
  count(main_group, sort = TRUE)



# 5. Speichern (optional)
write.csv(euforplants, "./data/landcover_analysis/euforplants_summary.csv", row.names = FALSE)
euforplants <- read.csv("./data/landcover_analysis/euforplants_summary.csv", stringsAsFactors = FALSE)

# ---- Prepare EIVE data ----

# Load raw EIVE dataset (vegetation classification)
eive <- readxl::read_excel("./data/EIVE/vegetation_classification_and_survey-004-007-g008.xlsx")

# Select relevant columns by index (e.g., Taxon info + Ellenberg values)
eive <- eive[, c(1, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18)]

# Check the taxonomic ranks available
unique(eive$TaxonRank)

# Keep only records identified to the species level
eive <- eive %>%
  dplyr::filter(TaxonRank == "Species")

# Clean the taxon names:
# - Remove labels like "agg.", "s. l.", "subg."
# - Remove the hybrid marker "×"
# - Remove extra whitespace
eive_cleaned <- eive %>%
  dplyr::mutate(TaxonName_cleaned = stringr::str_replace_all(TaxonConcept, c(
    "agg\\." = "",
    "s\\. l\\." = "",
    "subg\\." = "",
    "\\b×\\b" = " "
  ))) %>%
  dplyr::mutate(
    TaxonName_cleaned = stringr::str_trim(TaxonName_cleaned),
    TaxonName_cleaned = stringr::str_squish(TaxonName_cleaned)
  )

# ---- Match cleaned names to WCVP ----

eive_wcvp <- wcvp_match_names(
  eive_cleaned,
  name_col = "TaxonName_cleaned",
  fuzzy = TRUE
)

# Identify unmatched names
unmatched <- eive_wcvp %>%
  dplyr::filter(is.na(match_type))

# Save unmatched species for manual inspection
write.csv(unmatched, "./data/EIVE/unmatched_eive.csv", row.names = FALSE)

# Save the full WCVP match result
write.csv(eive_wcvp, "./data/EIVE/eive_wcvp.csv", row.names = FALSE)

# Prioritize accepted or synonym names and keep one entry per taxon
eive_wcvp_acc <- eive_wcvp %>%
  dplyr::arrange(
    desc(wcvp_status %in% c("Accepted", "Synonym")),
    desc(wcvp_status == "Accepted")
  ) %>%
  dplyr::distinct(TaxonName_cleaned, .keep_all = TRUE)

# Check again for unmatched names in cleaned dataset
unmatched <- eive_wcvp_acc %>%
  dplyr::filter(is.na(wcvp_name) | wcvp_name == "")

# ---- Handle fuzzy matches ----

# Flag fuzzy matches with good similarity (≥ 0.9) or edit distance = 1
fuzzy_matches_eive <- eive_wcvp_acc %>%
  dplyr::filter(stringr::str_detect(match_type, "Fuzzy")) %>%
  dplyr::mutate(keep = dplyr::case_when(
    match_similarity < 0.9 ~ NA_real_,
    match_edit_distance == 1 ~ 1
  ))

# Summary of fuzzy match decisions
table(fuzzy_matches_eive$keep, useNA = "always")

# Save fuzzy matches for manual review (optional)
# readr::write_csv(fuzzy_matches_eive, "./data/EIVE/fuzzy-tocheck-eive.csv")

# ---- Load manually reviewed fuzzy matches ----

fuzzy_checked <- read.csv("./data/EIVE/fuzzy-checked-eive.csv",
                          stringsAsFactors = FALSE, sep = ";", header = TRUE) %>%
  dplyr::filter(keep == "keep") %>%
  dplyr::select(-keep) %>%
  dplyr::mutate(resolved_match_type = "Manual_Fuzzy_OK")

# ---- Finalize WCVP-matched dataset ----

eive_wcvp_final <- eive_wcvp_acc %>%
  # Remove all fuzzy matches from the original
  dplyr::filter(match_type != "Fuzzy") %>%
  # Also remove any species already included in fuzzy_checked (just to be sure)
  dplyr::anti_join(fuzzy_checked, by = "wcvp_name") %>%
  # Add manually accepted fuzzy matches
  dplyr::bind_rows(fuzzy_checked) %>%
  # Drop records with missing wcvp_name
  dplyr::filter(!is.na(wcvp_name) & wcvp_name != "") %>%
  # Keep only one row per wcvp_name, giving priority to manually reviewed matches
  dplyr::arrange(wcvp_name, desc(resolved_match_type == "Manual_Fuzzy_OK")) %>%
  dplyr::distinct(wcvp_name, .keep_all = TRUE)

# ---- Save final output ----

# Create final output with only selected trait columns
eive_final <- eive_wcvp_final %>%
  dplyr::select(wcvp_name, wcvp_authors, 1:12)

# Save full match info
write.csv(eive_wcvp_final, "./data/EIVE/eive_final_taxonharminfo.csv", row.names = FALSE)

# Save reduced summary file for merging with sMon data
write.csv(eive_final, "./data/EIVE/eive_final.csv", row.names = FALSE)

