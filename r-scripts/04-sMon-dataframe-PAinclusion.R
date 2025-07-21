# 04-sMon_dataframe_PA.R
# -----------------------------------------------------------------------------
# Purpose:
# This script creates the final merged sMon dataframe that contains:
# - species occurrence probabilities (Eichenberg et al. 2021, harmonized red list subset)
# - spatial grid references (MTB_Q cells)
# - protection status of each grid cell (including coverage fractions and protection levels)
# - land cover classification (urban/non-urban, forest proportion)
# - information on the forest affinity of the species (gathered from EuForPlants)
#
# The protected area data used was prepared in script "02-Protected-area-analysis.R"
# and enriched with land cover data in "03-Landcover_analysis.R".
#
#
# Author: Marie Englert
# Date: 03.04.2025
# =============================================================

# 0) Load packages ----
source("./r_scripts/00-preamble.R")


# 1) Load protected areas and grid data ----

# 1.1) Read in the shapefile/geopackage of the protected area 
# These geopackage was prepared in "02-Protected-area-analysis.R", 
# further processed and fused with the land cover data from the HILDA+ database
# (urban/non-urban classification) and forest_prop was added in "03-Landcover_analysis"
protected_areas <- st_read("./data/landcover_analysis/grid_sf_landcover.gpkg") # without coastal/marine and marine PAs


# 2) Load sMon red-listed species data ----

# Load the harmonized red list species (sMon) dataset 
red_listed_smon <- read.csv("./data/sMon/red_listed_sMon_occprobs020425.csv")
timestep_2 <- read.csv("./data/sMon/timestep_2.csv")

 
duplikate2 <- red_listed_smon %>%
  count(MTB_Q, TaxonName, Period) %>%
  filter(n > 1)
# 5) Merge protected area information with sMon dataset ----

# Load spatial point locations with MTB_Q and unique ID per lat/long pair
lat_long_ids_sf <- st_read("./data/landcover_analysis/lat_long_ids_sf.shp")

# Merge occurrence data with spatial grid cell IDs
merged_sMon_ids <- red_listed_smon %>%
  left_join(lat_long_ids_sf, by = "MTB_Q")

# same with timestep 2
merged_sMon_ids_t2 <- timestep_2 %>%
  left_join(lat_long_ids_sf, by = "MTB_Q")


duplikate <- merged_sMon_ids %>%
  count(id, TaxonName, Period) %>%
  filter(n > 1)

# Merge spatial attributes (protection status, urban/forest coverage, etc.)
merged_sMon_ids <- merged_sMon_ids %>%
  left_join(protected_areas, by = c("id", "MTB_Q"))
merged_sMon_ids_t2 <- merged_sMon_ids_t2 %>%
  left_join(protected_areas, by = c("id", "MTB_Q"))

# Count number of unique species
unique_species_count <- n_distinct(merged_sMon_ids$TaxonName)
cat("Number of unique species in TaxonName:", unique_species_count, "\n")


# 5.1) Save final dataset ----

# Save merged dataset as CSV
write.csv(merged_sMon_ids, "./data/sMon/sMon_long_090425.csv", row.names = FALSE)
write.csv(merged_sMon_ids_t2, "./data/sMon/sMon_long_t2_220525.csv", row.names = FALSE)
merged_sMon_ids <- read.csv("./data/sMon/sMon_long_090425.csv")
# Save as spatial geopackage (preserving geometry)
st_write(merged_sMon_ids, "./data/sMon/sMon_long_090425.gpkg")
st_write(merged_sMon_ids_t2, "./data/sMon/sMon_long_t2_220525.gpkg")
problems(merged_sMon_ids)

merged_sMon_ids <- st_read("./data/sMon/sMon_long_090425.gpkg") #rather use this one
rm(merged_sMon_ids)

duplikate <- merged_sMon_ids %>%
  count(id, TaxonName, Period) %>%
  filter(n > 1) #Rhinantus serotinus is doubled


# Convert into wide Format for analysis
sMon_wide <- merged_sMon_ids %>%
  pivot_wider(
    names_from = Timestep,  
    values_from = c(OP, OP_sd, Period),  
    names_prefix = "T",  
    id_cols = c(id, TaxonName,wcvp_name, MTB_Q, Longitude, Latitude,  geom, IUCN_CAT_final, 
                #WDPAID, DESIG_ENG, NAME,IUCN_CAT_numeric,GIS_M_AREA, GIS_T_AREA,  GIS_AREA, REP_AREA, overlap_area, 
                cov_frac, protection90, protection80, protection70, protection60, protection50,urban_area, urban_prop, urban_class, forest_prop)  
  )

sMon_widet2 <- merged_sMon_ids_t2 %>%
  pivot_wider(
    names_from = Timestep,  
    values_from = c(OP, OP_sd, Period),  
    names_prefix = "T",  
    id_cols = c(id, TaxonName,wcvp_name, MTB_Q, Longitude, Latitude,  geometry, geom, IUCN_CAT_final, 
                #WDPAID, DESIG_ENG, NAME,IUCN_CAT_numeric,GIS_M_AREA, GIS_T_AREA,  GIS_AREA, REP_AREA, overlap_area, 
                cov_frac, protection90, protection80, protection70, protection60, protection50,urban_area, urban_prop, urban_class, forest_prop)  
  )
# EuForPlants data inclusion -----------

euforplants_summary <- read.csv("./data/landcover_analysis/euforplants_summary_new.csv")

# Merge with euforplants data
# Select only necessary columns for the join
main_group_info <- euforplants_summary %>%
  select(wcvp_name, main_group)

# Inner join: keep only rows that exist in both datasets
smon_wide_joined <- sMon_wide %>%
  left_join(main_group_info, by = "wcvp_name")

# Reorder columns: put main_group at position 3
smon_wide_joined <- smon_wide_joined %>%
  select(1:2, main_group, everything())


#for t2

# Inner join: keep only rows that exist in both datasets
smon_wide_joinedt2 <- sMon_widet2 %>%
  left_join(main_group_info, by = "wcvp_name")

# Reorder columns: put main_group at position 3
smon_wide_joinedt2 <- smon_wide_joinedt2 %>%
  select(1:2, main_group, everything())

# EIVE data inclusion -----------

# Merge with eive data
# Select only necessary columns for the join
eive_summary <- read.csv("./data/EIVE/eive_final.csv")
eive_data <- eive_summary %>%
  select(wcvp_name, 5:14)

# Check for duplicate wcvp_name entries in eive_data
dups <- eive_data %>%
  group_by(wcvp_name) %>%
  filter(n() > 1) %>%
  arrange(wcvp_name) %>%
  ungroup()

# Join sMon data with EIVE traits using wcvp_name
# This is a left join: it keeps all species from sMon_wide and adds EIVE data if available
smon_wide_joined <- smon_wide_joined %>%
  left_join(eive_data, by = "wcvp_name")
# for t2
smon_wide_joinedt2 <- smon_wide_joinedt2 %>%
  left_join(eive_data, by = "wcvp_name")

# Summary: how many species in sMon_wide have EIVE trait data?
sMon_wide %>%
  distinct(wcvp_name, .keep_all = TRUE) %>%
  summarise(
    total = n(),  # total number of unique species
    with_eive = sum(!is.na(`EIVEres.M`)),  # species with EIVE data
    without_eive = sum(is.na(`EIVEres.M`)),  # species without EIVE data
    pct_with_eive = round(100 * mean(!is.na(`EIVEres.M`)), 1)  # percentage with data
  )
# Result: 93.2% of species have EIVE trait data

# Create a data frame of species (wcvp_name) that did not get matched EIVE data
no_eive_names <- smon_wide_joined %>%
  filter(is.na(`EIVEres-M`)) %>%
  distinct(wcvp_name) %>%
  arrange(wcvp_name)

# View the species without EIVE data
View(no_eive_names)

# TRY data inclusion -----------
smon_wide_joined <- st_read("./data/sMon/sMon_wide_050525.gpkg")
try_data <- read.csv("./data/TaxonHarm/rl_wcvp_try.csv")
# Select only relevant columns
try_data <- try_data[, c(3, 5:21)]

# Join sMon data with TRY data using wcvp_name
# This is a left join: it keeps all species from sMon_wide and adds try data if available
smon_wide_joined <- smon_wide_joined %>%
  left_join(try_data, by = "wcvp_name")

# for t2
smon_wide_joinedt2 <- smon_wide_joinedt2 %>%
  left_join(try_data, by = "wcvp_name")

# Summary: how many species in sMon_wide have try data?
smon_wide_joined %>%
  distinct(wcvp_name, .keep_all = TRUE) %>%
  summarise(
    total = n(),  # total number of unique species
    with_try = sum(!is.na(`Family`)),  # species with EIVE data
    without_eive = sum(is.na(`Family`)),  # species without EIVE data
    pct_with_eive = round(100 * mean(!is.na(`Family`)), 1)  # percentage with data
  )

# Save wide format as CSV
write_csv(smon_wide_joined, "./data/sMon/sMon_wide_100625.csv")
sMon_wide_joined <- read_csv("./data/sMon/sMon_wide_100625.csv")
# Save as spatial geopackage (preserving geometry)
st_write(smon_wide_joined, "./data/sMon/sMon_wide_100625.gpkg")



t2_data <- smon_wide_joinedt2 %>%
  select(id, TaxonName, Longitude, Latitude, OP_T2, OP_sd_T2, Period_T2)

smon_all <- smon_wide_joined %>%
  left_join(t2_data, by = c("id", "TaxonName", "Longitude", "Latitude"))


nrow(smon_wide_joined)
nrow(smon_all)

write_csv(smon_all, "./data/sMon/sMon_wide_t1-3_220525.csv")
sMon_wide_joined_all <- read_csv("./data/sMon/sMon_wide_t1-3_220525.csv")
# Save as spatial geopackage (preserving geometry)
st_write(smon_all, "./data/sMon/sMon_wide_t1-3_220525.gpkg")
