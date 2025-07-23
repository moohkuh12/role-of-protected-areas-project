# External calculation of management index for protected areas
# This should replace the mean_management column, which is based on binary management yes/no deciscions
# I thought about making management being weighted more fine scaled
# For that i needed to check which management is present in the different protected area designs
# For the Design categories i assigned them a management score which should represent the "intensity" of management 


source("./r-scripts/00-preamble.R")

intersected <- st_read("./data/Protected-areas/Intermediate/intersected_protected_areas.shp")

# Assign numeric protection categories and flag zones with active management------------------------

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
    # IUCN category classification (differentiated for biosphere core zone)
    IUCN_CAT_numeric = case_when(
      IUCN_CAT %in% c("Ia", "Ib") ~ 1,
      IUCN_CAT == "II" ~ 2,
      IUCN_CAT == "III" ~ 3,
      IUCN_CAT == "IV" ~ 4,
      IUCN_CAT == "V" ~ 5,
      IUCN_CAT == "Not Reported" & DESIG == "Special Areas of Conservation (Habitats Directive)" ~ 6,
      IUCN_CAT == "Not Reported" & DESIG == "Special Protection Area (Birds Directive)" ~ 7,
      DESIG == "Biosphärenreservat - Kernzone" ~ 8,
      DESIG %in% c("Biosphärenreservat - Pflegezone", "Biosphärenreservat - Entwicklungszone") ~ 9,
      IUCN_CAT %in% c("Not Applicable", "Not Assigned") ~ 10,
      TRUE ~ 11
    ),
    
    # Flag for management
    has_management = case_when(
      IUCN_CAT_numeric %in% c(4, 5, 6, 7) ~ TRUE,
      DESIG %in% c("Biosphärenreservat - Pflegezone", "Biosphärenreservat - Entwicklungszone") ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  
  # Exclude large uninformative parent Biosphärenreservat hulls
  filter(!(DESIG %in% c("Biosphärenreservat", "UNESCO-MAB Biosphere Reserve"))) %>%
  mutate(overlap_area = as.numeric(st_area(.)))

intersected_mgmt <- intersected_filtered %>%
  mutate(
    # Assign a continuous management score (0–1) to each protected area fragment.
    # This score reflects the likely intensity and relevance of conservation management, particularly for plants.
    management_score = case_when(
      
      # IUCN Category IV – Habitat/Species Management Areas
      # These areas are specifically managed with targeted actions (e.g., mowing, rewetting) to maintain or restore habitats or species.
      # Among the most directly and actively managed protected areas — highly relevant for plant conservation.
      IUCN_CAT_numeric == 4 ~ 1.0,
      
      # IUCN Category V – Protected Landscapes/Seascapes
      # These are often traditionally used cultural landscapes maintained through extensive agriculture, grazing, or forestry.
      # While not always species-specific, they are usually subject to continuous, low-intensity management that supports open habitat plant communities.
      IUCN_CAT_numeric == 5 ~ 0.8,
      
      # Habitats Directive (Natura 2000) – Special Areas of Conservation (SAC)
      # Legally designated under EU law to protect specific habitats and species — including many plants.
      # Management plans are mandatory, but implementation varies between regions. Still, it is the most plant-relevant EU protection type.
      IUCN_CAT_numeric == 6 ~ 0.8,
      
      # Birds Directive (Natura 2000) – Special Protection Areas (SPA)
      # Established to protect birds and their habitats. Management often focuses on wetlands, grasslands, or open water bodies.
      # Benefits to plants are mostly indirect, so management relevance for plant conservation is lower than for SACs.
      IUCN_CAT_numeric == 7 ~ 0.5,
      
      # UNESCO Biosphere Reserve – Buffer and Transition Zones
      # These zones combine human land use with conservation goals. Some management exists (e.g., through agri-environment schemes), but often not species-specific.
      IUCN_CAT_numeric == 9 ~ 0.5,
      
      # IUCN Category II – National Parks
      # Focus on ecosystem protection and recreation. Active plant-focused habitat management is not guaranteed; usually more passive.
      IUCN_CAT_numeric == 2 ~ 0.4,
      
      # UNESCO Biosphere Reserve – Core Zone
      # Strictly protected areas with minimal or no human intervention. Generally no active management — good for wilderness, but not for disturbance-dependent plant species.
      IUCN_CAT_numeric == 8 ~ 0.2,
      
      # Not Assigned / Not Applicable / Mixed or Undefined Protection Types
      # These categories lack clarity or legal obligation regarding habitat or species management. Assumed to have little or no active management.
      IUCN_CAT_numeric %in% c(10, 11) ~ 0.0,
      
      # Fallback default (in case of missing or unexpected category values)
      TRUE ~ 0.0
    )
  )



new_management <- intersected_mgmt %>%
  group_by(id) %>%
  summarise(
    mean_mgmt_new = sum(overlap_area * management_score, na.rm = TRUE) / sum(overlap_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    management_class = case_when(
      mean_mgmt_new >= 0.75 ~ "high",
      mean_mgmt_new >= 0.4  ~ "moderate",
      mean_mgmt_new > 0     ~ "low",
      TRUE                  ~ "none"
    )
  )

# Save the new management index to a GeoPackage
st_write(new_management, "./data/Protected-areas/new_management_index.gpkg", delete_layer = TRUE)
st
# Join the new management index with the existing filtered data
smon_filtered_updated <- smon_filtered %>%
  select(-mean_mgmt, -dominant_management) %>%
  left_join(
    new_management %>% st_set_geometry(NULL),  # removes geometry for joining
    by = "id"
  ) 
