# Main figure 1: Map of Germany with four Panels
# Panel 1: SOPcell at T1 (i.e. 1960-1987)
# Panel 2: Loss of SOPcell towards T2 (i.e. 1997-2017)
# Panel 3: Percentage of Protection per Grid-Cell
# Panel 4: Percentage of Protection + Loss of SOPcell

# Load setup and libraries
source("./r-scripts/00-preamble.R")

# -----------------------------------------------
# PROTECTION STATUS MAP - Panel 3
# -----------------------------------------------
dichromat::colorschemes$LightBluetoDarkBlue.7
dichromat_palette <- c("#FFFFFF", "#CCFDFF" ,"#99F8FF" ,"#66F0FF" ,"#33E4FF" ,"#00AACC" ,"#007A99")
# scale_color_gradientn(colors = dichromat::colorschemes$LightBluetoDarkBlue.7)
theme_map_base <- theme_minimal(base_family = "roboto_condensed", base_size = 16) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = rel(1.8)),
    legend.text  = element_text(size = rel(1.3)),
    legend.key.height = unit(4, "cm"),
    legend.key.width  = unit(0.6, "cm"),
    plot.margin = margin(2, 2, 2, 2, unit = "mm")
  )


# Load full grid coverage
grid_sf_coverage_all <- st_read("./data/Protected-areas/grid_sf_protectionstatus.gpkg")

# Extract unique protection status per grid cell
protection_status <- smon_filtered %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, protection_cat)

# Join with grid geometries
grid_plot <- grid_sf_coverage_all %>%
  left_join(protection_status, by = "id") %>%
  mutate(protection_cat = factor(protection_cat, levels = c("not protected", "part protected", "protected")))

# Set CRS and replace NA with "urban"
grid_plot_proj <- st_transform(grid_plot, crs = 25832)
grid_plot_proj$protection_cat <- forcats::fct_na_value_to_level(grid_plot_proj$protection_cat, level = "urban")

# PA intersections per grid cell
intersected <- st_read("data/Protected-areas/Intermediate/intersected_protected_areas.shp")
intersected <- st_transform(intersected, crs = st_crs(grid_plot_proj))

# Load Bundesland boundaries
de_states <- st_read("https://raw.githubusercontent.com/isellsoap/deutschlandGeoJSON/main/2_bundeslaender/2_hoch.geo.json")
de_states_proj <- st_transform(de_states, crs = st_crs(grid_plot_proj))

# Plot protection status map
protection_map <- ggplot() +
  geom_sf(data = grid_plot_proj, aes(fill = protection_cat), color = "black", size = 0.1, alpha = 0.85) +
  geom_sf(data = de_states_proj, fill = NA, color = "grey30", linewidth = 0.8, alpha = 0.5) +
  geom_sf(data = de_states_proj, fill = NA, color = "grey15", linewidth = 0.6) +
  scale_fill_manual(values = dichromat_palette, drop = FALSE) +
  coord_sf(datum = NA, expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 15)) +
  labs(fill = "Protection")

# Protection gradient map --------------

# Exclude urban cells by setting them to NA for plotting
grid_combined <- grid_plot_proj %>%
  mutate(cov_frac_display = ifelse(protection_cat == "urban", NA, cov_frac))

# Create the protection coverage map
protection_map <- ggplot() +
  # Draw grid cells with fill based on protection coverage
  geom_sf(
    data = grid_combined,
    aes(fill = cov_frac_display),
    color = "grey20",
    size = 0.1,
    alpha = 0.85
  ) +
  # Overlay state boundaries
  geom_sf(
    data = de_states_proj,
    fill = NA,
    color = "grey30",
    linewidth = 1
  ) +
  
  # Color gradient scale using a dichromat-friendly blue palette
  scale_fill_gradientn(
    colors = dichromat::colorschemes$LightBluetoDarkBlue.7,
    na.value = "#b3afa7",            # color for NA (e.g. urban cells)
    name = "Protection",            # legend title
    guide = guide_colorbar(
      direction = "vertical",       # vertical legend
      barheight = unit(20, "cm"),   # height of the gradient bar
      barwidth = unit(1, "cm"),     # width of the gradient bar
      title.vjust = 4               # push legend title slightly upward
    )
  ) +
  
  # Coordinate settings (no datum displayed)
  coord_sf(datum = NA, expand = FALSE) +
  
  # Base theme settings
  theme_minimal(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    
    # Legend styling
    legend.title = element_text(size = 90),
    legend.text = element_text(size = 80),
    
    # Axis styling
    axis.title.x = element_blank(),          # remove x-axis title
    axis.title.y = element_text(size = 90),
    axis.text.x = element_text(size = 80),
    axis.text.y = element_text(size = 74)
  )

# Display the map
protection_map

# Save map to file
ggsave(
  filename = "./figures/protection_cov_frac_gradient.png",
  plot = protection_map,
  width = 20,       # in cm
  height = 25,      # in cm
  dpi = 200,        # resolution
  bg = "white"      # background color
)

ggplot() +
  geom_sf(data = grid_combined, aes(fill = cov_frac), color = "black", size = 0.1, alpha = 0.85) +
  scale_fill_viridis_c(option = "viridis", name = "Protected area\nfraction") +
  coord_sf(datum = NA, expand = TRUE) +
  theme_minimal(base_family = "sans", base_size = 60) +
  theme(
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18)
  )


ggsave("./figures/protection_card.png", plot = protection_map, width = 10, height = 8, dpi = 300)


# SOP T1 Map - Panel 1 -----------------------------

# 1. Calculate the summed occurrence probability (SOP) per grid cell (baseline period T1: 1960–1987)
smon_sopgrid <- smon_filtered %>%
  group_by(id) %>%
  summarise(summed_OP = sum(OP_T1, na.rm = TRUE))  # sum occurrence probability per cell

# 2. Combine with grid geometry (sf object)
grid_sop <- grid_sf_coverage_all %>%
  left_join(smon_sopgrid, by = "id")

# 3. Plot the SOP as a heatmap
sr_plot <- ggplot() +
  # Main layer: fill grid cells by summed occurrence probability
  geom_sf(data = grid_sop, aes(fill = summed_OP), color = "gray20") +
  
  # Fill gradient using a dichromat-friendly blue palette
  scale_fill_gradientn(
    colors = dichromat::colorschemes$LightBluetoDarkBlue.7,
    na.value = "#b3afa7",               # color for NA (e.g. urban or missing cells)
    name = "Summed \noccurrence\nprobability",  # legend title (3 lines)
    guide = guide_colorbar(
      direction = "vertical",           # vertical layout of colorbar
      barheight = unit(20, "cm"),       # colorbar height
      barwidth  = unit(1, "cm"),        # colorbar width
      title.vjust = 4                   # shift legend title slightly upward
    )
  ) +
  
  # Overlay state boundaries
  geom_sf(data = de_states_proj, fill = NA, color = "grey30", linewidth = 1) +
  
  # Set map projection and coordinate system
  coord_sf(datum = NA, expand = FALSE) +
  
  # Apply minimal theme and font settings
  theme_minimal(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),

    # Customize legend text and title
    legend.title = element_text(size = 90, lineheight = 0.4),
    legend.text  = element_text(size = 80, lineheight = 0.1),

    # Axis elements
    axis.title.x = element_blank(),          # no x-axis title
    axis.title.y = element_text(size = 90),
    axis.text.x  = element_text(size = 80),
    axis.text.y  = element_text(size = 74)
  )
  #theme_map_base

# 4. Export the heatmap to PNG
ggsave(
  filename = "./figures/sop_grid_map.png",
  plot = sr_plot,
  width = 20,        # in cm
  height = 25,       # in cm
  dpi = 200, # resolution in dots per inch
  bg = "white"       # white background
)

# Same for t2:
smon_sopgrid <- smon_filtered %>%
  group_by(id) %>%
  summarise(summed_OP = sum(OP_T3, na.rm = TRUE))  # T3 = 1997-2017

# 2. Kombiniere mit Grid-Geometrien
grid_sop <- grid_sf_coverage_all %>%
  left_join(smon_sopgrid, by = "id")

# 3. Plot als Heatmap
sr_plot2 <- ggplot() +
  geom_sf(data = grid_sop, aes(fill = summed_OP), color = NA) +
  scale_fill_viridis(
    option = "inferno",
    na.value = "grey30", name = "Summed \noccurrence\nprobability",
    limits = c(0, 160),  # Set same upper limit as in T1
    oob = scales::squish # to handle values outside the limits
  ) +
  geom_sf(data = de_states_proj, fill = NA, color = "grey70", linewidth = 0.5) +
  coord_sf(datum = NA, expand = FALSE) +
  theme_minimal(base_size = 18) +
  theme(
    legend.title = element_text(size = 50, lineheight = 0.3, family = "roboto_condensed"),
    legend.text = element_text(size = 45, family = "roboto_condensed"),
    text = element_text(family = "roboto_condensed")  # für alle Textelemente
  )

ggsave("./figures/sopgrid_card_t3.png", plot = sr_plot2, width = 10, height = 8, dpi = 300)


# Delta SOPcell to T2 - Panel 2 -----------------------------

smon_deltasop <- smon_filtered %>%
  group_by(id) %>%
  summarise(
    SOP_T1 = sum(OP_T1, na.rm = TRUE),
    SOP_T2 = sum(OP_T3, na.rm = TRUE),
    delta_SOP = SOP_T2 - SOP_T1,
    rel_loss = ifelse(SOP_T1 > 0, (delta_SOP / SOP_T1) * 100, NA)
  )


# Combine with grid geometries
grid_delta <- grid_sf_coverage_all %>%
  left_join(smon_deltasop, by = "id")


# Plot the delta SOP as a heatmap
delta_plot <- ggplot() +
  geom_sf(data = grid_delta, aes(fill = delta_SOP), color = "gray20") +
  
  scale_fill_gradientn(colors = rev(dichromat::colorschemes$LightBluetoDarkBlue.7),
                       name = "Δ Summed\noccurrence\nprobability",
                       guide = guide_colorbar(
                         direction = "vertical",
                         barheight = unit(20, "cm"),
                         barwidth = unit(1, "cm"),
                         title.vjust = 4
                       ),
                       na.value = "#b3afa7"
  ) +
  
  geom_sf(data = de_states_proj, fill = NA, color = "grey30", linewidth = 1) +
  
  coord_sf(datum = NA, expand = FALSE) +
  
  theme_minimal(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 90, lineheight = 0.4),
    legend.text  = element_text(size = 80, lineheight = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 90),
    axis.text.x  = element_text(size = 80),
    axis.text.y  = element_text(size = 74)
  )

ggsave(
  filename = "./figures/delta_sop_grid_map.png",
  plot = delta_plot,
  width = 20,
  height = 25,
  dpi = 200,
  bg = "white"
)

delta_plot_rel <- ggplot() +
  geom_sf(data = grid_delta, aes(fill = rel_loss), color = "gray20") +
  
  scale_fill_gradientn(
    colors = rev(dichromat::colorschemes$LightBluetoDarkBlue.7),
    limits = c(-40, 0),
    name = "Relative\nloss [%]",
    na.value = "#b3afa7",
    guide = guide_colorbar(
      direction = "vertical",
      barheight = unit(20, "cm"),
      barwidth = unit(1, "cm"),
      title.vjust = 4
    )
  ) +
  
  geom_sf(data = de_states_proj, fill = NA, color = "grey30", linewidth = 1) +
  
  coord_sf(datum = NA, expand = FALSE) +
  
  theme_minimal(base_family = "roboto_condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 90, lineheight = 0.4),
    legend.text  = element_text(size = 80, lineheight = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 90),
    axis.text.x  = element_text(size = 80),
    axis.text.y  = element_text(size = 74)
  )

ggsave(
  filename = "./figures/rel_delta_sop_grid_map.png",
  plot = delta_plot_rel,
  width = 20,
  height = 25,
  dpi = 200,
  bg = "white"
)


# Panel 4 ---------------------------
# Join protection_cat to delta grid
grid_delta_prot <- grid_delta %>%
  left_join(
    grid_plot_proj %>%
      st_drop_geometry() %>%
      select(id, protection_cat),
    by = "id"
  )



delta_prot_overlay_frac <- ggplot() +
  # Background: delta SOP
  geom_sf(data = grid_delta_prot, aes(fill = delta_SOP), color = "grey30") +
  
  # Overlay: fill with semi-transparent yellow depending on cov_frac
  geom_sf(
    data = grid_delta_prot %>% filter(!is.na(cov_frac)),
    aes(alpha = cov_frac),
    fill = "gold",
    color = NA
  ) +
  
  scale_alpha_continuous(range = c(0, 0.5), guide = "none") +  # optional legend
  scale_fill_gradientn(
    colors = rev(dichromat::colorschemes$LightBluetoDarkBlue.7),
    name = "Δ Summed\noccurrence\nprobability",
    guide = guide_colorbar(
      direction = "vertical",
      barheight = unit(20, "cm"),
      barwidth = unit(1, "cm"),
      title.vjust = 4
    ), na.value = "#b3afa7"
  ) +
  geom_sf(data = de_states_proj, fill = NA, color = "grey30", linewidth = 1) +
  coord_sf(datum = NA, expand = FALSE) +
  theme_minimal(base_family = "roboto_condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 90, lineheight = 0.4),
    legend.text  = element_text(size = 80, lineheight = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 90),
    axis.text.x  = element_text(size = 80),
    axis.text.y  = element_text(size = 74)
  )

ggsave(
  filename = "./figures/delta_sop_Protection_map.png",
  plot = delta_prot_overlay_frac,
  width = 20,
  height = 25,
  dpi = 200,
  bg = "white"
)

rel_delta_prot_overlay_frac <- ggplot() +
  # Background: delta SOP
  geom_sf(data = grid_delta_prot, aes(fill = rel_loss), color = "grey30") +
  
  # Overlay: fill with semi-transparent yellow depending on cov_frac
  geom_sf(
    data = grid_delta_prot %>% filter(!is.na(cov_frac)),
    aes(alpha = cov_frac),
    fill = "gold",
    color = NA
  ) +
  
  scale_alpha_continuous(range = c(0, 0.5), #name= "Protection"
                         guide = "none") +  # optional legend
  scale_fill_gradientn(
    colors = rev(dichromat::colorschemes$LightBluetoDarkBlue.7),
    limits = c(-40, 0),
    name = "Relative\nloss [%]",
    na.value = "#b3afa7",
    guide = guide_colorbar(
      direction = "vertical",
      barheight = unit(20, "cm"),
      barwidth = unit(1, "cm"),
      title.vjust = 4
    )
  ) +
  geom_sf(data = de_states_proj, fill = NA, color = "grey30", linewidth = 1) +
  coord_sf(datum = NA, expand = FALSE) +
  theme_minimal(base_family = "roboto_condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 90, lineheight = 0.4),
    legend.text  = element_text(size = 80, lineheight = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 90),
    axis.text.x  = element_text(size = 80),
    axis.text.y  = element_text(size = 74)
  )

ggsave(
  filename = "./figures/rel_delta_sop_Protection_map.png",
  plot = rel_delta_prot_overlay_frac,
  width = 20,
  height = 25,
  dpi = 200,
  bg = "white"
)

# External Test: Management --------------

grid_filtered <- grid_plot_proj %>%
  filter(cov_frac > 0.5)

management_map <- ggplot() +
  geom_sf(
    data = grid_filtered,
    aes(fill = mean_mgmt_new),
    color = "grey20",
    size = 0.1,
    alpha = 0.9
  ) +
  geom_sf(
    data = de_states_proj,
    fill = NA,
    color = "grey30",
    linewidth = 1
  ) +
  scale_fill_gradientn(
    colors = dichromat::colorschemes$LightBluetoDarkBlue.7,
    na.value = "grey50",  
    name = "Management score",
    limits = c(0, 1),
    guide = guide_colorbar(
      direction = "vertical",
      barheight = unit(20, "cm"),
      barwidth = unit(1, "cm"),
      title.vjust = 3
    )
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  theme_classic(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 90),
    legend.text = element_text(size = 80),
    legend.spacing = unit(0.2, "cm")
  )

# Show the map
management_map

# Save the management map to a file
ggsave(
  filename = "./figures/management_gradient_protected_only.png",
  plot = management_map,
  width = 20,
  height = 25,
  dpi = 200,
  bg = "white"
)