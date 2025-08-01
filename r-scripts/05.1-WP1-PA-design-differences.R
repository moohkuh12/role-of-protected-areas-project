# Protection Design Performance
# This script should model differences between different protected area design categories and check if there were differences in the protection efficiency


# Load setup and libraries
source("./r-scripts/00-preamble.R")

# wide format of sMon data 
sMon_wide <- read_csv("./data/sMon/sMon_wide_210725.csv")
smon_filtered <- sMon_wide %>%
  filter(urban_class != "urban")
rm(sMon_wide)

# Load trait df or create it
traits_df <- read_csv("./data/sMon/sMon_traits.csv")

# Plot/ Map of PA-Design categories over Germany ------------
# Load the shapefile for Germany
# Load Bundesland boundaries
de_states <- st_read("https://raw.githubusercontent.com/isellsoap/deutschlandGeoJSON/main/2_bundeslaender/2_hoch.geo.json")
# Load full grid coverage
grid_sf_coverage_all <- st_read("./data/Protected-areas/grid_sf_protectionstatus.gpkg")
intersections <- st_read("./data/Protected-areas/Intermediate/intersected_protected_areas.shp")


# Set CRS and replace NA with "urban"
grid_plot_proj <- st_transform(grid_sf_coverage_all, crs = 25832)
grid_plot_proj$protection_cat <- as.character(grid_plot_proj$protection_cat)
grid_plot_proj$protection_cat <- forcats::fct_na_value_to_level(grid_plot_proj$protection_cat, level = "urban")
de_states_proj <- st_transform(de_states, crs = st_crs(grid_plot_proj))
# For urban layer, we use the grid cell ids that are not in the smon_filtered dataset - excluded urban ids in the beginning
urban_cells <- grid_plot_proj %>%
  filter(!id %in% smon_filtered$id)

iucn_cat_order <- c(
    "Ia/Ib",
    "II",
    "Biosphere Reserve (Core Zone)",  # stricter protection than buffer and transition
    "III",
    "IV",
    "V",
    "Habitats Directive (Natura 2000)",
    "Birds Directive (Natura 2000)",
    "Ramsar Site",
    "Biosphere Reserve (Buffer/Transition)",
    "Other"
  )
  
# Rename design names
label_map <- c(
  "Ia/Ib" = "IUCN Ia/Ib",
  "II" = "IUCN II",
  "Biosphere Reserve (Core Zone)" = "BR Core",
  "III" = "IUCN III",
  "IV" = "IUCN IV",
  "V" = "IUCN V",
  "Habitats Directive (Natura 2000)" = "N2000 Habitats",
  "Birds Directive (Natura 2000)" = "N2000 Birds",
  "Ramsar Site" = "Ramsar Site",
  "Biosphere Reserve (Buffer/Transition)" = "BR Buffer",
  "Other" = "Other"
)

grid_to_plot <- grid_plot_proj %>%
  filter(cov_frac >= 0.1) %>%
  mutate(
    # Set short version
    IUCN_CAT_final = factor(IUCN_CAT_final, levels = names(label_map)),
    IUCN_CAT_short = factor(label_map[as.character(IUCN_CAT_final)],
                            levels = unname(label_map)),
    
    # Reassign IUCN_CAT_final to use short labels directly (incl. "Other")
    IUCN_CAT_final = IUCN_CAT_short
  )


colors <- natparks.pals("Denali",n=10)

# 1. Create interpolated base palette (5 source colors → 11 target colors)
palette_11 <- colorRampPalette(natparks.pals("Denali", n = 5))(10)

# 2. Lighten the last color (for BR Buffer)
palette_11[9] <- lighten(palette_11[9], amount = 0.6) # ramsar site 
palette_11[10] <- lighten(palette_11[10], amount = 0.8) # BR Buffer 
palette_11[8] <- lighten(palette_11[8], amount = 0.6) 
palette_11[7] <- lighten(palette_11[7], amount = 0.35) 
#palette_11[6] <- lighten(palette_11[6], amount = 0.2) 
palette_11[3] <- lighten(palette_11[3], amount = 0.2) # br core
palette_11[1] <- darken(palette_11[1], amount = 0.3)  
palette_11[4] <- lighten(palette_11[4], amount = 0.3)
palette_11[5] <- darken(palette_11[5], amount = 0.1)
# Show result
scales::show_col(palette_11)


# 3. Get the full level vector
cat_levels <- levels(grid_to_plot$IUCN_CAT_final)  # should be 11 levels (BR Buffer included!)

# 4. Assign each color to the corresponding category
colors <- c(
  setNames(palette_11, levels(grid_to_plot$IUCN_CAT_final)[1:10]),
  "Other" = "#a2cacc"
)

# check if the levels match
names(colors) <- levels(grid_to_plot$IUCN_CAT_final)

# Legend------------------

categories <- levels(grid_to_plot$IUCN_CAT_short)

legend_df <- tibble(
  x = c(1:10, 12),  # "Other" bei x = 13 → rechts außen
  y = rep(1, 11),   # gleiche Höhe
  label = c(categories[1:10], "Other"),
  fill = c(colors[1:10], "#a2cacc")  # neutrales Grau
)



# Erstellen des finalen Legendenplots
legend_plot <- ggplot(legend_df, aes(x = x, y = y)) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.06))) + # add padding on sides

  
  # Rechtecke mit Abstand
  geom_tile(aes(fill = fill), width = 0.9, height = 4, show.legend = FALSE) +
  
  # Farbwerte verwenden
  scale_fill_identity() +
  
  # Senkrechte Labels direkt unter jedem Quadrat
  geom_text(aes(label = label), y = -6, angle = 90, hjust = 1, vjust = 0.5, size = 18, family = "roboto_condensed") +
  
  # ↑ Move the arrow line up by increasing y, ↓ move it down by decreasing y
  annotate("segment", x = 1, xend = 10, y = 15, yend = 15,  # ← increase y to move arrow up
           arrow = arrow(type = "closed", length = unit(0.2, "inches")), linewidth = 3, color= "#1e4436") +
  
  # ↑ Move the left-side label ("Process Protection") up by increasing y
  annotate("text", x = -4, y = 25, label = "Process Protection", hjust = 0,  # ← increase y to move label up
           size = 18, family = "roboto_condensed") +
  
  # ↑ Move the right-side label ("Cultural Landscape") up by increasing y
  annotate("text", x = 15.5, y = 25, label = "Cultural Landscape", hjust = 1,  # ← increase y to move label up
           size = 18, family = "roboto_condensed") +


  # Achsen & Hintergrund leeren
  theme_void() +
  coord_cartesian(xlim = c(0.5, 13), 
    clip = "off")+
  theme(
    plot.margin = margin(t = 2, r = 1, b = 118, l = 1),
    aspect.ratio = 0.15
  )


legend_plot

map_plot <- ggplot() +
  geom_sf(data = grid_plot_proj, fill = "white", color = "grey70", size = 0.01) +
  geom_sf(data = grid_to_plot, aes(fill = IUCN_CAT_final), color = "grey70", size = 0.01) +
  # # Urban layer in gray (at bottom or top depending on visibility)
   geom_sf(data = urban_cells, fill = "grey70") +
  # Urban layer with pattern
  # geom_sf_pattern(data = urban_cells,
  #                 pattern = "stripe",
  #                 pattern_angle = 45,
  #                 pattern_density = 0.2,
  #                 pattern_spacing = 0.01,
  #                 #pattern_fill = "#a2cacc",
  #                 fill = "grey70",
  #                 color = NA,
  #                 inherit.aes = FALSE) +
  geom_sf(data = de_states_proj, fill = NA, color = "#1e4436", linewidth=0.6, alpha= 0.85) +
  coord_sf(expand = FALSE) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.position = "none",  # we show own legend
    plot.margin = margin(0, 0, 3, 0)
  )


final_plot <- map_plot / legend_plot +
  plot_layout(heights = c(15, 1.1), widths = c(15, 2.3)) 




# Anzeigen
print(final_plot)

ggsave("./figures/pa_map.png", final_plot,
       width = 7.5, height = 11.5, dpi = 300, units = "in")


# colored intersected elements map ----------------
# 1. Extract dominant protection categories from grid-level data (no geometry)
cat_by_id <- grid_to_plot %>%
  st_set_geometry(NULL) %>%
  select(id, IUCN_CAT_final)

# 2. Join intersected geometries with dominant category info by ID
intersected_colored <- intersections %>%
  left_join(cat_by_id, by = "id")


# 2. Assign "urban" label based on ID membership
urban_cells <- grid_plot_proj %>%
  filter(!id %in% smon_filtered$id)



Design_polyg_map <- ggplot() +
  # Colors for protection categories
  geom_sf(data = intersected_colored, aes(fill = IUCN_CAT_final), color=NA) +
  # Urban layer in gray (at bottom or top depending on visibility)
  geom_sf(data = urban_cells, fill = "grey70") +
  # Borders
  geom_sf(data = grid_plot_proj, fill = NA, color = "grey70", size = 0.01) +
  # Borders of german states
  geom_sf(data = de_states_proj, fill = NA, color = "#1e4436", linewidth = 0.7) +
  labs( fill= "Protection Design")+
  scale_fill_manual(values = colors, na.translate = FALSE)+
  theme_void() +
  theme(
    text = element_text(family = "roboto_condensed"),
    # Legend styling
    legend.title = element_text(size = 75),
    plot.margin = margin(0, 0, 0, 0),
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 60)
  )
Design_polyg_map
# Save map to file
ggsave(
  filename = "./figures/protection_design_map.png",
  plot = Design_polyg_map,
  width = 11, height = 10.5, dpi = 300, units = "in", bg= "white")


# traits_df <- smon_filtered%>%
#   select(TaxonName, main_group, EIVEres.M, EIVEres.M.nw3,EIVEres.N, EIVEres.N.nw3, EIVEres.L, EIVEres.L.nw3, EIVEres.T, EIVEres.T.nw3, EIVEres.R, EIVEres.R.nw3, Family, PlantGrowthForm, Woodiness, LeafType, LeafPhenology ) %>%
#   distinct()

# Create aggregated dataframe for the analysis ------------
# (aggregation at species and protection level)

# Aggregate data by protection status per species 
# summary general --------------------------

# Define the short label mapping
label_map <- c(
  "Ia/Ib" = "IUCN Ia/Ib",
  "II" = "IUCN II",
  "Biosphere Reserve (Core Zone)" = "BR Core",
  "III" = "IUCN III",
  "IV" = "IUCN IV",
  "V" = "IUCN V",
  "Habitats Directive (Natura 2000)" = "N2000 Habitats",
  "Birds Directive (Natura 2000)" = "N2000 Birds",
  "Ramsar Site" = "Ramsar Site",
  "Biosphere Reserve (Buffer/Transition)" = "BR Buffer",
  "Other" = "Other"
)

# Colors for map are to light for boxplots- we use interpolated base palette (5 source colors → 11 target colors)
palette_11 <- colorRampPalette(natparks.pals("Denali", n = 5))(10)

# Show result
scales::show_col(palette_11)


# 3. Get the full level vector
cat_levels <- levels(grid_to_plot$IUCN_CAT_final)  # should be 11 levels (BR Buffer included!)

# 4. Assign each color to the corresponding category
colors <- c(
  setNames(palette_11, levels(grid_to_plot$IUCN_CAT_final)[1:10]),
  "Other" = "#a2cacc"
)

# will be our main dataframe for the models 
summary_iucn <- smon_filtered %>%
  filter(OP_T1 > 0, cov_frac > 0.1) %>%
  mutate(occ_change = OP_T3 - OP_T1) %>%
  group_by(TaxonName, IUCN_CAT_final) %>%
  summarise( # mean_occ_change and logratio (of SOP) will be our main predictors in the models
    SOP_T1 = sum(OP_T1, na.rm = TRUE),
    SOP_T3 = sum(OP_T3, na.rm = TRUE),
    log_SOP_T1 = log(SOP_T1 + 0.01),
    mean_occ_change = mean(occ_change, na.rm = TRUE),
    mean_OP_T1 = mean(OP_T1, na.rm = TRUE),  # this is our covariate - the initial log SOP (t1).
    n_cells = n(),
    protected_cells_binary = sum(protection_cat == "protected"),
    #.groups = "drop"  # can be used alternatively to ungroup()
  ) %>%
  mutate( # this chunk calculates the metrics on the basis of the summed occurence probabilities per species and protection level per cell
    delta_SOP = SOP_T3 - SOP_T1,
    rel_change = (SOP_T3 - SOP_T1) / SOP_T1 * 100,
    logratio = log((SOP_T3 + 0.01)/(SOP_T1 + 0.01)),
    percent_range_protected = protected_cells_binary / n_cells
  )%>%
  ungroup()

summary_iucn_clean <- summary_iucn %>%
  filter(!is.na(IUCN_CAT_final)) %>%
  droplevels()

# Recode IUCN_CAT_final directly with short labels - labels are created above
summary_iucn_clean <- summary_iucn_clean %>%
  mutate(
    IUCN_CAT_final = factor(
      label_map[as.character(IUCN_CAT_final)],
      levels = unname(label_map)
    )
  )

# Test at the aggregated level

m_iucn <- lmer(mean_occ_change ~ IUCN_CAT_final + (1 | TaxonName), data = summary_iucn_clean)
summary(m_iucn)

plot( ggpredict(m_iucn, terms = "IUCN_CAT_final") ) +
  labs(x = "IUCN Category", y = "Log Ratio of SOP (T3/T1)") +
  theme_minimal()

# plotting

preds <- ggpredict(m_iucn, terms = c( "IUCN_CAT_final"))
# Scale estiamtes to percentage
preds$predicted <- preds$predicted * 100
preds$conf.low <- preds$conf.low * 100
preds$conf.high <- preds$conf.high * 100
preds$group <- factor(preds$group)


# 1) left panel: half-violins + rawdatapoints + boxplot
p1_iucn <- ggplot(summary_iucn_clean, aes(x = IUCN_CAT_final, y = mean_occ_change*100)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
  # Violinplots
  geom_half_violin(
    data = summary_iucn_clean,
    aes(x = IUCN_CAT_final, y = mean_occ_change*100, fill = IUCN_CAT_final),
    side = "l", alpha = 0.6, color = NA, position = position_nudge(x = -0.05), scale= "count"
  ) +
  # slimmer boxplots
  geom_boxplot(
    data = summary_iucn_clean,
    aes(x = IUCN_CAT_final, y = mean_occ_change*100, fill = IUCN_CAT_final, color= IUCN_CAT_final),
    width = 0.3,
    outlier.shape = NA,
    linewidth = 0.8,
    position = position_nudge(x = 0.2),
    alpha = 0.6
  ) +
  # geom_jitter(aes(color = protection_cat), width  = 0.02, size   = 1.5,alpha  = 0.4) +
  labs(
    x = "Protection design",
    y = NULL,
    color = "Protection design",
    fill = "Protection design"
  ) +
  scale_fill_manual(values = colors)+
scale_color_manual(values = colors)+

  ylim(-30, 30) +

  theme_bw(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    text             = element_text(family = "roboto_condensed"),
    legend.title     = element_text(size = 80),
    legend.text      = element_text(size = 70),
    axis.title.x     = element_blank(),       # no x-axis title
    axis.title.y     = element_text(size = 80),
    axis.text.x      = element_text(size = 90), # no x-axis text
    axis.text.y      = element_text(size = 80),
    legend.position  = "none"
  )
p1_iucn

preds$x <- factor(preds$x, levels = levels(summary_iucn_clean$IUCN_CAT_final))



# 2) right panel - Preds with SE
p2_iucn <- ggplot() +
  # Modelestimates with Errorbars (from`preds`)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
  # datapoints (from `summary_general`)
  geom_jitter(
    data = summary_iucn_clean,
    aes(x = IUCN_CAT_final, y = mean_occ_change * 100, color = IUCN_CAT_final),
    width = 0.2,
    size = 2,
    alpha = 0.1
  ) +
  # estimates and errorbars
  geom_errorbar(
    data = preds,
    aes(x = x, ymin = conf.low, ymax = conf.high, color = x),
    width = 0.03,
    color="black",
    linewidth = 1,
    #  position = position_nudge(x = 0.12)
  ) +
  geom_point(
    data = preds,
    aes(x = x, y = predicted, color = x),
    size = 4,
    color= "black",
    #  position = position_nudge(x = 0.12),
    alpha = 0.7
  ) +
  ylim(-35,35)+
  
  coord_flip() +
  #scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.position = "none",
    axis.text.y     = element_blank(), 
    axis.ticks.y    = element_blank(),
    #axis.text.y = element_text(lineheight = 0.8),  
    axis.title.x    = element_text(size = 70),
    axis.text.x     = element_text(size = 80)
  )
p2_iucn



p3_iucn <- ggplot() +
  # Modelestimates with Errorbars (from`preds`)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
  # # datapoints (from `summary_general`)
  # geom_jitter(
  #   data = summary_iucn_clean,
  #   aes(x = IUCN_CAT_final, y = mean_occ_change * 100, color = IUCN_CAT_final),
  #   width = 0.2,
  #   size = 2,
  #   alpha = 0.1
  # ) +
  # estimates and errorbars
  geom_errorbar(
    data = preds,
    aes(x = x, ymin = conf.low, ymax = conf.high, color = x),
    width = 0.3,
   # color="black",
    linewidth = 1.2,
    #  position = position_nudge(x = 0.12)
  ) +
  geom_point(
    data = preds,
    aes(x = x, y = predicted, color = x),
    size = 5,
    #color= "black",
    #  position = position_nudge(x = 0.12),
    alpha = 0.8
  ) +
  #ylim(-35,35)+
  
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
  # scale_color_manual(
  #   values = c(
  #     "not protected" = "#9ab0c3",  
  #     "part protected" = "#0077AA",
  #     "protected" = "#005B96"
  #   )
  # ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.position = "none",
    axis.text.y     = element_blank(), 
    axis.ticks.y    = element_blank(),
    axis.title.x    = element_text(size = 70),
    axis.text.x     = element_text(size = 80)
  )
p3_iucn

# 3) both plots together

p_patch <- p1_iucn + p2_iucn & xlab(NULL) & theme(plot.margin = margin(5.5, 20, 0, 5.5))
p_patch2 <- p1_iucn + p3_iucn & xlab(NULL) & theme(plot.margin = margin(5.5, 20, 0, 5.5))
iucn_plot <- wrap_elements(panel = p_patch) +
  labs(tag = "Predicted mean occurrence change (%)") +
  theme(
    text  = element_text(family = "roboto_condensed"),
    plot.tag = element_text(size = rel(8)),
    plot.tag.position = "bottom"
  )
iucn_plot2 <- wrap_elements(panel = p_patch2) +
  labs(tag = "Predicted mean occurrence change (%)") +
  theme(
    text  = element_text(family = "roboto_condensed"),
    plot.tag = element_text(size = rel(8)),
    plot.tag.position = "bottom"
  )

# Save the plot
ggsave(
  filename = "./figures/iucn-protection-design-performance.png",
  plot = iucn_plot,
  width = 53,
  height = 25,
  units = "cm",
  dpi = 300
)
ggsave(
  filename = "./figures/iucn-protection-design-performance2.png",
  plot = iucn_plot2,
  width = 53,
  height = 25,
  units = "cm",
  dpi = 300
)
