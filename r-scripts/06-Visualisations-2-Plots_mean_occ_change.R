# Load setup and libraries
source("./r_scripts/00-preamble.R")

# This is the corresponding plot script to the 05-WP1-data_analysis_MODELS_PAeffects.R script.
# The names of the models are:
# 1 General models: 
# m1 - mean_occ_change ~ protection_cat
# m1.1 - mean_occ_change ~ protection_90
# m1.2 - mean_occ_change ~ protection_80
# m1.3 - mean_occ_change ~ protection_70
# m1.4 - mean_occ_change ~ protection_60
# m1.5 - mean_occ_change ~ protection_50

# 2 Models for Nitrogen interaction:
# m2.1 - mean_occ_change ~ protection_cat * EIVEres.N
# m2.1.1 - mean_occ_change ~ protection_90 * EIVEres.N
# m2.1.2 - mean_occ_change ~ protection_80 * EIVEres.N
# m2.1.3 - mean_occ_change ~ protection_70 * EIVEres.N
# m2.1.4 - mean_occ_change ~ protection_60 * EIVEres.N
# m2.1.5 - mean_occ_change ~ protection_50 * EIVEres.N

# 3 Models for Ph Reaction interaction:
# m2.2 - mean_occ_change ~ protection_cat * EIVEres.R
# m2.2.1 - mean_occ_change ~ protection_90 * EIVEres.R
# m2.2.2 - mean_occ_change ~ protection_80 * EIVEres.R
# m2.2.3 - mean_occ_change ~ protection_70 * EIVEres.R
# m2.2.4 - mean_occ_change ~ protection_60 * EIVEres.R
# m2.2.5 - mean_occ_change ~ protection_50 * EIVEres.R

# 4 Models for Temperature interaction:
# m2.3 - mean_occ_change ~ protection_cat * EIVEres.T
# m2.3.1 - mean_occ_change ~ protection_90 * EIVEres.T
# m2.3.2 - mean_occ_change ~ protection_80 * EIVEres.T
# m2.3.3 - mean_occ_change ~ protection_70 * EIVEres.T
# m2.3.4 - mean_occ_change ~ protection_60 * EIVEres.T
# m2.3.5 - mean_occ_change ~ protection_50 * EIVEres.T

# 5 Models for Moisture interaction:
# m2.4 - mean_occ_change ~ protection_cat * EIVEres.M
# m2.4.1 - mean_occ_change ~ protection_90 * EIVEres.M
# m2.4.2 - mean_occ_change ~ protection_80 * EIVEres.M
# m2.4.3 - mean_occ_change ~ protection_70 * EIVEres.M
# m2.4.4 - mean_occ_change ~ protection_60 * EIVEres.M
# m2.4.5 - mean_occ_change ~ protection_50 * EIVEres.M

# 6 Models for Light interaction:
# m2.5 - mean_occ_change ~ protection_cat * EIVEres.L
# m2.5.1 - mean_occ_change ~ protection_90 * EIVEres.L
# m2.5.2 - mean_occ_change ~ protection_80 * EIVEres.L
# m2.5.3 - mean_occ_change ~ protection_70 * EIVEres.L
# m2.5.4 - mean_occ_change ~ protection_60 * EIVEres.L
# m2.5.5 - mean_occ_change ~ protection_50 * EIVEres.L

# 7 Models for Main Group interaction:
# m2.6 - mean_occ_change ~ protection_cat * main_group
# m2.6.1 - mean_occ_change ~ protection_90 * main_group
# m2.6.2 - mean_occ_change ~ protection_80 * main_group
# m2.6.3 - mean_occ_change ~ protection_70 * main_group
# m2.6.4 - mean_occ_change ~ protection_60 * main_group
# m2.6.5 - mean_occ_change ~ protection_50 * main_group

# 8 Models for Plant Growth Form interaction:
# m2.7 - mean_occ_change ~ protection_cat * PlantGrowthForm
# m2.7.1 - mean_occ_change ~ protection_90 * PlantGrowthForm
# m2.7.2 - mean_occ_change ~ protection_80 * PlantGrowthForm
# m2.7.3 - mean_occ_change ~ protection_70 * PlantGrowthForm
# m2.7.4 - mean_occ_change ~ protection_60 * PlantGrowthForm
# m2.7.5 - mean_occ_change ~ protection_50 * PlantGrowthForm

# 9 Models for Woodiness interaction:
# m2.8 - mean_occ_change ~ protection_cat * Woodiness
# m2.8.1 - mean_occ_change ~ protection_90 * Woodiness
# m2.8.2 - mean_occ_change ~ protection_80 * Woodiness
# m2.8.3 - mean_occ_change ~ protection_70 * Woodiness
# m2.8.4 - mean_occ_change ~ protection_60 * Woodiness
# m2.8.5 - mean_occ_change ~ protection_50 * Woodiness

# 10 Models for Leaf Type interaction:
# m2.9 - mean_occ_change ~ protection_cat * LeafType
# m2.9.1 - mean_occ_change ~ protection_90 * LeafType
# m2.9.2 - mean_occ_change ~ protection_80 * LeafType
# m2.9.3 - mean_occ_change ~ protection_70 * LeafType
# m2.9.4 - mean_occ_change ~ protection_60 * LeafType
# m2.9.5 - mean_occ_change ~ protection_50 * LeafType

# Colorpalettes:
acadia_custom <-  c("#72874EFF","#D4AE4F","#023743FF")  
eiv_n_colors <-   c("#5D7E81","#A8994D","#C15C39")
main_group_colors <- c(
  "forest specialist" = "#6F4742",   # erdiges Dunkelrotbraun
  "generalist"     = "#8A837C",   # neutrales Steingrau
  "open specialist"      = "#C4B45F" ,   # warmes, gedecktes Gelbgrün
  "montane forest"            = "#3B5F41",  # kühles, gedecktes Tannengrün
  "montane open"              = "#A7B49D"   # weiches, moosiges Graugrün
) 

pgf_colors <- c(
    "fern"        = "#BF7C63",  # warmes Ziegelrot – leicht rötlich, gedeckt
    "graminoid"   = "#B5A442",  # Senf-Oliv – harmoniert mit D4AE4F
    "herb"        = "#72874E",  # olivegrün – passt exakt zu acadia_custom[1]
    "herb/shrub"  = "#4D6B5B",  # gedecktes Blaugraugrün – unauffällig, unterscheidbar
    "shrub"       = "#577590",  # dezentes Graublau – klar, aber ruhig
    "shrub/tree"  = "#7E6686",  # Lavendelgrau – leicht violett, aber matt
    "tree"        = "#023743"   # tiefes Petrol – passt zu acadia_custom[3]
  )
woodiness_colors <- c(
  "non-woody"         = "#88A27F",  # gedämpftes Mittelgrün – weich, krautig
  "non-woody/woody"   = "#A08F6F",  # schlammiges Olivbeige – neutral, Übergang
  "woody"             = "#6B4F3B"   # sattes Waldbraun – holzig, stabil
)

leaf_type_colors <- c(
  "broadleaved"     = "#7D9A65",  # sattes, natürliches Blattgrün
  "microphylle"     = "#A5A960",  # mattes Gelboliv, feiner Laubtyp
  "needleleaved"    = "#5A6E68",  # kühles Graugrün – nadelig, robust
  "scale-shaped"    = "#847E75",  # neutrales Schlammgrau – überlappend, unscheinbar
  "without leaves"  = "#B5B1AA"   # blassgrau – leblos, abstrahiert
)

# 1 General Models - Plots: ---------------------------------------------
# m1 - mean_occ_change ~ protection_cat

preds <- ggpredict(m1, terms = c( "protection_cat"))
# Scale estiamtes to percentage
preds$predicted <- preds$predicted * 100
preds$conf.low <- preds$conf.low * 100
preds$conf.high <- preds$conf.high * 100
preds$group <- factor(preds$group)

# 1) left panel: half-violins + rawdatapoints + boxplot
p1 <- ggplot(summary_general, aes(x = protection_cat, y = mean_occ_change*100)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
  # Violinplots
  geom_half_violin(
    data = summary_general,
    aes(x = protection_cat, y = mean_occ_change*100, fill = protection_cat),
    side = "l", alpha = 0.45, color = NA, position = position_nudge(x = -0.05), scale= "count"
  ) +
  # slimmer boxplots
  geom_boxplot(
    data = summary_general,
    aes(x = protection_cat, y = mean_occ_change*100, fill = protection_cat, color= protection_cat),
    width = 0.1,
    outlier.shape = NA,
    linewidth = 0.8,
    position = position_nudge(x = 0.1),
    alpha = 0.7
  ) +
 # geom_jitter(aes(color = protection_cat), width  = 0.02, size   = 1.5,alpha  = 0.4) +
  labs(
    x = "Protection status",
    y = NULL,
    color = "Protection",
    fill = "Protection"
  ) +
  scale_fill_manual(#values = acadia_custom
    values = c("not protected" = "#9cc1e2", "part protected" = "#6497b1", "protected" = "#005b96")) +
  scale_color_manual(
    values = c(
      "not protected" = "#9ab0c3",   # deutlich dunkler als hellblau
      "part protected" = "#0077AA",
      "protected" = "#005B96")
  )+
  theme_bw(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    text             = element_text(family = "roboto_condensed"),
    legend.title     = element_text(size = 80),
    legend.text      = element_text(size = 70),
    axis.title.x     = element_blank(),       # no x-axis title
    axis.title.y     = element_text(size = 80),
    axis.text.x      = element_text(size = 70), # no x-axis text
    axis.text.y      = element_text(size = 64),
    legend.position  = "none"
  )
p1
# 2) right panel - Preds with SE
p2 <- ggplot() +
  # Modelestimates with Errorbars (from`preds`)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
  # datapoints (from `summary_general`)
  geom_jitter(
    data = summary_general,
    aes(x = protection_cat, y = mean_occ_change * 100, color = protection_cat),
    width = 0.09,
    size = 1.5,
    alpha = 0.15
  ) +
  # estimates and errorbars
  geom_errorbar(
    data = preds,
    aes(x = x, ymin = conf.low, ymax = conf.high, color = "black"),
    width = 0.03,
    color="black",
    linewidth = 1,
  #  position = position_nudge(x = 0.12)
  ) +
  geom_point(
    data = preds,
    aes(x = x, y = predicted, color = "black"),
    size = 2,
    color= "black",
  #  position = position_nudge(x = 0.12),
    alpha = 0.7
  ) +
  
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  
  scale_color_manual(
    values = c(
      "not protected" = "#9ab0c3",  
      "part protected" = "#0077AA",
      "protected" = "#005B96"
    )
  ) +
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
    axis.text.x     = element_text(size = 70)
  )
p2

# 3) both plots together

p_patch <- p1 + p2 & xlab(NULL) & theme(plot.margin = margin(5.5, 20, 0, 5.5))
m1.2_persistplot <- wrap_elements(panel = p_patch) +
  labs(tag = "Predicted mean occurrence change (%)") +
  theme(
    text  = element_text(family = "roboto_condensed"),
    plot.tag = element_text(size = rel(8)),
    plot.tag.position = "bottom"
  )

# 5) save the plot
ggsave("./figures/occ_change_comparison010725.png",
       m1.2_persistplot,
       width = 40, height = 20, units = "cm", dpi = 300)

# m1.1 - m1.5 - mean_occ_change ~ protection_90 - 50 - Plots: ---------------------------------------------


# Define custom color palette
custom_colors <- c(
  "not protected" = "#9cc1e2",
  "part protected" = "#6497b1",
  "protected" = "#005b96"
)

# List of models and protection variables
model_list <- list(
  m1.1 = m1.1,
  m1.2 = m1.2,
  m1.3 = m1.3,
  m1.4 = m1.4,
  m1.5 = m1.5
)

protection_vars <- c(
  m1.1 = "protection90",
  m1.2 = "protection80",
  m1.3 = "protection70",
  m1.4 = "protection60",
  m1.5 = "protection50"
)

for (model_name in names(model_list)) {
  
  m1 <- model_list[[model_name]]
  protection_var <- protection_vars[[model_name]]
  
  summary_prot <- get(paste0("summary_prot", substr(protection_var, 11, 12)))
  stopifnot(protection_var %in% names(summary_prot))
  
  preds <- ggpredict(m1, terms = protection_var)
  preds$predicted <- preds$predicted * 100
  preds$conf.low <- preds$conf.low * 100
  preds$conf.high <- preds$conf.high * 100
  preds$group <- factor(preds$group)
  
  # Panel 1: Violin + Box + Rohdaten
  p1 <- ggplot(summary_general, aes_string(x = "protection_cat", y = "mean_occ_change * 100")) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) +
    
    geom_half_violin(
      data = summary_prot,
      aes_string(x = protection_var, y = "mean_occ_change*100", fill = protection_var),
      side = "l", alpha = 0.45, color = NA, position = position_nudge(x = -0.05), scale = "count"
    ) +
    
    geom_boxplot(
      data = summary_prot,
      aes_string(x = protection_var, y = "mean_occ_change*100", fill = protection_var, color = protection_var),
      width = 0.1,
      outlier.shape = NA,
      linewidth = 0.8,
      position = position_nudge(x = 0.1),
      alpha = 0.7
    ) +
    
    geom_jitter(
      aes(color = protection_cat),
      width = 0.02,
      size = 1.5,
      alpha = 0.4
    ) +
    
    labs(
      title = paste("Protection threshold:", substr(protection_var, 11, 12), "%"),
      x = protection_var,
      y = NULL,
      color = "Protection",
      fill = "Protection"
    ) +
    
    scale_fill_manual(values = custom_colors) +
    scale_color_manual(values = custom_colors) +
    
    theme_bw(base_family = "Roboto Condensed", base_size = 13) +
    theme(
      text = element_text(family = "roboto_condensed"),
      legend.title = element_text(size = 80),
      legend.text = element_text(size = 70),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 80),
      axis.text.x = element_text(size = 70),
      axis.text.y = element_text(size = 64),
      title = element_text(size = 80),
      legend.position = "none"
    )
  
  # Panel 2: Modellvorhersagen + Rohdaten (jitter)
  p2 <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) +
    
    geom_jitter(
      data = summary_prot,
      aes_string(x = protection_var, y = "mean_occ_change * 100", color = protection_var),
      width = 0.09,
      size = 1.5,
      alpha = 0.15
    ) +
    
    geom_errorbar(
      data = preds,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      width = 0.03,
      color = "black",
      linewidth = 1
    ) +
    
    geom_point(
      data = preds,
      aes(x = x, y = predicted),
      size = 3,
      color = "black",
      alpha = 0.7
    ) +
    
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    scale_color_manual(values = custom_colors) +
    
    labs(x = NULL, y = NULL) +
    theme_bw(base_family = "Roboto Condensed", base_size = 13) +
    theme(
      text = element_text(family = "roboto_condensed"),
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(size = 70),
      axis.text.x = element_text(size = 70)
    )
  
  # Combine panels
  final_plot <- wrap_elements(panel = p1 + p2 & xlab(NULL) & theme(plot.margin = margin(5.5, 20, 0, 5.5))) +
    labs(tag = paste0("Predicted mean occurrence change (%)")) +
    theme(
      text = element_text(family = "roboto_condensed"),
      plot.tag = element_text(size = rel(8)),
      plot.tag.position = "bottom"
    )
  
  # Save
  ggsave(
    filename = paste0("./figures/occ-change-threshs/occ_change_", protection_var, ".png"),
    plot = final_plot,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 300
  )
}


# 2 Models for Nitrogen interaction - Plots: ---------------------------------------------

# m2.1 - mean_occ_change ~ protection_cat * EIVEres.N
# m2.1.1 - mean_occ_change ~ protection_90 * EIVEres.N
# m2.1.2 - mean_occ_change ~ protection_80 * EIVEres.N
# m2.1.3 - mean_occ_change ~ protection_70 * EIVEres.N
# m2.1.4 - mean_occ_change ~ protection_60 * EIVEres.N
# m2.1.5 - mean_occ_change ~ protection_50 * EIVEres.N

# Plot for Nitrogen across species over Germany 
m2.1 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * EIVEres.N +  mean_OP_T1 + (1|TaxonName)) #model like in model script
summary(m2.1)

# Calculate predictions
preds <- ggpredict(m2.1, terms = c("protection_cat", "EIVEres.N"))


# Scale estiamtes to percentage
preds$predicted <- preds$predicted * 100
preds$conf.low <- preds$conf.low * 100
preds$conf.high <- preds$conf.high * 100
preds$group <- factor(preds$group)

# Plot
nplot <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(aes(group = group), 
            position = position_dodge(width = 0.5),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.5),
    width = 0.2,
    linewidth = 0.7) +
  scale_color_manual(values = eiv_n_colors) + 
  labs(
    x = "Protection status",
    y = "Predicted change in\noccurrence probability (%)",
    color = "Nitrogen preference") +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

nplot

ggsave(filename = "./figures/lmer_occ_change_nitrogen.png", 
       plot = nplot,
       width = 30, height = 20, units = "cm", dpi = 120)

# m2.1.1 - m2.1.5 - mean_occ_change ~ protection_90 - 50 * EIVEres.N - Plots: ---------------------------------------------

# List of models and protection variables
model_list_n <- list(
  m2.1.1 = m2.1.1,
  m2.1.2 = m2.1.2,
  m2.1.3 = m2.1.3,
  m2.1.4 = m2.1.4,
  m2.1.5 = m2.1.5
)

protection_vars_n <- c(
  m2.1.1 = "protection90",
  m2.1.2 = "protection80",
  m2.1.3 = "protection70",
  m2.1.4 = "protection60",
  m2.1.5 = "protection50"
)

# For-Loop to iterate over models and create plots
for (model_name in names(model_list_n)) {
  
  m2 <- model_list_n[[model_name]]
  protection_var <- protection_vars_n[[model_name]]
  
  # summary_protXX laden
  summary_prot <- get(paste0("summary_prot", substr(protection_var, 11, 12)))
  stopifnot(protection_var %in% names(summary_prot))
  
  # Vorhersage berechnen
  preds <- ggpredict(m2, terms = c(protection_var, "EIVEres.N"))
  preds$predicted <- preds$predicted * 100
  preds$conf.low <- preds$conf.low * 100
  preds$conf.high <- preds$conf.high * 100
  preds$group <- factor(preds$group)
  print(preds)

  p_N <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
    geom_line(aes(group = group), 
              position = position_dodge(width = 0.5),
              linewidth = 1.2,
              alpha = 0.8) +
   # geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      position = position_dodge(0.5),
      width = 0.2,
      linewidth = 0.7) +
    scale_color_manual(values = eiv_n_colors) + 
    labs(
      x = "Protection status",
      y = "Predicted change in\noccurrence probability (%)",
      color = "Nitrogen preference") +
    theme_classic(base_family = "roboto_condensed") +
    theme(
      text = element_text(family = "roboto_condensed"),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 25),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      axis.text = element_text(size = 25),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )

  print(paste("Saving:", protection_var))
  print(preds)
  print(head(summary_prot))
  print(final_plot)
  # Saving
  ggsave(
    filename = paste0("./figures/Nitrogen_occ_change_", protection_var, ".png"),
    plot = p_N,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 140
  )
}



# 3 Models for Ph Reaction interaction - Plots: ---------------------------------------------

# m2.2 - mean_occ_change ~ protection_cat * EIVEres.R
# m2.2.1 - mean_occ_change ~ protection_90 * EIVEres.R
# m2.2.2 - mean_occ_change ~ protection_80 * EIVEres.R
# m2.2.3 - mean_occ_change ~ protection_70 * EIVEres.R
# m2.2.4 - mean_occ_change ~ protection_60 * EIVEres.R
# m2.2.5 - mean_occ_change ~ protection_50 * EIVEres.R

# Plot for pH across species over Germany 
m2.2 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * EIVEres.R +  mean_OP_T1 + (1|TaxonName)) #model like in model script
summary(m2.2)

# Calculate predictions
preds <- ggpredict(m2.2, terms = c("protection_cat", "EIVEres.R"))


# Scale estiamtes to percentage
preds$predicted <- preds$predicted * 100
preds$conf.low <- preds$conf.low * 100
preds$conf.high <- preds$conf.high * 100
preds$group <- factor(preds$group)

# Plot
rplot <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(aes(group = group), 
            position = position_dodge(width = 0.5),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.5),
    width = 0.2,
    linewidth = 0.7) +
  scale_color_manual(values = eiv_n_colors) + 
  labs(
    x = "Protection status",
    y = "Predicted change in\noccurrence probability (%)",
    color = "pH preference") +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

rplot

ggsave(filename = "./figures/lmer_occ_change_phreaction.png", 
       plot = rplot,
       width = 30, height = 20, units = "cm", dpi = 120)

# m2.2.1 - m2.2.5 - mean_occ_change ~ protection_90 - 50 * EIVEres.R - Plots: ---------------------------------------------

# List of models and protection variables
model_list_r <- list(
  m2.2.1 = m2.2.1,
  m2.2.2 = m2.2.2,
  m2.2.3 = m2.2.3,
  m2.2.4 = m2.2.4,
  m2.2.5 = m2.2.5
)

protection_vars_r <- c(
  m2.2.1 = "protection90",
  m2.2.2 = "protection80",
  m2.2.3 = "protection70",
  m2.2.4 = "protection60",
  m2.2.5 = "protection50"
)

# For-Loop to iterate over models and create plots
for (model_name in names(model_list_r)) {
  
  m3 <- model_list_r[[model_name]]
  protection_var <- protection_vars_r[[model_name]]
  
  # summary_protXX laden
  summary_prot <- get(paste0("summary_prot", substr(protection_var, 11, 12)))
  stopifnot(protection_var %in% names(summary_prot))
  
  # Vorhersage berechnen
  preds <- ggpredict(m3, terms = c(protection_var, "EIVEres.R"))
  preds$predicted <- preds$predicted * 100
  preds$conf.low <- preds$conf.low * 100
  preds$conf.high <- preds$conf.high * 100
  preds$group <- factor(preds$group)
  print(preds)
  
  p_R <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
    geom_line(aes(group = group), 
              position = position_dodge(width = 0.5),
              linewidth = 1.2,
              alpha = 0.8) +
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      position = position_dodge(0.5),
      width = 0.2,
      linewidth = 0.7) +
    scale_color_manual(values = eiv_n_colors) + 
    labs(
      x = "Protection status",
      y = "Predicted change in\noccurrence probability (%)",
      color = "pH preference") +
    theme_classic(base_family = "roboto_condensed") +
    theme(
      text = element_text(family = "roboto_condensed"),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 25),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      axis.text = element_text(size = 25),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
  
  print(paste("Saving:", protection_var))
  print(preds)
  print(head(summary_prot))
  # Saving
  ggsave(
    filename = paste0("./figures/pH_occ_change_", protection_var, ".png"),
    plot = p_R,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 140
  )
}



# 4 Models for Temperature interaction - Plots: ---------------------------------------------

# m2.3 - mean_occ_change ~ protection_cat * EIVEres.T
# m2.3.1 - mean_occ_change ~ protection_90 * EIVEres.T
# m2.3.2 - mean_occ_change ~ protection_80 * EIVEres.T
# m2.3.3 - mean_occ_change ~ protection_70 * EIVEres.T
# m2.3.4 - mean_occ_change ~ protection_60 * EIVEres.T
# m2.3.5 - mean_occ_change ~ protection_50 * EIVEres.T

# Plot for Temperature across species over Germany
m2.3 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * EIVEres.T +  mean_OP_T1 + (1|TaxonName)) #model like in model script
summary(m2.3)

# Calculate predictions
preds <- ggpredict(m2.3, terms = c("protection_cat", "EIVEres.T"))
# Scale estiamtes to percentage
preds$predicted <- preds$predicted * 100
preds$conf.low <- preds$conf.low * 100
preds$conf.high <- preds$conf.high * 100
preds$group <- factor(preds$group)

# Plot
tplot <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(aes(group = group), 
            position = position_dodge(width = 0.5),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.5),
    width = 0.2,
    linewidth = 0.7) +
  scale_color_manual(values = eiv_n_colors) + 
  labs(
    x = "Protection status",
    y = "Predicted change in\noccurrence probability (%)",
    color = "Temperature preference") +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

tplot
ggsave(filename = "./figures/lmer_occ_change_temperature.png", 
       plot = tplot,
       width = 30, height = 20, units = "cm", dpi = 120)

# m2.3.1 - m2.3.5 - mean_occ_change ~ protection_90 - 50 * EIVEres.T - Plots: ---------------------------------------------

# List of models and protection variables
model_list_t <- list(
  m2.3.1 = m2.3.1,
  m2.3.2 = m2.3.2,
  m2.3.3 = m2.3.3,
  m2.3.4 = m2.3.4,
  m2.3.5 = m2.3.5
)

protection_vars_t <- c(
  m2.3.1 = "protection90",
  m2.3.2 = "protection80",
  m2.3.3 = "protection70",
  m2.3.4 = "protection60",
  m2.3.5 = "protection50"
)

# For-Loop to iterate over models and create plots
for (model_name in names(model_list_t)) {
  
  m4 <- model_list_t[[model_name]]
  protection_var <- protection_vars_t[[model_name]]
  
  # summary_protXX laden
  summary_prot <- get(paste0("summary_prot", substr(protection_var, 11, 12)))
  stopifnot(protection_var %in% names(summary_prot))
  
  # Vorhersage berechnen
  preds <- ggpredict(m4, terms = c(protection_var, "EIVEres.T"))
  preds$predicted <- preds$predicted * 100
  preds$conf.low <- preds$conf.low * 100
  preds$conf.high <- preds$conf.high * 100
  preds$group <- factor(preds$group)
  print(preds)
  
  p_T <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
    geom_line(aes(group = group), 
              position = position_dodge(width = 0.5),
              linewidth = 1.2,
              alpha = 0.8) +
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      position = position_dodge(0.5),
      width = 0.2,
      linewidth = 0.7) +
    scale_color_manual(values = eiv_n_colors) + 
    labs(
      x = "Protection status",
      y = "Predicted change in\noccurrence probability (%)",
      color = "Temperature preference") +
    theme_classic(base_family = "roboto_condensed") +
    theme(
      text = element_text(family = "roboto_condensed"),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 25),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      axis.text = element_text(size = 25),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
  
  print(paste("Saving:", protection_var))
  print(preds)
  print(head(summary_prot))
  # Saving
  ggsave(
    filename = paste0("./figures/Temperature_occ_change_", protection_var, ".png"),
    plot = p_T,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 140
  )
}

# 5 Models for Moisture interaction - Plots: ---------------------------------------------

# m2.4 - mean_occ_change ~ protection_cat * EIVEres.M
# m2.4.1 - mean_occ_change ~ protection_90 * EIVEres.M
# m2.4.2 - mean_occ_change ~ protection_80 * EIVEres.M
# m2.4.3 - mean_occ_change ~ protection_70 * EIVEres.M
# m2.4.4 - mean_occ_change ~ protection_60 * EIVEres.M
# m2.4.5 - mean_occ_change ~ protection_50 * EIVEres.M

# Plot for Moisture across species over Germany
m2.4 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * EIVEres.M +  mean_OP_T1 + (1|TaxonName)) #model like in model script
summary(m2.4)
# Calculate predictions
preds <- ggpredict(m2.4, terms = c("protection_cat", "EIVEres.M"))
# Scale estiamtes to percentage
preds$predicted <- preds$predicted * 100
preds$conf.low <- preds$conf.low * 100
preds$conf.high <- preds$conf.high * 100
preds$group <- factor(preds$group)


# Plot
mplot <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(aes(group = group), 
            position = position_dodge(width = 0.5),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.5),
    width = 0.2,
    linewidth = 0.7) +
  scale_color_manual(values = eiv_n_colors) + 
  labs(
    x = "Protection status",
    y = "Predicted change in\noccurrence probability (%)",
    color = "Moisture preference") +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

mplot

ggsave(filename = "./figures/lmer_occ_change_moisture.png", 
       plot = mplot,
       width = 30, height = 20, units = "cm", dpi = 120)

# m2.4.1 - m2.4.5 - mean_occ_change ~ protection_90 - 50 * EIVEres.M - Plots: ---------------------------------------------

# List of models and protection variables
model_list_m <- list(
  m2.4.1 = m2.4.1,
  m2.4.2 = m2.4.2,
  m2.4.3 = m2.4.3,
  m2.4.4 = m2.4.4,
  m2.4.5 = m2.4.5
)

protection_vars_m <- c(
  m2.4.1 = "protection90",
  m2.4.2 = "protection80",
  m2.4.3 = "protection70",
  m2.4.4 = "protection60",
  m2.4.5 = "protection50"
)

# For-Loop to iterate over models and create plots
for (model_name in names(model_list_m)) {
  
  m5 <- model_list_m[[model_name]]
  protection_var <- protection_vars_m[[model_name]]
  
  # summary_protXX laden
  summary_prot <- get(paste0("summary_prot", substr(protection_var, 11, 12)))
  stopifnot(protection_var %in% names(summary_prot))
  
  # Vorhersage berechnen
  preds <- ggpredict(m5, terms = c(protection_var, "EIVEres.M"))
  preds$predicted <- preds$predicted * 100
  preds$conf.low <- preds$conf.low * 100
  preds$conf.high <- preds$conf.high * 100
  preds$group <- factor(preds$group)
  print(preds)
  
  p_M <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
    geom_line(aes(group = group), 
              position = position_dodge(width = 0.5),
              linewidth = 1.2,
              alpha = 0.8) +
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      position = position_dodge(0.5),
      width = 0.2,
      linewidth = 0.7) +
    scale_color_manual(values = eiv_n_colors) + 
    labs(
      x = "Protection status",
      y = "Predicted change in\noccurrence probability (%)",
      color = "Moisture preference") +
    theme_classic(base_family = "roboto_condensed") +
    theme(
      text = element_text(family = "roboto_condensed"),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 25),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      axis.text = element_text(size = 25),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
  
  print(paste("Saving:", protection_var))
  print(preds)
  print(head(summary_prot))
  
  # Saving
  ggsave(
    filename = paste0("./figures/Moisture_occ_change_", protection_var, ".png"),
    plot = p_M,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 140
  )
}

# 6 Models for Light interaction - Plots: ---------------------------------------------

# m2.5 - mean_occ_change ~ protection_cat * EIVEres.L
# m2.5.1 - mean_occ_change ~ protection_90 * EIVEres.L
# m2.5.2 - mean_occ_change ~ protection_80 * EIVEres.L
# m2.5.3 - mean_occ_change ~ protection_70 * EIVEres.L
# m2.5.4 - mean_occ_change ~ protection_60 * EIVEres.L
# m2.5.5 - mean_occ_change ~ protection_50 * EIVEres.L

# Plot for Light across species over Germany
m2.5 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * EIVEres.L +  mean_OP_T1 + (1|TaxonName)) #model like in model script
summary(m2.5)
# Calculate predictions
preds <- ggpredict(m2.5, terms = c("protection_cat", "EIVEres.L"))
# Scale estiamtes to percentage
preds$predicted <- preds$predicted * 100
preds$conf.low <- preds$conf.low * 100
preds$conf.high <- preds$conf.high * 100
preds$group <- factor(preds$group)
# Plot
lplot <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(aes(group = group), 
            position = position_dodge(width = 0.5),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.5),
    width = 0.2,
    linewidth = 0.7) +
  scale_color_manual(values = eiv_n_colors) + 
  labs(
    x = "Protection status",
    y = "Predicted change in\noccurrence probability (%)",
    color = "Light preference") +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

lplot

ggsave(filename = "./figures/lmer_occ_change_light.png", 
       plot = lplot,
       width = 30, height = 20, units = "cm", dpi = 120)

# m2.5.1 - m2.5.5 - mean_occ_change ~ protection_90 - 50 * EIVEres.L - Plots: ---------------------------------------------

# List of models and protection variables
model_list_l <- list(
  m2.5.1 = m2.5.1,
  m2.5.2 = m2.5.2,
  m2.5.3 = m2.5.3,
  m2.5.4 = m2.5.4,
  m2.5.5 = m2.5.5
)

protection_vars_l <- c(
  m2.5.1 = "protection90",
  m2.5.2 = "protection80",
  m2.5.3 = "protection70",
  m2.5.4 = "protection60",
  m2.5.5 = "protection50"
)

# For-Loop to iterate over models and create plots
for (model_name in names(model_list_l)) {
  
  m6 <- model_list_l[[model_name]]
  protection_var <- protection_vars_l[[model_name]]
  
  # summary_protXX laden
  summary_prot <- get(paste0("summary_prot", substr(protection_var, 11, 12)))
  stopifnot(protection_var %in% names(summary_prot))
  
  # Vorhersage berechnen
  preds <- ggpredict(m6, terms = c(protection_var, "EIVEres.L"))
  preds$predicted <- preds$predicted * 100
  preds$conf.low <- preds$conf.low * 100
  preds$conf.high <- preds$conf.high * 100
  preds$group <- factor(preds$group)

  p_L <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
    geom_line(aes(group = group), 
              position = position_dodge(width = 0.5),
              linewidth = 1.2,
              alpha = 0.8) +
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      position = position_dodge(0.5),
      width = 0.2,
      linewidth = 0.7) +
    scale_color_manual(values = eiv_n_colors) + 
    labs(
      x = "Protection status",
      y = "Predicted change in\noccurrence probability (%)",
      color = "Light preference") +
    theme_classic(base_family = "roboto_condensed") +
    theme(
      text = element_text(family = "roboto_condensed"),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 25),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      axis.text = element_text(size = 25),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
  
  print(paste("Saving:", protection_var))
  print(preds)
  print(head(summary_prot))
  # Saving
  
  ggsave(
    filename = paste0("./figures/Light_occ_change_", protection_var, ".png"),
    plot = p_L,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 140
  )
}

# 7 Models for Main Group interaction - Plots: ---------------------------------------------

# m2.6 - mean_occ_change ~ protection_cat * main_group
# m2.6.1 - mean_occ_change ~ protection_90 * main_group
# m2.6.2 - mean_occ_change ~ protection_80 * main_group
# m2.6.3 - mean_occ_change ~ protection_70 * main_group
# m2.6.4 - mean_occ_change ~ protection_60 * main_group
# m2.6.5 - mean_occ_change ~ protection_50 * main_group

# Plot for Main Group across species over Germany
m2.6 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * main_group +  mean_OP_T1 + (1|TaxonName)) #model like in model script
summary(m2.6)
# Calculate predictions
preds <- ggpredict(m2.6, terms = c("protection_cat", "main_group"))

# Scale estiamtes to percentage
preds$predicted <- preds$predicted * 100
preds$conf.low <- preds$conf.low * 100
preds$conf.high <- preds$conf.high * 100
preds$group <- factor(preds$group)

# Plot
forest_plot <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(aes(group = group), 
            position = position_dodge(width = 0.5),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.5),
    width = 0.2,
    linewidth = 0.7) +
  scale_color_manual(values = main_group_colors) + 
  labs(
    x = "Protection status",
    y = "Predicted change in\noccurrence probability (%)",
    color = "Forest affiliation") +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

forest_plot
ggsave(filename = "./figures/lmer_occ_change_forest.png", 
       plot = forest_plot,
       width = 30, height = 20, units = "cm", dpi = 120)

# m2.6.1 - m2.6.5 - mean_occ_change ~ protection_90 - 50 * main_group - Plots: ---------------------------------------------

# List of models and protection variables
model_list_forest <- list(
  m2.6.1 = m2.6.1,
  m2.6.2 = m2.6.2,
  m2.6.3 = m2.6.3,
  m2.6.4 = m2.6.4,
  m2.6.5 = m2.6.5
)

protection_vars_forest <- c(
  m2.6.1 = "protection90",
  m2.6.2 = "protection80",
  m2.6.3 = "protection70",
  m2.6.4 = "protection60",
  m2.6.5 = "protection50"
)

# For-Loop to iterate over models and create plots

for (model_name in names(model_list_forest)) {
  
  m7 <- model_list_forest[[model_name]]
  protection_var <- protection_vars_forest[[model_name]]
  
  # summary_protXX laden
  summary_prot <- get(paste0("summary_prot", substr(protection_var, 11, 12)))
  stopifnot(protection_var %in% names(summary_prot))
  
  # Vorhersage berechnen
  preds <- ggpredict(m7, terms = c(protection_var, "main_group"))
  preds$predicted <- preds$predicted * 100
  preds$conf.low <- preds$conf.low * 100
  preds$conf.high <- preds$conf.high * 100
  preds$group <- factor(preds$group)
  
  p_F <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
    geom_line(aes(group = group), 
              position = position_dodge(width = 0.5),
              linewidth = 1.2,
              alpha = 0.8) +
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      position = position_dodge(0.5),
      width = 0.2,
      linewidth = 0.7) +
    scale_color_manual(values = main_group_colors) + 
    labs(
      x = "Protection status",
      y = "Predicted change in\noccurrence probability (%)",
      color = "Forest affiliation") +
    theme_classic(base_family = "roboto_condensed") +
    theme(
      text = element_text(family = "roboto_condensed"),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 25),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      axis.text = element_text(size = 25),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
  
  print(paste("Saving:", protection_var))
  print(preds)
  print(head(summary_prot))
  
  # Saving
   ggsave(
    filename = paste0("./figures/Forest_occ_change_", protection_var, ".png"),
    plot = p_F,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 140
  )
}

# 8 Models for Plant Growth Form interaction - Plots: ---------------------------------------------

# m2.7 - mean_occ_change ~ protection_cat * PlantGrowthForm
# m2.7.1 - mean_occ_change ~ protection_90 * PlantGrowthForm
# m2.7.2 - mean_occ_change ~ protection_80 * PlantGrowthForm
# m2.7.3 - mean_occ_change ~ protection_70 * PlantGrowthForm
# m2.7.4 - mean_occ_change ~ protection_60 * PlantGrowthForm
# m2.7.5 - mean_occ_change ~ protection_50 * PlantGrowthForm

# Plot for Plant Growth Form across species over Germany
m2.7 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * PlantGrowthForm +  mean_OP_T1 + (1|TaxonName)) #model like in model script
summary(m2.7)
# Calculate predictions
preds <- ggpredict(m2.7, terms = c("protection_cat", "PlantGrowthForm"))
# Scale estiamtes to percentage
preds$predicted <- preds$predicted * 100
preds$conf.low <- preds$conf.low * 100
preds$conf.high <- preds$conf.high * 100
preds$group <- factor(preds$group)
# Plot
pgf_plot <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(aes(group = group), 
            position = position_dodge(width = 0.5),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.5),
    width = 0.2,
    linewidth = 0.7) +
  scale_color_manual(values = pgf_colors) + 
  labs(
    x = "Protection status",
    y = "Predicted change in\noccurrence probability (%)",
    color = "Plant Growth Form") +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

pgf_plot

ggsave(filename = "./figures/lmer_occ_change_pgf.png", 
       plot = pgf_plot,
       width = 30, height = 20, units = "cm", dpi = 120)

# m2.7.1 - m2.7.5 - mean_occ_change ~ protection_90 - 50 * PlantGrowthForm - Plots: ---------------------------------------------

# List of models and protection variables
model_list_pgf <- list(
  m2.7.1 = m2.7.1,
  m2.7.2 = m2.7.2,
  m2.7.3 = m2.7.3,
  m2.7.4 = m2.7.4,
  m2.7.5 = m2.7.5
)

protection_vars_pgf <- c(
  m2.7.1 = "protection90",
  m2.7.2 = "protection80",
  m2.7.3 = "protection70",
  m2.7.4 = "protection60",
  m2.7.5 = "protection50"
)

# For-Loop to iterate over models and create plots

for (model_name in names(model_list_pgf)) {
  
  m8 <- model_list_pgf[[model_name]]
  protection_var <- protection_vars_pgf[[model_name]]
  
  # summary_protXX laden
  summary_prot <- get(paste0("summary_prot", substr(protection_var, 11, 12)))
  stopifnot(protection_var %in% names(summary_prot))
  
  # Vorhersage berechnen
  preds <- ggpredict(m8, terms = c(protection_var, "PlantGrowthForm"))
  preds$predicted <- preds$predicted * 100
  preds$conf.low <- preds$conf.low * 100
  preds$conf.high <- preds$conf.high * 100
  preds$group <- factor(preds$group)
  
  p_PGF <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
    geom_line(aes(group = group), 
              position = position_dodge(width = 0.5),
              linewidth = 1.2,
              alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      position = position_dodge(0.5),
      width = 0.2,
      linewidth = 0.7) +
    scale_color_manual(values = pgf_colors) + 
    labs(
      x = "Protection status",
      y = "Predicted change in\noccurrence probability (%)",
      color = "Plant Growth Form") +
    theme_classic(base_family = "roboto_condensed") +
    theme(
      text = element_text(family = "roboto_condensed"),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 25),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      axis.text = element_text(size = 25),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background =
        element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
  print(paste("Saving:", protection_var))
  print(preds)
  print(head(summary_prot))
  
  # Saving
  ggsave(
    filename = paste0("./figures/PGF_occ_change_", protection_var, ".png"),
    plot = p_PGF,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 140
  )
}

# 9 Models for Woodiness interaction - Plots: ---------------------------------------------

# m2.8 - mean_occ_change ~ protection_cat * Woodiness
# m2.8.1 - mean_occ_change ~ protection_90 * Woodiness
# m2.8.2 - mean_occ_change ~ protection_80 * Woodiness
# m2.8.3 - mean_occ_change ~ protection_70 * Woodiness
# m2.8.4 - mean_occ_change ~ protection_60 * Woodiness
# m2.8.5 - mean_occ_change ~ protection_50 * Woodiness

# Plot for Woodiness across species over Germany
m2.8 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * Woodiness +  mean_OP_T1 + (1|TaxonName)) #model like in model script
summary(m2.8)
# Calculate predictions
preds <- ggpredict(m2.8, terms = c("protection_cat", "Woodiness"))
# Scale estiamtes to percentage
preds$predicted <- preds$predicted * 100
preds$conf.low <- preds$conf.low * 100
preds$conf.high <- preds$conf.high * 100
preds$group <- factor(preds$group)

# Plot

woodiness_plot <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(aes(group = group), 
            position = position_dodge(width = 0.5),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.5),
    width = 0.2,
    linewidth = 0.7) +
  scale_color_manual(values = woodiness_colors) + 
  labs(
    x = "Protection status",
    y = "Predicted change in\noccurrence probability (%)",
    color = "Woodiness") +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background =
      element_rect(fill = "transparent", color = NA),
    legend.box.background =
      element_rect(fill = "transparent", color = NA)
  )

woodiness_plot

ggsave(filename = "./figures/lmer_occ_change_woodiness.png", 
       plot = woodiness_plot,
       width = 30, height = 20, units = "cm", dpi = 120)

# m2.8.1 - m2.8.5 - mean_occ_change ~ protection_90 - 50 * Woodiness - Plots: ---------------------------------------------

# List of models and protection variables
model_list_woodiness <- list(
  m2.8.1 = m2.8.1,
  m2.8.2 = m2.8.2,
  m2.8.3 = m2.8.3,
  m2.8.4 = m2.8.4,
  m2.8.5 = m2.8.5
)

protection_vars_woodiness <- c(
  m2.8.1 = "protection90",
  m2.8.2 = "protection80",
  m2.8.3 = "protection70",
  m2.8.4 = "protection60",
  m2.8.5 = "protection50"
)

# For-Loop to iterate over models and create plots

for (model_name in names(model_list_woodiness)) {
  
  m9 <- model_list_woodiness[[model_name]]
  protection_var <- protection_vars_woodiness[[model_name]]
  
  # summary_protXX laden
  summary_prot <- get(paste0("summary_prot", substr(protection_var, 11, 12)))
  stopifnot(protection_var %in% names(summary_prot))
  
  # Vorhersage berechnen
  preds <- ggpredict(m9, terms = c(protection_var, "Woodiness"))
  preds$predicted <- preds$predicted * 100
  preds$conf.low <- preds$conf.low * 100
  preds$conf.high <- preds$conf.high * 100
  preds$group <- factor(preds$group)
  
  p_W <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
    geom_line(aes(group = group), 
              position = position_dodge(width = 0.5),
              linewidth = 1.2,
              alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      position = position_dodge(0.5),
      width = 0.2,
      linewidth = 0.7) +
    scale_color_manual(values = woodiness_colors) + 
    labs(
      x = "Protection status",
      y = "Predicted change in\noccurrence probability (%)",
      color = "Woodiness") +
    theme_classic(base_family = "roboto_condensed") +
    theme(
      text = element_text(family = "roboto_condensed"),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 25),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      axis.text = element_text(size = 25),
      panel.background =
        element_rect(fill = "transparent", color = NA),
      plot.background =
        element_rect(fill = "transparent", color = NA),
      legend.background =
        element_rect(fill = "transparent", color = NA),
      legend.box.background =
        element_rect(fill = "transparent", color = NA)
    )
  print(paste("Saving:", protection_var))
  print(preds)
  print(head(summary_prot))
  # Saving
  ggsave(
    filename = paste0("./figures/Woodiness_occ_change_", protection_var, ".png"),
    plot = p_W,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 140
  )
  
}




# 10 Models for Leaf Type interaction - Plots: ---------------------------------------------

# m2.9 - mean_occ_change ~ protection_cat * LeafType
# m2.9.1 - mean_occ_change ~ protection_90 * LeafType
# m2.9.2 - mean_occ_change ~ protection_80 * LeafType
# m2.9.3 - mean_occ_change ~ protection_70 * LeafType
# m2.9.4 - mean_occ_change ~ protection_60 * LeafType
# m2.9.5 - mean_occ_change ~ protection_50 * LeafType

# Plot for Leaf Type across species over Germany
m2.9 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * LeafType +  mean_OP_T1 + (1|TaxonName)) #model like in model script
summary(m2.9)
# Calculate predictions
preds <- ggpredict(m2.9, terms = c("protection_cat", "LeafType"))
# Scale estiamtes to percentage
preds$predicted <- preds$predicted * 100
preds$conf.low <- preds$conf.low * 100
preds$conf.high <- preds$conf.high * 100
preds$group <- factor(preds$group)
# Plot
leaf_type_plot <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(aes(group = group), 
            position = position_dodge(width = 0.5),
            linewidth = 1.2,
            alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.5),
    width = 0.2,
    linewidth = 0.7) +
  scale_color_manual(values = leaf_type_colors) + 
  labs(
    x = "Protection status",
    y = "Predicted change in\noccurrence probability (%)",
    color = "Leaf Type") +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
    panel.background =
      element_rect(fill = "transparent", color = NA),
    plot.background =
      element_rect(fill = "transparent", color = NA),
    legend.background =
      element_rect(fill = "transparent", color = NA),
    legend.box.background =
      element_rect(fill = "transparent", color = NA)
  )

leaf_type_plot
ggsave(filename = "./figures/lmer_occ_change_leaf_type.png", 
       plot = leaf_type_plot,
       width = 30, height = 20, units = "cm", dpi = 120)

# m2.9.1 - m2.9.5 - mean_occ_change ~ protection_90 - 50 * LeafType - Plots: ---------------------------------------------
# List of models and protection variables
model_list_leaf_type <- list(
  m2.9.1 = m2.9.1,
  m2.9.2 = m2.9.2,
  m2.9.3 = m2.9.3,
  m2.9.4 = m2.9.4,
  m2.9.5 = m2.9.5
)

protection_vars_leaf_type <- c(
  m2.9.1 = "protection90",
  m2.9.2 = "protection80",
  m2.9.3 = "protection70",
  m2.9.4 = "protection60",
  m2.9.5 = "protection50"
)

# For-Loop to iterate over models and create plots

for (model_name in names(model_list_leaf_type)) {
  
  m10 <- model_list_leaf_type[[model_name]]
  protection_var <- protection_vars_leaf_type[[model_name]]
  
  # summary_protXX laden
  summary_prot <- get(paste0("summary_prot", substr(protection_var, 11, 12)))
  stopifnot(protection_var %in% names(summary_prot))
  
  # Vorhersage berechnen
  preds <- ggpredict(m10, terms = c(protection_var, "LeafType"))
  preds$predicted <- preds$predicted * 100
  preds$conf.low <- preds$conf.low * 100
  preds$conf.high <- preds$conf.high * 100
  preds$group <- factor(preds$group)
  
  p_LT <- ggplot(preds, aes(x = x, y = predicted, color = group)) +
    geom_line(aes(group = group), 
              position = position_dodge(width = 0.5),
              linewidth = 1.2,
              alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) + 
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      position = position_dodge(0.5),
      width = 0.2,
      linewidth = 0.7) +
    scale_color_manual(values = leaf_type_colors) + 
    labs(
      x = "Protection status",
      y = "Predicted change in\noccurrence probability (%)",
      color = "Leaf Type") +
    theme_classic(base_family = "roboto_condensed") +
    theme(
      text = element_text(family = "roboto_condensed"),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 25),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      axis.text = element_text(size = 25),
      panel.background =
        element_rect(fill = "transparent", color = NA),
      plot.background =
        element_rect(fill = "transparent", color = NA),
      legend.background =
        element_rect(fill = "transparent", color = NA),
      legend.box.background =
        element_rect(fill = "transparent", color = NA)
    )
  print(paste("Saving:", protection_var))
  print(preds)
  print(head(summary_prot))
  
  # Saving
  ggsave(
    filename = paste0("./figures/LeafType_occ_change_", protection_var, ".png"),
    plot = p_LT,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 140
  )
  
}

