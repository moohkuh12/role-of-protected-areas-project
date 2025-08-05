# -----------------------------------------------
# SETUP & DATA LOADING
# -----------------------------------------------
# color palettes
colors1 <- c("#7B786B", "#A6A66A","#568C8C")
colors2 <- c( "#576D5D", "#B0A25E", "#D2825C")
colors3 <- c("#60728E", "#B7A553","#B55A69")
colors4 <- c( "#5D7E81", "#A8994D", "#C15C39")






# Load setup and libraries
source("./r_scripts/00-preamble.R")


# Load dataframe
#sMon <- read_csv("./data/sMon/sMon_long.csv", col_names = TRUE)
sMon_wide <- read_csv("./data/sMon/sMon_wide_100625.csv")
smon_filtered <- sMon_wide %>%
  filter(urban_class != "urban", OP_T1 > 0)
rm(sMon_wide)

# Filter for species with high occurrence probability at T1
#smon_filtered_high_occ <- smon_filtered %>%
#filter(OP_T1 >= 0.8)

# 1.3. Linear model ------------------------------
smon_filtered$region <- with(smon_filtered, ifelse(Latitude >= 51 & Longitude >= 10.5, "NE",
                                                   ifelse(Latitude >= 51 & Longitude < 10.5, "NW",
                                                          ifelse(Latitude < 51 & Longitude >= 10.5, "SE", "SW"))))





# Population trend analysis
poptrend_summary <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  mutate(
    occ_change = OP_T3 - OP_T1,
    
    # Alternative protection calssification
    alt_protection = if_else(cov_frac >= 0.7, "protected", "not")
  ) %>%
  group_by(TaxonName) %>%
  summarise(
    SOP_T1 = sum(OP_T1, na.rm = TRUE),
    SOP_T3 = sum(OP_T3, na.rm = TRUE),
    delta_SOP = SOP_T3 - SOP_T1,
    logratio = log((SOP_T3 + 0.01)/(SOP_T1 + 0.01)),
    log_SOP_T1 = log(SOP_T1 + 0.01),
    n_cells_T1 = n(),  # number of occupied cells at T1
    n_protected_T1 = sum(protection_cat == "protected", na.rm = TRUE),  # original
    percent_range_protected = n_protected_T1 / n_cells_T1 * 100,        # original
    n_protected_T1_70 = sum(alt_protection == "protected", na.rm = TRUE),  # alternative
    percent_range_protected70 = n_protected_T1_70 / n_cells_T1 * 100       # alternative
  ) %>%
  ungroup()

# 
# poptrend_summary_reg <- smon_filtered %>%
#   filter(OP_T1 > 0) %>%
#   mutate(occ_change = OP_T3 - OP_T1) %>%
#   group_by(TaxonName, region) %>%
#   summarise(
#     SOP_T1 = sum(OP_T1, na.rm = TRUE),
#     SOP_T3 = sum(OP_T3, na.rm = TRUE),
#     delta_SOP = SOP_T3 - SOP_T1,
#     logratio = log((SOP_T3 + 0.01)/(SOP_T1 + 0.01)),
#     log_SOP_T1 = log(SOP_T1 + 0.01),
#     n_cells_T1 = n(),  # durch filter(OP_T1 > 0) = Zellen mit Vorkommen in T1
#     n_protected_T1 = sum(protection_cat == "protected", na.rm = TRUE),
#     percent_range_protected = n_protected_T1 / n_cells_T1 * 100
#   ) %>%
#   ungroup()


# lmer for populationtrand analysis

poptrend_lm1 <- lm(logratio ~ percent_range_protected + log_SOP_T1 , data = poptrend_summary)
# poptrend_lm3 <- lm(logratio ~ percent_range_protected + log_SOP_T1 + log(n_cells_T1) , data = poptrend_summary)
 summary(poptrend_lm1)
# summary(poptrend_lm2)
# summary(poptrend_lm3)
# AIC(poptrend_lm1, poptrend_lm3)

# Plot the model
plot(poptrend_lm1)

# Plot for population trend analysis ---------------

# Generate estimates
pred <- ggpredict(poptrend_lm1, terms = c("percent_range_protected"))


model_data <- model.frame(poptrend_lm1)


poptrend <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(
    data = model_data,
    aes(x = percent_range_protected, y = logratio, color= group),
    color = "#0077AA",
    alpha = 0.1,
    size = 2,
    inherit.aes = FALSE
  )+
  geom_line(aes(color = group), linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, color = group), alpha = 0.2, color = NA) +
  scale_color_manual(
    values = "#0077AA", guide="none"
    ) +
  labs(
    x = bquote("% of range protected (t "  [1]* ")"),
    y = bquote("Population Trend " ~ log[SOP(t[3]/t[1])])
  )+
  theme_classic(base_family = "roboto_condensed") +
  theme(
    strip.text = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 24),
    axis.title = element_text(size = 32),
    axis.text = element_text(size = 24)
  )
poptrend 


ggsave(
  filename = "./figures/poptrend_range.png",
  plot = poptrend,
  width = 30, height = 17, dpi = 140, units= "cm"
)












poptrend_summary_treg$main_group <- relevel(poptrend_summary_treg$main_group, ref = "generalist")
poptrend_summary_treg$main_group2 <- relevel(poptrend_summary_treg$main_group2, ref = "generalist")

poptrend_lm4 <- lm(logratio ~ percent_range_protected*main_group + log_SOP_T1 + log(n_cells_T1) , data = poptrend_summary_t)
summary(poptrend_lm4)
plot(poptrend_lm4)
poptrend_lm5 <- lm(logratio ~ percent_range_protected*main_group2*region + log_SOP_T1 + log(n_cells_T1) , data = poptrend_summary_treg)
summary(poptrend_lm5)
plot(ggpredict(poptrend_lm5, terms = c("percent_range_protected", "main_group2", "region")))
plot(ggpredict(poptrend_lm5, terms = c("percent_range_protected","region","main_group")))
plot(ggpredict(poptrend_lm5, terms = c("region","percent_range_protected","main_group")))
plot(ggpredict(poptrend_lm5, terms = c( "main_group2","percent_range_protected")))
plot(ggpredict(poptrend_lm4, terms = c( "main_group","percent_range_protected")))
plot(ggpredict(poptrend_lm1, terms = c("percent_range_protected")))

# pop trens for traits -------------
poptrend_lm6 <- lm(logratio ~ percent_range_protected*EIVEres.N + log_SOP_T1 + log(n_cells_T1) , data = poptrend_summary_t)
summary(poptrend_lm6)
plot(poptrend_lm6)
plot(ggpredict(poptrend_lm6, terms = c( "percent_range_protected", "EIVEres.N")))

summary(poptrend_lm6)
plot(poptrend_lm6)
plot(ggpredict(poptrend_lm6, terms = c( "percent_range_protected", "EIVEres.N")))


poptrend_lm7 <- lm(logratio ~ percent_range_protected*EIVEres.L + log_SOP_T1 + log(n_cells_T1) , data = poptrend_summary_t)
summary(poptrend_lm7)
plot(poptrend_lm7)
plot(ggpredict(poptrend_lm7, terms = c( "percent_range_protected", "EIVEres.L")))

poptrend_lm8 <- lm(logratio ~ 
                     percent_range_protected * EIVEres.L + 
                     percent_range_protected * EIVEres.L.nw3 +
                     log_SOP_T1 + log(n_cells_T1),
                   data = poptrend_summary_t)
summary(poptrend_lm8)
poptr9 <- lm(logratio ~ percent_range_protected * EIVEres.L + 
     EIVEres.L.nw3 +  # einfach als zusätzlicher Prädiktor
     log_SOP_T1 + log(n_cells_T1),
   data = poptrend_summary_t)

summary(poptr9)

poptr11 <- lm(logratio ~ percent_range_protected * EIVEres.T + 
                percent_range_protected * EIVEres.T.nw3 +
               log_SOP_T1 + log(n_cells_T1),
             data = poptrend_summary_t)
summary(poptr11)


poptr10 <- lm(logratio ~ percent_range_protected * EIVEres.T + 
                EIVEres.T.nw3 +  # einfach als zusätzlicher Prädiktor
                log_SOP_T1 + log(n_cells_T1),
              data = poptrend_summary_t)
summary(poptr10)
plot(ggpredict(poptr10, terms = c( "percent_range_protected", "EIVEres.T")))
poptrend_summary_treg$region <- as.factor(poptrend_summary_treg$region)

poptr_t_regional <- lm(logratio ~ percent_range_protected * EIVEres.T * region +
                         #EIVEres.T.nw3 +
                         log_SOP_T1 + log(n_cells_T1),
                       data = poptrend_summary_treg)

summary(poptr_t_regional)
# Prediction für percent_range_protected × EIVEres.T je Region
pred <- ggpredict(poptr_t_regional, terms = c("percent_range_protected","EIVEres.T", "region"))

# Plotten
plot(pred) +
  labs(
    title = "Interaction: Protection × Temperature Zeigerwert by Region",
    x = "% of Range Protected",
    y = "Predicted Population Logratio"
  ) +
  theme_minimal()



ggplot(poptrend_summary_treg, aes(x = EIVEres.T, fill = region)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution of Temperature Indicator Values by Region",
       x = "EIVEres.T (Temperature Indicator)",
       y = "Density") +
  theme_minimal()


# Plot for main group
poptrend_lm4 <- lm(logratio ~ percent_range_protected*main_group + log_SOP_T1 + log(n_cells_T1) , data = poptrend_summary_t)
summary(poptrend_lm4)
plot(poptrend_lm4)


poptrend_lm4_reg <- lm(logratio ~ percent_range_protected*main_group*region + log_SOP_T1 + log(n_cells_T1) , data = poptrend_summary_treg)
summary(poptrend_lm4_reg)
car::Anova(poptrend_lm4_reg, type = 3)
car::Anova(poptrend_lm4_reg, type = 2)
plot(poptrend_lm4_reg)
plot(ggpredict(poptrend_lm4_reg, terms = c("percent_range_protected", "main_group", "region")))
plot(allEffects(poptrend_lm4_reg))
emm <- emmeans(poptrend_lm4_reg, ~ main_group * region)
pairs(emm, adjust = "tukey")


# check if groups are unbalanced or not
table(poptrend_summary_treg$main_group, poptrend_summary_treg$region)
table(poptrend_summary_treg$main_group, poptrend_summary_treg$region, poptrend_summary_treg$protection_cat)

library(ggplot2)

ggplot(poptrend_summary_treg, aes(x = percent_range_protected)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  facet_grid(main_group ~ region) +
  labs(
    x = "% of range protected",
    y = "Density",
    title = "Distribution of % Range Protected across Groups"
  ) +
  theme_minimal(base_family = "roboto_condensed") +
  theme(
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  )


# Plot
plot(pred) +
  labs(
    title = "Effect of Protection per Region and Group",
    x = "% of Range Protected",
    y = "Predicted Population Trend (logratio)"
  ) +
  theme_minimal()


# Vorhersagen generieren
pred <- ggpredict(poptrend_lm4_reg, terms = c("percent_range_protected", 
                                              "main_group", 
                                              "region"))


model_data <- model.frame(poptrend_lm4_reg)

# Wenn du Facets nach region willst (wie in pred$facet):
model_data$facet <- model_data$region

forestreg <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(
    data = model_data,
    aes(x = percent_range_protected, y = logratio, color = main_group),
    alpha = 0.4,
   size = 2,
    inherit.aes = FALSE
  ) +
  geom_line(aes(color = group), linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  facet_wrap(~ facet) +
  scale_color_manual(
    values = c(
      "forest specialist" = "#6F4742",   # erdiges Dunkelrotbraun
      "generalist"        = "#8A837C",   # neutrales Steingrau
      "open specialist"   = "#C4B45F"    # warmes, gedecktes Gelbgrün
    )
  ) +
  scale_fill_manual(
    values = c(
      "forest specialist" = "#6F4742",
      "generalist"        = "#8A837C",
      "open specialist"   = "#C4B45F"
    )
  ) +
  labs(
    x = expression("% of range protected (t"[1]*")"),
    y = expression("Population Trend (log(SOP (t"[3]*"/t"[1]*"))"),
    color = "Habitat type",
    fill = "Habitat type"
  ) +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    strip.text = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 24),
    axis.title = element_text(size = 32),
    axis.text = element_text(size = 24)
  )
ggsave(
  filename = "./figures/poptrend_range_model_reg.png",
  plot = forestreg,
  width = 30, height = 17, dpi = 140, units= "cm"
)

range(poptrend_summary$percent_range_protected, na.rm = TRUE)
range(model_data$percent_range_protected, na.rm = TRUE)



poptrend_summary_treg %>%
  count(region, main_group) %>%
  arrange(desc(n))

# poptrend by region and main group2 ------------------------------

poptrend_summary_treg$main_group2 <- factor(poptrend_summary_treg$main_group2)
poptrend_summary_treg$region <- factor(poptrend_summary_treg$region)
poptrend_lm5 <- lm(logratio ~ percent_range_protected*main_group2*region + log_SOP_T1 + log(n_cells_T1) , data = poptrend_summary_treg)
summary(poptrend_lm5)
plot(ggpredict(poptrend_lm5, terms = c("percent_range_protected", "main_group2", "region")))




# Begrenze X-Achse auf den beobachteten Bereich, z. B. 0–30 % in 5%-Schritten
pred <- ggpredict(poptrend_lm5, terms = c("percent_range_protected [0:40 by=5]", 
                                              "main_group2", 
                                              "region"))

# Plot
plot(pred) +
  labs(
    title = "Effect of Protection per Region and Group",
    x = "% of Range Protected",
    y = "Predicted Population Trend (logratio)"
  ) +
  theme_minimal()


# Vorhersagen generieren
pred_mon <- ggpredict(poptrend_lm5, terms = c("percent_range_protected ",# [0:50 by=5]", 
                                              "main_group2", 
                                              "region"))


model_data_mon <- model.frame(poptrend_lm5)

# colours
main_group_colors <- c(
  "forest specialist" = "#6F4742",   # erdiges Dunkelrotbraun
  "generalist"     = "#8A837C",   # neutrales Steingrau
  "open specialist"      = "#C4B45F" ,   # warmes, gedecktes Gelbgrün
  "montane forest"            = "#3B5F41",  # kühles, gedecktes Tannengrün
  "montane open"              = "#A7B49D"   # weiches, moosiges Graugrün
) 


# Wenn du Facets nach region willst (wie in pred$facet):
model_data_mon$facet <- model_data_mon$region

forestreg2 <- ggplot(pred_mon, aes(x = x, y = predicted)) +
  geom_line(aes(color = group), linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, color = NA) +
  facet_wrap(~ facet) +
  scale_color_manual(
    values = main_group_colors
  ) +
  geom_point(
    data = model_data_mon,
    aes(x = percent_range_protected, y = logratio, color = main_group2),
    alpha = 0.4,
    size = 2,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = main_group_colors
  ) +
  labs(
    x = expression("% of range protected (t"[1]*")"),
    y = expression("Population Trend (log(SOP (t"[3]*"/t"[1]*"))"),
    color = "Habitat type",
    fill = "Habitat type"
  ) +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    strip.text = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 24),
    axis.title = element_text(size = 32),
    axis.text = element_text(size = 24),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
forestreg2

ggsave(
  filename = "./figures/poptrend_range_model_reg_montane.png",
  plot = forestreg2,
  width = 30, height = 17, dpi = 140, units= "cm", bg = "transparent"
)









# Predictions for Interaction protection × main_group
preds <- ggpredict(poptrend_lm4, terms = c("percent_range_protected", "main_group"))
summary(poptrend_lm4)
summary(emtrends(poptrend_lm4, ~ main_group, var = "percent_range_protected"), infer = TRUE)



model_data1 <- model.frame(poptrend_lm4)
# Plot
poptrend <-ggplot(preds, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_point(
    data = model_data1,
    aes(x = percent_range_protected, y = logratio, color = main_group),
    alpha = 0.4,
    size = 2,
    inherit.aes = FALSE
  ) +
  geom_line(
    aes(linetype= group),
    position = position_dodge(width = 0.5),
    linewidth = 1.2,
    alpha = 0.8
  ) +
  scale_linetype_manual(guide= "none",
    values = c(
      "forest specialist" = "solid",
      "generalist" = "dashed",
      "open specialist" = "dashed"
    )
  )+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  scale_color_manual(values = main_group_colors) +
  scale_fill_manual(values = main_group_colors) +
  labs(
    x = expression("% of range protected (t"[1]*")"),
    y = "Predicted population trend (log-ratio)",
    color = "Habitat type",
    fill = "Habitat type"
  ) +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 24),
    axis.title = element_text(size = 32),
    axis.text = element_text(size = 24),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
ggsave(
  filename = "./figures/poptrend_range_model.png",
  plot = poptrend,
  width = 30, height = 17, dpi = 140, units= "cm", bg = "transparent"
)
poptrend_summary_t %>%
  count(main_group)


# poptren for each region alone--------------------

# Regionen definieren
regions <- unique(poptrend_summary_treg$region)

# Leere Listen
pred_list <- list()
model_summaries <- list()

# Schleife über Regionen
for (r in regions) {
  df_region <- filter(poptrend_summary_treg, region == r)
  
  model <- lm(
    logratio ~ percent_range_protected * main_group+log_SOP_T1 + log(n_cells_T1) ,
    data = df_region
  )
  
  preds <- ggpredict(model, terms = c("percent_range_protected", "main_group"))
  preds$region <- r
  pred_list[[r]] <- preds
  
  # Model summary speichern
  model_summaries[[r]] <- capture.output(summary(model))
}


for (r in regions) {
  cat("\n\n===== Region:", r, "=====\n")
  print(summary(model_summaries[[r]]))
}

# Alle Vorhersagen zusammenführen
preds_all <- bind_rows(pred_list)
preds_all$group <- preds_all$main_group
model_data_reg <- model.frame(model)

plot_main_group_by_region_trend <- ggplot(preds_all, aes(x = x, y = predicted, group = group, color = group, fill = group)) +
  geom_line(
    linewidth = 1.2,
    alpha = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  geom_point(
    data = model_data_reg,
    aes(x = percent_range_protected, y = logratio, color = main_group),
    alpha = 0.4,
    size = 2,
    inherit.aes = FALSE
  ) +
  facet_wrap(~region) +
  scale_color_manual(values = main_group_colors, name = "Habitat type") +
  scale_fill_manual(values = main_group_colors, name = "Habitat type") +
  labs(
    x = expression("% of range protected (t"[1]*")"),
    y = "Predicted population trend (log-ratio)"
  ) +
  theme_classic(base_family = "roboto_condensed") +
  theme(
    axis.line = element_line(color = "black"),
    text = element_text(family = "roboto_condensed"),
    legend.title = element_text(size = 32),
    legend.text = element_text(size = 26),
    axis.title = element_text(size = 36),
    axis.text = element_text(size = 26),
    strip.text = element_text(size = 30, face = "italic", color = "brown"),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )


# Anzeigen
plot_main_group_by_region_trend
levels(preds_all$main_group2) 
ggsave(
  filename = "./figures/poptrend_range_model_regionsalone.png",
  plot = plot_main_group_by_region_trend,
  width = 30, height = 17, dpi = 140, units= "cm", bg = "transparent"
)


# Histogram percentage range protected t1----------

trend_hist_t1 <- ggplot(poptrend_summary, aes(x = percent_range_protected)) +
  geom_histogram(binwidth = 0.4, color = "gray95", linewidth = 0.5, aes(fill=..count..)) +
  scale_fill_gradient(
    low = "#A7B49D",     # sehr dezentes Grün
    high = "#8B9B79",    # etwas satteres Grün
    guide = "none"       # 👉 entfernt die Legende
  ) +
  labs(
    x = expression("% of range protected (t "[ 1]*")"),
    y = "Number of species"
  ) +
  theme_few(base_family = "roboto_condensed") +
  theme(
    axis.title = element_text(size = 50),
    axis.text = element_text(size = 40),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
# Save the histogram
ggsave(
  filename = "./figures/poptrend_hist_t1.png",
  plot = trend_hist_t1,
  width = 30, height = 30, dpi = 160, units= "cm", bg= "transparent"
)

trend_hist_t2 <- ggplot(poptrend_summary, aes(x = percent_range_protected70)) +
  geom_histogram(binwidth = 0.4, color = "gray95", linewidth = 0.5, aes(fill = ..count..)) +
  scale_fill_gradient(
    low = "#4C6D8C",   # Hauptton
    high = "#5B7B99",  # sehr ähnliche, etwas hellere Variante
    guide = "none"
  ) +
  labs(
    x = expression("% of range protected (t"[1]*")"),
    y = "Number of species"
  ) +
  theme_few(base_family = "roboto_condensed") +
  theme(
    axis.title = element_text(size = 50),
    axis.text = element_text(size = 40),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )


ggsave(
  filename = "./figures/poptrend_hist_t2_70%.png",
  plot = trend_hist_t2,
  width = 30, height = 30, dpi = 160, units= "cm", bg= "transparent"
)

# Deeper Look at forest specialist species
forest_specialists <- poptrend_summary_t %>%
  filter(main_group == "forest specialist") %>%
  arrange(desc(percent_range_protected))
forest <- lm(logratio ~ EIVEres.M, data = forest_specialists)
summary(forest)

forest_hist_t1 <- ggplot(forest_specialists, aes(x = percent_range_protected)) +
  geom_histogram(binwidth = 0.4, color = "gray95", linewidth = 0.5, aes(fill=..count..)) +
  scale_fill_gradient(
    low = "#A7B49D",     # sehr dezentes Grün
    high = "#8B9B79",    # etwas satteres Grün
    guide = "none"       # 👉 entfernt die Legende
  ) +
  labs(
    x = expression("% of range protected (t "[ 1]*")"),
    y = "Number of Forest species"
  ) +
  theme_few(base_family = "roboto_condensed") +
  theme(
    axis.title = element_text(size = 50),
    axis.text = element_text(size = 40),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )




library(dplyr)
library(ggplot2)

# Schritt 1: Daten ggf. runden, um Gitter zu erzeugen
heatmap_data <- forest_specialists %>%
  mutate(
    N_class = round(EIVEres.N, 0),     # oder z.B. floor() für gröbere Klassen
    M_class = round(EIVEres.M, 0)
  ) %>%
  group_by(N_class, M_class) %>%
  summarise(
    mean_logratio = mean(logratio, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# Schritt 2: Heatmap zeichnen
library(dplyr)
library(ggplot2)

# Schritt 1: Daten ggf. runden, um Gitter zu erzeugen
heatmap_data <- forest_specialists %>%
  mutate(
    N_class = round(EIVEres.N, 0),     # oder z.B. floor() für gröbere Klassen
    M_class = round(EIVEres.M, 0)
  ) %>%
  group_by(N_class, M_class) %>%
  summarise(
    mean_logratio = mean(logratio, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# Schritt 2: Heatmap zeichnen
ggplot(heatmap_data, aes(x = N_class, y = M_class, fill = mean_logratio)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#b2182b",   # rot für Rückgang
    mid = "white",
    high = "#2166ac",  # blau für Anstieg
    midpoint = 0,
    name = "Mean\nlog-ratio"
  ) +
  labs(
    x = "Ellenberg N indicator value (Nitrogen preference)",
    y = "Ellenberg M indicator value (Moisture preference)",
    title = "Forest specialists: Population trend across N and Moisture gradients"
  ) +
  theme_minimal(base_family = "roboto_condensed") +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold")
  )
  labs(
    x = "Ellenberg N indicator value (Nitrogen preference)",
    y = "Ellenberg M indicator value (Moisture preference)",
    title = "Forest specialists: Population trend across N and Moisture gradients"
  ) +
  theme_minimal(base_family = "roboto_condensed") +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold")
  )

  
  
  levels(forest_specialists$TaxonName) 
  
  
  # Sortiere nach Ellenberg-N, um Histogramm optisch klarer zu machen
  forest_specialists <- forest_specialists %>%
    arrange(EIVEres.N) %>%
    mutate(TaxonName = factor(TaxonName, levels = TaxonName))
  
  # 1. Histogramm der N-Werte (eigentlich ein Balkendiagramm mit Artnamen)
  ggplot(forest_specialists, aes(x = TaxonName, y = EIVEres.N)) +
    geom_col(fill = "darkolivegreen4") +
    labs(
      title = "Ellenberg N-Werte der Waldspezialisten",
      x = NULL,
      y = "Ellenberg N (Nährstoffanspruch)"
    ) +
    theme_minimal(base_family = "roboto_condensed") +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      plot.title = element_text(face = "bold", size = 14)
    )
  
  # 2. Scatterplot N vs. Licht
  ggplot(forest_specialists, aes(x = Ellenberg_N, y = Ellenberg_L, label = TaxonName)) +
    geom_point(color = "forestgreen", size = 3) +
    geom_text(size = 3, vjust = -0.5, family = "roboto_condensed") +
    labs(
      title = "Zusammenhang von Nährstoff- und Lichtanspruch",
      x = "Ellenberg N",
      y = "Ellenberg L"
    ) +
    theme_minimal(base_family = "roboto_condensed")
  
  
  
  # Beispiel: PCA auf Ellenberg-Werten (ohne fehlende Werte)
  library(ggfortify)
  scale_colour_gradient2(
    low = "#a50026",       # dunkelrot
    mid = "#f7f7f7",       # hellgrau-beige
    high = "#006837",      # sattes tiefgrün
    midpoint = 0,
    name = "Pop.-Trend"
  )
 
  
  
 # PCA forest --------------------------- 
  
   # 1. Prepare the variables
  ellenberg_vars <- forest_specialists %>%
    select(matches("^EIVEres\\.(?!.*\\.nw3$)", perl = TRUE)) %>%
    na.omit()
  
  # 2. PCA
  pca <- prcomp(ellenberg_vars, scale. = TRUE)
  
  # 3. Scores und Metadaten
  scores <- as.data.frame(pca$x)
  forest_specialists_filtered <- forest_specialists %>%
    filter(!is.na(logratio) & !is.na(percent_range_protected)) %>%
    slice(1:nrow(ellenberg_vars))
  
  scores$TaxonName <- forest_specialists_filtered$TaxonName
  scores$logratio <- forest_specialists_filtered$logratio
  scores$protected <- forest_specialists_filtered$percent_range_protected
  
  # 4. Loadings vorbereiten
  loadings <- as.data.frame(pca$rotation)
  colnames(loadings)[1:2] <- c("PC1", "PC2")
  loadings$variable <- rownames(loadings)
  loadings$label <- recode(loadings$variable,
                           "EIVEres.N" = "Nitrogen (N)",
                           "EIVEres.L" = "Light (L)",
                           "EIVEres.T" = "Temperature (T)",
                           "EIVEres.R" = "pH (R)",
                           "EIVEres.M" = "Moisture (M)"
  )
  # Anteil der erklärten Varianz pro Achse
  explained_var <- summary(pca)$importance[2, 1:2]  # PC1 und PC2
  percent_var <- round(explained_var * 100, 1)
  
  # 5. Scaling factor
  arrow_scale <- 3
  
  
  # Plot mit erklärter Varianz in Achsentiteln
  pca_plot <- ggplot(scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(colour = logratio, size = protected), alpha = 0.7) +
    geom_text_repel(aes(label = TaxonName), size = 4.5, family = "roboto_condensed") +
    geom_segment(data = loadings,
                 aes(x = 0, y = 0, xend = PC1 * arrow_scale, yend = PC2 * arrow_scale),
                 arrow = arrow(length = unit(0.6, "cm")),
                 colour = "#014636", linewidth = 1) +
    geom_text(data = loadings,
              aes(x = PC1 * arrow_scale * 1.17, y = PC2 * arrow_scale * 1.17, label = label),
              family = "roboto_condensed", colour = "#014636", size = 8) +
    scale_colour_gradient2(
      low = "#8c510a", mid = "#f6f1e1", high = "#01665e", midpoint = 0,
      name = "Population trend\n(log ratio)"
    ) +
    scale_size_continuous(range = c(2, 10), name = "% range protected") +
    labs(
      x = paste0("PC1 (", percent_var[1], "%)"),
      y = paste0("PC2 (", percent_var[2], "%)")
    ) +
    theme_minimal(base_family = "roboto_condensed") +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_text(size = 32),
      legend.text = element_text(size = 26),
      axis.title = element_text(size = 36),
      axis.text = element_text(size = 26),
      legend.position = "right"
    )
  
  
  pca_plot
  
  ggsave(
    filename = "./figures/pca_forestspecies.png",
    plot = pca_plot,
    width = 30,
    height = 18,
    dpi = 160,
    units = "cm",
    bg = "transparent"
  ) 
  
  
  
  
  
  # Beispielstruktur: EIVEres.N enthält 'TaxonName', 'N' (Zeigerwert), 'nw3' (Nischenbreite)
  # Berechne untere und obere Grenzen für die Linie
  EIVEres.N <- forest_specialists %>%
    select(TaxonName, N = EIVEres.N, nw3 = EIVEres.N.nw3) %>%
    filter(!is.na(N) & !is.na(nw3))  # optional: fehlende Werte ausschließen
  
  
  EIVEres.N <- EIVEres.N %>%
    mutate(
      TaxonName = factor(TaxonName, levels = TaxonName[order(N)]),
      N_lower = N - (nw3 / 2),
      N_upper = N + (nw3 / 2)
    )
  
  # Plot
  ggplot(EIVEres.N, aes(x = TaxonName, y = N)) +
    geom_errorbar(aes(ymin = N_lower, ymax = N_upper), width = 0.3, color = "grey50") +
    geom_point(size = 2.5, color = "darkgreen") +
    labs(
      title = "Ellenberg N-Wert und Nischenbreite",
      x = NULL,
      y = "Ellenberg N"
    ) +
    theme_minimal(base_family = "roboto_condensed") +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
      plot.title = element_text(face = "bold")
    )

  
  
  # Einfaches lineares Modell: Populationstrend als Funktion von Ellenberg N
  model_n <- lm(logratio ~ EIVEres.T+ log_SOP_T1 + n_cells_T1, data = forest_specialists)
  
  # Zusammenfassung anzeigen
  summary(model_n)
  ggplot(forest_specialists, aes(x = EIVEres.T, y = logratio, size = percent_range_protected)) +
    geom_jitter(width = 0.2, height = 0.05, alpha = 0.7, colour = "#3b8c5a") +  # neue Punktfarbe
    geom_smooth(
      method = "lm", 
      se = TRUE,
      colour = "#028b7e",      # grünliches Petrol für Linie
      fill = "#a3d5cd",        # dazu passender Ribbon
      linewidth = 1.2,
      alpha = 0.4
    ) +
    scale_size_continuous(name = "% range protected", range = c(1, 10)) +
    labs(
      x = "Ellenberg T (temperature preference)",
      y = "Population trend (log ratio)"
    ) +
    theme_minimal(base_family = "roboto_condensed") +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  

