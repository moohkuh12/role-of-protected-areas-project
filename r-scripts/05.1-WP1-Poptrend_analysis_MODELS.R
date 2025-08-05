# Survival trens analysis

# Load preamble (setup and libraries) -------------------------------
source("./r_scripts/00-preamble.R")

# First tries of data analysis (Eichenberg et al. 2021 sMon data)

# Load dataframe
#sMon <- read_csv("./data/sMon/sMon_long.csv", col_names = TRUE)
sMon_wide <- read_csv("./data/sMon/sMon_wide_210725.csv")
smon_filtered <- sMon_wide %>%
  filter(urban_class != "urban")
rm(sMon_wide)
# we just use grid cells that are less than 50% urban 
head(sMon_filtered)

# 3. Increase Probability -----------------------

# This section estimates the probability that a species persists 
# in a grid cell between t1 and t3, depending on protection status.
# We use binomial GLMMs with species identity as random effect.
# ---------------------------------------------------------



trend_data<- smon_filtered%>%
  filter(OP_T1 > 0) %>%
  mutate(
    # Trend: 1 wenn OP_T3 > OP_T1 (Zunahme), sonst 0 (Abnahme oder gleich)
    increase = ifelse(OP_T3 > OP_T1, 1, 0),
    protection_cat = protection_cat) %>%
  select(id, TaxonName, increase, #starts_with("protection")
  protection_cat, cov_frac)

# Step 1: Prepare increase/decrease dataset (species presence in t1 ≥1%)
trend_data <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  mutate(
    increase = ifelse(OP_T3 > OP_T1, 1, 0),
    protecttion_cat = protection_cat,  # Tippfehler: protect**t**ion_cat
    # optional: Umbenennung entfernen, wenn gleichnamig
  ) %>%
  select(id, TaxonName, increase, starts_with("protection"))


# Step 2: Define protection thresholds
thresholds <- c("protection50", "protection60", "protection70", "protection80", "protection90", "protection_cat")

# Step 3: Fit binomial GLMMs per threshold
models_by_threshold <- list()

for (thresh in thresholds) {
  message("Running model for: ", thresh)
  
  trend_data_thresh <- smon_filtered %>%
    filter(OP_T1 >0) %>%
    mutate(
      increase = ifelse(OP_T3 > OP_T1, 1, 0),
      protection = .data[[thresh]]
    ) %>%
    select(TaxonName, protection, increase)
  
  models_by_threshold[[thresh]] <- glmer(
    increase ~ protection + (1 | TaxonName),
    data = trend_data_thresh,
    family = binomial
  )
}

# models take really long: saved output
#for (thresh in thresholds) {
 model_trend <- models_by_threshold[[thresh]]
 saveRDS(model_trend, file.path("saved_models", paste0("model_trend", thresh, ".rds")))
# }

# Load models
for (thresh in thresholds) {
  models_by_threshold[[thresh]] <- readRDS(file.path("saved_models", paste0("model_", thresh, ".rds")))
}

 
# Test: Binomial model with management as random effect -----
 model_mgmt <- glmer(
   increase ~ protection_cat + (1 | TaxonName) +(1|management_class),
   data = trend_data_mgmt,
   family = binomial
 )
 
 
 # Step 4: Overdispersion check
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  ratio <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  data.frame(chisq = Pearson.chisq, ratio = ratio, rdf = rdf, p.value = pval)
}

dispersion_table <- lapply(models_by_threshold, overdisp_fun) %>%
  bind_rows(.id = "threshold") %>%
  mutate(threshold = gsub("protection", "≥", threshold))

# Step 5: Extract predicted survival probabilities (emmeans, type = "response")
all_preds <- list()

for (thresh in thresholds) {
  mod <- models_by_threshold[[thresh]]
  em <- emmeans(mod, "protection", type = "response") %>% as.data.frame()
  em$threshold <- gsub("protection", "≥", thresh)
  all_preds[[thresh]] <- em
}

trend_table <- bind_rows(all_preds)

summary(models_by_threshold[["protection_cat"]])
summary(models_by_threshold[["protection90"]])




# Create filtered survival tables for plotting
plot_prob90t <- filter(trend_table, threshold == "≥90")
plot_prob_cat_t <- filter(trend_table, threshold == "≥_cat")

# Step 6: Extract odds ratios via contrasts (still on response scale)
all_contrasts <- list()

for (thresh in thresholds) {
  mod <- models_by_threshold[[thresh]]
  
  cont <- contrast(emmeans(mod, "protection", type = "response"), method = "pairwise") %>%
    summary(infer = TRUE) %>%
    as.data.frame()
  
  cont$threshold <- gsub("protection", "≥", thresh)
  cont$signif <- cut(cont$p.value,
                     breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                     labels = c("***", "**", "*", ".", ""))
  
  all_contrasts[[thresh]] <- cont
}

contrast_table <- bind_rows(all_contrasts) %>%
  select(threshold, contrast, odds.ratio, SE, z.ratio, p.value, signif) 


# Step 7: Model summary table with R²
model_summary_table <- lapply(names(models_by_threshold), function(thresh) {
  mod <- models_by_threshold[[thresh]]
  s <- summary(mod)$coefficients
  df <- as.data.frame(s)
  df$effect <- rownames(df)
  df$threshold <- gsub("protection", "≥", thresh)
  df
}) %>%
  bind_rows() %>%
  select(threshold, effect, Estimate, `Std. Error`, `z value`, `Pr(>|z|)`) %>%
  rename(
    estimate = Estimate,
    SE = `Std. Error`,
    z = `z value`,
    p.value = `Pr(>|z|)`
  ) %>%
  mutate(
    signif = cut(p.value,
                 breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                 labels = c("***", "**", "*", ".", ""))
  )


# Step 8: Plot predicted probabilities with significance for ≥90
contrasts_90 <- filter(contrast_table, threshold == "≥90")

comparisons <- list(
  c("not protected", "part protected"),
  c("not protected", "protected"),
  c("part protected", "protected")
)

annotations <- c("***", "**", "ns")

# Jetzt NUR contrasts_pl90 definieren:
contrasts_pl90 <- ggplot(plot_prob90t, aes(x = protection, y = prob, fill = protection)) +
  geom_point(size = 14, shape = 21, color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1, size = 1) +
  geom_signif(
    comparisons = comparisons,
    annotations = annotations,
   # y_position = c(0.970, 0.9715, 0.9724),
    tip_length = 0.01,
    textsize = 10
  ) +
  #ylim(0.964, 0.973) +
  scale_fill_manual(values = c(
    "protected" = "#006837",
    "part protected" = "#74c476",
    "not protected" = "#d9f0d3"
  )) +
  labs(
    x = "Protection status (≥90%)",
    y = "Predicted probability of persistence"
  ) +
  theme_minimal(base_size = 30) +
  theme(legend.position = "none",
        axis.title = element_text(size = 36),
        axis.text = element_text(size = 28))



ggsave(
  filename = "./figures/survival_probability90.png", 
  plot = pl90,
  width = 50, height = 30, units = "cm", dpi = 100
)

# Step 9: Same for ≥80
contrasts_cat <- filter(contrast_table, threshold == "≥_cat")
annotations_cat <- contrasts_cat$signif

comparisons_cat <- list(
  c("not protected", "part protected"),
  c("not protected", "protected"),
  c("part protected", "protected")
)
pl_cat <- ggplot(plot_prob_cat_t, aes(x = protection, y = prob, fill = protection)) +
  geom_point(size = 14, shape = 21, alpha = 0.8) +  # keine schwarze Umrandung
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL, color = protection),  # errorbar in acadia_border
    width = 0.1, size = 1, linewidth = 1.5
  ) +
  scale_fill_manual(values = acadia_custom) +
  scale_color_manual(values = acadia_border) +
  labs(
    x = "Protection status",
    y = "Predicted probability of increase in occ. prob."
  ) +
  theme_bw(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    legend.position = "none",
    text = element_text(family = "roboto_condensed"),
    axis.title = element_text(size = 36),
    axis.text = element_text(size = 28)
  ) +
  coord_flip()  # Plot drehen


ggsave(
  filename = "./figures/increase_prob_glmm.png", 
  plot = pl_cat,
  width = 50, height = 30, units = "cm", dpi = 100
)

# Step 10: Export results
write_xlsx(survival_table, path = "./tables/survival_probabilities.xlsx")
write_xlsx(contrast_table, path = "./tables/survival_contrasts_oddsratios.xlsx")
write_xlsx(model_summary_table, path = "./tables/survival_model_summary.xlsx")
write_xlsx(dispersion_table, path = "./tables/survival_dispersion_checks.xlsx")


#

valid_survival <- smon_filtered %>%
  filter(OP_T1 > 0.01)  # only take gridcells where a species occurred with a probability over 1% in T1


survival_analysis <- valid_survival %>%
  group_by(TaxonName, protection_cat) %>%  # per species and protection status
  summarise(
    total_cells_T1 = n(),                              # number of gridcells in T1
    survived_cells_T3 = sum(OP_T3 > 0.01, na.rm = TRUE),  # Number of survived gridcells in T3
    survival_prob = survived_cells_T3 / total_cells_T1  # Survival probability
  ) %>%
  ungroup()

hist(survival_analysis$total_cells_T1)
survival_analysis_clean <- survival_analysis %>%
  filter(total_cells_T1 >= 5)

# 3.2 Beta-Regression: Protection status as predictor for survival probability -----------
m_linear <- betareg(survival_prob ~ protection_cat,  data = survival_analysis)
m_linear2 <- betareg(survival_prob ~ protection_cat,  data = survival_analysis_clean)
#m_log <- betareg(survival_prob ~ protection_cat + log1p(total_cells_T1), data = survival_analysis)

ggplot(survival_analysis_clean, aes(x = total_cells_T1, y = survival_prob)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  scale_x_log10() +
  theme_minimal()
plot(m_linear, which = 1)

summary(survival_analysis$total_cells_T1)
any(is.na(survival_analysis$total_cells_T1))

qbinom_surv <- glm(
  cbind(survived_cells_T3, total_cells_T1 - survived_cells_T3) ~ protection_cat + total_cells_T1 ,
  family = quasibinomial,
  data = survival_analysis
)

qbinom_surv2 <- glm(
  cbind(survived_cells_T3, total_cells_T1 - survived_cells_T3) ~ protection_cat + log1p(total_cells_T1),
  family = quasibinomial,
  data = survival_analysis_clean
)
summary(binom_surv)

binom_surv <- glm(
  cbind(survived_cells_T3, total_cells_T1 - survived_cells_T3) ~ protection_cat + offset(log(total_cells_T1)),
  family = binomial,
  data = survival_analysis
)
summary(binom_surv)

overdisp_fun <- function(binom_surv) {
  rdf <- df.residual(binom_surv)
  rp <- residuals(binom_surv, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  ratio <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  data.frame(chisq = Pearson.chisq, ratio = ratio, rdf = rdf, p.value = pval)
}

overdisp_fun(binom_surv)
emmeans(binom_surv, "protection90", type = "response")
contrast(emmeans(binom_surv, "protection90", type = "response"), method = "pairwise") %>%
  summary(infer = TRUE)
plot(emmeans(binom_surv, "protection90", type = "response"))



# model comparision - what is the best model?-----------------
# Beta-Modell: z. B. ohne total_cells_T1
pred_beta2 <- predict(m_linear2, type = "response")

# Quasibinomial-Modell: mit total_cells_T1
pred_qb2 <- predict(qbinom_surv2, type = "response")

library(yardstick)

# Annahme: survival_prob ist in survival_analysis_clean
rmse_beta2 <- rmse_vec(truth = survival_analysis_clean$survival_prob, estimate = pred_beta2)
rmse_qb2   <- rmse_vec(truth = survival_analysis_clean$survival_prob, estimate = pred_qb2)

mae_beta2 <- mae_vec(truth = survival_analysis_clean$survival_prob, estimate = pred_beta2)
mae_qb2   <- mae_vec(truth = survival_analysis_clean$survival_prob, estimate = pred_qb2)
####------------


m_beta_plot <- ggplot(survival_analysis, aes(x = protection_cat, y = survival_prob, fill = protection_cat)) +
  
  # Half-violin (links)
  geom_half_violin(
    side = "l",
    alpha = 0.3,
    color = NA,
    scale= "count",
    position = position_nudge(x = -0.1)
  ) +
  
  # Jitterpunkte (leicht nach rechts versetzt)
  geom_jitter(
    aes(color = protection_cat),
    size = 1.2,
    alpha = 0.4,
    width = 0.05,
    #position = position_nudge(x = 0.15)
  ) +
  
  # Boxplot (zentriert)
  geom_boxplot(
    aes(color = protection_cat),
    outlier.shape = NA,
    width = 0.15,
    position = position_nudge(x = 0.18)
  ) +
  
  # Farben
  scale_fill_manual(values = acadia_custom) +
  scale_color_manual(values = acadia_border) +
  labs(
    x = "Protection category",
    y = NULL
  ) +
  #ylab(NULL) +
  ylim(0, 1) +
  coord_flip() +  # Plot drehen
  theme_bw(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    axis.title = element_text(size = 80),
    axis.text = element_text(size = 70),
    legend.position = "none"
  )
ggsave(
  filename = "./figures/beta_survival_plot.png",
  plot = m_beta_plot,
  width = 25, height = 25, units = "cm", dpi = 100
)

# Neue Vorhersagespalte auf der Wahrscheinlichkeits-Skala
survival_glmdata_plot <- survival_analysis %>%
  mutate(predicted_prob = predict(qbinom_surv, type = "response"))


m_qb_plot <- ggplot(survival_glmdata_plot, aes(x = protection_cat, y = predicted_prob, fill = protection_cat)) +
  
  # Half-violin (links)
  geom_half_violin(
    side = "l",
    alpha = 0.3,
    color = NA,
    scale = "count",
    position = position_nudge(x = -0.1)
  ) +
  
  # Jitterpunkte (leicht rechts)
  geom_jitter(
    aes(color = protection_cat),
    size = 1.2,
    alpha = 0.4,
    width = 0.05
  ) +
  
  # Boxplot (leicht nach rechts versetzt)
  geom_boxplot(
    aes(color = protection_cat),
    outlier.shape = NA,
    width = 0.15,
    position = position_nudge(x = 0.18)
  ) +
  
  # Farben
  scale_fill_manual(values = acadia_custom) +
  scale_color_manual(values = acadia_border) +
  
  # Achsen und Theme
  coord_flip() +
  ylim(0, 1) +
  labs(
    x = "Protection category",
    y = NULL
  ) +
  ylab(NULL) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 24),
    legend.position = "none"
  )

em_qb <- emmeans(qbinom_surv, ~ protection_cat, type = "response") %>% 
  as.data.frame()


plot_em_qb <- ggplot(em_qb, aes(x = protection_cat, y = prob, fill = protection_cat)) +
  geom_point(size = 6, shape = 21, alpha = 0.8) + 
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, color=protection_cat), width = 0.15, linewidth = 1.2) +
  
  # Farben
  scale_fill_manual(values = acadia_custom) +
  scale_color_manual(values = acadia_border) +
  
  # Achsen & Theme
  coord_flip() +
 # ylim(0.85, 1) +
  labs(
    x = NULL,
    y = NULL
  ) +
  ylab(NULL) +  # Kein y-Achsentitel
  theme_bw(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 64),
    axis.text.y = element_blank(),  # Keine y-Achsenticks
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

p_patch_trend <- m_beta_plot + plot_em_qb & xlab(NULL) & theme(plot.margin = margin(5.5, 20, 0, 5.5))
surv_plot <- wrap_elements(panel = p_patch_trend) +
  labs(tag = "Estimated survival probability") +
  theme(
    text  = element_text(family = "roboto_condensed"),
    plot.tag = element_text(size = 80),
    plot.tag.position = "bottom"
  )
ggsave("./figures/survival130625.png",
       surv_plot,
       width = 40, height = 20, units = "cm", dpi = 300)

# Survival glmer-----------------------
survival_analysis <- survival_analysis %>%
  mutate(obs_id = row_number())  # Jede Zeile bekommt eindeutige ID
# Fit a beta-binomial model with an observation-level random effect

survival_analysis$protection_cat <- factor(survival_analysis$protection_cat, levels = c("not protected", "part protected", "protected"))

# betabinomial model -------------
model_bb <- glmmTMB(
  cbind(survived_cells_T3, total_cells_T1 - survived_cells_T3) ~ 
    protection_cat + log1p(total_cells_T1) + (1 | TaxonName),
  family = betabinomial(),
  data = survival_analysis
)


#Ein Observation-Level Random Effect (OLRE) gibt jeder Beobachtung eine eigene Zufallskomponente, 
# um zusätzliche Streuung im Modell zu berücksichtigen – besonders nützlich bei Overdispersion.

pred_mixed <- ggpredict(model_bb, terms = "protection_cat", bias_correction = TRUE)
plot(pred_mixed)

summary(model_bb)

# Generate predicted marginal means for protection category
em_qb <- ggpredict(model_bb, terms = "protection_cat", bias_correction = T)
em_qb$group <- factor(em_qb$x, levels = c("not protected", "part protected", "protected"))


emm <- emmeans(model_bb, ~ protection_cat, type = "response")
pairwise_df <- pairs(emm) %>%
  as.data.frame() %>%
  mutate(
    group1 = str_split_fixed(contrast, " / ", 2)[,1],
    group2 = str_split_fixed(contrast, " / ", 2)[,2],
    label = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    # y-positions for significance annotations (fine-tune as needed)
    y.position = seq(0.987, 0.991, length.out = n())
  )

comparisons = Map(c, pairwise_df$group1, pairwise_df$group2)
levels(em_qb$group)
unique(pairwise_df$group1)
unique(pairwise_df$group2)


em_qb$group <- factor(em_qb$x, levels = c("not protected", "part protected", "protected"))

# Plot
plot_em_qb <- ggplot(em_qb, aes(x = group, y = predicted, fill = group)) +
  geom_signif(
    comparisons = Map(c, pairwise_df$group1, pairwise_df$group2),
    annotations = pairwise_df$label,
   # y_position = c(0.987, 0.991, 0.995), 
    step_increase = 0.04,
    tip_length = 0.01,
   vjust= 0.7,
    textsize = 20,
    color = "black"
  )+ 
  
  # Horizontale Referenzlinie bei y = 1
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.6) +

  geom_point(
    data = survival_analysis,
    aes(x = protection_cat, y = survival_prob, color = protection_cat),
    size = 1.5,
    alpha = 0.15,
    position = position_jitter(width = 0.09),
    inherit.aes = FALSE
  )+
  geom_point(size = 2, shape = 21, alpha = 0.8, fill="black") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = group),
    width = 0.03,
    linewidth = 1,
    color= "black"
  ) +
  # Achsen
  coord_flip() +
  ylim(0, 1.14) +
  labs(x = NULL, y = NULL) +
  
  
  # Farben
  scale_color_manual(
    values = c(
      "not protected" = "#9ab0c3",   # deutlich dunkler als hellblau
      "part protected" = "#0077AA",
      "protected" = "#005B96")
  )+
  scale_fill_manual(#values = acadia_custom
    values = c("not protected" = "#9cc1e2", "part protected" = "#6497b1", "protected" = "#005b96")) +
  
  # Theme
  theme_bw(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 64),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

# Anzeigen
plot_em_qb




###############
# Plot panel 1:
glmTBMplot <- ggplot(survival_analysis, aes(x = protection_cat, y = survival_prob, fill = protection_cat)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.6) +
  # Half-violin (links)
  geom_half_violin(
    side = "l",
    alpha = 0.3,
    color = NA,
    scale = "count",
    position = position_nudge(x = -0.06)
  ) +
  
  # # Jitterpunkte (leicht rechts)
  # geom_jitter(
  #   aes(color = protection_cat),
  #   size = 1.2,
  #   alpha = 0.4,
  #   width = 0.05
  # ) +
  
  # Boxplot (leicht nach rechts versetzt)
  geom_boxplot(
    aes(color = protection_cat),
    alpha= 0.4,
    outlier.shape = NA,
    width = 0.15,
    position = position_nudge(x = 0.12)
  ) +
  
  # Farben
  scale_color_manual(
    values = c(
      "not protected" = "#9ab0c3",   # deutlich dunkler als hellblau
      "part protected" = "#0077AA",
      "protected" = "#005B96")
  )+
  scale_fill_manual(#values = acadia_custom
    values = c("not protected" = "#9cc1e2", "part protected" = "#6497b1", "protected" = "#005b96")) +
  
  # Achsen und Theme
  coord_flip() +
  ylim(0, 1) +
  labs(
    x = "Protection category",
    y = NULL
  ) +
  ylab(NULL) +
  theme_bw(base_family = "roboto_condensed", base_size = 13) +
  theme(
    text = element_text(family = "roboto_condensed"),
    axis.title = element_text(size = 70),
    axis.text = element_text(size = 64),
    legend.position = "none"
  ) 

# 3) both plots together

p_patch_surv <- glmTBMplot + plot_em_qb & xlab(NULL) & theme(plot.margin = margin(5.5, 20, 0, 5.5))
p_surv <- wrap_elements(panel = p_patch_surv) +
  labs(tag = "Predicted survival probability") +
  theme(
    text  = element_text(family = "roboto_condensed"),
    plot.tag = element_text(size = rel(8)),
    plot.tag.position = "bottom"
  )

# 5) save the plot
ggsave("./figures/survivalprob280725.png",
       p_surv,
       width = 40, height = 20, units = "cm", dpi = 300)



# Beta-Modell: z. B. ohne total_cells_T1
m_linear <- betareg(survival_prob ~ protection_cat,  data = survival_analysis)
pred_beta <- predict(m_linear, type = "response")
summary(m_linear)

# model comparision - what is the best model?-----------------
# Quasibinomial-Modell: mit total_cells_T1
pred_qb <- predict(qbinom_surv, type = "response")
pred_glmer <- predict(model_bb , type = "response")

library(yardstick)

# Annahme: survival_prob ist in survival_analysis_clean
rmse_beta <- rmse_vec(truth = survival_analysis$survival_prob, estimate = pred_beta)
rmse_qb   <- rmse_vec(truth = survival_analysis$survival_prob, estimate = pred_qb)
rmse_glmer   <- rmse_vec(truth = survival_analysis$survival_prob, estimate = pred_glmer)
mae_beta <- mae_vec(truth = survival_analysis$survival_prob, estimate = pred_beta)
mae_qb   <- mae_vec(truth = survival_analysis$survival_prob, estimate = pred_qb)
mae_glmer   <- mae_vec(truth = survival_analysis$survival_prob, estimate = pred_glmer)


p_patch_trend <- m_beta_plot + plot_em_qb & xlab(NULL) & theme(plot.margin = margin(5.5, 20, 0, 5.5))
surv_plot <- wrap_elements(panel = p_patch_trend) +
  labs(tag = "Estimated survival probability") +
  theme(
    text  = element_text(family = "roboto_condensed"),
    plot.tag = element_text(size = 80),
    plot.tag.position = "bottom"
  )
ggsave("./figures/survival130625.png",
       surv_plot,
       width = 40, height = 20, units = "cm", dpi = 300)



# Beta-Regression all protection threshholds plots ---------

# Alle Schwellen kombinieren
protection_vars <- c("protection50", "protection60", "protection70", "protection80", "protection90")

beta_data_all <- lapply(protection_vars, function(thresh) {
  smon_filtered %>%
    filter(OP_T1 > 0.01) %>%
    mutate(
      protection = .data[[thresh]],
      threshold = gsub("protection", "≥", thresh),
      survived = ifelse(OP_T3 > 0.01, 1, 0)
    ) %>%
    group_by(TaxonName, protection, threshold) %>%
    summarise(
      total_cells_T1 = n(),
      survived_cells_T3 = sum(survived),
      survival_prob = survived_cells_T3 / total_cells_T1
    )
}) %>%
  bind_rows()

# Plot with facet_wrap for all threshholds - supplementary figure
beta_r_all <- ggplot(beta_data_all, aes(x = protection, y = survival_prob, fill = protection)) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  facet_wrap(~ threshold, nrow = 1) +
  scale_fill_manual(values = c(
    "protected" = "#006837",
    "part protected" = "#74c476",
    "not protected" = "#d9f0d3"
  )) +
  theme_minimal(base_size = 16) +
  labs(
    x = "Protection status",
    y = "Survival probability"
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 25, face = "bold"),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 28),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave( 
  filename = "./figures/beta_survival_plot_all.png",
  plot = beta_r_all,
  width = 40, height = 30, units = "cm", dpi = 100
)

# Beta - Regession models for all threshholds with summary table

# all protection thresholds
protection_vars <- c("protection50", "protection60", "protection70", "protection80", "protection90")

# empty lists to store models and summaries
beta_models <- list()
beta_model_summaries <- list()

# For each threshhold, fit a beta regression model
for (thresh in protection_vars) {
  message("Running beta regression for: ", thresh)
  
  # aggregate per species and protection status
  surv_data <- smon_filtered %>%
    filter(OP_T1 > 0.01) %>%
    mutate(
      protection = .data[[thresh]],
      survived = ifelse(OP_T3 > 0.01, 1, 0)
    ) %>%
    group_by(TaxonName, protection) %>%
    summarise(
      total_cells_T1 = n(),
      survived_cells_T3 = sum(survived),
      survival_prob = survived_cells_T3 / total_cells_T1,
      .groups = "drop"
    ) %>%
    # Beta regression doesnt work with 0 and 1 - transform slightly
    filter(survival_prob > 0 & survival_prob < 1)
  
  # Fitting the models
  mod <- betareg(survival_prob ~ protection, data = surv_data)
  beta_models[[thresh]] <- mod
  
  # Summary
  mod_summary <- broom::tidy(mod) %>%
    mutate(threshold = gsub("protection", "≥", thresh))
  
  beta_model_summaries[[thresh]] <- mod_summary
}

# bind summaries together
beta_summary_table <- bind_rows(beta_model_summaries)

# select relevant columns
beta_summary_table <- beta_summary_table %>%
  select(threshold, term, estimate, std.error, statistic, p.value) %>%
  mutate(
    signif = cut(p.value,
                 breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                 labels = c("***", "**", "*", ".", ""))
  )

# save table 
#write_xlsx(beta_summary_table, "./tables/beta_regression_summary.xlsx")



# Agg. Binomial increase / decrease -------------------
smon_filtered$region <- with(smon_filtered, ifelse(Latitude >= 51 & Longitude >= 10.5, "NE",
                                                   ifelse(Latitude >= 51 & Longitude < 10.5, "NW",
                                                          ifelse(Latitude < 51 & Longitude >= 10.5, "SE", "SW"))))


model_cellwise <- glmmTMB(
  increase ~ protection_cat + log1p(OP_T1) + (1 | TaxonName),
  data = trend_binary,
  family = betabinomial()
)
