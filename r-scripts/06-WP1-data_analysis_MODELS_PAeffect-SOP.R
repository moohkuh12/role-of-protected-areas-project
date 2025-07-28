# Load setup and libraries
source("./r_scripts/00-preamble.R")

# This script is used to prepare the data for the analysis of the protection effect 
# on species occurrence in the sMon dataset. After aggregation of the dataset at species and protection level, this script includes
# the models, summaries and diagnostics at the Germany scale. 
#Another script with the corresponding plots for the models is prepared seperately.

# Load dataframe
#sMon <- read_csv("./data/sMon/sMon_long.csv", col_names = TRUE)

# Long format of sMon data 
# NOTE: here the "old" classification for main_group is still used. tests with main_goup aka forest affiliation
# use the new euforplants csv and add it to the traits_df
sMon_wide <- read_csv("./data/sMon/sMon_wide_210725.csv")
smon_filtered <- sMon_wide %>%
  filter(urban_class != "urban")
rm(sMon_wide)


# Load the adapted euforplants data
euforplants <- read.csv("./data/landcover_analysis/euforplants_summary_new.csv")
euforplants2 <- read.csv("./data/landcover_analysis/euforplants_summary_new_montane2506.csv")

# Load trait df or create it
traits_df <- read_csv("./data/sMon/sMon_traits.csv")

traits_df <- smon_filtered%>%
  select(TaxonName, main_group, EIVEres.M, EIVEres.M.nw3,EIVEres.N, EIVEres.N.nw3, EIVEres.L, EIVEres.L.nw3, EIVEres.T, EIVEres.T.nw3, EIVEres.R, EIVEres.R.nw3, Family, PlantGrowthForm, Woodiness, LeafType, LeafPhenology ) %>%
  distinct()

# add new euforplants traits
traits_df<- traits_df %>%
  select(-main_group) %>%
  left_join(
    euforplants %>% select(wcvp_name, main_group),
    by = c("TaxonName" = "wcvp_name")
  )
traits_df<- traits_df %>%
  left_join(
    euforplants2 %>% select(wcvp_name, main_group2),
    by = c("TaxonName" = "wcvp_name")
  )

###############################
###############################

# Create aggregated dataframe for the analysis ------------
# (aggregation at species and protection level)

# Aggregate data by protection status per species 
# summary general --------------------------
# will be our main dataframe for the models 
summary_general <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  mutate(occ_change = OP_T3 - OP_T1) %>%
  group_by(TaxonName, protection_cat) %>%
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

# These are the same summarys for the other thresholds - these can be used for testing other threshholds for protection 
# sensitivity analysis ------------------------
summary_prot90 <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  mutate(occ_change = OP_T3 - OP_T1) %>%
  group_by(TaxonName, protection90) %>%
  summarise(
    SOP_T1 = sum(OP_T1, na.rm = TRUE),
    SOP_T3 = sum(OP_T3, na.rm = TRUE),
    log_SOP_T1 = log(SOP_T1 + 0.01),
    mean_occ_change = mean(occ_change, na.rm = TRUE),
    mean_OP_T1 = mean(OP_T1, na.rm = TRUE),  
    n_cells = n()) %>%
  mutate(
    delta_SOP = SOP_T3 - SOP_T1,
    rel_change = (SOP_T3 - SOP_T1) / SOP_T1 * 100,
    logratio = log((SOP_T3 + 0.01)/(SOP_T1 + 0.01))
  )
summary_prot80 <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  mutate(occ_change = OP_T3 - OP_T1) %>%
  group_by(TaxonName, protection80) %>%
  summarise(
    SOP_T1 = sum(OP_T1, na.rm = TRUE),
    SOP_T3 = sum(OP_T3, na.rm = TRUE),
    log_SOP_T1 = log(SOP_T1 + 0.01),
    mean_occ_change = mean(occ_change, na.rm = TRUE),
    mean_OP_T1 = mean(OP_T1, na.rm = TRUE),  
    n_cells = n()) %>%
  mutate(
    delta_SOP = SOP_T3 - SOP_T1,
    rel_change = (SOP_T3 - SOP_T1) / SOP_T1 * 100,
    logratio = log((SOP_T3 + 0.01)/(SOP_T1 + 0.01))
  )
summary_prot70 <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  mutate(occ_change = OP_T3 - OP_T1) %>%
  group_by(TaxonName, protection70) %>%
  summarise(
    SOP_T1 = sum(OP_T1, na.rm = TRUE),
    SOP_T3 = sum(OP_T3, na.rm = TRUE),
    log_SOP_T1 = log(SOP_T1 + 0.01),
    mean_occ_change = mean(occ_change, na.rm = TRUE),
    mean_OP_T1 = mean(OP_T1, na.rm = TRUE),  
    n_cells = n() )%>%
  mutate(
    delta_SOP = SOP_T3 - SOP_T1,
    rel_change = (SOP_T3 - SOP_T1) / SOP_T1 * 100,
    logratio = log((SOP_T3 + 0.01)/(SOP_T1 + 0.01))
  )
summary_prot60 <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  mutate(occ_change = OP_T3 - OP_T1) %>%
  group_by(TaxonName, protection60) %>%
  summarise(
    SOP_T1 = sum(OP_T1, na.rm = TRUE),
    SOP_T3 = sum(OP_T3, na.rm = TRUE),
    log_SOP_T1 = log(SOP_T1 + 0.01),
    mean_occ_change = mean(occ_change, na.rm = TRUE),
    mean_OP_T1 = mean(OP_T1, na.rm = TRUE),  
    n_cells = n()) %>%
  mutate(
    delta_SOP = SOP_T3 - SOP_T1,
    rel_change = (SOP_T3 - SOP_T1) / SOP_T1 * 100,
    logratio = log((SOP_T3 + 0.01)/(SOP_T1 + 0.01))
  )
summary_prot50 <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  mutate(occ_change = OP_T3 - OP_T1) %>%
  group_by(TaxonName, protection50) %>%
  summarise(
    SOP_T1 = sum(OP_T1, na.rm = TRUE),
    SOP_T3 = sum(OP_T3, na.rm = TRUE),
    log_SOP_T1 = log(SOP_T1 + 0.01),
    mean_occ_change = mean(occ_change, na.rm = TRUE),
    mean_OP_T1 = mean(OP_T1, na.rm = TRUE),  
    n_cells = n()) %>%
  mutate(
    delta_SOP = SOP_T3 - SOP_T1,
    rel_change = (SOP_T3 - SOP_T1) / SOP_T1 * 100,
    logratio = log((SOP_T3 + 0.01)/(SOP_T1 + 0.01))
  )

# Linear mixed effect models - SOP (summed occurence change) --------------------------

# Linear mixed model
m1_sop <- lmer(data = summary_general,
           logratio ~ protection_cat +  log_SOP_T1 + (1|TaxonName))

summary(m1_sop) # no significant effects of protection_cat, but significant effect of mean_OP_T1

plot(ggpredict(m1_sop, terms = "protection_cat"))

# 90% threshold - protected means that at least 90% of a cell is covered by protected areas, not protected is everything beneath 10%
m1.1_sop <- lmer(data = summary_prot90, logratio ~ protection90 +  log_SOP_T1 + (1|TaxonName))
summary(m1.1_sop) # no significant effects of protection_cat, but significant effect of mean_OP_T1
plot(ggpredict(m1.1_sop, terms = "protection90"))

# 80% threshold - protected means that at least 80% of a cell is covered by protected areas, not protected is everything beneath 10%
m1.2_sop <- lmer(data = summary_prot80, logratio ~ protection80 +  log_SOP_T1 + (1|TaxonName))
summary(m1.2_sop) # no significant effects of protection_cat, but significant effect of mean_OP_T1
plot(ggpredict(m1.2_sop, terms = "protection80"))

# 70% threshold - protected means that at least 70% of a cell is covered by protected areas, not protected is everything beneath 10%
m1.3_sop <- lmer(data = summary_prot70, logratio ~ protection70 +  log_SOP_T1 + (1|TaxonName))
summary(m1.3_sop) # no significant effects of protection_cat, but significant effect of mean_OP_T1
plot(ggpredict(m1.3_sop, terms = "protection70"))

# 60% threshold - protected means that at least 60% of a cell is covered by protected areas, not protected is everything beneath 10%
m1.4_sop <- lmer(data = summary_prot60, logratio ~ protection60 +  log_SOP_T1 + (1|TaxonName))
summary(m1.4_sop) # no significant effects of protection_cat, but significant effect of mean_OP_T1
plot(ggpredict(m1.4_sop, terms = "protection60"))

# 50% threshold - protected means that at least 50% of a cell is covered by protected areas, not protected is everything beneath 10%
m1.5_sop <- lmer(data = summary_prot50, logratio ~ protection50 +  log_SOP_T1 + (1|TaxonName))
summary(m1.5_sop) # no significant effects of protection_cat, but significant effect of mean_OP_T1
plot(ggpredict(m1.5_sop, terms = "protection50"))


#####################
# Model diagnostics
####################
resid <- residuals(m1_sop)
fitted <- fitted(m1_sop)
qq <- qqnorm(resid, plot.it = FALSE)

# 2. PNG-file
#png("./figures/diagnostic_plots_m1_logratio.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")

# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")

# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")

# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")

dev.off()
# all in all the model diagnostics look good, no homoscedasticity, a little deviation from normality, but not too bad.



# 2 
# Next we wanted to check if protection effects differ for plants with differing characteristics and ecological preferences.
# We test this first at the national level and use again mixed effect modelling with interactions.
# Linear mixed effect models - logratio with interactions --------------------------

# First, we need to merge the traits_df with the summary_general and the other threshold-summaries dataframes
summary_gen_traits <- left_join(summary_general, traits_df, by = "TaxonName")
summary_gen_traits90 <- left_join(summary_prot90, traits_df, by = "TaxonName")
summary_gen_traits80 <- left_join(summary_prot80, traits_df, by = "TaxonName")
summary_gen_traits70 <- left_join(summary_prot70, traits_df, by = "TaxonName")
summary_gen_traits60 <- left_join(summary_prot60, traits_df, by = "TaxonName")
summary_gen_traits50 <- left_join(summary_prot50, traits_df, by = "TaxonName")


# Nitrogen  ---------------
m2.1_sop <- lmer(data = summary_gen_traits, logratio ~ protection_cat * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1_sop) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1_sop)
plot(ggpredict(m2.1_sop, terms = c("protection_cat","EIVEres.N"))) # we will use this variant of visualization for the plots
plot(ggpredict(m2.1_sop, terms = c("EIVEres.N", "protection_cat"))) # this doesnt fit



# Diagnostics N --------
resid <- residuals(m2.1_sop)
fitted <- fitted(m2.1_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1_Nitrogen_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Nitrogen 90% threshold ---------------
m2.1.1_sop <- lmer(data = summary_gen_traits90, logratio ~ protection90 * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1.1_sop) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1.1_sop)
plot(ggpredict(m2.1.1_sop, terms = c("protection90","EIVEres.N"))) # we will use this variant of visualization for the plots

# Diagnostics N 90% threshold --------
resid <- residuals(m2.1.1_sop)
fitted <- fitted(m2.1.1_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1.1_Nitrogen90_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Nitrogen 80% threshold ---------------
m2.1.2_sop <- lmer(data = summary_gen_traits80, logratio ~ protection80 * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1.2_sop) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1.2_sop)
plot(ggpredict(m2.1.2_sop, terms = c("protection80","EIVEres.N"))) # we will use this variant of visualization for the plots


# Diagnostics N 80% threshold --------
resid <- residuals(m2.1.2_sop)
fitted <- fitted(m2.1.2_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1.2_Nitrogen80_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Nitrogen 70% threshold ---------------
m2.1.3_sop <- lmer(data = summary_gen_traits70, logratio ~ protection70 * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1.3_sop) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1.3_sop)
plot(ggpredict(m2.1.3_sop, terms = c("protection70","EIVEres.N"))) # we will use this variant of visualization for the plots


# Diagnostics N 70% threshold --------
resid <- residuals(m2.1.3_sop)
fitted <- fitted(m2.1.3_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1.2_Nitrogen70_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Nitrogen 60% threshold ---------------
m2.1.4_sop <- lmer(data = summary_gen_traits60, logratio ~ protection60 * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1.4_sop) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1.4_sop)
plot(ggpredict(m2.1.4_sop, terms = c("protection60","EIVEres.N"))) # we will use this variant of visualization for the plots


# Diagnostics N 60% threshold --------
resid <- residuals(m2.1.4_sop)
fitted <- fitted(m2.1.4_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1.2_Nitrogen60_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Nitrogen 50% threshold ---------------
m2.1.5_sop <- lmer(data = summary_gen_traits50, logratio ~ protection50 * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1.5_sop) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1.5_sop)
plot(ggpredict(m2.1.5_sop, terms = c("protection50","EIVEres.N"))) # we will use this variant of visualization for the plots


# Diagnostics N 50% threshold --------
resid <- residuals(m2.1.5_sop)
fitted <- fitted(m2.1.5_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1.2_Nitrogen50_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()




# Ph - Reaction ---------------
m2.2_sop <- lmer(data = summary_gen_traits, logratio ~ protection_cat * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2_sop) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2_sop)
plot(ggpredict(m2.2_sop, terms = c("protection_cat","EIVEres.R"))) # we will use this variant of visualization for the plots

# Diagnostics R --------
resid <- residuals(m2.2_sop)
fitted <- fitted(m2.2_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.2_pH_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Ph - Reaction 90% threshold ---------------
m2.2.1_sop <- lmer(data = summary_gen_traits90, logratio ~ protection90 * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2.1_sop) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2.1_sop)
plot(ggpredict(m2.2.1_sop, terms = c("protection90","EIVEres.R"))) # we will use this variant of visualization for the plots


# Diagnostics R 90% threshold --------
resid <- residuals(m2.2.1_sop)
fitted <- fitted(m2.2.1_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.2.1_pH90_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Ph - Reaction 80% threshold ---------------
m2.2.2_sop <- lmer(data = summary_gen_traits80, logratio ~ protection80 * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2.2_sop) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2.2_sop)
plot(ggpredict(m2.2.2_sop, terms = c("protection80","EIVEres.R"))) # we will use this variant of visualization for the plots

# Diagnostics R 80% threshold --------
resid <- residuals(m2.2.2_sop)
fitted <- fitted(m2.2.2_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.2.2_pH80_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Ph - Reaction 70% threshold ---------------
m2.2.3_sop <- lmer(data = summary_gen_traits70, logratio ~ protection70 * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2.3_sop) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2.3_sop)
plot(ggpredict(m2.2.3_sop, terms = c("protection70","EIVEres.R"))) # we will use this variant of visualization for the plots
# Diagnostics R 70% threshold --------
resid <- residuals(m2.2.3_sop)
fitted <- fitted(m2.2.3_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.2.3_pH70.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Ph - Reaction 60% threshold ---------------
m2.2.4_sop <- lmer(data = summary_gen_traits60, logratio ~ protection60 * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2.4_sop) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2.4_sop)
plot(ggpredict(m2.2.4_sop, terms = c("protection60","EIVEres.R"))) # we will use this variant of visualization for the plots


# Diagnostics R 60% threshold --------
resid <- residuals(m2.2.4_sop)
fitted <- fitted(m2.2.4_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.2.4_pH60_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Ph - Reaction 50% threshold ---------------
m2.2.5_sop <- lmer(data = summary_gen_traits50, logratio ~ protection50 * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2.5_sop) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2.5_sop)
plot(ggpredict(m2.2.5_sop, terms = c("protection50","EIVEres.R"))) # we will use this variant of visualization for the plots


# Diagnostics R 50% threshold --------
resid <- residuals(m2.2.5_sop)
fitted <- fitted(m2.2.5_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.2.5_pH50_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()




# Temperature ---------------
m2.3_sop <- lmer(data = summary_gen_traits, logratio ~ protection_cat * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3_sop) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3_sop)
plot(ggpredict(m2.3_sop, terms = c("protection_cat","EIVEres.T"))) # we will use this variant of visualization for the plots

# Diagnostics T --------
resid <- residuals(m2.3_sop)
fitted <- fitted(m2.3_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3_Temperature_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Temperature 90% threshold ---------------
m2.3.1_sop <- lmer(data = summary_gen_traits90, logratio ~ protection90 * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.1_sop) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3.1_sop)
plot(ggpredict(m2.3.1_sop, terms = c("protection90","EIVEres.T"))) # we will use this variant of visualization for the plots

# Diagnostics T 90% threshhold--------
resid <- residuals(m2.3.1_sop)
fitted <- fitted(m2.3.1_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.1_Temperature90_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Temperature 80% threshold ---------------
m2.3.2_sop <- lmer(data = summary_gen_traits80, logratio ~ protection80 * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.2_sop) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3.2_sop)
plot(ggpredict(m2.3.2_sop, terms = c("protection80","EIVEres.T"))) # we will use this variant of visualization for the plots

# Diagnostics T 80% threshold--------
resid <- residuals(m2.3.2_sop)
fitted <- fitted(m2.3.2_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.2_Temperature80_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Temperature 70% threshold ---------------
m2.3.3_sop <- lmer(data = summary_gen_traits70, logratio ~ protection70 * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.3_sop) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3.3_sop)
plot(ggpredict(m2.3.3_sop, terms = c("protection70","EIVEres.T"))) # we will use this variant of visualization for the plots

# Diagnostics T 70% threshold--------
resid <- residuals(m2.3.3_sop)
fitted <- fitted(m2.3.3_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.3_Temperature70_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Temperature 60% threshold ---------------
m2.3.4_sop <- lmer(data = summary_gen_traits60, logratio ~ protection60 * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.4_sop) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3.4_sop)
plot(ggpredict(m2.3.4_sop, terms = c("protection60","EIVEres.T"))) # we will use this variant of visualization for the plots

# Diagnostics T 60% threshold--------
resid <- residuals(m2.3.4_sop)
fitted <- fitted(m2.3.4_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.4_Temperature60_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Temperature 50% threshold ---------------
m2.3.5_sop <- lmer(data = summary_gen_traits50, logratio ~ protection50 * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.5_sop) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3.5_sop)
plot(ggpredict(m2.3.5_sop, terms = c("protection50","EIVEres.T"))) # we will use this variant of visualization for the plots

# Diagnostics T 50% threshold--------
resid <- residuals(m2.3.5_sop)
fitted <- fitted(m2.3.5_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.5_Temperature50_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Moisture -------------------

m2.3_sop <- lmer(data = summary_gen_traits, logratio ~ protection_cat*EIVEres.M +mean_OP_T1  + (1|TaxonName))
summary(m2.3_sop) # no interaction with moisture but significant effect of EIVEres.M and marginal significant effect for "protected" alone
plot(m2.3_sop)
plot(ggpredict(m2.3_sop, terms = c("protection_cat","EIVEres.M"))) # we will use this variant of visualization for the plots

# Diagnostics M --------
resid <- residuals(m2.3_sop)
fitted <- fitted(m2.3_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3_Moisture_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Moisture 90% threshold ---------------
m2.3.1_sop <- lmer(data = summary_gen_traits90, logratio ~ protection90 * EIVEres.M +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.1_sop) # no interaction with moisture but significant effect of EIVEres.M and marginal significant effect for "protected" alone
plot(m2.3.1_sop)
plot(ggpredict(m2.3.1_sop, terms = c("protection90","EIVEres.M"))) # we will use this variant of visualization for the plots

# Diagnostics M 90% threshold--------
resid <- residuals(m2.3.1_sop)
fitted <- fitted(m2.3.1_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.1_Moisture90_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Moisture 80% threshold ---------------

m2.3.2_sop <- lmer(data = summary_gen_traits80, logratio ~ protection80 * EIVEres.M +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.2_sop) # no interaction with moisture but significant effect of EIVEres.M 
plot(m2.3.2_sop)
plot(ggpredict(m2.3.2_sop, terms = c("protection80","EIVEres.M"))) # we will use this variant of visualization for the plots

# Diagnostics M 80% threshold--------
resid <- residuals(m2.3.2_sop)
fitted <- fitted(m2.3.2_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.2_Moisture80_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Moisture 70% threshold ---------------
m2.3.3_sop <- lmer(data = summary_gen_traits70, logratio ~ protection70 * EIVEres.M +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.3_sop) # no interaction with moisture but marginal significant effect of EIVEres.M alone
plot(m2.3.3_sop)
plot(ggpredict(m2.3.3_sop, terms = c("protection70","EIVEres.M"))) # we will use this variant of visualization for the plots

# Diagnostics M 70% threshold--------
resid <- residuals(m2.3.3_sop)
fitted <- fitted(m2.3.3_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.3_Moisture70_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Moisture 60% threshold ---------------
m2.3.4_sop <- lmer(data = summary_gen_traits60, logratio ~ protection60 * EIVEres.M +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.4_sop) # no interaction with moisture but marginal significant effect of EIVEres.M alone
plot(m2.3.4_sop)
plot(ggpredict(m2.3.4_sop, terms = c("protection60","EIVEres.M"))) # we will use this variant of visualization for the plots

# Diagnostics M 60% threshold--------
resid <- residuals(m2.3.4_sop)
fitted <- fitted(m2.3.4_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.4_Moisture60_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Moisture 50% threshold ---------------
m2.3.5_sop <- lmer(data = summary_gen_traits50, logratio ~ protection50 * EIVEres.M +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.5_sop) # marginal significant nteraction with moisture and marginal significant effect of EIVEres.M and significant effect for "protected" alone
plot(m2.3.5_sop)
plot(ggpredict(m2.3.5_sop, terms = c("protection50","EIVEres.M"))) # we will use this variant of visualization for the plots

# Diagnostics M 50% threshold--------
resid <- residuals(m2.3.5_sop)
fitted <- fitted(m2.3.5_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.5_Moisture50_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Light Preference -------------------
m2.4_sop <- lmer(data = summary_gen_traits, logratio ~ protection_cat*EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.4_sop)
plot(m2.4_sop)
plot(ggpredict(m2.4_sop, terms = c("protection_cat","EIVEres.L"))) # we will use this variant of visualization for the plots

# Diagnostics L --------
resid <- residuals(m2.4_sop)
fitted <- fitted(m2.4_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.4_Light_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Light Preference 90% threshold ---------------
m2.4.1_sop <- lmer(data = summary_gen_traits90, logratio ~ protection90 * EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.4.1_sop)
plot(m2.4.1_sop)
plot(ggpredict(m2.4.1_sop, terms = c("protection90","EIVEres.L"))) # we will use this variant of visualization for the plots

# Diagnostics L 90% threshold--------
resid <- residuals(m2.4.1_sop)
fitted <- fitted(m2.4.1_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.4.1_Light90.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Light Preference 80% threshold ---------------
m2.4.2_sop <- lmer(data = summary_gen_traits80, logratio ~ protection80 * EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.4.2_sop)
plot(m2.4.2_sop)
plot(ggpredict(m2.4.2_sop, terms = c("protection80","EIVEres.L"))) # we will use this variant of visualization for the plots
# Diagnostics L 80% threshold--------
resid <- residuals(m2.4.2_sop)
fitted <- fitted(m2.4.2_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.4.2_Light80_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Light Preference 70% threshold ---------------
m2.4.3_sop <- lmer(data = summary_gen_traits70, logratio ~ protection70 * EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.4.3_sop)
plot(m2.4.3_sop)
plot(ggpredict(m2.4.3_sop, terms = c("protection70","EIVEres.L"))) # we will use this variant of visualization for the plots
# Diagnostics L 70% threshold--------
resid <- residuals(m2.4.3_sop)
fitted <- fitted(m2.4.3_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.4.3_Light70_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Light Preference 60% threshold ---------------
m2.4.4_sop <- lmer(data = summary_gen_traits60, logratio ~ protection60 * EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.4.4_sop)
plot(m2.4.4_sop)
plot(ggpredict(m2.4.4_sop, terms = c("protection60","EIVEres.L"))) # we will use this variant of visualization for the plots
# Diagnostics L 60% threshold--------
resid <- residuals(m2.4.4_sop)
fitted <- fitted(m2.4.4_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.4.4_Light60_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Light Preference 50% threshold ---------------
m2.4.5_sop <- lmer(data = summary_gen_traits50, logratio ~ protection50 * EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.4.5_sop)
plot(m2.4.5_sop)
plot(ggpredict(m2.4.5_sop, terms = c("protection50","EIVEres.L"))) # we will use this variant of visualization for the plots

# Diagnostics L 50% threshold--------
resid <- residuals(m2.4.5_sop)
fitted <- fitted(m2.4.5_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.4.5_Light50_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Forest affiliation -------------------

m2.5_sop <- lmer(data = summary_gen_traits, logratio ~ protection_cat*main_group +  mean_OP_T1 + (1|TaxonName))

summary(m2.5_sop) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.5_sop)
plot(m2.5_sop)
plot(ggpredict(m2.5_sop, terms = c("protection_cat","main_group")))


# Diagnostics Forest affiliation --------
resid <- residuals(m2.5_sop)
fitted <- fitted(m2.5_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.5_ForestAffiliation_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Forest affiliation 90% threshold ---------------
m2.5.1_sop <- lmer(data = summary_gen_traits90, logratio ~ protection90 * main_group +  mean_OP_T1 + (1|TaxonName))
summary(m2.5.1_sop) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.5.1_sop)
plot(m2.5.1_sop)
plot(ggpredict(m2.5.1_sop, terms = c("protection90","main_group")))

# Diagnostics Forest affiliation 90% threshold--------
resid <- residuals(m2.5.1_sop)
fitted <- fitted(m2.5.1_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.5.1_ForestAffiliation90.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Forest affiliation 80% threshold ---------------
m2.5.2_sop <- lmer(data = summary_gen_traits80, logratio ~ protection80 * main_group +  mean_OP_T1 + (1|TaxonName))
summary(m2.5.2_sop) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.5.2_sop)
plot(m2.5.2_sop)
plot(ggpredict(m2.5.2, terms = c("protection80","main_group")))

# Diagnostics Forest affiliation 80% threshold--------
resid <- residuals(m2.5.2_sop)
fitted <- fitted(m2.5.2_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.5.2_ForestAffiliation80_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()



# Forest affiliation 70% threshold ---------------
m2.5.3_sop <- lmer(data = summary_gen_traits70, logratio ~ protection70 * main_group +  mean_OP_T1 + (1|TaxonName))
summary(m2.5.3_sop) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.5.3_sop)
plot(m2.5.3_sop)
plot(ggpredict(m2.5.3_sop, terms = c("protection70","main_group")))
# Diagnostics Forest affiliation 70% threshold--------
resid <- residuals(m2.5.3_sop)
fitted <- fitted(m2.5.3_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.5.3_ForestAffiliation70_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Forest affiliation 60% threshold ---------------
m2.5.4_sop <- lmer(data = summary_gen_traits60, logratio ~ protection60 * main_group +  mean_OP_T1 + (1|TaxonName))
summary(m2.5.4_sop) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.5.4_sop)
plot(m2.5.4_sop)
plot(ggpredict(m2.5.4_sop, terms = c("protection60","main_group")))

# Diagnostics Forest affiliation 60% threshold--------
resid <- residuals(m2.5.4_sop)
fitted <- fitted(m2.5.4_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.5.4_ForestAffiliation60_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Forest affiliation 50% threshold ---------------
m2.5.5_sop <- lmer(data = summary_gen_traits50, logratio ~ protection50 * main_group +  mean_OP_T1 + (1|TaxonName))
summary(m2.5.5_sop) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.5.5_sop)
plot(m2.5.5_sop)
plot(ggpredict(m2.5.5_sop, terms = c("protection50","main_group")))

# Diagnostics Forest affiliation 50% threshold--------
resid <- residuals(m2.5.5_sop)
fitted <- fitted(m2.5.5_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.5.5_ForestAffiliation50_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()



# Plant Growth Form -------------------
m2.6_sop <- lmer(data = summary_gen_traits, logratio ~ protection_cat*PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))

summary(m2.6_sop)
anova(m2.6_sop)
plot(m2.6_sop)
plot(ggpredict(m2.6_sop, terms = c("PlantGrowthForm","protection_cat")))

# Diagnostics Plant Growth Form --------
resid <- residuals(m2.6_sop)
fitted <- fitted(m2.6_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6_PlantGrowthForm_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Plant Growth Form 90% threshold ---------------
m2.6.1_sop <- lmer(data = summary_gen_traits90, logratio ~ protection90 * PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))
summary(m2.6.1_sop)
anova(m2.6.1_sop)
plot(m2.6.1_sop)
plot(ggpredict(m2.6.1_sop, terms = c("PlantGrowthForm","protection90")))

# Diagnostics Plant Growth Form 90% threshold--------
resid <- residuals(m2.6.1_sop)
fitted <- fitted(m2.6.1_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6.1_PlantGrowthForm90_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Plant Growth Form 80% threshold ---------------
m2.6.2_sop <- lmer(data = summary_gen_traits80, logratio ~ protection80 * PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))
summary(m2.6.2_sop)
anova(m2.6.2_sop)
plot(m2.6.2_sop)
plot(ggpredict(m2.6.2_sop, terms = c("PlantGrowthForm","protection80")))
# Diagnostics Plant Growth Form 80% threshold--------
resid <- residuals(m2.6.2_sop)
fitted <- fitted(m2.6.2_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6.2_PlantGrowthForm80_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Plant Growth Form 70% threshold ---------------
m2.6.3_sop <- lmer(data = summary_gen_traits70, logratio ~ protection70 * PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))
summary(m2.6.3_sop)
anova(m2.6.3_sop)
plot(m2.6.3_sop)
plot(ggpredict(m2.6.3_sop, terms = c("PlantGrowthForm","protection70")))
# Diagnostics Plant Growth Form 70% threshold--------
resid <- residuals(m2.6.3_sop)
fitted <- fitted(m2.6.3_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6.3_PlantGrowthForm70_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Plant Growth Form 60% threshold ---------------
m2.6.4_sop <- lmer(data = summary_gen_traits60, logratio ~ protection60 * PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))
summary(m2.6.4_sop)
anova(m2.6.4_sop)
plot(m2.6.4_sop)
plot(ggpredict(m2.6.4_sop, terms = c("PlantGrowthForm","protection60")))

# Diagnostics Plant Growth Form 60% threshold--------
resid <- residuals(m2.6.4_sop)
fitted <- fitted(m2.6.4_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6.4_PlantGrowthForm60_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Plant Growth Form 50% threshold ---------------
m2.6.5_sop <- lmer(data = summary_gen_traits50, logratio ~ protection50 * PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))
summary(m2.6.5_sop)
anova(m2.6.5_sop)
plot(m2.6.5_sop)
plot(ggpredict(m2.6.5_sop, terms = c("PlantGrowthForm","protection50")))

# Diagnostics Plant Growth Form 50% threshold--------
resid <- residuals(m2.6.5_sop)
fitted <- fitted(m2.6.5_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6.5_PlantGrowthForm50_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()



# Woodiness -------------------

m2.7_sop <- lmer(data = summary_gen_traits, logratio ~ protection_cat*Woodiness+  mean_OP_T1 + (1|TaxonName))

summary(m2.7_sop)
anova(m2.7_sop)
plot(m2.7_sop)
plot(ggpredict(m2.7_sop, terms = c("protection_cat","Woodiness")))
preds <- ggpredict(m2.7_sop, terms = c("protection_cat","Woodiness"))

# Diagnostics Woodiness --------
resid <- residuals(m2.7_sop)
fitted <- fitted(m2.7_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7_Woodiness_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Woodiness 90% threshold ---------------
m2.7.1_sop <- lmer(data = summary_gen_traits90, logratio ~ protection90 * Woodiness +  mean_OP_T1 + (1|TaxonName))
summary(m2.7.1_sop)
anova(m2.7.1_sop)
plot(m2.7.1_sop)
plot(ggpredict(m2.7.1_sop, terms = c("protection90","Woodiness")))

# Diagnostics Woodiness 90% threshold--------
resid <- residuals(m2.7.1_sop)
fitted <- fitted(m2.7.1_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7.1_Woodiness90_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Woodiness 80% threshold ---------------
m2.7.2_sop <- lmer(data = summary_gen_traits80, logratio ~ protection80 * Woodiness +  mean_OP_T1 + (1|TaxonName))
summary(m2.7.2_sop)
anova(m2.7.2_sop)
plot(m2.7.2_sop)
plot(ggpredict(m2.7.2_sop, terms = c("protection80","Woodiness")))

# Diagnostics Woodiness 80% threshold--------
resid <- residuals(m2.7.2_sop)
fitted <- fitted(m2.7.2_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7.2_Woodiness80_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Woodiness 70% threshold ---------------
m2.7.3_sop <- lmer(data = summary_gen_traits70, logratio ~ protection70 * Woodiness +  mean_OP_T1 + (1|TaxonName))
summary(m2.7.3_sop)
anova(m2.7.3_sop)
plot(m2.7.3_sop)
plot(ggpredict(m2.7.3_sop, terms = c("protection70","Woodiness")))

# Diagnostics Woodiness 70% threshold--------
resid <- residuals(m2.7.3_sop)
fitted <- fitted(m2.7.3_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7.3_Woodiness70_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Woodiness 60% threshold ---------------
m2.7.4_sop <- lmer(data = summary_gen_traits60, logratio ~ protection60 * Woodiness +  mean_OP_T1 + (1|TaxonName))
summary(m2.7.4_sop)
anova(m2.7.4_sop)
plot(m2.7.4_sop)
plot(ggpredict(m2.7.4_sop, terms = c("protection60","Woodiness")))

# Diagnostics Woodiness 60% threshold--------
resid <- residuals(m2.7.4_sop)
fitted <- fitted(m2.7.4_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7.4_Woodiness60_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Woodiness 50% threshold ---------------
m2.7.5_sop <- lmer(data = summary_gen_traits50, logratio ~ protection50 * Woodiness +  mean_OP_T1 + (1|TaxonName))
summary(m2.7.5_sop)
anova(m2.7.5_sop)
plot(m2.7.5_sop)
plot(ggpredict(m2.7.5_sop, terms = c("protection50","Woodiness")))

# Diagnostics Woodiness 50% threshold--------
resid <- residuals(m2.7.5_sop)
fitted <- fitted(m2.7.5_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7.5_Woodiness50_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Leaf Type -----------------

m2.8_sop <- lmer(data = summary_gen_traits, logratio ~ protection_cat*LeafType+  mean_OP_T1 + (1|TaxonName))

summary(m2.8_sop)
anova(m2.8_sop)
plot(m2.8_sop)
plot(ggpredict(m2.8_sop, terms = c("LeafType","protection_cat")))

# Diagnostics Leaf Type --------
resid <- residuals(m2.8_sop)
fitted <- fitted(m2.8_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8_LeafType_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Leaf Type 90% threshold ---------------
m2.8.1_sop <- lmer(data = summary_gen_traits90, logratio ~ protection90 * LeafType +  mean_OP_T1 + (1|TaxonName))
summary(m2.8.1_sop)
anova(m2.8.1_sop)
plot(m2.8.1_sop)
plot(ggpredict(m2.8.1_sop, terms = c("LeafType","protection90")))
# Diagnostics Leaf Type 90% threshold--------
resid <- residuals(m2.8.1_sop)
fitted <- fitted(m2.8.1_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8.1_LeafType90_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Leaf Type 80% threshold ---------------
m2.8.2_sop <- lmer(data = summary_gen_traits80, logratio ~ protection80 * LeafType +  mean_OP_T1 + (1|TaxonName))
summary(m2.8.2_sop)
anova(m2.8.2_sop)
plot(m2.8.2_sop)
plot(ggpredict(m2.8.2_sop, terms = c("LeafType","protection80")))

# Diagnostics Leaf Type 80% threshold--------
resid <- residuals(m2.8.2_sop)
fitted <- fitted(m2.8.2_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8.2_LeafType80_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()


# Leaf Type 70% threshold ---------------
m2.8.3_sop <- lmer(data = summary_gen_traits70, logratio ~ protection70 * LeafType +  mean_OP_T1 + (1|TaxonName))
summary(m2.8.3_sop)
anova(m2.8.3_sop)
plot(m2.8.3_sop)
plot(ggpredict(m2.8.3_sop, terms = c("LeafType","protection70")))

# Diagnostics Leaf Type 70% threshold--------
resid <- residuals(m2.8.3_sop)
fitted <- fitted(m2.8.3_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8.3_LeafType70_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Leaf Type 60% threshold ---------------
m2.8.4_sop <- lmer(data = summary_gen_traits60, logratio ~ protection60 * LeafType +  mean_OP_T1 + (1|TaxonName))
summary(m2.8.4_sop)
anova(m2.8.4_sop)
plot(m2.8.4_sop)
plot(ggpredict(m2.8.4_sop, terms = c("LeafType","protection60")))

# Diagnostics Leaf Type 60% threshold--------
resid <- residuals(m2.8.4_sop)
fitted <- fitted(m2.8.4_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8.4_LeafType60_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()

# Leaf Type 50% threshold ---------------
m2.8.5_sop <- lmer(data = summary_gen_traits50, logratio ~ protection50 * LeafType +  mean_OP_T1 + (1|TaxonName))
summary(m2.8.5_sop)
anova(m2.8.5_sop)
plot(m2.8.5_sop)
plot(ggpredict(m2.8.5_sop, terms = c("LeafType","protection50")))

# Diagnostics Leaf Type 50% threshold--------
resid <- residuals(m2.8.5_sop)
fitted <- fitted(m2.8.5_sop)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8.5_LeafType50_sop.png", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
# Plot 1: Residuals vs. Fitted
plot(fitted, resid, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "steelblue")
abline(h = 0, col = "red")
# Plot 2: Q-Q Plot
qqnorm(resid, main = "Normal Q-Q", pch = 16, col = "steelblue")
qqline(resid, col = "red")
# Plot 3: Histogram of Residuals
hist(resid, breaks = 40, main = "Histogram of Residuals", col = "lightgray",
     xlab = "Residuals")
# Plot 4: Scale-Location (sqrt(|residuals|) vs fitted)
plot(fitted, sqrt(abs(resid)),
     xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location", pch = 16, col = "steelblue")
dev.off()