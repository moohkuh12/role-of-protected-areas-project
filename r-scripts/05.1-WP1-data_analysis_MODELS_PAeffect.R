# Load setup and libraries
source("./r-scripts/00-preamble.R")

# This script is used to prepare the data for the analysis of the protection effect 
# on species occurrence in the sMon dataset. After aggregation of the dataset at species and protection level, this script includes
# the models, summaries and diagnostics at the Germany scale. 
#Another script with the corresponding plots for the models is prepared seperately.

# Load dataframe
#sMon <- read_csv("./data/sMon/sMon_long.csv", col_names = TRUE)

# Long format of sMon data 
sMon_wide <- read_csv("./data/sMon/sMon_wide_220725.csv")
smon_filtered <- sMon_wide %>%
  filter(urban_class != "urban")
rm(sMon_wide)


# Load the adapted euforplants data
#euforplants <- read.csv("./data/landcover_analysis/euforplants_summary_new.csv")
#euforplants2 <- read.csv("./data/landcover_analysis/euforplants_summary_new_montane2506.csv")

# Load trait df or create it
traits_df <- read_csv("./data/sMon/sMon_traits.csv")

traits_df <- smon_filtered%>%
  select(TaxonName, main_group, main_group2, EIVEres.M, EIVEres.M.nw3,EIVEres.N, EIVEres.N.nw3, EIVEres.L, EIVEres.L.nw3, EIVEres.T, EIVEres.T.nw3, EIVEres.R, EIVEres.R.nw3, Family, PlantGrowthForm, Woodiness, LeafType, LeafPhenology ) %>%
  distinct()

# Protection_cat will be our used protection defonition (besides the other threshholds)
# Define protection categories based on cov_frac
smon_filtered<- smon_filtered%>%
  mutate(protection_cat = case_when(
    cov_frac <= 0.005 ~ "not protected",
    cov_frac < 0.9    ~ "part protected",
    TRUE              ~ "protected"
  )) %>%
  mutate(protection_cat = factor(protection_cat, levels = c("not protected", "part protected", "protected")))


# add new euforplants traits
#traits_df<- traits_df %>%
 # select(-main_group) %>%
 # left_join(
#    euforplants %>% select(wcvp_name, main_group),
 #   by = c("TaxonName" = "wcvp_name")
 # )
#traits_df<- traits_df %>%
#  left_join(
 #   euforplants2 %>% select(wcvp_name, main_group2),
 #   by = c("TaxonName" = "wcvp_name")
 # )

###############################
###############################

# Create aggregated dataframe for the analysis ------------
# (aggregation at species and protection level)

# Aggregate data by protection status per species 
# summary general --------------------------
# will be our main dataframe for the models 
total_cells_per_species <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  group_by(TaxonName) %>%
  summarise(total_cells = n(), .groups = "drop")

summary_general <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  mutate(occ_change = OP_T3 - OP_T1) %>%
  group_by(TaxonName, protection_cat) %>%
  summarise( # mean_occ_change and logratio (of SOP) will be our main predictors in the models
    SOP_T1 = sum(OP_T1, na.rm = TRUE),
    SOP_T3 = sum(OP_T3, na.rm = TRUE),
    mean_occ_change = mean(occ_change, na.rm = TRUE),
    mean_OP_T1 = mean(OP_T1, na.rm = TRUE), # this is our covariate - the initial mean occurence probability
    n_cells = n(),
    .groups = "drop"
  ) %>%
  left_join(total_cells_per_species, by = "TaxonName") %>%
  mutate(
    delta_SOP = SOP_T3 - SOP_T1,
    rel_change = (SOP_T3 - SOP_T1) / SOP_T1 * 100,
    logratio = log((SOP_T3 + 0.01)/(SOP_T1 + 0.01))
  ) %>%
  ungroup()

summary_range_protected <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  mutate(occ_change = OP_T3 - OP_T1,
         protected_binary = protection_cat == "protected") %>%
  group_by(TaxonName) %>%
  summarise(
    SOP_T1 = sum(OP_T1, na.rm = TRUE),
    SOP_T3 = sum(OP_T3, na.rm = TRUE),
    mean_occ_change = mean(occ_change, na.rm = TRUE),
    mean_OP_T1 = mean(OP_T1, na.rm = TRUE),
    n_cells = n(),
    protected_cells_binary = sum(protected_binary),
    delta_SOP = SOP_T3 - SOP_T1,
    rel_change = (SOP_T3 - SOP_T1) / SOP_T1 * 100,
    logratio = log((SOP_T3 + 0.01)/(SOP_T1 + 0.01)),
    percent_range_protected = protected_cells_binary / n_cells,
    .groups = "drop"
  )


# These are the same summarys for the other thresholds - these can be used for testing other threshholds for protection 
# sensitivity analysis ------------------------
summary_prot90 <- smon_filtered %>%
  filter(OP_T1 > 0) %>%
  mutate(occ_change = OP_T3 - OP_T1) %>%
  group_by(TaxonName, protection90) %>%
  summarise(
    SOP_T1 = sum(OP_T1, na.rm = TRUE),
    SOP_T3 = sum(OP_T3, na.rm = TRUE),
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
    mean_occ_change = mean(occ_change, na.rm = TRUE),
    mean_OP_T1 = mean(OP_T1, na.rm = TRUE),  
    n_cells = n()) %>%
  mutate(
    delta_SOP = SOP_T3 - SOP_T1,
    rel_change = (SOP_T3 - SOP_T1) / SOP_T1 * 100,
    logratio = log((SOP_T3 + 0.01)/(SOP_T1 + 0.01))
  )

# 1. Linear mixed effect models - mean_occ_change --------------------------

# Linear mixed model
m1 <- lmer(data = summary_general,
             mean_occ_change ~ protection_cat +  mean_OP_T1 + (1|TaxonName))

summary(m1) # no significant effects of protection_cat, but significant effect of mean_OP_T1

plot(ggpredict(m1, terms = "protection_cat"))

# 90% threshold - protected means that at least 90% of a cell is covered by protected areas, not protected is everything beneath 10%
m1.1 <- lmer(data = summary_prot90, mean_occ_change ~ protection90 +  mean_OP_T1 + (1|TaxonName))
summary(m1.1) # no significant effects of protection_cat, but significant effect of mean_OP_T1
plot(ggpredict(m1.1, terms = "protection90"))

# 80% threshold - protected means that at least 80% of a cell is covered by protected areas, not protected is everything beneath 10%
m1.2 <- lmer(data = summary_prot80, mean_occ_change ~ protection80 +  mean_OP_T1 + (1|TaxonName))
summary(m1.2) # no significant effects of protection_cat, but significant effect of mean_OP_T1
plot(ggpredict(m1.2, terms = "protection80"))

# 70% threshold - protected means that at least 70% of a cell is covered by protected areas, not protected is everything beneath 10%
m1.3 <- lmer(data = summary_prot70, mean_occ_change ~ protection70 +  mean_OP_T1 + (1|TaxonName))
summary(m1.3) # no significant effects of protection_cat, but significant effect of mean_OP_T1
plot(ggpredict(m1.3, terms = "protection70"))

# 60% threshold - protected means that at least 60% of a cell is covered by protected areas, not protected is everything beneath 10%
m1.4 <- lmer(data = summary_prot60, mean_occ_change ~ protection60 +  mean_OP_T1 + (1|TaxonName))
summary(m1.4) # no significant effects of protection_cat, but significant effect of mean_OP_T1
plot(ggpredict(m1.4, terms = "protection60"))

# 50% threshold - protected means that at least 50% of a cell is covered by protected areas, not protected is everything beneath 10%
m1.5 <- lmer(data = summary_prot50, mean_occ_change ~ protection50 +  mean_OP_T1 + (1|TaxonName))
summary(m1.5) # no significant effects of protection_cat, but significant effect of mean_OP_T1
plot(ggpredict(m1.5, terms = "protection50"))


#####################
# Model diagnostics
####################
resid <- residuals(m1)
fitted <- fitted(m1)
qq <- qqnorm(resid, plot.it = FALSE)

# 2. PNG-file
#png("./figures/diagnostic_plots_m1_mean_occ_change.png", width = 1000, height = 1000)
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
# Linear mixed effect models - mean_occ_change with interactions --------------------------

# First, we need to merge the traits_df with the summary_general and the other threshold-summaries dataframes
summary_gen_traits <- left_join(summary_general, traits_df, by = "TaxonName")
summary_gen_traits90 <- left_join(summary_prot90, traits_df, by = "TaxonName")
summary_gen_traits80 <- left_join(summary_prot80, traits_df, by = "TaxonName")
summary_gen_traits70 <- left_join(summary_prot70, traits_df, by = "TaxonName")
summary_gen_traits60 <- left_join(summary_prot60, traits_df, by = "TaxonName")
summary_gen_traits50 <- left_join(summary_prot50, traits_df, by = "TaxonName")


# Nitrogen  ---------------
m2.1 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1)
plot(ggpredict(m2.1, terms = c("protection_cat","EIVEres.N"))) # we will use this variant of visualization for the plots
plot(ggpredict(m2.1, terms = c("EIVEres.N", "protection_cat"))) # this doesnt fit

# Nitrogen 90% threshold ---------------
m2.1.1 <- lmer(data = summary_gen_traits90, mean_occ_change ~ protection90 * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1.1) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1.1)
plot(ggpredict(m2.1.1, terms = c("protection90","EIVEres.N"))) # we will use this variant of visualization for the plots

# Nitrogen 80% threshold ---------------
m2.1.2 <- lmer(data = summary_gen_traits80, mean_occ_change ~ protection80 * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1.2) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1.2)
plot(ggpredict(m2.1.2, terms = c("protection80","EIVEres.N"))) # we will use this variant of visualization for the plots

# Nitrogen 70% threshold ---------------
m2.1.3 <- lmer(data = summary_gen_traits70, mean_occ_change ~ protection70 * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1.3) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1.3)
plot(ggpredict(m2.1.3, terms = c("protection70","EIVEres.N"))) # we will use this variant of visualization for the plots

# Nitrogen 60% threshold ---------------
m2.1.4 <- lmer(data = summary_gen_traits60, mean_occ_change ~ protection60 * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1.4) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1.4)
plot(ggpredict(m2.1.4, terms = c("protection60","EIVEres.N"))) # we will use this variant of visualization for the plots

# Nitrogen 50% threshold ---------------
m2.1.5 <- lmer(data = summary_gen_traits50, mean_occ_change ~ protection50 * EIVEres.N +  mean_OP_T1 + (1|TaxonName))
summary(m2.1.5) # significant negative interaction between protection_cat and EIVEres.N
plot(m2.1.5)
plot(ggpredict(m2.1.5, terms = c("protection50","EIVEres.N"))) # we will use this variant of visualization for the plots


# Diagnostics N --------
resid <- residuals(m2.1)
fitted <- fitted(m2.1)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1_Nitrogen060625.png", width = 1000, height = 1000)
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

# Diagnostics N 90% threshold --------
resid <- residuals(m2.1.1)
fitted <- fitted(m2.1.1)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1.1_Nitrogen90.png", width = 1000, height = 1000)
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



# Diagnostics N 80% threshold --------
resid <- residuals(m2.1.2)
fitted <- fitted(m2.1.2)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1.2_Nitrogen80.png", width = 1000, height = 1000)
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



# Diagnostics N 70% threshold --------
resid <- residuals(m2.1.3)
fitted <- fitted(m2.1.3)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1.2_Nitrogen70.png", width = 1000, height = 1000)
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



# Diagnostics N 60% threshold --------
resid <- residuals(m2.1.4)
fitted <- fitted(m2.1.4)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1.2_Nitrogen60.png", width = 1000, height = 1000)
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



# Diagnostics N 50% threshold --------
resid <- residuals(m2.1.5)
fitted <- fitted(m2.1.5)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.1.2_Nitrogen50.png", width = 1000, height = 1000)
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
m2.2 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2)
plot(ggpredict(m2.2, terms = c("protection_cat","EIVEres.R"))) # we will use this variant of visualization for the plots

# Ph - Reaction 90% threshold ---------------
m2.2.1 <- lmer(data = summary_gen_traits90, mean_occ_change ~ protection90 * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2.1) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2.1)
plot(ggpredict(m2.2.1, terms = c("protection90","EIVEres.R"))) # we will use this variant of visualization for the plots

# Ph - Reaction 80% threshold ---------------
m2.2.2 <- lmer(data = summary_gen_traits80, mean_occ_change ~ protection80 * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2.2) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2.2)
plot(ggpredict(m2.2.2, terms = c("protection80","EIVEres.R"))) # we will use this variant of visualization for the plots

# Ph - Reaction 70% threshold ---------------
m2.2.3 <- lmer(data = summary_gen_traits70, mean_occ_change ~ protection70 * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2.3) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2.3)
plot(ggpredict(m2.2.3, terms = c("protection70","EIVEres.R"))) # we will use this variant of visualization for the plots

# Ph - Reaction 60% threshold ---------------
m2.2.4 <- lmer(data = summary_gen_traits60, mean_occ_change ~ protection60 * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2.4) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2.4)
plot(ggpredict(m2.2.4, terms = c("protection60","EIVEres.R"))) # we will use this variant of visualization for the plots

# Ph - Reaction 50% threshold ---------------
m2.2.5 <- lmer(data = summary_gen_traits50, mean_occ_change ~ protection50 * EIVEres.R +  mean_OP_T1 + (1|TaxonName))
summary(m2.2.5) # no interaction between protection_cat and EIVEres.R but significant effect of EIVEres.R alone
plot(m2.2.5)
plot(ggpredict(m2.2.5, terms = c("protection50","EIVEres.R"))) # we will use this variant of visualization for the plots


# Diagnostics R --------
resid <- residuals(m2.2)
fitted <- fitted(m2.2)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.2_pH060625.png", width = 1000, height = 1000)
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

# Diagnostics R 90% threshold --------
resid <- residuals(m2.2.1)
fitted <- fitted(m2.2.1)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.2.1_pH90.png", width = 1000, height = 1000)
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



# Diagnostics R 80% threshold --------
resid <- residuals(m2.2.2)
fitted <- fitted(m2.2.2)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.2.2_pH80.png", width = 1000, height = 1000)
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



# Diagnostics R 70% threshold --------
resid <- residuals(m2.2.3)
fitted <- fitted(m2.2.3)
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



# Diagnostics R 60% threshold --------
resid <- residuals(m2.2.4)
fitted <- fitted(m2.2.4)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.2.4_pH60.png", width = 1000, height = 1000)
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



# Diagnostics R 50% threshold --------
resid <- residuals(m2.2.5)
fitted <- fitted(m2.2.5)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.2.5_pH50.png", width = 1000, height = 1000)
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
m2.3 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3)
plot(ggpredict(m2.3, terms = c("protection_cat","EIVEres.T"))) # we will use this variant of visualization for the plots

# Temperature 90% threshold ---------------
m2.3.1 <- lmer(data = summary_gen_traits90, mean_occ_change ~ protection90 * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.1) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3.1)
plot(ggpredict(m2.3.1, terms = c("protection90","EIVEres.T"))) # we will use this variant of visualization for the plots

# Temperature 80% threshold ---------------
m2.3.2 <- lmer(data = summary_gen_traits80, mean_occ_change ~ protection80 * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.2) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3.2)
plot(ggpredict(m2.3.2, terms = c("protection80","EIVEres.T"))) # we will use this variant of visualization for the plots

# Temperature 70% threshold ---------------
m2.3.3 <- lmer(data = summary_gen_traits70, mean_occ_change ~ protection70 * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.3) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3.3)
plot(ggpredict(m2.3.3, terms = c("protection70","EIVEres.T"))) # we will use this variant of visualization for the plots

# Temperature 60% threshold ---------------
m2.3.4 <- lmer(data = summary_gen_traits60, mean_occ_change ~ protection60 * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.4) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3.4)
plot(ggpredict(m2.3.4, terms = c("protection60","EIVEres.T"))) # we will use this variant of visualization for the plots

# Temperature 50% threshold ---------------
m2.3.5 <- lmer(data = summary_gen_traits50, mean_occ_change ~ protection50 * EIVEres.T +  mean_OP_T1 + (1|TaxonName))
summary(m2.3.5) # negative interaction between protection_cat and EIVEres.T and significant effect of EIVEres.T and "protected" alone
plot(m2.3.5)
plot(ggpredict(m2.3.5, terms = c("protection50","EIVEres.T"))) # we will use this variant of visualization for the plots


# Diagnostics T --------
resid <- residuals(m2.3)
fitted <- fitted(m2.3)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3_Temperature060625.png", width = 1000, height = 1000)
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

# Diagnostics T 90% threshhold--------
resid <- residuals(m2.3.1)
fitted <- fitted(m2.3.1)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.1_Temperature90.png", width = 1000, height = 1000)
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


# Diagnostics T 80% threshold--------
resid <- residuals(m2.3.2)
fitted <- fitted(m2.3.2)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.2_Temperature80.png", width = 1000, height = 1000)
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


# Diagnostics T 70% threshold--------
resid <- residuals(m2.3.3)
fitted <- fitted(m2.3.3)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.3_Temperature70.png", width = 1000, height = 1000)
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


# Diagnostics T 60% threshold--------
resid <- residuals(m2.3.4)
fitted <- fitted(m2.3.4)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.4_Temperature60.png", width = 1000, height = 1000)
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


# Diagnostics T 50% threshold--------
resid <- residuals(m2.3.5)
fitted <- fitted(m2.3.5)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3.5_Temperature50.png", width = 1000, height = 1000)
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

m2.4 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat*EIVEres.M +mean_OP_T1  + (1|TaxonName))
summary(m2.4) # no interaction with moisture but significant effect of EIVEres.M and marginal significant effect for "protected" alone
plot(m2.4)
plot(ggpredict(m2.4, terms = c("protection_cat","EIVEres.M"))) # we will use this variant of visualization for the plots

# Moisture 90% threshold ---------------
m2.4.1 <- lmer(data = summary_gen_traits90, mean_occ_change ~ protection90 * EIVEres.M +  mean_OP_T1 + (1|TaxonName))
summary(m2.4.1) # no interaction with moisture but significant effect of EIVEres.M and marginal significant effect for "protected" alone
plot(m2.4.1)
plot(ggpredict(m2.4.1, terms = c("protection90","EIVEres.M"))) # we will use this variant of visualization for the plots

# Moisture 80% threshold ---------------
m2.4.2 <- lmer(data = summary_gen_traits80, mean_occ_change ~ protection80 * EIVEres.M +  mean_OP_T1 + (1|TaxonName))
summary(m2.4.2) # no interaction with moisture but significant effect of EIVEres.M 
plot(m2.4.2)
plot(ggpredict(m2.4.2, terms = c("protection80","EIVEres.M"))) # we will use this variant of visualization for the plots

# Moisture 70% threshold ---------------
m2.4.3 <- lmer(data = summary_gen_traits70, mean_occ_change ~ protection70 * EIVEres.M +  mean_OP_T1 + (1|TaxonName))
summary(m2.4.3) # no interaction with moisture but marginal significant effect of EIVEres.M alone
plot(m2.4.3)
plot(ggpredict(m2.4.3, terms = c("protection70","EIVEres.M"))) # we will use this variant of visualization for the plots

# Moisture 60% threshold ---------------
m2.4.4 <- lmer(data = summary_gen_traits60, mean_occ_change ~ protection60 * EIVEres.M +  mean_OP_T1 + (1|TaxonName))
summary(m2.4.4) # no interaction with moisture but marginal significant effect of EIVEres.M alone
plot(m2.4.4)
plot(ggpredict(m2.4.4, terms = c("protection60","EIVEres.M"))) # we will use this variant of visualization for the plots

# Moisture 50% threshold ---------------
m2.4.5 <- lmer(data = summary_gen_traits50, mean_occ_change ~ protection50 * EIVEres.M +  mean_OP_T1 + (1|TaxonName))
summary(m2.4.5) # marginal significant nteraction with moisture and marginal significant effect of EIVEres.M and significant effect for "protected" alone
plot(m2.4.5)
plot(ggpredict(m2.4.5, terms = c("protection50","EIVEres.M"))) # we will use this variant of visualization for the plots


# Diagnostics M --------
resid <- residuals(m2.4)
fitted <- fitted(m2.4)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.3_Moisture060625.png", width = 1000, height = 1000)
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


# Diagnostics M 90% threshold--------
resid <- residuals(m2.4.1)
fitted <- fitted(m2.4.1)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.4.1_Moisture90.png", width = 1000, height = 1000)
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


# Diagnostics M 80% threshold--------
resid <- residuals(m2.4.2)
fitted <- fitted(m2.4.2)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.4.2_Moisture80.png", width = 1000, height = 1000)
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



# Diagnostics M 70% threshold--------
resid <- residuals(m2.4.3)
fitted <- fitted(m2.4.3)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.4.3_Moisture70.png", width = 1000, height = 1000)
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



# Diagnostics M 60% threshold--------
resid <- residuals(m2.4.4)
fitted <- fitted(m2.4.4)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.4.4_Moisture60.png", width = 1000, height = 1000)
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



# Diagnostics M 50% threshold--------
resid <- residuals(m2.4.5)
fitted <- fitted(m2.4.5)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.4.5_Moisture50.png", width = 1000, height = 1000)
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
m2.5 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat*EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.5)
plot(m2.5)
plot(ggpredict(m2.5, terms = c("protection_cat","EIVEres.L"))) # we will use this variant of visualization for the plots

# Light Preference 90% threshold ---------------
m2.5.1 <- lmer(data = summary_gen_traits90, mean_occ_change ~ protection90 * EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.5.1)
plot(m2.5.1)
plot(ggpredict(m2.5.1, terms = c("protection90","EIVEres.L"))) # we will use this variant of visualization for the plots

# Light Preference 80% threshold ---------------
m2.5.2 <- lmer(data = summary_gen_traits80, mean_occ_change ~ protection80 * EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.5.2)
plot(m2.5.2)
plot(ggpredict(m2.5.2, terms = c("protection80","EIVEres.L"))) # we will use this variant of visualization for the plots

# Light Preference 70% threshold ---------------
m2.5.3 <- lmer(data = summary_gen_traits70, mean_occ_change ~ protection70 * EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.5.3)
plot(m2.5.3)
plot(ggpredict(m2.5.3, terms = c("protection70","EIVEres.L"))) # we will use this variant of visualization for the plots

# Light Preference 60% threshold ---------------
m2.5.4 <- lmer(data = summary_gen_traits60, mean_occ_change ~ protection60 * EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.5.4)
plot(m2.5.4)
plot(ggpredict(m2.5.4, terms = c("protection60","EIVEres.L"))) # we will use this variant of visualization for the plots

# Light Preference 50% threshold ---------------
m2.5.5 <- lmer(data = summary_gen_traits50, mean_occ_change ~ protection50 * EIVEres.L +  mean_OP_T1 + (1|TaxonName))
summary(m2.5.5)
plot(m2.5.5)
plot(ggpredict(m2.5.5, terms = c("protection50","EIVEres.L"))) # we will use this variant of visualization for the plots


# Diagnostics L --------
resid <- residuals(m2.5)
fitted <- fitted(m2.5)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.5_Light060625.png", width = 1000, height = 1000)
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



# Diagnostics L 90% threshold--------
resid <- residuals(m2.5.1)
fitted <- fitted(m2.5.1)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.5.1_Light90.png", width = 1000, height = 1000)
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



# Diagnostics L 80% threshold--------
resid <- residuals(m2.5.2)
fitted <- fitted(m2.5.2)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.5.2_Light80.png", width = 1000, height = 1000)
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


# Diagnostics L 70% threshold--------
resid <- residuals(m2.5.3)
fitted <- fitted(m2.5.3)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file

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


# Diagnostics L 60% threshold--------
resid <- residuals(m2.5.4)
fitted <- fitted(m2.5.4)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.5.4_Light60.png", width = 1000, height = 1000)
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


# Diagnostics L 50% threshold--------
resid <- residuals(m2.5.5)
fitted <- fitted(m2.5.5)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.5.5_Light50.png", width = 1000, height = 1000)
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

m2.6 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat*main_group +  mean_OP_T1 + (1|TaxonName))
summary(m2.6) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.6)
plot(m2.6)
plot(ggpredict(m2.6, terms = c("protection_cat","main_group")))

# Forest affiliation 90% threshold ---------------
m2.6.1 <- lmer(data = summary_gen_traits90, mean_occ_change ~ protection90 * main_group +  mean_OP_T1 + (1|TaxonName))
summary(m2.6.1) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.6.1)
plot(m2.6.1)
plot(ggpredict(m2.6.1, terms = c("protection90","main_group")))

# Forest affiliation 80% threshold ---------------
m2.6.2 <- lmer(data = summary_gen_traits80, mean_occ_change ~ protection80 * main_group +  mean_OP_T1 + (1|TaxonName))
summary(m2.6.2) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.6.2)
plot(m2.6.2)
plot(ggpredict(m2.6.2, terms = c("protection80","main_group")))

# Forest affiliation 70% threshold ---------------
m2.6.3 <- lmer(data = summary_gen_traits70, mean_occ_change ~ protection70 * main_group +  mean_OP_T1 + (1|TaxonName))
summary(m2.6.3) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.6.3)
plot(m2.6.3)
plot(ggpredict(m2.6.3, terms = c("protection70","main_group")))

# Forest affiliation 60% threshold ---------------
m2.6.4 <- lmer(data = summary_gen_traits60, mean_occ_change ~ protection60 * main_group +  mean_OP_T1 + (1|TaxonName))
summary(m2.6.4) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.6.4)
plot(m2.6.4)
plot(ggpredict(m2.6.4, terms = c("protection60","main_group")))

# Forest affiliation 50% threshold ---------------
m2.6.5 <- lmer(data = summary_gen_traits50, mean_occ_change ~ protection50 * main_group +  mean_OP_T1 + (1|TaxonName))
summary(m2.6.5) #negative marginal interaction and significant for open specialists and forest specialists as well as "protected" alone
anova(m2.6.5)
plot(m2.6.5)
plot(ggpredict(m2.6.5, terms = c("protection50","main_group")))


# Diagnostics Forest affiliation --------
resid <- residuals(m2.6)
fitted <- fitted(m2.6)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6_ForestAffiliation060625.png", width = 1000, height = 1000)
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



# Diagnostics Forest affiliation 90% threshold--------
resid <- residuals(m2.6.1)
fitted <- fitted(m2.6.1)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6.1_ForestAffiliation90.png", width = 1000, height = 1000)
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



# Diagnostics Forest affiliation 80% threshold--------
resid <- residuals(m2.6.2)
fitted <- fitted(m2.6.2)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6.2_ForestAffiliation80.png", width = 1000, height = 1000)
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




# Diagnostics Forest affiliation 70% threshold--------
resid <- residuals(m2.6.3)
fitted <- fitted(m2.6.3)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6.3_ForestAffiliation70.png", width = 1000, height = 1000)
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


# Diagnostics Forest affiliation 60% threshold--------
resid <- residuals(m2.6.4)
fitted <- fitted(m2.6.4)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6.4_ForestAffiliation60.png", width = 1000, height = 1000)
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


# Diagnostics Forest affiliation 50% threshold--------
resid <- residuals(m2.6.5)
fitted <- fitted(m2.6.5)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.6.5_ForestAffiliation50.png", width = 1000, height = 1000)
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
m2.7 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat*PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))

summary(m2.7)
anova(m2.7)
plot(m2.7)
plot(ggpredict(m2.7, terms = c("PlantGrowthForm","protection_cat")))

# Plant Growth Form 90% threshold ---------------
m2.7.1 <- lmer(data = summary_gen_traits90, mean_occ_change ~ protection90 * PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))
summary(m2.7.1)
anova(m2.7.1)
plot(m2.7.1)
plot(ggpredict(m2.7.1, terms = c("PlantGrowthForm","protection90")))

# Plant Growth Form 80% threshold ---------------
m2.7.2 <- lmer(data = summary_gen_traits80, mean_occ_change ~ protection80 * PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))
summary(m2.7.2)
anova(m2.7.2)
plot(m2.7.2)
plot(ggpredict(m2.7.2, terms = c("PlantGrowthForm","protection80")))

# Plant Growth Form 70% threshold ---------------
m2.7.3 <- lmer(data = summary_gen_traits70, mean_occ_change ~ protection70 * PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))
summary(m2.7.3)
anova(m2.7.3)
plot(m2.7.3)
plot(ggpredict(m2.7.3, terms = c("PlantGrowthForm","protection70")))

# Plant Growth Form 60% threshold ---------------
m2.7.4 <- lmer(data = summary_gen_traits60, mean_occ_change ~ protection60 * PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))
summary(m2.7.4)
anova(m2.7.4)
plot(m2.7.4)
plot(ggpredict(m2.7.4, terms = c("PlantGrowthForm","protection60")))

# Plant Growth Form 50% threshold ---------------
m2.7.5 <- lmer(data = summary_gen_traits50, mean_occ_change ~ protection50 * PlantGrowthForm +  mean_OP_T1 + (1|TaxonName))
summary(m2.7.5)
anova(m2.7.5)
plot(m2.7.5)
plot(ggpredict(m2.7.5, terms = c("PlantGrowthForm","protection50")))

# Diagnostics Plant Growth Form --------
resid <- residuals(m2.7)
fitted <- fitted(m2.7)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7_PlantGrowthForm060625.png", width = 1000, height = 1000)
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


# Diagnostics Plant Growth Form 90% threshold--------
resid <- residuals(m2.7.1)
fitted <- fitted(m2.7.1)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7.1_PlantGrowthForm90.png", width = 1000, height = 1000)
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



# Diagnostics Plant Growth Form 80% threshold--------
resid <- residuals(m2.7.2)
fitted <- fitted(m2.7.2)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7.2_PlantGrowthForm80.png", width = 1000, height = 1000)
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



# Diagnostics Plant Growth Form 70% threshold--------
resid <- residuals(m2.7.3)
fitted <- fitted(m2.7.3)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7.3_PlantGrowthForm70.png", width = 1000, height = 1000)
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



# Diagnostics Plant Growth Form 60% threshold--------
resid <- residuals(m2.7.4)
fitted <- fitted(m2.7.4)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7.4_PlantGrowthForm60.png", width = 1000, height = 1000)
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



# Diagnostics Plant Growth Form 50% threshold--------
resid <- residuals(m2.7.5)
fitted <- fitted(m2.7.5)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.7.5_PlantGrowthForm50.png", width = 1000, height = 1000)
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

m2.8 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat*Woodiness+  mean_OP_T1 + (1|TaxonName))

summary(m2.8)
anova(m2.8)
plot(m2.8)
plot(ggpredict(m2.8, terms = c("protection_cat","Woodiness")))
preds <- ggpredict(m2.8, terms = c("protection_cat","Woodiness"))

# Woodiness 90% threshold ---------------
m2.8.1 <- lmer(data = summary_gen_traits90, mean_occ_change ~ protection90 * Woodiness +  mean_OP_T1 + (1|TaxonName))
summary(m2.8.1)
anova(m2.8.1)
plot(m2.8.1)
plot(ggpredict(m2.8.1, terms = c("protection90","Woodiness")))

# Woodiness 80% threshold ---------------
m2.8.2 <- lmer(data = summary_gen_traits80, mean_occ_change ~ protection80 * Woodiness +  mean_OP_T1 + (1|TaxonName))
summary(m2.8.2)
anova(m2.8.2)
plot(m2.8.2)
plot(ggpredict(m2.8.2, terms = c("protection80","Woodiness")))

# Woodiness 70% threshold ---------------
m2.8.3 <- lmer(data = summary_gen_traits70, mean_occ_change ~ protection70 * Woodiness +  mean_OP_T1 + (1|TaxonName))
summary(m2.8.3)
anova(m2.8.3)
plot(m2.8.3)
plot(ggpredict(m2.8.3, terms = c("protection70","Woodiness")))

# Woodiness 60% threshold ---------------
m2.8.4 <- lmer(data = summary_gen_traits60, mean_occ_change ~ protection60 * Woodiness +  mean_OP_T1 + (1|TaxonName))
summary(m2.8.4)
anova(m2.8.4)
plot(m2.8.4)
plot(ggpredict(m2.8.4, terms = c("protection60","Woodiness")))

# Woodiness 50% threshold ---------------
m2.8.5 <- lmer(data = summary_gen_traits50, mean_occ_change ~ protection50 * Woodiness +  mean_OP_T1 + (1|TaxonName))
summary(m2.8.5)
anova(m2.8.5)
plot(m2.8.5)
plot(ggpredict(m2.8.5, terms = c("protection50","Woodiness")))

# Diagnostics Woodiness --------
resid <- residuals(m2.8)
fitted <- fitted(m2.8)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8_Woodiness060625.png", width = 1000, height = 1000)
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


# Diagnostics Woodiness 90% threshold--------
resid <- residuals(m2.8.1)
fitted <- fitted(m2.8.1)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8.1_Woodiness90.png", width = 1000, height = 1000)
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



# Diagnostics Woodiness 80% threshold--------
resid <- residuals(m2.8.2)
fitted <- fitted(m2.8.2)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8.2_Woodiness80.png", width = 1000, height = 1000)
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



# Diagnostics Woodiness 70% threshold--------
resid <- residuals(m2.8.3)
fitted <- fitted(m2.8.3)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8.3_Woodiness70.png", width = 1000, height = 1000)
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



# Diagnostics Woodiness 60% threshold--------
resid <- residuals(m2.8.4)
fitted <- fitted(m2.8.4)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8.4_Woodiness60.png", width = 1000, height = 1000)
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



# Diagnostics Woodiness 50% threshold--------
resid <- residuals(m2.8.5)
fitted <- fitted(m2.8.5)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.8.5_Woodiness50.png", width = 1000, height = 1000)
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

m2.9 <- lmer(data = summary_gen_traits, mean_occ_change ~ protection_cat*LeafType+  mean_OP_T1 + (1|TaxonName))

summary(m2.9)
anova(m2.9)
plot(m2.9)
plot(ggpredict(m2.9, terms = c("LeafType","protection_cat")))

# Leaf Type 90% threshold ---------------
m2.9.1 <- lmer(data = summary_gen_traits90, mean_occ_change ~ protection90 * LeafType +  mean_OP_T1 + (1|TaxonName))
summary(m2.9.1)
anova(m2.9.1)
plot(m2.9.1)
plot(ggpredict(m2.9.1, terms = c("LeafType","protection90")))

# Leaf Type 80% threshold ---------------
m2.9.2 <- lmer(data = summary_gen_traits80, mean_occ_change ~ protection80 * LeafType +  mean_OP_T1 + (1|TaxonName))
summary(m2.9.2)
anova(m2.9.2)
plot(m2.9.2)
plot(ggpredict(m2.9.2, terms = c("LeafType","protection80")))

# Leaf Type 70% threshold ---------------
m2.9.3 <- lmer(data = summary_gen_traits70, mean_occ_change ~ protection70 * LeafType +  mean_OP_T1 + (1|TaxonName))
summary(m2.9.3)
anova(m2.9.3)
plot(m2.9.3)
plot(ggpredict(m2.9.3, terms = c("LeafType","protection70")))

# Leaf Type 60% threshold ---------------
m2.9.4 <- lmer(data = summary_gen_traits60, mean_occ_change ~ protection60 * LeafType +  mean_OP_T1 + (1|TaxonName))
summary(m2.9.4)
anova(m2.9.4)
plot(m2.9.4)
plot(ggpredict(m2.9.4, terms = c("LeafType","protection60")))

# Leaf Type 50% threshold ---------------
m2.9.5 <- lmer(data = summary_gen_traits50, mean_occ_change ~ protection50 * LeafType +  mean_OP_T1 + (1|TaxonName))
summary(m2.9.5)
anova(m2.9.5)
plot(m2.9.5)
plot(ggpredict(m2.9.5, terms = c("LeafType","protection50")))

# Diagnostics Leaf Type --------
resid <- residuals(m2.9)
fitted <- fitted(m2.9)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.9_LeafType060625.png", width = 1000, height = 1000)
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


# Diagnostics Leaf Type 90% threshold--------
resid <- residuals(m2.9.1)
fitted <- fitted(m2.9.1)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.9.1_LeafType90.png", width = 1000, height = 1000)
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


# Diagnostics Leaf Type 80% threshold--------
resid <- residuals(m2.9.2)
fitted <- fitted(m2.9.2)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.9.2_LeafType80.png", width = 1000, height = 1000)
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



# Diagnostics Leaf Type 70% threshold--------
resid <- residuals(m2.9.3)
fitted <- fitted(m2.9.3)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.9.3_LeafType70.png", width = 1000, height = 1000)
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


# Diagnostics Leaf Type 60% threshold--------
resid <- residuals(m2.9.4)
fitted <- fitted(m2.9.4)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.9.4_LeafType60.png", width = 1000, height = 1000)
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


# Diagnostics Leaf Type 50% threshold--------
resid <- residuals(m2.9.5)
fitted <- fitted(m2.9.5)
qq <- qqnorm(resid, plot.it = FALSE)
# PNG-file
#png("./figures/diagnostic_plots_m2.9.5_LeafType50.png", width = 1000, height = 1000)
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