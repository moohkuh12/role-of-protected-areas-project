# This is a seperate script to be runned in background (model takes quite long)
library(lme4)
# Test: Binomial model with management as random effect -----
model_mgmt <- glmer(
  increase ~ protection_cat + (1 | TaxonName),
  data = trend_data_mgmt,
  family = binomial
)
summary(model_mgmt)
