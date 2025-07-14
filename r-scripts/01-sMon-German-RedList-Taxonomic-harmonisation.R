# =============================================================
# 01_taxonomic_harmonization.R
# =============================================================
# Purpose:
#   - Harmonize species names from the German Red List and sMon datasets
#   - Match species names to WCVP using exact and fuzzy matching
#   - Identify threatened and non-native species
#   - Save harmonized datasets for downstream analyses
#
# Author: Marie Englert
# Date: 01.04.2025
# =============================================================

# PREAMBLE: SETUP & LIBRARIES ----
# Load common functions, packages, and settings.
source("./r-scripts/00-preamble.R")

# PART 1: LOAD & PROCESS RED LIST DATA ----

# Step 1: Read the Red List data (ferns and flowering plants) from the Excel file.
drl_germany <- read_excel("./data/RoteListe_Farn- und Bluetenpflanzen_2018/02_Datentabelle_RL_Farn-_und_Bluetenpflanzen_2018_Deutschland_20210317-1607.xlsx")

# Step 2: Select only the relevant columns and rename them for clarity.
drl_germany <- drl_germany %>% select(
  species      = Name,                              # Full species name including authors.
  type         = Arten,                             # Species type (e.g., native, non-native).
  native       = Status,                            # Native status indicator.
  range        = `aktuelle Bestandssituation`,       # Current population range.
  trend        = `langfristiger Bestandstrend`,      # Long-term population trend.
  threat_2018  = `RL Kat.`,                         # Red List category (2018).
  threat_1998  = `alte RL- Kat.`                    # Previous Red List category (1998).
)

# Step 3: Filter out rows with missing 'type' to keep only valid species.
drl_germany <- drl_germany %>% filter(!is.na(type))

# Step 4: Remove author names from the species names using the flora package.
# Note: flora::remove.authors() may not work perfectly for every species.
species_without <- vector("character", nrow(drl_germany))
for (i in 1:nrow(drl_germany)) {
  species_without[i] <- flora::remove.authors(drl_germany$species[i])
}
# Add the cleaned species names as a new column.
drl_germany$species_without <- species_without

# Step 5: Harmonize the 'native' status categories for clarity.
drl_germany <- drl_germany %>% mutate(
  native = ifelse(
    native == "I", "native",
    ifelse(native == "N", "established non-native",
           ifelse(native == "U", "transient", "doubtful")
    )
  )
)

# Step 6: Remove the 'type' column (no longer needed) and assign the processed data to 'rl'.
rl <- drl_germany %>% select(-type)

# NOTE: LOAD MANUALLY CORRECTED SPECIES NAMES ----
# The automatic removal of author names was not perfect.
# Manually corrected these issues in Excel and saved the corrected data as
# "rl_withoutauthors.csv". Therefore, we load the corrected file.
# Corrected Issues were: Hieracium duerkhemiense is actually Pilosella duerkhemiense 
# (gets the match and is consistent with the denoted authors "(Zahn) Gottschl. & Meierott)", 
# some species had "f.", "Jansen", "Lecoq" or "Rydb." on it
rl <- read.csv("./data/TaxonHarm/rl_withoutauthors.csv", 
               stringsAsFactors = FALSE, sep = ";", header = TRUE)

# PART 2: HARMONIZE SPECIES NAMES WITH THE WCVP DATABASE ----

# Step 7: Match Red List species names (using column 'species_without') with the WCVP database.
# We use fuzzy matching to improve results.
wcvp_match_rl <- wcvp_match_names(rl, name_col = "species_without", fuzzy = TRUE)

# In our workflow, the matching results were saved to file.
# write.csv(wcvp_match_rl, "./data/TaxonHarm/complete_matching_rl.csv", row.names = FALSE)
# Re-import the matching results.
exact_matches_rl <- read.csv("./data/TaxonHarm/complete_matching_rl.csv")

# Step 8: Prioritize accepted names and synonyms if multiple matches exist.
# Here we remove duplicates based on the cleaned species name.
wcvp_match_rl <- exact_matches_rl %>% 
  arrange(
    desc(wcvp_status %in% c("Accepted", "Synonym")),  # Prioritize Accepted/Synonym.
    desc(wcvp_status == "Accepted")                    # Further prioritize Accepted.
  ) %>%
  distinct(species_without, .keep_all = TRUE)

# (Optional diagnostic: You could check unmatched entries here if needed.)
# unmatched_species_diag <- wcvp_match_rl %>% filter(is.na(wcvp_status))

# Step 10: Prioritize accepted names by removing duplicates based on the unique WCVP ID.
# This step reduces the dataset; for example, from 4119 to 4036 rows.
wcvp_acc_rl <- wcvp_match_rl %>% 
  arrange(
    desc(wcvp_status %in% c("Accepted", "Synonym")),
    desc(wcvp_status == "Accepted")
  ) %>%
  distinct(wcvp_accepted_id, .keep_all = TRUE)
# Note: 83 species did not receive a wcvp_accepted_id.

# Step 11: Merge the Red List data (rl) with the WCVP matches using the column 'species_without'.
rl_wcvp <- inner_join(rl, wcvp_acc_rl, by = "species_without")

# Step 11b: Handle unmatched species ----
# (Optional diagnostic: Check which species in rl were not matched in wcvp_acc_rl.)
unmatched_species <- anti_join(rl, wcvp_acc_rl, by = "species_without")
# Optionally, save these for manual correction:
# write.csv(unmatched_species, "./data/TaxonHarm/unmatched_species_rl.csv", row.names = FALSE)

# Trim whitespace from species names in unmatched_species and re-run fuzzy matching.
unmatched_species$species_without <- str_trim(unmatched_species$species_without)
unmatched_species <- wcvp_match_names(unmatched_species, name_col = "species_without", fuzzy = TRUE)

# Separate the newly matched species:
matched_unmatched <- unmatched_species %>% filter(!is.na(wcvp_accepted_id))
# And those that remain unmatched (to be inspected manually).
unmatched_for_manual <- unmatched_species %>% filter(is.na(wcvp_accepted_id))

# Combine the fuzzy-matched (previously unmatched) entries with our main dataset.
wcvp_acc_rl <- bind_rows(wcvp_acc_rl, unmatched_species)

# Step 11c: Check for duplicate species in the combined dataset 
duplicates <- wcvp_acc_rl %>%
  group_by(species) %>%
  summarise(count = n()) %>%
  filter(count > 1)
print(duplicates)  # Display duplicate counts for species_without.
# If duplicates remain, you can resolve them (e.g., by keeping the entry with "Accepted" status).

# Step 11d: Finalize by removing duplicates 
wcvp_acc_rl <- wcvp_acc_rl %>% 
  arrange(
    desc(wcvp_status %in% c("Accepted", "Synonym")),
    desc(wcvp_status == "Accepted")
  ) %>%
  distinct(species_without, .keep_all = TRUE)

# Step 12: Assign threat categories based on the Red List data. 
endangered_categories <- c("0", "1", "2", "3", "G", "R", "V")   # Categories indicating threatened species.
not_endangered_categories <- c("*")                                # Category indicating non-threatened species.
rl_wcvp <- wcvp_match_rl %>%
  mutate(endangered = ifelse(
    threat_2018 %in% endangered_categories, "Endangered",
    ifelse(threat_2018 %in% not_endangered_categories, "Non-endangered", "Data Deficient")
  ))

# Step 13: Create lists for non-native and endangered species. 
non_native_sp <- rl_wcvp %>%
  filter(native == "established non-native") %>%
  select(wcvp_name) %>%
  mutate(non_native = "yes") %>%
  distinct()

red_listed_sp <- rl_wcvp %>%
  filter(endangered == "Endangered") %>%
  select(wcvp_name, species_without, native, wcvp_accepted_id) %>%
  mutate(threatened = "yes") %>%
  distinct()

# Preview the red listed species.
head(red_listed_sp)  # 1692 red listed species

# (Optional: save the red listed species list)
# write.csv(red_listed_sp, "./data/TaxonHarm/red_listed_sp.csv", row.names = FALSE)
red_listed_sp <- read.csv("./data/TaxonHarm/red_listed_sp.csv", stringsAsFactors = FALSE)

# PART 3: PROCESS SMON DATA ----

# Step 14: Load SMON species data from multiple CSV files and combine them.
# We read each file, select only the "TaxonName" column, ensure uniqueness in each,
# and then combine them into one dataset.
species_smon <- bind_rows(
  list(
    read.csv("./data/RawData/1875_9_1875_2_Modelled_OPs_incl_sd_pt_1.csv") %>% select(TaxonName) %>% distinct(),
    read.csv("./data/RawData/1875_9_1875_2_Modelled_OPs_incl_sd_pt_2.csv") %>% select(TaxonName) %>% distinct(),
    read.csv("./data/RawData/1875_9_1875_2_Modelled_OPs_incl_sd_pt_3.csv") %>% select(TaxonName) %>% distinct(),
    read.csv("./data/RawData/1875_9_1875_2_Modelled_OPs_incl_sd_pt_4.csv") %>% select(TaxonName) %>% distinct()
  )
) %>% distinct()

# (Optional) Step 15: Save the combined SMON species names for reference.
# write.csv(species_smon, "./data/TaxonHarm/species_names_smon.csv", row.names = FALSE)

# Step 15: Create a new column for cleaned names while keeping the original `TaxonName`
species_smon <- species_smon %>%
  mutate(TaxonName_cleaned = TaxonName) %>%  # Duplicate the column before modifications as we need it for joining to the rest of the dataset later
  mutate(TaxonName_cleaned = str_replace_all(TaxonName_cleaned, 
                                             c("agg\\." = "",        # Remove "agg."
                                               "s\\. l\\." = "",      # Remove "s. l."
                                               "subg\\." = ""         # Remove "subg."
                                             ))) %>%
  mutate(TaxonName_cleaned = str_trim(TaxonName_cleaned)) %>%  # Remove leading/trailing whitespace.
  mutate(TaxonName_cleaned = str_squish(TaxonName_cleaned))      # Remove extra spaces within the name.

# Correct a specific species name manually in the cleaned version
species_smon <- species_smon %>%
  mutate(TaxonName_cleaned = str_replace(TaxonName_cleaned, 
                                         "Botrychium multifidum subsp\\. multifidum", 
                                         "Botrychium multifidum"))

# Step 16: Identify duplicate species names in the SMON dataset.
# This helps diagnose if any duplicates remain after combining.
duplicates_spsmon <- species_smon %>%
  count(TaxonName_cleaned) %>%    # Count occurrences of each species name.
  filter(n > 1)           # Keep only species that appear more than once.
print(duplicates_spsmon)  # Review duplicate species names - Diphasiastrum complanatum is the only one

# PART 4: MATCH SMON SPECIES NAMES WITH WCVP (EXACT & FUZZY) ----

# Step 17: Match SMON species names with the WCVP database using fuzzy matching.
wcvp_match_smon <- wcvp_match_names(
  species_smon, 
  name_col = "TaxonName_cleaned",
  fuzzy = TRUE
)

# For species flagged as "Fuzzy" matches, set up a decision column.
# I decided to keep matches with similarity of at least 0.9,
# and if the edit distance is 1 (only one letter different), we mark them to keep.
fuzzy_matches <- wcvp_match_smon %>%
  filter(str_detect(match_type, "Fuzzy")) %>%
  mutate(keep = case_when(
    match_similarity < 0.9 ~ NA_real_,  # Discard matches with similarity < 0.9.
    match_edit_distance == 1 ~ 1         # Mark to keep if only one letter differs.
  ))

# Review the summary of the fuzzy decision.
table(fuzzy_matches$keep, useNA = "always")

# Saved these fuzzy decisions for manual review:
readr::write_excel_csv(fuzzy_matches, "./data/TaxonHarm/fuzzy-tocheck.csv")



# Load the manually reviewed fuzzy matches.
fuzzy_checked <- read.csv("./data/TaxonHarm/smon-fuzzy-checked1.csv", 
                          stringsAsFactors = FALSE, sep = ";", header = TRUE) %>% 
  select(-keep) %>%
  mutate(resolved_match_type = ifelse(!is.na(resolved_match_type),
                                      resolved_match_type, match_type))
fuzzy_checked <- fuzzy_checked %>%
  mutate(multiple_matches = as.logical(ifelse(multiple_matches == "WAHR", "TRUE",
                                              ifelse(multiple_matches == "FALSCH", "FALSE",
                                                     multiple_matches))))
# unfortunately my excell messed the language up, so I had to correct it here

# Combine non-fuzzy matches with the manually corrected fuzzy matches.
checked_matches <- wcvp_match_smon %>%
  filter(!str_detect(match_type, "Fuzzy")) %>%  # Keep entries not flagged as fuzzy.
  bind_rows(fuzzy_checked)                        # Add the manually corrected fuzzy matches.

# Step 18: Extract matched species and their WCVP statuses from the SMON matching results.
# We keep key columns: TaxonName, wcvp_name, wcvp_status, and wcvp_accepted_id.
wcvp_acc_smon1 <- checked_matches %>% select(TaxonName,TaxonName_cleaned, wcvp_name, wcvp_status, wcvp_accepted_id)

# Step 19: Prioritize accepted names for SMON by removing duplicates based on TaxonName.
# This step ensures that each species (as identified by TaxonName) appears only once.
wcvp_acc_smon2 <- wcvp_acc_smon1 %>% 
  arrange(
    desc(wcvp_status %in% c("Accepted", "Synonym")),  # Prioritize "Accepted" or "Synonym".
    desc(wcvp_status == "Accepted")                   # Further prioritize if status is "Accepted".
  ) %>%
  distinct(TaxonName_cleaned, .keep_all = TRUE)

# (Optional) Check for duplicate wcvp_accepted_id entries in the SMON matching results.
duplicates <- wcvp_acc_smon2 %>% 
  group_by(wcvp_accepted_id) %>% 
  filter(n() > 1) %>% 
  arrange(wcvp_accepted_id)
print(duplicates)
# Note:
# 19 duplicates here, some share the same wcvp accepted id.
# Since in the original data some species were monitored as separate species that here have the same wcvp id,
# I would delete a species that would probably be lost when combining with the Red List anyway, so it doesn't matter.

# Optional: Clean duplicates by grouping by wcvp_accepted_id.
#wcvp_acc_smon_clean <- wcvp_acc_smon2 %>%
#group_by(wcvp_accepted_id) %>%            # Group by unique identifier.
#arrange(desc(wcvp_status == "Accepted"), .by_group = TRUE) %>%  # Prioritize "Accepted" entries.
#slice(1) %>%                              # Take the first entry from each group.
#ungroup()

# I didnt do that

# PART 5: COMBINE SMON DATA WITH RED LIST DATA ----

# Ensure uniqueness in both datasets before joining.
# For the SMON data, use the dataset where duplicates were removed by TaxonName.
wcvp_acc_smon_unique <- wcvp_acc_smon2 %>% distinct(wcvp_name, .keep_all = TRUE)

# For the Red List, ensure that species names (TaxonName) are unique.
# (Assuming 'red_listed_sp' is loaded from a previous step and contains Red List species.)
red_listed_sp_unique <- red_listed_sp %>% distinct(wcvp_name, .keep_all = TRUE)

# Convert wcvp_accepted_id to numeric in the Red List data if needed.
red_listed_sp_unique <- red_listed_sp_unique %>% mutate(wcvp_accepted_id = as.numeric(wcvp_accepted_id))
#colnames(red_listed_sp_unique  )[colnames(red_listed_sp_unique ) == "wcvp_name"] <- "TaxonName"

# Perform an inner join to retain only species that are present in both SMON and Red List datasets.
smon_red_listed <- inner_join(
  wcvp_acc_smon_unique,  
  red_listed_sp_unique,  
  by = c("wcvp_name", "wcvp_accepted_id")
)

smon_red_listed <- smon_red_listed %>% select(-species_without)

# Preview the combined SMON-Red List dataset.
head(smon_red_listed)
# Display the total number of matching species.
nrow(smon_red_listed)

# EXPORT FINAL COMBINED DATASET ----

write.csv(smon_red_listed, "./data/TaxonHarm/harmonized_redlist_smon_data_020425.csv", row.names = FALSE)
# Load the final combined dataset
smon_red_listed <- read.csv("./data/TaxonHarm/harmonized_redlist_smon_data_020425.csv", stringsAsFactors = FALSE)

# Taxonomic Harmonisation with TRY categorical trait-data ------------------

# Load TRY data
try_data <- read_xlsx("./data/Try2025515135032TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease/TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.xlsx") 
smon_red_listed <- smon_red_listed[, c(1:3)]
# Select only relevant columns
try_data <- try_data[, c(2, 6:22)]

# Step 7: Match TRY-Data species names (using column 'AccSpeciesName') with the WCVP database.
# We use fuzzy matching to improve results.
wcvp_match_try <- wcvp_match_names(try_data, name_col = "AccSpeciesName", fuzzy = TRUE)

# In our workflow, the matching results were saved to file.
# write.csv(wcvp_match_try, "./data/TaxonHarm/complete_matching_trydata.csv", row.names = FALSE)
# Re-import the matching results.
exact_matches_try <- read.csv("./data/TaxonHarm/complete_matching_trydata.csv")

# Step 8: Prioritize accepted names and synonyms if multiple matches exist.
# Here we remove duplicates based on the cleaned species name.
wcvp_match_try <- exact_matches_try %>% 
  arrange(
    desc(wcvp_status %in% c("Accepted", "Synonym")),  # Prioritize Accepted/Synonym.
    desc(wcvp_status == "Accepted")                    # Further prioritize Accepted.
  ) %>%
  distinct(AccSpeciesName, .keep_all = TRUE)

wcvp_match_try_clean <- exact_matches_try %>%
  group_by(wcvp_name) %>%
  arrange(
    desc(wcvp_status %in% c("Accepted", "Synonym")),
    desc(wcvp_status == "Accepted"),
    desc(match_type == "Exact")
  ) %>%
  slice(1) %>%
  ungroup()

# (Optional diagnostic: You could check unmatched entries here if needed.)
unmatched_species_diag <- wcvp_match_try_clean %>% filter(is.na(wcvp_status))

wcvp_match_try_clean %>% count(wcvp_name) %>% filter(n > 1)      

dups <- wcvp_match_try %>%
  group_by(wcvp_name) %>%
  filter(n() > 1) %>%
  arrange(wcvp_name)


# Merge the Red List data  with the WCVP matches for the TRY data using the column 'wcvp_name'.
rl_wcvp_try <- inner_join(smon_red_listed, wcvp_match_try_clean, by = "wcvp_name")

# save rl_wcvp_try
write.csv(rl_wcvp_try, "./data/TaxonHarm/rl_wcvp_try.csv", row.names = FALSE)
