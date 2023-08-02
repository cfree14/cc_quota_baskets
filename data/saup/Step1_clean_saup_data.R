
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/saup/raw"
outdir <- "data/saup/processed"

# Read data
data_orig <- read.csv(file.path(indir, "SAU EEZ 192 v50-1.csv"), as.is = T, na.strings="")

# Read species
spp_do <- read.csv("data/karr/processed/karr_cuba_fish_life_history.csv")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(sci_name=scientific_name,
         comm_name=common_name,
         catch_mt=tonnes,
         value_usd=landed_value) %>% 
  # Remove useless columns
  select(-c(area_name, area_type))

# Inspect data
str(data)

# Check species names
spp_key <- data %>% 
  select(sci_name, comm_name, functional_group, commercial_group) %>% 
  unique()
freeR::which_duplicated(spp_key$sci_name)
freeR::check_names(spp_key$sci_name)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "saup_cuba_catch_data"))


