
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/karr/raw"
outdir <- "data/karr/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "karr_cuba_fish.xlsx"))


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Format sci name
  mutate(sci_name=gsub("\\*", "", sci_name) %>% stringr::str_squish()) %>% 
  # Fix bad sci names
  mutate(sci_name=recode(sci_name,
                         "Ophistonema oglynum"="Opisthonema oglinum",  
                         "Pseudopeneus maculatus"="Pseudupeneus maculatus", 
                         "Selar crumehopthalmus"="Selar crumenophthalmus",  
                         "Tarpon atlanticus"="Megalops atlanticus")) %>% 
  # Format comm name
  mutate(comm_name=recode(comm_name,
                          "False herring, false pilchard"="False herring",
                          "Redear pilchard, sardine"="Redear herring",
                          "King mackerel *"="King mackerel"))

# Check names
freeR::check_names(data$sci_name)

# Get FL life history data
data_lh <- freeR::fishlife(species = data$sci_name)


