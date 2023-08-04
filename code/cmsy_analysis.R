
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/saup/raw"
outdir <- "data/saup/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS("data/saup/processed/saup_cuba_catch_data")

# Read species
spp_key <- read.csv("data/karr/processed/karr_cuba_fish_life_history.csv", as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to species of interest
  filter(sci_name %in% spp_key$sci_name) %>% 
  # Summarize
  group_by(sci_name, comm_name, year) %>% 
  summarize(catch_mt=sum(catch_mt, na.rm=T)) %>% 
  ungroup() 

# Species
species_do <- sort(unique(data$sci_name))

# Resilience values
res_df <- datalimited2::resilience(species_do)


# Fit CMSY
################################################################################

# Loop through species
i <- 1
for(i in 1:length(species_do)){
  
  # Species
  spp_do <- species_do[i]
  res <- res_df$resilience[res_df$species==spp_do]
  
  # Subset data
  sdata <- data %>% 
    filter(sci_name==spp_do)
  
  # Fit CMSY
  fit <- datalimited2::cmsy2(year=sdata$year,
                             catch=sdata$catch_mt, 
                             resilience = res)
  
  # Plot CMSY
  datalimited2::plot_dlm(fit)
  
  # Extract reference points and time series from output
  ref_pts <- fit[["ref_pts"]] %>% 
    mutate(sci_name=spp_do) %>% 
    select(sci_name, everything())
  ref_ts <- fit[["ref_ts"]] %>% 
    mutate(sci_name=spp_do) %>% 
    select(sci_name, everything())
  
  # Merge
  if(i==1){
    ref_pt_out <- ref_pts
    ref_ts_out <- ref_ts
  }else{
    ref_pt_out <- bind_rows(ref_pt_out, ref_pts)
    ref_ts_out <- bind_rows(ref_ts_out, ref_ts)
  }
  
}






