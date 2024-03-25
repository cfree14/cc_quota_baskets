

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/mauricio/raw"
outdir <- "data/mauricio/processed"
plotdir <- "figures"

# Read data
spp_orig <- read.csv(file.path(indir, "species_belize.csv"), na.strings=c("#N/A", "NA"))
data_orig <- read.csv(file.path(indir, "result_basket.csv"))


# Format species data
################################################################################

# Format darta
spp <- spp_orig %>% 
  # Remove useless
  select(-c(region, edf, r_fishbase, r_cmsy, price_source, k_cmsy, k_regressuib, 
            last_catch, year, msy_source, q_source)) %>% 
  # Fill in missing common names
  mutate(comm_name=ifelse(is.na(comm_name), comm_name_foreign, comm_name)) %>% 
  select(-comm_name_foreign) %>% 
  mutate(comm_name=recode(comm_name, 
                          "Deep water blackgin snapper"="Deepwater blackfin snapper",
                          "Great amberjack"="Greater amberjack",
                          "Marlin stripe"="Striped marlin",
                          "Marlin white"="White marlin",
                          "Mangrove/Mahogany snapper"="Mangrove snapper",
                          "Mojarra (Pompano)"="Pompano mojarra",
                          "Mojarra (yellowfin)"="Yellowfin mojarra")) %>% 
  # Rename
  rename(basket_id=basket,
         basket_name=group_name,
         r=r_used,
         k_mt=k_used,
         msy_mt=msy, 
         indicator_yn=indicator_spe, 
         q_div_r=q.r) %>% 
  # Add BMSY
  mutate(bmsy_mt=k_mt/2) %>% 
  # Format basket names
  mutate(basket_name=stringr::str_to_sentence(basket_name),
         basket_name=recode(basket_name,
                            "Bait"="Bait for other fisheries",
                            "Fished together/needs management"="Fished together",
                            "Forefreed/open/handline"="Forereef/handline",
                            "Habitats/traps/lines/nets"="Trap/line/net fisheries",
                            "Pelagic/migratory/gear"="Pelagic/migratory",
                            "Pelagic/migratory/gear-handline"="Pelagic/migratory handline",
                            "Rebuild"="Needs to be rebuilt",
                            "Special"="Special consideration")) %>% 
  # Arrange
  select(basket_id, basket_name, comm_name, sci_name, indicator_yn, priority, price, 
         r, k_mt, msy_mt, bmsy_mt, q, q_div_r, 
         everything())

# Inspect
sort(unique(spp$basket_name))

# Export
saveRDS(spp, file=file.path(outdir, "species_key.Rds"))


# Clean data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(basket_id=basket, 
         comm_name=species,
         msy_perc=per_quota,
         profits_usd_tot=tot_profits,
         revenues_usd=revenue,
         biomass_mt=biomass, 
         catch_mt=harvest,
         effort=effort.t_1,
         er_mauricio=exploitation.rate) %>% 
  # Format common name
  mutate(comm_name=recode(comm_name, 
                          "Deep water blackgin snapper"="Deepwater blackfin snapper",
                          "Great amberjack"="Greater amberjack",
                          "Marlin stripe"="Striped marlin",
                          "Marlin white"="White marlin",
                          "Mangrove/Mahogany snapper"="Mangrove snapper",
                          "Mojarra (Pompano)"="Pompano mojarra",
                          "Mojarra (yellowfin)"="Yellowfin mojarra")) %>% 
  # Add metadata
  left_join(spp %>% select(comm_name, basket_name, bmsy_mt), by="comm_name") %>% 
  # Add columns
  mutate(bbmsy=biomass_mt/bmsy_mt,
         er=catch_mt/biomass_mt,
         er=ifelse(is.infinite(er), NA, er)) %>% 
  # Arrange
  select(basket_id, basket_name, comm_name, msy_perc, year, bmsy_mt,
         biomass_mt, bbmsy, effort, er, 
         catch_mt,  revenues_usd, profits_usd_tot, 
         everything())


# Inspect
str(data)
freeR::complete(data)

# Export
saveRDS(data, file=file.path(outdir, "results.Rds"))
