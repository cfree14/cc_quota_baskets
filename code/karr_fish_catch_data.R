
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
  group_by(sci_name, comm_name, year, reporting_status) %>% 
  summarize(catch_mt=sum(catch_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Build name label
  mutate(name_label=paste0(comm_name, "\n(", sci_name, ")"))

# Build order key
order_key <- data %>% 
  # Total by year
  group_by(sci_name, comm_name, name_label, year) %>% 
  summarise(catch_mt=sum(catch_mt)) %>% 
  # Average across years
  group_by(sci_name, comm_name, name_label) %>% 
  summarise(catch_mt_avg=mean(catch_mt, na.rm=T)) %>% 
  # Arrange 
  arrange(desc(catch_mt_avg))

# Order data
data_ordered <- data %>% 
  mutate(name_label=factor(name_label, levels=order_key$name_label))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_ordered, aes(x=year, y=catch_mt, fill=reporting_status)) +
  facet_wrap(~name_label, ncol=4) +
  geom_bar(stat="identity") + 
  # Labels
  labs(x="Year", y="Catch") +
  # Legend
  scale_fill_discrete(name="Catch type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.85, 0.2))
g

# Export
ggsave(g, filename=file.path(plotdir, "karr_cuba_fish_saup_catch_data.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


