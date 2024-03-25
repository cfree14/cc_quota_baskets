

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
spp <- readRDS(file.path(outdir, "species_key.Rds"))
data <- readRDS(file.path(outdir, "results.Rds"))

  
# Read data
################################################################################

# Build paramter stats
stats1 <- spp %>% 
  # Reduce to species evaluated
  filter(comm_name %in% data$comm_name) %>% 
  # Calculate basket-wide stats
  group_by(basket_id, basket_name) %>% 
  summarize(nspp=n(),
            r_avg=mean(r),
            r_cv=sd(r)/mean(r),
            qdivr_avg=mean(q_div_r),
            qdivr_cv=sd(q_div_r)/mean(q_div_r)) %>% 
  ungroup() %>% 
  # Remove 1 species baskets
  filter(nspp>1)

# Build initial status stats
stats2 <- data %>% 
  # Reduce to start years
  filter(year==0 & msy_perc==100) %>% 
  # Round B/BMSY to avoid CV errors
  mutate(bbmsy=round(bbmsy, 3)) %>% 
  # Calculate average/CV
  group_by(basket_id) %>% 
  summarize(bbmsy_avg=mean(bbmsy),
            bbmsy_cv=sd(bbmsy)/mean(bbmsy)) %>% 
  ungroup()

# Build performance stats
stats3 <- data %>% 
  # Only years with B/BMSY greater than 1
  filter(year==30 & bbmsy>=1) %>% 
  # Find maximum MSY perc that keeps each species over BMSY
  arrange(basket_id, comm_name, desc(msy_perc)) %>% 
  group_by(basket_id, comm_name) %>%
  slice(1) %>% 
  ungroup() %>% 
  # Find maximum MSY that works for a basket
  group_by(basket_id) %>% 
  summarize(msy_perc=min(msy_perc)) %>% 
  ungroup()

# Merge stats
stats <- stats1 %>% 
  left_join(stats2) %>% 
  left_join(stats3)

# Plot
ggplot(stats, aes(y=msy_perc, x=qdivr_cv)) +
  geom_point()

ggplot(stats, aes(y=msy_perc, x=qdivr_avg)) +
  geom_point()

ggplot(stats, aes(y=msy_perc, x=r_avg)) +
  geom_point()

ggplot(stats, aes(y=msy_perc, x=r_cv)) +
  geom_point()

ggplot(stats, aes(y=msy_perc, x=bbmsy_cv)) +
  geom_point()

ggplot(stats, aes(y=msy_perc, x=bbmsy_avg)) +
  geom_point()

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats, aes(y=msy_perc/100, x=qdivr_cv, fill=bbmsy_avg)) +
  geom_smooth(method="lm", fill="grey85", color="grey40") +
  geom_point(size=4, pch=21) +
  geom_text(mapping=aes(label=basket_id), size=2) +
  # Labels
  labs(x="Coefficient of variation of\nthe vulerability ratio (q/r)",
       y="Percent of cumulative MSY\nthat keeps all species above BMSY") +
  # Axes
  scale_y_continuous(labels=scales::percent_format()) +
  coord_cartesian(ylim = c(0,NA)) +
  # Legend
  scale_fill_gradient2(name="Initial\nB/BMSY", midpoint=1, low="darkred", high="navy") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_performance_drivers.png"), 
       width=5, height=4.5, units="in", dpi=600)
  
