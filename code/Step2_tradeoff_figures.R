

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



# Plot data
################################################################################

basket_num <- 1
plot_timeseries <- function(basket_num){
  
  # Subset data
  sdata <- data %>% 
    # Reduce
    filter(basket_id==basket_num & year==30) %>% 
    filter(bbmsy>0)
  
  # Highest % before going below B/BMSY
  stats <- sdata %>%
    # Years above BMSY
    filter(bbmsy>=1) %>% 
    # Largest harvest that keeps above BMSY
    group_by(comm_name) %>% 
    arrange(comm_name, desc(msy_perc)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    # Order of largest harvest
    arrange(desc(msy_perc), desc(bbmsy))
  
  # Order data
  sdata_ordered <- sdata %>% 
    mutate(comm_name=factor(comm_name, levels=stats$comm_name))
  
  # Theme
  my_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=7),
                     plot.title=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Plot title
  plot_title <- paste0("Basket ", basket_num, ": ", unique(sdata$basket_name))
  
  # Plot data
  g <- ggplot(sdata_ordered, aes(x=bbmsy, y=profits_usd_tot/1000, color=comm_name, fill=comm_name, size=msy_perc)) +
    geom_point(alpha=0.5, pch=21) +
    # Ref lines
    geom_vline(xintercept=1, linetype="dashed", linewidth=0.3) +
    # Text
    geom_text(data=stats, mapping=aes(x=bbmsy, y=profits_usd_tot/1000, label=paste0(msy_perc, "%")), 
              color="black", inherit.aes = F, size=2.2) +
    # Labels
    labs(x="Terminal B/BMSY", y="Total profits (USD thousands)", title=  plot_title ) +
    # Legend
    scale_fill_discrete(name="Species") +
    scale_color_discrete(name="Species") +
    scale_size_continuous(name="% of MSY") +
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 1), size = guide_legend(order = 2)) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.key.size = unit(0.3, "cm"))
  g
  
  # Export figure
  figname <- paste0("tradeoffs_basket", basket_num, ".png")
  ggsave(g, filename=file.path(plotdir, figname), 
         width=5, height=3.5, units="in", dpi=600)
  
}

# Plot data
################################################################################

basket_ids <- c(1:5, 7:10, 13)

for(i in basket_ids){
  plot_timeseries(i)
}


