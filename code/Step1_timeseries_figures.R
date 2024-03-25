

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
    filter(basket_id==basket_num) %>% 
    # Reduce
    select(basket_id:year, bbmsy, er_mauricio, revenues_usd) %>% 
    # Convert revenues
    mutate(revenues_usd=revenues_usd/1e3) %>% 
    # Gather
    gather(key="metric", value="value", 6:ncol(.)) %>% 
    # Recode
    mutate(metric=recode_factor(metric,
                                "bbmsy"="B/BMSY",
                                "er_mauricio"="Exploitation rate",
                                "revenues_usd"="Revenues\n(USD thousands)")) %>% 
    # Format common name
    mutate(comm_name=gsub(" ", "\n", comm_name))
  
  # Plot title
  plot_title <- paste0("Basket ", basket_num, ": ", unique(sdata$basket_name))
  
  # Reference lines
  ref_lines <- tibble(metric=factor("B/BMSY", levels=levels(sdata$metric)), 
                      value=1)
  
  # Theme
  my_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=7),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=6),
                     plot.title=element_text(size=7),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=value, color=msy_perc, group=msy_perc)) +
    facet_grid(metric ~ comm_name, scales="free_y") +
    geom_line(linewidth=0.2) +
    # Ref lines
    geom_hline(data=ref_lines, mapping=aes(yintercept=value),
               color="black", linewidth=0.3, linetype="dashed") +
    # Labels
    labs(x="Year", y="", title=  plot_title ) +
    # Legend
    scale_color_gradientn(name="% of MSY", 
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
    guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.key.size = unit(0.3, "cm"),
          axis.title.y = element_blank())
  g
  
  # Export figure
  figname <- paste0("time_series_basket", basket_num, ".png")
  ggsave(g, filename=file.path(plotdir, figname), 
         width=6.5, height=3.25, units="in", dpi=600)
  
}

# Plot data
################################################################################

basket_ids <- c(1:5, 7:10, 13)

for(i in basket_ids){
  plot_timeseries(i)
}


