## 02. regression analyses

# Outdated -- we don't adjust for collinearity (?)
collinear_helper <- function (df) {
  
  df2 <- 
    df %>%
    mutate(rain_ramadan = case_when(ramadan == 1L ~ r1h_now * 1,
                                  ramadan == 0L ~ r1h_now * 0,
                                  .default      = NA))
  
  return(df2)
  
}


## 03. figures
create_jitter_ggplot <- function (df, plotlab="Plot", 
                                  xlab2="Time", 
                                  ylab2="Visits per 1000 Residents",
                                  ymaxval2=1e3) {
  
  tempplot <- ggplot(df, aes(x=visit_date, y=n_visit_per_1000, group=near_violence)) +
    
    annotate("rect", xmin = as.Date("2020-07-01"), xmax = as.Date("2020-11-01"), 
             ymin = 0, ymax = ymaxval2, fill = "gray80", alpha=0.5) +
    annotate("rect", xmin = as.Date("2021-07-01"), xmax = as.Date("2021-11-01"), 
             ymin = 0, ymax = ymaxval2, fill = "gray80", alpha=0.5) +
    annotate("rect", xmin = as.Date("2022-07-01"), xmax = as.Date("2022-11-01"), 
             ymin = 0, ymax = ymaxval2, fill = "gray80", alpha=0.5) +
    
    annotate("rect", xmin = as.Date("2020-04-01"), xmax = as.Date("2020-06-01"), 
             ymin = 0, ymax = ymaxval2, fill = "gray60", alpha=0.5) +
    annotate("rect", xmin = as.Date("2021-04-01"), xmax = as.Date("2021-06-01"), 
             ymin = 0, ymax = ymaxval2, fill = "gray60", alpha=0.5) +
    annotate("rect", xmin = as.Date("2022-04-01"), xmax = as.Date("2022-06-01"), 
             ymin = 0, ymax = ymaxval2, fill = "gray60", alpha=0.5) +
    
    geom_point(aes(color=near_violence,shape=near_violence), alpha=0.4, position = "jitter") +
    geom_smooth(method="loess", se=F, fullrange=T, aes(colour=near_violence)) +
    
    scale_color_manual(name = "Shape Legend", values=c("black", "red", "red4")) + 
    
    scale_y_continuous(limits = c(0,ymaxval2)) +
    labs(title = plotlab, x = xlab2, y = ylab2) +
    theme(legend.position="none") + theme_bw()
  
  return(tempplot)
  
}

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="right"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

norm_dim = function(obj){
  bbox = st_bbox(obj)
  width = bbox[["xmax"]] - bbox[["xmin"]]
  height = bbox[["ymax"]] - bbox[["ymin"]]
  w = width / max(width, height)
  h = height / max(width, height)
  return(unit(c(w, h), "snpc"))
}

create_ggplot_map = function (viol_shp, csps_shp, title="") {
  
  my_breaks <- seq(0,max(df_all$n_visit,25))
  
  tempgg <- 
    ggplot() +
    geom_sf(data=bfa_shape,        # Kossi province 
            lwd=0.5) +
    
    coord_sf(crs = 4326) +
    
    geom_sf(data=viol_shp,         # Event 
            aes(shape = "Event"),
            fill="red3", size=3) +
    
    geom_sf(data=csps_shp,         # CSPS
            aes(shape = "CSPS", fill=n_visit), 
            size=3) +
    
    scale_shape_manual(
      name = NULL,
      values = c("Event" = 21, "CSPS" = 24)
    ) +
    
    scale_fill_viridis(
      name = "Total CSPS Visits",
      direction = 1,
      option = "G"
      #,
      # breaks = my_breaks,
      # labels = my_breaks
    ) +
    labs(x="",y="") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      plot.margin = margin(0,0,0,0, "pt")
    ) + 
    theme_minimal() +
    ggtitle(title)
  
  return(tempgg)
}

create_ggplot_map_facet = function (viol_shp, csps_shp, title) {
  
  tempgg <- 
    ggplot() +
    geom_sf(data=bfa_shape,lwd=0.5) +
    coord_sf(crs = 4326) +
    geom_sf(data=viol_shp, color="red", size=2.5, pch=19) +
    geom_sf(data=csps_shp, aes(color=n_visit_per_1000),size=2.5, pch=18) +
    facet_wrap(~ visit_year, ncol = 1) +
    scale_color_gradient(name = "Monthly CSPS Visits per 1000 Children") +
    labs(x="",y="") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      plot.margin = margin(0,0,0,0, "pt")
    ) + 
    theme_minimal()
  
  return(tempgg)
}
