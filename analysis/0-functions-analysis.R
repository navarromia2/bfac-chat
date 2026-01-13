## 02. regression analyses
create_collinear_cov <- function (df) {
  
  df2 <- df %>% mutate(dryRamadan = case_when((wetSeason=="Dry" & isRamadan=="Yes") ~ "Dry season, Ramadan",
                                              (wetSeason=="Dry" & isRamadan=="No")  ~ "Dry season, no Ramadan",
                                              (wetSeason=="Wet" & isRamadan=="No")  ~ "Rainy season, no Ramadan",
                                              .default = NA))
  # print(unique(df2$wetSeason))
  
  df2$dryRamadan <- as.factor(df2$dryRamadan) %>% relevel(ref = "Dry season, no Ramadan")
  
  return(df2)
  
}

create_glmer_near <- function (df) {
  m <- glmer(n_visit ~ 1 + near_violence + dryRamadan + 
               (1 | csps) +
               offset(log(offset_n_visit_tot_csps)), 
             family = poisson(link = "log"), 
             # REML = T, 
             data = df)
  return(m)
}

create_glmer_any <- function (df) {
  m <- glmer(n_visit ~ 1 + any_violence + dryRamadan + 
               (1 | csps) +
               offset(log(offset_n_visit_tot_csps)), 
             family = poisson(link = "log"), 
             # REML = T, 
             data = df)
  return(m)
}

check_resid_dist <- function(m) { return(hist(summary(m)$resid)) }

create_pois_near <- function (df) {
  
  m <- glm(n_visit ~ near_violence + dryRamadan, 
           offset = log(offset_n_visit_tot_csps), 
           family = poisson(link = "log"), 
           data = df)
  
  return(m)
}

create_pois_any <- function (df) {
  m <- glm(n_visit ~ any_violence + dryRamadan, 
           offset = log(offset_n_visit_tot_csps), 
           family = poisson(link = "log"), 
           data = df)
  
  return(m)
}

create_pois_cont <- function (df) {
  m <- glm(n_visit ~ min_dist_to_viol + dryRamadan, 
           offset = log(offset_n_visit_tot_csps), 
           family = poisson(link = "log"), 
           data = df)
  
  return(m)
}

create_table_1 <- function (create_doc=T) {
  
  if (create_doc) {
    
    fulltab_any <- tab_model(m1_pois_any,m2_mal_pois_any,m2_pne_pois_any,m2_dia_pois_any,
                             show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                             pred.labels = c("Intercept",
                                             "Violence: Yes",
                                             "Season: Dry, Ramadan: Yes",
                                             "Season: Rainy, Ramadan: No"),
                             dv.labels = c("Total Visits", 
                                           "Malaria Visits", 
                                           "Pneumonia Visits", 
                                           "Diarrhea Visits"),
                             file = paste0(table_path,"table-1-pois.doc"))
    
  } else {
    
    fulltab_any <- tab_model(m1_pois_any,m2_mal_pois_any,m2_pne_pois_any,m2_dia_pois_any,
                             show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                             pred.labels = c("Intercept",
                                             "Violence: Yes",
                                             "Season: Dry, Ramadan: Yes",
                                             "Season: Rainy, Ramadan: No"),
                             dv.labels = c("Total Visits", 
                                           "Malaria Visits", 
                                           "Pneumonia Visits", 
                                           "Diarrhea Visits"))
    
  }
  
  return(fulltab_any)
  
}

create_table_2 <- function (create_doc=T) {
  
  if (create_doc) {
    
    fulltab_near <- tab_model(m1_pois_near,m2_mal_pois_near,m2_pne_pois_near,m2_dia_pois_near,
                              show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                              pred.labels = c("Intercept",
                                              "Violence: local (<=20 km)",
                                              "Violence: distant (>20 km)",
                                              "Season: Dry, Ramadan: Yes",
                                              "Season: Rainy, Ramadan: No"),
                              dv.labels = c("Total Visits", 
                                            "Malaria Visits", 
                                            "Pneumonia Visits", 
                                            "Diarrhea Visits"),
                              file = paste0(table_path,"table-2-pois.doc"))
    
  } else {
    
    fulltab_near <- tab_model(m1_pois_near,m2_mal_pois_near,m2_pne_pois_near,m2_dia_pois_near,
                              show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                              pred.labels = c("Intercept",
                                              "Violence: local (<=20 km)",
                                              "Violence: distant (>20 km)",
                                              "Season: Dry, Ramadan: Yes",
                                              "Season: Rainy, Ramadan: No"),
                              dv.labels = c("Total Visits", 
                                            "Malaria Visits", 
                                            "Pneumonia Visits", 
                                            "Diarrhea Visits"))
    
  }
  
  return(fulltab_near)
  
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
    geom_sf(data=bfa_shape,lwd=0.5) +
    coord_sf(crs = 4326) +
    geom_sf(data=viol_shp, fill="red3", size=3, pch=21) +
    geom_sf(data=csps_shp, aes(fill=n_visit), size=3, pch=22, size=1) +
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
