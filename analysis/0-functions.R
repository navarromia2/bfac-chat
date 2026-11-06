#########################################
# Function for data cleaning & analysis
#########################################

####################################################
# FUNCTION: w2l_helper()
# LOCATION: file 01A
# Wide-to-long + add NAs for unobs months
# input:  wide_df = wide df to transform
# output: long_df = long df with NAs
####################################################
w2l_helper <- function (wide_df) {
  
  df_large <- data.frame(csps = c(rep(1,35),rep(2,35),rep(3,35),rep(4,35),rep(6,35),
                                  rep(7,35),rep(8,35),rep(9,35),rep(10,35),rep(11,35),
                                  rep(12,35),rep(13,35),rep(14,35),rep(15,35),rep(16,35),
                                  rep(17,35),rep(18,35),rep(19,35),rep(20,35),rep(21,35),
                                  rep(22,35),rep(23,35),rep(24,35),rep(25,35),rep(26,35),
                                  rep(27,35),rep(29,35),rep(30,35),rep(31,35),rep(32,35),
                                  rep(34,35),rep(35,35),rep(36,35),rep(37,35),rep(38,35),
                                  rep(39,35),rep(40,35),rep(41,35),rep(42,35),rep(43,35),
                                  rep(44,35),rep(45,35),rep(46,35),rep(49,35),rep(51,35)),
                         visit_year = rep(c(rep(2020,11),rep(2021,12),rep(2022,12)),45),
                         visit_month = rep(c(2:12,1:12,1:12),45))
  
  long_df <- df_large %>% 
    left_join(wide_df, by=join_by(csps,visit_year,visit_month)) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  
  return(long_df)
  
}


####################################################
# FUNCTION: offset_helper()
# LOCATION: file 01A
# Merge offset var + create adjusted outcome var
# input:  df  = df without offset var
# output: df2 = df with offset and adj outcome var
####################################################
offset_helper <- function (df) {
  
  df2 <- df %>% 
    left_join(visits_01A_by_csps, 
              by=join_by(csps==csps)) %>%
    mutate(n_visit_per_1000 = n_visit * 1000 / offset_tot_csps_visits)
    
  return(df2)
  
}


####################################################
# FUNCTION: new_exposure_vars_helper()
# LOCATION: file 01B
# Merge all data, create new exposure variables for
# sensitivity analyses / robustness checks
# input:  df  = df with outcome + covar data only
# output: df + 1ry exposure + addt'l exposure vars
####################################################
new_exposure_vars_helper <- function (df) {
  
  return (
    df %>%
      left_join(events_01B_variables,
                by = join_by(csps, 
                             visit_year == year, 
                             visit_month == month)) %>%
      left_join(events_01B_prior,
                by = join_by(csps, 
                             visit_date_1molag == event_date)) %>%
      relocate(event_any_prior, .after = "event_any") %>%
      mutate(
        across(c(starts_with("event_"), -event_min_dist, -event_max_dist) , 
               ~ replace_na(.x, as.integer(0))),
        event_any_cumu = 
          ifelse(event_any + event_any_prior > 0, as.integer(1), as.integer(0)),
        event_any_loc = 
          factor(event_any_loc, 
                 levels = 0:2,
                 labels = c("None", 
                            "Local (<=20 km)", 
                            "Distant (>20 km)")),
        event_any_vio = 
          factor(event_any_vio, 
                 levels = 0:2,
                 labels = c("None",
                            "Violent",
                            "Non-violent")) ) %>%
      relocate(event_any_cumu, .after = "event_any_vio")
  )
  
}


####################################################
# FUNCTION: negbin_helper()
# LOCATION: files 02A-02C
# Create negative binomial regression models
# input:  df = df with variable data
#         exposure = exposure variable name
# output: glm.nb() model
####################################################
negbin_helper <- function(df, exposure) {
  
  f <- 
    reformulate(
      c(exposure,
        "ramadan", 
        "r1h_now",
        "visit_year",
        "offset(log(offset_tot_csps_visits))"),
      response = "n_visit" )
  
  glm.nb(f, link = "log", data = df)
  
}


####################################################
# FUNCTION: bfa_map_helper()
# LOCATION: file 03B
# Create ggplot maps with event + CSPS geodata
# input:  viol_shp = shp with event data
#         csps_shp = shp with outcome name
#         max_val  = maximum fill (csps visit) value
#         pt_size  = point size (triangles/circles)
#         title    = map title
# output: tempgg   = ggplot map element
####################################################
bfa_map_helper <- function (viol_shp, 
                            csps_shp, 
                            max_val = 12400,
                            pt_size = 2.25,
                            title="") {
  
  my_breaks <- seq(0,max_val,6000)
  
  tempgg <- 
    ggplot() +
    geom_sf(data=shp_03B_kossi,lwd=0.5,fill="#fdf6eb") +
    coord_sf(crs = 4326) +
    geom_sf(data=viol_shp, aes(shape = "Event"),
            fill="#D55E00", size=pt_size) +
    geom_sf(data=csps_shp, aes(shape = "CSPS", fill=n_visit), size=pt_size) +
    scale_shape_manual(
      name = NULL,
      values = c("Event" = 21, "CSPS" = 24)
    ) +
    scale_fill_viridis(
      name = "Total CSPS Visits",
      direction = 1,
      option = "G",
      limits = c(0,max_val),
      breaks = my_breaks
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
