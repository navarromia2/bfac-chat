## 01A
merge_long_data <- function (df,outcol) {
  
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
                         visit_month = rep(c(2:12,1:12,1:12),45)) %>%
    mutate(wetSeason = case_when(visit_month %in% c(11:12,1:3) ~ 0, .default = 1),
           isRamadan = case_when(visit_month %in% 3:4 ~ 1, .default = 0)) #%>%
  # arrange(csps, visit_year, visit_month)
  
  df2 <- df_large %>% 
    left_join(df, by=join_by(csps,visit_year,visit_month)) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  
  return(df2)
  
}

merge_offset_percap <- function (df) {
  
  df2 <- df %>% 
    left_join(csps_n_tot, by=join_by(csps==csps)) %>%
    mutate(n_visit_per_1000 = n_visit*1000/offset_n_visit_tot_csps)
    
  return(df2)
  
}

##01B

make_viol_df_long = function (dff,val_name="near_violence") {
  
  tempdf <- dff %>%
    pivot_longer(
      cols = colnames(dff)[2:46],
      names_to = "csps",
      values_to = val_name) %>%
    mutate(year = year(date),
           month = month(date)) %>%
    dplyr::select(c(2,4,5,3))
  
  tempdf$csps <- as.double(tempdf$csps)
  
  return(tempdf)
  
}

add_covariates_nmo <- function (df) {
  
  df2 <- df %>%
    mutate(wetSeason = case_when(visit_month %in% c(7:10) ~ 1, .default = 0),
           isRamadan = case_when(visit_month %in% 4:5 ~ 1, .default = 0)) 
  
  df2$n_months <- rep(1:35,length(unique(df2$csps)))
  
  return(df2)
  
}

merge_data_violcspsgps <- function (df) {
  
  df2 <- df %>% 
    left_join(df_viol4_cat,by=join_by(csps==csps, visit_year==year, visit_month==month)) %>%
    left_join(df_viol4_cont,by=join_by(csps==csps, visit_year==year, visit_month==month)) %>%
    left_join(csps_gps, by="csps") %>% 
    dplyr::filter(!csps %in% c(28,50)) #Remove those that close during follow up period.
  
  return(df2)
  
}

define_factors <- function (df) {
  
  n_na <- sum(is.na(df$near_violence))
  df$near_violence[is.na(df$near_violence)] <- rep(0,n_na)
  df$wetSeason <- factor(df$wetSeason, levels = c(0,1), labels = c("Dry","Wet"))
  df$isRamadan <- factor(df$isRamadan, levels = c(0,1), labels = c("No","Yes"))
  df$near_violence <- factor(df$near_violence, levels = c(0,1,2), labels = c("No violence",
                                                                             "Violence >20 km away",
                                                                             "Violence <=20 km away"))
  df$any_violence <- case_when(as.character(df$near_violence) == "No violence" ~ 0,
                               as.character(df$near_violence) == "Violence >20 km away" ~ 1,
                               as.character(df$near_violence) == "Violence <=20 km away" ~ 1,
                               .default = NA)
  df$any_violence <- factor(df$any_violence, levels = c(0,1), labels = c("No","Yes"))
  
  df2 <- df %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  
  return(df2)
  
}

make_viol_grps = function (df) {
  
  df2 <- df %>%
    mutate(min_dist_to_viol <- round(min_dist_to_viol, 0),
           grp_min_dist_to_viol = cut(min_dist_to_viol, 
                                      breaks = seq(min(min_dist_to_viol),max(min_dist_to_viol)+10,10),
                                      labels = 1:12))
  
  return(df2)
  
}

add_dates <- function (df) {
  
  df$visit_date <- as.Date(paste0(df$visit_month,"/01/",df$visit_year), "%m/%d/%Y")
  
  return(df)
  
}

generate_final_data = function (df) {
  
  tempdf <- 
    add_dates(
      make_viol_grps(
        define_factors(
          merge_data_violcspsgps(
            add_covariates_nmo(df)
          ))))
  
  tempdf <-
    tempdf %>%
    relocate(csps_lat, .after=csps) %>%
    relocate(csps_lon, .before=csps_lat) %>%
    relocate(visit_date, .before=visit_year) %>%
    relocate(n_months, .after=visit_month) %>%
    relocate(any_violence, .before=near_violence) %>%
    rename(min_dist_to_viol_km = min_dist_to_viol,
           min_dist_to_viol_rounded = `min_dist_to_viol <- round(min_dist_to_viol, 0)`,
           min_dist_to_viol_factor = grp_min_dist_to_viol)
    
  
  return(tempdf)
  
}
