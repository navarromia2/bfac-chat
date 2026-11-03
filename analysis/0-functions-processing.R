#########################################
# Function for data analysis
# (files 01A-01B)
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
# FUNCTION: merge_offset()
# LOCATION: file 01A
# Merge offset var + create adjusted outcome var
# input:  df  = df without offset var
# output: df2 = df with offset and adj outcome var
####################################################
merge_offset <- function (df) {
  
  df2 <- df %>% 
    left_join(visits_01A_by_csps, 
              by=join_by(csps==csps)) %>%
    mutate(n_visit_per_1000 = n_visit * 1000 / offset_tot_csps_visits)
    
  return(df2)
  
}
