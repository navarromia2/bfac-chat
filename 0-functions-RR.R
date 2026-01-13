####For the R&R

replaceNAs <- function (df) {
  
  tempdf <- df %>%
    mutate(is_near_csps = ifelse(is.na(is_near_csps),0,is_near_csps),
           n_near_csps = ifelse(is.na(n_near_csps),0,n_near_csps),
           n_fat_near_csps = ifelse(is.na(n_fat_near_csps),0,n_fat_near_csps),
           any_PV = ifelse(is.na(any_PV),0,any_PV),
           any_SD = ifelse(is.na(any_SD),0,any_SD),
           any_D = ifelse(is.na(any_D),0,any_D)
    )
  
  return(tempdf)
    
}

create_pois_any_RR <- function (df) {
  m <- glm(n_visit ~ any_violence + 
             isRamadan + r1h_now + 
             n_near_csps + n_fat_near_csps,
           # + any_PV + any_SD + any_D, 
           offset = log(offset_n_visit_tot_csps), 
           family = poisson(link = "log"), 
           data = df)
  
  return(m)
}

create_pois_near_RR <- function (df) {
  m <- glm(n_visit ~ near_violence + 
             isRamadan + r1h_now + 
             n_near_csps + n_fat_near_csps,
           # + any_PV + any_SD + any_D, 
           offset = log(offset_n_visit_tot_csps), 
           family = poisson(link = "log"), 
           data = df)
  
  return(m)
}

create_nb_any_RR <- function (df) {
  m <- glm.nb(n_visit ~ any_violence + 
                isRamadan + r1h_now + 
                n_near_csps + n_fat_near_csps +
                offset(log(offset_n_visit_tot_csps)), 
              link = "log", 
              data = df)
  
  return(m)
}

create_nb_near_RR <- function (df) {
  m <- glm.nb(n_visit ~ near_violence + 
                isRamadan + r1h_now + 
                n_near_csps + n_fat_near_csps +
                offset(log(offset_n_visit_tot_csps)), 
              link = "log", 
              data = df)
  
  return(m)
}


create_table_1_RR <- function (create_doc=T) {
  
  if (create_doc) {
    
    fulltab_any <- tab_model(m1_pois_any_RR,m2_mal_pois_any_RR,m2_pne_pois_any_RR,m2_dia_pois_any_RR,
                             show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                             # pred.labels = c("Intercept",
                             #                 "Violence",
                             #                 "Ramadan",
                             #                 "Rainfall (mm)",
                             #                 "Local Violent Events (n)",
                             #                 "Local Political Violence (n)",
                             #                 "Local Strategic Developments (n)",
                             #                 "Local Demonstrations (n)"
                             #                 ),
                             dv.labels = c("Total Visits", 
                                           "Malaria Visits", 
                                           "Pneumonia Visits", 
                                           "Diarrhea Visits"),
                             file = paste0(table_path,"table-1-pois-RR.doc"))
    
  } else {
    
    fulltab_any <- tab_model(m1_pois_any_RR,m2_mal_pois_any_RR,m2_pne_pois_any_RR,m2_dia_pois_any_RR,
                             show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                             # pred.labels = c("Intercept",
                             #                 "Violence",
                             #                 "Ramadan",
                             #                 "Rainfall (mm)",
                             #                 "Local Violent Events (n)",
                             #                 "Local Political Violence (n)",
                             #                 "Local Strategic Developments (n)",
                             #                 "Local Demonstrations (n)"
                             # ),
                             dv.labels = c("Total Visits", 
                                           "Malaria Visits", 
                                           "Pneumonia Visits", 
                                           "Diarrhea Visits"))
    
  }
  
  return(fulltab_any)
  
}

create_table_2_RR <- function (create_doc=T) {
  
  if (create_doc) {
    
    fulltab_near <- tab_model(m1_pois_near2,m2_mal_pois_near2,m2_pne_pois_near2,m2_dia_pois_near2,
                              show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                              # pred.labels = c("Intercept",
                              #                 "Violence: Local (<=20 km)",
                              #                 "Violence: Distant (>20 km)",
                              #                 "Ramadan",
                              #                 "Rainfall (mm)",
                              #                 "Local Violent Events (n)",
                              #                 "Local Political Violence (n)",
                              #                 "Local Strategic Developments (n)",
                              #                 "Local Demonstrations (n)"
                              #                 ),
                              dv.labels = c("Total Visits", 
                                            "Malaria Visits", 
                                            "Pneumonia Visits", 
                                            "Diarrhea Visits"),
                              file = paste0(table_path,"table-2-pois-RR.doc"))
    
  } else {
    
    fulltab_near <- tab_model(m1_pois_near2,m2_mal_pois_near2,m2_pne_pois_near2,m2_dia_pois_near2,
                              show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                              # pred.labels = c("Intercept",
                              #                 "Violence: Local (<=20 km)",
                              #                 "Violence: Distant (>20 km)",
                              #                 "Ramadan",
                              #                 "Rainfall (mm)",
                              #                 "Local Violent Events (n)",
                              #                 "Local Political Violence",
                              #                 "Local Strategic Developments",
                              #                 "Local Demonstrations"
                              # ),
                              dv.labels = c("Total Visits", 
                                            "Malaria Visits", 
                                            "Pneumonia Visits", 
                                            "Diarrhea Visits"))
    
  }
  
  return(fulltab_near)
  
}


create_table_1_nb_RR <- function (create_doc=T) {
  
  if (create_doc) {
    
    fulltab_any <- tab_model(m1_nb_any_RR,m2_mal_nb_any_RR,m2_pne_nb_any_RR,m2_dia_nb_any_RR,
                             show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                             # pred.labels = c("Intercept",
                             #                 "Violence",
                             #                 "Ramadan",
                             #                 "Rainfall (mm)",
                             #                 "Local Violent Events (n)",
                             #                 "Local Political Violence (n)",
                             #                 "Local Strategic Developments (n)",
                             #                 "Local Demonstrations (n)"
                             #                 ),
                             dv.labels = c("Total Visits", 
                                           "Malaria Visits", 
                                           "Pneumonia Visits", 
                                           "Diarrhea Visits"),
                             file = paste0(table_path,"table-1-nb-RR.doc"))
    
  } else {
    
    fulltab_any <- tab_model(m1_nb_any_RR,m2_mal_nb_any_RR,m2_pne_nb_any_RR,m2_dia_nb_any_RR,
                             show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                             # pred.labels = c("Intercept",
                             #                 "Violence",
                             #                 "Ramadan",
                             #                 "Rainfall (mm)",
                             #                 "Local Violent Events (n)",
                             #                 "Local Political Violence (n)",
                             #                 "Local Strategic Developments (n)",
                             #                 "Local Demonstrations (n)"
                             # ),
                             dv.labels = c("Total Visits", 
                                           "Malaria Visits", 
                                           "Pneumonia Visits", 
                                           "Diarrhea Visits"))
    
  }
  
  return(fulltab_any)
  
}

create_table_2_nb_RR <- function (create_doc=T) {
  
  if (create_doc) {
    
    fulltab_near <- tab_model(m1_nb_near2,m2_mal_nb_near2,m2_pne_nb_near2,m2_dia_nb_near2,
                              show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                              # pred.labels = c("Intercept",
                              #                 "Violence: Local (<=20 km)",
                              #                 "Violence: Distant (>20 km)",
                              #                 "Ramadan",
                              #                 "Rainfall (mm)",
                              #                 "Local Violent Events (n)",
                              #                 "Local Political Violence (n)",
                              #                 "Local Strategic Developments (n)",
                              #                 "Local Demonstrations (n)"
                              #                 ),
                              dv.labels = c("Total Visits", 
                                            "Malaria Visits", 
                                            "Pneumonia Visits", 
                                            "Diarrhea Visits"),
                              file = paste0(table_path,"table-2-nb-RR.doc"))
    
  } else {
    
    fulltab_near <- tab_model(m1_nb_near2,m2_mal_nb_near2,m2_pne_nb_near2,m2_dia_nb_near2,
                              show.loglik=T,string.est = "IRR",string.ci = "95% CI",
                              # pred.labels = c("Intercept",
                              #                 "Violence: Local (<=20 km)",
                              #                 "Violence: Distant (>20 km)",
                              #                 "Ramadan",
                              #                 "Rainfall (mm)",
                              #                 "Local Violent Events (n)",
                              #                 "Local Political Violence",
                              #                 "Local Strategic Developments",
                              #                 "Local Demonstrations"
                              # ),
                              dv.labels = c("Total Visits", 
                                            "Malaria Visits", 
                                            "Pneumonia Visits", 
                                            "Diarrhea Visits"))
    
  }
  
  return(fulltab_near)
  
}


## 03. figures
create_jitter_ggplot_RR <- function (df, plotlab="Plot", 
                                  xlab2="Time", 
                                  ylab2="Visits per 1000 Residents",
                                  ymaxval2=250) {
  
  tempplot <- ggplot() +
    
    # annotate("rect", xmin = as.Date("2020-07-01"), xmax = as.Date("2020-11-01"), 
    #          ymin = 0, ymax = ymaxval2, fill = "gray80", alpha=0.5) +
    # annotate("rect", xmin = as.Date("2021-07-01"), xmax = as.Date("2021-11-01"), 
    #          ymin = 0, ymax = ymaxval2, fill = "gray80", alpha=0.5) +
    # annotate("rect", xmin = as.Date("2022-07-01"), xmax = as.Date("2022-11-01"), 
    #          ymin = 0, ymax = ymaxval2, fill = "gray80", alpha=0.5) +
    
    annotate("rect", xmin = as.Date("2020-04-01"), xmax = as.Date("2020-06-01"), 
             ymin = 0, ymax = ymaxval2, fill = "gray60", alpha=0.5) +
    annotate("rect", xmin = as.Date("2021-04-01"), xmax = as.Date("2021-06-01"), 
             ymin = 0, ymax = ymaxval2, fill = "gray60", alpha=0.5) +
    annotate("rect", xmin = as.Date("2022-04-01"), xmax = as.Date("2022-06-01"), 
             ymin = 0, ymax = ymaxval2, fill = "gray60", alpha=0.5) +
    
    geom_point(data=df, aes(x=visit_date, y=n_visit_per_1000, group=near_violence, color=near_violence,shape=near_violence), alpha=0.4, position = "jitter") +
    geom_smooth(data=df, aes(x=visit_date, y=n_visit_per_1000, group=near_violence, color=near_violence,shape=near_violence), method="loess", se=F, fullrange=T) +
    scale_y_continuous(limits = c(0,ymaxval2)) +
    
    geom_line(data=df, aes(x=visit_date, y=r1h_now/100, color=" Rainfall (0.01mm)")) + 
    
    scale_color_manual(values=c("blue", "black", "red", "red4")) +
    
    labs(title = plotlab, x = xlab2, y = ylab2, colour="Color", shape="Shape") +
    theme(legend.position="none") + theme_bw()
  
  return(tempplot)
  
}
