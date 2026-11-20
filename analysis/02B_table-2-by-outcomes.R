##############################################
# 02B. Comparative table of results by 
# overall & diagnosis-specific CSPS visits
##############################################


############################
# Configure workspace
############################
source(here::here("analysis/0-config.R"))

############################
# File 01B data
############################
df_all <- readRDS(paste0(final_data_path,"df_01B_all.rds"))
df_mal <- readRDS(paste0(final_data_path,"df_01B_mal.rds"))
df_pne <- readRDS(paste0(final_data_path,"df_01B_pne.rds"))
df_dia <- readRDS(paste0(final_data_path,"df_01B_dia.rds"))

#################################
# Create negative binomial
# regression models across diff
# exposure variables.
# (See 0-functions-analysis.R
# for helper function details.)
#################################
# 1ry analysis: Overall
m1_all_any <- negbin_helper(df_all, "event_any")

# 2ry analysis: By diagnosis (any event)
m1_mal_any <- negbin_helper(df_mal, "event_any")
m1_pne_any <- negbin_helper(df_pne, "event_any")
m1_dia_any <- negbin_helper(df_dia, "event_any")

# 2ry analysis: Event proximity (added in R&R 1)
m2_all_any_loc <- negbin_helper(df_all, "event_any_loc")

# 3ry analysis: Event intensity (added in R&R 1)
m3_all_any_vio <- negbin_helper(df_all, "event_any_vio")

#################################
# Look at thetas of ea. model.
# A value of ~0.5 suggests
# overdispersion and "justifies"
# using negative binomial.
#################################
m1_all_any$theta  # 0.20
m1_mal_any$theta  # 0.20
m1_pne_any$theta  # 0.19
m1_dia_any$theta  # 0.20

# Evidence of strong overdispersion.

#################################
# Preview Table 2 (by diagnosis)
#################################
### 2A. Format results table. Download. (Visit type)
table_2ry_dx_html <- 
  
  tab_model(
    m1_all_any,
    m1_mal_any,
    m1_pne_any,
    m1_dia_any,
    
    show.loglik = T,
    emph.p      = F,
    
    string.est = "aIRR",
    string.ci = "95% CI",
    string.p = "p-value",
    
    rm.terms = c("(Intercept)"),
    
    pred.labels = c(
                    # "Intercept",
                    "Any event (Yes)",
                    "Ramadan (Yes)",
                    "Rainfall (cm)",
                    "Year"
                    ),
    
    dv.labels = c("Overall", 
                  "Malaria Visits", 
                  "Pneumonia Visits",
                  "Diarrhea Visits") )

table_2ry_dx_html

tab_model(
  m1_all_any,
  m1_mal_any,
  m1_pne_any,
  m1_dia_any,
  
  show.loglik = T,
  emph.p      = F,
  
  string.est = "aIRR",
  string.ci = "95% CI",
  string.p = "p-value",
  
  rm.terms = c("(Intercept)"),
  
  pred.labels = c(
                    # "Intercept",
                    "Any event (Yes)",
                    "Ramadan (Yes)",
                    "Rainfall (cm)",
                    "Year"
                    ),
  
  dv.labels = c("Overall", 
                "Malaria Visits", 
                "Pneumonia Visits",
                "Diarrhea Visits"),
  
  file = paste0(table_path,"table-2-diff-outcomes.doc") 
  )
