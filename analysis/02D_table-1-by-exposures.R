# 02D. Comparative table of results overall, by intensity, and by proximity

# Main findings from this table:
# 1. Event occurrence, intensity, and proximity all had statistically significant associations with the outcome.
# 2. Events >20 km from CSPS had the weakest association with the outcome.
# 3. Event intensity and proximity had similar magnitudes of association with the outcome, 
#   with event intensity having a slightly greater effect estimate.
 
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



### 2A. Format results table. Download. (Disorder type)
table_3ry_all_html <- 
  
  tab_model(
    m1_all_any,
    m3_all_any_vio,
    m2_all_any_loc,
    
    show.loglik = T,
    emph.p      = F,
    
    string.est = "aIRR",
    string.ci = "95% CI",
    string.p = "p-value",

    pred.labels = c("Intercept",
                    "Any event (Yes)",
                    "Ramadan (Yes)",
                    "Rainfall (cm)",
                    "Any event (Violent)",
                    "Any event (Local, <=20 km)",
                    "Any event (Distant, >20 km)"
                    ),
    
    dv.labels = c("Event Occurrence", 
                  "Event Intensity", 
                  "Event Proximity" ) )

table_3ry_all_html

tab_model(
  m1_all_any,
  m3_all_any_vio,
  m2_all_any_loc,
  
  show.loglik = T,
  emph.p      = F,
  
  string.est = "aIRR",
  string.ci = "95% CI",
  string.p = "p-value",
  
  pred.labels = c("Intercept",
                  "Any event (Yes)",
                  "Ramadan (Yes)",
                  "Rainfall (cm)",
                  "Any event (Violent)",
                  "Any event (Local, <=20 km)",
                  "Any event (Distant, >20 km)"
  ),
  
  dv.labels = c("Event Occurrence", 
                "Event Intensity", 
                "Event Proximity" ),
  
  file = paste0(table_path,"table-1-diff-exposures.doc") )

