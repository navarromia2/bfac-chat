############################
# Flow Chart Statistics
# CSPS encounter level
############################

source(here::here("analysis/0-config.R"))


############################
# Village id and names
############################
vill_id_name <- 
  read_csv(paste0(raw_data_path,"vill_codes_names.csv")) %>%
  arrange(village)

#################################
# Village population size
#################################
vill_01A_pop <- 
  read_csv(paste0(temp_data_path,"vill_01A_pop.csv"))

############################
# CHAT household census data
############################
census <- 
  read_csv(paste0(raw_data_path,"CHAT_child_census_dataset.csv"))

############################
# CSPS geodata + visit data
############################
vill_csps_gps <- 
  read_csv(paste0(raw_data_path,"vill_csps_gps.csv"), 
           col_types = cols(csps = col_integer(), 
                            village = col_integer()))

############################
# CSPS surveillance data
############################
passive <- 
  read_csv(paste0(raw_data_path,"passive (1).csv"))




#################################
# Create dx and date variables
#################################
visit_04A_labelled <- 
  passive %>%
  mutate(visit_date = as.Date(starttime),
         visit_year = year(starttime),
         visit_month = month(starttime),
         dx_type = case_when(diagnosis__57 == 1 ~ 1, #sev malaria
                             diagnosis__58 == 1 ~ 1, #non-sev malaria
                             diagnosis__39 == 1 ~ 2, #broncho-pneum
                             diagnosis__40 == 1 ~ 2, #low pneum
                             diagnosis__64 == 1 ~ 2, #pneum
                             diagnosis__21 == 1 ~ 3, #diarrhea
                             .default = 0)) 

View(visit_04A_labelled)
unique(visit_04A_labelled$csps_name)

#################################
# Merge CSPS and village dem data
#################################
visit_04A_merged <- 
  visit_04A_labelled %>%
  left_join(vill_01A_pop, 
            by=join_by(commune,
                         visit_year == census_year, 
                         visit_month == census_month)
            )

#################################
# Apply ineligibility and 
# exclusion criteria
#################################
visit_04A_eligible <- 
  visit_04A_merged %>% 
  dplyr::filter(!grepl("Outside Nouna",csps_name)) %>%
  dplyr::filter(between(starttime, 
                         as.Date("2020-02-01"), 
                         as.Date("2022-12-31")))  #too early/late

nrow(visit_04A_eligible) #N = 201,856 eligible
View(visit_04A_eligible)

visit_04A_analytic <-
  visit_04A_eligible %>%
  mutate(csps = if_else(csps<0, NA, csps)) %>%
  mutate(village = if_else(`village.x`<0, NA, `village.x`)) %>%
  mutate(village = if_else(`village.x`>990, 9999, `village.x`)) %>%
  dplyr::filter(!is.na(csps_name)) %>%
  dplyr::filter(!is.na(village)) %>%
  dplyr::filter(village!=9999)


#################################
# Generate counts for flow chart
# (patients)
#################################
length(unique(visit_04A_merged$childid))   # N = 8166 pts in full sample
length(unique(visit_04A_eligible$childid)) # N = 8138 pts in eligible sample
length(unique(visit_04A_analytic$childid)) # N = 6892 pts in analytic sample


#################################
# Generate counts for flow chart
# (encounter)
#################################
nrow(visit_04A_merged)   # N = 232,095 full sample
nrow(visit_04A_eligible) # N = 201,856 eligible
nrow(visit_04A_analytic) # N = 173,788 included


# N = 26,956 outside of Nouna
nrow(visit_04A_merged %>% 
       dplyr::filter(grepl("Outside Nouna",csps_name))
     )

# N = 3,281 outside of study period
nrow(visit_04A_merged %>% 
       dplyr::filter(!between(starttime, 
                              as.Date("2020-02-01"), 
                              as.Date("2022-12-31")))
     )

# N = 19 missing CSPS ids
nrow(visit_04A_eligible %>%
       mutate(csps = if_else(csps<0, NA, csps)) %>%
       mutate(village = if_else(`village.x`<0, NA, `village.x`)) %>%
       mutate(village = if_else(`village.x`>990, 9999, `village.x`)) %>%
       dplyr::filter(is.na(csps_name))
       # dplyr::filter(!is.na(village)) %>%
       # dplyr::filter(village!=9999)
)


# N = 19 missing village ids
nrow(visit_04A_eligible %>%
       mutate(csps = if_else(csps<0, NA, csps)) %>%
       mutate(village = if_else(`village.x`<0, NA, `village.x`)) %>%
       mutate(village = if_else(`village.x`>990, 9999, `village.x`)) %>%
       # dplyr::filter(!is.na(csps_name)) %>%
       dplyr::filter(is.na(village))
       # dplyr::filter(village!=9999)
)

# N = 28,049 "other" village ids
nrow(visit_04A_eligible %>%
       mutate(csps = if_else(csps<0, NA, csps)) %>%
       mutate(village = if_else(`village.x`<0, NA, `village.x`)) %>%
       mutate(village = if_else(`village.x`>990, 9999, `village.x`)) %>%
       # dplyr::filter(!is.na(csps_name)) %>%
       # dplyr::filter(!is.na(village)) %>%
       dplyr::filter(village==9999)
)


table(visit_04A_eligible$village)




