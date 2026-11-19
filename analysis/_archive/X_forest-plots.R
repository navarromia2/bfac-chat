

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
# 1ry analysis: Binary exposure (any event)
m1_all_any <- negbin_helper(df_all, "event_any")
m1_mal_any <- negbin_helper(df_mal, "event_any")
m1_pne_any <- negbin_helper(df_pne, "event_any")
m1_dia_any <- negbin_helper(df_dia, "event_any")

# 2ry analysis: Categorical exposure (proximity)
m2_all_any_loc <- negbin_helper(df_all, "event_any_loc")
m2_mal_any_loc <- negbin_helper(df_mal, "event_any_loc")
m2_pne_any_loc <- negbin_helper(df_pne, "event_any_loc")
m2_dia_any_loc <- negbin_helper(df_dia, "event_any_loc")

# 3ry analysis: Stratified analyses by disorder type (added in R&R)
m3_all_any_pol <- negbin_helper(df_all, "event_any_pol")
m3_all_any_dem <- negbin_helper(df_all, "event_any_dem")
m3_all_any_str <- negbin_helper(df_all, "event_any_str")

# 3ry analysis: Stratified analyses by event intensity (added in R&R)
m3_all_any_vio <- negbin_helper(df_all, "event_any_vio")


#################################
# Create forest plots
#################################
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)

# 1. Fit the models and name them
models <- list(
  "Event occurrence"   = m1_all_any,
 
  "Event intensity"    = m3_all_any_vio,
  
  "Event proximity"    = m2_all_any_loc
  
)

# 2. Extract RRs and 95% CIs for the exposure term only
rr_df <- imap_dfr(models, function(m, label) {
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl("^event_", term)) %>%
    mutate(
      model = if_else(
        term == "event_loc" | grepl("^event_loc", term),
        paste0(label, ": ", sub("^event_loc", "", term)),
        label
      )
    )
}) %>%
  mutate(model = case_when(grepl("Local", term) ~ "Event proximity (Local, <=20 km)",
                           grepl("Distant", term) ~  "Event proximity (Distant, >20 km)",
                           .default = model)
  ) %>%
  mutate(model = factor(model, levels = rev(c("Event occurrence",
                                              "Event intensity",
                                              "Event proximity (Local, <=20 km)",
                                              "Event proximity (Distant, >20 km)" ))))

rr_df

# 3. Forest plot
ggplot(rr_df, aes(x = estimate, y = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_x_log10() +
  labs(x = "IRR (95% CI)", y = NULL) +
  theme_minimal()
