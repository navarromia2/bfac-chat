#################################
# BFAC-run-all.R
#
# Run all analysis scripts
#
# there are 12 scripts, all 
# in R or R markdown in:
# BFAC-projects/analysis
#
# output is saved in:
# BFAC-projects/results
#################################

#-----------------------------
# preamble 
# source configuration file
#-----------------------------
library(here)
# source(here("R/0-AVENIR-primary-Config.R"))

#################################
# Prepare data for analysis
#################################
rmarkdown::render(here::here("analysis/01A_clean-outcome-covar.Rmd"))
rmarkdown::render(here::here("analysis/01B_clean-exposure.Rmd"))

#################################
# Figure 1
# Temporal graph of monthly
# visits during study period
#################################
rmarkdown::render(here::here("analysis/03A_fig-1-temporal-graph.Rmd"))

#################################
# Figure 2
# Spatiotemporal maps of
# mo visits and violent events
#################################
rmarkdown::render(here::here("analysis/03B_fig-2-spatiotemporal-map.Rmd"))

#################################
# Table 1
# Effect of violence 
# on monthly visit IRRs
#################################
rmarkdown::render(here::here("analysis/02D_table-1-by-exposures.Rmd"))

#################################
# Table 2
# Effect of violence 
# on monthly visit IRRs (by dx)
#################################
rmarkdown::render(here::here("analysis/02E_table-2-by-outcomes.Rmd"))


#################################
# session info
#################################
sessionInfo()