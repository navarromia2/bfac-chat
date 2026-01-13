#-----------------------------
# BFAC-run-all.R
#
# Run all analysis scripts
#
# there are 6 scripts, all 
# in R markdown in:
# BFAC-projects/analysis
#
# output is saved in:
# BFAC-projects/results
#-----------------------------

#-----------------------------
# preamble 
# source configuration file
#-----------------------------
library(here)
source(here("R/0-AVENIR-primary-Config.R"))

#-----------------------------
# Prepare data for analysis
#-----------------------------
rmarkdown::render(here::here("analysis/01A_clean-outcome-cov.Rmd"))
rmarkdown::render(here::here("analysis/01B_clean-exposure.Rmd"))

#-----------------------------
# Figure 1
# Temporal graph of monthly
# visits during study period
#-----------------------------
rmarkdown::render(here::here("analysis/03A_fig-1-temporal-graph.Rmd"))

#-----------------------------
# Figure 2
# Spatiotemporal maps of
# mo visits and violent events
#-----------------------------
rmarkdown::render(here::here("analysis/03B_fig-2-spatiotemporal-map.Rmd"))

#-----------------------------
# Table 1
# Effect of any violence 
# on monthly visit IRRs
#-----------------------------
rmarkdown::render(here::here("analysis/02A_tab-1-pois-any-violence.Rmd"))

#-----------------------------
# Table 2
# Effect of local violence 
# on monthly visit IRRs
#-----------------------------
rmarkdown::render(here::here("analysis/02B_tab-2-pois-near-violence.Rmd"))


#-----------------------------
# session info
#-----------------------------
sessionInfo()