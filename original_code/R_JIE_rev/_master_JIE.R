# This is the version of the master file for the revised version of the JIE paper.
# Intended as a replication file.
# The R files, Tables and Plots are saved in _JIE_rev folders 

# Generate figures 2 to 5
source("R_JIE_rev/continuum_plots.R")
#
# Tau index files (Inputs are NOT in replication folder, because commercial data):
source("R_JIE_rev/sales_clean.R") # this files cleans sales data needed to generate tau index
source("R_JIE_rev/tau_index_DRF.R") # this one is intended for simulations both without (D) and with (D,R, and F) relocation. 
#outputs Data/RDS_JIE_rev/tau_index_DRF.rds, which is anonymized
#
# The tau index densities (Figure 9) are done in Julia (tau_index.jl).
#
# AALA - related files: 
source("R_JIE_rev/AALA_clean.R") # Cleans AALA data. Outputs AALA_rev.rds. 
source("R_JIE_rev/AALA_IHS_table.R") # Outputs Table 1 in the paper
source("R_JIE_rev/AALA_calibration_plots.R") # Outputs Figure 8: density plot + model-illustrative graph (calls AALA_calibration_functions.R)
#
# Main parameter estimation here ====
source("R_JIE_rev/AALA_calibration_brute_force.R") # Brute force calibration, takes around 60 min
# It saves params_opt.rds
# It calls AALA_calibration_functions.R
# It finally sources AALA_calibration_density_laffer_curves.R, to generate Figure 10 (a) and (b)  + Figure 11 (a) and (b)
#
# Laffer curve for employment ====
source("R_JIE_rev/assembly_employment.R") # Compute zeta
source("R_JIE_rev/Nafta_import_share.R") # Computes share of imports (out of Nafta origin sales in Nafta): no output, just numbers used in simulation.
source("R_JIE_rev/XL_laffer.R") # Laffer curve indexed to RCR=0 of parts share and employment changes, takes a bit of time 
source("R_JIE_rev/XL_tables.R") # Makes many tables using RDS files output by XL_laffer.R
#run XL_laffer_DRF 3 times for kappa of 10, 20, 30. be sure it is reset to 20 at the end.
rm(list=ls())
kappa_AVE <- 20
source("R_JIE_rev/XL_laffer_DRF.R") # Version with relocation and EHA, creates choice and laffer figures by HS & D (V_iso_o)
rm(list=ls())
kappa_AVE <- 10
source("R_JIE_rev/XL_laffer_DRF.R") # Version with relocation and EHA, creates choice and laffer figures by HS & D (V_iso_o)
rm(list=ls())
kappa_AVE <- 30
source("R_JIE_rev/XL_laffer_DRF.R") # Version with relocation and EHA, creates choice and laffer figures by HS & D (V_iso_o)
rm(list=ls())
kappa_AVE <- 20
source("R_JIE_rev/XL_tables_DRF.R") # Makes many tables using RDS files output by XL_laffer_DRF.R
#tables just needs to run once as it handles all three kappa settings.
source("R_JIE_rev/XL_figures_DRF.R") # Makes many figures using RDS files output by XL_laffer_DRF.R
#run once (it loops over the kappa values)
#
# Back to Nafta-wide laffer for last figure ====
source("R_JIE_rev/LT_laffer.R") # Laffer curve indexed to RCR=0 of total employment changes + table to be done
