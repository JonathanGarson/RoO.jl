# This code replicate the simulation results from the paper. 

include("AALA_calibration_functions.jl")

# Loading Data


# Calibration of the model 
calib_year = collect(2011:2019)
gen_figs = false
CAMUS = ["CA", "MX", "US"]
Mex_con_lib = "con"
dist_alpha = "Beta"
alpha_base = 0.15
theta_base = 4
params = Dict(:RCR => [0.625])


