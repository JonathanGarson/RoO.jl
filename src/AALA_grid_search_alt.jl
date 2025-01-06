# This code replicate the simulation results from the paper. 

using Chain
using DataFrames
using DataFramesMeta
using IterTools
using KernelDensity
using ProgressBars
using RData
using Revise
using Statistics
using Serialization

include("files_path.jl")
include("AALA_solving_alt.jl")

# loading data =============================================================
DR = DataFrame(load("$input_clean_data/AALA_rev.rds"))
DC = DataFrame(load("$input_clean_data/tau_index_DRF.rds")) # call only the relevant columns

# Calibration of the model ================================================
const calib_year = 2011:2019
const Mex_con_lib = "con"
const countries = ["CA", "MX", "US"]
const alpha_base = 0.15
const theta_base = 4.0
const RCR_pct = 0.625
const RCR = RCR_pct*100
const mu = 0.0
const sigma = 0.0
const tau = DC[!, :tauD]
const conc_err = 1e10

# Compute the data density ================================================
if Mex_con_lib == "con" 
    DR[!, :nafta_shr] = DR[!, :nafta_shr_con] 
else Mex_con_lib == "lib" 
    DR[!, :nafta_shr] = DR[!, :nafta_shr_lib]
end

DR = @rsubset(DR, :year in calib_year)
DD = @chain DR begin
    @rsubset(:ell in countries, !ismissing(:nafta_shr))  # Filter rows
    @select(:nafta_shr)
    @combine(
        :kernell_x = kde(:nafta_shr).x,
        :kernell_y = kde(:nafta_shr).density
        )                      
    end

const num_obs = size(DC, 1) # match the number of observations in the simulated data to avoid dimension mismatch

DD = clean_density_data(DD, :den_data)

# Grid search =============================================================
const mu_grid = -0.1:0.01:0.25
const sigma_grid = 0.0:0.01:0.25
const alpha_con_grid = [1,1.25,1.5,1.75,2,2.25,2.5, 3:20...]
const errcon_grid = [2:25...]

# We run the grid search
results = grid_search_loss(
    RCR = RCR_pct, 
    θ = theta_base, 
    τ_data = tau, 
    α_centre = alpha_base, 
    N = num_obs, 
    mu_grid = mu_grid, 
    sigma_grid = sigma_grid, 
    alpha_con_grid = alpha_con_grid, 
    errcon_grid = errcon_grid, 
    df_data = DD
    )
 
# We convert our results matrix into a DataFrame and look for the parameter combination that minimizes the loss function
results_t = results'
results_df = DataFrame(results_t, [:mu, :sigma, :alpha_con, :errcon, :loss])
best_params = @chain results_df begin
    sort(:loss)
    first
end

# we save the optimal parameters
optimal_params = Dict(
    :mu_opt => best_params[1],
    :sigma_opt => best_params[2],
    :alpha_con_opt => best_params[3],
    :errcon_opt => best_params[4],
    :loss_opt => best_params[5],
    :alpha_base => alpha_base,
    :theta_base => theta_base,
    :RCR => RCR_pct,
    :tau => tau,
    :num_obs => num_obs,
    :conc_err => conc_err,
    :calib_year => calib_year
    )

# save the optimal parameters
serialize("$output_parameters/optimal_params.jls", optimal_params)