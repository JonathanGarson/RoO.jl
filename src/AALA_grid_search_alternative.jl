# This code replicate the simulation results from the paper. 
# ajouter des constantes

using Chain
using DataFrames
using DataFramesMeta
using IterTools
using KernelDensity
using ProgressBars
using RData
using Revise
using Statistics

include("AALA_solving_model_alternative.jl")

# loading data =============================================================
DR = DataFrame(load("data/RDS_JIE_rev/AALA_rev.rds"))
DC = DataFrame(load("data/RDS_JIE_rev/tau_index_DRF.rds")) # call only the relevant columns

# Calibration of the model ================================================
const calib_year = 2011:2019
const Mex_con_lib = "con"
const CAMUS = ["CA", "MX", "US"]
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
    @rsubset(:ell in CAMUS, !ismissing(:nafta_shr))  # Filter rows
    @select(:nafta_shr)
    @combine(
        :kernell_x = kde(:nafta_shr).x,
        :kernell_y = kde(:nafta_shr).density
        )                      
    end

const num_obs = size(DC, 1) # match the number of observations in the simulated data to avoid dimension mismatch

DD = clean_density_data(DD, :den_data)

# Grid search =============================================================
mu_grid = collect(-0.1:0.01:0.25) # pas besoin collect
sigma_grid = collect(0.0:0.01:0.25)
alpha_con_grid = [1,1.25,1.5,1.75,2,2.25,2.5, 3:20...]
errcon_grid = [2:25...]

# We run the grid search
@time results = grid_search_loss(
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
