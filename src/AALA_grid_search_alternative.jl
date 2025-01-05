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

include("AALA_solving_model_alternative.jl")
alter_solving = AALA_solving_model_alternative

# loading data =============================================================
DR = DataFrame(load("data/RDS_JIE_rev/AALA_rev.rds"))
DC = DataFrame(load("data/RDS_JIE_rev/tau_index_DRF.rds")) # call only the relevant columns

# Calibration of the model ================================================
calib_year = collect(2011:2019)
Mex_con_lib = "con"
CAMUS = ["CA", "MX", "US"]
alpha_base = 0.15
theta_base = 4.0
RCR_pct = 0.625
RCR = RCR_pct*100
mu = 0.0
sigma = 0.0
tau = DC[!, :tauD]
conc_err = 1e10

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

# num_obs = size(DD, 1)
num_obs = 1758 # match the number of observations in the simulated data to avoid dimension mismatch

DD = alter_solving.clean_density_data(DD, :den_data)

# Grid search =============================================================
mu_grid = collect(-0.1:0.01:0.25)
sigma_grid = collect(0.0:0.01:0.25)
alpha_con_grid = [1,1.25,1.5,1.75,2,2.25,2.5, 3:20...]
errcon_grid = [2:25...]

# grid_param = IterTools.product(mu_grid, sigma_grid, alpha_con_grid, errcon_grid)

# # solving the model for each parameter combination
# # first we store in a matrix the results of the simulation for each parameter combination

# n_total = length(mu_grid) * length(sigma_grid) * length(alpha_con_grid) * length(errcon_grid)
# results = zeros(n_total, 5)

results = alter_solving.grid_search_loss(
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
results_df = DataFrame(results, [:mu, :sigma, :alpha_con, :errcon, :loss])
best_params = @chain results_df begin
    sort(:loss)
    first
end
