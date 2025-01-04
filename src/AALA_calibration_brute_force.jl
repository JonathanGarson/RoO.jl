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

include("AALA_calibration_functions.jl")
calibration_functions = AALA_calibration_functions

# loading data =============================================================
DR = DataFrame(load("data/RDS_JIE_rev/AALA_rev.rds"))
DC = DataFrame(load("data/RDS_JIE_rev/tau_index_DRF.rds")) # call only the relevant columns

# Calibration of the model ================================================
calib_year = collect(2011:2019)
gen_figs = false
CAMUS = ["CA", "MX", "US"]
Mex_con_lib = "con"
dist_alpha = "Beta"
alpha_base = 0.15
theta_base = 4
RCR_pct = [0.625]
RCR = RCR_pct*100
mu = 0.0
sigma = 0.0
tau = DC[!, :tauD]
tauQ = 1
conc_err = 1e10

# Compute the data density ================================================
# Choose the conservative or liberal going forward, and compute data density, also load tau index
if Mex_con_lib == "con" 
    DR[!, :nafta_shr] = DR[!, :nafta_shr_con] 
else Mex_con_lib == "lib" 
    DR[!, :nafta_shr] = DR[!, :nafta_shr_lib]
end

DD = @chain DR begin
    @rsubset(:ell in CAMUS, !ismissing(:nafta_shr))  # Filter rows
    @select(:nafta_shr)
    @combine(
        :kernell_x = kde(:nafta_shr).x,
        :kernell_y = kde(:nafta_shr).density
        )                      
    end
    
num_obs = size(DD, 1) # 2048 (300 more than in the original paper but no substantial differences in the results)

DD = calibration_functions.clean_density_data(DD, :den_data)

# We store in a dictionnary the different parameters of calibration 
calib_param = Dict(
    :RCR => RCR_pct,
    :theta => theta_base,
    :mu => mu,
    :sigma => sigma,
    :tau => tau,
    :tauQ => tauQ,
    :mu_alpha => alpha_base,
    :conc_err => conc_err,
    :num_obs => num_obs
    )

#  We create the grid for the calibration =================================
mu_grid = collect(-0.1:0.01:0.25)
sigma_grid = collect(0.0:0.01:0.25)
alpha_con_grid = [1,1.25,1.5,1.75,2,2.25,2.5, 3:20...]
errcon_grid = [2:25...]

grid_param = IterTools.product(sigma_grid, alpha_con_grid, errcon_grid)

# MODEL RESOLUTION ========================================================

# Initialize the dataframe to store the results
results = DataFrame(
    mu = Float64[],
    sigma = Float64[],
    alpha = Float64[],
    conc_err = Float64[],
    loss = Float64[]
)

n_total = length(mu_grid)*length(grid_param)

# Loop over mu and the grid
for mu in mu_grid
    for (sigma, alpha, conc_err) in grid_param
       for i in ProgressBar(1:n_total)
            param_batch = Dict(:sigma => sigma, :conc_alpha => alpha, :conc_err => conc_err)
            loss = calibration_functions.loss_fun_alpha(mu=mu, grid_param=param_batch, calib_param=calib_param, DD=DD)
            push!(results, [mu, sigma, alpha, conc_err, loss])
        end
    end
end
