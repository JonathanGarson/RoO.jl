# This file contains the functions used to calibrate the AALA model.
module AALA_calibration_functions

using Chain
using Distributions
using DataFrames
using DataFramesMeta
using KernelDensity
using LinearAlgebra
using Roots
using Random
using StatsBase

# BASE FUNCTIONS ==============================================================

#lambda_R as a function of RCR (expressed as cost share incl assembly), for a given alpha  
function lambda_RCR(RCR::Float64, alpha::Union{Float64, AbstractVector{Float64}})
    if isa(alpha, AbstractVector)
        # Handle vector input
        return [0.0 <= a < RCR ? (RCR - a) / (1 - a) : 0.0 for a in alpha]
    else
        # Handle scalar input
        return 0.0 <= alpha < RCR ? (RCR - alpha) / (1 - alpha) : 0.0
    end
end

#Je pense que cette fonction ne concerne pas des vecteurs, 
# sinon i faudrait ajouter des broadcasts

# chi_R as a function  of lambda_R 
function chi_lambda(lambda_R::Union{Float64, AbstractVector{Float64}}, delta::Union{Float64, AbstractVector{Float64}}, theta::Float64)
    # Compute denom element-wise
    denom = 1 .+ ((1 ./ lambda_R .- 1) ./ delta) .^ (theta / (theta + 1))

    # Return element-wise result
    return 1 ./ denom
end

# Unconstrained parts share
function chi_U(delta::Union{Float64, AbstractVector{Float64}}, theta::Float64)
    return 1 ./ (1 .+ delta.^(-theta))
end

# Unconstrained costs share : uses the EK miracle 
const lambda_U = chi_U

# #Analytic density of chi_U (and lambda_U) for unconstrained firms
function pdf_U(x::Union{Real, AbstractVector}, theta::Float64, mu::Float64, sigma::Float64; pct::Bool=true)
    # Convert scalar to vector if needed
    x = typeof(x) <: AbstractVector ? x : [x]

    # Scale x if expressed in percentage
    if pct
        x .= x ./ 100  # Element-wise division
    end

    # Transform x to y
    y = x ./ (1 .- x)  # Element-wise operations

    # Compute the density
    g = pdf.(LogNormal(mu, sigma), y .^ (1 / theta)) .* (1 ./ (theta .* x .^ 2)) .* y .^ (1 + 1 / theta)

    # Return scaled density if pct is TRUE
    return pct ? g ./ 100 : g
end
# Je pense que c'est une option pour exprimer les résultats en pourcentage mais je trouve ça étrange
# Je pense qu'on devrait typer nos outputs pour éviter les erreurs

# Unconstrained cost : Compute the index using chi_U
function C_U(delta::Union{Float64, AbstractVector{Float64}}, theta::Float64)
    index = chi_U(delta, theta).^(1 / theta)
    return index
end

# Cost of compliance 
# function C_comply(lambda_R::AbstractVector, delta::Float64, theta::Float64)
#     chi_R = chi_lambda.(lambda_R, delta, theta)
#     k = (1 + theta) / theta
#     return chi_R.^k .+ delta .* (1 .- chi_R).^k
# end

function C_comply(lambda_R::Union{Float64, AbstractVector{Float64}}, delta::Union{Float64, AbstractVector{Float64}}, theta::Float64)
    # Ensure lambda_R is always treated as a vector
    chi_R = chi_lambda.(lambda_R, delta, theta)  # Broadcasting over vector
    k = (1 + theta) / theta
    return chi_R.^k .+ delta .* (1 .- chi_R).^k
end

# C.tilde is C_comply / C_U
function C_tilde(lambda_R::Union{Float64, AbstractVector{Float64}}, delta::Union{Float64, AbstractVector{Float64}}, theta::Float64)
    # Compute the threshold lambda_U
    lambda_u = lambda_U(delta, theta)  # lambda_U is chi_U in Julia
    
    # Compute compliance condition
    cons = lambda_R .> lambda_u  # Element-wise comparison for vectors
    
    # Compute C_comply and C_U
    comply_values = C_comply.(lambda_R, delta, theta) ./ C_U(delta, theta)
    
    # Combine results
    y = cons .* comply_values .+ (.!cons .* 1)  # Element-wise logical negation for (1 - cons)
    return y
end

# Limit of delta star as lambda_R -> 1 (in paper lambda_R -> chi_R, delta_max -> \bar{\delta}(\tau))
function delta_max(tau::Float64, theta::Float64)
    return (tau.^theta - 1).^(-1 / theta)
end

# Cutoff delta for complying
function delta_star(lambda_R::Union{Float64, AbstractVector{Float64}}, tau::Float64, theta::Float64)
    # Define the auxiliary function ufn
    ufn = x -> C_tilde(lambda_R, x, theta) - tau
    
    # Calculate the maximum delta (upper bound of the interval)
    dmax = delta_max(tau, theta)
    
    # Find the root of the equation ufn(x) = 0 in the interval [0.00001, dmax]
    return find_zero(ufn, (0.00001, dmax))
end

# Cutoff delta for complying - unconstrained
function delta_circ(lambda_R::Union{Float64, AbstractVector{Float64}})
    return (lambda_R.^(-1)-1).^(-1/theta-10)
end # module

# Function for generating heterogenous alpha constrained between 0 and 1 ======
function beta_draws(N::Int64, centre::Float64, concentration::Float64)
    # Set the parameters & limits
    a = 0
    c = 2 * centre 
    mu = centre/c
    x = rand(Beta(mu * concentration, (1 - mu) * concentration), N)
    return a .+ (c - a) .* x
end 

# Function for generating distributions bounded between 0 and 1 =================
function ubeta_draws(N::Int64, centre::Float64, concentration::Float64)
    # Validate that centre is less than 1
    if centre >= 1
        throw(ArgumentError("The parameter `centre` must be less than 1, but got $centre."))
    end

    # Generate random values from Beta distribution
    return rand(Beta(centre * concentration, (1 - centre) * concentration), N)
end

# TEST ==========================================================
# Lambda simulations simplest version =========================================

function sim_lambda(RCR, mu, sigma, theta, tau_data, alpha_lo, alpha_hi, alpha_a, alpha_b, N)
    #  Draw the three dimensions of heterogeneity : 
    # - delta : substituability of foreing vers domestic imput 
    # - alpha : share of local production
    # - lambda_R : cost share of compliance to RCR (regional content ratio)
    delta = rand(LogNormal(mu, sigma), N) #always firm specific
    alpha = alpha_lo .+ rand(Beta(alpha_a, alpha_b), N) .* (alpha_hi - alpha_lo)
    lambda_R = lambda_RCR(RCR, alpha) # firm specific if alpha is heterogenous

    # Compute firm choices given constraints
    if length(tau_data) == 1
        tau = tau_data[1]  # Use the single value directly
    else
        tau = rand(tau_data, N)  # Sample N values from tau_data
    end

    # Compute the parameter lambda_model
    lambda_u = lambda_U(delta, theta)
        
    comply_cost = C_tilde(lambda_R, delta, theta)

    comply_con = (comply_cost .<= tau) .& (lambda_u .<= lambda_R) # comply if cost penalty < tariff penalty and compliance is constrained if ideal lambda < required lambda
    
    # Compute the lambda model
    lambda_model = lambda_R .* Int.(comply_con) .+ lambda_u .* (1 .- Int.(comply_con)) # if comply_con is true, lambda_model = lambda_R, else lambda_model = lambda_U
    
    RCS = alpha .* (1 .- lambda_model) .+ lambda_model # regional content share 

    # Store the results in a dictionary and return it
    return result = Dict(
    :lambda_U => lambda_u,
    :lambda_model => lambda_model,
    :lambda_R => lambda_R,
    :RCS => RCS,
    :comply_cost => comply_cost,
    :comply_frac => mean(comply_con),
    :alpha => alpha,
    :alpha_rng => extrema(alpha),
    :delta_rng => extrema(delta),
    :tau_rng => extrema(tau)
) 
end

# Seems to work

# MAIN FUNCTIONS ===============================================================
# This is the main function which gets used for two purposes:
# 1. Calibration of the model to lambda
# 2. Welfare calculations using chi 

# Main function for simulating lambda and alpha =================================
function sim_lambda_alpha(RCR, mu, sigma, theta, tau_data, mu_alpha, conc_alpha, conc_err, N)
# We draw the three dimensions of heterogeneity :
# - delta : substituability of foreing vers domestic imput
# - tau, tauQ : tariff from imports
# - alpha : share of local production before measure
    delta = rand(LogNormal(mu, sigma), N) #always firm specific
    tauDT = tau_data[sample(1:length(tau_data), N, replace=true)] #always firm specific
    tau = select(tauDT, "tau") # not sure about this part
    tauQ = select(tauDT, "tauQ") # not sure about this part 
    alpha = ubeta_draws(N, mu_alpha, conc_alpha)

    # Compute costs
    lambda_R = lambda_RCR(RCR, alpha) # firm specific if alpha is heterogenous
    lambda_U = lambda_U(delta, theta)
    C_R = C_comply(lambda_R, delta, theta)
    C_u = C_U(delta, theta)
    comply_cost = C_tile(lambda_R, delta, theta).^(1-alpha) #modified version from the authors (l.133)

    # Condition compliance
    comply_con = (comply_cost .<= tau) .& (lambda_U .<= lambda_R)
    comply_uncon = lambda_U  .>= lambda_R
    noncomp = 1-comply_con-comply_uncon
    compliance = 
        ifelse.(comply_con .== true, "CC",
        ifelse.(comply_uncon .== true, "CU",
        ifelse.(noncomp .== true, "NC")))

    # Dummy for formulation of if/else
    lambda_true = lambda_R .* comply_con .+ lambda_U .* (1 .- comply_con)
    chi_true = chi_lambda(lambda_R, delta, theta).*comply_con .+ lambda_U .* (1 .- comply_con)
    cost_true = comply_con .* C_R.^(1-alpha) .+ comply_uncon.*C_u.^(1-alpha) .+ noncomp.*tau.*(C_u.^(1-alpha)) 
    # add on error
    lambda_model = ubeta_draws(N, lambda_true, conc_err)
    chi_model = chi_true
    RCS = alpha .* (1 .- lambda_true) .+ lambda_true # regional content share

    # Store the results in a dictionary and return it
    return results = Dict(
        :lambda_U => lambda_U,
        :lambda_R => lambda_R,
        :lambda_model => lambda_model,
        :chi_model => chi_model,
        :RCS => RCS,
        :cost_true => cost_true,
        :comply_cost => comply_cost,
        :compliance => compliance,
        :CC_frac => mean(comply_con),
        :CU_frac => mean(comply_uncon),
        :alpha_mean => mean(alpha),
        :alpha_rng => extrema(alpha), 
        :delta_rng => extrema(delta), 
        :tau_rng => extrema(tau),     
        :alpha => alpha,
        :tau => tau,
        :tauQ => tauQ,
        :delta => delta)
end

# Main functions for simulating lambda and alpha and keeping track of V_iso_o ========================
function sim_lambda_alpha_o(; RCR, mu, sigma, theta, tau_data, mu_alpha, conc_alpha, conc_err, N)
# We draw the three dimensions of heterogeneity :
# - delta : substituability of foreing vers domestic imput
# - tau, tauQ : tariff from imports
# - alpha : share of local production before measure
    delta = rand(LogNormal(mu, sigma), N) #always firm specific
    tau_O = tau_data[sample(1:length(tau_data), N, replace=true)] #always firm specific
    tau = select(tau_O, "tau") # not sure about this part - je pense que c'est faux, ça fait bugger les autres fonctions sim_choice
    tauQ = select(tau_O, "tauQ") # not sure about this part 
    V_iso_o = select(tau_O, "V_iso_o") # not sure about this part 
    alpha = ubeta_draws(N, mu_alpha, conc_alpha)

    # Compute costs
    lambda_R = lambda_RCR(RCR, alpha) # firm specific if alpha is heterogenous
    lambda_U = lambda_U(delta, theta)
    C_R = C_comply(lambda_R, delta, theta)
    C_u = C_U(delta, theta)
    comply_cost = C_tile(lambda_R, delta, theta).^(1-alpha) #modified version from the authors (l.133)

    # Condition compliance
    comply_con = (comply_cost .<= tau) .& (lambda_U .<= lambda_R)
    comply_uncon = lambda_U  .>= lambda_R
    noncomp = 1-comply_con-comply_uncon
    compliance = 
        ifelse.(comply_con .== true, "CC",
        ifelse.(comply_uncon .== true, "CU",
        ifelse.(noncomp .== true, "NC")))

    # Dummy for formulation of if/else
    lambda_true = lambda_R .* comply_con .+ lambda_U .* (1 .- comply_con)
    chi_true = chi_lambda(lambda_R, delta, theta).*comply_con .+ lambda_U .* (1 .- comply_con)
    cost_true = comply_con .* C_R.^(1-alpha) .+ comply_uncon.*C_u.^(1-alpha) .+ noncomp.*tau.*(C_u.^(1-alpha)) 
    # add on error
    lambda_model = ubeta_draws(N, lambda_true, conc_err)
    chi_model = chi_true
    RCS = alpha .* (1 .- lambda_true) .+ lambda_true # regional content share

    # Store the results in a dictionary and return it
    return results = Dict(
        :lambda_U => lambda_U,
        :lambda_R => lambda_R,
        :lambda_model => lambda_model,
        :chi_model => chi_model,
        :RCS => RCS,
        :cost_true => cost_true,
        :comply_cost => comply_cost,
        :compliance => compliance,
        :CC_frac => mean(comply_con),
        :CU_frac => mean(comply_uncon),
        :alpha_mean => mean(alpha),
        :alpha_rng => extrema(alpha), 
        :delta_rng => extrema(delta), 
        :tau_rng => extrema(tau),     
        :alpha => alpha,
        :tau => tau,
        :tauQ => tauQ,
        :delta => delta,
        :V_iso_o => V_iso_o)
end

# Function for simulating with relocation =======================================
function sim_lambda_alpha_DRF(RCR, mu, sigma, theta, tau_data, mu_alpha, conc_alpha, conc_err, omegaF,omegaR,kappa,N)
    # We draw the three dimensions of heterogeneity
    delta_tilde = rand(LogNormal(N, mu, sigma), N) # always firm specific
    delta = delta_tilde./kappa # kappa is for relocation
    tauDT = tau_data[sample(1:length(tau_data), N, replace= true)] # carlines specific beta_draws
    alpha = ubeta_draws(N, mu_alpha, conc_alpha)
    Cost_W = DataFrame(id = 1:length(tauDT), tau_dt = tauDT, alph = alpha, delt = delta)

    # Compute costs, n.b. the kappa needs to be multiplied back on! 
    Cost_W[:, :lambda_R] = lambda_RCR(RCR, alpha) # parts costr share equivalent of the RCR
    Cost_W[:, C_u] = C_U(delta.*kappa, theta) #parts cost of complying domestic *unconstrained* 
    Cost_W[:, CDC] = C_tilde(lambda_R, delta.*kappa, theta).^(1-alpha) # rel. cost of complying domestically *constrained*
    Cost_W[:, NCD] = tauD # rel. cost of non-compliance domestically (stay in MX, pay the MFN to Canada and US)
    Cost_W[:, NCR] = tauR.*omegaR  # rel. cost of non-compliance in the RTA (->USA in fact), where costs are omegaR higher
    Cost_W[:, NCF] = tauF.*omegaF.*(kappa.*(C_U(delta./kappa, theta)/C_u).^(1-alpha)) # rel. cost of non-compliance in Foreing
    
    # PROBABLY TO BE RECHECKED

    # Melt the DataFrame
    idvars = names(Cost_W)[1:13]
    Cost_L = stack(Cost_W, [:CDC, :NCD, :NCR, :NCF], id_vars = idvars , variable_name=:choice, value_name=:relcost)
    sort!(Cost_L, [:id, :relcost]) # Sort by ID and relcost

    # Compute minimum cost
    Cost_min = @chain Cost_L begin
        groupby(idvars)
        @combine(:choice => first(:choice), :relcost => first(:relcost))
    end

    # Modify choice based on conditions
    # important: when relcost is one and choice is CD, 
    # then complying is unconstrained
    @chain Cost_min begin
        @where :relcost .== 1 .&& :choice .== "CDC"
        @transform!(:choice => "CDU")
    end
    
    # Compute costs in levels, modified to include alpha
    Cost_min[!, :cost_true] .= Cost_min[!, :relcost] .* (C_U ^ (1 - alpha))

    # Compute parts cost share before adding errors
    Cost_min[!, :lambda_U] .= 
    ifelse.(:choice .∈ ["NCD", "CDU", "NCR", "CDC"], 
    lambda_U(delta .* kappa, theta),
    lambda_U(delta ./ kappa, theta))

    # Compute lambda_true
    Cost_min[!, :lambda_true] .= 
    ifelse.(:choice .∈ ["NCD", "CDU", "NCR"], lambda_U(delta .* kappa, theta),
    ifelse.(:choice .== "NCF", lambda_U(delta ./ kappa, theta)),
    ifelse.(:choice .== "CDC", lambda_R, missing))

    # Compute chi_true, now convert to fractions of parts, requires a special transformation for the constrained carlines
    Cost_min[!, :chi_true] .= 
    ifelse.(:choice .∈ ["NCD", "CDU", "NCR", "NCF"], :lambda_true,
    ifelse.(:choice .== "CDC", chi_lambda(lambda_R, delta .* kappa, theta), missing))

    # Add error to lambda_true, has to be done like this to keep  0<=lambda_model <= 1
    Cost_min[!, :lambda_model] .= ubeta_draws(N, centre=:lambda_true, concentration=conc_err)

    # Assign chi_model, no err because this is not part of calibration
    Cost_min[!, :chi_model] .= Cost_min[!, :chi_true]

    # Compute RCS
    Cost_min[!, :RCS] .= alpha .* (1 .- Cost_min[!, :lambda_true]) + Cost_min[!, :lambda_true]
  
    return Cost_min
end

# FUNCTIONS FOR THE LAFFER CURVE ===============================================
# !!!!!!!! problem ici 
function sim_avg_RCS_alpha(RCR, theta, mu, sigma, tau_data, alpha, conc_alpha, conc_err, N)
    # Set random seed for reproducibility
    Random.seed!(140341)

    # Call the simulation function with the provided arguments
    sim_out = sim_lambda_alpha(RCR, theta, mu, sigma,
                               alpha, conc_alpha, 
                               conc_err, tau_data, N)

    # Scale the RCS values by 100
    RCS = 100 .* sim_out[:RCS]

    # Compute and return the mean of the scaled RCS values
    return mean(RCS)
end

function sim_choice( RCR, mu, sigma, theta, tau_data, mu_alpha, conc_alpha, conc_err, N)
    # Set random seed for reproducibility
    Random.seed!(140341)

    # Call the simulation function
    sim_out = sim_lambda_alpha(RCR=RCR, mu=mu, sigma=sigma, theta=theta, tau_data=tau_data,
                            mu_alpha=mu_alpha, conc_alpha=conc_alpha,
                               conc_err=conc_err,  N=N)

    # Extract the compliance data
    compliance_data = sim_out[:compliance]

    # Create a DataFrame from the compliance data
    sim_df = DataFrame(choice=compliance_data)

    # Count the number of occurrences for each choice
    choices = combine(groupby(sim_df, :choice), nrow => :N)

    # Add the RCR value to the result and return it
    return DataFrame(RCR=RCR, choices)
end

# LOSS FUNCTIONS ===============================================================

# Loss functions for  parameters to estimate. theta is now assumed fixed in all of those ====
# This function is supposed to provide the same output as loss_fun_4par but include more parameters with the idea of improving reproducibility
function loss_fun_alpha(; params_glob::Dict, param_est::Dict?, num_obs::Int64, DR::DataFrame, DS::DataFrame, CAMUS::Vector)
    seed(123)

    sim_out = sim_lambda_alpha(RCR = params_glob[:RCR], theta = params_glob[:theta], mu_alpha = params_glob[:mu_alpha], tau_data = params_glob[:tau], 
                               mu = param_est[:mu], sigma = sqrt.(abs.(param_est[:sigma]).^2),
                               mu_err = param_est[:mu_err], sigma_err = sqrt.(abs.(param_est[:sigma_err]).^2), 
                               conc_alpha = param_est[:conc_alpha], N=num_obs*20)
    lambda_sim = 100 .* sim_out[:lambda_model]
    lambda_sim_d = kde(sim_out[:lambda_model])

    # Density of the simulated data
    DS = DataFrame(:x = lambda_sim_d.x, :y = lambda_sim_d.density)
    DS[!, x_round] = round.(DS[!, :x], digits=2)
    DS = @chain DS begin
        @filter(:x_round .>= 0.00 .&& :x_round .<= 100.00)
        @groupby(:x_round)
        @combine(:den_sim = mean(:y))
    end 

    # Density of the observed data
    lambda_data_d = @chain DR begin
        @filter(!ismissing(:nafta_shr) && :ell in CAMUS)  # Filter rows
        @select(:nafta_shr)                              # Select relevant column
        x -> kde(x.nafta_shr)                            # Perform density estimation
        x -> DataFrame(x = x.x, density = x.density)     # Create output DataFrame
    end
    DS = join(DD, DS, on = :x_rnd, kind = :inner)
    
    # Compute the distance (loss) between the two densities
    den_data = DS.den_data
    den_sim = DS.den_sim
    fit = norm(den_data .- den_sim) # Compute L2 norm
    return fit
end

# BRUT FORCE FUNCTION ==========================================================
# TEMPORARY FUNCTION FOR TESTING PURPOSES

function brut_force_optim(mu_val, num_obs)
    mu_rep = reapeat([mu_val], num_obs)

    # Compute loss for each combination of parameters
    loss = map(
        (sigma, alphacon, errcon) -> loss_fun_alpha(mu_val, sigma, alphacon, errcon),
        param_grid.sigma,
        param_grid.alphacon,
        param_grid.errcon
    )

    # Create and return a DataFrame
    return DataFrame(
        mu = mu_rep,
        sigma = param_grid.sigma,
        cona = param_grid.alphacon,
        cone = param_grid.errcon,
        loss = loss
    )
end

end # module
