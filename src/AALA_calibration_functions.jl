# This file contains the functions used to calibrate the AALA model.
module AALA_calibration_functions

using Distributions
using Roots
using DataTables
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
function chi_lambda(lambda_R::Union{Float64, AbstractVector{Float64}}, delta::Float64, theta::Float64)
    # Compute denom element-wise
    denom = 1 .+ ((1 ./ lambda_R .- 1) ./ delta) .^ (theta / (theta + 1))

    # Return element-wise result
    return 1 ./ denom
end

# Unconstrained parts share
function chi_U(delta, theta)
    return 1 / (1 .+ delta.^(-theta))
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
        ifelse.(noncomp .== true, "NC", "Unknown")))

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




end # module
