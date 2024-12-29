# This file contains the functions used to calibrate the AALA model.
module AALA_calibration_functions

using Distributions
using Roots

# Base functions ==============================================================

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
    return 1 / (1 + delta^(-theta))
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
function C_U(delta::Float64, theta::Float64)
    index = chi_U(delta, theta)^(1 / theta)
    return index::Float64
end

# Cost of compliance 
# function C_comply(lambda_R::AbstractVector, delta::Float64, theta::Float64)
#     chi_R = chi_lambda.(lambda_R, delta, theta)
#     k = (1 + theta) / theta
#     return chi_R.^k .+ delta .* (1 .- chi_R).^k
# end

function C_comply(lambda_R::Union{Float64, AbstractVector{Float64}}, delta::Float64, theta::Float64)
    # Ensure lambda_R is always treated as a vector
    chi_R = chi_lambda.(lambda_R, delta, theta)  # Broadcasting over vector
    k = (1 + theta) / theta
    return chi_R.^k .+ delta .* (1 .- chi_R).^k
end

# C.tilde is C_comply / C_U
function C_tilde(lambda_R::Union{Float64, AbstractVector{Float64}}, delta::Float64, theta::Float64)
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
    return (tau^theta - 1)^(-1 / theta)
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
    return (lambda_R^(-1)-1)^(-1/theta-10)
end # module