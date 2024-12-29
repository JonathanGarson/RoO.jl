# This file contains the functions used to calibrate the AALA model.

# Module definition
module AALA_calibration_functions

using Distributions
# using Root

# Base functions ==============================================================

#lambda_R as a function of RCR (expressed as cost share incl assembly), for a given alpha  
function lambda_RCR(RCR, alpha)
    lambda_R = [a >= 0 && a < RCR ? (RCR - a) / (1 - a) : 0 for a in alpha]
    return lambda_R
end
#Je pense que cette fonction ne concerne pas des vecteurs, sinon i faudrait ajouter des broadcasts

# chi_R as a function  of lambda_R 
function chi_lambda(lambda_R::AbstractVector, delta, theta)
    # Compute the denominator for each element
    denom = 1 .+ ((1 ./ lambda_R .- 1) ./ delta) .^ (theta / (theta + 1))
    
    # Return the result
    return 1 ./ denom
end

# Unconstrained parts share
function chi_U(delta, theta)
    return 1 / (1 + delta^(-theta))
end

# Unconstrained costs share : uses the EK miracle 
const lambda_U = chi_U

#Analytic density of chi_U (and lambda_U) for unconstrained firms
function pdf_U(x, theta, mu, sigma; pct=true)
    # Convert x to proportions if expressed in percentage
    if pct
        x = x / 100
    end

    # Transformation of x to y
    y = x / (1 - x)

    # Compute the density
    g = pdf(LogNormal(mu, sigma), y^(1 / theta)) * (1 / (theta * x^2)) * y^(1 + 1 / theta)

    # Convert g back to percentage scale if required
    return pct ? g / 100 : g
end
#je suis pas sure de comprendre pct=TRUE, ça deviendra peut être plus clair quand on utilisera la fonction.

# Unconstrained cost : Compute the index using chi_U
function C_U(delta, theta)
    index = chi_U(delta, theta)^(1 / theta)
    return index
end

# Cost of compliance 
function C_comply(lambda_R, delta, theta)
    chi_R = chi_lambda.(lambda_R, delta, theta)
    k = (1 + theta) / theta
    return chi_R.^k .+ delta .* (1 .- chi_R).^k
end
 
# C.tilde is C_comply / C_U
function C_tilde(lambda_R, delta, theta)
    # Compute the threshold lambda_U
    lambda_u = lambda_U(delta, theta)  # lambda_U is chi_U in Julia
    
    # Compute compliance condition
    cons = lambda_R .> lambda_u  # Element-wise comparison for vectors
    
    # Compute C_comply and C_U
    comply_values = C_comply.(lambda_R, delta, theta) ./ C_U(delta, theta)
    
    # Combine results
    y = cons .* comply_values .+ .!cons  # Element-wise logical negation for (1 - cons)
    return y
end

# Limit of delta star as lambda_R -> 1 (in paper lambda_R -> chi_R, delta_max -> \bar{\delta}(\tau))
function delta_max(tau, theta)
    return (tau^theta - 1)^(-1 / theta)
end


# Cutoff delta for complying
function delta_star(lambda_R, tau, theta)
    # Define the auxiliary function ufn
    ufn = x -> C_tilde(lambda_R, x, theta) - tau
    
    # Calculate the maximum delta (upper bound of the interval)
    dmax = delta_max(tau, theta)
    
    # Find the root of the equation ufn(x) = 0 in the interval [0.00001, dmax]
    return find_zero(ufn, (0.00001, dmax))
end

end # module