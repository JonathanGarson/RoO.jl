using JuMP, Plots, Optim, Distributions

function λ_RCR(RCR::Union{Float64, AbstractVector{Float64}}, α::Union{Float64, AbstractVector{Float64}})
    """
    This function calculates the share of regional parts costs (λ_R) based on a regional content requirement (RCR) and the proportion of assembly costs in the total cost (α).

    Parameters:
    RCR: float
        Regional content requirement
    α: float
        Proportion of assembly costs in the total cost

    Returns:
    float
        Share of regional parts costs
        """
        if α .>= 0 && α .< RCR
            λ_R = (RCR .- α)/(1 .- α)
        return λ_R
    else
        λ_R = 0
        return λ_R 
    end
end

# Unrestricted allocation of production
# Equation 1
function χ_U(δ, θ)
    """
    This function calculates the optimal domestic content share (χ_U) for a firm with a foreign cost advantage (δ) and a Weibull shape parameter (θ).
    
    Parameters:
    δ: float
        Foreign cost advantage
    θ: float
        Weibull shape parameter

    Returns:
    float
        Optimal domestic content share
    """

    return (1 .+ δ^(-θ)).^(-1)
end

# Restricted allocation of production
# Equation 2
function χ_λ(δ, θ, λ_R::Union{Float64, AbstractVector{Float64}})
   """
    This function calculates the optimal domestic content share (χ_λ) for a firm with a foreign cost advantage (δ), a Weibull shape parameter (θ), and a regional content requirement (λ_R).
    
    Parameters:
    δ: float
        Foreign cost advantage
    θ: float
        Weibull shape parameter
    λ_R: float
        Regional content requirement

    Returns:
    float
        Optimal domestic content share under regional content requirement
   """
    denom = 1 .+ ((1/ λ_R .- 1) ./ δ) .^ (θ/(θ .+ 1)) 
    return 1/denom
end

#  Distribution of cost shares
function pdf_U(x, θ, μ, σ, pct = true)
    """
    This function calculates the probability density function (PDF) of the cost share distribution for unrestricted allocation of production.

    Parameters:
    x: float
        Cost share
    θ: float
        Shape parameter
    μ: float
        mean of the lognormal distribution
    σ: float
        standard deviation of the lognormal distribution
    pct: bool, optional
        Whether to return the PDF as a percentage (default is true)

    Returns:
    float
        PDF of the cost share distribution
    """
    if pct == true 
        x = x/100
        y = x ./ (1 .- x)
        g = pdf.(LogNormal(μ, σ), y .^ (1 ./ θ)) .* (1 ./ (θ .* x .^ 2) .* y .^ (1+1 ./ θ))
        return pct ? g./100 : g
    end
end

# Cost function for unconstrained firms
# Given as so in the paper
function C_U(δ, θ)
    """
    This function calculates the cost function for unconstrained firms.

    Parameters:
    δ: float
        Foreign cost advantage
    θ: float
        Elasticity to foreign cost advantage

    Returns:
    float
        Cost function for unconstrained firms
    """
    index = χ_U(δ, θ) .^(1 ./ θ)
    return index
end

# Cost function for constrained firms
# Equation 3
function C_R(δ, θ, λ_R::Union{Float64, AbstractVector{Float64}})
    """
    This function calculates the cost function for constrained firms.

    Parameters:
    δ: float
        Foreign cost advantage
    θ: float
        Elasticity to foreign cost advantage

    Returns:
    float
        Cost function for constrained firms
    """
    χ_R = χ_λ.(δ, θ, λ_R) # regional content requirement
    k = (1 .+ θ)./θ # elasticity of substitution
    return χ_R .^ k + (1 - χ_R) .^ k .* δ # cost function for constrained firms
end

# Binding constraint is defined as χ_R > χ_U(δ), C_tilde = C_R/C_U 
# Equation 6
function C_tilde(λ_R, δ, θ)
    """
    This function calculates the ratio of the cost function for constrained firms to the cost function for unconstrained firms.

    Parameters:
    λ_R: float
        Regional content requirement
    δ: float
        Foreign cost advantage
    θ: float
        Elasticity to foreign cost advantage
    
    Returns:
    y: float
        Ratio of the cost function for constrained firms to the cost function for unconstrained firms
    """
    thres = χ_λ(δ, θ, λ_R) .> χ_U(δ, θ) # we willingly depart from the code to follow the paper that states : "or the ensuing analysis, we assume that the rule is specified in terms of a part share 𝜒_𝑅"
    # thres = λ_R .> χ_U(δ, θ)
    y = thres ? C_R(δ, θ, λ_R) ./ C_U(δ, θ) : 1
    return y
end

# Cut off functions : we design delta depending on the regional

# Limit of delta star as lambda_R -> 1 (in paper lambda_R -> chi_R, delta_max -> \bar{\delta}(\tau))
function δ_max(τ, θ)
    """
    This function calculates the maximum foreign cost advantage (δ_max) as the regional content requirement (λ_R) approaches 1.

    Parameters:
    τ: float
        Tariff penalty for non-compliance
    θ: float
        Elasticity to foreign cost advantage

    Returns:
    float
        Maximum foreign cost advantage
    """
    (τ .^(θ) -1) .^ (-1 ./θ)
end

# Cut off to determine the optimal delta for a given lambda_R
function δ_star(λ_R, τ, θ)
    """
    This function calculates the optimal foreign cost advantage (δ_star) for a given regional content requirement (λ_R) and tariff penalty for non-compliance (τ).
    It minimizes the function ufn on the interval 1e-6 to δ_max using the Optim package for univariate optimization, the Brent algorithm.
        
    Parameters:
    λ_R: float
        Regional content requirement
    τ: float
        Tariff penalty for non-compliance
    θ: float
            
    Returns:
    float
        Optimal foreign cost advantage
    """
    ufn = x -> C_tilde(λ_R, x, θ) - τ
    dmax = δ_max(τ, θ)
    result = Optim.optimize(ufn, 1e-6, dmax).minimizer
end

# Cutt off for compliant constrained firms 
# Circ stand for circle in the paper
function δ_circ(λ_R, θ)
    """
    This function calculates the optimal foreign cost advantage (δ_circ) for a compliant unconstrained firm 

    Parameters:
    λ_R: float
        Regional content requirement
    θ: float
        Elasticity to foreign cost advantage

    Returns:
    float
        Optimal foreign cost advantage
    """
    return (λ_R.^(-1) .- 1).^(-1 ./(θ .- 1))
end

# Draw beta values from a uniform distribution to create heterogenous alpha
function ubeta_draws(n, centre, concentration)
    """
    This function draws n beta values from a uniform distribution to create heterogenous alpha values.

    Parameters:
    n: int
        Number of beta values to draw
    centre: float
        Centre of the beta distribution
    concentration: float
        Concentration of the beta distribution

    Returns:
    Union{Float64, AbstractVector{Float64}}
        Beta values
    """
    rand(Beta(centre .* concentration, (1 - centre) .* concentration), n)
end

