using Optim
using Distributions
using BenchmarkTools
using KernelDensity
using LinearAlgebra
using DataFrames
using DataFramesMeta
using Chain
using IterTools
using ProgressBars

export λ_RCR, χ_U, χ_λ, pdf_U, C_U, C_R, C_tilde, δ_max, δ_star, δ_circ, ubeta_draws, clean_density_data, simple_simulation, loss_fn_with_sim, grid_search_loss

# Main Equations of the Model ========================================================================
# These equations describe the main behavior of the model

# zeros(alpha) plus stable
# ifelse.( RCR .> alpha .> 0, (RCR .- alpha) ./ (1 .- alpha), 0.0)
function λ_RCR(RCR, α::Union{Float64, AbstractVector{Float64}})
    """
    Calculates the share of regional parts costs (λ_R) based on a regional content requirement (RCR)
    and the proportion of assembly costs in the total cost (α).

    Parameters:
    - RCR: Regional content requirement (scalar or vector)
    - α: Proportion of assembly costs in the total cost (scalar or vector)

    Returns:
    - λ_R: Share of regional parts costs (scalar or vector)
    """
    # return ifelse.((α .>= 0) .& (α .< RCR), (RCR .- α) ./ (1 .- α), 0.0)
    return ifelse.( RCR .> α .> 0, (RCR .- α) ./ (1 .- α), zero.(α))
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

    return (1 .+ δ .^ (-θ)) .^ (-1)
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
    denom = 1 .+ ((1 ./ λ_R .- 1) ./ δ) .^ (θ./(θ .+ 1)) 
    return 1/denom
end

#  Distribution of cost shares
function pdf_U(x, θ, μ, σ)
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
    x = x/100
    y = x ./ (1 .- x)
    g = pdf.(LogNormal(μ, σ), y .^ (1 ./ θ)) .* (1 ./ (θ .* x .^ 2) .* y .^ (1+1 ./ θ))
    return  g./100
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
    return χ_U(δ, θ) .^(1 ./ θ)
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
    return χ_R .^ k + (1 .- χ_R) .^ k .* δ # cost function for constrained firms
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
    # thres = χ_λ(δ, θ, λ_R) .> χ_U(δ, θ) # we willingly depart from the code to follow the paper that states : "or the ensuing analysis, we assume that the rule is specified in terms of a part share 𝜒_𝑅"
    thres = λ_R .> χ_U(δ, θ)
    y = ifelse.(thres, C_R(δ, θ, λ_R) ./ C_U(δ, θ), 1)
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
    # ufn = x -> C_tilde(λ_R, x, θ) - τ
    dmax = δ_max(τ, θ)
    # result = Optim.optimize(ufn, 1e-6, dmax).minimizer
    result = Optim.optimize(x -> C_tilde(λ_R, x, θ) - τ, 1e-6, dmax).minimizer
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

function ubeta_draws(N::Int64, centre::Union{Float64, Vector{Float64}}, concentration::Real)
    """
    Draws Beta-distributed random samples based on the centre and concentration parameters.
    
    Parameters:
    - N::Int64: Number of random values to draw for each Beta distribution.
    - centre::Union{Float64, Vector{Float64}}: Centre(s) of the Beta distribution.
    - concentration::Real: Concentration parameter for the Beta distribution.

    Returns:
    - Vector{Float64}: Flattened vector of Beta samples if `centre` is a vector.
    - Vector{Float64}: Vector of Beta samples if `centre` is a scalar.
    """
    if typeof(centre) <: Vector
        # Ensure all centre values are valid
        if any(c -> c >= 1 || c <= 0, centre)
            throw(ArgumentError("All `centre` values must be in the range (0, 1), but got $centre."))
        end

        # Generate samples for each `centre` value, concatenate into a single vector
        return vcat([rand(Beta(c * concentration, (1 - c) * concentration), N) for c in centre]...)
    else
        # Single centre value validation
        if !(0 < centre < 1)
            throw(ArgumentError("The parameter `centre` must be in the range (0, 1), but got $centre."))
        end

        return rand(Beta(centre * concentration, (1 - centre) * concentration), N)
    end
end

function clean_density_data(df::DataFrame, data_dens_name::Symbol)
    clean_df = @chain df begin
        @transform!(:x_round = round.(:kernell_x))  # Round values
        @rsubset(:x_round >= 0.00, :x_round <= 100.00)  # Filter valid ranges
        @rsubset(:x_round != -0.0)  # Exclude -0.0
        @groupby(:x_round)  # Group by x_round
        @combine(:change_name = mean(:kernell_y))  # Dynamically assign column name
    end
    rename!(clean_df, [:x_round, data_dens_name])  # Rename columns
    return clean_df
end

# We start the simualtion of the model for given variables ==========================================

function simple_simulation(RCR, μ, σ, θ, τ, α_centre, α_concentration, concentration_error, N)
    """
    This function simulates the model for a given set of parameters.

    Parameters:
    RCR: float
        Regional content requirement
    μ: float
        Mean of the lognormal distribution
    σ: float
        Standard deviation of the lognormal distribution
    θ: float
        Elasticity to foreign cost advantage
    τ_data: vector of float
        Tariff penalty for non-compliance
    α_centre: float
        Centre of the beta distribution for alpha
    α_concentration: float
        Concentration of the beta distribution for alpha
    concentration_error: float
        Concentration error for the beta distribution
    N: int
        Number of firms to simulate

    Returns:
    Union{Float64, AbstractVector{Float64}}
        Simulated lambda values
    """
    δ = rand(LogNormal(μ, σ), N) # Range of  values (foreign cost advantage)
    α = rand(Beta(α_centre * α_concentration, (1 - α_centre) * α_concentration), N) # Range of values (proportion of assembly costs in the total cost)
    
    # Calculate the share of regional parts 
    λ_R = λ_RCR(RCR, α) 
    λ_U = χ_U(δ, θ) 

    # Calculate the compliance cost
    comply_cost = C_tilde(λ_R, δ, θ).^(1  .- α)

    # Evaluate the compliance status
    comply_constrained = (comply_cost .<= τ) .& (λ_U .< λ_R)

    # Estimate the optimal lambda
    λ_true = comply_constrained .* λ_R .+ (1 .- comply_constrained) .* λ_U

    # Add noise to the lambda of the model #utiliser view(lambda_true) pour éviter de copier la mémoire
    λ_model = ubeta_draws(N,  λ_true[1:10], concentration_error)

    return λ_model
end

function loss_fn_with_sim(RCR, μ, σ, θ, τ_data, α_centre, α_concentration, concentration_error, N ; df_data::DataFrame)
    """
    Calculate the loss function for the given model parameters and observed data density.

    Parameters:
    - RCR, μ, σ, θ, τ_data, α_centre, α_concentration, concentration_error, N: Model parameters
    - df_data: DataFrame with observed data density
    
    Returns:
    - loss: Loss value
    """
    
    λ_model = simple_simulation(RCR, μ, σ, θ, τ_data, α_centre, α_concentration, concentration_error, N)
    
    # Calculate the kernell density of the model
    λ_sim = λ_model .* 100
    density = kde(λ_sim)
    
    # Store in a dataframe
    df_sim = DataFrame(:kernell_x => density.x, :kernell_y => density.density)
    df_sim = clean_density_data(df_sim, :den_sim)
    
    # Merge the two dataframes
    df_merged = innerjoin(df_data, df_sim, on = :x_round)
    
    # Calculate the loss
    loss = norm(df_merged.den_data .- df_merged.den_sim, 2)
    
    return loss
end

function grid_search_loss(; RCR, θ, τ_data, α_centre, N, mu_grid, sigma_grid, alpha_con_grid, errcon_grid , df_data::DataFrame)  
    """
    Perform a grid search over the given parameter grids and calculate the loss for each combination.

    Parameters:
    - RCR, θ, τ_data, α_centre, α_concentration, concentration_error, N: Model parameters
    - df_data: DataFrame with observed data density
    - mu_grid, sigma_grid, alpha_con_grid, errcon_grid: Arrays of values for the parameter grid

    Returns:
    - results: Matrix with rows corresponding to parameter combinations and columns [μ, σ, alpha_con, errcon, loss]
    """
    # Total number of combinations
    n_total = length(mu_grid) * length(sigma_grid) * length(alpha_con_grid) * length(errcon_grid)

    # Preallocate results matrix
    # results = zeros(5, n_total)  # Columns: μ, σ, alpha_con, errcon, loss
    results = Array{Float64}(undef, 5, n_total)  # Columns: μ, σ, alpha_con, errcon, loss
    
    # Iterate over all combinations of parameters
    row = 1
    for (μ, σ, alpha_con, errcon) in ProgressBar(IterTools.product(mu_grid, sigma_grid, alpha_con_grid, errcon_grid))
        # Calculate loss for the current parameter combination
        loss = loss_fn_with_sim(RCR, μ, σ, θ, τ_data, α_centre, alpha_con, errcon, N ; df_data = df_data)

        # Store results
        results[:, row] .= [μ, σ, alpha_con, errcon, loss]
        row += 1
    end

    return results
end