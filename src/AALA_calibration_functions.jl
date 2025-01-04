# This file contains the functions used to calibrate the AALA model.
module AALA_calibration_functions

using Chain
using Distributions
using DataFrames
using DataFramesMeta
using KernelDensity
using LinearAlgebra
using Random
using Revise
using Roots
using StatsBase

# EXPORT FUNCTIONS =============================================================
export lambda_RCR, chi_lambda, chi_U, lambda_U, pdf_U, C_U, C_comply, C_tilde, delta_max, delta_star, delta_circ, beta_draws, ubeta_draws, clean_density_data, sim_lambda, sim_lambda_alpha, sim_lambda_alpha_o, sim_lambda_alpha_DRF, sim_avg_RCS_alpha, sim_choice, sim_avg_RCS_alpha_DRF, sim_choice_DRF, loss_fun_alpha, brut_force_optim

# BASE FUNCTIONS ==============================================================

function lambda_RCR(RCR::Union{Float64, AbstractVector{Float64}}, alpha::Union{Float64, AbstractVector{Float64}})
    if isa(alpha, AbstractVector) && isa(RCR, AbstractVector)
        # Handle vector-vector input
        return [(0.0 <= a < r ? (r - a) / (1 - a) : 0.0) for (a, r) in zip(alpha, RCR)]
    elseif isa(alpha, AbstractVector)
        # Handle vector alpha and scalar RCR
        return [0.0 <= a < RCR ? (RCR - a) / (1 - a) : 0.0 for a in alpha]
    elseif isa(RCR, AbstractVector)
        # Handle scalar alpha and vector RCR
        return [0.0 <= alpha < r ? (r - alpha) / (1 - alpha) : 0.0 for r in RCR]
    else
        # Handle scalar alpha and scalar RCR
        return 0.0 <= alpha < RCR ? (RCR - alpha) / (1 - alpha) : 0.0
    end
end

#Je pense que cette fonction ne concerne pas des vecteurs, 
# sinon i faudrait ajouter des broadcasts

# chi_R as a function  of lambda_R 
function chi_lambda(lambda_R::Union{Float64, AbstractVector{Float64}}, delta::Union{Float64, AbstractVector{Float64}}, theta::Real)
    # Compute denom element-wise
    denom = 1 .+ ((1 ./ lambda_R .- 1) ./ delta) .^ (theta / (theta + 1))

    # Return element-wise result
    return 1 ./ denom
end

# Unconstrained parts share
function chi_U(delta::Union{Float64, AbstractVector{Float64}}, theta::Real)
    return 1 ./ (1 .+ delta.^(-theta))
end

# Unconstrained costs share : uses the EK miracle 
function lambda_U(delta::Union{Float64, AbstractVector{Float64}}, theta::Real)
    return 1 ./ (1 .+ delta.^(-theta))
end

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

# Unconstrained cost : Compute the index using chi_U
function C_U(delta::Union{Float64, AbstractVector{Float64}}, theta::Real)
    index = chi_U(delta, theta).^(1 / theta)
    return index
end

function C_comply(lambda_R::Union{Float64, AbstractVector{Float64}}, delta::Union{Float64, AbstractVector{Float64}}, theta::Real)
    # Ensure lambda_R is always treated as a vector
    chi_R = chi_lambda.(lambda_R, delta, theta)  # Broadcasting over vector
    k = (1 + theta) / theta
    return chi_R.^k .+ delta .* (1 .- chi_R).^k
end

# C.tilde is C_comply / C_U
function C_tilde(lambda_R::Union{Float64, AbstractVector{Float64}}, delta::Union{Float64, AbstractVector{Float64}}, theta::Real)
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
function delta_max(tau::Float64, theta::Real)
    return (tau.^theta - 1).^(-1 / theta)
end

# Cutoff delta for complying
function delta_star(lambda_R::Union{Float64, AbstractVector{Float64}}, tau::Float64, theta::Real)
    # Define the auxiliary function ufn
    ufn = x -> C_tilde(lambda_R, x, theta) - tau
    
    # Calculate the maximum delta (upper bound of the interval)
    dmax = delta_max(tau, theta)
    
    # Find the root of the equation ufn(x) = 0 in the interval [0.00001, dmax]
    return find_zero(ufn, (0.00001, dmax))
end

# Cutoff delta for complying - unconstrained
function delta_circ(lambda_R::Union{Float64, AbstractVector{Float64}}, theta::Real)
    return (lambda_R.^(-1) .- 1) .^ (-1 ./(theta-1))
end # module

# Function for generating heterogenous alpha constrained between 0 and 1 
function beta_draws(N::Int64, centre::Float64, concentration::Float64)
    # Set the parameters & limits
    a = 0
    c = 2 * centre 
    mu = centre/c
    x = rand(Beta(mu * concentration, (1 - mu) * concentration), N)
    return a .+ (c - a) .* x
end 

function ubeta_draws(N::Int64, centre::Union{Float64, Vector{Float64}}, concentration::Real)
    if typeof(centre) <: Vector
        # Validate all `centre` values are less than 1
        if any(c -> c >= 1, centre)
            throw(ArgumentError("All `centre` values must be less than 1, but got $centre."))
        end
        
        # Generate random values for each `centre` value and flatten the result
        return vcat([rand(Beta(c * concentration, (1 - c) * concentration), N) for c in centre]...)
    else
        # Single `centre` value
        if centre >= 1
            throw(ArgumentError("The parameter `centre` must be less than 1, but got $centre."))
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

# MAIN FUNCTIONS ===============================================================
# This is the main function which gets used for two purposes:
# 1. Calibration of the model to lambda
# 2. Welfare calculations using chi 

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

# Main function for simulating lambda and alpha =================================
# TO BE CONSISTENT PARAMETERS MUST BE STORED IN DICTIONARY ON THE MODEL OF THE CALIBRATION

function sim_lambda_alpha(; mu::Union{Float64, AbstractVector{Float64}}, calib_param::Dict, grid_param::Dict)
"""
Simulate the lambda and alpha values for the AALA model.

Parameters
----------
mu : AbstractVector{Float64}
    The mean of the log-normal distribution for delta.
calib_param : Dict
    A dictionary containing the calibration parameters of the model.
grid_params : DataFrame 
    A DataFrame containing the grid parameters of the model.

Returns
-------
results : Dict
    A dictionary containing the results of the simulation.
"""

    # We draw the three dimensions of heterogeneity :
    # delta : substituability of foreing vers domestic imput
    delta = rand(LogNormal(mu, grid_param[:sigma]), calib_param[:num_obs]) #always firm specific

    # tau, tauQ : tariff from imports
    tau = rand(calib_param[:tau], calib_param[:num_obs]) #tau is in a dictionnary, which one is it?
    tauQ = rand(calib_param[:tauQ], calib_param[:num_obs]) #tauQ is in a dictionnary, which one is it? 

    # alpha : share of local production before measure
    alpha = ubeta_draws(calib_param[:num_obs], calib_param[:mu_alpha], grid_param[:conc_alpha])

    # Compute costs
    lambda_R = lambda_RCR(calib_param[:RCR], alpha) # firm specific if alpha is heterogenous
    lambda_u = lambda_U(delta, calib_param[:theta])
    C_R = C_comply(lambda_R, delta, calib_param[:theta])
    C_u = C_U(delta, calib_param[:theta])
    comply_cost = C_tilde(lambda_R, delta, calib_param[:theta]).^(1 .-alpha) #modified version from the authors (l.133)

    # Condition compliance
    comply_con = (comply_cost .<= tau) .& (lambda_u .<= lambda_R)
    comply_uncon = lambda_u  .>= lambda_R
    noncomp = 1 .- comply_con .- comply_uncon
    compliance = 
        ifelse.(comply_con .== true, "CC",
        ifelse.(comply_uncon .== true, "CU",
        ifelse.(noncomp .== true, "NC", missing)))

    # Dummy for formulation of if/else
    lambda_true = lambda_R .* comply_con .+ lambda_u .* (1 .- comply_con)
    chi_true = chi_lambda(lambda_R, delta, calib_param[:theta]).*comply_con .+ lambda_u .* (1 .- comply_con)
    cost_true = comply_con .* C_R.^(1 .- alpha) .+ comply_uncon.*C_u.^(1 .- alpha) .+ noncomp.*tau.*(C_u .^ (1 .- alpha)) 
    # add on error
    lambda_model = ubeta_draws(calib_param[:num_obs], lambda_true, grid_param[:conc_err])
    chi_model = chi_true
    RCS = alpha .* (1 .- lambda_true) .+ lambda_true # regional content share

    # Store the results in a dictionary and return it
    return results = Dict(
        :lambda_U => lambda_u,
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
    
    # Melt the DataFrame
    idvars = names(Cost_W)[1:13]
    Cost_L = stack(Cost_W, [:CDC, :NCD, :NCR, :NCF], id_vars = idvars , variable_name=:choice, value_name=:relcost)
    sort!(Cost_L, [:id, :relcost]) # Sort by ID and relcost

    # Compute minimum cost
    Cost_min = @chain Cost_L begin
        @groupby(idvars)
        @combine(
            :choice = first(:choice), 
            :relcost = first(:relcost))
    end

    # Modify choice based on conditions
    # important: when relcost is one and choice is CD, 
    # then complying is unconstrained
    @chain Cost_min begin
        @rsubset(:relcost .== 1, :choice .== "CDC")
        @transform!(:choice = "CDU")
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

function sim_avg_RCS_alpha(RCR, theta, mu, sigma, tau_data, alpha, conc_alpha, conc_err, N)
    # Set random seed for reproducibility
    Random.seed!(42)

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
    Random.seed!(42)

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

function sim_avg_RCS_alpha_DRF(RCR,theta,mu,sigma,tau_data,alpha,conc_alpha,conc_err,N,omegaR,omegaF,kappa)
    Random.seed!(42)

    sim_out = sim_lambda_alpha_DRF(RCR = RCR,theta=theta,mu=mu,sigma=sigma,
                                   tau_data=tau_data,alpha=alpha,conc_alpha=conc_alpha,
                                   conc_err=conc_err,omegaR=omegaR,omegaF=omegaF,kappa=kappa,N=N
                                   )
                                
    RCS = 100 .* sim_out[:RCS]
    return avg_RCS = mean(RCS)
end

function sim_choice_DRF(RCR,theta,mu,sigma,tau_data,alpha,conc_alpha,conc_err,N,omegaR,omegaF,kappa)
    Random.seed!(42)

    sim_out = sim_lambda_alpha_DRF(RCR = RCR,theta=theta,mu=mu,sigma=sigma,
                                   tau_data=tau_data,alpha=alpha,conc_alpha=conc_alpha,
                                   conc_err=conc_err,omegaR=omegaR,omegaF=omegaF,kappa=kappa,N=N
                                   )
    
    choices = @chain sim_out begin
        @groupby(:V_iso_o, :choice)
        @combine(
            :N = nrow,
            :N_USA = sum(:chosen_R == "USA"),
            :N_CAN = sum(:chosen_R == "CAN"),
            :N_MEX = sum(:chosen_R == "MEX")
        )
    end
    
    return DataFrame(RCR=RCR, choices)
end 

# LOSS FUNCTIONS ===============================================================

# Loss functions for  parameters to estimate. theta is now assumed fixed in all of those ====
# This function is supposed to provide the same output as loss_fun_4par but include more parameters with the idea of improving reproducibility
function loss_fun_alpha(; mu::Union{Float64, AbstractVector{Float64}}, calib_param::Dict, grid_param::Dict, DD::DataFrame)
    """
    Compute the loss function for the calibration of the AALA model.

    Parameters
    ----------
    mu : AbstractVector{Float64}
        The mean of the log-normal distribution for delta.
    calib_param : Dict
        A dictionary containing the calibration parameters of the model.
    grid_params : DataFrame
        A DataFrame containing the grid parameters of the model.
    DD : DataFrame
        The DataFrame containing the data (real) density.
    
    Returns
    -------
    fit : Float64
        The loss value for the given parameters.
    """
    
    Random.seed!(42)

    sim_out = sim_lambda_alpha(mu = mu, calib_param = calib_param, grid_param = grid_param)

    lambda_sim = 100 .* sim_out[:lambda_model]
    lambda_sim_d = kde(sim_out[:lambda_model])
    
    # Density of the simulated data
    DS = DataFrame(:kernell_x => lambda_sim_d.x, :kernell_y => lambda_sim_d.density)
    # println(size(DS,1))

    DS = clean_density_data(DS, :den_sim)
    # println(size(DS,1))
    # println(size(DD,1))

    # merge data
    DS = innerjoin(DD, DS, on = :x_round)
    
    # Compute the distance (loss) between the two densities
    den_data = DS.den_data
    den_sim = DS.den_sim
    fit = norm(den_data .- den_sim) # Compute L2 norm
    return fit
end

# BRUT FORCE FUNCTION ==========================================================
# TEMPORARY FUNCTION FOR TESTING PURPOSES

function brut_force_optim(; mu_val::AbstractVector{Float64}, calib_param::Dict, grid_param::DataFrame, DD::DataFrame)
    # the four parameters to estimate are mu, sigma, alphacon, errcon
    mu_rep = reapeat([mu_val], calib_param[:num_obs])
    
    # Compute loss for each combination of parameters
    loss = map(
        (sigma, alphacon, errcon) -> loss_fun_alpha(
            mu = mu_rep,
            calib_param = calib_param,
            grid_param = grid_param,
            DD = DD
        ),
    )

    # Create and return a DataFrame
    return DataFrame(
        :mu => mu_rep,
        :sigma => grid_param.sigma,
        :cona => grid_param.alphacon,
        :cone => grid_param.errcon,
        :loss => loss
    )
end

end # module
