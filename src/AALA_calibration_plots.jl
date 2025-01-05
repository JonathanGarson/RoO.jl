# Load the necessary functions from a separate Julia file
include("AALA_calibration_functions.jl")
include("files_path.jl")
using CSV
using DataFrames
using Statistics
using KernelDensity
using Distributions
using Plots
using LaTeXStrings
using Random



function check_or_create_directory(relative_path::String)
    # Get the absolute path based on the relative path
    base_dir = @__DIR__  # Represents the directory of the current Julia file
    full_path = joinpath(base_dir, "..", relative_path)

    # Check if the directory exists
    if isdir(full_path)
        println("Directory exists")
    else
        # Create the directory if it does not exist
        mkdir(full_path)
        println("Directory did not exist, so it was created")
    end
end


check_or_create_directory(relative_path)

# Define calibration years
calib_years = 2011:2019

# Define the L2 norm function
function L2norm(u::Vector{Float64}, v::Vector{Float64})
    sqrt(sum((u .- v) .^ 2))
end

# Define the regions
CAMUS = ["CA", "MX", "US"]

# Define assumptions for the Mexican share
MEX_con_lib = "con"  # Either "con" or "lib" assumption

# Define the distribution for alpha
dist_alpha = "Beta"

# Set baseline values for parameters
alpha_base = 0.15
theta_base = 4

# Define a dictionary of parameters
params = Dict(
    :theta => theta_base,
    :mu => 0.1,
    :sigma => 0.1,
    :RCR => 0.625,
    :tau => 1.025,
    :alpha_lo => 0.0,
    :alpha_hi => 0.0,
    :alpha_a => 1e7,
    :alpha_b => 1e7
)

# Compute RCR as a percentage
RCR = 100 * params[:RCR]
#
#Load Data
DR = CSV.read("$output_data/AALA_rev.csv", DataFrame)
#
#  Choose the conservative or liberal going forward, and compute data density
if MEX_con_lib == "con"
    DR[!, :nafta_shr] .= DR.nafta_shr_con  # Update column with `nafta_shr_con`
else
    DR[!, :nafta_shr] .= DR.nafta_shr_lib  # Update column with `nafta_shr_lib`
end

println(describe(DR, :all, cols=:nafta_shr))

# Filter rows and compute density
filtered_DR = filter(row -> !ismissing(row[:nafta_shr]) && row[:ell] in CAMUS, DR)
DR = filter(row -> row[:year] in calib_years, DR)

# Density estimation for `nafta_shr` (requires the `KernelDensity` package)
nafta_shr_values = filtered_DR[:, :nafta_shr]
lambda_data_d = kde(nafta_shr_values)

# Count observations satisfying the condition
 
# Plot : model of density of lambda with only delta heterogeneity, including data density ====
# pedagogical: shows density hole and spike problems
# do it for alpha = 0, so that we have a minimal version 
#
# simulation

# Set the seed for reproducibility
Random.seed!(140422)

# Define the parameters as a dictionary
params = Dict(
    :theta => Float64(theta_base),
    :mu => 0.0,
    :sigma => 0.2,
    :RCR => 0.625,
    :tau => 1.1,
    :alpha_lo => 0.0,
    :alpha_hi => 0.0,
    :alpha_a => 1e7,
    :alpha_b => 1e7
)

params[:mu]
# Call the simulation function
Num_obs = 4165  # Replace with the actual value
sim_out = sim_lambda(
    params[:RCR], 
    params[:mu], 
    params[:sigma], 
    Float64(params[:theta]), 
    [params[:tau]],  # Assuming tau_data is a single value; adjust if it's a vector
    params[:alpha_lo], 
    params[:alpha_hi], 
    params[:alpha_a], 
    params[:alpha_b], 
    Num_obs
)


# Convert simulated lambda_model to percentages
lambda_sim = 100 .* sim_out[:lambda_model]

# Compute the density for lambda_sim
lambda_sim_d = kde(lambda_sim)

# Compute the density for unconstrained lambda_U
lambda_U_d = kde(100 .* sim_out[:lambda_U])

# Inspect the results (densities)
println("Lambda_sim density: ", lambda_sim_d)
println("Lambda_U density: ", lambda_U_d)

# Compute the x range for the graph
x_rng = 0.01:0.01:99.9

y_rng = pdf_U(collect(x_rng), params[:theta], params[:mu], params[:sigma])

# Cap for y-axis
ycap = 0.065

# Maximum y value
lambda_data_d_y = lambda_data_d.density
lambda_U_d_y = lambda_U_d.density
ymax = maximum([maximum(lambda_data_d_y), maximum(lambda_U_d_y)])
ymax = 1.2 * minimum([ymax, ycap])

# Epsilon value
eps = 0.5


lambda_RCR_value = lambda_RCR(params[:RCR], Float64(0))  # Firm-specific if alpha is heterogeneous
RCR = 100 * lambda_RCR_value

# Define the break and gap for the graph
brk = ymax * 0.85
gap = 0.0015

# Print results for verification
println("x_rng: ", x_rng)
println("y_rng: ", y_rng)
println("ymax: ", ymax)
println("brk: ", brk)
println("gap: ", gap)
println("RCR: ", RCR)

# Create a PDF for the plot
plot_path = "$output_data/AALA_calib_model_data.pdf"

# Set plot size (7.5 x 4 inches in R is approximately 750 x 400 pixels in Julia)
default(size=(750, 400))

# Set margins (equivalent to `par(mar=...)` in R)
plot_margin = (left=0.1, right=0.1, top=0.1, bottom=0.1)

#Define parameters
delta_star_value = delta_star(lambda_RCR(params[:RCR], Float64(0)), params[:tau], params[:theta])
lambda_star = 100 * lambda_U(delta_star_value, params[:theta])

# Axis marks and labels
xaxmarks = [0, lambda_star, 25, 50, RCR, 75, 85, 100]
xlabels = [
    "0",
    L"\lambda_U(\delta^*)",
    "25",
    "50",
    L"\lambda_R = $(round(RCR, digits=1))",
    "75",
    "85",
    "100"
]

# Create the plot
p = plot(x_rng, y_rng, color="orange", lw=3, label=L"Model (no ROO)", xlabel="Nafta cost share (%)", ylabel="Density",
    xlim=(0, 100), ylim=(0.0, ymax), legend=:topleft)


xticks!([0, 18.18, 62.5, 100], ["0", L"\lambda_U(\delta^*)", L"\lambda_R", "100"])
# Add data density
plot!(lambda_data_d.x, lambda_data_d.density, color="black", lw=2, label="Data")

# Add unconstrained density
plot!(x_rng[x_rng .< lambda_star], y_rng[x_rng .< lambda_star], color="forestgreen", lw=1, label=L"Model(ROO)")
plot!(x_rng[x_rng .> RCR], y_rng[x_rng .> RCR], color="forestgreen", lw=1, label="")
y_value = pdf_U(lambda_star,params[:theta], params[:mu], params[:sigma])  # Valeur de la PDF à lambda_star


x_shaded = x_rng[(x_rng .>= RCR) .& (x_rng .<= 100)]
y_shaded = y_rng[(x_rng .>= RCR) .& (x_rng .<= 100)]
plot!(x_shaded, y_shaded, fillrange = zero(x_shaded), fc=:green,  alpha=0.3, label="")
plot!(x_rng[x_rng .< lambda_star], y_rng[x_rng .< lambda_star], fillrange = zero(x_rng[x_rng .< lambda_star]), fc=:green,  alpha=0.3, label="")


# Définir les paramètres de l'ombrage
ycap = 0.065
ymax = maximum(vcat(lambda_data_d_y, lambda_U_d_y))
ymax = 1.2*min(ymax,ycap)
eps = 0.5  # Petit offset autour de RCR
brk = ymax*0.85  # Point de rupture (par exemple)
gap = 0.0015  # Écart pour la zone inférieure


# Fill
plot!(
    [RCR - eps, RCR + eps, RCR + eps, RCR - eps],  # Coordonnées x
    [brk, brk - gap, 0, 0],  # Coordonnées y
    seriestype=:shape,
    color=:black,
    alpha=0.25,
    label=""
)

# Fill
plot!(
    [RCR - eps, RCR + eps, RCR + eps, RCR - eps],  # Coordonnées x
    [ymax * 0.95, ymax * 0.95, brk, brk + gap],  # Coordonnées y
    seriestype=:shape,
    color=:black,
    alpha=0.25,
    label=""
)

text_latex = L" \theta=4,\quad \mu=0,\quad \sigma=0.2, \quad \theta=1.1, \quad \alpha=0"
annotate!(100, 0.025, Plots.text(text_latex, 8, :right))  

text_latex = L" Comply\quad  uncon. "
annotate!(80, 0.005, Plots.text(text_latex, 8, :right))  

text_latex = L" Non-compliers"
annotate!(16, 0.005, Plots.text(text_latex, 8, :right))  

text_latex = L" Complier \quad con.=0.71"
annotate!(60, 0.022, Plots.text(text_latex, 8, :right)) 

savefig("$output_figures/AALA_calib_model_data.pdf")