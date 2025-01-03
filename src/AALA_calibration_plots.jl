using CSV
using DataFrames
using Statistics
using KernelDensity
using Distributions
using Plots
using LaTeXStrings
using PlotShapes



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

relative_path = "data/RDS_JIE_rev/Params4"
check_or_create_directory(relative_path)


# Load the necessary functions from a separate Julia file
include("AALA_calibration_functions.jl")

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
DR = CSV.read("data/AALA_rev.csv", DataFrame)
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
using Random

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
plot_path = "data/output/AALA_calib_model_data.pdf"

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

# Add data density
plot!(lambda_data_d.x, lambda_data_d.density, color="black", lw=2, label="Data")

# Add unconstrained density
plot!(x_rng[x_rng .< lambda_star], y_rng[x_rng .< lambda_star], color="forestgreen", lw=1, label="Non-compliers")

x_shaded = x_rng[x_rng .<= lambda_star]  # Select x values below lambda_star
y_shaded = y_rng[x_rng .<= lambda_star] 
y_0 =zeros(length(x_shaded)) 
fill_between = x_rng[x_rng .< lambda_star]  # Function defining the region to shade
plot!( fill_between=(y_0, y_shaded), color=:forestgreen, alpha=0.5, label="Shaded Area")


# Add constrained compliance marker
plot!(fill_between([RCR - eps, RCR + eps], [0, 0], [brk - gap, ymax * 0.95],
    color="forestgreen", alpha=0.25, label=""))

# Add text and markers
annotate!(RCR, ymax * 0.95, text("Comply con. = $(round(sim_out[:comply_frac], digits=2))", :left, 8))
annotate!(lambda_star - 3 * eps, ymax / 3, text("Non-compliers", :center, 8))
x_start= 10
y_start= 0.010  # Starting point of the arrow
dx = 14
dy =10  # Direction and length
quiver!(
    [x_start], [y_start],  # Starting point
    quiver=([dx], [dy]),   # Direction and length
    arrow=true,            # Enable arrowhead
    lw=1,                  # Line width
    color=:black
)


# Add legend with LaTeX labels
plot!(legend=:topleft)
plot!(label="", xlabel="", ylabel="")


# Save to PDF
savefig(p, "Plots_JIE_rev/AALA_calib/AALA_calib_model_data.pdf")
