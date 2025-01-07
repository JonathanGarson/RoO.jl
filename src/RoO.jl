module RoO

using BenchmarkTools
using Chain
using DataFrames
using DataFramesMeta
using Distributions
using IterTools
using KernelDensity
using LinearAlgebra
using Optim
using Pkg
using ProgressBars
using RData
using Revise
using Statistics
using Serialization
using Test

export run_short, run_sim

# import all functions from the script 

function run_short()
    include("src/files_path.jl")                    # Define the file paths
    include("src/AALA_clean.jl")                    # Cleans AALA data and outputs a file
    include("src/AALA_IHS_table.jl")                # Generates Table 1
    include("src/AALA_calibration_plots.jl")        # Generate plot 1
    include("src/mathematica_plots.jl")             # Generate figure 1 and 5
end

function run_sim()
    include("src/AALA_solving_alt.jl")              # Functions to solve the model
    include("src/AALA_grid_search_alt.jl")          # Grid search to find the best parameters
end

end # module
