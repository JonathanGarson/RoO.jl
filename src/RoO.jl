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
using ProgressBars
using RData
using Revise
using Statistics

# import all functions from the script 

include("AALA_solving_model_alternative.jl")
include("AALA_grid_search_alternative.jl")

using Pkg
Pkg.instantiate()  # Install the dependencies listed in Project.toml

function run()
    include("src/AALA_clean.jl")        # Cleans AALA data and outputs a file
    include("src/AALA_IHS_table.jl")    # Generates Table 1
    include("src/AALA_calibration_plots.jl") # Generate plot 1
end 


# Main function to execute the package

end
