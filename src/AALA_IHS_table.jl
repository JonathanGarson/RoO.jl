using DataFramesMeta
using DataFrames
using StatsBase
using CSV
using RCall

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
DR = CSV.read("data/AALA_rev.csv", DataFrame)

#  Choose the conservative or liberal going forward, and compute data density
if MEX_con_lib == "con"
    DR[!, :nafta_shr] .= DR.nafta_shr_con  # Update column with `nafta_shr_con`
else
    DR[!, :nafta_shr] .= DR.nafta_shr_lib  # Update column with `nafta_shr_lib`
end

println(describe(DR, :all, cols=:nafta_shr))

@transform!(DR, typeA = ifelse.(occursin.(r"Truck", coalesce.(DR.type, "")), "Truck", "Car/MPV"))

function classify_type(type)
    if type isa Missing
        return "Unknown"
    elseif occursin(r"Truck", type)
        return "Truck"
    else
        return "Car/MPV"
    end
end

@transform!(DR, typeA = classify_type.(DR.type))

filtered_DR = filter(row -> row.ell in CAMUS, DR)

grouped = DataFrames.groupby(filtered_DR, :ell)
DRc = combine(grouped, 
    :carlines => x -> length(unique(x)) => :unique_carlines,
    :nafta_shr => median => :median_nafta_shr,
    :nafta_shr => iqr => :iqr_nafta_shr
)


# Bring in tau
DT = rcopy(DataFrame, R"readRDS('Data/RDS_JIE_rev/tau_index_DRF.rds')")

count_non_missing = count(!ismissing, DT.tauD)

open("original_code/Tables_JIE_rev/tauD_count.txt", "w") do file
    write(file, "$count_non_missing\n")
end

rename!(DT, :tauD => :tau_index)

#unique model 
# Group by V_iso_o and HS_head, and calculate uniqueN and median for V_id and tau_index
grouped = DataFrames.groupby(DT, [:V_iso_o, :HS_head])
aggregated = combine(grouped, 
    :V_id => (x -> length(unique(x))) => :V_id_uniqueN,
    :V_id => median => :V_id_median,
    :tau_index => (x -> length(unique(x))) => :tau_index_uniqueN,
    :tau_index => median => :tau_index_median
)

# Reshape to wide format
DTc = DataFrame(V_iso_o = unique(aggregated.V_iso_o))  # Initialize wide DataFrame

# Loop over unique HS_head values to create columns
for hs in unique(aggregated.HS_head)
    # Filter data for this HS_head
    sub = filter(row -> row.HS_head == hs, aggregated)

    # Add columns to the wide DataFrame
    insertcols!(DTc, Symbol("V_id_uniqueN_$hs") => sub.V_id_uniqueN)
    insertcols!(DTc, Symbol("V_id_median_$hs") => sub.V_id_median)
    insertcols!(DTc, Symbol("tau_index_uniqueN_$hs") => sub.tau_index_uniqueN)
    insertcols!(DTc, Symbol("tau_index_median_$hs") => sub.tau_index_median)
end
# Display result
println(DTc)

# Step 1: Combine the DataFrames column-wise
DB = hcat(DRc, DTc)

# Step 2: Create the LaTeX-formatted output for each row
function texout(row)
    """
    Formats a row of data as a LaTeX table row.
    """
    return join(row, " & ") * " \\\\"
end

# Apply the texout function to each row
DB.out = [texout(row) for row in eachrow(DB)]

# Step 3: Write the LaTeX rows to a file
output_path = "AALA_IHS_table.tex"
open(output_path, "w") do file
    write(file, join(DB.out, "\n"))
end

