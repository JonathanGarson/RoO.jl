using DataFramesMeta
using DataFrames
using StatsBase
using CSV
using RData
include("files_path.jl")

export L2norm 

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
DR = CSV.read("$output_data/AALA_rev.csv", DataFrame)
DR= filter(:year => in(calib_years), DR)

#  Choose the conservative or liberal going forward, and compute data density
if MEX_con_lib == "con"
    DR[!, :nafta_shr] .= DR.nafta_shr_con  # Update column with `nafta_shr_con`
else
    DR[!, :nafta_shr] .= DR.nafta_shr_lib  # Update column with `nafta_shr_lib`
end

println(describe(DR, :all, cols=:nafta_shr))

@transform!(DR, :typeA = ifelse.(occursin.(r"Truck", coalesce.(DR.type, "")), "Truck", "Car/MPV"))

function classify_type(type)
    if type isa Missing
        return "Unknown"
    elseif occursin(r"Truck", type)
        return "Truck"
    else
        return "Car/MPV"
    end
end
@transform!(DR, :ellA = ifelse.(:ell .== "CA", "Canada", 
                    ifelse.(:ell .== "US", "USA", 
                    ifelse.(:ell .== "MX", "Mexico", missing))))
@transform!(DR, :typeA = classify_type.(DR.type))

filtered_DR = filter(row -> row.ell in CAMUS, DR)

grouped = DataFrames.groupby(filtered_DR, :ell)
DRc = combine(grouped, 
    :carlines => x -> length(unique(x)) => :unique_carlines,
    :nafta_shr => median => :median_nafta_shr,
    :nafta_shr => iqr => :iqr_nafta_shr
)
# Filtrer pour `typeA = "Car/MPV"`
filtered_car = filter(:typeA => ==("Car/MPV"), DR)

# Effectuer les agrégations
DRc_car = combine(DataFrames.groupby(filtered_car, :ellA),
    :carlines => x -> length(unique(x)), 
    :nafta_shr => median => :median_nafta_shr_Car,
    :nafta_shr => x -> quantile(x, 0.75) - quantile(x, 0.25) 
)

rename!(DRc_car, :carlines_function => :carline_uniqueN_Car)
rename!(DRc_car, :nafta_shr_function => :iqr_nafta_shr_Car)
delete!(DRc_car, [4])
# Filtrer pour `typeA = "Truck"`
filtered_truck = filter(:typeA => ==("Truck"), DR)

# Effectuer les agrégations
DRc_truck = combine(DataFrames.groupby(filtered_truck, :ellA),
    :carlines => x -> length(unique(x)),
    :nafta_shr => median => :median_nafta_shr_Truck,
    :nafta_shr => x -> quantile(x, 0.75) - quantile(x, 0.25)
)
rename!(DRc_truck, :carlines_function => :carline_uniqueN_Truck)
rename!(DRc_truck, :nafta_shr_function => :iqr_nafta_shr_Truck)
delete!(DRc_truck, [4])

DRc =  innerjoin(DRc_car, DRc_truck, on=:ellA)
# Bring in tau
DT = DataFrame(load("$input_clean_data/tau_index_DRF.rds"))

count_non_missing = count(!ismissing, DT.tauD)

# open("original_code/Tables_JIE_rev/tauD_count.txt", "w") do file
#     write(file, "$count_non_missing\n")
# end

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
select!(DTc, Not(:V_id_median_8703))
select!(DTc, Not(:V_id_median_8704))
select!(DTc, Not(:tau_index_uniqueN_8703))
select!(DTc, Not(:tau_index_uniqueN_8704))
rename!(DTc, :V_iso_o => :ellA)
@transform!(DTc, :ellA = ifelse.(:ellA .==  "CAN", "Canada",
                    ifelse.(:ellA .== "MEX","Mexico", "USA")))
# Step 1: Combine the DataFrames column-wise
DB = innerjoin(DRc, DTc, on=:ellA)
select!(DB, :ellA, :carline_uniqueN_Car,  :median_nafta_shr_Car, :V_id_uniqueN_8703, :tau_index_median_8703, :carline_uniqueN_Truck,  :median_nafta_shr_Truck, :V_id_uniqueN_8704, :tau_index_median_8704)
unique(DTc.ellA)
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
output_path = "output/tables/AALA_IHS_table.tex"
open(output_path, "w") do file
    write(file, join(DB.out, "\n"))
end

