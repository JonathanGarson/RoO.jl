using CSV
using DataFrames
using DataFramesMeta
using StatsBase

# Charger les données depuis le fichier CSV
file_path = "Data/AALA/data_aala_raw.csv"

DR_raw=CSV.read(file_path, DataFrame)
DR = CSV.read(file_path, DataFrame)
frequencies = DataFrame(year=[2011, 2011, 2012, 2013, 2013, 2013, 2014, 2015, 2015])


#-> start out by fixing some shifted columns in 2011 and 2017
DR.flag = (DR.source_T1 .== "") .& (DR.source_T2 .!= "")# Create a "flag" column to identify rows that need fixing
flagged_indices = findall(DR.flag)  # Indices where flag is true
for i in flagged_indices
    DR.source_T1[i] = DR.source_T2[i]
    DR.source_T2[i] = ""
end

for i in flagged_indices # Move data from source_T2 to source_T1 and clear source_T2
    DR.source_T1[i] = DR.source_T2[i]
    DR.source_T2[i] = ""
end

for i in flagged_indices #Apply specific fixes based on `carlines` 
    if DR.carlines[i] == "Renegade"
        DR.final_assembly1[i] = "IT"  # Correct to Melfi factory
    elseif DR.carlines[i] == "ATS"
        DR.source_T2[i] = DR.source_T1[i]
        DR.source_T1[i] = DR.source_E2[i]
        DR.source_E2[i] = DR.final_assembly2[i]
        DR.final_assembly2[i] = ""
    end
end



select!(DR, Not(:flag))  # Remove the flag column in place

#2 problem w/ columns being shifted from one page of the VW raw data for 2017
DR[!, :flag] .= (DR.final_assembly1 .== "") .& (DR.final_assembly2 .!= "") .& (DR.source_E1 .== "")
DR[DR.flag .== true, :final_assembly1] .= DR[DR.flag .== true, :final_assembly2]
DR[DR.flag .== true, :final_assembly2] .= ""
DR[DR.flag .== true, :source_E1] .= DR[DR.flag .== true, :source_E2]
DR[DR.flag .== true, :source_E2] .= ""

select!(DR, Not(:flag))

#3, page 12 in 2011 has a column shift, need to deal with this in two pieces.
DR[!, :flag] .= (DR.source_E1 .== "") .& (DR.final_assembly2 .!= "") .& (DR.year .== 2011)
DR[DR.flag .== true, :source_E1] .= DR[DR.flag .== true, :final_assembly2]
DR[DR.flag .== true, :final_assembly2] .= ""
DR[DR.flag .== true, :source_T1] .= DR[DR.flag .== true, :source_E2]
DR[DR.flag .== true, :source_E2] .= ""
select!(DR, Not(:flag))

#same page, special case
DR[!, :flag] .= (DR.year .== 2011) .& (DR.carlines .== "Routan")

DR[DR.flag .== true, :source_E1] .= DR[DR.flag .== true, :final_assembly2]
DR[DR.flag .== true, :final_assembly2] .= ""
DR[DR.flag .== true, :source_E2] .= DR[DR.flag .== true, :source_E1]
DR[DR.flag .== true, :source_T1] .= DR[DR.flag .== true, :source_E2]

select!(DR, Not(:flag))
#4 Verano, Regal and CTS all appear to be shifted in 2017
DR[!, :flag] .= (DR.source_E1 .== "") .& (DR.final_assembly2 .!= "") .& (DR.year .== 2017)
DR[DR.flag .== true, :source_E1] .= DR[DR.flag .== true, :final_assembly2]
DR[DR.flag .== true, :final_assembly2] .= ""
DR[DR.flag .== true, :source_T1] .= DR[DR.flag .== true, :source_E2]
DR[DR.flag .== true, :source_E2] .= ""
DR[(DR.flag .== true) .& (DR.carlines .== "Verano"), :percent_content_other1] .= "15.00% M"

select!(DR, Not(:flag))

#5 second source is same as first source
DR[!, :flag] .= (coalesce.(DR.final_assembly1, "") .== coalesce.(DR.final_assembly2, "")) .& (coalesce.(DR.final_assembly1, "") .!= "")
DR[DR.flag .== true, :final_assembly2] .= ""
select!(DR, Not(:flag))

#
@transform!(DR, :mfg_HQ = "ROW") # Default manufacturing headquarters (HQ) to "ROW"

@rtransform!(DR, 
    :mfg_HQ = if occursin(r"^General", :mfg) || :mfg == "GM LLC" || occursin(r"^Ford", :mfg) || occursin(r"^Tesla", :mfg)
        "USA"
    else
        :mfg_HQ
    end
) # Set HQ to "USA" based on manufacturer

# Set HQ to "KOR" for Korean manufacturers
@rtransform!(DR, 
    :mfg_HQ = if occursin(r"^Hyundai", :mfg) || occursin(r"^Kia", :mfg)
        "KOR"
    else
        :mfg_HQ
    end
)

# Set HQ to "JPN" for Japanese manufacturers
@rtransform!(DR, 
    :mfg_HQ = if occursin(r"^Toyota", :mfg) || occursin(r"^Mazda", :mfg) || occursin(r"Honda", :mfg) || occursin(r"^Nissan", :mfg) ||
                    occursin(r"Suzuki", :mfg) || occursin(r"^Fuji", :mfg) || occursin(r"^Subaru", :mfg) || occursin(r"^Mitsub", :mfg)
        "JPN"
    else
        :mfg_HQ
    end
)


# Correct a typo in manufacturer name
@rtransform!(DR, 
    :mfg = if :mfg == "PoyrscheAG"
        "Porsche AG"
    else
        :mfg
    end
)

# Set HQ to "DEU" for German manufacturers
@rtransform!(DR, 
    :mfg_HQ = if occursin(r"^Volks", :mfg) || occursin(r"^Merc", :mfg) || occursin(r"^Porsche", :mfg) || 
                  occursin(r"^BMW", :mfg) || occursin(r"^Audi", :mfg)
        "DEU"
    else
        :mfg_HQ
    end
)

# Determine source of engines
@rtransform!(DR, 
    :E_US = occursin(r"US", :source_E1) || occursin(r"US", :source_E2)
)

@rtransform!(DR, 
    :E_CA = occursin(r"CN", :source_E1) || occursin(r"CN", :source_E2) || 
            occursin(r"CAN", :source_E1) || occursin(r"CAN", :source_E2) || 
            ((occursin(r"^C", :source_E1) && !occursin(r"CH", :source_E1)) || 
             (occursin(r"^C", :source_E2) && !occursin(r"CH", :source_E2)))
)

#Determining transmission sources
@rtransform!(DR, :T_US = occursin(r"US", :source_T1) || occursin(r"US", :source_T2))
@rtransform!(DR, 
    :T_CA = occursin(r"CN", :source_T1) || occursin(r"CN", :source_T2) || 
            occursin(r"CAN", :source_T1) || occursin(r"CAN", :source_T2) || 
            ((occursin(r"^C", :source_T1) && !occursin(r"CH", :source_T1)) || 
             (occursin(r"^C", :source_T2) && !occursin(r"CH", :source_T2)))
)

#Determining engine and transmission sources for Mexico
@rtransform!(DR, :E_MX = occursin(r"MX", :source_E1) || occursin(r"MX", :source_E2) || 
                        occursin(r"^M", :source_E1) || occursin(r"^M", :source_E2))

#content shares

@rtransform!(DR, 
    :us_ca_shr = if !ismissing(:percent_content_USA_CAN) && !isempty(:percent_content_USA_CAN)
        # Supprimer le symbole `%`, diviser par les sauts de ligne, et prendre le premier chiffre
        parse(Float64, split(replace(string(:percent_content_USA_CAN), "%" => ""), r"\n")[1])
    else
        missing
    end
)

missing_values = DR[ismissing.(DR.us_ca_shr), :percent_content_USA_CAN]
 # there are 52 blanks
 println(countmap(DR.final_assembly1))
 DR.other1_shr = map(x -> 
 if ismissing(x) || isempty(x)
     missing
 else
     m = match(r"^\d+", x)
     m === nothing ? missing : parse(Float64, m.match)
 end, 
 DR.percent_content_other1
)

DR.other1_who = map(x -> 
    if ismissing(x) || isempty(x)
        missing
    else
        m = match(r".*%", x)
        m === nothing ? missing : replace(x, r".*%" => "")
    end, 
    DR.percent_content_other1
)

#je capte pas à quoi servent les ligne 69 et 70 du code original other1_shr n'a déjà pas de % dans notre dataframe

# Filter rows where `other1_who` starts with "M"
starts_with_M = filter(row -> 
    !ismissing(row[:other1_who]) && occursin(r"^M", row[:other1_who]), 
    eachrow(DR)
)

println("Occurrences starting with M:")
println(countmap([row[:other1_who] for row in starts_with_M]))

# Filter rows where `other1_who` contains "M"
contains_M = filter(row -> 
    !ismissing(row[:other1_who]) && occursin(r"M", row[:other1_who]), 
    eachrow(DR)
)


#
non_empty_count = sum(.!ismissing.(DR.percent_content_other2) .& .!(DR.percent_content_other2 .== ""))#1335 obs
println("Non-empty values in percent_content_other2: $non_empty_count")

rows_with_M = filter(row -> !ismissing(row[:percent_content_other2]) && occursin("M", row[:percent_content_other2]), eachrow(DR))
println("Table of percent_content_other2 with 'M':")
println(countmap([row[:percent_content_other2] for row in rows_with_M]))  # Indeed these all look like Mexico

#Extract Numeric Prefix from percent_content_other2 into other2_shr:
DR.other2_shr = map(x -> 
    if ismissing(x) || isempty(x)
        missing
    else
        m = match(r"^\d+", x)
        m === nothing ? missing : parse(Float64, m.match)
    end, DR.percent_content_other2
)

#je pense qu'il y a une erreure dans leur code à la ligne 75-76 car leur code produit uniquement des NA pour other2_shr ce qui n'est pas consistent avec la colonne percent_content_other2

println("Summary of other2_shr:")
println(describe(DR.other2_shr))

#Extract Characters After % into other2_who
DR.other2_who = map(x -> 
    if ismissing(x) || isempty(x)
        missing
    else
        replace(x, r".*%" => "")
    end, DR.percent_content_other2
)

#Filter Rows Where other2_who Contains "M" and Tabulate:
rows_with_M_in_other2_who = filter(row -> !ismissing(row[:other2_who]) && occursin("M", row[:other2_who]), eachrow(DR))
println("Table of other2_who with 'M':")
println(countmap([row[:other2_who] for row in rows_with_M_in_other2_who]))

#Filter Rows Where other2_who Contains "M" and other2_shr Is missing:
rows_with_M_and_missing_other2_shr = filter(row -> 
    !ismissing(row[:other2_who]) && occursin("M", row[:other2_who]) && ismissing(row[:other2_shr]), 
    eachrow(DR)
)
println("Rows where other2_who contains 'M' and other2_shr is missing:")
println(DataFrame(rows_with_M_and_missing_other2_shr))

#one fix for Hyundai Accent and Hyundai Accent (Manual Transmission) in 2018 where columns are messed up
# Update rows where carlines == "Accent" and year == 2018
for row in eachrow(DR)
    if row[:carlines] == "Accent" && row[:year] == 2018
        row[:other1_shr] = 47
        row[:other1_who] = "M"
        row[:other2_who] = ""
    end
end

# Update rows where carlines == "Accent (Manual Transmission)" and year == 2018
for row in eachrow(DR)
    if row[:carlines] == "Accent (Manual Transmission)" && row[:year] == 2018
        row[:other1_shr] = 44
        row[:other1_who] = "M"
        row[:other2_who] = ""
    end
end

# Now starts encoding of assembly 1 and assembly 2
#
# fix country codes
US = ["US", "USA"]
CA = ["CN", "C", "CAN"]
USCA = vcat(CA, US, "US, CN")  # Combine CA and US codes
MX = ["M", "MX"]
USCAMX = vcat(USCA, MX, "M, US")  # Combine all with additional entry

MX2 = unique(
    filter(row -> 
        !ismissing(row[:final_assembly2]) && occursin("M", row[:final_assembly2]), 
        eachrow(DR)
    ) |> x -> [row[:final_assembly2] for row in x]
)

println("MX2: $MX2")

CA2 = unique(
    filter(row -> 
        !ismissing(row[:final_assembly2]) && 
        occursin("C", row[:final_assembly2]) && 
        row[:final_assembly2] != "M (Reg\nCab)", 
        eachrow(DR)
    ) |> x -> [row[:final_assembly2] for row in x]
)

US2 = unique(
    filter(row -> 
        !ismissing(row[:final_assembly2]) && 
        occursin("US", row[:final_assembly2]), 
        eachrow(DR)
    ) |> x -> [row[:final_assembly2] for row in x]
)

USCAMX2 = union(union(US2, CA2), MX2)

#ligne 99 et 107 : j'y arrive pas

#
#Nafta assembly location 1 countries
DR.ell = [
    ismissing(final_assembly1) ? missing :
    final_assembly1 in US ? "US" :
    final_assembly1 in CA ? "CA" :
    final_assembly1 in MX ? "MX" :
    "ROW"
    for final_assembly1 in DR.final_assembly1
]

# Define the sets
valid_ell = ["CA", "US", "MX"]
valid_ell = vec(valid_ell) 

#lignes 117- 120 : coding strategy differs a bit from r

function count_rows(dataframe, condition_func)
    count = 0
    for row in eachrow(dataframe)
        if condition_func(row)
            count += 1
        end
    end
    return count
end

# Count rows for each condition
count1 = count_rows(DR, condition1)
println("Count 1: $count1")

count2 = count_rows(DR, condition2)
println("Count 2: $count2")

count3 = count_rows(DR, condition3)
println("Count 3: $count3")

# Filter rows where `us_ca_shr` is not missing
DR = filter(row -> !ismissing(row[:us_ca_shr]), eachrow(DR)) |> DataFrame


# Now starts the treatment of second assembly sites that are in NAFTA
#
# second assembly location logicals
DR.ell2CA = map(x -> !ismissing(x) && x in CA2, DR.final_assembly2)
DR.ell2US = map(x -> !ismissing(x) && x in US2, DR.final_assembly2)
DR.ell2MX = map(x -> !ismissing(x) && x in MX2, DR.final_assembly2)

# melt in the style of a Stata reshape long. Note that the expands dataset by the number of countries : 4666*3 = 13998
D2 = stack(
    DR, 
    [:ell2CA, :ell2US, :ell2MX],  # Columns to melt
    variable_name=:ell2,          # Name for new column storing original column names
    value_name=:add_plant         # Name for new column storing values
)

#Remove "ell2" prefix from `ell2` column
D2.ell2 .= replace.(D2.ell2, r"^ell2" => "")

# Filter rows where `add_plant` is true
D2 = filter(row -> row[:add_plant], D2)


# drop primary assembly site (ell) and then rename the additional sites as ell, so we can append
# Remove the `ell` column
select!(D2, Not(:ell))
# Rename `ell2` to `ell`
rename!(D2, :ell2 => :ell)

#append the additional sites, fill in add_plant as false for the final_assembly1 sites: 4788 obs now : 4666+122
# Step 1: Append the DataFrames, ensuring all columns are included
DR_combined = vcat(DR, D2; cols=:union)
# Step 2: Replace missing values in `add_plant` with `false`
DR_combined.add_plant .= coalesce.(DR_combined.add_plant, false)
