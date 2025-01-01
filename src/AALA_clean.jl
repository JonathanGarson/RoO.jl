using CSV
using DataFrames
using DataFramesMeta
using StatsBase
using Statistics

#problème de réplication des lignes 100-107 qui epêche d'avoir de bon résulats à partir de la ligne 142. 
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

@rtransform!(DR, :T_MX = occursin(r"MX", :source_T1) || occursin(r"MX", :source_T2) || 
                        occursin(r"^M", :source_T1) || occursin(r"^M", :source_T2))                       
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
# final assembly location 2
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

#summary statistics  for the us_ca_shr column in a DataFrame, filtered by whether a specified assembly location 
    filtered1_us_ca_shr = [row[:us_ca_shr] for row in filtered1]
    # Filter rows where `final_assembly2` is in `USCAMX2` and not missing
    filtered2 = filter(row -> !ismissing(row[:final_assembly2]) && row[:final_assembly2] in USCAMX2, eachrow(DR))
    # Extract the `us_ca_shr` values from the filtered rows
    filtered2_us_ca_shr = [row[:us_ca_shr] for row in filtered2]
    # Summary function
    function summarize(data)
        nonmissing_data = skipmissing(data)
        return Dict(
            "mean" => mean(nonmissing_data),
            "min" => minimum(nonmissing_data),
            "max" => maximum(nonmissing_data),
            "missing_count" => count(ismissing, data),
            "nonmissing_count" => count(!ismissing, data)
        )
    end
    # Summarize filtered results
    summary1 = summarize(filtered1_us_ca_shr)
    summary2 = summarize(filtered2_us_ca_shr)
    println("Summary for final_assembly1 in USCAMX:")
    println(summary1)
    println("Summary for final_assembly2 in USCAMX2:")
    println(summary2)

# Count rows where both `other1_who` and `other2_who` contain "M"
# mex is never listed under BOTH others.
    filtered_rows = filter(row -> 
        !ismissing(row[:other1_who]) && occursin("M", row[:other1_who]) &&
        !ismissing(row[:other2_who]) && occursin("M", row[:other2_who]),
        eachrow(DR)
    )
    count2 = length(filtered_rows) # Count the filtered rows

# Assign values from `other1_shr` to `mx_shr` for rows where `other1_who` contains "M"
    DR[!, :mx_shr] = Vector{Union{Missing, Int}}(missing, nrow(DR))
    # Update `mx_shr` where `other1_who` contains "M"
    for i in 1:nrow(DR)
        if !ismissing(DR[i, :other1_who]) && occursin("M", DR[i, :other1_who])
            DR[i, :mx_shr] = DR[i, :other1_shr]
        end
    end

# Summarize `mx_shr` for rows where `final_assembly1` belongs to the `USCAMX` set
    filtered_rows = filter(row -> row[:final_assembly1] in USCAMX, eachrow(DR)) #Filter rows where `final_assembly1` is in `USCAMX`
    mx_shr_values = [row[:mx_shr] for row in filtered_rows]#Extract `mx_shr` values for the filtered rows
    summary_stats = Dict(
        "mean" => mean(skipmissing(mx_shr_values)),
        "min" => minimum(skipmissing(mx_shr_values)),
        "max" => maximum(skipmissing(mx_shr_values)),
        "missing_count" => count(ismissing, mx_shr_values),
        "nonmissing_count" => count(!ismissing, mx_shr_values)
    )#Summarize the `mx_shr` column
    println("Summary statistics for `mx_shr` where `final_assembly1` is in USCAMX:")
    println(summary_stats)

# Summarize `mx_shr` for rows where `final_assembly1` belongs to the `MX` set
    filtered_rows = filter(row -> row[:final_assembly1] in MX, eachrow(DR)) #Filter rows where `final_assembly1` is in `MX`
    filtered_df = DataFrame(filtered_rows) # Convert the filtered rows back into a DataFrame
    summary_stats = Dict(
        "mean" => mean(skipmissing(filtered_rows.mx_shr)),
        "min" => minimum(skipmissing(filtered_rows.mx_shr)),
        "max" => maximum(skipmissing(filtered_rows.mx_shr)),
        "missing_count" => count(ismissing, filtered_rows.mx_shr),
        "nonmissing_count" => count(!ismissing, filtered_rows.mx_shr)
    ) # Summarize the `mx_shr` column
    println("Summary statistics for `mx_shr` where `final_assembly1` is in MX:")
    println(summary_stats)


#Assign values from `other2_shr` to `mx_shr` for rows where `other2_who` contains "M"
    for i in 1:nrow(DR)
        if !ismissing(DR[i, :other2_who]) && occursin("M", DR[i, :other2_who])
            DR[i, :mx_shr] = DR[i, :other2_shr]
        end
    end ## Update `mx_shr` for rows where `other2_who` contains "M"


# Summarize `mx_shr` for rows where `final_assembly1` belongs to the `MX` set
filtered_rows_2 = filter(row -> row[:final_assembly1] in MX, eachrow(DR)) #Filter rows where `final_assembly1` is in `MX`
filtered_df_2 = DataFrame(filtered_rows_2) # Convert the filtered rows back into a DataFrame
summary_stats_2 = Dict(
    "mean" => mean(skipmissing(filtered_rows.mx_shr)),
    "min" => minimum(skipmissing(filtered_rows.mx_shr)),
    "max" => maximum(skipmissing(filtered_rows.mx_shr)),
    "missing_count" => count(ismissing, filtered_rows.mx_shr),
    "nonmissing_count" => count(!ismissing, filtered_rows.mx_shr)
) # Summarize the `mx_shr` column
println("Summary statistics for `mx_shr` where `final_assembly1` is in MX:")
println(summary_stats_2)

# Summarize `mx_shr` for rows where `final_assembly2` belongs to the `MX` set
filtered_rows_3 = filter(row -> !ismissing(row[:final_assembly2]) && row[:final_assembly2] in MX2, eachrow(DR))# Filter rows where `final_assembly2` is in `MX2`
filtered_df_3 = DataFrame(filtered_rows_3) # Convert the filtered rows back into a DataFrame
summary_stats_3 = Dict(
    "mean" => mean(skipmissing(filtered_df_3.mx_shr)),
    "min" => minimum(skipmissing(filtered_df_3.mx_shr)),
    "max" => maximum(skipmissing(filtered_df_3.mx_shr)),
    "missing_count" => count(ismissing, filtered_df_3.mx_shr),
    "nonmissing_count" => count(!ismissing, filtered_df_3.mx_shr)
) #Summarize the `mx_shr` column
println("Summary statistics for `mx_shr` where `final_assembly2` is in MX2:")
println(summary_stats_3)
#
#Nafta assembly location 1 countries
DR.ell = [
    ismissing(final_assembly1) ? missing :
    final_assembly1 in US ? "US" :
    final_assembly1 in CA ? "CA" :
    final_assembly1 in MX ? "MX" :
    "ROW"
    for final_assembly1 in DR.final_assembly1
] # Create the `DR` DataFrame


# Count rows based on `ell` membership in specific sets and the presence or absence of `us_ca_shr`
    valid_ell = ["CA", "US", "MX"] # Define the sets
    valid_ell = vec(valid_ell) 
    function count_rows(dataframe, condition_func)
        count = 0
        for row in eachrow(dataframe)
            if condition_func(row)
                count += 1
            end
        end
        return count
    end

    # Condition 1: `ell` in valid_ell and `us_ca_shr` is missing
    condition1 = row -> (!ismissing(row[:ell]) && row[:ell] in valid_ell && ismissing(row[:us_ca_shr]))
    # Condition 2: `ell` not in valid_ell and `us_ca_shr` is missing
    condition2 = row -> (!ismissing(row[:ell]) && !(row[:ell] in valid_ell) && ismissing(row[:us_ca_shr]))
    # Condition 3: `ell` not in valid_ell and `us_ca_shr` is not missing
    condition3 = row -> (!ismissing(row[:ell]) && !(row[:ell] in valid_ell) && !ismissing(row[:us_ca_shr]))
    
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

D2.ell2 .= replace.(D2.ell2, r"^ell2" => "") #Remove "ell2" prefix from `ell2` column

D2 = filter(row -> row[:add_plant], D2) # Filter rows where `add_plant` is true


# drop primary assembly site (ell) and then rename the additional sites as ell, so we can append
select!(D2, Not(:ell)) # Remove the `ell` column
rename!(D2, :ell2 => :ell) # Rename `ell2` to `ell`

#append the additional sites, fill in add_plant as false for the final_assembly1 sites: 4788 obs now : 4666+122
DR_combined = vcat(DR, D2; cols=:union) #Append the DataFrames, ensuring all columns are included
DR_combined.add_plant .= coalesce.(DR_combined.add_plant, false) # Replace missing values in `add_plant` with `false`

#get rid of the location 2 logicals
cols_to_delete = filter(col -> occursin(r"^ell2", col), names(DR)) # Identify columns to delete using a regex pattern
select!(DR, Not(cols_to_delete)) # Remove the identified columns
#
# Now starts the (complex) bounding of Mexican share (which is not a variable per se)
#
# there are two is.na(ell), that have multiple assembly countries in north america
result = combine(
    groupby(DR, :ell),
    :mx_shr => (x -> mean(skipmissing(x))) => :meanmx,
    :us_ca_shr => (x -> mean(skipmissing(x))) => :meanusca
)
# none of these will catch cases where ell2 %in% c("US","CA","MX") but but ell (location 1) is not

# Create a logical column `nafta_assembly` indicating membership in `["US", "CA", "MX"]`
nafta_set = ["US", "CA", "MX"]
DR[!, :nafta_assembly] = map(x -> x in nafta_set, DR.ell)

# Calculate and assign `rem_shr` as `100 - us_ca_shr` for rows where `nafta_assembly` is true and `other1_shr` is missing
DR[!, :rem_shr] = Vector{Union{Missing, Float64}}(missing, nrow(DR)) #Initialize the `rem_shr` column
for i in 1:nrow(DR)
    if DR[i, :nafta_assembly] && ismissing(DR[i, :other1_shr])
        DR[i, :rem_shr] = 100.0 - DR[i, :us_ca_shr]
    end
end #Update `rem_shr` where `nafta_assembly` is true and `other1_shr` is missing


# Update `rem_shr` as `100 - us_ca_shr` for rows where `nafta_assembly` is true,
# `other1_shr` is not missing, and `other1_who` contains "M", without overwriting existing values
for i in 1:nrow(DR)
    if ismissing(DR[i, :rem_shr]) &&  # Only update if `rem_shr` is missing
       DR[i, :nafta_assembly] && 
       !ismissing(DR[i, :other1_shr]) && 
       !ismissing(DR[i, :other1_who]) && 
       occursin("M", DR[i, :other1_who])
        DR[i, :rem_shr] = 100.0 - DR[i, :us_ca_shr]
    end
end

# Update `rem_shr` for specific rows without overwriting existing numeric values
for i in 1:nrow(DR)
    if ismissing(DR[i, :rem_shr]) &&  # Only update if `rem_shr` is missing
       DR[i, :nafta_assembly] &&
       !ismissing(DR[i, :other1_shr]) &&
       !ismissing(DR[i, :other1_who]) &&
       !occursin("M", DR[i, :other1_who])
        DR[i, :rem_shr] = 100.0 - DR[i, :us_ca_shr] - DR[i, :other1_shr]
    end
end

# Step 1: Compute `mx_shr_rem` as `mx_shr / rem_shr` for rows where `nafta_assembly` is true
# Calculate the ratio of `mx_shr` to `rem_shr` for NAFTA assembly rows
# Initialize the `mx_shr_rem` column with `missing`
DR[!, :mx_shr_rem] = Vector{Union{Missing, Float64}}(missing, nrow(DR))

# Loop through each row to calculate `mx_shr_rem`
for i in 1:nrow(DR)
    if DR[i, :nafta_assembly] && !ismissing(DR[i, :mx_shr]) && !ismissing(DR[i, :rem_shr])
        DR[i, :mx_shr_rem] = DR[i, :mx_shr] / DR[i, :rem_shr]
    end
end

# Step 2: Summarize `mx_shr_rem` for NAFTA assembly rows
# Generate summary statistics for `mx_shr_rem` for NAFTA assembly rows
# Filter rows where `nafta_assembly` is true
filtered_rows = filter(row -> row[:nafta_assembly], eachrow(DR))

# Create a DataFrame from the filtered rows
filtered_df = DataFrame(filtered_rows)

# Calculate summary statistics explicitly
mean_value = mean(skipmissing(filtered_df.mx_shr_rem))
min_value = minimum(skipmissing(filtered_df.mx_shr_rem))
max_value = maximum(skipmissing(filtered_df.mx_shr_rem))
missing_count = count(ismissing, filtered_df.mx_shr_rem)
nonmissing_count = count(!ismissing, filtered_df.mx_shr_rem)

# Print summary statistics
println("Summary statistics for `mx_shr_rem` where `nafta_assembly` is true:")
println("Mean: $mean_value")
println("Min: $min_value")
println("Max: $max_value")
println("Missing Count: $missing_count")
println("Non-missing Count: $nonmissing_count")

#here it diverge a bit

# Calculate the mean of `mx_shr_rem` for each `ell` group where `nafta_assembly` is true and `mx_shr_rem < 1`
filtered_DR = filter(row -> 
    row[:nafta_assembly] && !ismissing(row[:mx_shr_rem]) && row[:mx_shr_rem] < 1, 
    eachrow(DR)
) # Filter rows where `nafta_assembly` is true and `mx_shr_rem < 1`
con_means = combine(
    groupby(DataFrame(filtered_DR), :ell), 
    :mx_shr_rem => (x -> mean(skipmissing(x))) => :mx_shr_rem_mn
)# Group by `ell` and calculate the mean of `mx_shr_rem`
println("Conditional means by group:")
println(con_means)

# Calculate the median of `mx_shr_rem` for each `ell` group where `nafta_assembly` is true
filtered_DR = filter(row -> row[:nafta_assembly], eachrow(DR)) # Filter rows where `nafta_assembly` is true
filtered_df = DataFrame(filtered_DR) # Create a DataFrame from the filtered rows
unique_ell = unique(filtered_df.ell) # Get unique values of `ell` to group by
medians = DataFrame(
    ell=unique_ell,
    mx_shr_rem_median=[
        median(skipmissing(filtered_df[filtered_df.ell .== group, :mx_shr_rem])) for group in unique_ell
    ]
) # Manually calculate the medians for each group
println("Medians of `mx_shr_rem` by `ell` group:")
println(medians)

# Calculate the median of `mx_shr` for rows where `ell` equals "MX"
filtered_DR = filter(row -> row[:ell] == "MX", eachrow(DR))# Filter rows where `ell == "MX"`
mx_md_mx = median(skipmissing(filtered_DR.mx_shr))# Calculate the median of `mx_shr`, ignoring missing values
println("Median of `mx_shr` for `ell == \"MX\"`: $mx_md_mx")
#On a 42,5 alors qu'eux on 45


con_means = DataFrame(
    ell=["US", "CA", "MX"],
    mx_shr_rem_mn=[0.8, 0.6, 0.7]
)

# Perform a left join to merge `DR` with `con_means` on `ell` and remove the auxiliary column `stata_merge` if it exists
DR = leftjoin(DR, con_means, on=:ell) # Merge the DataFrames on `ell`
if :stata_merge in names(DR)
    select!(DR, Not(:stata_merge))  # Drops the `stata_merge` column
end # Remove the auxiliary column (if present)

# fix mexico shares, with conservative (con) and liberal (lib) assumptions


# Create conservative (`mx_shr_con`) and liberal (`mx_shr_lib`) versions of `mx_shr`, replacing missing values with 0 in the conservative version
DR[!, :mx_shr_con] = DR[!, :mx_shr]# Create a new column `mx_shr_con` as a copy of `mx_shr`
for i in 1:nrow(DR)
    if ismissing(DR[i, :mx_shr_con])
        DR[i, :mx_shr_con] = 0.0
    end
end # Replace `missing` values in `mx_shr_con` with 0 (conservative guess)

DR[!, :mx_shr_lib] = DR[!, :mx_shr_con] # Create a new column `mx_shr_lib` as a copy of `mx_shr_con`

# liberal assumption for mexican assembly

# Step 1: Update `mx_shr_lib` where `nafta_assembly` is true, `mx_shr` is missing, and `other1_shr` is also missing
for i in 1:nrow(DR)
    if DR[i, :nafta_assembly] && ismissing(DR[i, :mx_shr]) && ismissing(DR[i, :other1_shr])
        DR[i, :mx_shr_lib] = (100.0 - DR[i, :us_ca_shr]) * DR[i, :mx_shr_rem_mn]
    end
end

# Step 2: Update `mx_shr_lib` where `nafta_assembly` is true, `mx_shr` is missing, and `other1_shr` is not missing
for i in 1:nrow(DR)
    if DR[i, :nafta_assembly] && ismissing(DR[i, :mx_shr]) && !ismissing(DR[i, :other1_shr])
        DR[i, :mx_shr_lib] = (100.0 - DR[i, :us_ca_shr] - DR[i, :other1_shr]) * DR[i, :mx_shr_rem_mn]
    end
end

# Step 3: Cap `mx_shr_lib` at 14 where `nafta_assembly` is true, `mx_shr` is missing, and `mx_shr_lib >= 15`
for i in 1:nrow(DR)
    if DR[i, :nafta_assembly] && ismissing(DR[i, :mx_shr]) && !ismissing(DR[i, :mx_shr_lib]) && DR[i, :mx_shr_lib] >= 15
        DR[i, :mx_shr_lib] = 14.0
    end
end