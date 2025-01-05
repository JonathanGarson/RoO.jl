using CSV
using DataFrames
using DataFramesMeta
using StatsBase
using Statistics
using IterTools
include("files_path.jl")


file_path = "$input_raw_data/data_aala_raw.csv"

#Load raw data
DR = CSV.read(file_path, DataFrame)

# Checking that we have the same number of observation by year than the original paper
# Frequency_table = countmap(DR.year)
# 2011 => 379,  2012 => 347, 2013 => 386, 2014 => 370, 2015 => 432, 2016 => 385, 2017 => 572, 2018 => 630, 2019 => 549, 2020 => 617
# We note that we have slightly less observations for 2017 (572 VS 576), 2018 (630 VS 640), 2019 (549  VS 586)  and 2020 (617 VS 618).
# This could be due to later modifications to raw data       

# First step : cleanning problems in raw data.

#1. Fix some shifted columns in 2011 and 2017
DR.flag = (DR.source_T1 .== "") .& (DR.source_T2 .!= "")# Create a "flag" column to identify rows that need fixing
flagged_indices = findall(DR.flag)  # Indices where flag is true

for i in flagged_indices # Move data from source_T2 to source_T1 and clear source_T2
    DR.source_T1[i] = DR.source_T2[i]
    DR.source_T2[i] = ""
end

for i in flagged_indices #Apply specific fixes based on `carlines` 
    if DR.carlines[i] == "Renegade"
        DR.final_assembly1[i] = "IT"  # Correct the Melfi factory that was listed as US while its IT
    elseif DR.carlines[i] == "ATS" #last 4 vars all shifted
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

# Step 2 : create new variables 

# create variable mfg_HQ
@transform!(DR, :mfg_HQ = "ROW") # Default manufacturing headquarters (HQ) to "ROW"

    # Set HQ to "USA" based on manufacturer
    @rtransform!(DR, 
        :mfg_HQ = if occursin(r"^General", :mfg) || :mfg == "GM LLC" || occursin(r"^Ford", :mfg) || occursin(r"^Tesla", :mfg)
            "USA"
        else
            :mfg_HQ
        end
    ) 

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

# Create variables to determine the source of engines
@rtransform!(DR, 
    :E_US = occursin(r"US", :source_E1) || occursin(r"US", :source_E2)
)

@rtransform!(DR, 
    :E_CA = occursin(r"CN", :source_E1) || occursin(r"CN", :source_E2) || 
            occursin(r"CAN", :source_E1) || occursin(r"CAN", :source_E2) || 
            ((occursin(r"^C", :source_E1) && !occursin(r"CH", :source_E1)) || 
             (occursin(r"^C", :source_E2) && !occursin(r"CH", :source_E2)))
)

# Create variables to determine the source of the transmission.
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

# Create new varaible containing the content shares produced in US CA
@rtransform!(DR, 
    :us_ca_shr = if !ismissing(:percent_content_USA_CAN) && !isempty(:percent_content_USA_CAN)
        # Supprimer le symbole `%`, diviser par les sauts de ligne, et prendre le premier chiffre
        parse(Float64, split(replace(string(:percent_content_USA_CAN), "%" => ""), r"\n")[1])
    else
        missing
    end
)

# missing_values = DR[ismissing.(DR.us_ca_shr), :percent_content_USA_CAN]
# As in the original article, there are 52 blanks

# we count the frequencies of the values taken by final_assembly 1.
# println(countmap(DR.final_assembly1))
# We obtain the same results as the original paper

# Create new varaible containing the content shares of produced by countries other than US CA
 DR.other1_shr = map(x -> 
 if ismissing(x) || isempty(x)
     missing
 else
     m = match(r"^\d+", x)
     m === nothing ? missing : parse(Float64, m.match)
 end, 
 DR.percent_content_other1
)

# Create new varaible containing the country code of the country that produced the content 
DR.other1_who = map(x -> 
    if ismissing(x) || isempty(x)
        missing
    else
        m = match(r".*%", x)
        m === nothing ? missing : replace(x, r".*%" => "")
    end, 
    DR.percent_content_other1
)


# Filter rows where `other1_who` starts with "M"
starts_with_M = filter(row -> 
    !ismissing(row[:other1_who]) && occursin(r"^M", row[:other1_who]), 
    eachrow(DR)
)

# check that we have the same number of observations as in the original paper.
# println(countmap([row[:other1_who] for row in starts_with_M]))
# we have 2 more obs for M

# Filter rows where `other1_who` contains "M"
contains_M = filter(row -> !ismissing(row[:other1_who]) && occursin("M", row[:other1_who]), DR)
frequency_table_2 = countmap(contains_M.other1_who)

#check that we have the same as original paper
#println(frequency_table_2)
# Dict{Union{Missing, String}, Int64}(" Mexico" => 15, "M" => 218, "Mexico" => 2, " MX" => 61, " M " => 6, " M" => 357)

# Count non-empty, non-missing entries in the column `percent_content_other2`
non_empty_count = sum(.!ismissing.(DR.percent_content_other2) .& .!(DR.percent_content_other2 .== ""))#1335 obs as in the original code

# Filter rows where `percent_content_other2` contains "M", handling missing values, and count occurrences
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

# Line 76 there is an issue with the original code.
#Running DR[,other2_shr := as.numeric(st_left(percent_content_other2,"%"))]produces only NA
# using DR[, numeric_part := fifelse(!is.na(percent_content_other2) & grepl("^[0-9]+", percent_content_other2),
#                              sub("^([0-9]+).*", "\\1", percent_content_other2),
#                              NA_character_)]
#DR[,other2_who := st_right(percent_content_other2,"%")]
#allows to accuratly capture the percentage value in other2_shr.

#Additional checks
#println("Summary of other2_shr:")
#println(describe(DR.other2_shr))

#Extract Characters After % into other2_who
DR.other2_who = map(x -> 
    if ismissing(x) || isempty(x)
        missing
    else
        replace(x, r".*%" => "")
    end, DR.percent_content_other2
)

# There is a similar issue line 80 above in the original code
#the command DR[,other2_who := st_right(percent_content_other2,"%")]creates a collumn of NA
# using DR[, letters_part := fifelse(
#  !is.na(percent_content_other2) & grepl("%", percent_content_other2), # Vérifier si `%` est présent
#  sapply(strsplit(percent_content_other2, "%"), function(x) ifelse(length(x) > 1, gsub("[^A-Za-z]", "", x[2]), NA_character_)),
#  NA_character_
#)]
#Allows to build other2_who accurately



#Filter Rows Where other2_who Contains "M" and Tabulate:
rows_with_M_in_other2_who = filter(row -> !ismissing(row[:other2_who]) && occursin("M", row[:other2_who]), eachrow(DR))
println("Table of other2_who with 'M':")
println(countmap([row[:other2_who] for row in rows_with_M_in_other2_who]))

#Filter Rows Where other2_who Contains "M" and other2_shr Is missing:
#rows_with_M_and_missing_other2_shr = filter(row -> !ismissing(row[:other2_who]) && occursin("M", row[:other2_who]) && ismissing(row[:other2_shr]), DR)
#println(rows_with_M_and_missing_other2_shr =)

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


#summary statistics  for the us_ca_shr column in a DataFrame, filtered by whether a specified assembly location 
    filtered1 = filter(row -> row[:final_assembly1] in USCAMX, eachrow(DR))
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
    println(summary1)
    println(summary2)
#we obtain the same result as original code (modified for spotted issues)

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
    for i in 1:nrow(DR)
        if !ismissing(DR[i, :other1_who]) && occursin(" M", DR[i, :other1_who])
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
] # Create ell


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

# Identify and dropcolumns whose names start with "ell2"
cols_to_delete = filter(name -> occursin(r"^ell2", name), names(DR))
select!(DR, Not(Symbol.(cols_to_delete)))

#append the additional sites, fill in add_plant as false for the final_assembly1 sites: 4788 obs now : 4666+122
DR= vcat(DR, D2; cols=:union) #Append the DataFrames, ensuring all columns are included
DR.add_plant .= coalesce.(DR.add_plant, false) # Replace missing values in `add_plant` with `false`
#
# Now starts the (complex) bounding of Mexican share (which is not a variable per se)
#
# there are two is.na(ell), that have multiple assembly countries in north america
result = combine(
    DataFrames.groupby(DR, :ell),
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


# Calculate the ratio of `mx_shr` to `rem_shr` for NAFTA assembly rows
# Initialize the `mx_shr_rem` column with `missing`
DR[!, :mx_shr_rem] = Vector{Union{Missing, Float64}}(missing, nrow(DR))

# Loop through each row to calculate `mx_shr_rem`
for i in 1:nrow(DR)
    if DR[i, :nafta_assembly] && !ismissing(DR[i, :mx_shr]) && !ismissing(DR[i, :rem_shr]) 
        DR[i, :mx_shr_rem] = DR[i, :mx_shr] / DR[i, :rem_shr]
    end
end

# Summarize `mx_shr_rem` for NAFTA assembly rows
# Generate summary statistics for `mx_shr_rem` for NAFTA assembly rows
# Create a DataFrame from the filtered rows
#filtered_nafta = DataFrame(nafta_assembly_true)
#filtered_nafta_without_NAN = dropmissing(filtered_nafta)
#summary_stats_4 = describe(filtered_nafta_without_NAN.mx_shr_rem)

# Calculate the mean of `mx_shr_rem` for each `ell` group where `nafta_assembly` is true and `mx_shr_rem < 1`
# Filter rows where `nafta_assembly` is true and `mx_shr_rem < 1`

filtered_DR = filter(row -> 
    !ismissing(row[:nafta_assembly]) && row[:nafta_assembly] && 
    !ismissing(row[:mx_shr_rem]) && row[:mx_shr_rem] < 1, DR)

# Group by `ell` and compute the mean of `mx_shr_rem`, skipping missing values
con_means = combine(
    DataFrames.groupby(filtered_DR, :ell),
    :mx_shr_rem => (x -> mean(skipmissing(x))) => :mx_shr_rem_mn
)



# Calculate the median of `mx_shr_rem` for each `ell` group where `nafta_assembly` is true
filtered_nafta = filter(row -> row[:nafta_assembly] == true, DR)
filtered_df = DataFrame(filtered_nafta) # Create a DataFrame from the filtered rows
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
#We obtain 41 as in the original code.

# fonction to replicate stat_merge from package headR


function merge_stata(DTx::DataFrame, DTy::DataFrame, by)
    # Create copies to avoid modifying the original dataframes
    x = deepcopy(DTx)
    y = deepcopy(DTy)
    
    # Add indicator columns
    x[!, :stata_master] .= 1
    y[!, :stata_using] .= 2
    
    # Perform a full outer join
    DT = outerjoin(x, y, on = by, makeunique=true)
    
    # Define a custom function to handle missing values
    function sum_with_na(x, y)
        if isnothing(x)
            return y
        elseif isnothing(y)
            return x
        else
            return x + y
        end
    end
    
    # Compute the stata_merge column
    DT[:, :stata_merge] = map(sum_with_na, DT[:, :stata_master], DT[:, :stata_using])
    
    # Remove temporary indicator columns
    select!(DT, Not([:stata_master, :stata_using]))
    
    # Count rows by stata_merge categories
    counts = combine(DataFrames.groupby(DT, :stata_merge), nrow => :count)
    println(counts)
    
    # Return the merged DataFrame
    return DT
end

# Merge the DataFrames on `ell`
DR =merge_stata(DR,con_means, :ell)

# fix mexico shares, with conservative (con) and liberal (lib) assumptions

# Create conservative (`mx_shr_con`) and liberal (`mx_shr_lib`) versions of `mx_shr`, replacing missing values with 0 in the conservative version
DR[!, :mx_shr_con] = copy(DR[!, :mx_shr])# Create a new column `mx_shr_con` as a copy of `mx_shr`
for i in 1:nrow(DR)
    if ismissing(DR[i, :mx_shr_con])
        DR[i, :mx_shr_con] = 0.0
    end
end # Replace `missing` values in `mx_shr_con` with 0 (conservative guess)
DR[!, :mx_shr_lib] = Float64.(coalesce.(DR[!, :mx_shr_con], missing))
# liberal assumption for mexican assembly
# Update :mx_shr_lib in the loop
for i in 1:nrow(DR)
    if DR[i, :nafta_assembly] && ismissing(DR[i, :mx_shr]) && ismissing(DR[i, :other1_shr]) 
        DR[i, :mx_shr_lib] = (100 - DR[i, :us_ca_shr]) * DR[i, :mx_shr_rem_mn]
    end
end

# Update `mx_shr_lib` where `nafta_assembly` is true, `mx_shr` is missing, and `other1_shr` is also missing
for i in 1:nrow(DR)
    if DR[i, :nafta_assembly] && ismissing(DR[i, :mx_shr]) && !ismissing(DR[i, :other1_shr])
        DR[i, :mx_shr_lib] = DR[i, :mx_shr_lib] = (100 - DR[i, :us_ca_shr] - DR[i, :other1_shr]) * DR[i, :mx_shr_rem_mn]
    end
end #Update `mx_shr_lib` where `nafta_assembly` is true, `mx_shr` is missing, and `other1_shr` is not missing

for i in 1:nrow(DR)
    if DR[i, :nafta_assembly] && ismissing(DR[i, :mx_shr]) && DR[i, :mx_shr_lib] >= 15
        DR[i, :mx_shr_lib] = 14
    end
end# Cap `mx_shr_lib` at 14 where `nafta_assembly` is true, `mx_shr` is missing, and `mx_shr_lib >= 15`

# Create Cartesian product of `ell` and assumptions
# Define the vectors
ell = ["US", "CA", "MX"]
assump = ["con", "lib"]

# Calculer le produit cartésien et convertir en DataFrame
it = DataFrame(
    ell = ["CA", "CA", "MX", "MX", "US", "US"],
    assump = ["con", "lib", "con", "lib", "con", "lib"]
)
# Display the resulting DataFrame
println(it)

function sumit(DR::DataFrame, x::String, y::String)
    # Dynamically construct the column name
    column_name = ("mx_shr_" * y)
     # Check if the column exists in the original DataFrame
     if !(column_name in names(DR))
        error("The column $column_name does not exist in the DataFrame.")
    end
    #Filter rows where ell == x
    filtered_data_sumit = filter(row -> row.ell == x, DR)
    # Calculate summary statistics for the filtered column
    summary_stats = describe(filtered_data_sumit[!, column_name])
    return summary_stats
    
end

DR[!, :nafta_shr_lib] = DR[!, :us_ca_shr] .+ coalesce.(DR[!, :mx_shr_lib], 0.0)
DR[!, :nafta_shr_con] = DR[!, :us_ca_shr] .+ coalesce.(DR[!, :mx_shr_con], 0.0)

function sumit_2(DR::DataFrame, x::String, y::String)
    # Dynamically construct the column name
    column_name = ("nafta_shr_" * y)
     # Check if the column exists in the original DataFrame
     if !(column_name in names(DR))
        error("The column $column_name does not exist in the DataFrame.")
    end
    #Filter rows where ell == x
    filtered_data_sumit = filter(row -> row.ell == x, DR)
    # Calculate summary statistics for the filtered column
    summary_stats = describe(filtered_data_sumit[!, column_name])
    return summary_stats
    
end
# Call the function
#result = sumit(DR, "CA", "con")
#println(result)
#same result as original code
result = sumit_2(DR, "CA", "con")
println(result)


#  Redirect output to a file
open("$output_tables/nafta_shr_lib_con_rev.txt", "w") do file
    redirect_stdout(file)
    
    println("Mexico shares by assembly location, conservative and then liberal")
    
    # Apply cfunction for all combinations in it
    for i in 1:nrow(it)
        ell_value = it[i, :ell]
        assump_value = it[i, :assump]
        println("Summary for ell = $ell_value and assumption = $assump_value:")
        println(sumit(DR, ell_value, assump_value))
    end

    println("Nafta shares by assembly location")
    for i in 1:nrow(it)
        ell_value = it[i, :ell]
        assump_value = it[i, :assump]
        println("Summary for ell = $ell_value and assumption = $assump_value:")
        println(sumit_2(DR, ell_value, assump_value))
    end 
    # End redirection automatically at the end of the block
end

CSV.write("$output_data/AALA_rev.csv", DR)