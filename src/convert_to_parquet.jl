using RData
using CSV
using Parquet
using DataFrames

# Function to load data from supported formats
function load_data(file_path::String)::Tuple{Any, String}
    if endswith(file_path, ".rds")
        R"roo_data <- readRDS('$file_path')"
        return (R"roo_data", "R")
    elseif endswith(file_path, ".csv")
        return (CSV.read(file_path, DataFrame), "CSV")
    elseif endswith(file_path, ".parquet")
        return (Parquet.read_parquet(file_path), "Parquet")
    elseif endswith(file_path, [".tsv", ".txt"])  # For tab-separated or text files
        return (CSV.read(file_path, DataFrame; delim='\t'), "TSV")
    else
        error("Unsupported file format: $file_path")
    end
end

# Function to convert loaded data to Parquet format
function convert_to_parquet(; input_folder_path::String, output_folder_path::String)::Vector{String}
    generated_files = String[]
    # List all files in the input folder
    collect_files = readdir(input_folder_path)

    # Create the output folder if it does not exist
    if !isdir(output_folder_path)
        mkpath(output_folder_path)
    end

    # Loop through the files and process each one
    for file in collect_files
        file_path = joinpath(input_folder_path, file)

        try
            # Load the data using the load_data function
            if endswith(file, ".rds")
                escaped_file_path = replace(file_path, "\\" => "\\\\")  # Escape backslashes for R
                R"roo_data <- readRDS(\"$escaped_file_path\")"
                data = R"roo_data"
                df = DataFrame(R"as.data.frame(roo_data)")
            else
                data, format = load_data(file_path)
                df = DataFrame(data)
            end

            # Write the DataFrame to a Parquet file in the output folder
            parquet_file_path = joinpath(output_folder_path, replace(file, r"\\..+$" => ".parquet"))
            Parquet.write(parquet_file_path, df)
        catch e
            println("Failed to process file $file_path: $e")
        end
    end
    return generated_files
end


# Convert the data in the input folder to Parquet format and save in the output folder
folders = ["AALA", "Gravdata", "Mfg_surveys", "RDS_JIE_rev"]
for folder in folders
    convert_to_parquet(input_folder_path=joinpath("data", folder), output_folder_path=joinpath("data","parquet",folder))
end
