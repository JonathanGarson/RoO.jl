# RoO

[![Build Status](https://github.com/JonathanGarson/RoO.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/JonathanGarson/RoO.jl/actions/workflows/CI.yml?query=branch%3Amain)

This project is the replication of **[Head&amp;al (2024)](https://www.sciencedirect.com/science/article/pii/S0022199624000357)** paper on the Laffer Curve of Rule of Origin. The objective is to reproduce the main results obtained by the authors using Julia and, eventually, improving on the speed to simulate the results. This is a last year work of the graduate program in Economics of [SciencesPo Paris Economic Department](https://www.sciencespo.fr/department-economics/), for the class of computational economics taught by Professor [Florian Oswald](https://floswald.github.io/).

We opted to replicate the data cleaning procedures, the generation of key figures, and the primary simulation presented in the paper. This served as a foundation for producing additional replication outputs in subsequent stages.

**Authors : Mathilde Blanchon & Jonathan Garson & Marie-Ange Ortiz**

## Reproduction

At the address of the replicator. The first step is to download the package RoO. For that enter in the Julia terminal:

```
] add RoO
```

or alternatively in the command terminal :

```
git clone https://github.com/JonathanGarson/RoO.jl.git
cd RoO
julia # to turn to julia language
```

Then, to replicate easily the outputs you can run the following commands in the Julia REPL:

```
using RoO

run_short() # generate the outputs without simulation to reduce computational requirements and generation time
run_sim() # generate the simulation 
```


| Generating File          | Output                            | Output Folder     |
| ------------------------ | --------------------------------- | ----------------- |
| AALA_clean.jl            | DR (Dataframe)                    | output/data       |
| AALA_clean.jl            | nafta_shr_lib_con_rev.txt (table) | output/table      |
| AALA_calibration_plot.jl | AALA_calib_model_data.pdf (graph) | output/figures    |
| AALA_IHS_table.jl        | AALA_IHS_table.tex (table)        | output/table      |
| AALA_grid_search_alt.jl  | optimal_params.jls                | output/parameters |

## Folder organisation

The folders are organised as follow:

```
├── README.md          <- The top-level README for developers using this project.
│
├── data               <- folder containing all the original data (copy of `original_code/Data`)
│   ├── AALA           <- AALA data
│   ├── GravData       <- Gravity Data
│   ├── Mfg_surveys    <- Manufacturing surveys
│   └── RDS_JIE        <- RDS data used in the paper
|
├── docs               <- documentation for GitHub Pages
│   
├── references         <- Data dictionaries, manuals, and all other explanatory materials.
│   
├── output             <- Output generated by the replication package.
│    ├── data          <- folder containing all the originary data.
│    ├── figures       <- contains the figures in latex and pdf/png/etc. formats.
│    └── tables        <- contains the tables in latex and pdf/png/etc. formats.
│
├── test               <- Contains the unit test files.
│
└── src                <- Source code for use in this project.
```

## Computational Requirements

We indicate here the minimum ressources used for replication on our own devices. Expected run time may vary depending on your own machine.

*OS*: Windows 11

*CPU*: Intel i5 13600k - 3.5Ghz - up to 20 threads

*Multithreading*: the entire computation has been run on a single thread to ease portability and limits CPU requirements from device to device. From this perspective we depart from the authors more demanding computational requirements.

*RAM* : ~3.6 Gb / 32Go available

*GPU*: N/A

*Software version* : Julia 1.6

## Expected run time

The total expected running time of the replication package is:

- run_short() : ~3 mn
- run_long() : ~30 mn

For the simulation and estimation of the optimal parameters : ~25 mn (which compares to 1h for the original package).

## Data Availibility

| Dataset           | Availibility | Cleaning Code - Blanchon, Garson & Ortiz | Cleaning Code - Head, Mayer & Melitz |
| ----------------- | ------------ | ---------------------------------------- | ------------------------------------ |
| data_aala_raw.csv | YES          | AALA_clean.jl                            | AALA_clean.R                         |
| IHS_sales         | NO           | NA                                       | data_aala_raw.csv                    |

## Details on the replication code

### Replication of AALA_clean.

This Julia script performs the cleaning and analysis of AALA data from a raw CSV file. It includes the following steps :

1. Loading the raw data : We note that we have slightly less observations for 2017 (572 VS 576), 2018 (630 VS 640), 2019 (549  VS 586)  and 2020 (617 VS 618) than what is commented in the code.
2. Correcting column shifts in specific years : The main cleaning and consist in correcting column shifts for certain years.

- mfg_HQ to identify the manufacturer’s country.
- E_US, E_CA, T_US, T_CA, etc., to determine the origins of engines and transmissions.

3. Split  the colomn percent_content_other1 and percent_content_other2, which contains both percentage and country of origine in to distinct column other1_shr et other2_shr for percent content, other1_who and  other2_who for country code.
   We note an error in the package replication : Running `DR[,other2_shr := as.numeric(st_left(percent_content_other2,"%"))]` produces only `NA` wich is not consistent with what the code intend to do.
   using the following command instead allows to accuratly capture the percentage value in `other2_shr` : `DR[, numeric_part := fifelse(!is.na(percent_content_other2)`&`grepl("^[0-9]+"percent_content_other2),ub("^([0-9]+).*", "\\1", percent_content_other2), NA_character_)]` `DR[,other2_who := st_right(percent_content_other2,"%")]`
   We notice a similar issue while creating the column `other2_who`.
   This could lead to potential descrapency between the article's outputs and our own outputs.
4. Filtering and Tabulating Rows Based on `other2_who` Column, and Correcting Specific Data Issues
5. Standardize country codes (US, CA, MX) for assembly locations, filters rows based on these codes, computes summary statistics for the `us_ca_shr` column, and counts rows where both `other1_who` and `other2_who` contain "M," ensuring consistency and addressing data issues.
6. Creating `mx_shr column` and assign values from `other1_shr` and `other2_shr` to the `mx_shr` column for rows where `other1_who` or `other2_who` contains "M." Filter rows based on assembly location (USCAMX, MX, and MX2 sets) and compute summary statistics (mean, min, max, and counts of missing/non-missing values) for the `mx_shr` column across these subsets.
7. Melt the dataset (DR) to reshape columns representing countries (ell2CA, ell2US, ell2MX) into a long format, expanding the dataset. Filter rows with valid entries in the `add_plant` column and rename melted columns for clarity.Drop columns starting with ell2 and append the reshaped data back to the main dataset.
8. Add a column (`nafta_assembly`) to identify assembly locations within NAFTA countries (US, CA, MX).
   Compute a residual share (`rem_shr`) for rows where assembly location belongs to NAFTA and key data is missing or incomplete.Update `rem_shr` values iteratively, accounting for conditions involving `other1_shr` and related columns.
9. Compute Ratios and Summary Statistics : Calculate the ratio of Mexican shares (`mx_shr`) to residual shares (`rem_shr`) for NAFTA assembly rows and summarize the results. Group by assembly location (`ell`) to compute means and medians for the `mx_shr_rem` ratio. Derive specific statistics for Mexican shares when assembly is based in Mexico.
10. Merge and Validate Calculations:

    1. Perform a custom merge operation to integrate grouped statistics into the dataset.
    2. Generate conservative (`mx_shr_con`) and liberal (`mx_shr_lib`) assumptions for Mexican shares based on available data and capping where necessary.
11. Summarize Shares by Assumptions

    1. Compute overall NAFTA shares (`nafta_shr_con` and `nafta_shr_lib`) based on assembly location.
    2. Create summary statistics for NAFTA and Mexican shares across conservative and liberal assumptions by assembly location (`ell`).
12. Output and Save Results : Write detailed summaries for each assembly location and assumption to a text file (`nafta_shr_lib_con_rev.txt`).Save the final dataset to a CSV file (`data/AALA_rev.csv`) for further analysis or reporting.

Remark : In order to replicate this script we had the replicate the fonction stata_merge of the author's custom package HeadR.

### Replication of AALA_calibration_functions and AALA_calibration_brut_force

We provide two types of code for replications which reflect both different use and progression.

The firsts are `AALA_calibration_functions.jl` and `AALA_calibration_brute_force.jl` which are close copy of their R counterparts. These codes were made first and even though they are working, they are not optimized for Julia. We use them to produce output requiring few simulations such as
such as Table 1 , the counts and medians of NAFTA parts cost shares and tariff indexes,  (with `AALA_ISH_table.jl`) and Figure 8, the Density of  model vs data, (with `AALA_calibration_plot.jl.

The seconds are `AALA_solving_alt.jl` and `AALA_grid_search_alt.jl`. These codes are optimized for Julia and largely rely on matrix manipulation to enhance speed and reduce computational requirements. `AALA_solving_alt.jl` contains all the functions necessary to solve the model and `AALA_grid_search_alt.jl` execute them and store the optimal parameters in a dictionary.

Here are the results in term of computational requirements :

| Category               | Head, Mayer & Melitz                                                                                  | Blanchon, Garson & Ortiz |
| ---------------------- | ----------------------------------------------------------------------------------------------------- | ------------------------ |
| Computation time       | ~ 1 hour                                                                                              | ~ 25 minutes             |
| Computation complexity | Mulithreading                                                                                         | Single thread            |
| OS limitations         | Multithreading implemented only available on MacOS, makes computation on Windows very long (>5 hours) | Theoretically none       |
| RAM                    | Not known                                                                                             | 3.6 Gb                   |

And here the difference between the results of the four optimized parameters obtained by HMM and obtained MGO:

| Parameters           | Head, Mayer & Melitz | Blanchon, Garson & Ortiz |
| -------------------- | -------------------- | ------------------------ |
| mu                   | 0.12                 | 0.2                      |
| sigma                | 0.18                 | 0.18                     |
| alpha_concentration  | 3.0                  | 3.0                      |
| erreur_concentration | 18                   | 17                       |

### Replication of AALA_IHS_table

AALA_IHS_table produces Table 1 of the paper, which summarizes key statistics from the AALA data. It presents, for each North American assembly country (Canada, Mexico, and the USA), the following metrics:

- The number of vehicles.
- The median parts cost share  for cars (including Sport Utility and Multi-Purpose Vehicles such as minivans) and light trucks (pickup trucks and vans).

### Replication of AALA_calibration_plot

AALA_calibration_plot generates a graph with the empirical distribution of regional shares for carlines produced in Canada, Mexico, or the USA, pooled across the years 2011–2019, during which the RCR (Regional Content Requirement) was 62.5%. The resulting distribution is depicted as the black line in Figure 8. This visual representation allows for a comparison between observed data (AALA data) and model predictions. This graph is designed to facilitate the comparison of theoretical model predictions with real-world data, emphasizing areas of misalignment.
