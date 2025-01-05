# RoO

[![Build Status](https://github.com/JonathanGarson/RoO.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/JonathanGarson/RoO.jl/actions/workflows/CI.yml?query=branch%3Amain)

This project is the replication of **[Head&amp;al (2024)](https://www.sciencedirect.com/science/article/pii/S0022199624000357)** paper on the Laffer Curve of Rule of Origin. The objective is to reproduce the main results obtained by the authors using Julia and, eventually, improving on the speed to simulate the results. This is a last year work of the graduate program in Economics of [SciencesPo Paris Economic Department](https://www.sciencespo.fr/department-economics/), for the class of computational economics taught by Professor [Florian Oswald](https://floswald.github.io/).

**Authors : Mathilde Blanchon & Jonathan Garson & Marie-Ange Ortiz**

## Folder organisation

The folders are organised as follow:

```
├── README.md          <- The top-level README for developers using this project.
│
├── data               <- folder containing all the originary data
│   ├── AALA           <- AALA data
│   ├── GravData       <- Gravity Data
│   ├── Mfg_surveys    <- Manufacturing surveys
│   └── RDS_JIE        <- RDS data used in the paper
|
├── original_code
│   ├── Julia          <- Julia files
│   ├── Mathematica    <- Mathematica files
│   ├── Plots_JIE_rev  <- Plots for the Japanese Journal of International Economics
│   ├── R_JIE_files    <- R files to generate simulations and plots
│   └── Tables_JIE_rev <- Tables for the Japanese Journal of International Economics
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
````
Additional files :

**ROO_Pkg.jl** : Is a package which contain a fonction run. Using this package and the run() fonction allows to run the whole replication and open output.

## Computational Requirements
We indicate here the minimum ressources used for replication on our own devices. Expected run time may vary depending on your own machine.

## Expected run time

## Reproduction

At the address of the replicator. The first step is to download the package RoO. For that enter in the Julia terminal:

```
] add RoO
```
Then, to replicate easily the outputs you can run the following commands:

```
using RoO

run_short() # generate the outputs without simulation to reduce computational requirements and generation time
run_long() # generate the outputs with the simulation 
```

| file name                 | output                            |
|---------------------------|-----------------------------------|
| AALA_clean.jl             | DR (Dataframe)                    |
| AALA_clean.jl             | nafta_shr_lib_con_rev.txt (table) |
| AALA_calibration_plot.jl  | AALA_calib_model_data.pd (graph)  |
| AALA_IHS_table.jl         | AALA_IHS_table.tex (table)        |


# RoO

[![Build Status](https://github.com/JonathanGarson/RoO.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/JonathanGarson/RoO.jl/actions/workflows/CI.yml?query=branch%3Amain)

This project is the replication of **[Head&amp;al (2024)](https://www.sciencedirect.com/science/article/pii/S0022199624000357)** paper on the Laffer Curve of Rule of Origin. The objective is to reproduce the main results obtained by the authors using Julia and, eventually, improving on the speed to simulate the results. This is a last year work of the graduate program in Economics of [SciencesPo Paris Economic Department](https://www.sciencespo.fr/department-economics/), for the class of computational economics taught by Professor [Florian Oswald](https://floswald.github.io/).

**Authors : Mathilde Blanchon & Jonathan Garson & Marie-Ange Ortiz**

## Folder organisation

The folders are organised as follow:

**Data** : Contains all the necessary data for the replication of the paper in a non-proprietary formats. Data are anonymized.

**Docs** : Contains all the complementary documentation to the paper replication (the paper, the appendix, the readme).

**Original Code** : Contains both the original code in R, mathematica, and julia, as well as the data in the originary format provided in the replication package of the authors. The data are not directly available in the repo, they need to be downloaded from the replication package.

**Src** : Contains the main codes for replications (see below for more details of each code), and the test folders containing the unit tests.

Additional files : 

**ROO_Pkg.jl** : Is a package which contain a fonction run. Using this package and the run() fonction allows to run the whole replication and open output.

## Reproduction

## Replication of AALA_clean.

This Julia script performs the cleaning and analysis of AALA data from a raw CSV file. It includes the following steps : 

1.  Loading the raw data : We note that we have slightly less observations for 2017 (572 VS 576), 2018 (630 VS 640), 2019 (549  VS 586)  and 2020 (617 VS 618) than what is commented in the code. 

2. Correcting column shifts in specific years : The main cleaning and consist in correcting column shifts for certain years.
  - mfg_HQ to identify the manufacturer’s country.
  - E_US, E_CA, T_US, T_CA, etc., to determine the origins of engines and transmissions.

3. Split  the colomn percent_content_other1 and percent_content_other2, which contains both percentage and country of origine in to distinct column other1_shr et other2_shr for percent content, other1_who and  other2_who for country code. 
    We note an errer in the package replication : Running DR[,other2_shr := as.numeric(st_left(percent_content_other2,"%"))]produces only NA wich is not consistent with what the code intend to do. 
    using the following command instead allows to accuratly capture the percentage value in other2_shr :
    DR[, numeric_part := fifelse(!is.na(percent_content_other2) & grepl("^[0-9]+"percent_content_other2),ub("^([0-9]+).*", "\\1", percent_content_other2), NA_character_)]
    DR[,other2_who := st_right(percent_content_other2,"%")]
    We notice a similar issue while creating the column other2_who.
    This could lead to potential descrapency between the article's outputs and our own outputs.

4. Filtering and Tabulating Rows Based on other2_who Column, and Correcting Specific Data Issues

5. Standardize country codes (US, CA, MX) for assembly locations, filters rows based on these codes, computes summary statistics for the us_ca_shr column, and counts rows where both other1_who and other2_who contain "M," ensuring consistency and addressing data issues.

6. Creating mx_shr column and assign values from other1_shr and other2_shr to the mx_shr column for rows where other1_who or other2_who contains "M." Filter rows based on assembly location (USCAMX, MX, and MX2 sets) and compute summary statistics (mean, min, max, and counts of missing/non-missing values) for the mx_shr column across these subsets. 

7. Melt the dataset (DR) to reshape columns representing countries (ell2CA, ell2US, ell2MX) into a long format, expanding the dataset.Filter rows with valid entries in the add_plant column and rename melted columns for clarity.Drop columns starting with ell2 and append the reshaped data back to the main dataset.

8. Add a column (nafta_assembly) to identify assembly locations within NAFTA countries (US, CA, MX).
Compute a residual share (rem_shr) for rows where assembly location belongs to NAFTA and key data is missing or incomplete.Update rem_shr values iteratively, accounting for conditions involving other1_shr and related columns.

9. Compute Ratios and Summary Statistics : Calculate the ratio of Mexican shares (mx_shr) to residual shares (rem_shr) for NAFTA assembly rows and summarize the results. Group by assembly location (ell) to compute means and medians for the mx_shr_rem ratio. Derive specific statistics for Mexican shares when assembly is based in Mexico.

10. Merge and Validate Calculations

Perform a custom merge operation to integrate grouped statistics into the dataset.
Generate conservative (mx_shr_con) and liberal (mx_shr_lib) assumptions for Mexican shares based on available data and capping where necessary.
11. Summarize Shares by Assumptions

Compute overall NAFTA shares (nafta_shr_con and nafta_shr_lib) based on assembly location.
Create summary statistics for NAFTA and Mexican shares across conservative and liberal assumptions by assembly location (ell).

12. Output and Save Results : Write detailed summaries for each assembly location and assumption to a text file (nafta_shr_lib_con_rev.txt).Save the final dataset to a CSV file (data/AALA_rev.csv) for further analysis or reporting.

Remark : In order to replicate this script we had the replicate the fonction stata_merge of the author's custom package  HeadR. 

## Replication of AALA_IHS_table