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

This Julia script performs the cleaning and analysis of AALA data from a raw CSV file. It includes the following steps.

In order to replicate this script we had the replicate some fonction of the author's custom package  HeadR, 

1.  Loading the raw data : We note that we have slightly less observations for 2017 (572 VS 576), 2018 (630 VS 640), 2019 (549  VS 586)  and 2020 (617 VS 618).
2. Correcting column shifts in specific years : The main cleaning and consist in correcting column shifts for certain years.
  - mfg_HQ to identify the manufacturer’s country.
  - E_US, E_CA, T_US, T_CA, etc., to determine the origins of engines and transmissions.
3. Spliting split the colomn percent_content_other1 and percent_content_other2, which contains both percentage and country of origine in to distinct column other1_shr et other2_shr for percent content, other1_who and  other2_who for country code. 
    We note 
5. Creating aggregation variables based on assembly for NAFTA regions.
6. Calculating market shares for Mexico, the U.S., and Canada.