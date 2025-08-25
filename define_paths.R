pacman::p_load(
  readxl, dplyr, tidyr, ggplot2, sf, stargazer, AER, broom, ggrepel, sf, viridis, stringr, ragg, fixest, sandwich, zoo, readr, rdd, rddtools, magrittr, rdrobust, estimatr, knitr, modelsummary, rddensity, gridExtra, rnaturalearth, rnaturalearthdata, forcats, cowplot, plm, grid, patchwork, reshape2,
  kableExtra, caret, htetree, glmnet, rpart, randomForest, purrr, devtools,  SuperLearner, xgboost, psych, MatchIt, multiwayvcov, clubSandwich, patchwork, stringr, progress, scales
  
)


rm(list=ls())
path_out <- "/Users/ilanpargamin/Desktop/ECONOMICS/thesis/THESIS_REPRO July 2025/OUTPUT/"
path_figures <- paste0(path_out, "figures/")
path_tables <- paste0(path_out, "tables/")
keep_vars<- c("path_data", "path_out", "path_figures", "path_tables")

main_path <- "/Users/ilanpargamin/Desktop/ECONOMICS/thesis/THESIS_REPRO July 2025/"
path_data <- paste0(main_path, "data/")

raw_data_path <- paste0(path_data, "raw data/")
processed_data_path <-  paste0(path_data,  "processed data/")


# To print the latex table in a specific way
options("modelsummary_format_numeric_latex" = "plain")


# Labels
labels <- c("FN1995" = "FN vote share in 1995",
            "FN2002" = "FN vote share in 2002",
            "RPR2002" = "RPR vote share in 2002",
            "turnout_2002" = "Turnout in 2002",
            "FN2007" = "FN vote share in 2007",
            "FN2012" = "FN vote share in 2012",
            "FN1988"= "Vote share for FN in 1988",
            "pchom" = "Unemployed (%)", 
            "pop" = "Population", 
            "log_pop" = "Population (log)",
            "ratEmp" = "In the labor force (%)", 
            "ratForeigners" = "Foreigners (%)", 
            "asso" = "OPI per 1,000 inhabitants", 
            "educNoDiplomaPerK" = "No diploma (%)", 
            "educSUPPerK" = "Academic (%)", 
            "educBACPerK" = "Highschool (%)", 
            "educCAPBEPPerK" = "Technical (%)", 
            "poph" = "Ages 20-40 (%), men", 
            "popf" = "Ages 20-40 (%), women", 
            "pagri" = "Agriculture (%)", 
            "pindp" = "Independant (%)", 
            "ppint" = "Intermediate occupations (%)", 
            "pempl" = "Clerical (%)", 
            "pouvr" = "Manual (%)", 
            "altitude" = "Altitude", 
            "superficie" = "Area in km2 (log)", 
            "logVac" = "Vacant housing (%)", 
            "haie" = "Fences per squared km", 
            "vigne" = "Vines per squared km", 
            "revenuPerK" = "Taxable income per capita  (log)",
            "delta_pop_1982_1990" = "Population change in p.p. 1982-1990", 
            "delta_emp_1982_1990" = "Change in p.p. in the labor force 1982-1990", 
            "min_distance_to_agglo" = "Distance to closest agglomeration in meters (log)",
            "delta_pop_1980_1995" = "Population change in p.p. 1980-1990",
            "popDensity" = "Population density",
            "popYoungOld" = "Age ratio young/old (%)",
            "nobs" = "Number of observations",
            "FN" = "FN vote share")
