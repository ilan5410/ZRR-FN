# ==============================================================================
# DEFINE PATHS AND LABELS
# ==============================================================================
# No action needed
# ==============================================================================


# ==============================================================================
# DEFINE PATHS 
# ==============================================================================


path_data                 <- paste0(main_path, "DATA/")
raw_data_path             <- paste0(path_data, "raw data/")
processed_data_path       <- paste0(path_data,  "processed data/")

raw_code_path             <- paste0(main_path, "CODE/")
path_code_prepare_data    <- paste0(raw_code_path, "prepare data/")
path_code_prepare_figures <- paste0(raw_code_path, "prepare figures/")
path_code_prepare_tables  <- paste0(raw_code_path, "prepare tables/")

path_out                  <- paste0(main_path, "OUTPUT/")
path_figures              <- paste0(path_out, "figures/")
path_tables               <- paste0(path_out, "tables/")

# ==============================================================================
# DEFINE BANDWIDTHS 
# ==============================================================================

bandwidths = c(20000, 10000, 5000)

# ==============================================================================
# DEFINE CONTROLS 
# ==============================================================================

controls <- c("pchom", "FN1988", "delta_pop_1980_1995", "pop", "poph", "popf", 
              "ratEmp", "ratForeigners", "popYoungOld", "asso", 
              "educNoDiplomaPerK", "educSUPPerK", "educBACPerK", 
              "educCAPBEPPerK", "pagri", "pindp", "ppint", "pempl", "pouvr", 
              "altitude", "superficie", "min_distance_to_agglo", "logVac", 
              "revenuPerK", "popDensity", "typologie"         
              )


# ==============================================================================
# DEFINE LABELS 
# ==============================================================================

labels <- c("FN1995" = "FN vote share in 1995",
            "FN2002" = "FN vote share in 2002",
            "RPR2002" = "RPR vote share in 2002",
            "turnout_2002" = "Turnout in 2002",
            "FN2007" = "FN vote share in 2007",
            "FN2012" = "FN vote share in 2012",
            "FN1988"= "Vote share for FN in 1988",
            "pchom" = "Unemployed (%)", 
            "pop" = "Population", 
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
            "typologie" = "Classification rural/urban")
