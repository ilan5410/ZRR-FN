# main.RData


# ==============================================================================
# French Commune Data Processing Pipeline
# ==============================================================================
# This script processes various socio-economic indicators for French communes
# and creates a comprehensive dataset for analysis
# ==============================================================================


# ==============================================================================
# MAIN DATA PROCESSING PIPELINE
# ==============================================================================

#' Process French commune data
#' @param raw_data_path Path to raw data directory
#' @param processed_data_path Path to processed data directory
process_commune_data <- function(raw_data_path, processed_data_path) {
  
  cat("Starting commune data processing...\n")
  
  # --------------------------------------------------------------------------
  # 1. INITIALIZE BASE DATASET (ZRR Data)
  # --------------------------------------------------------------------------
  cat("Loading ZRR base data...\n")
  
  dfZRR_raw <- read.csv(file.path(raw_data_path, "ZRR.csv")) %>%
    select(-treatmentLong, -treatment, -nom) %>%
    mutate(codecommune = standardize_commune_codes(codecommune))
  
  # Add canton codes from shapefile
  canton_data <- st_read(file.path(raw_data_path, "france1999.dbf")) %>%
    mutate(
      codecommune = standardize_commune_codes(paste0(DEP, COM)),
      canton = paste0(DEP, CT)
    ) %>%
    select(codecommune, canton) %>%
    mutate(canton = as.character(canton))
  
  dfZRR_raw <- merge(dfZRR_raw, canton_data, by = "codecommune")
  
  # --------------------------------------------------------------------------
  # 2. ADD ELECTORAL DATA
  # --------------------------------------------------------------------------
  cat("Processing electoral data...\n")
  
  # Presidential elections (FN)
  electoral_pres <- load_and_process_data(
    file.path(raw_data_path, "pvoixFNpres.xlsx"),
    col_types = c(codecommune = "text")
  ) %>%
    select(codecommune, starts_with("FN")) # %>%
    # select(-FN1988, -FN1995)  # Keep these for later use
  
  dfZRR_raw <- left_join(dfZRR_raw, electoral_pres, by = "codecommune")
  
  # European elections (FN)
  electoral_eu <- load_and_process_data(
    file.path(raw_data_path, "EU.xlsx"),
    col_types = c(codecommune = "text")
  ) %>%
    select(codecommune, starts_with("FN"))
  
  dfZRR_raw <- left_join(dfZRR_raw, electoral_eu, by = "codecommune")
  
  # RPR Presidential elections
  electoral_rpr <- load_and_process_data(
    file.path(raw_data_path, "pvoixRPRpres.xlsx"),
    col_types = c(codecommune = "text")
  ) %>%
    select(codecommune, starts_with("RPR"))
  
  dfZRR_raw <- left_join(dfZRR_raw, electoral_rpr, by = "codecommune")
  
  # Turnout data
  turnout_data <- load_and_process_data(
    file.path(raw_data_path, "df_turnout.csv")
  ) %>%
    select(codecommune, starts_with("turnout_2002")) %>%
    filter(!is.na(turnout_2002))
  
  dfZRR_raw <- left_join(dfZRR_raw, turnout_data, by = "codecommune")
  
  # --------------------------------------------------------------------------
  # 3. PROCESS TIME-VARYING CONTROL VARIABLES
  # --------------------------------------------------------------------------
  cat("Processing time-varying control variables...\n")
  
  # Initialize merged dataset with unemployment data
  unemployment_data <- load_and_process_data(
    file.path(raw_data_path, "chomCommune.xlsx"),
    col_types = c(codecommune = "text")
  )
  
  df_merged <- interpolate_variable(unemployment_data, "pchom") %>%
    mutate(pchom = pchom / 100)  # Convert to proportion
  
  # Add previous election results
  previous_elections <- load_and_process_data(
    file.path(raw_data_path, "pvoixFNpres.xlsx"),
    col_types = c(codecommune = "text")
  ) %>%
    select(codecommune, nomcommune, dep, FN1995, FN1988) %>%
    mutate(
      FN1988 = as.numeric(FN1988),
      FN1995 = as.numeric(FN1995)
    )
  
  df_merged <- left_join(df_merged, previous_elections, by = "codecommune")
  
  # --------------------------------------------------------------------------
  # 4. PROCESS POPULATION DATA
  # --------------------------------------------------------------------------
  cat("Processing population data...\n")
  
  population_data <- load_and_process_data(
    file.path(raw_data_path, "popcommunes.csv")
  )
  
  # Select population columns and calculate growth
  pop_cols <- names(population_data)[grepl("^pop", names(population_data)) & nchar(names(population_data)) < 9]
  
  dfPop <- population_data %>%
    select(codecommune, reg, all_of(pop_cols)) %>%
    mutate(
      codecommune = standardize_commune_codes(codecommune),
      delta_pop_1980_1995 = (pop1995 - pop1980) / pop1980
    )
  
  # Interpolate population data
  dfPopLong <- interpolate_variable(dfPop, "pop")
  df_merged <- left_join(df_merged, dfPopLong, by = c("codecommune", "year"))
  
  
  
  
  
  # Population shares data

  col_names <- read_csv(file = file.path(raw_data_path, "agesexcommunes.csv"), show_col_types = FALSE) %>% names()
  pop_share_data <- read_csv(file = file.path(raw_data_path, "agesexcommunes.csv"), 
                 col_types = ifelse(grepl("^poph1539|^popf1539", col_names), "c", "_"),
                 show_col_types = FALSE)
  

  pop_share_data <- pop_share_data %>% select(c("codecommune", names(pop_share_data)[grepl("^poph1539", names(pop_share_data)) | 
                                                   grepl("^popf1539", names(pop_share_data))])) 
  
  pop_share_data <- inner_join(pop_share_data, dfPop, by = "codecommune")
  
  
  for (year in 1965:2022) {
    pop_share_data <- pop_share_data %>%
      mutate(!!paste0("poph", year) :=  .data[[paste0("poph1539", year)]] / .data[[paste0("pop", year)]]) %>%
      mutate(!!paste0("popf", year) :=  .data[[paste0("popf1539", year)]] / .data[[paste0("pop", year)]])
  }
  
  pop_share_data <- pop_share_data[, !grepl("1539", names(pop_share_data))]
  pop_share_data <- pop_share_data[, grepl("codecommune|popf|poph", names(pop_share_data))]
  
  pop_share_data <- pop_share_data %>%
    mutate(codecommune = as.character(codecommune)) %>%
    mutate(codecommune = sub("^0+", "", as.character(codecommune)))
  
  pop_share_data_interpolated_poph <- interpolate_variable(pop_share_data %>% select(codecommune, starts_with("poph")) , "poph")
  pop_share_data_interpolated_popf <- interpolate_variable(pop_share_data %>% select(codecommune, starts_with("popf")) , "popf")
  pop_share_data_interpolated <- inner_join(pop_share_data_interpolated_poph, pop_share_data_interpolated_popf, 
                                            by = c("codecommune", "year"))
  
  df_merged <- left_join(df_merged, 
                         pop_share_data_interpolated %>%
                           select(codecommune, year, poph, popf),
                         by = c("codecommune", "year"))
  

  # --------------------------------------------------------------------------
  # 5. PROCESS EMPLOYMENT DATA
  # --------------------------------------------------------------------------
  cat("Processing employment data...\n")
  
  employment_data <- load_and_process_data(
    file.path(raw_data_path, "txEmploi.csv")
  ) %>%
    left_join(
      dfPop %>% select(codecommune, pop1975, pop1982, pop1990, pop1999, pop2009, pop2014, pop2020),
      by = "codecommune"
    )
  
  # Calculate employment ratios
  emp_years <- c(1975, 1982, 1990, 1999, 2009, 2014, 2020)
  for (y in emp_years) {
    employment_data[[paste0("ratEmp", y)]] <- employment_data[[paste0("emp", y)]] / employment_data[[paste0("pop", y)]]
  }
  
  employment_interpolated <- interpolate_variable(
    employment_data %>% select(codecommune, starts_with("ratEmp")),
    "ratEmp"
  )
  
  df_merged <- left_join(df_merged, employment_interpolated, by = c("codecommune", "year"))
  
  # --------------------------------------------------------------------------
  # 6. PROCESS DEMOGRAPHIC DATA
  # --------------------------------------------------------------------------
  cat("Processing demographic data...\n")
  
  # Foreign population percentage
  foreigners_data <- load_and_process_data(
    file.path(raw_data_path, "etrangers.csv")
  ) %>%
    left_join(
      dfPop %>% select(codecommune, matches("pop(197[5-9]|19[8-9][0-9]|200[0-9]|201[0-9]|2020)")),
      by = "codecommune"
    )
  
  # Calculate foreigner ratios
  for (y in 1975:2020) {
    if (paste0("etranger", y) %in% names(foreigners_data) && paste0("pop", y) %in% names(foreigners_data)) {
      foreigners_data[[paste0("ratForeigners", y)]] <- foreigners_data[[paste0("etranger", y)]] / foreigners_data[[paste0("pop", y)]]
    }
  }
  
  foreigners_interpolated <- interpolate_variable(
    foreigners_data %>% select(codecommune, starts_with("ratForeigners")),
    "ratForeigners"
  )
  
  df_merged <- left_join(df_merged, foreigners_interpolated, by = c("codecommune", "year"))
  
  # Age structure data
  age_data <- load_and_process_data(
    file.path(raw_data_path, "age_demog.xlsx")
  ) %>%
    select(codecommune, popYoungOld1995, popYoungOld2002)
  
  age_interpolated <- interpolate_variable(age_data, "popYoungOld")
  df_merged <- left_join(df_merged, age_interpolated, by = c("codecommune", "year"))
  
  # --------------------------------------------------------------------------
  # 7. PROCESS ADDITIONAL SOCIO-ECONOMIC DATA
  # --------------------------------------------------------------------------
  cat("Processing additional socio-economic data...\n")
  
  # Associations data (JOAFE)
  associations_data <- load_and_process_data(
    file.path(raw_data_path, "dbStock.xlsx")
  ) %>%
    rename(codecommune = insee) %>%
    mutate(codecommune = standardize_commune_codes(codecommune)) %>%
    select(codecommune, starts_with("asso")) %>%
    inner_join(dfPop, by = "codecommune")
  
  # Transform association data (log per 1000 inhabitants)
  years <- 1965:2022
  associations_data <- reduce(years, function(df, y) {
    asso_var <- paste0("asso", y)
    pop_var <- paste0("pop", y)
    if (asso_var %in% names(df) && pop_var %in% names(df)) {
      df %>% mutate(!!asso_var := log(1000 * .data[[asso_var]] / .data[[pop_var]]))
    } else {
      df
    }
  }, .init = associations_data) %>%
    select(codecommune, matches("^asso"))
  
  associations_interpolated <- interpolate_variable(associations_data, "asso")
  df_merged <- left_join(df_merged, associations_interpolated, by = c("codecommune", "year"))
  
  # Education data
  education_data <- load_and_process_data(
    file.path(raw_data_path, "educProcessed.xlsx")
  )
  
  educ_cols <- names(education_data)[grepl("^educ(NoDiploma|SUP|BAC|CAPBEP)PerK", names(education_data))]
  education_data <- education_data %>%
    select(codecommune, all_of(educ_cols))
  
  education_interpolated <- interpolate_variable(
    education_data,
    c("educNoDiplomaPerK", "educSUPPerK", "educBACPerK", "educCAPBEPPerK")
  ) %>%
    # remove columns with suffixes .x and .y
    select(c("codecommune", "year", "educNoDiplomaPerK", "educSUPPerK", "educBACPerK", "educCAPBEPPerK"))
  
  
  df_merged <- left_join(df_merged, education_interpolated, by = c("codecommune", "year"))
  
  ## CSP (socio-economic status shares)
  cat("Processing CSP data...\n")
  
  col_names <- read_csv(file = file.path(raw_data_path, "cspcommunes.csv"), n_max = 0, show_col_types = FALSE) %>% names()
  
  csp_data <- read_csv(file.path(raw_data_path, "cspcommunes.csv"), col_types = ifelse(grepl("^pagri|pindp|pempl|pouvr|ppint", col_names), "c", "_"), show_col_types = FALSE)  
  
  csp_data <- csp_data %>%
    select(c("codecommune", names(csp_data)[
      grepl("^pagri", names(csp_data)) | 
        grepl("^pindp", names(csp_data)) |                          
        grepl("^ppint", names(csp_data)) |                          
        grepl("^pempl", names(csp_data)) |                          
        grepl("^pouvr", names(csp_data))])) %>%
    mutate(codecommune = sub("^0+", "", as.character(codecommune)))  %>%
    pivot_longer(
      cols = c(starts_with("pagri"), 
               starts_with("pindp"), 
               starts_with("ppint"), 
               starts_with("pempl"), 
               starts_with("pouvr")), 
      names_to = c(".value", "year"), 
      names_pattern = "(p[^0-9]+)([0-9]+)", 
      names_transform = list(year = as.integer)
    ) %>%
    filter(!is.na(pagri) | !is.na(pindp) | !is.na(pempl) | !is.na(pouvr)) %>%
    filter(!is.na(codecommune)) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(pagri = as.numeric(pagri)) %>%
    mutate(pindp = as.numeric(pindp)) %>%
    mutate(pempl = as.numeric(pempl)) %>%
    mutate(pouvr = as.numeric(pouvr)) %>%
    mutate(ppint = as.numeric(ppint)) 
  
  
  df_merged <- left_join(df_merged, csp_data, by = c("codecommune", "year"))
  
  # --------------------------------------------------------------------------
  # 8. PROCESS GEOGRAPHIC AND INFRASTRUCTURE DATA
  # --------------------------------------------------------------------------
  cat("Processing geographic data...\n")
  
  # Altitude and surface area
  geographic_data <- load_and_process_data(
    file.path(raw_data_path, "altitudeAndMore.xlsx")
  ) %>%
    select(codecommune, altitude, superficie) %>%
    mutate(
      altitude = log(altitude + 1),
      superficie = log(superficie)
    )
  
  df_merged <- left_join(df_merged, geographic_data, by = "codecommune")
  
  # Distance to agglomeration
  distance_data <- load_and_process_data(
    file.path(processed_data_path, "distAgglo.xlsx")
  ) %>%
    select(codecommune, min_distance_to_agglo) %>%
    mutate(min_distance_to_agglo = log(min_distance_to_agglo + 1))
  
  df_merged <- left_join(df_merged, distance_data, by = "codecommune")
  
  # Vacant housing
  housing_data <- load_and_process_data(
    file.path(raw_data_path, "logVac.xlsx"),
    sheet = "Data"
  ) %>%
    select(-nom)
  
  housing_interpolated <- interpolate_variable(housing_data, "logVac")
  df_merged <- left_join(df_merged, housing_interpolated, by = c("codecommune", "year"))
  
  # --------------------------------------------------------------------------
  # 9. PROCESS ADDITIONAL DATASETS
  # --------------------------------------------------------------------------
  cat("Processing additional datasets...\n")
  
  # Revenue
  
  revenu_data <- load_and_process_data(
    file.path(raw_data_path, "revenuImposable.csv")
  ) %>%
    select(codecommune, starts_with("revenu"))  %>%
    mutate(across(starts_with("revenu"), as.numeric))
  
  revenu_interpolated <- interpolate_variable(revenu_data, "revenuImposable")
  
  df_merged <- left_join(df_merged, revenu_interpolated, by = c("codecommune", "year")) %>%
    mutate(revenuPerK = log(revenuImposable / pop)) %>%
    select(-revenuImposable)
  
  
  ## Add classification
  
  typology_data <- read_excel(file.path(raw_data_path, "typoRuralUrbain.xlsx")) %>% 
    mutate(codecommune = sub("^0+", "", as.character(codecommune)))
  
  
  df_merged <- left_join(df_merged, typology_data, by = c("codecommune"))
  
  
  
  # Process remaining datasets (hedges, vineyards, etc.)
  # removed
  
  # --------------------------------------------------------------------------
  # 10. FINALIZE DATASET
  # --------------------------------------------------------------------------
  cat("Finalizing dataset...\n")
  
  # Add final variables and clean
  df_merged <- df_merged %>%
    filter(!is.na(codecommune)) %>%
    # Add population density
    mutate(popDensity = pop / exp(superficie))  # superficie is log-transformed
  
  # Save processed data
  save(df_merged, dfZRR_raw, file = file.path(processed_data_path, "main.RData"))
  
  cat("Data processing completed successfully!\n")
  cat("Final dataset contains", nrow(df_merged), "observations and", ncol(df_merged), "variables.\n")
  
  
  cat("✓ Saved main.RData\n")
  
  cat("\n===============================================\n")
  cat("MAIN DATA PROCESSING COMPLETED SUCCESSFULLY\n")
  cat("===============================================\n")
  
  
  
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Run the processing pipeline
process_commune_data(raw_data_path, processed_data_path)
