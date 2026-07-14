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
  data_quality_path <- file.path(main_path, "OUTPUT", "data_quality")
  dir.create(data_quality_path, recursive = TRUE, showWarnings = FALSE)
  merge_ledger <- new_merge_ledger()
  
  # --------------------------------------------------------------------------
  # 1. INITIALIZE BASE DATASET (ZRR Data)
  # --------------------------------------------------------------------------
  cat("Loading ZRR base data...\n")
  
  zrr_source <- read.csv(file.path(raw_data_path, "ZRR.csv")) %>%
    mutate(codecommune = standardize_commune_codes(codecommune))

  zrr_commune_names <- zrr_source %>%
    distinct(codecommune, zrr_nom = nom) %>%
    mutate(
      .audit_zrr_name_key = gsub(
        "[^a-z0-9]",
        "",
        tolower(iconv(zrr_nom, to = "ASCII//TRANSLIT"))
      )
    ) %>%
    group_by(codecommune) %>%
    dplyr::slice(1) %>%
    ungroup()

  dfZRR_raw <- zrr_source %>%
    select(-treatmentLong, -treatment, -nom)
  
  # Add canton codes from 1999 commune-canton data. Some communes span multiple
  # canton fragments or have missing canton codes; use commune-specific cluster
  # ids for those cases rather than assigning them to arbitrary pseudo-cantons.
  canton_data_raw <- st_read(file.path(raw_data_path, "france1999.dbf")) %>%
    mutate(
      codecommune = standardize_commune_codes(paste0(DEP, COM)),
      canton_fragment = trimws(as.character(CT)),
      canton_fragment = dplyr::na_if(canton_fragment, ""),
      canton = dplyr::if_else(
        is.na(canton_fragment) | canton_fragment == "NA",
        NA_character_,
        paste0(DEP, canton_fragment)
      )
    ) %>%
    select(codecommune, canton) %>%
    mutate(canton = as.character(canton)) %>%
    st_drop_geometry() %>%
    drop_identical_rows("france1999 canton bridge")

  canton_bridge <- canton_data_raw %>%
    group_by(codecommune) %>%
    summarise(
      n_cantons = n_distinct(canton, na.rm = TRUE),
      has_missing_canton = any(is.na(canton)),
      canton_list = collapse_non_missing_sorted(canton),
      canton_primary_audit = first_non_missing_sorted(canton),
      .groups = "drop"
    ) %>%
    mutate(
      is_multi_canton = n_cantons > 1,
      canton = case_when(
        has_missing_canton ~ paste0("missing_canton_", codecommune),
        is_multi_canton ~ paste0("split_", codecommune),
        TRUE ~ canton_primary_audit
      )
    )

  multi_canton_communes <- canton_bridge %>%
    filter(is_multi_canton)

  if (nrow(multi_canton_communes) > 0) {
    write_csv(
      multi_canton_communes,
      file.path(processed_data_path, "multi_canton_communes.csv")
    )
    cat(
      "Saved multi-canton commune audit:",
      nrow(multi_canton_communes),
      "communes\n"
    )
  }

  canton_bridge_issues <- canton_bridge %>%
    filter(is_multi_canton | has_missing_canton)
  if (nrow(canton_bridge_issues) > 0) {
    write_csv(
      canton_bridge_issues,
      file.path(data_quality_path, "canton_bridge_issues.csv")
    )
  }

  write_csv(canton_bridge, file.path(data_quality_path, "multi_canton_bridge.csv"))

  canton_data <- canton_bridge %>%
    select(codecommune, canton, canton_primary_audit, canton_list, n_cantons, is_multi_canton, has_missing_canton)

  assert_unique_key(canton_data, "codecommune", "canton_data")
  
  dfZRR_raw <- dfZRR_raw %>%
    audited_left_join(canton_data, by = "codecommune", relationship = "many-to-one", join_name = "ZRR + canton bridge", ledger = merge_ledger)
  
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
  assert_unique_key(electoral_pres, "codecommune", "pvoixFNpres")
  
  dfZRR_raw <- audited_left_join(dfZRR_raw, electoral_pres, by = "codecommune", relationship = "many-to-one", join_name = "ZRR + FN presidential vote", ledger = merge_ledger)
  
  # European elections (FN)
  electoral_eu <- load_and_process_data(
    file.path(raw_data_path, "EU.xlsx"),
    col_types = c(codecommune = "text")
  ) %>%
    select(codecommune, starts_with("FN"))
  assert_unique_key(electoral_eu, "codecommune", "EU")
  
  dfZRR_raw <- audited_left_join(dfZRR_raw, electoral_eu, by = "codecommune", relationship = "many-to-one", join_name = "ZRR + FN European vote", ledger = merge_ledger)
  
  # RPR Presidential elections
  electoral_rpr <- load_and_process_data(
    file.path(raw_data_path, "pvoixRPRpres.xlsx"),
    col_types = c(codecommune = "text")
  ) %>%
    select(codecommune, starts_with("RPR"))
  assert_unique_key(electoral_rpr, "codecommune", "pvoixRPRpres")
  
  dfZRR_raw <- audited_left_join(dfZRR_raw, electoral_rpr, by = "codecommune", relationship = "many-to-one", join_name = "ZRR + RPR presidential vote", ledger = merge_ledger)
  
  # Turnout data
  turnout_data <- load_and_process_data(
    file.path(raw_data_path, "df_turnout.csv")
  ) %>%
    select(codecommune, starts_with("turnout_2002")) %>%
    filter(!is.na(turnout_2002)) %>%
    drop_identical_rows("df_turnout")
  assert_unique_key(turnout_data, "codecommune", "df_turnout")
  
  dfZRR_raw <- audited_left_join(dfZRR_raw, turnout_data, by = "codecommune", relationship = "many-to-one", join_name = "ZRR + turnout", ledger = merge_ledger)
  assert_unique_key(dfZRR_raw, c("codecommune", "year"), "dfZRR_raw")
  
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
  assert_unique_key(df_merged, c("codecommune", "year"), "df_merged_unemployment")
  
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
  assert_unique_key(previous_elections, "codecommune", "previous_elections")
  
  df_merged <- audited_left_join(df_merged, previous_elections, by = "codecommune", relationship = "many-to-one", join_name = "controls + previous elections", ledger = merge_ledger)
  
  # --------------------------------------------------------------------------
  # 4. PROCESS POPULATION DATA
  # --------------------------------------------------------------------------
  cat("Processing population data...\n")
  
  population_data <- load_and_process_data(
    file.path(raw_data_path, "popcommunes.csv")
  )
  
  # Select population columns and calculate growth
  pop_cols <- names(population_data)[grepl("^pop(197[5-9]|19[8-9][0-9]|200[0-9]|201[0-9]|2020)$", names(population_data))]
  
  dfPop <- population_data %>%
    select(codecommune, reg, all_of(pop_cols)) %>%
    mutate(
      codecommune = standardize_commune_codes(codecommune),
      delta_pop_1980_1995 = (pop1995 - pop1980) / pop1980
    )
  assert_unique_key(dfPop, "codecommune", "popcommunes")
  
  # Interpolate population data
  dfPopLong <- interpolate_variable(dfPop, "pop")
  assert_unique_key(dfPopLong, c("codecommune", "year"), "dfPopLong")
  df_merged <- audited_left_join(df_merged, dfPopLong, by = c("codecommune", "year"), relationship = "one-to-one", join_name = "controls + population long", ledger = merge_ledger)

  population_static <- dfPop %>%
    select(codecommune, reg, delta_pop_1980_1995)
  assert_unique_key(population_static, "codecommune", "population_static")
  df_merged <- audited_left_join(df_merged, population_static, by = "codecommune", relationship = "many-to-one", join_name = "controls + population static", ledger = merge_ledger)
  
  
  
  
  
  # Population shares data

  col_names <- read_csv(file = file.path(raw_data_path, "agesexcommunes.csv"), show_col_types = FALSE) %>% names()
  pop_share_data <- read_csv(file = file.path(raw_data_path, "agesexcommunes.csv"), 
                 col_types = ifelse(grepl("^poph1539|^popf1539", col_names), "c", "_"),
                 show_col_types = FALSE)

  pop_share_data_interpolated <- pop_share_data %>%
    select(codecommune, matches("^poph1539|^popf1539")) %>%
    mutate(codecommune = standardize_commune_codes(codecommune)) %>%
    pivot_longer(
      cols = matches("^poph1539|^popf1539"),
      names_to = c("share_variable", "year"),
      names_pattern = "(poph1539|popf1539)([0-9]+)",
      values_to = "age_sex_population"
    ) %>%
    mutate(
      year = as.numeric(year),
      share_variable = dplyr::recode(share_variable, poph1539 = "poph", popf1539 = "popf"),
      age_sex_population = as.numeric(age_sex_population)
    ) %>%
    filter(year >= 1965, year <= 2022) %>%
    audited_left_join(dfPopLong, by = c("codecommune", "year"), relationship = "many-to-one", join_name = "age-sex population + population long", ledger = merge_ledger) %>%
    mutate(value = age_sex_population / pop) %>%
    select(codecommune, year, share_variable, value) %>%
    pivot_wider(names_from = share_variable, values_from = value)

  pop_share_data_interpolated <- pop_share_data_interpolated %>%
    select(codecommune, year, poph, popf)
  assert_unique_key(pop_share_data_interpolated, c("codecommune", "year"), "pop_share_data_interpolated")

  df_merged <- audited_left_join(
    df_merged,
    pop_share_data_interpolated,
    by = c("codecommune", "year"),
    relationship = "one-to-one",
    join_name = "controls + age-sex shares",
    ledger = merge_ledger
  )
  

  # --------------------------------------------------------------------------
  # 5. PROCESS EMPLOYMENT DATA
  # --------------------------------------------------------------------------
  cat("Processing employment data...\n")
  
  employment_data <- load_and_process_data(
    file.path(raw_data_path, "txEmploi.csv")
  ) %>%
    audited_left_join(
      dfPop %>% select(codecommune, pop1975, pop1982, pop1990, pop1999, pop2009, pop2014, pop2020),
      by = "codecommune",
      relationship = "many-to-one",
      join_name = "employment + population denominators",
      ledger = merge_ledger
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
  
  assert_unique_key(employment_interpolated, c("codecommune", "year"), "employment_interpolated")
  df_merged <- audited_left_join(df_merged, employment_interpolated, by = c("codecommune", "year"), relationship = "one-to-one", join_name = "controls + employment", ledger = merge_ledger)
  
  # --------------------------------------------------------------------------
  # 6. PROCESS DEMOGRAPHIC DATA
  # --------------------------------------------------------------------------
  cat("Processing demographic data...\n")
  
  # Foreign population percentage
  foreigners_data <- load_and_process_data(
    file.path(raw_data_path, "etrangers.csv")
  ) %>%
    audited_left_join(
      dfPop %>% select(codecommune, matches("pop(197[5-9]|19[8-9][0-9]|200[0-9]|201[0-9]|2020)")),
      by = "codecommune",
      relationship = "many-to-one",
      join_name = "foreigners + population denominators",
      ledger = merge_ledger
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
  
  assert_unique_key(foreigners_interpolated, c("codecommune", "year"), "foreigners_interpolated")
  df_merged <- audited_left_join(df_merged, foreigners_interpolated, by = c("codecommune", "year"), relationship = "one-to-one", join_name = "controls + foreigners", ledger = merge_ledger)
  
  # Age structure data
  age_data <- load_and_process_data(
    file.path(raw_data_path, "age_demog.xlsx")
  ) %>%
    select(codecommune, popYoungOld1995, popYoungOld2002)
  
  age_interpolated <- interpolate_variable(age_data, "popYoungOld")
  assert_unique_key(age_interpolated, c("codecommune", "year"), "age_interpolated")
  df_merged <- audited_left_join(df_merged, age_interpolated, by = c("codecommune", "year"), relationship = "one-to-one", join_name = "controls + age structure", ledger = merge_ledger)
  
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
    audited_left_join(dfPop, by = "codecommune", relationship = "many-to-one", join_name = "associations + population denominators", ledger = merge_ledger) %>%
    filter(!is.na(reg))
  
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
  assert_unique_key(associations_interpolated, c("codecommune", "year"), "associations_interpolated")
  df_merged <- audited_left_join(df_merged, associations_interpolated, by = c("codecommune", "year"), relationship = "one-to-one", join_name = "controls + associations", ledger = merge_ledger)
  
  # Education data
  education_data <- load_and_process_data(
    file.path(raw_data_path, "educProcessed.xlsx")
  )
  
  educ_cols <- names(education_data)[grepl("^educ(NoDiploma|SUP|BAC|CAPBEP)PerK", names(education_data))]
  education_data <- education_data %>%
    mutate(
      .audit_education_name_key = gsub(
        "[^a-z0-9]",
        "",
        tolower(iconv(nom, to = "ASCII//TRANSLIT"))
      )
    ) %>%
    left_join(zrr_commune_names, by = "codecommune") %>%
    mutate(.audit_name_matches_zrr = as.integer(.audit_education_name_key == .audit_zrr_name_key)) %>%
    resolve_duplicate_rows_by_nonzero_coverage(
      keys = "codecommune",
      value_cols = educ_cols,
      dataset_name = "educProcessed",
      audit_path = file.path(data_quality_path, "education_duplicate_resolution.csv"),
      preference_col = ".audit_name_matches_zrr"
    ) %>%
    select(codecommune, all_of(educ_cols)) %>%
    drop_identical_rows("educProcessed")
  
  education_interpolated <- interpolate_variable(
    education_data,
    c("educNoDiplomaPerK", "educSUPPerK", "educBACPerK", "educCAPBEPPerK")
  ) %>%
    # remove columns with suffixes .x and .y
    select(c("codecommune", "year", "educNoDiplomaPerK", "educSUPPerK", "educBACPerK", "educCAPBEPPerK"))
  
  
  assert_unique_key(education_interpolated, c("codecommune", "year"), "education_interpolated")
  df_merged <- audited_left_join(df_merged, education_interpolated, by = c("codecommune", "year"), relationship = "one-to-one", join_name = "controls + education", ledger = merge_ledger)
  
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
  
  
  csp_data <- drop_identical_rows(csp_data, "csp_data")
  assert_unique_key(csp_data, c("codecommune", "year"), "csp_data")
  df_merged <- audited_left_join(df_merged, csp_data, by = c("codecommune", "year"), relationship = "one-to-one", join_name = "controls + CSP", ledger = merge_ledger)
  
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
  
  assert_unique_key(geographic_data, "codecommune", "altitudeAndMore")
  df_merged <- audited_left_join(df_merged, geographic_data, by = "codecommune", relationship = "many-to-one", join_name = "controls + altitude surface", ledger = merge_ledger)
  
  # Distance to agglomeration
  distance_data <- load_and_process_data(
    file.path(processed_data_path, "distAgglo.xlsx")
  ) %>%
    select(codecommune, min_distance_to_agglo) %>%
    mutate(min_distance_to_agglo = log(min_distance_to_agglo + 1))
  
  assert_unique_key(distance_data, "codecommune", "distAgglo")
  df_merged <- audited_left_join(df_merged, distance_data, by = "codecommune", relationship = "many-to-one", join_name = "controls + agglomeration distance", ledger = merge_ledger)
  
  # Vacant housing
  housing_data <- load_and_process_data(
    file.path(raw_data_path, "logVac.xlsx"),
    sheet = "Data"
  ) %>%
    select(-nom)
  
  housing_interpolated <- interpolate_variable(housing_data, "logVac")
  assert_unique_key(housing_interpolated, c("codecommune", "year"), "housing_interpolated")
  df_merged <- audited_left_join(df_merged, housing_interpolated, by = c("codecommune", "year"), relationship = "one-to-one", join_name = "controls + housing vacancy", ledger = merge_ledger)
  
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
  assert_unique_key(revenu_interpolated, c("codecommune", "year"), "revenu_interpolated")
  
  df_merged <- audited_left_join(df_merged, revenu_interpolated, by = c("codecommune", "year"), relationship = "one-to-one", join_name = "controls + taxable income", ledger = merge_ledger) %>%
    mutate(revenuPerK = log(revenuImposable / pop)) %>%
    select(-revenuImposable)
  
  
  ## Add classification
  
  typology_data <- read_excel(file.path(raw_data_path, "typoRuralUrbain.xlsx")) %>% 
    mutate(codecommune = standardize_commune_codes(codecommune)) %>%
    drop_identical_rows("typoRuralUrbain")
  assert_unique_key(typology_data, "codecommune", "typoRuralUrbain")
  
  
  df_merged <- audited_left_join(df_merged, typology_data, by = c("codecommune"), relationship = "many-to-one", join_name = "controls + rural urban typology", ledger = merge_ledger)
  
  
  
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
    mutate(popDensity = pop / exp(superficie)) %>%  # superficie is log-transformed
    drop_identical_rows("df_merged")

  assert_unique_key(dfZRR_raw, c("codecommune", "year"), "dfZRR_raw")
  assert_unique_key(df_merged, c("codecommune", "year"), "df_merged")
  
  # Save processed data
  save(df_merged, dfZRR_raw, file = file.path(processed_data_path, "main.RData"))
  write_merge_ledger(merge_ledger, data_quality_path)
  
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
