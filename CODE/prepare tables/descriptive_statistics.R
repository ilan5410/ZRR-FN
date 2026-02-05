# ================================================================================
# DESCRIPTIVE STATISTICS (1990): BUILD LATeX TABLE
# ================================================================================

build_descriptive_1990 <- function(processed_data_path,
                                   path_tables,
                                   y = 1990,
                                   revenue_year = 1994,
                                   youngold_year = 1995) {
  cat("===============================================\n")
  cat("DESCRIPTIVE STATISTICS TABLE\n")
  cat("===============================================\n")
  cat("Target year:", y, "\n")
  cat("Aux years  :", "revenuPerK ->", revenue_year, "| popYoungOld ->", youngold_year, "\n\n")
  
  # --------------------------------------------------------------------------
  # LOAD DATA + BASIC CHECKS
  # --------------------------------------------------------------------------
  data_env_path <- file.path(processed_data_path, "dataDes.RData")
  if (!file.exists(data_env_path)) stop("❌ dataDes.RData not found at: ", data_env_path)
  if (missing(path_tables) || is.null(path_tables)) stop("❌ 'path_tables' must be provided")
  
  load(data_env_path) # expects dfZRRControls, dfZRR, labels (if available)
  if (!exists("dfZRRControls")) stop("❌ dfZRRControls not found in dataDes.RData")
  if (!exists("dfZRR")) warning("⚠️ dfZRR not found in dataDes.RData (will skip join back if needed)")
  if (!exists("labels")) { labels <- setNames(names(dfZRRControls), names(dfZRRControls)) }
  
  cat("✓ Loaded dfZRRControls with", nrow(dfZRRControls), "rows\n\n")
  
  # --------------------------------------------------------------------------
  # DEFINE VARIABLE SETS
  # --------------------------------------------------------------------------
  cat("2) Defining variable sets...\n")
  
  # variables to exclude from summary
  drop_vars <- c("codecommune","nomcommune","nom","year_treat","canton","dep","year","reg",
                 "typologie","vigne","haie","revenuPerK","popYoungOld","turnout_2002",
                 "deltaFN","FN","RPR", "treatment", "treatment_in_1995")
  
  # variables kept for summary (to be completed with revenuPerK & popYoungOld later)
  dataNames <- setdiff(names(dfZRRControls), drop_vars)
  
  # percentage-like variables to scale (only if present)
  percentage_vars <- c("pchom","ratEmp","ratForeigners","educNoDiplomaPerK","educSUPPerK",
                       "educBACPerK","educCAPBEPPerK","poph","popf","pagri","pindp",
                       "ppint","pempl","pouvr","logVac","FN1995","FN1988","popYoungOld")
  
  # display order (labels must exist for these keys)
  labels_ordered <- c(
    # Past elections
    "Vote share for FN in 1988",
    "FN vote share in 1995",
    
    # Employment
    "Unemployed (%)","In the labor force (%)","Agriculture (%)","Independant (%)",
    "Intermediate occupations (%)","Clerical (%)","Manual (%)",
    
    # Demographics
    "Population","Foreigners (%)","Ages 20-40 (%), men","Ages 20-40 (%), women",
    "Age ratio young/old (%)","Population density","Population change in p.p. 1980-1990",
    "Vacant housing (%)","OPI per 1,000 inhabitants","Taxable income per capita  (log)",
    
    # Education
    "No diploma (%)","Academic (%)","Highschool (%)","Technical (%)",
    
    # Geography
    "Altitude","Distance to closest agglomeration in meters (log)","Area in km2 (log)",
    "Observations"
  )
  
  # row section mapping for nice group headers
  section_map <- list(
    "Past elections" = c("Vote share for FN in 1988","FN vote share in 1995"),
    "Employment"     = c("Unemployed (%)","In the labor force (%)","Agriculture (%)",
                         "Independant (%)","Intermediate occupations (%)","Clerical (%)","Manual (%)"),
    "Demographics"   = c("Population","Foreigners (%)","Ages 20-40 (%), men","Ages 20-40 (%), women",
                         "Age ratio young/old (%)","Population density","Population change in p.p. 1980-1990",
                         "Vacant housing (%)","OPI per 1,000 inhabitants","Taxable income per capita  (log)"),
    "Education"      = c("No diploma (%)","Academic (%)","Highschool (%)","Technical (%)"),
    "Geography"      = c("Altitude","Distance to closest agglomeration in meters (log)","Area in km2 (log)")
  )
  
  cat("✓ Variables prepared\n\n")
  
  # --------------------------------------------------------------------------
  # BUILD BASE DATA FOR 1990 (+ AUX YEARS)
  # --------------------------------------------------------------------------
  cat("3) Building base dataset...\n")
  
  df_1990 <- dfZRRControls %>%
    dplyr::filter(year == y) %>%
    dplyr::select(any_of(c(dataNames, "codecommune")))
  
  # join year_treat from dfZRR (unique per commune)
  if (exists("dfZRR")) {
    df_1990 <- df_1990 %>%
      dplyr::left_join(
        dfZRR %>% dplyr::select(codecommune, year_treat) %>% dplyr::distinct(),
        by = "codecommune"
      )
  } else if (!"year_treat" %in% names(df_1990)) {
    stop("❌ year_treat not available")
  }
  
  # revenue at 1994, young/old at 1995
  dfRevenue <- dfZRRControls %>% dplyr::filter(year == revenue_year) %>%
    dplyr::select(codecommune, revenuPerK)
  dfYoungOld <- dfZRRControls %>% dplyr::filter(year == youngold_year) %>%
    dplyr::select(codecommune, popYoungOld)
  
  df_1990 <- df_1990 %>%
    dplyr::left_join(dfRevenue,  by = "codecommune") %>%
    dplyr::left_join(dfYoungOld, by = "codecommune")
  
  # add those to analysis set
  dataNames_plus <- union(dataNames, c("revenuPerK","popYoungOld"))
  
  # coerce numerics + drop Inf
  df_1990[dataNames_plus] <- lapply(df_1990[dataNames_plus], function(x) {
    x <- suppressWarnings(as.numeric(x))
    x[is.infinite(x)] <- NA_real_
    x
  })
  
  # treatment groups
  df_1990 <- df_1990 %>%
    dplyr::mutate(year_treat = dplyr::if_else(is.na(year_treat), 0, year_treat)) %>%
    dplyr::mutate(
      group = dplyr::case_when(
        year_treat == 1995 ~ "Treated in 1995",
        year_treat == 0    ~ "Never Treated",
        year_treat > 1995  ~ "Treated after 1995",
        TRUE               ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(group))
  
  # scale percentages by 100 if present
  df_1990 <- df_1990 %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(percentage_vars), ~ .x * 100))
  
  # order groups (columns)
  group_levels <- c("Never Treated","Treated after 1995","Treated in 1995")
  df_1990 <- df_1990 %>% dplyr::mutate(group = factor(group, levels = group_levels))
  
  cat("✓ Base data built. Rows:", nrow(df_1990), " | Groups:", paste(levels(df_1990$group), collapse = " | "), "\n\n")
  
  # --------------------------------------------------------------------------
  # SUMMARY STATS: MEAN & SD → SINGLE CELL PER GROUP
  # --------------------------------------------------------------------------
  cat("4) Computing summary statistics...\n")
  
  fmt_num <- function(v) sprintf("%.2f", v)
  
  summary_cells <- df_1990 %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(dataNames_plus),
                    list(mean = ~mean(.x, na.rm = TRUE),
                         sd   = ~sd(.x,   na.rm = TRUE)),
                    .names = "{.col}__{.fn}"),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = -group,
      names_to = c("variable","stat"),
      names_sep = "__",
      values_to = "val"
    ) %>%
    tidyr::pivot_wider(names_from = stat, values_from = val) %>%
    dplyr::mutate(cell = ifelse(
      is.finite(mean),
      paste0("\\makecell{", fmt_num(mean), " \\\\ (", fmt_num(sd), ")}"),
      ""
    )) %>%
    dplyr::select(group, variable, cell)
  
  # pivot so each group is a column with the composed cell
  summary_wide <- summary_cells %>%
    tidyr::pivot_wider(names_from = group, values_from = cell) %>%
    dplyr::mutate(label = labels[variable]) %>%
    dplyr::mutate(label = ifelse(is.na(label), variable, label)) %>%
    dplyr::relocate(label) %>%
    dplyr::rename(Variable = label) %>%
    dplyr::select(Variable, dplyr::all_of(group_levels))
  
  # counts row (plain integers)
  counts <- df_1990 %>%
    dplyr::count(group) %>%
    tidyr::pivot_wider(names_from = group, values_from = n) %>%
    dplyr::mutate(Variable = "Observations") %>%
    dplyr::select(Variable, dplyr::all_of(group_levels))  %>%
    mutate(across(all_of(group_levels), as.character))
  
  # combine
  summary_table <- dplyr::bind_rows(summary_wide, counts)
  
  cat("✓ Summary table assembled with", nrow(summary_table), "rows\n\n")
  
  # --------------------------------------------------------------------------
  # ORDER ROWS + SECTION GROUPING
  # --------------------------------------------------------------------------
  cat("5) Ordering rows and preparing row groups...\n")
  
  # order by labels_ordered
  summary_table <- summary_table %>%
    dplyr::arrange(factor(Variable, levels = labels_ordered))
  
  # map each row to a section
  section_of <- function(lbl) {
    for (sec in names(section_map)) if (lbl %in% section_map[[sec]]) return(sec)
    if (lbl == "Observations") return("Geography") # keep at the end under last section
    return(NA_character_)
  }
  summary_table$Section <- vapply(summary_table$Variable, section_of, character(1))
  summary_table <- summary_table %>% dplyr::relocate(Section, .before = Variable)
  
  # compute pack_rows indices
  row_index <- which(!is.na(summary_table$Section))
  # we’ll compute contiguous blocks per Section
  sections <- na.omit(unique(summary_table$Section))
  blocks <- lapply(sections, function(sec) which(summary_table$Section == sec))
  
  cat("✓ Row ordering ready\n\n")
  
  # --------------------------------------------------------------------------
  # RENDER KABLE (LaTeX): 3 VALUE COLUMNS, MEAN \\ (SD)
  # --------------------------------------------------------------------------
  cat("6) Rendering LaTeX table...\n")
  
  # drop helper col
  out_df <- summary_table %>% dplyr::select(-Section)
  
  # build kable (escape=FALSE to allow '\\' for line breaks)
  kb <- knitr::kable(
    out_df,
    format   = "latex",
    booktabs = TRUE,
    align    = c("l","c","c","c"),
    col.names = c("Variable", group_levels),
    caption  = "Descriptive Statistics in 1990",
    label    = "descriptive",
    escape   = FALSE
  ) %>%
    kableExtra::kable_styling(latex_options = "hold_position") %>%
    kableExtra::add_header_above(c(" " = 1, "Groups" = 3)) %>%
    kableExtra::footnote(
      general = "Cells report the mean (first line) and standard deviation (in parentheses, second line). Some variables are taken from nearby years: taxable income (1994) and young/old ratio (1995). For precise definitions and sources, see the Data Section.",
      threeparttable = TRUE
    )
  
  # add section row groups
  for (sec in sections) {
    idx <- which(summary_table$Section == sec)
    if (length(idx) > 0) {
      first <- min(idx)
      last  <- max(idx)
      kb <- kb %>% kableExtra::pack_rows(sec, first, last)
    }
  }
  
  # Reduce the size
  kb <- kb %>%
    kableExtra::column_spec(1, width = "7cm") 
  
  # Replace "%" by "\%" for LaTeX
  kb_tex <- gsub("%", "\\\\%", kb)
  
  # save
  out_path <- file.path(path_tables, "descriptive_statistics.tex")
  kableExtra::save_kable(kb_tex, out_path)
  cat("✓ Saved LaTeX table to:", out_path, "\n")
  
  cat("\n===============================================\n")
  cat("DONE - descriptive_statistics.R\n")
  cat("===============================================\n")
  
  invisible(out_df)
  
  
  
  
  # --------------------------------------------------------------------------
  # SPLIT TABLE INTO TWO PARTS
  # --------------------------------------------------------------------------
  summary_table_part1 <- summary_table %>%
    dplyr::filter(Section %in% c("Past elections","Employment","Demographics"))
  
  summary_table_part2 <- summary_table %>%
    dplyr::filter(Section %in% c("Education","Geography"))
  
  # --------------------------------------------------------------------------
  # RENDER PART 1
  # --------------------------------------------------------------------------
  out_df1 <- summary_table_part1 %>% dplyr::select(-Section)
  
  kb1 <- knitr::kable(
    out_df1,
    format   = "latex",
    booktabs = TRUE,
    align    = c("l","c","c","c"),
    col.names = c("Variable", group_levels),
    caption  = "Descriptive Statistics in 1990",   # caption only for part 1
    label    = "descriptive",
    escape   = FALSE
  ) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    kableExtra::add_header_above(c(" " = 1, "Groups" = 3))

  for (sec in unique(summary_table_part1$Section)) {
    idx <- which(summary_table_part1$Section == sec)
    kb1 <- kb1 %>% kableExtra::pack_rows(sec, min(idx), max(idx))
  }

  # Reduce the size
  kb1 <- kb1 %>%
    kableExtra::column_spec(1, width = "7cm")
  
  # Replace "%" by "\%" for LaTeX
  kb1 <- gsub("%", "\\\\%", kb1)
  
  # --------------------------------------------------------------------------
  # RENDER PART 2
  # --------------------------------------------------------------------------
  out_df2 <- summary_table_part2 %>% dplyr::select(-Section)
  

  kb2 <- knitr::kable(
    dplyr::mutate(out_df2, dplyr::across(where(is.numeric), ~ sprintf("%.2f", .x))),
    format   = "latex",
    booktabs = TRUE,
    align    = c("l","c","c","c"),
    col.names = c("Variable", group_levels),
    caption  = "Descriptive Statistics in 1990 (continued)",  # caption for continuation
    escape   = FALSE
  ) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    kableExtra::add_header_above(c(" " = 1, "Groups" = 3)) %>%
    kableExtra::footnote(
      general = "Cells report the mean (first line) and standard deviation (in parentheses, second line). Some variables are taken from nearby years: taxable income (1994) and young/old ratio (1995).",
      threeparttable = TRUE
    )

  for (sec in unique(summary_table_part2$Section)) {
    idx <- which(summary_table_part2$Section == sec)
    kb2 <- kb2 %>% kableExtra::pack_rows(sec, min(idx), max(idx))
  }

  # Reduce the size
  kb2 <- kb2 %>%
    kableExtra::column_spec(1, width = "7cm")
  
  # Replace "%" by "\%" for LaTeX
  kb2 <- gsub("%", "\\\\%", kb2)
  
  # --------------------------------------------------------------------------
  # COMBINE INTO A SINGLE LaTeX TABLE ENVIRONMENT
  # --------------------------------------------------------------------------

  # Note: kb1 and kb2 each contain complete \begin{table}...\end{table}
  # We need to combine them properly for a two-page table

  full_table_tex <- c(
    kb1,
    "\\clearpage",
    kb2
  )

  # --------------------------------------------------------------------------
  # ADD FOOTNOTE ABOUT TAXABLE INCOME SOURCE
  # --------------------------------------------------------------------------

  # Add the Julia Cagé & Piketty footnote as specified in PLAN.md
  cage_piketty_footnote <- paste0(
    "\\item[a] In the socio-economic database assembled by Julia Cag\\'e and Thomas Piketty (2023), ",
    "the average income per municipality is defined as the total income reported on tax declarations ",
    "(before any deductions or allowances) divided by the total number of inhabitants (including children). ",
    "Source: \\url{https://www.unehistoireduconflitpolitique.fr/glossaire.html}, the website associated with ",
    "the book by Julia Cag\\'e and Thomas Piketty (2023): \\textit{Une histoire du conflit politique. ",
    "\\'{E}lections et in\\'{e}galit\\'{e}s sociales en France, 1789--2022}, Paris, Le Seuil."
  )

  # Insert the footnote before the last \end{tablenotes}
  # Use fixed = TRUE to avoid regex escaping issues with LaTeX backslashes
  full_table_tex <- sub(
    "\\end{tablenotes}",
    paste0(cage_piketty_footnote, "\n\\end{tablenotes}"),
    full_table_tex,
    fixed = TRUE
  )

  # --------------------------------------------------------------------------
  # SAVE TO FILE
  # --------------------------------------------------------------------------
  out_path <- file.path(path_tables, "descriptive_statistics.tex")
  writeLines(full_table_tex, out_path)
  cat("✓ LaTeX table exported to:", out_path, "\n")
  
}

# ==============================================================================
# EXECUTION
# ==============================================================================

build_descriptive_1990(processed_data_path, path_tables)
