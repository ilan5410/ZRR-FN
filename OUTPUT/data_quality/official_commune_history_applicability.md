# Official INSEE Commune-History Applicability Audit

Generated: 2026-07-14 14:47:49 CEST

## Official Sources Checked

- INSEE COG download index: https://www.insee.fr/fr/information/2560452
- INSEE COG 1999 download page: https://www.insee.fr/fr/information/2560686
- 1999 all-communes DBF archive: https://www.insee.fr/fr/statistiques/fichier/2560686/france1999-dbf.zip
- Communes since 1943 CSV: https://www.insee.fr/fr/statistiques/fichier/8740222/v_commune_depuis_1943.csv
- Commune movements since 1943 CSV: https://www.insee.fr/fr/statistiques/fichier/8740222/v_mvt_commune_2026.csv
- Current 2026 communes CSV: https://www.insee.fr/fr/statistiques/fichier/8740222/v_commune_2026.csv

## Bottom Line

- The checked-in `DATA/raw data/france1999.dbf` matches INSEE's official `france1999-dbf.zip`: **Yes**.
- The 1999 canton bridge therefore has an official source. The INSEE 1999 page explicitly states that multi-canton communes are represented by one row per canton fraction plus a `canton non precise` row; the current split-canton cluster convention is appropriate and should be documented as INSEE-driven, not as a source anomaly.
- The current 2026 `Communes depuis 1943` and `Évènements sur les communes` files apply to this project as validation/crosswalk inputs, but they do not by themselves tell us whether each raw election/control file is already harmonized to a common vintage. They should be used to build an explicit harmonization check before strengthening publication claims.
- Across audited project sources, 25176 source-code appearances are known historical commune codes that are not active current 2026 commune codes and therefore may need an explicit movement-based crosswalk if that source is merged to a modern-vintage file.
- Across audited project sources, 46 source-code appearances are not found in the official 2026 commune-history file; these are mostly formatting, overseas coverage, or source-specific coding cases that require manual review.

## Source-Level Summary

# A tibble: 21 × 8
   source    relative_path unique_codes codes_known_to_offic…¹ codes_active_2026
   <chr>     <chr>                <int>                  <int>             <int>
 1 1999 can… DATA/raw dat…        39150                  39135             34858
 2 CSP       DATA/raw dat…        37985                  37979             34745
 3 age-sex … DATA/raw dat…        37985                  37979             34745
 4 foreigne… DATA/raw dat…        37261                  37261             34693
 5 turnout   DATA/raw dat…        36668                  36651             34746
 6 taxable … DATA/raw dat…        36605                  36605             34707
 7 ZRR       DATA/raw dat…        36724                  36724             34875
 8 RPR pres… DATA/raw dat…        36219                  36218             34357
 9 altitude… DATA/raw dat…        36742                  36742             34869
10 FN Europ… DATA/raw dat…        35758                  35758             33956
11 rural-ur… DATA/raw dat…        34968                  34968             34864
12 education DATA/raw dat…        34855                  34855             34733
13 populati… DATA/raw dat…        34856                  34856             34734
14 FN presi… DATA/raw dat…        34506                  34505             34368
15 2022 com… DATA/raw dat…        34955                  34955             34863
16 distance… DATA/process…        34798                  34798             34706
17 distance… DATA/process…        34798                  34798             34706
18 distance… DATA/process…        34826                  34826             34734
19 employme… DATA/raw dat…        34928                  34928             34847
20 housing … DATA/raw dat…        34945                  34945             34864
21 border p… DATA/process…         7331                   7331              7306
# ℹ abbreviated name: ¹​codes_known_to_official_history
# ℹ 3 more variables: codes_needing_crosswalk <int>,
#   codes_unknown_to_official_history <int>,
#   codes_inactive_at_observed_year <int>

## Main Flags

# A tibble: 31 × 3
   source                    flag                        codes
   <chr>                     <chr>                       <int>
 1 1999 canton bridge        historical_not_active_2026   4232
 2 CSP                       historical_not_active_2026   3214
 3 age-sex population        historical_not_active_2026   3214
 4 foreigners                historical_not_active_2026   2548
 5 turnout                   historical_not_active_2026   1860
 6 taxable income            historical_not_active_2026   1853
 7 RPR presidential vote     historical_not_active_2026   1841
 8 altitude/surface          historical_not_active_2026   1828
 9 FN European vote          historical_not_active_2026   1782
10 ZRR                       historical_not_active_2026   1754
11 ZRR                       inactive_at_observed_year     108
12 rural-urban typology      historical_not_active_2026    104
13 education                 historical_not_active_2026    102
14 population                historical_not_active_2026    102
15 2022 commune shapefile    historical_not_active_2026     92
16 FN presidential vote      historical_not_active_2026     92
17 distance no epicenter     historical_not_active_2026     92
18 distance to ZRR border    historical_not_active_2026     92
19 distance to agglomeration historical_not_active_2026     92
20 employment                historical_not_active_2026     81
21 housing vacancy           historical_not_active_2026     81
22 border pairs              historical_not_active_2026     25
23 turnout                   unknown_to_official_history    17
24 1999 canton bridge        unknown_to_official_history    15
25 distance no epicenter     inactive_at_observed_year      12
26 distance to ZRR border    inactive_at_observed_year      12
27 CSP                       unknown_to_official_history     6
28 age-sex population        unknown_to_official_history     6
29 border pairs              inactive_at_observed_year       2
30 FN presidential vote      unknown_to_official_history     1
31 RPR presidential vote     unknown_to_official_history     1

## How This Applies To The Paper

- Replace the previous wording that the `france1999.dbf` provenance is unrecovered: it is now verified as INSEE COG 1999 `france1999.dbf`.
- Keep the manuscript's caution that canton clustering is an analysis convention for split/missing canton cases, but now cite the official INSEE 1999 row convention.
- Add a next-step requirement: build an actual movement-based harmonization layer from `v_mvt_commune_2026.csv` before final submission, then compare estimates with and without harmonizing historical commune codes.

## Analysis-Sample Exposure

The raw-source flag counts above are intentionally conservative because old geography files naturally include communes that no longer exist in 2026. In the processed analysis objects, the exposure is smaller:

```text
# A tibble: 4 × 8
  dataset       path       rows unique_codes flagged_codes needs_crosswalk_codes
  <chr>         <chr>     <int>        <int>         <int>                 <int>
1 df_merged     DATA/pr… 1.59e6        34563            94                    81
2 dfZRR_raw     DATA/pr… 4.01e5        36724          1862                  1849
3 dfDistance    DATA/pr… 3.48e4        34798           105                    92
4 dfZRRControls DATA/pr… 3.66e4        36565          1860                  1847
# ℹ 2 more variables: unknown_to_official_history_codes <int>,
#   inactive_at_observed_year_codes <int>
```

The primary merged panel has a much smaller flagged-code exposure than the broader treatment/control universe. This warrants an explicit crosswalk sensitivity, but it does not invalidate the current merge audit by itself.

## Machine-Readable Outputs

- `OUTPUT/data_quality/official_commune_history_source_check.csv`
- `OUTPUT/data_quality/commune_history_code_audit.csv`
- `OUTPUT/data_quality/commune_history_code_flags.csv`
- `OUTPUT/data_quality/commune_history_analysis_sample_audit.csv`
