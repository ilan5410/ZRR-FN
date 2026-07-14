# Commune Merge Sensitivity Summary

Generated: 2026-07-14 05:27:26 CEST

## Canton Bridge

- Canton bridge rows: 39150
- Split-canton communes: 342
- Missing-canton communes: 2471
- Nonstandard canton bridge communes: 2813

## Education Duplicate Resolution

- Duplicate education rows audited: 754
- Rows selected: 377
- Rule: prefer the row with greatest non-zero education coverage; if tied, prefer the commune name matching `ZRR.csv`; fail on remaining conflicting ties.

## Analysis Samples

# A tibble: 3 × 9
  dataset                   rows communes split_canton_rows missing_canton_rows
  <chr>                    <int>    <int>             <int>               <int>
1 script_sharp             36565    36565               314                   0
2 script_sharp_noEpicenter 36565    36565               314                   0
3 eco_outcomes             69126    34563               678                  56
# ℹ 4 more variables: nonstandard_canton_rows <int>,
#   split_canton_communes <int>, missing_canton_communes <int>,
#   nonstandard_canton_communes <int>

## RDD Sensitivity Check

# A tibble: 4 × 6
  dataset                  sample      observations estimate_z    se_z p_value_z
  <chr>                    <chr>              <int>      <dbl>   <dbl>     <dbl>
1 script_sharp             all_commun…        34789    -0.0196 0.00278  1.88e-12
2 script_sharp             exclude_sp…        34475    -0.0196 0.00279  2.53e-12
3 script_sharp_noEpicenter all_commun…        34789    -0.0227 0.00254  7.00e-19
4 script_sharp_noEpicenter exclude_sp…        34475    -0.0227 0.00256  1.07e-18

## CSV Outputs

- `OUTPUT/data_quality/commune_merge_sensitivity_samples.csv`
- `OUTPUT/data_quality/commune_merge_sensitivity_models.csv`
- `OUTPUT/data_quality/canton_bridge_issues.csv`
- `OUTPUT/data_quality/education_duplicate_resolution.csv`
