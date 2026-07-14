# Commune-Code And Canton-Bridge Stability Review

Generated: 2026-07-14 05:27:25 CEST

## What Is Verified In This Repository

- `DATA/raw_data_files_used.xlsx` lists raw file names, purposes, and consuming scripts, but it does not provide download URLs or source vintages for most files.
- The merge audit normalizes commune codes consistently before joins and records missing/unmatched keys.
- `france1999.dbf` contains multiple canton rows for 342 commune codes in this checkout. The pipeline therefore no longer assigns those communes to the first sorted canton; it uses commune-specific split cluster IDs and saves the canton list for audit.
- `france1999.dbf` has missing canton codes for 2471 commune codes. The pipeline no longer collapses these into artificial department-level `NA` clusters; it uses commune-specific `missing_canton_<codecommune>` cluster IDs.
- `educProcessed.xlsx` has duplicate rows for 377 commune codes (754 raw duplicate rows audited). The pipeline now chooses the row with the greatest non-zero education coverage, then breaks ties by matching the `ZRR.csv` commune name.

## What Still Needs External Documentation

- Locate the original download page or archival source for `france1999.dbf` and identify the official geography/year it represents.
- Confirm whether multiple rows per commune in `france1999.dbf` are official canton fractions, pseudo-cantons such as `Canton non precise`, or GIS artifacts.
- Check whether commune codes changed between the election/control years and the source geography vintages. This is especially important for historical controls, 1999 canton geography, and 2022 shapefile comparisons.
- Identify whether raw electoral and socio-economic files already harmonize communes to a common vintage or require an explicit commune-history crosswalk.

## Current Decision

- Canton bridge source status: `missing_source_doc_for_canton_bridge`. Until external documentation is found, split communes are handled conservatively through split-cluster IDs and sensitivity checks rather than arbitrary canton assignment.
- Missing-canton communes are treated as commune-specific clusters until the `france1999.dbf` documentation explains whether missing `CT` represents a non-precise canton, an official category, or a source artifact.
- Education duplicates are resolved by coverage and ZRR-name matching, but the duplicated names/codes still indicate that a commune-history crosswalk should be added if the raw source vintage can be recovered.
- Manuscript wording should describe the canton cluster as an analysis clustering convention, not as proof of a unique historical canton for split communes.

## Outputs To Check

- `OUTPUT/data_quality/raw_data_provenance.csv`
- `OUTPUT/data_quality/multi_canton_bridge.csv`
- `OUTPUT/data_quality/canton_bridge_issues.csv`
- `OUTPUT/data_quality/education_duplicate_resolution.csv`
- `OUTPUT/data_quality/commune_merge_sensitivity_summary.md`
- `OUTPUT/data_quality/raw_source_profile.csv`
