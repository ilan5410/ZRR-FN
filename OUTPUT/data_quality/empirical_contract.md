# Empirical Contract For Publication Draft

Generated: 2026-07-14

## Primary Estimand

The main estimand is the difference in the change in Front National (FN) presidential vote share between municipalities that entered ZRR in the initial wave and municipalities that entered only after 2004:

`Delta FN_i = FN2002_i - FN1988_i`

The target coefficient is the association between initial ZRR entry and this change, conditional on pre-treatment covariates and, in the preferred adjusted specification, department fixed effects.

## Primary Design

- Design label: early-versus-later treated first-difference design. It may be described as DiD only with the explicit caveat that the central presidential-election comparison has one baseline election and one post-treatment election.
- Treatment: commune belongs to the initial ZRR wave.
- Comparison group: communes that entered ZRR only after 2004.
- Outcome: change in FN vote share from 1988 to 2002.
- Main sample: communes in the processed sharp sample with observed FN1988, FN2002, treatment status, department, canton-cluster convention, and required controls.
- Controls: baseline or predetermined controls only in the primary specification. Post-treatment mediators belong in mechanisms or robustness tables, not the preferred causal specification.
- Fixed effects: department fixed effects in the most conservative adjusted specification.
- Standard errors: clustered at the canton-cluster convention produced by the audited data pipeline.

## Canton And Commune-Code Convention

- The 1999 canton bridge is no longer collapsed by selecting the first sorted canton.
- Communes split across multiple canton rows receive commune-specific split-cluster IDs.
- Communes with missing canton codes receive commune-specific missing-canton cluster IDs.
- Until external documentation is fully recovered, canton clustering should be described as an analysis convention, not as proof that every commune has a unique historical canton.
- Split/missing canton exclusion sensitivities are required for any claim that depends materially on canton clustering.

## Evidence Status

- Main first-difference DiD table: `OUTPUT/tables/DiD_main_results.tex`.
- DiD model manifest: `OUTPUT/data_quality/model_manifest.csv`.
- Merge/canton sensitivity: `OUTPUT/data_quality/commune_merge_sensitivity_summary.md`.
- Raw-data provenance: `OUTPUT/data_quality/raw_data_provenance.csv` and `OUTPUT/data_quality/raw_data_external_documentation.md`.

## Claim Discipline

- Do not claim that parallel trends are verified. The presidential-election series used in the main comparison has only one clean pre-treatment election, 1988.
- Do not present the 1995 election coefficient as definitive post-treatment evidence. The LOADT law instituted ZRR in February 1995, but the defining decree is dated February 14, 1996, and the draft itself describes official launch/implementation as 1996. The 1995 coefficient is therefore a timing and selection diagnostic, not the core causal estimate.
- Treat spatial RDD/border estimates as supporting evidence. They corroborate the sign of the effect but are fragile as a primary design because precision depends on department fixed effects and the very narrow-band evidence is weaker.
- If new official commune-history crosswalk work changes treatment or outcome harmonization, the empirical contract must be revised before manuscript claims are updated.
