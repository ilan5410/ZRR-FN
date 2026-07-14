# Referee Risk Memo

Generated: 2026-07-14

## Highest-Risk Issues

1. Timing of treatment versus the 1995 election.
   - Risk: the dynamic figure shows a gap in 1995, but official ZRR definition/implementation appears to occur around 1996.
   - Response: do not use 1995 as causal post-treatment evidence. Treat it as a warning that early-treated municipalities may already have been on different trajectories. Make 1988-2002 the main comparison and add more pre-treatment elections if possible.

2. Parallel trends.
   - Risk: one clean pre-treatment presidential election cannot validate parallel trends.
   - Response: say the assumption is not directly testable with the current presidential panel. Support it through selection-direction arguments, controls, later-treated comparison logic, and robustness, but do not overclaim.

3. Early-versus-later treated control validity.
   - Risk: later entrants are not randomly delayed and may differ in latent trends.
   - Response: document the eligibility/timing logic, show baseline differences, and keep the "direction of bias" paragraph. Add official timing and crosswalk validation before submission.

4. Spatial RDD fragility.
   - Risk: prior methodology review found standard `rdrobust` null results, department-FE dependence, geographic confounding, and attenuation at 1 km.
   - Response: keep RDD as supporting evidence and avoid making it the headline design.

5. Commune/canton historical geography.
   - Risk: commune codes and canton mappings may change over time, and `france1999.dbf` source URL is not recovered locally.
   - Response: use the audited merge ledger, split/missing canton cluster convention, exclusion sensitivity, and external INSEE COG documentation. Still mark official crosswalk recovery as a prerequisite for journal submission.

## Medium-Risk Issues

- Post-treatment controls: ensure the preferred specification only uses pre-treatment or predetermined controls.
- Multiple testing/specification search: use one pre-declared empirical contract and put secondary robustness in appendices.
- Spatial correlation: canton clustering is reasonable but not definitive; department clustering and split-canton exclusion should remain visible.
- Mechanisms: socioeconomic nulls are useful, but mechanism claims should be modest.
- Generated artifacts: final PDF and replication package should be rebuilt from a clean worktree immediately before submission.

## Required Manuscript Responses

- Abstract and introduction must headline the first-difference design, not the RDD.
- DiD section must state that the dynamic figure is diagnostic and not a verified pre-trend test.
- Spatial section must acknowledge department-FE dependence and narrow-band attenuation.
- Data section must mention audited merge/canton handling and the remaining source-documentation gap.
- Appendix or data-quality note must preserve the raw provenance and commune-code/canton-bridge caveats.
