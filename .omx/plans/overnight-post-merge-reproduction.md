# Overnight Post-Merge Reproduction Plan

**Status:** Implemented through merge/provenance QA and full reproduction build  
**Prepared:** 2026-07-13  
**Starting point:** `codex/commune-merge-audit` at `d8a42d6`

## Requirements Summary

The overnight run will turn the commune-merge repair into a complete, auditable
reproduction of the empirical results and paper. It will:

1. preserve the current dirty manuscript work without overwriting or committing it accidentally;
2. strengthen the raw-data audit beyond row counts by detecting conflicting duplicate values and measuring merge coverage;
3. document raw-data provenance and verify whether commune-canton mappings and commune codes are historically stable enough for the analysis;
4. resolve the substantively important multi-canton mapping problem;
5. rebuild Python geographic inputs, R processed data, tables, and figures;
6. compare corrected results with the pre-fix baseline at the sample and coefficient level;
7. formalize and validate the DiD specification before retaining causal claims;
8. update only claims supported by regenerated evidence;
9. compile the paper, commit with Lore messages, and push a dedicated branch.

No new dependency will be added. R 4.4.0, `latexmk` 4.85, and Biber 2.21 are
already installed. The slow randomization generator remains out of the standard
Python master by design (`CODE/Python code/master.py:17-18`).

## Current Evidence And Decisions

- The corrected commune-year spine is unique at the end of `main.R`
  (`CODE/prepare data/main.R:466-470`), and the processed-data checks cover the
  main analytical RData files (`CODE/prepare data/check_commune_merges.R:35-67`).
- The raw audit currently reports key multiplicity, but it does not distinguish
  exact duplicates from conflicting values (`CODE/prepare data/audit_commune_keys.R:95-149`).
- The current canton repair prevents row multiplication by selecting the first
  sorted canton (`CODE/prepare data/main.R:61-68`). That is deterministic but not
  substantively justified. There are 342 multi-canton communes in the audit;
  314 enter `script_sharp.RData`, 150 are within 20 km, and 9 of those are treated.
- The untracked `CODE/DiD_main.R` is a useful draft, not yet a publication-ready
  source of truth. Its event-study comment says commune and year fixed effects,
  while the model uses department effects (`CODE/DiD_main.R:93-111`).
- The manuscript currently says the event study confirms parallel trends
  (`Latex/ZRR and populist vote/DID.tex:44`), but the available presidential
  outcome series appears to provide only one pre-treatment election (1988).
  That claim cannot stand unless additional pre-treatment outcomes are found.
- The manuscript and its figures are already dirty. All overnight work will be
  isolated, and existing edits will be imported selectively only after validation.

## Acceptance Criteria

1. **Raw-key audit:** every source used by `prepare_data` has a declared grain,
   normalized key, missing-key count, exact-duplicate count, conflicting-duplicate
   count, and machine-readable conflict examples.
2. **Merge audit:** every commune join records input rows, unique keys, matched and
   unmatched keys, match rate, output rows, and whether cardinality matched the
   declared relationship. No unexplained row multiplication remains.
3. **Duplicate handling:** no production transformation silently applies `first()`
   to conflicting non-key values. Exact duplicates may be removed; conflicting
   records require a documented aggregation, source rule, exclusion, or failure.
4. **Raw-data provenance:** the report identifies where key raw inputs were
   downloaded or documents that provenance is missing. In particular, it checks
   the commune-canton source documentation, whether the 1999 canton-commune bridge
   is valid for the paper's election/control years, and whether commune codes
   changed over time in a way that can affect joins.
5. **Canton handling:** the primary analysis does not use arbitrary first-canton
   assignment. It reports sensitivity for at least: multi-canton exclusion,
   commune-specific cluster IDs for split communes, and department clustering.
6. **Processed keys:** all assertions in `check_commune_merges.R` pass, including
   one row per commune-year or commune-pair at each declared analytical grain.
7. **A/B evidence:** a comparison report gives pre-fix versus corrected sample N,
   treated/control N, key coefficients, standard errors, confidence intervals,
   p-values, and bandwidth-specific changes for every main result used in the paper.
8. **DiD contract:** the main DiD uses the same documented early-versus-late entrant
   sample across table and figures, a 1988-to-2002 outcome change, only defensible
   pre-treatment controls, department fixed effects where specified, and explicit
   clustering/sensitivity choices.
9. **Claim discipline:** no text claims a verified pre-trend without at least two
   pre-treatment periods. The legal and electoral timing around 1995 is verified
   before describing the 1995 coefficient as post-treatment.
10. **Reproduction:** Python master completes 4/4 scripts; the R data, table, and
   figure pipelines complete; all generated outputs are inventoried with hashes.
11. **Paper QA:** `latexmk`/Biber produce the PDF with zero LaTeX errors, zero
    undefined references, and zero undefined citations. Remaining warnings are listed.
12. **Git hygiene:** pre-existing dirty files are preserved byte-for-byte in the
    original worktree. Only reviewed overnight changes are committed and pushed on
    a new `codex/` branch using Lore commit messages.

## Implementation Steps

### 1. Isolate The Run And Capture The Baseline

- Record `git status`, hashes of all dirty files, tool versions, package versions,
  and the exact starting commit.
- Create an isolated worktree/branch named `codex/post-merge-reproduction` from
  `d8a42d6`. Save the current manuscript diff as a patch for reference; do not apply
  it wholesale.
- Create separate baseline and corrected artifact directories. Use the same raw-data
  files for both, but never let either run write into the original dirty manuscript.
- Treat committed parent outputs at `68b5159` as the paper baseline. Where feasible,
  rerun the parent code in a temporary harness so the merge fix is the only empirical
  difference; clearly label any comparison that relies on committed rather than
  freshly regenerated baseline artifacts.

**Proof:** initial manifest, dirty-file hash manifest, and isolated branch/worktree.

### 2. Upgrade Raw-Data And Merge Auditing

- Extend `CODE/prepare data/audit_commune_keys.R` to inspect non-key values inside
  duplicate key groups and classify duplicates as exact, compatible/complementary,
  or conflicting.
- Add a raw-data provenance ledger for the datasets used by `prepare_data`. Search
  repository notes, `DATA/raw_data_files_used.xlsx`, raw-file metadata, and script
  comments for download/source clues. The report must explicitly flag missing
  provenance rather than inferring it.
- Check documentation for the 1999 commune-canton bridge before treating it as
  authoritative: where `france1999.dbf` came from, what year/geography it encodes,
  whether canton fragments represent administrative reality or GIS artifacts, and
  whether commune-code changes over time could create false matches or misses.
- Add a merge ledger helper near the existing key assertions in
  `CODE/prepare data/prepare_data.R:36-78`. Each production join in
  `CODE/prepare data/main.R` will emit coverage and row-cardinality diagnostics.
- Produce:
  - `OUTPUT/data_quality/raw_source_profile.csv`
  - `OUTPUT/data_quality/raw_data_provenance.csv`
  - `OUTPUT/data_quality/commune_code_stability_review.md`
  - `OUTPUT/data_quality/raw_key_conflicts.csv`
  - `OUTPUT/data_quality/merge_ledger.csv`
  - `OUTPUT/data_quality/unmatched_commune_codes.csv`
- Replace any silent conflicting-value selection, including the generic
  `summarise(value = first(value))` path at
  `CODE/prepare data/prepare_data.R:101-103`, with an explicit rule or a hard failure.
- Add focused synthetic R tests for code normalization, exact duplicates,
  conflicting duplicates, join multiplication, and unmatched keys.

**Proof:** synthetic tests fail on conflicting duplicates before the fix and pass
afterward; production audit has zero unresolved conflicts in variables used by an
analysis; provenance gaps and commune-code/canton-bridge assumptions are documented
before final causal wording is updated.

### 3. Resolve Multi-Canton Communes Defensibly

- Build an analytical impact table from the full bridge in
  `DATA/raw data/france1999.dbf`, showing each split commune's canton list,
  treatment status, distance/bandwidth membership, and use in DiD/RDD/border samples.
- Remove the first-sorted-canton rule as the primary analytical mapping.
- Use a conservative primary cluster definition: ordinary canton for communes with
  one canton; a commune-specific cluster for communes split across cantons. This
  preserves observations without pretending a split commune belongs wholly to one
  arbitrary canton.
- Re-estimate key models under two sensitivities: exclude split communes; cluster by
  department. If an authoritative canton-ou-ville identifier can be derived from an
  existing source, add it as another sensitivity, not an unverified replacement.
- Document the chosen rule and its consequences in the data-quality report and table
  notes.

**Decision gate:** if the main DiD/RDD coefficient moves by at least 0.1 percentage
point, changes sign, or its confidence interval changes whether it includes zero,
the paper will report the sensitivity prominently rather than silently choosing the
most favorable mapping.

### 4. Rebuild Geographic And Commune-Level Data

- Run `CODE/Python code/master.py`, which regenerates the four required geographic
  outputs listed at `CODE/Python code/master.py:28-34`.
- Validate Python/R commune-code normalization on a shared fixture, including leading
  zeros, decimal-looking codes, Corsican alphanumeric codes, blanks, and missing values
  (`CODE/Python code/config.py:32-36`; `CODE/prepare data/prepare_data.R:15-20`).
- Run the R data pipeline through `CODE/prepare data/prepare_data.R:229-252` and rerun
  `audit_commune_keys.R` plus `check_commune_merges.R` independently.
- Save row counts, unique keys, sample attrition, file sizes, runtimes, and hashes for
  every processed artifact.

**Proof:** Python reports 4/4 success; R merge checks pass; no unclassified conflict
or unexplained cardinality change remains.

### 5. Re-Estimate And Compare Every Main Result

- Regenerate the table scripts sourced by
  `CODE/prepare tables/prepare_tables.R:168-227` and the figure scripts sourced by
  `CODE/prepare figures/prepare_figures.R:41-104`.
- Extract model results directly from model objects or structured result frames, not
  by parsing rendered LaTeX where avoidable.
- Produce `OUTPUT/data_quality/regression_impact_summary.csv` and
  `OUTPUT/data_quality/post_merge_reproduction_report.md` covering:
  - descriptive sample composition;
  - DiD estimates;
  - RDD estimates by bandwidth/specification;
  - border-pair estimates;
  - placebo/balance results;
  - absolute-vote and socioeconomic outcomes;
  - split-commune sensitivities.
- Flag coefficient changes above 0.1 percentage point, sample changes above 1%,
  sign changes, and significance changes.

**Proof:** every numerical claim in the main text can be traced to a regenerated
table/model and appears in the comparison report.

### 6. Formalize The DiD And Dynamic Figure

- Reconcile `CODE/prepare tables/DID_results.R` with the draft `CODE/DiD_main.R` into
  one reproducible implementation wired into the normal table/figure masters.
- Freeze the primary sample definition: 1995 entrants versus post-2004 entrants,
  common support for FN1988/FN2002, one row per commune, and a documented rule for
  never-treated municipalities.
- Make the first-difference equation and code agree. Use baseline/pre-treatment
  controls for the adjusted cross-sectional change model; do not condition the main
  estimate on post-treatment mediators without labeling that specification separately.
- Rebuild the dynamic election-year figure with the correct fixed effects and
  clustering. Describe it as dynamic relative-year contrasts unless the data truly
  contain enough pre-treatment periods to test pre-trends.
- Verify the date of the 1995 presidential outcome relative to legal and effective
  ZRR treatment. If timing is ambiguous, exclude 1995 from causal post-treatment
  interpretation and say so.
- Generate the requested `dFN` versus log-population figure on the exact main DiD
  sample, with clear descriptive language and no causal overclaim.

**Proof:** table, equation, notes, sample counts, clustering, and figures all encode
the same specification; a machine-readable model manifest records each choice.

### 7. Reconcile The Manuscript With Verified Results

- Import the useful parts of the existing dirty draft selectively after comparing
  them with regenerated evidence.
- Update the DiD-first framing in `Introduction.tex:43-45` and the DiD discussion in
  `DID.tex:1-56` only where the estimates and design checks support the language.
- Remove or soften unsupported statements, especially the current parallel-trends
  claim at `DID.tex:44` and any “bias makes this a lower bound” statement not backed
  by a formal argument.
- Update `Discussion.tex:26` from regenerated absolute-vote estimates.
- Refresh table/figure copies in the LaTeX folder only for artifacts actually used by
  the manuscript. Move obsolete exploratory outputs aside rather than mixing versions.
- Resolve remaining directly related reviewer markers; leave unrelated stylistic or
  structural edits for a separate commit unless needed for internal consistency.

**Proof:** a claim-to-output checklist maps each changed number and causal statement
to its source table, figure, model, or documented limitation.

### 8. Compile, Review, Commit, And Push

- Run the full R master at `CODE/master.R:67-77` once component runs pass.
- Compile `Latex/ZRR and populist vote/main.tex` with the full LaTeX/Biber cycle.
- Check the PDF page count, missing figures/tables, undefined citations/references,
  overfull boxes, and obvious layout regressions.
- Recheck the original dirty-worktree hash manifest to prove no accidental overwrite.
- Make reviewable Lore commits, expected split:
  1. data semantics, merge ledger, and tests;
  2. defensible canton clustering and sensitivity outputs;
  3. reproducible DiD/results regeneration;
  4. manuscript/output reconciliation and build evidence.
- Update `AGENTS.md`, push `codex/post-merge-reproduction`, and report the commit IDs,
  branch URL, changed-result summary, remaining risks, and exact verification evidence.

## Risks And Mitigations

- **Multi-canton mapping has no authoritative commune-level answer in the raw file.**
  Use transparent primary and sensitivity definitions; never hide dependence on one.
- **The old pipeline may not reproduce under the current environment.** Use the
  parent commit plus a path-only harness; distinguish historical committed output from
  a clean A/B rerun in the report.
- **Long-running border/randomization work may dominate runtime.** Run required
  components with logs and checkpoints. Randomization is rerun only if its inputs or
  implementation changed; the border table remains required because it is cited.
- **Existing manuscript edits could be lost or accidentally committed.** Work in an
  isolated worktree, save a patch and hashes, and apply only reviewed hunks.
- **Regenerated results may undermine current claims.** Evidence controls prose. A
  null or unstable result is reported as such; the run does not optimize specifications
  for significance.
- **Only one pre-treatment election may be available.** Do not call the dynamic plot a
  pre-trend test; frame it as timing/descriptive evidence and state the limitation.

## Verification Sequence

1. Static checks: Python compile; R parse/source checks for changed helpers.
2. Focused tests: key normalization, duplicate conflicts, joins, canton definitions.
3. Data integration: Python 4/4, R processed-data rebuild, merge assertions.
4. Model integration: structured coefficient and sample manifests for all main models.
5. A/B validation: parent versus corrected result report and sensitivity thresholds.
6. End-to-end: full `CODE/master.R` run and complete LaTeX/Biber build.
7. Provenance review: raw-source ledger, commune-code stability note, and canton
   bridge documentation status are checked against the empirical claims.
8. Repository proof: scoped diff review, dirty-file hash comparison, Lore commits, push.

## Implementation Notes - 2026-07-14

- Added raw-data provenance/stability review to the plan and implementation. The
  generated report documents that `DATA/raw_data_files_used.xlsx` lists local files
  and consumers but not original download URLs or source vintages, so external
  documentation recovery remains required before treating the canton bridge as
  authoritative.
- Replaced arbitrary first-canton assignment with conservative commune-specific
  cluster IDs for split or missing canton bridge cases, and generated explicit
  audits for 342 split-canton communes and 2,471 missing-canton communes.
- Added merge-ledger and duplicate-resolution helpers, focused R tests, raw-key
  conflict reports, education duplicate resolution, and commune merge sensitivity
  summaries under `OUTPUT/data_quality/`.
- Rebuilt Python geographic inputs, R processed data, tables, figures, and the
  LaTeX manuscript. `main.pdf` compiles to 62 pages with no LaTeX errors; remaining
  warnings are layout-only, notably one appendix float too tall and bibliography URL
  overfull boxes.
- The broader DiD-first manuscript reframing and full claim rewrite remain the next
  paper-writing phase; this run made the commune-level merge foundation auditable
  and reproducible.

## Overnight Completion Deliverables

- a pushed `codex/post-merge-reproduction` branch;
- strengthened raw-key/conflict and merge-ledger code;
- defensible multi-canton handling plus sensitivity results;
- freshly rebuilt processed data, tables, figures, and PDF;
- a machine-readable before/after regression comparison;
- a concise reproduction report identifying every material result change;
- manuscript wording aligned with the evidence;
- updated `AGENTS.md` with completed work and remaining scientific risks.

## Approval Boundary

Approval authorizes the full sequence above, including code and manuscript edits,
regeneration of ignored local data artifacts, creation of a new branch/worktree,
commits, and push. It does not authorize overwriting or committing unrelated existing
dirty work, force-pushing, adding dependencies, or choosing a favorable specification
when sensitivity results disagree.
