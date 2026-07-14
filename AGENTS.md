# THESIS REPRODUCTION PROJECT - AGENTS.md
**Project:** ZRR and Populist Vote - Academic Paper
**GitHub Repository:** https://github.com/ilan5410/ZRR-FN
**Last Updated:** 2026-07-14

---

## INSTRUCTIONS FOR Codex

**At the start of each session:**
- Read this AGENTS.md file completely
- Review "NEXT ACTIONS" section
- Check "Progress Tracking" to understand current state
- Continue from where previous session left off

**During each session:**
- Update "SESSION NOTES" with accomplishments
- Mark completed checklist items
- Note any issues encountered
- Update progress percentages

**At the end of each session:**
- Update "NEXT ACTIONS" section
- Update "Progress Tracking" percentages
- Add session summary to "SESSION NOTES"
- Update "Last Updated" date at top
- Save this file

---

## IMPORTANT REMINDERS

1. **Always update this AGENTS.md at the end of each session**
2. **Make backups before major restructuring**
3. **Test table compilation after each fix**
4. **Document solutions for reproducibility**
5. **Keep the GitHub repository public/private as appropriate**

---

## PROGRESS TRACKING

| Stage | Status | Progress |
|-------|--------|----------|
| 1. Table Formatting | **COMPLETE** | 100% |
| 2. Code Reorganization | Complete | 100% |
| 3. GitHub Push | Complete | 100% |
| 4. RDD Methodology Review | **COMPLETE** | 100% |
| 5. Paper Finalization | In Progress | 75% |

---

## STAGE 1: Table Formatting (100% Complete)

**All tables compile cleanly, PDF renders at 62 pages with 0 errors**

| Table | Issue | Fix Applied |
|-------|-------|-------------|
| Table 1 (descriptive_statistics) | Split into 2 tables | scale_down + Cagé-Piketty footnote (fixed escaping) |
| Table 3 (DID_results) | Horizontal overflow + `\times` outside math mode | `$\times$` in math mode + notes in `\parbox` + removed duplicate column labels |
| Table 6 (main_results_diff_specifications) | Horizontal overflow | `format_latex_table()` with resizebox |
| Table 8 (balancing_tests) | Variable names overflow | `p{5.5cm}` column + resizebox |
| Table 9 (border_muni_results) | Too wide for portrait | Rotated `sidewaystable` output |
| Table 10 (winsorizing_trimming_doughnut) | Horizontal overflow | Parbox notes + `format_latex_table()` |
| Table 11 (heterogeneity_causal) | `\\%` breaking table parsing | Fixed `\\\\%` to `\\%` in R notes string |
| effect_on_1999_socioeco | 7.5pt overflow | Added `font_size = 9` + notes parbox |
| absolute_vote | 15pt overflow | Added `footnotesize` + `format_latex_table()` resizebox |

**Completed:**
- [x] Re-run R scripts to regenerate .tex files
- [x] Test LaTeX compilation
- [x] Verify tables fit within page margins

**LaTeX preamble requirements:**
- `\usepackage{graphicx}` - for `\resizebox`
- `\usepackage{pdflscape}` - for landscape tables
- `\usepackage{booktabs}` - for professional rules

---

## STAGE 2: Code Reorganization (100% Complete)

**Completed:**
- `CODE/README.md` - comprehensive documentation
- `CODE/Python code/requirements.txt` - Python dependencies
- `master.R` uses `here::here()` for relative paths
- `master.py` + `config.py` for Python pipeline
- 8 figure R scripts fixed (trailing comma errors in `grid.arrange()`)

---

## STAGE 3: GitHub Push (100% Complete)

Repository: https://github.com/ilan5410/ZRR-FN
- Force pushed 2026-02-03
- Fixed HTTP 400 with `git config http.postBuffer 524288000`

---

## STAGE 4: RDD Methodology Review (100% Complete)

**Goal:** Review paper against RDD best practices from academic literature

**Reference:** `Literature/RDD_Methodology_Summary.md` (based on Lee & Lemieux 2010)

**Tasks Completed:**
1. [x] Read RDD methodology summary (Lee & Lemieux 2010)
2. [x] Implement RDD best practices analysis (`CODE/RDD_best_practices.R`)
3. [x] Run formal validity tests (McCrary density, covariate balance)
4. [x] Investigate why department FE matter (`CODE/investigate_dept_fe.R`)
5. [x] Explore alternative identification strategies (`CODE/alternative_identification.R`)
6. [x] Document findings in LaTeX report

**Key Findings:**
1. **Standard RDD (rdrobust) finds NO significant effect** (p = 0.93)
2. **Significance requires department FE** - they reduce SE by 36%
3. **Geographic confounding exists** - correlation -0.38 between dept ZRR rate and FN vote
4. **DiD is more robust** - 9/12 specifications show significant negative effect
5. **Effect disappears at 1km** - inconsistent with true spatial discontinuity
6. **Event study supports causality** - effect appears in 1995 when ZRR starts

**Recommendation:** Consider DiD as primary identification strategy

**Output Files:**
- `OUTPUT/RDD_best_practices/RDD_Methodology_Review.pdf` - Full report
- `OUTPUT/RDD_best_practices/figures/` - RDD diagnostic plots
- `OUTPUT/RDD_best_practices/investigation/` - Dept FE analysis
- `OUTPUT/RDD_best_practices/alternative_strategies/` - DiD/event study results

---

## STAGE 5: Paper Finalization (75% - In Progress)

**TODO tracking documents (created Session 6):**
- `Latex/ZRR and populist vote/TODOs.md` - full inventory of TODOs + reviewer [YS:] comments
- `Latex/ZRR and populist vote/TODO_PLAN.md` - phased implementation plan with status table

**Tasks:**
- [x] Verify all tables render correctly (all tables compile, 0 errors)
- [x] Verify all figures render correctly (all figures load in PDF)
- [x] Final PDF compilation (66 pages, clean build)
- [x] Fix bibliography (biber 2.21 installed, references resolved)
- [x] Fix remaining undefined reference (`tab:1988-2002` — appendix uncommented, resolves)
- [x] Phase 1 quick fixes: 6 figure captions ("double title"), IGN2020 reference added
- [x] Phase 2: Update DID.tex line 42 paragraph (parallel trends/timing caveat)
- [ ] Phase 2: Update Discussion.tex line 26 paragraph (absolute_vote results changed with new sample)
- [ ] Phase 3: Structural revisions per YS comments (heterogeneity conclusion, randomization discussion)
- [x] Phase 4: Add ΔFN vs log-pop figure
- [ ] Phase 4: Remaining figure enhancements (density plot on Fig 6, balancing test CIs)
- [ ] Phase 5: Minor clarifications (DID.tex post-2004, Background.tex fence density)
- [x] Phase 5: Spatial.tex canton size clarification/comment cleanup
- [x] Add empirical contract, raw-data documentation note, model manifest, and referee-risk memo
- [ ] Address remaining 9 [YS:] reviewer comments across 6 .tex files
- [ ] Proofread all sections
- [ ] Fix biber warnings (duplicate Fetzer2019 key at references.bib lines 465 & 507, month format warnings)

---

## TECHNICAL REFERENCE

### Key Folders
- `CODE/prepare tables/` - R scripts generating .tex tables
- `CODE/prepare figures/` - R scripts generating .png figures
- `OUTPUT/tables/` - Generated .tex files (19 total)
- `OUTPUT/figures/` - Generated .png files (25 total)
- `OUTPUT/data_quality/` - Commune-key audit reports
- `Latex/ZRR and populist vote/` - LaTeX source files (main.tex, preamble.tex, etc.)

### Data Pipeline QA
- `CODE/prepare data/audit_commune_keys.R` - audits raw/processed commune keys and duplicate patterns
- `CODE/prepare data/audit_official_commune_history.R` - verifies INSEE COG 1999 provenance and audits official commune-history applicability
- `CODE/prepare data/check_commune_merges.R` - asserts expected commune and commune-year merge cardinality
- `DATA/processed data/multi_canton_communes.csv` - communes with multiple 1999 canton fragments

### Helper Functions (prepare_tables.R)
1. `format_latex_table(tex_file, use_resizebox, font_size, use_landscape, notes_width)`
   - Post-processes stargazer output with resizebox, landscape, notes formatting
2. `convert_to_longtable(tex_file, footnote_text)`
   - Converts split tables to longtable format with custom footnotes

### Run Full Pipeline
```bash
# Python preprocessing
cd "CODE/Python code" && python master.py

# R tables/figures
Rscript CODE/master.R
```

---

## SESSION NOTES

### Session 12 - 2026-07-14
**Official commune-history crosswalk applicability check**
- Switched from the deleted `codex/publication-readiness` remote branch to `main` after `git fetch --prune`; `origin/main` now contains the merged publication-readiness PR.
- Found the official INSEE COG 1999 page and verified that checked-in `DATA/raw data/france1999.dbf` is byte-for-byte identical to INSEE's `france1999-dbf.zip`.
- Confirmed INSEE's 1999 documentation explicitly says multi-canton communes are represented by one row per canton fraction plus one `canton non precise` row, so the split-canton rows are official, not a GIS artifact.
- Added `CODE/prepare data/audit_official_commune_history.R` plus outputs `official_commune_history_applicability.md`, `official_commune_history_source_check.csv`, `commune_history_code_audit.csv`, `commune_history_code_flags.csv`, and `commune_history_analysis_sample_audit.csv`.
- Found that official 2026 INSEE commune-history files apply as validation/crosswalk inputs: raw sources contain many historical/non-current commune codes, but the primary `df_merged` analysis panel has 94 flagged codes and the RDD distance sample has 105.
- Next implication: the paper can remove the unrecovered-source caveat for `france1999.dbf`, but should still run a movement-based harmonization sensitivity using `v_mvt_commune_2026.csv` before final submission.

### Session 11 - 2026-07-14
**Publication-readiness implementation and full verified rebuild**
- Integrated the audited post-merge reproduction foundation into `codex/publication-readiness` after preserving the prior manuscript baseline in a separate commit.
- Wired `CODE/DiD_main.R` into `CODE/master.R` so the publication DiD table and two DiD figures are regenerated with the main R pipeline and mirrored to the LaTeX folders.
- Added `OUTPUT/data_quality/empirical_contract.md`, `model_manifest.csv`, `raw_data_external_documentation.md`, and `referee_risk_memo.md` to document the estimand, source assumptions, model artifacts, and referee-facing weaknesses.
- Checked official documentation anchors for INSEE COG/canton conventions, ZRR legal timing, and election data sources. Key implication: the 1995 coefficient is a timing/selection diagnostic, not clean post-treatment evidence, because the ZRR law dates to 1995 but the defining decree dates to 1996.
- Rewrote the abstract, introduction, DID section, data note, spatial-RDD framing, and border-municipality interpretation so the first-difference design is primary and spatial/RDD evidence is supporting.
- Fixed the FN1988 placebo plotting scripts so the placebo outcome is no longer included as its own control, then regenerated the standard and shortest-distance placebo figures.
- Verification completed: `.venv/bin/python "CODE/Python code/master.py"` passed 4/4 scripts; `Rscript CODE/master.R` completed; `Rscript "CODE/tests/run_data_quality_tests.R"` passed; `Rscript "CODE/prepare data/check_commune_merges.R"` passed; `latexmk -pdf -interaction=nonstopmode main.tex` produced a 66-page PDF.
- Remaining risks: exact original raw-data download URLs are still incomplete, official commune-history crosswalk validation is not yet applied, and the RDD placebo/signal diagnostics show 1995 differences.

### Session 10 - 2026-07-14
**Commune-level merge audit, raw-data provenance, and full reproduction build**
- Implemented `CODE/prepare data/data_quality_helpers.R` and `CODE/tests/run_data_quality_tests.R` for commune-code normalization, duplicate classification, audited joins, merge ledgers, and conflict-aware duplicate collapse.
- Reworked `CODE/prepare data/main.R`, `eco_outcomes.R`, `script_sharp.R`, and `script_sharp_noEpicenter.R` so split/missing canton bridge cases no longer silently select the first canton; split/missing cases receive conservative commune-specific cluster IDs and audit flags.
- Added raw-data documentation/provenance checks in `document_raw_data_provenance.R`; current finding: local inventory identifies files/purposes/consumers, but original download URLs and source vintages are missing, so commune-code/canton-bridge documentation still needs external recovery before final causal wording.
- Added sensitivity outputs in `summarize_commune_merge_sensitivity.R`: split-canton exclusion does not materially move the main RDD coefficient in the quick check, but the manuscript should still describe canton clustering as an analysis convention.
- Rebuilt Python geographic inputs, R processed data, tables, figures, and LaTeX manuscript. Final `latexmk` build: 62 pages, zero fatal errors; remaining warnings are layout-only (appendix float too tall, bibliography URL overfull boxes).
- Fixed table-generation compatibility issues exposed by the rebuild: explicit numeric selection for `effect_on_1999_socioeco.R`, modern `if_all()` use in `annex_var_evolution.R`, direct `modelsummary()` output for no-epicenter annex tables, `sidewaystable` rotation for `border_muni_results.tex`, and biblatex aliases for `\citep`/`\citet`.

### Session 9 - 2026-07-13
**Commune-level data merge audit and fixes**
- Added raw/processed commune-key audit reports in `OUTPUT/data_quality/`
- Added merge integrity checks and wired them into `CODE/prepare data/prepare_data.R`
- Fixed accidental row multiplication from `france1999.dbf` multi-canton commune fragments by using a deterministic primary canton and saving `multi_canton_communes.csv`
- Tightened downstream joins in `borders_pair.R`, `FN_growth.R`, `script_sharp.R`, `script_sharp_noEpicenter.R`, and `dataDes.R`
- Standardized Python preprocessing commune-code normalization and fixed the no-epicenter output filename in `CODE/Python code/master.py`
- Removed hardcoded `main_path` override in `CODE/master.R`
- Optimized population processing by limiting controls to 1975-2020 and replacing slow age-sex wide loops with a long-format calculation
- Verification: `check_commune_merges.R` passes; `main.RData::dfZRR_raw` and `main.RData::df_merged` now have row counts equal to unique `(codecommune, year)` counts

### Session 8 - 2026-07-13
**Fresh project read & new plan**
- Re-read all 14 .tex files, all [YS:] comments, and RDD_Methodology_Review.tex conclusions
- Decided (pending Ilan's confirmation): make DiD the primary identification strategy, RDD supporting
- Replaced NEXT ACTIONS with 4-workstream plan (A: reframe identification, B: stale paragraphs, C: YS comments, D: polish)
- New findings from read: buried unaddressed comment in DID.tex on sign of selection bias; Intro contribution section already frames paper DiD-first (inconsistent with "main one being spatial RDD" earlier in Intro); heterogeneity section has IP note suggesting removal; mixed I/we voice throughout

### Session 7 - 2026-07-13
**Repo review & AGENTS.md sync**
- Reviewed repo state; no code/paper changes since 2026-02-06 (last commit 68b5159)
- Backfilled Session 6 notes (was done but never recorded in AGENTS.md)
- Updated Stage 5 checklist from TODO_PLAN.md tracking table and current .tex contents
- Verified: DID.tex:42 and Discussion.tex:26 TODOs still open; 9 [YS:] comments remain; Fetzer2019 duplicated in references.bib (lines 465, 507)
- Uncommitted local changes: 19 modified figure PNGs in Latex/.../figures/, untracked main.pdf and .Codex/

### Session 6 - 2026-02-06
**TODO Inventory & Phase 1 Quick Fixes - Complete** (commits 3de9085, 3c9f63a, 68b5159)
- Created `TODOs.md`: inventory of all TODOs and [YS:] reviewer comments (6 caption fixes, 3 content updates, 8 reviewer comments, 1 missing reference)
- Created `TODO_PLAN.md`: 5-phase implementation plan with tracking table
- **Phase 1 completed:**
  - Fixed 6 figure captions ("TODO: remove double title") in Background.tex, EvolutionFN.tex, robustness.tex
  - Added IGN2020 citation to references.bib, cited in Data.tex
  - Uncommented tab:1988-2002 appendix section — all references now resolve (0 undefined)
- PDF compiles cleanly: 62 pages, 0 errors

### Session 5 - 2026-02-05
**LaTeX Compilation & Table Fixes - Complete**

**LaTeX source fixes (non-table .tex files):**
- Added `\usepackage{dsfont}` to preamble.tex for `\mathds{1}` command
- Added `\usepackage{appendix}` to preamble.tex
- Added `\setcounter{biburllcpenalty}{7000}` / `biburlucpenalty{8000}` for bibliography URL breaking
- Removed stray `\end{table}` and duplicate notes from heterogeneity.tex
- Fixed `&` → `\&` in 4 references.bib publisher fields
- Enabled `\printbibliography` and `\input{Appendices.tex}` in main.tex
- Fixed 7 figure filename references in Appendices.tex
- Changed `\include` to `\input` for descriptive_statistics in Data.tex
- Fixed `\ref{later_elections}` → `\ref{fig:later_elections}` typo in Spatial.tex
- Added `\footnotesize` to 2 inline appendix tables in Appendices.tex
- Installed biber 2.21 via homebrew for biblatex 3.20 compatibility

**R code fixes:**
- `DID_results.R`: Fixed `\times` to `$\times$` (math mode), removed duplicate `column.labels`
- `descriptive_statistics.R`: Fixed Cagé footnote escaping (switched gsub to `sub(..., fixed=TRUE)`)
- `heterogeneity_causal_fm.R`: Fixed `10\\\\%` → `10\\%` (was producing `\\%` = linebreak+comment in LaTeX)
- `absolute_vote.R`: Added `font.size = "footnotesize"` + `format_latex_table()` for 15pt overflow
- `effect_on_1999_socioeco.R`: Added `font_size = 9` + notes parbox for 7.5pt overflow
- `prepare_tables.R`: Fixed `format_latex_table()` resizebox closing regex
- `main_results_different_bandwidths.R`: Changed `\small` to `\footnotesize`
- `main_results_diff_outcomes.R`: Added `\footnotesize`

**Tables regenerated and copied from OUTPUT/tables/ to Latex/.../tables/:**
- descriptive_statistics.tex, DID_results.tex, summary_stats_bandwidth.tex
- main_results_different_bandwidths.tex, main_results_diff_specifications.tex
- main_results_diff_outcomes.tex, balancing_tests.tex
- winsorizing_trimming_doughnut.tex, heterogeneity_causal.tex
- absolute_vote.tex, effect_on_1999_socioeco.tex

**Final build result:** 62 pages, 0 errors, 4 overfull hbox (2 negligible text, 2 bibliography URLs)

### Session 4 - 2026-02-04
**RDD Methodology Review - Complete**
- Read RDD methodology summary (Lee & Lemieux 2010)
- Created `CODE/RDD_best_practices.R` implementing:
  - McCrary density test (rddensity)
  - Optimal bandwidth selection (rdrobust)
  - Bias-corrected inference
  - Covariate balance tests
  - Polynomial robustness
  - Placebo cutoff tests
- Created `CODE/investigate_dept_fe.R` to analyze why dept FE matter
- Created `CODE/alternative_identification.R` exploring:
  - Difference-in-Differences
  - Propensity Score Matching + DiD
  - Border pair analysis
  - Event study
- Created comprehensive LaTeX report: `OUTPUT/RDD_best_practices/RDD_Methodology_Review.pdf`
- **Major finding:** Standard RDD finds no effect; DiD is more robust

### Session 3 - 2026-02-03
- Fixed 8 R figure scripts (trailing comma errors)
- Force pushed to GitHub
- Created Python venv, tested full Python pipeline

### Session 2 - 2026-02-02
- Created `format_latex_table()` and `convert_to_longtable()` helpers
- Fixed all 7 table R scripts
- Created CODE/README.md and requirements.txt
- Fixed master.R to use here::here()

### Session 1 - 2026-02-01
- Created AGENTS.md structure
- Identified 7 table formatting issues from screenshots
- Documented fix requirements for each table

---

## NEXT ACTIONS — PUBLICATION READINESS (Session 11, 2026-07-14)

**Current status:** The draft now uses the early-versus-later treated first-difference design as the primary evidence, with spatial/RDD estimates as supporting evidence. The full Python/R/LaTeX reproduction path runs, and the empirical caveats are documented. The paper is closer to publishable, but not yet submission-ready.

### Workstream A — Source and geography validation
1. Recover exact original download URLs/vintages for every raw election, census, ZRR, JOAFE, income, and geography file.
2. Use the verified INSEE COG 1999 citation and extraction rule in the manuscript/replication README: `france1999.dbf` comes from https://www.insee.fr/fr/statistiques/fichier/2560686/france1999-dbf.zip.
3. Build and apply a movement-based official commune-history crosswalk using INSEE `v_mvt_commune_2026.csv`, then rerun `check_commune_merges.R` and compare estimates with/without harmonization.

### Workstream B — Identification hardening
4. Investigate the 1995 placebo/signal results directly; the current manuscript correctly treats 1995 as a warning, but the diagnostics need a referee-ready appendix explanation.
5. Explore whether additional pre-treatment presidential/legislative data can create a real pre-trend check; otherwise keep the one-pre-period caveat prominent.

### Workstream C — Manuscript finishing
6. Rewrite the remaining stale `Discussion.tex` absolute-vote paragraph using the regenerated `absolute_vote.tex` results.
7. Finish YS comment cleanup: heterogeneity and randomization should likely be appendix-first, with modest main-text interpretation.
8. Add the remaining minor clarifications: post-2004 eligibility wording, fence-density limitation, balancing-test CI/nonlinearity note.
9. Standardize voice (`I` vs `we`) and do a full proofread for causal overclaiming.

### Workstream D — Submission polish
10. Fix bibliography warnings (duplicate `Fetzer2019`, month formatting) and remaining layout warnings, especially the oversized appendix float.
11. Rebuild from a clean worktree immediately before submission and record the exact verification commands in the replication README.

---

*End of AGENTS.md*

## Imported Claude Cowork project instructions

Academic paper that I want to publish. Still a draft.
