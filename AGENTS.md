# THESIS REPRODUCTION PROJECT - AGENTS.md
**Project:** ZRR and Populist Vote - Academic Paper
**GitHub Repository:** https://github.com/ilan5410/ZRR-FN
**Last Updated:** 2026-07-13

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
| 5. Paper Finalization | In Progress | 55% |

---

## STAGE 1: Table Formatting (100% Complete)

**All tables compile cleanly, PDF renders at 62 pages with 0 errors**

| Table | Issue | Fix Applied |
|-------|-------|-------------|
| Table 1 (descriptive_statistics) | Split into 2 tables | scale_down + Cagé-Piketty footnote (fixed escaping) |
| Table 3 (DID_results) | Horizontal overflow + `\times` outside math mode | `$\times$` in math mode + notes in `\parbox` + removed duplicate column labels |
| Table 6 (main_results_diff_specifications) | Horizontal overflow | `format_latex_table()` with resizebox |
| Table 8 (balancing_tests) | Variable names overflow | `p{5.5cm}` column + resizebox |
| Table 9 (border_muni_results) | Too wide for portrait | Landscape mode |
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

## STAGE 5: Paper Finalization (55% - In Progress)

**TODO tracking documents (created Session 6):**
- `Latex/ZRR and populist vote/TODOs.md` - full inventory of TODOs + reviewer [YS:] comments
- `Latex/ZRR and populist vote/TODO_PLAN.md` - phased implementation plan with status table

**Tasks:**
- [x] Verify all tables render correctly (all tables compile, 0 errors)
- [x] Verify all figures render correctly (all figures load in PDF)
- [x] Final PDF compilation (62 pages, clean build)
- [x] Fix bibliography (biber 2.21 installed, references resolved)
- [x] Fix remaining undefined reference (`tab:1988-2002` — appendix uncommented, resolves)
- [x] Phase 1 quick fixes: 6 figure captions ("double title"), IGN2020 reference added
- [ ] Phase 2: Update DID.tex line 42 paragraph (parallel trends, new references)
- [ ] Phase 2: Update Discussion.tex line 26 paragraph (absolute_vote results changed with new sample)
- [ ] Phase 3: Structural revisions per YS comments (restructure DID section around Fig 7, heterogeneity conclusion, randomization discussion)
- [ ] Phase 4: Figure enhancements (density plot on Fig 6, ΔFN vs log-pop figure, balancing test CIs)
- [ ] Phase 5: Minor clarifications (Spatial.tex canton size, DID.tex post-2004, Background.tex fence density)
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

### Session 10 - 2026-07-13
**Overnight post-merge reproduction plan prepared (awaiting approval)**
- Wrote `.omx/plans/overnight-post-merge-reproduction.md`
- Scoped an isolated A/B rebuild from the pre-fix parent and corrected merge branch
- Identified the next data-quality gap: the raw audit counts duplicate keys but does not yet distinguish exact duplicates from conflicting values
- Identified a substantive remaining issue: `main.R` prevents row multiplication by selecting the first sorted canton for 342 multi-canton communes; 150 of these enter the 20 km sharp sample, so canton clustering needs explicit sensitivity analysis
- Identified a DiD claim risk: the current draft describes the dynamic figure as validating parallel trends despite apparently having only one pre-treatment presidential election
- Confirmed the overnight run will preserve the current dirty manuscript, regenerate all evidence, compile, commit with Lore messages, and push a dedicated branch only after Ilan approves the plan

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

## NEXT ACTIONS — OVERNIGHT REPRODUCTION (Session 10, 2026-07-13)

**Approval status:** Awaiting Ilan's approval of
`.omx/plans/overnight-post-merge-reproduction.md`. Once approved, execute the full
data audit, multi-canton sensitivity analysis, Python/R rebuild, result comparison,
DiD validation, manuscript reconciliation, LaTeX compilation, Lore commits, and push.

The workstreams below remain the paper-finalization direction, but numerical and
causal wording must follow the regenerated evidence. In particular, DiD-first framing
is provisional until the sample, controls, timing, clustering, and sensitivity checks
in the overnight plan pass.

**Provisional strategy after full re-read of paper + RDD review:** Reframe the paper around
DiD (ΔFN specification + event study) as the PRIMARY identification strategy, with the spatial
RDD/border analysis as supporting evidence. Rationale: (a) the RDD review showed the RDD effect
depends entirely on dept FE and vanishes at 1km (no true spatial discontinuity), while DiD is
robust (0.7–1.2 pp, timing matches 1995 launch); (b) YS's comments already push this way
("start by presenting Figure 7", requests for ΔFN figures); (c) the Intro's contribution section
already describes the paper as DiD-first ("We further enhance robustness... spatial RDD").

### Workstream A — Reframe identification (core, ~2-3 sessions)
1. Promote `alternative_identification.R` exploratory DiD into a formal script producing
   publication-quality tables (ΔFN spec, canton-clustered SEs) + event study figure
2. Restructure Results: DID subsection opens with Figure 7 (YS), add ΔFN vs log(pop) figure
   (YS, DID.tex:25), then present spatial RDD honestly (dept FE dependence acknowledged)
3. Address the buried comment in DID.tex ("why is the difference negative and so strong?
   selection should bias the other way") — needs an explicit paragraph
4. Update Introduction/abstract: main finding becomes DiD-based (~1 pp), RDD supporting
   (currently claims 0.3–0.5 pp RDD as main result)

### Workstream B — Stale paragraphs (quick, independent)
5. DID.tex:42 — rewrite parallel-trends paragraph (tab:1988-2002 now resolves)
6. Discussion.tex:26 — check current absolute_vote.tex results, rewrite numerator/denominator
   conclusion to match

### Workstream C — Remaining YS comments
7. Heterogeneity → move to appendix (YS: "at best belongs in appendix"; IP note in tex agrees:
   "I suggest removing this part"), leave 1-paragraph pointer in main text
8. Randomization exercise → move to appendix + acknowledge (i) 33% arbitrary (ii) placebo lacks
   ZRR spatial correlation (robustness.tex:42)
9. Spatial.tex:5 canton-size comment — text above it already says "6 to 10 on average";
   verify sufficient, then delete comment
10. Background.tex:129 fence density — add footnote acknowledging limitation (per-agricultural-land
    version deemed too costly)
11. DID.tex post-2004 question ("did any qualify under earlier criteria?") — check in data
    (CODE/prepare data), answer in text
12. EvolutionFN.tex:37 — add treated-municipality density to Fig 6 (FN_versus_pop.R)
13. Spatial.tex:45 — investigate non-linear CIs in balancing figure (check R code)

### Workstream D — Polish & housekeeping
14. Fix duplicate Fetzer2019 (references.bib:465,507), month-format biber warnings
15. Full proofread (note: mixed "I"/"we" throughout — standardize)
16. Git hygiene: commit/discard 19 modified figure PNGs, gitignore main.pdf
17. Optional: regenerate border_muni_results.R (very slow, 7K+ pair FEs)

**Suggested order:** B (quick wins) → A (core, after Ilan confirms) → C → D

**Completed prerequisite (Session 9):** commune-level merge QA is now in place. Before
interpreting new estimates, check `OUTPUT/data_quality/raw_commune_key_audit.csv` and run
`Rscript "CODE/prepare data/check_commune_merges.R"` after any data-prep change.

---

*End of AGENTS.md*

## Imported Claude Cowork project instructions

Academic paper that I want to publish. Still a draft.
