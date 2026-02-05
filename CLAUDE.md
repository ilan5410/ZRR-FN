# THESIS REPRODUCTION PROJECT - CLAUDE.md
**Project:** ZRR and Populist Vote - Academic Paper
**GitHub Repository:** https://github.com/ilan5410/ZRR-FN
**Last Updated:** 2026-02-05

---

## INSTRUCTIONS FOR CLAUDE

**At the start of each session:**
- Read this CLAUDE.md file completely
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

1. **Always update this CLAUDE.md at the end of each session**
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
| 5. Paper Finalization | In Progress | 40% |

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

## STAGE 5: Paper Finalization (40% - In Progress)

**Tasks:**
- [x] Verify all tables render correctly (all tables compile, 0 errors)
- [x] Verify all figures render correctly (all figures load in PDF)
- [x] Final PDF compilation (62 pages, clean build)
- [x] Fix bibliography (biber 2.21 installed, references resolved)
- [ ] Fix remaining undefined reference (`tab:1988-2002` — commented-out appendix)
- [ ] Proofread all sections
- [ ] Check citations and bibliography (minor: duplicate Fetzer2019 key, month format warnings)
- [ ] Address DID.tex TODO note about updating references

---

## TECHNICAL REFERENCE

### Key Folders
- `CODE/prepare tables/` - R scripts generating .tex tables
- `CODE/prepare figures/` - R scripts generating .png figures
- `OUTPUT/tables/` - Generated .tex files (19 total)
- `OUTPUT/figures/` - Generated .png files (25 total)
- `Latex/ZRR and populist vote/` - LaTeX source files (main.tex, preamble.tex, etc.)

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
- Created CLAUDE.md structure
- Identified 7 table formatting issues from screenshots
- Documented fix requirements for each table

---

## NEXT ACTIONS

1. **Decision:** Choose primary identification strategy (RDD with dept FE vs DiD)
2. **Stage 5:** Update paper methodology section based on RDD review findings
3. **Stage 5:** Address DID.tex TODO note and `tab:1988-2002` reference
4. **Stage 5:** Proofread all sections
5. **Stage 5:** Fix biber warnings (duplicate Fetzer2019 key, month format)
6. **Optional:** Regenerate border_muni_results.R (was not re-run — very slow with 7K+ pair FEs)
7. **Optional:** Create formal DiD analysis script if switching identification strategy

---

*End of CLAUDE.md*
