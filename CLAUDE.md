# THESIS REPRODUCTION PROJECT - CLAUDE.md
**Project:** ZRR and Populist Vote - Academic Paper
**GitHub Repository:** https://github.com/ilan5410/ZRR-FN
**Last Updated:** 2026-02-04

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
| 1. Table Formatting | R scripts modified | 85% |
| 2. Code Reorganization | Complete | 100% |
| 3. GitHub Push | Complete | 100% |
| 4. RDD Methodology Review | **COMPLETE** | 100% |
| 5. Paper Finalization | Pending | 10% |

---

## STAGE 1: Table Formatting (85%)

**R scripts modified, pending LaTeX compilation test**

| Table | Issue | Fix Applied |
|-------|-------|-------------|
| Table 1 (descriptive_statistics) | Split into 2 tables | longtable + Cagé-Piketty footnote |
| Table 3 (DID_results) | Horizontal overflow | Notes in `\parbox` |
| Table 6 (main_results_diff_specifications) | Horizontal overflow | `format_latex_table()` with resizebox |
| Table 8 (balancing_tests) | Variable names overflow | `p{5.5cm}` column + resizebox |
| Table 9 (border_muni_results) | Too wide for portrait | Landscape mode |
| Table 10 (winsorizing_trimming_doughnut) | Horizontal overflow | Parbox notes + `format_latex_table()` |
| Table 11 (heterogeneity_causal) | Horizontal overflow | Parbox notes + `format_latex_table()` |

**Remaining tasks:**
- [ ] Re-run R scripts to regenerate .tex files
- [ ] Test LaTeX compilation
- [ ] Verify tables fit within page margins

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

## STAGE 5: Paper Finalization (10% - Pending)

**Tasks:**
- [ ] Verify all figures render correctly
- [ ] Verify all tables render correctly (after Stage 1)
- [ ] Proofread all sections
- [ ] Check citations and bibliography
- [ ] Final PDF compilation

---

## TECHNICAL REFERENCE

### Key Folders
- `CODE/prepare tables/` - R scripts generating .tex tables
- `CODE/prepare figures/` - R scripts generating .png figures
- `OUTPUT/tables/` - Generated .tex files (19 total)
- `OUTPUT/figures/` - Generated .png files (25 total)
- `PAPER/` - LaTeX source files

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
2. **Stage 1:** Test LaTeX compilation of tables
3. **Stage 5:** Update paper methodology section based on RDD review findings
4. **Stage 5:** Final paper polish
5. **Optional:** Create formal DiD analysis script if switching identification strategy

---

*End of CLAUDE.md*
