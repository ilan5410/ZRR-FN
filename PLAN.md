# THESIS REPRODUCTION PROJECT - PLAN.md
**Project:** ZRR and Populist Vote - Academic Paper  
**GitHub Repository:** https://github.com/ilan5410/ZRR-FN  
**Last Updated:** 2026-02-02  

---

## 📋 GENERAL WORK PLAN

### Project Overview
This project analyzes the impact of the ZRR (Zone de Revitalisation Rurale) program on Front National vote shares using a Regression Discontinuity Design (RDD). The workflow involves:
- Data processing (R + Python)
- Statistical analysis and figure generation (R)
- Table generation exported to .tex format
- LaTeX document compilation for academic paper

### Main Objectives
1. **Fix .tex table formatting issues** (PRIORITY #1)
2. Reorganize code folder for reproducibility
3. Push clean codebase to GitHub
4. Finalize academic paper compilation

---

## 🎯 IMPLEMENTATION BY STAGES

### Stage 1: Fix .tex Table Formatting ✅ R SCRIPTS MODIFIED (PENDING TESTING)
**Goal:** Ensure R-generated .tex tables compile correctly in main LaTeX document

**IDENTIFIED ISSUES (from screenshots):**

#### Issue 1: Table 1 (descriptive_statistics.tex) - CRITICAL
**Problem:** Creates 2 separate tables instead of 1 table continued across 2 pages
**R Script:** `CODE/prepare tables/descriptive_statistics.R`
**Fix needed:** 
- Use `longtable` environment instead of splitting into separate tables
- Ensure continuous table numbering across pages
- Add table continuation header on second page

**Additional requirement:**
- Add footnote [a] with citation to Julia Cagé and Thomas Piketty (2023)
```latex
[a] In the socio-economic database assembled by Julia Cagé and Thomas Piketty (2023), 
the average income per municipality is defined as the total income reported on tax 
declarations (before any deductions or allowances) divided by the total number of 
inhabitants (including children).
Source: https://www.unehistoireduconflitpolitique.fr/glossaire.html, the website 
associated with the book by Julia Cagé and Thomas Piketty (2023): Une histoire du 
conflit politique. Élections et inégalités sociales en France, 1789–2022, Paris, Le Seuil.
```

#### Issue 2: Table 3 (DID_results.tex) - CRITICAL
**Problem:** Table extends beyond page margins (horizontal overflow)
**R Script:** `CODE/prepare tables/DID_results.R`
**Fix needed:**
- Reduce column widths / adjust font size
- Use `tabular*` or `tabularx` to fit page width
- Consider using `\resizebox` or `\scalebox` if necessary
- Add `\small` or `\footnotesize` for text

#### Issue 3: Table 6 (main_results_diff_specifications.tex) - CRITICAL
**Problem:** Table extends beyond page margins (horizontal overflow)
**R Script:** `CODE/prepare tables/main_results_diff_specifications.R`
**Fix needed:**
- Same as Issue 2 (width management)
- Potentially abbreviate column headers
- Adjust spacing between columns

#### Issue 4: Table 8 (balancing_tests.tex) - CRITICAL
**Problem:** Variable names with excessive parentheses run off page
**R Script:** `CODE/prepare tables/balancing_tests.R`
**Fix needed:**
- Format variable names to wrap text properly
- Use `p{width}` column specifier for first column
- Consider multi-line variable names with `\parbox` or `\makecell`
- Clean up variable name formatting (too many nested parentheses)

#### Issue 5: Table 9 (border_muni_results.tex) - ENHANCEMENT
**Problem:** Too wide for portrait mode
**R Script:** `CODE/prepare tables/border_muni_results.R`
**Fix needed:**
- Automatically generate in landscape mode
- Use `\begin{landscape}...\end{landscape}` or `sidewaystable`
- Ensure table fits landscape page width

#### Issue 6: Table 10 (winsorizing_trimming_doughnut.tex) - CRITICAL
**Problem:** Table extends beyond page margins (horizontal overflow)
**R Script:** `CODE/prepare tables/winsorizing_trimming_doughnut.R`
**Fix needed:**
- Same as Issue 2 (width management)

#### Issue 7: Table 11 (heterogeneity_causal.tex) - CRITICAL
**Problem:** Table extends beyond page margins (horizontal overflow)
**R Script:** `CODE/prepare tables/heterogeneity_causal_fm.R`
**Fix needed:**
- Same as Issue 2 (width management)

---

**Implementation Strategy:**
1. **First:** Fix the width overflow issues (Tables 3, 6, 8, 10, 11)
   - Create a standardized LaTeX table wrapper function in R
   - Use `stargazer`, `xtable`, or `kableExtra` with proper width settings
   - Test compilation after each fix

2. **Second:** Fix Table 1 (longtable for page continuation)
   - Implement proper longtable environment
   - Add footnote

3. **Third:** Implement landscape mode for Table 9
   - Add landscape wrapper

4. **Testing approach:**
   - For each table fix: compile individual .tex file in minimal LaTeX document
   - Verify table fits within margins and looks professional
   - Then integrate into main.tex

**Common Solutions to Apply:**
- Use `\resizebox{\textwidth}{!}{...}` for tables that are slightly too wide
- Use `\footnotesize` or `\scriptsize` for tables with many columns
- Use `tabularx` with `X` columns for flexible width
- Ensure proper use of `booktabs` package for professional rules
- Add `\usepackage{pdflscape}` for landscape tables

---

### Stage 2: Code Reorganization for Reproducibility ⚠️ IN PROGRESS
**Goal:** Restructure CODE/ folder to be clean, documented, and reproducible

**Sub-tasks:**
- [x] Create clear README.md in CODE/ folder
- [x] Add dependencies list (R packages, Python libraries)
- [x] Verify all file paths are relative (not absolute) - Fixed master.R
- [x] Remove any hardcoded personal paths - Fixed in master.R (uses here::here())
- [ ] Ensure master.R runs entire pipeline cleanly
- [ ] Add comments/documentation to key scripts
- [ ] Create data pipeline flowchart
- [x] Fix hardcoded paths in Python scripts (created config.py, master.py)
- [ ] Test full pipeline from scratch

**Documentation Created:**
- `CODE/README.md` - Comprehensive documentation (how to run, folder structure, dependencies)
- `CODE/Python code/requirements.txt` - Python dependencies

---

### Stage 3: GitHub Push ⏳ PENDING
**Goal:** Push clean, reproducible code to https://github.com/ilan5410/ZRR-FN

**Sub-tasks:**
- [x] Create comprehensive .gitignore (exclude raw data if needed)
- [ ] Write project-level README.md
- [ ] Ensure sensitive data is not included
- [ ] Organize repository structure
- [ ] Initial commit and push
- [ ] Add license if applicable
- [ ] Create GitHub repository description

**Repository Structure:**
```
ZRR-FN/
├── README.md
├── CODE/
├── DATA/ (or link to data source)
├── OUTPUT/
└── .gitignore
```

---

### Stage 4: Paper Finalization ⏳ PENDING
**Goal:** Complete and polish the academic paper

**Sub-tasks:**
- [ ] Verify all figures render correctly
- [ ] Verify all tables render correctly (after Stage 1)
- [ ] Proofread all sections
- [ ] Check citations and bibliography
- [ ] Final PDF compilation
- [ ] Review formatting standards for target journal

---

## ✅ CHECKLIST

### Immediate Tasks (Current Session)
- [x] Identify specific .tex table formatting issues from screenshots
- [x] Document all 7 table problems in PLAN.md
- [x] Fix Table 3 (DID_results.tex) - horizontal overflow (R script modified)
- [x] Fix Table 6 (main_results_diff_specifications.tex) - horizontal overflow (R script modified)
- [x] Fix Table 8 (balancing_tests.tex) - variable name formatting (R script modified)
- [x] Fix Table 10 (winsorizing_trimming_doughnut.tex) - horizontal overflow (R script modified)
- [x] Fix Table 11 (heterogeneity_causal.tex) - horizontal overflow (R script modified)
- [x] Fix Table 1 (descriptive_statistics.tex) - longtable continuation + footnote (R script modified)
- [x] Fix Table 9 (border_muni_results.tex) - landscape mode (R script modified)
- [ ] Re-run R scripts to regenerate .tex files
- [ ] Test all fixed tables compile correctly
- [ ] Verify all tables look professional and fit on page

### Short-term Tasks (Next 1-2 Sessions)
- [x] Complete all table formatting fixes (R scripts modified)
- [x] Begin code reorganization
- [x] Write CODE/README.md
- [ ] Re-run R scripts to regenerate .tex files
- [ ] Test LaTeX compilation
- [ ] Fix Python hardcoded paths

### Medium-term Tasks (Next 3-5 Sessions)
- [ ] Complete code reorganization
- [ ] Test full reproducibility pipeline
- [ ] Prepare GitHub repository
- [ ] Push to GitHub

### Long-term Tasks
- [ ] Paper finalization
- [ ] Submission preparation

---

## 📊 PROGRESS TRACKING

### Overall Progress: 55%
- **Stage 1 (Table Formatting):** 70% (R scripts modified, pending regeneration and testing)
- **Stage 2 (Code Reorganization):** 70% (README created, R+Python paths fixed, master scripts created)
- **Stage 3 (GitHub Push):** 0%
- **Stage 4 (Paper Finalization):** 10% (structure exists, content needs polishing)

### Files Status
**Tables (19 total):**
- ✅ 7 table R scripts modified with formatting fixes
- ⏳ Need to re-run R scripts to regenerate .tex files
- ⏳ Need to test LaTeX compilation
- Location: `OUTPUT/tables/*.tex`

**Figures (25 total):**
- ✅ Generated (assumed working)
- Location: `OUTPUT/figures/*.png`

**Code Scripts:**
- ✅ Main pipeline exists (`master.R`)
- ⚠️ Reproducibility needs verification
- ⚠️ Documentation needed

---

## 🎬 NEXT ACTIONS

### Immediate (Next Session - Priority Order)
1. **Re-run table R scripts to regenerate .tex files**
   - Run `master.R` or individual table scripts
   - Verify .tex files are updated with formatting fixes
2. **Test LaTeX compilation**
   - Create minimal test document for each table
   - Verify tables fit within page margins
   - Check that notes wrap properly
3. **Verify landscape mode for Table 9**
   - Ensure `pdflscape` package is in LaTeX preamble
4. **Final verification in main document**
   - Compile full paper and check all tables render correctly

### Completed This Session
- [x] Created standardized `format_latex_table()` helper function in `prepare_tables.R`
- [x] Modified 7 R scripts with formatting fixes (resizebox, parbox notes, font sizes)
- [x] Added landscape mode support for Table 9
- [x] Added Julia Cagé & Piketty footnote to Table 1

---

## 📝 SESSION NOTES

### Session 1 - 2026-02-01
**Accomplishments:**
- ✅ Created PLAN.md structure
- ✅ Identified priority: .tex table formatting
- ✅ Analyzed 5 screenshots showing table rendering issues
- ✅ Documented 7 specific table problems with detailed fix requirements
- ✅ Created action plan with priority order for fixes

**Issues Identified:**
1. Table 1: Splits into 2 separate tables instead of continuing across pages
2. Tables 3, 6, 10, 11: Horizontal overflow beyond page margins
3. Table 8: Variable names with nested parentheses run off page
4. Table 9: Needs landscape mode implementation

**Next Steps:**
- Start implementing fixes beginning with horizontal overflow issues
- Create reusable R helper function for table formatting
- Test each fix individually before moving to next table

---

### Session 2 - 2026-02-02
**Accomplishments:**
- ✅ Created standardized `format_latex_table()` helper function in `CODE/prepare tables/prepare_tables.R`
  - Handles: resizebox wrapper, font sizing, landscape mode, notes width
- ✅ Fixed Table 3 (DID_results.R): Wrapped notes in `\parbox` for proper text wrapping
- ✅ Fixed Table 6 (main_results_diff_specifications.R): Added `format_latex_table()` call with resizebox
- ✅ Fixed Table 8 (balancing_tests.R): Changed column spec to `p{5.5cm}` for text wrapping, added resizebox
- ✅ Fixed Table 10 (winsorizing_trimming_doughnut.R): Added parbox notes and `format_latex_table()` call
- ✅ Fixed Table 11 (heterogeneity_causal_fm.R): Added parbox notes and `format_latex_table()` call
- ✅ Fixed Table 1 (descriptive_statistics.R): Removed extra `\end{table}`, added Julia Cagé & Piketty footnote
- ✅ Fixed Table 9 (border_muni_results.R): Added landscape mode via `format_latex_table()` call

**Files Modified:**
1. `CODE/prepare tables/prepare_tables.R` - Added `format_latex_table()` and `convert_to_longtable()` helper functions
2. `CODE/prepare tables/DID_results.R` - Notes wrapped in parbox
3. `CODE/prepare tables/main_results_diff_specifications.R` - Added format_latex_table() call
4. `CODE/prepare tables/balancing_tests.R` - p{5.5cm} column, resizebox wrapper
5. `CODE/prepare tables/winsorizing_trimming_doughnut.R` - Parbox notes, format_latex_table()
6. `CODE/prepare tables/heterogeneity_causal_fm.R` - Parbox notes, format_latex_table()
7. `CODE/prepare tables/descriptive_statistics.R` - Fixed table combination, added Cagé-Piketty footnote
8. `CODE/prepare tables/border_muni_results.R` - Landscape mode, parbox notes

**Technical Notes:**
- Helper function `format_latex_table()` post-processes stargazer output to add:
  - `\resizebox{\textwidth}{!}{...}` wrapper around tabular
  - Font size changes (footnotesize)
  - Landscape environment wrapper
  - Notes formatting with parbox
- LaTeX preamble must include `\usepackage{pdflscape}` for landscape tables
- LaTeX preamble must include `\usepackage{graphicx}` for resizebox

**Issues Encountered:**
- None significant

**Next Steps:**
1. Re-run R scripts to regenerate .tex files with new formatting
2. Test LaTeX compilation of each table
3. Verify all tables fit within page margins
4. Check main document compiles correctly with all tables

**Stage 2 Progress (same session):**
- ✅ Created `CODE/README.md` with comprehensive documentation
- ✅ Created `CODE/Python code/requirements.txt` for Python dependencies
- ✅ Fixed hardcoded path in `master.R` - now uses `here::here()` package
- ⏳ Python scripts still have hardcoded paths (need separate fix)

---

## 🔧 TECHNICAL NOTES

### R Packages Used (Known)
- stargazer (table generation)
- kableExtra (table formatting)
- dplyr, tidyr (data manipulation)
- To be fully documented during code review

### Python Libraries Used (Known)
- Listed in `CODE/Python code/` scripts
- To be documented during code review

### LaTeX Packages Required (for table fixes)
- `\usepackage{graphicx}` - Required for `\resizebox` command
- `\usepackage{pdflscape}` - Required for landscape tables (Table 9)
- `\usepackage{booktabs}` - For professional table rules
- `\usepackage{threeparttable}` - For table footnotes
- `\usepackage{makecell}` - For multi-line cells

### Helper Functions Added to prepare_tables.R
1. `format_latex_table(tex_file, use_resizebox, font_size, use_landscape, notes_width)`
   - Post-processes stargazer output
   - Adds resizebox wrapper
   - Handles landscape mode
   - Fixes notes formatting
2. `convert_to_longtable(tex_file, footnote_text)`
   - Converts split tables to longtable format
   - Adds custom footnotes

---

## 💡 IMPORTANT REMINDERS

1. **Always update this PLAN.md at the end of each session**
2. **Make backups before major restructuring**
3. **Test table compilation after each fix**
4. **Document solutions for reproducibility**
5. **Keep the GitHub repository public/private as appropriate**

---

## 🎯 INSTRUCTIONS FOR CLAUDE

**At the start of each session:**
- Read this PLAN.md file completely
- Review "NEXT ACTIONS" section
- Check "Progress Tracking" to understand current state
- Continue from where previous session left off

**During each session:**
- Update "SESSION NOTES" with accomplishments
- Mark completed checklist items with ✅
- Note any issues encountered
- Update progress percentages

**At the end of each session:**
- Update "NEXT ACTIONS" section
- Update "Progress Tracking" percentages
- Add session summary to "SESSION NOTES"
- Update "Last Updated" date at top
- Save this file

---

*End of PLAN.md - Ready for implementation*