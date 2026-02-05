# Plan to Address TODOs

**Created:** 2026-02-06

This document outlines the plan to systematically address all TODOs and reviewer comments identified in the LaTeX source files.

---

## Phase 1: Quick Fixes (Estimated: 1 session)

### 1.1 Remove Duplicate Titles from Figure Captions

**Files to modify:**
- Background.tex (lines 107, 154)
- EvolutionFN.tex (lines 17, 29)
- robustness.tex (lines 20, 63)

**Approach:**
1. Check each figure PNG to confirm it has an embedded title
2. Update captions to remove redundant descriptive text
3. Keep short, clean captions that complement (not duplicate) the figure

**Example transformation:**
```latex
% Before
\caption{TODO: remove double title. Mean FN by Typologie Group for Each Year}

% After
\caption{Mean FN Vote Share by Urbanization Category, 1988-2007}
```

### 1.2 Add Missing IGN Reference

**File:** Data.tex (line 3)

**Action:** Add citation for IGN satellite data:
```latex
Using satellite data from the National Geographic Institute (IGN) \citep{IGN2020},
```

Add to references.bib:
```bibtex
@misc{IGN2020,
  author = {{Institut national de l'information géographique et forestière}},
  title = {BD TOPO® - Base de données topographiques},
  year = {2020},
  url = {https://geoservices.ign.fr/bdtopo},
  note = {Accessed: 2024}
}
```

---

## Phase 2: Content Updates (Estimated: 1-2 sessions)

### 2.1 Uncomment and Update Appendix Table (tab:1988-2002)

**File:** Appendices.tex

**Steps:**
1. Remove `\mycomment{` wrapper from lines 7-53
2. Verify table compiles correctly
3. Check if data needs updating

### 2.2 Update DID.tex Paragraph (Line 42)

**Current issue:** Paragraph references commented-out appendix table and may not reflect current results.

**Steps:**
1. After uncommenting tab:1988-2002, review the t-test results
2. Rewrite paragraph to:
   - Accurately describe parallel trends assumption
   - Reference the appendix table correctly
   - Update any outdated claims about socioeconomic changes

**Draft revision approach:**
```
The parallel trends assumption requires that treated and control municipalities
would have followed similar voting trajectories absent the ZRR program. Several
factors challenge this assumption: (1) selection criteria correlated with voting
trends (e.g., population density, agricultural employment); (2) differential
socioeconomic evolution between groups, as documented in Appendix Table
\ref{tab:1988-2002}; and (3) potential political manipulation in post-2004
county assignments.
```

### 2.3 Update Discussion.tex Paragraph (Line 26)

**Steps:**
1. Review current absolute_vote.tex results:
   - Check coefficient signs and significance across bandwidths
   - Note any changes from previous sample
2. Rewrite paragraph to match current findings
3. Update conclusion about numerator vs denominator effects

**Key questions to answer:**
- Is the effect on absolute votes now significant?
- Does the effect vary by bandwidth?
- What does this imply for the mechanism (diminished support vs. mobilization)?

---

## Phase 3: Structural Revisions (Estimated: 2-3 sessions)

### 3.1 Restructure DID Section (YS Comment, Line 3)

**Goal:** Lead with Figure 7, build argument around it.

**Proposed structure:**
1. Open with Figure 7 and key observation
2. Explain methodology (first-difference approach)
3. Present Table results
4. Discuss parallel trends assumption and limitations
5. Address potential concerns (selection, manipulation)

### 3.2 Add Heterogeneity Section Conclusion (YS Comment, Line 3)

**File:** heterogeneity.tex

**Steps:**
1. Review causal forest results (heterogeneity_causal.tex)
2. Identify 2-3 key findings:
   - Which characteristics predict stronger/weaker treatment effects?
   - What story does this tell about the mechanism?
3. Write 1-2 paragraph conclusion
4. If no clear story emerges, prepare to move section to appendix

**Potential narrative angles:**
- Educational composition: Academic degree holders show different response
- Altitude/geography: Higher altitude municipalities respond differently
- Labor force participation: Differential effects by employment characteristics

### 3.3 Expand Randomization Discussion (YS Comment, robustness.tex Line 42)

**Options:**
A. **Expand in main text:**
   - Acknowledge 33% is arbitrary, report sensitivity to other percentages
   - Acknowledge spatial correlation difference between placebo and actual ZRR
   - Discuss implications for interpretation

B. **Move to appendix (recommended if space constrained):**
   - Keep brief mention in robustness section
   - Full discussion with additional robustness checks in appendix

---

## Phase 4: Figure Enhancements (Estimated: 1-2 sessions)

### 4.1 Add Density Plot to Figure 6 (YS Comment, EvolutionFN.tex Line 37)

**Goal:** Show where ZRR municipalities are situated on population distribution.

**Implementation:**
1. Modify R script: `CODE/prepare figures/FN_versus_pop.R`
2. Add density curve or rug plot for treated municipalities
3. Regenerate figure and copy to Latex folder

**R code addition (conceptual):**
```r
# Add to existing scatter plot
geom_density(data = df_treated, aes(x = log_pop),
             color = "red", alpha = 0.3, fill = "red")
```

### 4.2 Create ΔFN vs log(population) Figure (YS Comment, DID.tex Line 25)

**Goal:** Visualize change in FN support against population size.

**Implementation:**
1. Create new R script or add to existing
2. Calculate ΔFN = FN2002 - FN1988 (or appropriate years)
3. Create binned scatter plot similar to existing figures
4. Add to appropriate section

### 4.3 Investigate Non-linear CIs in Balancing Test (YS Comment, Spatial.tex Line 45)

**Steps:**
1. Review R code generating the balancing test figure
2. Check if CIs are correctly computed
3. If intentional (e.g., kernel-based), add explanation to notes
4. If error, fix and regenerate

---

## Phase 5: Minor Clarifications (Estimated: 1 session)

### 5.1 Spatial.tex (Line 5) - Canton Size Clarification

**Add text explaining:**
- Cantons contain a small number of municipalities (typically 5-15)
- This affects spatial analysis interpretation
- Treatment is assigned at canton level

### 5.2 DID.tex (Line 5) - Post-2004 Qualification Clarification

**Research needed:**
- Did any post-2004 counties qualify under 1995 criteria but weren't included?
- Why might they have been excluded initially?

### 5.3 Background.tex (Line 129) - Fence Density Clarification

**Status:** Author noted change to "sq km per agricultural land" is feasible but costly.

**Decision needed:** Proceed with change or add footnote explaining current methodology.

---

## Implementation Order

### Session A: Quick Fixes + Uncomment Appendix
1. Fix all 6 figure captions
2. Add IGN reference
3. Uncomment tab:1988-2002 in Appendices.tex
4. Recompile and verify

### Session B: Content Updates
1. Update DID.tex paragraph (2.2)
2. Review absolute_vote results
3. Update Discussion.tex paragraph (2.3)
4. Recompile and verify

### Session C: Structural Revisions
1. Restructure DID section (3.1)
2. Write heterogeneity conclusion (3.2)
3. Expand randomization discussion (3.3)

### Session D: Figure Enhancements
1. Add density plot to Figure 6 (4.1)
2. Create ΔFN figure (4.2)
3. Fix balancing test CIs if needed (4.3)

### Session E: Final Polish
1. Minor clarifications (5.1-5.3)
2. Full proofread
3. Final compilation check

---

## Tracking

| Task | Status | Notes |
|------|--------|-------|
| 1.1 Figure captions | Pending | |
| 1.2 IGN reference | Pending | |
| 2.1 Uncomment appendix | Pending | |
| 2.2 DID paragraph | Pending | |
| 2.3 Discussion paragraph | Pending | |
| 3.1 DID restructure | Pending | Requires author decision |
| 3.2 Heterogeneity conclusion | Pending | |
| 3.3 Randomization discussion | Pending | |
| 4.1 Density plot | Pending | R code modification |
| 4.2 ΔFN figure | Pending | New R script |
| 4.3 CI investigation | Pending | |
| 5.1-5.3 Clarifications | Pending | |
