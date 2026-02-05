# TODOs and Reviewer Comments

**Generated:** 2026-02-06

This document lists all TODOs and reviewer comments (marked with `[YS:]`) found in the LaTeX source files.

---

## Table of Contents

1. [Figure Caption Fixes (6 items)](#1-figure-caption-fixes)
2. [Content Updates Required (3 items)](#2-content-updates-required)
3. [Reviewer Comments - Content/Structure (8 items)](#3-reviewer-comments---contentstructure)
4. [Missing References (1 item)](#4-missing-references)

---

## 1. Figure Caption Fixes

These are straightforward fixes — the figures have captions that include titles duplicated from the figure content itself.

| # | File | Line | Caption to Fix |
|---|------|------|----------------|
| 1.1 | Background.tex | 107 | "TODO: remove double title. Mean FN by Typologie Group for Each Year" |
| 1.2 | Background.tex | 154 | "TODO: remove double title. Map of the municipalities in the ZRR program" |
| 1.3 | EvolutionFN.tex | 17 | "TODO: remove double title. Nonparametric Effect of several Locality Characteristics, as of 2002 on Support for FN over Time" |
| 1.4 | EvolutionFN.tex | 29 | "TODO: remove double title. Population size and FN voting" |
| 1.5 | robustness.tex | 20 | "TODO: remove double title. Local Linear Regressions with varying bandwidth: FN Share of vote in 2002" |
| 1.6 | robustness.tex | 63 | "TODO: remove double title. Permutation Distributions of Treatment Effects by Bandwidth" |

**Action:** Check each figure PNG file to see if it contains an embedded title, then either:
- Remove the title from the PNG (regenerate the figure), OR
- Remove the descriptive part from the LaTeX caption, keeping only "Figure X: [short title]"

---

## 2. Content Updates Required

These TODOs indicate that the text needs to be updated to match current results or add missing content.

### 2.1 DID.tex (Line 42)
> `[TODO: update this paragraph with new references]`

**Context:** The paragraph discusses parallel trends assumption and references `\ref{tab:1988-2002}` which points to a commented-out appendix table. The text mentions:
- Selection criteria potentially associated with voting trends
- Reference to Figure `\ref{fig:pop_vs_FN}` (population vs FN voting)
- Reference to Appendix `\ref{tab:1988-2002}` (t-test of socioeconomic changes 1988-2002)

**Action:**
1. Uncomment the `tab:1988-2002` appendix table in Appendices.tex (lines 7-53 are inside `\mycomment{}`)
2. Update the paragraph text to align with current results
3. Add any new relevant references

### 2.2 Discussion.tex (Line 26)
> `[TODO: The results changed with the "new" sample, need to adapt this paragraph]`

**Context:** Discusses Table `\ref{tab:absolute-vote}` results about effect on absolute number of FN votes. Current text says "no significant effect" but notes this may have changed with the new sample.

**Action:**
1. Review current absolute_vote.tex results
2. Rewrite paragraph to accurately describe current findings
3. Update the conclusion about numerator vs denominator effects

### 2.3 Spatial.tex (Line 140) — Commented out
> `%\textbf{TODO: add RDD graphs in the Appendix}`

**Context:** This is commented out, suggesting RDD graphs may already be added or this task was deprioritized.

**Action:** Verify if RDD graphs are in appendix; if not, decide whether to add them.

---

## 3. Reviewer Comments - Content/Structure

These are comments from reviewer "YS" requiring responses or revisions.

### 3.1 DID.tex (Line 3)
> `[YS: Let's discuss how to redo this subsection. I think we should start by presenting Figure 7]`

**Action:** Restructure the DID section to lead with Figure 7, then build the argument around it.

### 3.2 DID.tex (Line 5)
> `[YS: do we know if none of them qualified based on the earlier criteria?]`

**Context:** About counties entering after 2004 — whether any qualified earlier but weren't included.

**Action:** Research and add clarification about whether post-2004 counties could have qualified under 1995 criteria.

### 3.3 DID.tex (Line 25)
> `[YS: I would add here a discussion on the new figure, with dFN over logpopulation. Perhaps there should also be a figure dFN against density of canton]`

**Action:**
1. Create/reference figure showing ΔFN vs log(population)
2. Consider creating figure showing ΔFN vs canton density

### 3.4 EvolutionFN.tex (Line 37)
> `[YS: Figure 6 is very interesting and gives a nuanced picture. It looks like the populist turn was particularly strong in middle-sized localities and not in the smallest rural places. Where, approximately on the support of these figures are the ZRR places situated? Could we somehow add to these figures the distribution of treated municipalities (e.g., a density plot)?]`

**Action:** Add density plot of treated municipalities to Figure 6 (FN_versus_pop.png) to show where ZRR places are situated on the population distribution.

### 3.5 Spatial.tex (Line 5)
> `[YS: we'll need to elaborate here. Specifically, mention that the cantons have a small number of municipalities]`

**Action:** Add text explaining that cantons contain a small number of municipalities, which affects the spatial analysis.

### 3.6 Spatial.tex (Line 45)
> `[YS: why are the CI non linear? not sure]`

**Context:** About the balancing test figure for FN 1988 vote share — confidence intervals appear non-linear.

**Action:** Investigate why CIs are non-linear and either fix the visualization or add explanation.

### 3.7 heterogeneity.tex (Line 3)
> `[YS: This part needs a conclusion, and also a clearer motivation. Think of a concrete story that it tells. If it's hard to make up something, then this at best belongs in an appendix]`

**Action:**
1. Add conclusion to heterogeneity section
2. Strengthen motivation with a concrete narrative
3. OR move to appendix if no clear story emerges

### 3.8 robustness.tex (Line 42)
> `[YS: This needs to be developed further. It could be that in any case it is better to place it in an appendix. There are two issues here: (i) 33% is arbitrary (ii) the counties that receive a placebo treatment don't have the same spatial correlation that characterizes the ZRR assignment. it will be a pain to try and replicate this process, but at least the difference should be acknowledged]`

**Action:**
1. Acknowledge the 33% is arbitrary and discuss sensitivity
2. Acknowledge that placebo counties lack the spatial correlation of actual ZRR assignment
3. Consider moving to appendix with expanded discussion

---

## 4. Missing References

### 4.1 Data.tex (Line 3)
> `[YS: reference]`

**Context:** Missing reference for IGN (National Geographic Institute) satellite data source.

**Action:** Add proper citation for IGN data source.

---

## 5. Other Notes

### 5.1 Background.tex (Line 129)
> `[YS: how hard would it be to change it to sq km per agricultural land? IP: not straightforward at all, feasible but costly]`

**Status:** Author (IP) has responded that this change is feasible but costly. Decision needed on whether to proceed.

---

## Priority Ranking

### High Priority (blocking issues)
1. **2.1** - DID.tex paragraph update + uncomment tab:1988-2002
2. **2.2** - Discussion.tex paragraph update for absolute_vote results

### Medium Priority (reviewer requests)
3. **3.1** - Restructure DID section around Figure 7
4. **3.7** - Add conclusion/motivation to heterogeneity section
5. **3.8** - Expand randomization discussion or move to appendix
6. **3.4** - Add density plot to Figure 6

### Low Priority (cosmetic/minor)
7. **1.1-1.6** - Remove duplicate titles from figure captions
8. **4.1** - Add IGN reference
9. **3.2, 3.3, 3.5, 3.6** - Minor clarifications and additions
