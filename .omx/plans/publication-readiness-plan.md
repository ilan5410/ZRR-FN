# Publication Readiness Plan

Date: 2026-07-14
Status: Awaiting Ilan approval before implementation
Objective: Make the ZRR and populist vote paper publishable by locking a defensible empirical claim, verifying the raw-data geography, rebuilding the evidence package, and rewriting the manuscript around what the evidence can actually support.

## Requirements Summary

- Preserve the completed commune-merge audit and post-merge reproduction work as the empirical foundation.
- Resolve the remaining external-data documentation gap: where raw files were downloaded, what vintage/geography they encode, and whether commune/canton codes changed over time.
- Decide and document the paper's primary identification strategy before polishing the prose.
- Rebuild the empirical package from one canonical code path, with auditable outputs and exact model traceability.
- Reframe the manuscript so every claim matches the validated design, estimates, and limitations.
- Produce a clean, reproducible submission package.

## Guiding Principles

1. Identification before elegance: no manuscript polish should outrun the strongest defensible causal design.
2. Auditability over convenience: every dataset, merge, exclusion, and model must be traceable.
3. Conservative causal language: if a diagnostic is weak, the text says so plainly.
4. One source of truth: one branch, one pipeline, one final set of tables and figures.
5. Reviewer-first framing: anticipate the objections an applied micro referee will raise.

## Decision Drivers

- The current standard RDD evidence is fragile: standard `rdrobust` is null, significance depends on department fixed effects, and the effect disappears at 1 km.
- The merge/canton audit is now much stronger, but external raw-data provenance and official commune-history validation remain incomplete.
- A DiD-like early-vs-later comparison appears more promising, but the paper must not overclaim parallel trends if only one pre-treatment presidential election is available.

## Implementation Plan

### Phase 0 - Integrate Into One Publication Branch

Goal: Put the project on one clean publication-candidate branch before substantive edits.

Tasks:
- Inspect the current original worktree and the isolated post-merge reproduction worktree.
- Preserve all dirty manuscript/user changes in the original worktree.
- Merge or cherry-pick the post-merge reproduction foundation into one canonical branch.
- Confirm the branch contains the audited merge code, reports, regenerated outputs, and compiled PDF state.
- Update `.omx/plans/` and project notes so the active plan and branch are unambiguous.

Acceptance criteria:
- One active branch is identified as the publication branch.
- No user edits are overwritten.
- `git status` is understood and documented.
- The post-merge audit commit is present or explicitly integrated.

### Phase 1 - Raw-Data Provenance and Historical Geography

Goal: Close the most important data credibility gap before new causal claims are written.

Tasks:
- Inventory every raw input used by `prepare_data`, Python preprocessing, tables, and figures.
- For each raw file, document source, download URL or archive source, date/vintage, geographic unit, key columns, and downstream consumers.
- Locate documentation for `france1999.dbf`, ZRR eligibility/treatment files, election data, population/socioeconomic files, and geographic/canton mapping.
- Verify whether `france1999.dbf` canton rows correspond to official canton fragments, pseudo-canton GIS artifacts, or duplicated commune geometries.
- Build or obtain an official commune-history/crosswalk check for commune codes used across election years and controls.
- Verify whether commune codes changed between the outcome years and whether the current pipeline harmonizes them correctly.
- Verify treatment timing: legal adoption date, effective treatment date, and whether post-2004 entrants qualified under earlier criteria.
- Document unresolved items as explicit assumptions with sensitivity checks.

Acceptance criteria:
- A provenance table exists for all raw files.
- The canton-commune mapping status is classified as verified, partially verified, or unresolved.
- Commune-code stability is checked against an official or clearly documented source.
- Any unresolved mapping risk has an exclusion or sensitivity design.

### Phase 2 - Freeze the Empirical Contract

Goal: Decide exactly what the paper estimates before rebuilding final tables.

Tasks:
- Define the primary estimand in plain language.
- Define the main sample: likely 1995 entrants vs later/not-yet-treated communes, pending timing verification.
- Define the outcome window: likely 1988-2002 presidential FN vote change, with caveats if no additional pre-periods are available.
- Define treatment, controls, fixed effects, clustering, weights, exclusions, and geographic restrictions.
- Classify all controls as pre-treatment, contemporaneous, or post-treatment; remove or demote post-treatment controls from primary specifications.
- Decide whether the primary design can honestly be called DiD. If only two election periods are central, describe it as an early-vs-later treated first-difference design unless the panel/timing structure justifies the DiD label.
- Decide the role of RDD: likely supporting/diagnostic rather than primary, unless rebuilt diagnostics materially strengthen it.
- Draft a short "empirical contract" note that future scripts and manuscript sections must follow.

Acceptance criteria:
- A written empirical contract exists.
- The main specification can be stated in one paragraph without contradiction.
- The manuscript no longer relies on an unsupported parallel-trends claim.
- RDD is framed according to its actual diagnostics.

### Phase 3 - Build the Definitive Evidence Package

Goal: Produce the final empirical tables/figures and robustness matrix from canonical scripts.

Tasks:
- Promote the exploratory DiD/alternative-identification scripts into publication scripts with clear inputs and outputs.
- Generate the main estimate table for the primary design.
- Generate a compact robustness matrix covering:
  - raw vs adjusted models
  - canton clustering and alternative cluster levels
  - split/missing canton exclusions
  - department fixed effects sensitivity
  - population and density functional forms
  - treatment-timing definitions
  - later-treated comparison definitions
  - geographic restrictions and no-epicenter sample
  - RDD bandwidth choices and standard `rdrobust`
  - absolute votes, turnout, and socioeconomic outcomes
- Add the requested visual diagnostics:
  - `dFN` vs log population
  - treated municipality density/distribution in the relevant figure
  - clearer balancing-test confidence intervals
- Produce an old-vs-corrected result comparison report where feasible.
- Save a model manifest that records script, input hash/path, sample, formula, clustering, output file, and key estimate for each final table/figure.

Acceptance criteria:
- All final tables/figures are generated by documented scripts.
- Each main result has a matching robustness/sensitivity row or documented reason for omission.
- The evidence package can be regenerated from a clean run.
- Fragile or null specifications are included rather than hidden.

### Phase 4 - Rewrite the Manuscript Around the Evidence

Goal: Make the paper read like a coherent publishable article rather than a collection of evolving analyses.

Tasks:
- Rewrite abstract and introduction around the final primary design and estimate.
- Rewrite the identification/results section in the new order:
  - institutional assignment and timing
  - data construction and merge/crosswalk validation
  - primary design
  - main results
  - robustness
  - supporting RDD/spatial evidence and limitations
  - mechanisms and interpretation
- Remove or soften "high internal validity" and any wording that overstates RDD credibility.
- Rewrite the parallel-trends/event-study paragraph so it states exactly what can and cannot be tested.
- Address the DID selection-bias comment directly.
- Update the absolute-vote discussion to match regenerated results.
- Move weak heterogeneity and randomization exercises to the appendix unless they become central and defensible.
- Resolve all remaining `[YS:]` comments and TODOs.
- Standardize voice, notation, labels, captions, and cross-references.

Acceptance criteria:
- Abstract, intro, results, discussion, and conclusion all describe the same estimand and identification strategy.
- No known TODO or `[YS:]` comment remains in main text.
- The limitations section acknowledges the true weak points: geography, RDD fragility, limited pre-period evidence, and treatment timing ambiguity if unresolved.
- PDF compiles cleanly.

### Phase 5 - Adversarial Referee Pass

Goal: Stress-test the paper before submission.

Tasks:
- Run a referee-style critique focused on:
  - endogenous ZRR selection
  - validity of later-treated controls
  - geographic confounding
  - commune-code changes and historical boundary changes
  - post-treatment controls
  - multiple testing and specification search
  - spatial correlation and clustering
  - external validity
- Convert each serious critique into either a manuscript paragraph, robustness test, appendix note, or explicit limitation.
- Review tables and figures for whether a skeptical reader can understand sample, units, timing, and standard errors without reading the code.
- Verify the bibliography: duplicate `Fetzer2019`, month warnings, unresolved references.

Acceptance criteria:
- A referee-risk memo exists.
- Every high-risk critique has a response path.
- No table/figure depends on unexplained coding conventions.
- Bibliography warnings are resolved or documented as harmless.

### Phase 6 - Reproducibility and Submission Package

Goal: Make the project credible to a journal, replication archive, or advisor review.

Tasks:
- Run full Python preprocessing, R pipeline, table/figure generation, and LaTeX compilation from the publication branch.
- Test from a clean clone or clean worktree where feasible.
- Update README/codebook instructions, including environment, R/Python dependencies, data availability, and expected outputs.
- Add or update data provenance documentation and model manifest.
- Ensure generated outputs are either committed intentionally or documented as reproducible artifacts.
- Prepare a submission checklist: target journal fit, abstract, keywords, JEL codes, cover letter skeleton, replication package notes.
- Commit with Lore protocol and push.

Acceptance criteria:
- Full pipeline and PDF build pass.
- Reproduction instructions are current.
- Git history contains clear Lore commits.
- Branch is pushed and ready for review.

## Verification Plan

- `git status` before and after every integration/edit phase.
- Merge QA scripts after any data-prep change.
- Full Python preprocessing run after provenance/crosswalk changes.
- Full R pipeline run after empirical script changes.
- LaTeX build after manuscript/table/figure changes.
- Targeted output checks for row counts, duplicate keys, model samples, and table/figure timestamps.
- Clean worktree or clean-clone reproduction before final push.

## Risks and Mitigations

- Risk: official canton/commune documentation cannot be found quickly.
  Mitigation: document the gap, use exclusion/sensitivity designs, and avoid claims that depend on unverified canton identity.

- Risk: the primary DiD-like design is not defensible after timing/crosswalk checks.
  Mitigation: downgrade the causal claim and reposition the paper around descriptive/institutional evidence plus transparent quasi-experimental limitations.

- Risk: additional pre-treatment election data are unavailable or costly to harmonize.
  Mitigation: avoid parallel-trends validation language; present 1988 as baseline balance/change evidence rather than a trend test.

- Risk: regenerated estimates differ from existing manuscript claims.
  Mitigation: update claims to match the regenerated evidence and include an old-vs-new comparison report.

- Risk: branch/worktree confusion causes loss of manuscript changes.
  Mitigation: inspect, stash/commit selectively if needed, and never overwrite user edits.

## Approval Gate

Implementation should begin only after Ilan approves this plan.

Recommended execution order after approval:

1. Integrate branches into one publication branch.
2. Close raw-data provenance and geography/crosswalk documentation.
3. Freeze empirical contract.
4. Rebuild final evidence package.
5. Rewrite manuscript.
6. Run adversarial referee pass.
7. Reproduce, commit, and push.

## Definition of Done

The project is ready for serious external review when:

- The paper has one defensible primary identification strategy.
- The raw-data and commune/canton merge story is documented.
- All final results are regenerated from canonical scripts.
- The manuscript's claims match the diagnostics.
- The PDF compiles cleanly.
- The replication instructions are current.
- The publication branch is committed and pushed.
