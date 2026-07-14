# PDF Review Remediation Tracker

Review baseline: 66-page PDF, 2026-07-14. Statuses below distinguish a corrected item from a limitation retained transparently in the manuscript.

| Review scope | Status | Resolution or documented limitation | Evidence |
|---|---|---|---|
| pp. 1--5: title, abstract, terminology, date, roadmap | Resolved | Title/date/keywords corrected; abstract now names first differences as primary and RDD as sensitive supporting evidence. | `main.tex`, `Introduction.tex` |
| pp. 2--5: costs, mechanisms, causal diagram | Resolved | Costs are described as non-comparable descriptive magnitudes; mechanisms are hypotheses; dashed selection arrow added. | `Introduction.tex`, `Background.tex` |
| pp. 8--12: historical figures and timing | Partially resolved | Law (1995), decree/implementation (1996), and later entry (2005) are now distinguished. Figure-note revisions remain subject to final visual check. | `Background.tex`, `DiD_main.R` |
| pp. 14--17: COG provenance and commune history | Resolved with limitation | Official INSEE COG 1999 provenance is documented. Official history flags are audited and an exclusion sensitivity is reported; a full value-level historical harmonization remains a documented pre-submission task. | `Data.tex`, `commune_history_estimate_sensitivity.md` |
| pp. 18--25: main design and timing | Resolved | Main design is an early-versus-later first difference; the one pre-election limitation and 1995 timing ambiguity are explicit. | `DID.tex`, `empirical_contract.md` |
| pp. 26--35: spatial RDD | Resolved with limitation | RDD is supporting evidence; balance failures, fixed-effect sensitivity, and narrow-band imprecision are stated. | `Spatial.tex`, `robustness.tex` |
| pp. 36--39: border sample and Tables 9--10 | Resolved | Commune, pair, and commune-pair counts are separated; Table 9 escapes LaTeX cells; border estimates are corroborative. | `border_muni.tex`, `balancing_tests.R`, `border_muni_results.tex` |
| pp. 40--43: robustness, randomization, heterogeneity | Resolved | Doughnut values/inference corrected. Randomization and heterogeneity are excluded pending independently auditable redesign, rather than retained as invalid robustness evidence. | `robustness.tex`, `Appendices.tex` |
| pp. 44--46: mechanisms and conclusion | Partially resolved | Mechanisms are framed as compatible hypotheses. Final proofread of discussion/conclusion follows the rebuilt evidence. | `Introduction.tex`, `Discussion.tex`, `main.tex` |
| pp. 46--49: bibliography | In progress | DOI fields, author parsing, tracking URL, malformed DOI, and IGN online citation corrected; biber build is the final gate. | `references.bib` |
| pp. 50--61: appendices, matching, 1995 signal | Resolved | Hard-coded divergent tables are disabled. Generated matching tables share one sample; 1995 is a timing diagnostic. | `Appendices.tex`, `annex_matching.R`, `annex_main_results_1995.R` |
| pp. 62--66: randomization and heterogeneity | Resolved by removal | The existing implementations fail audit requirements (assignment/matching of IDs and out-of-sample CATE construction). They are not presented in the submission draft. | `Appendices.tex`, review record |
| Full PDF: layout, links, citations, warnings | Resolved | latexmk produced a 56-page PDF with no unresolved citations/references, no oversized float, and no material table/bibliography overflow. Contact sheets and targeted inspections covered all pages, including Tables 3, 10--11, 17--19 and the revised appendix figures. | main.pdf, /tmp/zrr-latex-final.log |

## Remaining Pre-submission Limits

- The commune-history audit supports an exclusion sensitivity, not a full value-level
  harmonization of all historical commune transformations.
- The primary first-difference comparison has one clean pre-treatment presidential
  election; it does not establish parallel trends.
- Randomization and heterogeneity analyses remain intentionally excluded until they
  are rebuilt with independently auditable assignment and out-of-sample procedures.
