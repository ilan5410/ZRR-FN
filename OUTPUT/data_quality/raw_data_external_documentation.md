# Raw-Data External Documentation Check

Generated: 2026-07-14

This note supplements `raw_data_provenance.csv`. The local repository still does not contain original download URLs for most raw files, but an external documentation pass recovered several official anchors that should guide the next reproducibility cleanup.

## Official Sources Located

- INSEE Code officiel geographique (COG): the observed columns in `france1999.dbf` (`ACTUAL`, `CHEFLIEU`, `CDC`, `RANG`, `REG`, `DEP`, `COM`, `AR`, `CT`, `MODIF`, `POLE`, `NCC`, `NCCCT`) match INSEE COG commune-history layouts. INSEE documentation defines `RANG` as the number of canton fractions plus one for multi-canton communes and describes commune-history files back to 1943. Source checked: https://www.insee.fr/fr/information/2560615
- INSEE COG dissemination: data.gouv records that INSEE makes available codes and labels for communes, cantons, arrondissements, departments, regions, and related geographies each year. Source checked: https://www.data.gouv.fr/datasets/code-officiel-geographique-cog
- Multi-canton/pseudo-canton convention: newer INSEE/IGN documentation states that multi-canton communes can receive pseudo-canton codes, which supports treating split-canton rows cautiously rather than forcing a unique canton assignment. Sources checked: https://www.insee.fr/fr/information/5057840 and https://bdtopoexplorer.ign.fr/commune
- ZRR legal timing: Vie Publique records that ZRR were instituted in 1995 by articles 42 and 52 of law no. 95-115 of February 4, 1995. Source checked: https://www.vie-publique.fr/rapport/34345-zones-de-revitalisation-rurale-zrr
- ZRR defining decree: Legifrance records decree no. 96-119 of February 14, 1996 defining the zones de revitalisation rurale, published in JORF no. 39 on February 15, 1996. Source checked: https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000000191822
- Election data archive: the Interior Ministry maintains an official election-results archive. Source checked: https://www.archives-resultats-elections.interieur.gouv.fr/
- Election data on data.gouv: data.gouv lists presidential election datasets and links commune-level 2002 first-round and second-round results published by the Interior Ministry; older 1965-2012 files also exist but some early first-round data are not full commune coverage. Source checked: https://www.data.gouv.fr/datasets/elections-presidentielles-1965-2012-1

## What This Resolves

- `france1999.dbf` is very likely derived from an INSEE COG commune-history/commune update file, not an arbitrary GIS file.
- The presence of multi-canton rows is not automatically an error; INSEE documents multi-canton fractions/pseudo-canton conventions.
- The conservative cluster convention introduced in the pipeline is appropriate until the exact 1999 COG archive and extraction rule are recovered.
- The 1995 election coefficient should not be described as clean post-treatment evidence because zone definition/implementation documentation points to 1996.

## Still Missing

- Exact original URL or archive for the checked-in `france1999.dbf`.
- Exact original URLs for the local Excel/CSV election, census, education, income, turnout, JOAFE, and ZRR files.
- A formal official commune-history crosswalk applied to every election/control year.
- Confirmation whether the local election files already harmonize historic communes to a common vintage.

## Manuscript Implication

The manuscript can now state that the code treats commune/canton geography conservatively and that the canton bridge appears consistent with INSEE COG structure. It should not yet claim a fully verified historical canton mapping or fully harmonized commune-code panel.
