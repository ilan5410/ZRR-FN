# Raw-Data External Documentation Check

Generated: 2026-07-14

This note supplements `raw_data_provenance.csv`. The local repository still does not contain original download URLs for most raw files, but an external documentation pass recovered several official anchors that should guide the next reproducibility cleanup.

## Official Sources Located

- INSEE Code officiel geographique (COG) 1999: the checked-in `DATA/raw data/france1999.dbf` is byte-for-byte identical to INSEE's official `france1999-dbf.zip` archive. The INSEE page describes the file as all communes existing on January 1, 1999 or having existed since 1943, and states that communes split across cantons are represented by one row per canton fraction plus one `canton non precise` row. Sources checked: https://www.insee.fr/fr/information/2560686 and https://www.insee.fr/fr/statistiques/fichier/2560686/france1999-dbf.zip
- INSEE current commune-history files: the 2026 COG provides `v_commune_depuis_1943.csv` and `v_mvt_commune_2026.csv`, which can validate commune-code periods and provide before/after movement records for a future harmonization layer. Sources checked: https://www.insee.fr/fr/statistiques/fichier/8740222/v_commune_depuis_1943.csv and https://www.insee.fr/fr/statistiques/fichier/8740222/v_mvt_commune_2026.csv
- INSEE COG dissemination: data.gouv records that INSEE makes available codes and labels for communes, cantons, arrondissements, departments, regions, and related geographies each year. Source checked: https://www.data.gouv.fr/datasets/code-officiel-geographique-cog
- Multi-canton/pseudo-canton convention: newer INSEE/IGN documentation states that multi-canton communes can receive pseudo-canton codes, which supports treating split-canton rows cautiously rather than forcing a unique canton assignment. Sources checked: https://www.insee.fr/fr/information/5057840 and https://bdtopoexplorer.ign.fr/commune
- ZRR legal timing: Vie Publique records that ZRR were instituted in 1995 by articles 42 and 52 of law no. 95-115 of February 4, 1995. Source checked: https://www.vie-publique.fr/rapport/34345-zones-de-revitalisation-rurale-zrr
- ZRR defining decree: Legifrance records decree no. 96-119 of February 14, 1996 defining the zones de revitalisation rurale, published in JORF no. 39 on February 15, 1996. Source checked: https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000000191822
- Election data archive: the Interior Ministry maintains an official election-results archive. Source checked: https://www.archives-resultats-elections.interieur.gouv.fr/
- Election data on data.gouv: data.gouv lists presidential election datasets and links commune-level 2002 first-round and second-round results published by the Interior Ministry; older 1965-2012 files also exist but some early first-round data are not full commune coverage. Source checked: https://www.data.gouv.fr/datasets/elections-presidentielles-1965-2012-1

## What This Resolves

- `france1999.dbf` is verified as the official INSEE COG 1999 all-communes DBF archive.
- The presence of multi-canton rows is not an error; INSEE's 1999 documentation explicitly describes multi-canton fraction rows plus a `canton non precise` row.
- The conservative split/missing canton cluster convention introduced in the pipeline is appropriate because it respects the official multi-row structure without pretending every commune has a unique historical canton row.
- The 1995 election coefficient should not be described as clean post-treatment evidence because zone definition/implementation documentation points to 1996.

## Still Missing

- Exact original URLs for the local Excel/CSV election, census, education, income, turnout, JOAFE, and ZRR files.
- A formal official commune-history crosswalk applied to every election/control year.
- Confirmation whether the local election files already harmonize historic communes to a common vintage.

## Manuscript Implication

The manuscript can now state that the 1999 canton bridge is INSEE COG 1999 and that the code treats its official multi-canton structure conservatively. It should not yet claim a fully harmonized commune-code panel until `v_mvt_commune_2026.csv` is used to build and test an explicit movement-based crosswalk.
