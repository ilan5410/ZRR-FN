# Relecture du PDF, page par page

## Périmètre de la revue

- PDF relu : `main.pdf`
- Version Git : `2d270cd`
- Compilation : 14 juillet 2026, 14:53 CEST
- Longueur : 66 pages
- SHA-256 : `ffb973b035376159a37de209d3edaac6037a9916e6cb7846939df46b4c169b73`
- Méthode : compilation complète avec `latexmk`, extraction du texte, rendu visuel des 66 pages en PNG, puis lecture croisée du fond et de la forme.
- Échelle de gravité : **critique** = erreur susceptible d'invalider ou de bloquer la soumission ; **majeur** = correction importante avant soumission ; **mineur** = correction éditoriale ou clarification ; **suggestion** = amélioration facultative.

Les numéros ci-dessous sont les numéros de page du PDF. Aucun fichier source n'a été modifié dans le cadre de cette revue.

## Priorités avant soumission

1. **Stabiliser le design principal.** Le texte oscille entre « Difference-in-Differences » et première différence avec une seule véritable élection pré-traitement. Il faut nommer exactement le design, expliciter ses hypothèses et éviter de présenter l'étude dynamique comme un test standard de tendances parallèles.
2. **Fixer la chronologie et les groupes.** Le manuscrit utilise alternativement 1995, 1996, « after 1995 », « after 2004 » et 2005. Il faut distinguer la loi, le décret, la mise en oeuvre et la vague de comparaison, puis employer la même convention partout.
3. **Recalibrer le spatial RDD.** Les tests d'équilibrage montrent plusieurs ruptures visibles, notamment pour la superficie, l'altitude, la densité et la distance à une agglomération. Le design spatial doit rester une preuve corroborative et les affirmations causales doivent refléter cette fragilité.
4. **Corriger les erreurs matérielles dans les résultats.** Les problèmes les plus urgents concernent les Tables 3, 8, 9, 10, 11, 12, 18, 19 et 20 : mauvais signe dans une note, mauvais millésime, variable dépendante erronée, échantillons incompatibles, libellés concaténés et conventions d'étoiles différentes.
5. **Mettre à jour la provenance des données.** La page 14 affirme encore que l'URL de `france1999.dbf` n'est pas retrouvée. C'est désormais faux : le fichier local a été vérifié comme identique au COG 1999 officiel de l'INSEE.
6. **Réparer la présentation.** La Table 9 est illisible ; les pages 38 et 63 sont des paysages sous-dimensionnés ; les pages 39, 42 et 43 sont presque vides ; plusieurs figures ont des axes ou notes incohérents.
7. **Nettoyer la bibliographie.** Plusieurs DOI sont préfixés deux fois, des URL de tracking apparaissent, certains auteurs sont mal formatés et plusieurs entrées sont incomplètes.

## Commentaires page par page

### Page 1

- **Fond — majeur.** L'abstract qualifie les résultats spatiaux de « smaller but consistent estimates » sans dire qu'ils dépendent fortement de la spécification, des effets fixes et de la bande passante. La dernière phrase est plus prudente, mais le résumé reste plus affirmatif que les diagnostics présentés ensuite.
- **Forme — mineur.** Corriger `Zone de Revitalization Rurale` en nom français officiel, harmoniser les capitales du titre (`dark Paths`, `state Policy`) et remplacer le mot-clé peu idiomatique `France rurality`.
- **Forme — majeur.** La date `August 2024` ne correspond plus à la version actuelle du papier.

### Page 2

- **Fond — majeur.** Le coût budgétaire de 100 puis 400 millions d'euros n'est pas défini : montant annuel ou cumulé, périmètre comparable ou non. Le lecteur ne peut pas interpréter l'ordre de grandeur.
- **Forme — mineur.** Corriger notamment `a French Enterprise-Zone` et `two potential mechanism`.
- **Forme — suggestion.** La longue note sous l'exergue surcharge la première page de texte.

### Page 3

- **Fond — critique.** Le groupe entré « only after 2004 » est présenté comme une comparaison naturelle, alors que les critères changent et qu'il n'existe qu'une vraie élection pré-traitement. `Natural comparison group` est trop fort.
- **Fond — critique.** Harmoniser `after 2004` avec l'élargissement daté de 2005 ailleurs dans le papier.
- **Forme — majeur.** Dans le schéma, la double flèche entre ZRR et résultats socioéconomiques suggère une causalité inverse. Si elle représente la sélection endogène, il faut l'indiquer explicitement.

### Page 4

- **Fond — majeur.** Le « state signaling » est une hypothèse interprétative, pas un mécanisme identifié. Le texte passe trop rapidement de l'absence d'effets économiques détectés à cette explication.
- **Forme — mineur.** `This state signaling effect could be the one that mitigates the populist support` est peu idiomatique.

### Page 5

- **Fond — majeur.** L'affirmation selon laquelle ZRR serait « the largest redistribution initiative for which electoral impacts have been measured » exige une comparaison documentée ou une formulation plus prudente.
- **Forme — majeur.** La feuille de route est erronée : elle annonce que la section 2 contient les données, alors que `Data` est la section 3 ; la numérotation annoncée des sections suivantes est également à revoir.
- **Forme — suggestion.** Stabiliser la référence `Cremaschi et al., n.d.` avant soumission.

### Page 6

- **Fond.** Rien à signaler sur le récit historique principal.
- **Forme — mineur.** Éviter la hiérarchie redondante `2. Background` puis `2.1 Background`.

### Page 7

- **Fond — suggestion.** L'extension du récit jusqu'aux élections de 2024 est intéressante mais éloigne du coeur causal 1995-2002 ; elle peut être raccourcie.
- **Forme.** Rien à signaler de bloquant.

### Page 8

- **Forme — critique.** La note de la Figure 1 ne décrit que l'élection présidentielle, alors que la figure contient aussi une série législative. Il faut définir séparément les deux séries.
- **Forme — mineur.** Les sources et la construction des taux devraient apparaître dans la note, pas seulement l'interprétation.

### Page 9

- **Forme — critique.** La note de la Figure 2 parle de `bars`, alors que le visuel est un graphique en lignes.
- **Forme — critique.** La note énumère 1988, 1995, 2002 et 2007, alors que la figure va jusqu'en 2022.
- **Forme — majeur.** La figure occupe peu d'espace et laisse une grande zone blanche ; elle pourrait être agrandie une fois la note corrigée.

### Page 10

- **Fond — suggestion.** La densité de haies peut capter une structure régionale large, pas uniquement le bocage. Il faut expliquer la variation identifiante et le rôle de cette variable dans les régressions.
- **Forme.** Rien à signaler de bloquant ; la note 15 est dense mais utile.

### Page 11

- **Fond — critique.** La phase initiale est datée `1996-2004`, alors que les groupes et figures parlent de traitement en 1995. Distinguer vote de la loi, décret, lancement effectif et codage empirique.
- **Fond — majeur.** La phrase selon laquelle les densités de 1990 et 1999 déterminent l'inclusion peut laisser croire que les deux critères s'appliquent simultanément à tous les groupes ; séparer vague initiale et extension.
- **Forme — mineur.** La grande note 17 contient une URL brute et coupe fortement le texte.

### Page 12

- **Fond — critique.** La note de la Figure 3 dit que les cantons noirs « entered ... in 1995 », en contradiction avec le lancement de 1996. Si 1995 signifie « définis par la loi », il faut le dire.
- **Forme — suggestion.** Les trois niveaux de gris sont difficiles à distinguer en impression et la légende est petite.

### Page 13

- **Fond — majeur.** `A particularly advantageous setting` est trop affirmatif au regard de l'endogénéité de l'affectation, du faible nombre de pré-périodes et de la dépendance aux effets fixes.
- **Fond — mineur.** Les montants du programme sont de nouveau présentés sans unité temporelle claire.
- **Forme.** Rien à signaler sur la mise en page.

### Page 14

- **Fond — critique.** Le paragraphe affirme que l'URL originale de `france1999.dbf` n'a pas été retrouvée. Cette phrase est obsolète : le fichier local est identique au fichier officiel INSEE COG 1999, et l'URL officielle est désormais documentée. Le passage doit être actualisé avant toute diffusion.
- **Fond — majeur.** La convention de cluster pour les communes multi-cantons est décrite, mais le panel de codes communaux n'est pas encore présenté comme harmonisé par un crosswalk historique complet. Conserver cette limite distincte de la provenance, désormais résolue.
- **Forme — mineur.** Stabiliser `commune`, `municipality`, `locality`, `canton` et `county`; `county` n'est pas une traduction satisfaisante de canton.

### Page 15

- **Fond.** Rien à signaler sur le message descriptif.
- **Forme — suggestion.** Les deux cartes sont très peu contrastées, les légendes sont minuscules et la comparaison 1988-2002 demande un effort excessif.
- **Forme — mineur.** Remplacer `locality level` par la terminologie commune/municipality utilisée ailleurs.

### Page 16

- **Fond — critique.** L'en-tête `Treated after 1995` ne correspond pas aux groupes définis ailleurs (`after 2004`, vague 2005). Le lecteur ne sait pas quelles communes sont incluses.
- **Fond — majeur.** Plusieurs unités sont ambiguës : `Population density` semble transformée sans l'indiquer ; `Population change in p.p.` doit être défini précisément ; les écarts-types de certaines parts méritent une vérification.
- **Forme.** Le tableau est lisible.

### Page 17

- **Fond — mineur.** `Harshest negative employment shock` est plus affirmatif que le tableau descriptif seul ; ajouter un test ou reformuler comme différence descriptive.
- **Forme — mineur.** Corriger `saw their population decreased` en `saw their population decrease` ou `saw their population decline`.
- **Forme — suggestion.** Remplacer l'URL brute Cagé-Piketty par une citation bibliographique propre.

### Page 18

- **Fond — majeur.** Le texte dit que `alpha_i` absorbe les différences à travers les départements. Avec l'indice `i`, il s'agit normalement d'un effet fixe municipal ; la notation et l'interprétation ne correspondent pas.
- **Fond — majeur.** Préciser si les élections présidentielles et européennes sont réellement comparables dans la même équation et quelles interactions absorbent leurs différences.
- **Forme — mineur.** Le panneau population utilise une échelle scientifique (`e-07`) difficile à lire ; reparamétrer l'unité.

### Page 19

- **Fond — suggestion.** La densité rouge des communes ZRR est superposée au vote sans axe ni échelle propre ; son amplitude n'est pas interprétable.
- **Forme — mineur.** Expliquer les annotations `r` et `n` dans la note et identifier explicitement la courbe rouge dans une légende.

### Page 20

- **Fond — suggestion.** Le placement des communes traitées sur la partie croissante de la relation est descriptif et ne constitue pas à lui seul un argument d'identification.
- **Forme — mineur.** Corriger `the linear specification obscure` en `obscures` et préférer `non-monotonic` à `non-monotonous`.
- **Forme — suggestion.** Grand blanc avant la section 5.

### Page 21

- **Fond — majeur.** L'équation (2) est une régression en première différence en coupe, pas un DiD canonique écrit comme interaction traitement x post dans un panel. Le titre et le vocabulaire doivent refléter le design exact.
- **Fond — majeur.** Le groupe tardivement traité n'est un contrefactuel crédible qu'avec des hypothèses explicites sur les différences de critères et de trajectoires ; `natural comparison group` doit être évité.
- **Forme — majeur.** Les occurrences de `counties` brouillent le niveau d'affectation ; utiliser `cantons` et `communes` sans traductions changeantes.

### Page 22

- **Fond.** La Figure 7 documente clairement la divergence descriptive entre groupes, mais ne valide pas seule la causalité.
- **Forme — suggestion.** Agrandir les libellés et remplacer les catégories techniques de légende par `Initial ZRR wave` et `Later ZRR entrants`.

### Page 23

- **Fond — critique.** La note de la Table 3 dit que des localités sont entrées dans ZRR en 1988. C'est une erreur factuelle : 1988 est la période de référence, pas le traitement.
- **Fond — critique.** La Table 3 affiche comme variable dépendante le vote FN en 2002 tout en présentant `Post x Treat` et en décrivant une première différence 1988-2002. Il faut vérifier la régression exacte et aligner titre, variable dépendante et note.
- **Forme — mineur.** Supprimer le doublon `Note:` / `Notes:` et harmoniser le niveau de clustering.

### Page 24

- **Fond — majeur.** L'argument de sélection ne permet pas d'affirmer que l'effet causal véritable est « at least as large as the estimate » sans hypothèses sur les inobservables. Présenter cela comme une direction possible du biais, pas comme une borne.
- **Fond — mineur.** La prudence sur le coefficient 1995 est appropriée.
- **Forme.** La Table 4 est propre et lisible.

### Page 25

- **Fond — majeur.** La ligne `ZRR launch` apparaît vers 1995 alors que la note rappelle un décret en 1996. Le graphique doit distinguer loi et mise en oeuvre.
- **Fond — majeur.** Le texte reconnaît qu'une seule vraie élection pré-traitement empêche un test standard de tendances parallèles ; cette limite doit apparaître dès le titre et le résumé du design.
- **Forme — mineur.** Remplacer `t-tests in Appendix 14` par `Table 14 in the Appendix`.

### Page 26

- **Fond — majeur.** L'équation (3) omet une interaction traitement x distance alors que les figures et certaines tables utilisent des pentes séparées. Vérifier et faire correspondre équation, code et tableaux.
- **Fond — majeur.** La notation est incohérente entre l'équation et le texte (`gamma X_i`, `gamma_k`, `epsilon_id`, `epsilon_ijdr`, département `d`).
- **Forme — mineur.** Corriger `allowing attributing` et stabiliser `county (canton)`.

### Page 27

- **Fond — majeur.** La note dit `three categories`, mais la légende en comporte davantage, y compris `Missing`.
- **Forme — mineur.** Les gris sont trop proches et la masse de points masque une partie de la géographie.
- **Forme — suggestion.** Ajouter un trait visible pour la frontière du programme.

### Page 28

- **Fond — majeur.** Le texte affirme qu'il n'y a pas de discontinuité conditionnelle, alors que les Figures 12a-12b montrent plusieurs ruptures visibles. Reformuler en diagnostic mixte et citer les covariables problématiques.
- **Fond — majeur.** Les coefficients annoncés « at the top of each figure » ne sont pas visibles dans le PDF.
- **Forme — mineur.** Définir les colonnes `C` et `T`, les unités et les abréviations de la Table 5.

### Page 29

- **Fond — mineur.** Le placebo 1988 est rassurant seulement au sens où aucun saut net n'est visible ; les intervalles sont larges.
- **Forme — majeur.** L'axe est intitulé en kilomètres mais gradué `-10k` à `10k`, notation qui correspond à des mètres. Corriger l'unité.
- **Forme — majeur.** Le coefficient mentionné dans la note n'apparaît pas sur le graphique.

### Page 30

- **Fond — majeur.** Les panneaux Population, changement de population, ratio jeunes/vieux et OPI suggèrent des ruptures ou changements de pente. Le texte principal doit les traiter comme des menaces à l'identification.
- **Forme — majeur.** Répétition de l'incohérence kilomètres / graduations en milliers ; annotations de tests invisibles.
- **Forme — suggestion.** Réduire le nombre de panneaux par page ou augmenter la taille des textes.

### Page 31

- **Fond — critique.** Les ruptures visibles de superficie, altitude, distance à l'agglomération, revenu et densité fragilisent directement le spatial RDD. La similarité locale ne peut pas être affirmée sans réserve.
- **Fond — majeur.** Une note disant que les effets « should be null and not significant » ne remplace pas un tableau de tests formels lisibles et une discussion des échecs d'équilibrage.
- **Forme — majeur.** Même incohérence d'unité sur l'axe ; caption principale placée après les notes, de manière non standard.

### Page 32

- **Fond — critique.** La phrase selon laquelle les résidus sont plus bas du côté non-ZRR semble inverser l'interprétation d'un effet réducteur du traitement. Vérifier le codage du côté gauche/droit et corriger le texte.
- **Fond — majeur.** Des bandes de confiance autour de deux courbes ne démontrent pas à elles seules la significativité d'une discontinuité au seuil.
- **Fond — majeur.** La conversion de 0,3-0,5 point en baisse relative de 1,8-3 % ne correspond pas précisément aux coefficients et à une moyenne de 17 %. Recalculer.
- **Fond — suggestion.** Préférer `effet local au seuil` à `LATE` si aucun argument d'instrumentation/compliance n'est posé.

### Page 33

- **Fond — critique.** La note de la Table 8 dit que l'effet sur la variation 1988-2002 est positif, alors que tous les coefficients affichés sont négatifs.
- **Forme — majeur.** La Table 7 expose des noms bruts de variables R (`treatmentZRRTRUE:x`, `superficie`) et des indicateurs `True/False`; ils doivent être remplacés par des libellés économiques.
- **Forme — mineur.** Les notes sont denses et la police est petite.

### Page 34

- **Fond — mineur.** La figure confirme surtout un saut négatif en 2002 ; les résultats ultérieurs sont moins nets et doivent être décrits comme tels.
- **Forme — majeur.** Page composée uniquement d'une figure à nombreux petits panneaux ; les titres et axes sont difficiles à lire.
- **Forme — mineur.** Harmoniser `FN2017`, `FN2022` avec les autres titres et définir la standardisation des résidus.

### Page 35

- **Fond — majeur.** Le cartouche de la Figure 14 indique un clustering au canton, tandis que la note indique `county level`. C'est une information d'inférence, pas un détail de style.
- **Fond — majeur.** Les intervalles semblent souvent recouvrir zéro après 2002 ; éviter de dire que l'effet reste significatif sur les élections ultérieures sans rapporter les tests exacts.
- **Forme — mineur.** Utiliser `5 km`, `10 km`, `20 km` dans la légende et renommer le titre si 2002 est inclus parmi les `later elections`.

### Page 36

- **Fond — majeur.** L'exercice sur communes frontalières doit être présenté d'emblée comme corroboratif : le texte reconnaît ensuite que les groupes sont mal équilibrés.
- **Fond — majeur.** Vérifier les tailles d'échantillon : 7 316 communes bordières puis 5 915 communes après formation des paires ; ces nombres ne concordent pas clairement avec les tables suivantes.
- **Forme — mineur.** Corriger `taken with precautions` en `interpreted with caution` et utiliser `treatment group`, pas `test group`.

### Page 37

- **Forme — critique.** La Table 9 est cassée : de nombreuses variables sont concaténées dans les mêmes cellules, les parenthèses ne ferment pas et les lignes ne sont pas interprétables. Elle n'est pas publiable.
- **Fond — critique.** Puisque la table d'équilibrage est illisible, le lecteur ne peut pas évaluer la validité du design de paires frontalières.
- **Forme — majeur.** Scinder le tableau ou utiliser un vrai paysage avec une ligne par variable et des unités explicites.

### Page 38

- **Fond — critique.** Les tailles d'échantillon sont incompatibles : le texte page 36 parle de 5 915 communes, la note de la Table 10 de 7 412 paires, et le tableau de 13 646 observations. Définir précisément communes, paires et observations.
- **Forme — critique.** La page paysage pivote correctement dans un lecteur compatible, mais le tableau n'occupe qu'une petite bande de la page et la police est trop petite. La lecture détaillée est pénible.
- **Forme — majeur.** Le tableau mélange OLS, panel, première différence et placebo sans séparation visuelle suffisante.

### Page 39

- **Fond — suggestion.** Ajouter une réserve sur ce que le matching peut corriger, compte tenu des déséquilibres résiduels.
- **Forme — majeur.** La page ne contient qu'un court paragraphe et un grand blanc ; déplacer le paragraphe pour supprimer cette page quasi vide.

### Page 40

- **Fond — majeur.** La significativité à 10 km ne justifie pas à elle seule le choix de bande passante. Commenter séparément stabilité du signe, amplitude et précision.
- **Fond — suggestion.** `LATE` est trop fort pour ce design ; préférer effet local estimé au voisinage de la frontière.
- **Forme — mineur.** Harmoniser mètres et kilomètres et simplifier `across all the different values of the bandwidths`.

### Page 41

- **Fond — critique.** Le texte décrit le coefficient doughnut comme environ `-0.007`, alors que la Table 11 affiche `-0.0105`. Corriger le chiffre et toute interprétation qui en découle.
- **Fond — majeur.** La stabilité des coefficients est seulement compatible avec de faibles spillovers ; elle ne permet pas de conclure qu'ils sont limités, surtout sans test de différence entre coefficients et avec changement d'échantillon.
- **Fond — majeur.** La note indique des erreurs robustes à l'hétéroscédasticité, alors que les résultats principaux sont clusterisés. Expliquer pourquoi l'inférence change.
- **Forme — mineur.** Remplacer `z` et `x` par des noms lisibles et corriger `1th` en `1st`.

### Page 42

- **Fond.** La réserve sur le placebo spatial est appropriée.
- **Forme — majeur.** Page presque vide avec un seul paragraphe ; à fusionner avec la page précédente.

### Page 43

- **Fond.** Le choix de qualifier l'hétérogénéité d'exploratoire et de la reléguer en annexe est approprié.
- **Forme — majeur.** Deuxième page presque vide consécutive ; le paragraphe peut être intégré à la Discussion ou à la page 42.

### Page 44

- **Fond — critique.** Le texte affirme que la Table 12 ne montre aucun effet significatif, alors que plusieurs coefficients sont étoilés à 20 km et 10 km. La conclusion défendable est l'absence de pattern cohérent et robuste, pas l'absence d'effet significatif.
- **Fond — majeur.** L'absence de résultat robuste en 1999 ne permet pas d'écarter les mécanismes socioéconomiques ; la puissance, le timing et les mesures disponibles limitent l'inférence.
- **Forme — majeur.** Après la note 21, la phrase reprend par `we conclude` sans majuscule ni couture syntaxique.
- **Forme — mineur.** La Table 12 est trop petite pour une table centrale de mécanismes.

### Page 45

- **Fond — majeur.** Une baisse du nombre absolu de voix FN et l'absence d'effet précis sur le turnout n'identifient pas formellement un mécanisme par le numérateur. Il faut analyser aussi les suffrages exprimés, votes valides et composition de l'électorat.
- **Fond — majeur.** `We conclude that the effect ... reflects diminished support` dépasse ce que la Table 13 seule établit ; présenter cela comme interprétation compatible.
- **Forme — mineur.** Renommer la variable dépendante `FN2002abs` et afficher une unité lisible pour la distance.

### Page 46

- **Fond — majeur.** La phrase d'ouverture de la conclusion est un fragment : `An expression of discontent ..., reminding to some of the interwar period.`
- **Fond — mineur.** La conclusion est plus prudente que certaines sections, mais l'idée que la preuve la plus forte « follows the timing » doit rester qualifiée en l'absence de tendances parallèles vérifiables.
- **Forme — majeur.** La première référence contient `https://doi.org/https://doi.org/...`.

### Page 47

- **Forme — majeur.** Plusieurs DOI sont dupliqués et certaines URL sont des liens secondaires plutôt que des identifiants bibliographiques propres.
- **Forme — majeur.** Les working papers, thèses, livres et articles suivent des styles hétérogènes ; l'entrée `Dickson ... working paper` est incomplète.
- **Fond — mineur.** Vérifier l'actualité des statuts `n.d.` et `working paper` avant soumission.

### Page 48

- **Forme — critique.** Les deux débordements LaTeX les plus importants se trouvent ici : l'URL Flammarion et l'entrée Lorenceau dépassent la largeur normale.
- **Forme — majeur.** Le DOI Gobillon semble incomplet (`10.1016/j.jpubeco.2012.06`) et plusieurs DOI sont encore préfixés deux fois.
- **Forme — majeur.** Harmoniser l'entrée IGN, les liens RePEc et les capitales des titres.

### Page 49

- **Forme — majeur.** L'URL de Rajan contient un paramètre de tracking `srsltid` et se casse sur plusieurs lignes ; utiliser une URL éditeur propre ou aucun lien marchand.
- **Forme — critique.** L'entrée `Y. Algan, E. P., S Guriev, & Passari, E.` a des auteurs manifestement mal parsés.
- **Fond — majeur.** Vérifier `Ronald, I., & Norris, P.` : la référence semble devoir être attribuée à Ronald Inglehart et Pippa Norris, avec noms/prénoms correctement encodés.

### Page 50

- **Fond — critique.** L'Appendix A est intitulé `Socioeconomic Evolution, 1988-2002`, alors que les données socioéconomiques du corps proviennent des recensements 1990 et 1999. Les colonnes 1988/2002 et la note doivent être vérifiées : il peut s'agir d'une erreur de millésime substantielle.
- **Fond — critique.** Le texte de l'Appendix B affirme d'abord la similarité locale, puis reconnaît des discontinuités et dit que l'identification n'est `not good enough`. Cette contradiction doit être répercutée clairement dans le texte principal.
- **Forme — mineur.** La Table 14 est très dense et son titre est lourd.

### Page 51

- **Fond — majeur.** L'histogramme présente une forte masse à zéro avec la mesure de distance entre frontières. Expliquer cette masse et ses conséquences pour l'estimation locale et les erreurs standards.
- **Forme — mineur.** La note parle de trois catégories alors que la légende en affiche davantage ; carte en km, histogramme en mètres.

### Page 52

- **Fond — majeur.** Plusieurs covariables montrent des ruptures ou pentes différentes. Le texte doit indiquer quelles variables échouent aux diagnostics, pas seulement `some discontinuities`.
- **Forme — majeur.** Les coefficients et p-values censés figurer en haut des panneaux ne sont pas visibles.
- **Forme — suggestion.** Harmoniser les titres des sous-graphes.

### Page 53

- **Fond — critique.** La rupture de `Area in km2 (log)` est très nette au seuil et touche une variable liée à la sélection et au vote FN. C'est une menace directe à l'interprétation causale.
- **Fond — majeur.** Identifier aussi explicitement les autres covariables problématiques au lieu d'une note générique.
- **Forme — majeur.** La note contient une parenthèse non fermée autour de `as in equation 2`.

### Page 54

- **Fond — majeur.** Le graphique permet de conclure à l'absence de saut clair ou statistiquement détectable en 1988, pas à l'absence certaine de discontinuité ; les intervalles sont larges et les points peu nombreux.
- **Forme — mineur.** `Remarkably` est trop rhétorique pour une note empirique.

### Page 55

- **Fond — majeur.** La Table 15 donne des effets de 0,5 à 0,7 point, pas uniformément 0,5 point.
- **Fond — majeur.** Dans la Table 16, l'effet varie de `-0.013` à `-0.005` selon la spécification : le signe est stable, l'amplitude beaucoup moins.
- **Forme — mineur.** `epicenters` est impropre ici ; utiliser `centroids`. Harmoniser `Treatment ZRR` / `treatment ZRR` et le niveau de clustering.

### Page 56

- **Fond — critique.** La description du matching est incohérente : 11 604 paires sont appariées puis 1 698 retirées, et le texte parle ensuite de 9 906 paires. Expliquer chaque étape et préciser s'il s'agit de communes, paires ou observations.
- **Fond — majeur.** `We do not need to perform the balancing checks again` est trop abrupt ; rappeler pourquoi les diagnostics du même échantillon sont réutilisables pour l'issue de 1995.
- **Fond — majeur.** Le texte parle de bandes 10 000 et 7 500, alors que les tables suivantes utilisent 20, 10 et 5 km.
- **Forme — mineur.** Corriger `caliber parameter` en `caliper parameter`.

### Page 57

- **Fond — majeur.** Après matching, OPI, diplôme secondaire, altitude, taille et vignes restent déséquilibrés. Le texte doit reconnaître que l'appariement ne résout pas toutes les différences structurelles.
- **Forme — mineur.** Corriger `Independant` en `Independent` et harmoniser `1,000`.

### Page 58

- **Fond — critique.** La Table 18 affiche 9 906 observations, sa note affirme 11 604 paires et le texte parle de 9 906 paires. Cette erreur de définition d'échantillon doit être résolue.
- **Fond — critique.** La Table 19 se trouve dans la section sur 1995 mais indique `FN vote share in 2002`. Vérifier s'il s'agit d'un mauvais libellé ou du mauvais tableau.
- **Forme — mineur.** Harmoniser `treatmentZRR`, la casse de `regression results` et les noms d'effets fixes.

### Page 59

- **Fond — critique.** La Table 20 utilise `* p<0.05; ** p<0.01; *** p<0.001`, contrairement aux autres tables (`0.1/0.05/0.01`). Une étoile n'a donc pas le même sens selon la page.
- **Fond — majeur.** Le texte mentionne 7,5 km, mais la table affiche 5 km.
- **Forme — majeur.** Corriger `effect of the 1995 elections of the ZRR program` en effet du programme sur le vote de 1995, et `department fixe effects` en `fixed effects`.

### Page 60

- **Fond — majeur.** La batterie multi-élections n'a pas de titre général ni de note visible sur cette page ; son rôle dans l'argument est indéterminé pour un lecteur isolé.
- **Forme — majeur.** La figure paraît coupée entre pages et n'affiche que `(a) Bandwidth of 20 kilometers` ; le numéro, la caption générale et la suite des panneaux doivent être rendus explicites.
- **Forme — mineur.** Harmoniser les titres `FN2017` et `FN2022`.

### Page 61

- **Fond — majeur.** Le saut 1995 n'est pas nettement visible, ce qui soutient une lecture prudente ; cette prudence doit primer sur les étoiles des bandes larges.
- **Forme — majeur.** La note dit que les bandes choisies sont 20, 10 et 5 km, mais la page ne montre qu'un seul graphique sans indiquer laquelle est représentée.

### Page 62

- **Fond — mineur.** La carte illustre la randomisation, mais ne valide pas sa crédibilité ; la discussion des limites doit rester associée à la figure.
- **Forme — mineur.** La note dit encore `three categories` alors que la légende en comporte davantage.

### Page 63

- **Fond — critique.** Avec seulement 100 permutations, un p-value empirique exact ne doit pas être reporté `p = 0.000`. Utiliser au minimum une correction de type `(b+1)/(B+1)` ou écrire `p < 0.01`, selon la procédure retenue.
- **Forme — majeur.** La Figure 22 dépasse la hauteur disponible selon LaTeX ; les titres/encadrés se chevauchent dans le panneau 10 km et la page paysage reste sous-optimisée.
- **Forme — mineur.** Corriger `The grey ones to that of the randomized permutations`.

### Page 64

- **Fond — critique.** L'équation (5) contient `gamma D_i X{i}` au lieu de `gamma D_i X_i`.
- **Fond — majeur.** La motivation sur les propriétaires fonciers et l'emploi agricole est spéculative ; la relier aux mécanismes du programme ou la supprimer.
- **Fond — majeur.** Discuter explicitement l'overfitting, l'honnêteté des arbres, le sample splitting et la stabilité des CATE entre méthodes.
- **Forme — mineur.** Corriger `several splitting rule`, `policy implication` et d'autres accords.

### Page 65

- **Fond — majeur.** La Table 21 ne rappelle pas que `bottom` correspond aux effets les plus négatifs et `top` aux plus positifs ; l'interprétation directionnelle n'est pas autonome.
- **Forme — majeur.** Remplacer les noms de colonnes de base de données (`p_value`, `mean_bottom`, `mean_top`) et harmoniser les unités (`km2`, pourcentages, logs).
- **Fond — mineur.** La conclusion qu'aucun modérateur unique n'est robuste reste cohérente, mais elle doit distinguer significativité descriptive et validation hors échantillon.

### Page 66

- **Fond — majeur.** Les quatre méthodes affichent exactement `Mean: -0.006`. Si c'est une propriété attendue de la construction, l'expliquer ; sinon vérifier un éventuel copier-coller des annotations.
- **Fond — suggestion.** Indiquer quelle distribution de CATE doit être privilégiée et pourquoi.
- **Forme — mineur.** Préciser l'unité de `Effect Size` et harmoniser les noms des méthodes avec le texte.

## Messages transversaux sur le fond

- Le papier est aujourd'hui plus convaincant lorsqu'il présente la première différence early-versus-later comme résultat principal prudent et le spatial RDD comme corroboration fragile.
- La transparence sur les menaces d'identification est un point fort, mais plusieurs résumés, notes de table et conclusions locales restent plus affirmatifs que les diagnostics.
- La chronologie 1995/1996 et 2004/2005 n'est pas une simple question de vocabulaire : elle détermine le statut pré/post-traitement de 1995 et la définition du groupe de comparaison.
- Les mécanismes `state signaling`, spillovers limités et effet par le numérateur ne sont pas identifiés de manière suffisamment directe pour être formulés comme conclusions.
- Les diagnostics d'équilibrage du spatial RDD et de l'appariement doivent être intégrés à l'interprétation, pas seulement relégués aux figures ou annexes.

## Messages transversaux sur la forme

- Le manuscrit est globalement compilable et aucune figure n'est entièrement manquante, mais plusieurs tables et captions sont encore au stade brouillon.
- Stabiliser une terminologie unique : `commune/municipality`, `canton`, `department`, `initial wave`, `later entrants`, `canton-clustered standard errors`.
- Remplacer tous les noms d'objets R dans les tables par des libellés publiables.
- Uniformiser les unités des distances, les conventions d'étoiles, la casse des titres et les niveaux de clustering.
- Rééquilibrer la pagination, en particulier les pages 39, 42 et 43, et refaire les paysages des pages 38 et 63.
- Faire une passe bibliographique dédiée avant soumission : DOI, auteurs, capitales, statuts des working papers, URLs et doublons.

## Avertissements de compilation à garder en tête

- Deux débordements bibliographiques importants sont signalés sur la page 48.
- La Figure 22 est déclarée trop grande de 148,8 points sur la page 63.
- Les pages 38 et 63 sont tournées à 90 degrés.
- La page 34 ne contient que des flottants.
- Quelques très faibles débordements de texte existent dans l'Introduction et le Background, sans impact visuel majeur.
