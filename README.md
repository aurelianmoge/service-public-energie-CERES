Ce dépôt GitHub s'inscrit dans le cadre du projet de recherche sur "les évolutions du service public de l'énergie" en France, porté par le CERES sous la direction de Marc Fleurbaey et Noé Kabouche.
Il recense les codes rédigés pour analyser les données tirées d'une enquête quantitative. Cette enquête a été réalisée auprès d'un échantillon représentatif de la population française (d'environ 3 000 individus) à propos de leur rapport au service public de l'électricité.

## Les données

Les fichiers de données ne sont pas versionnés. Après un clone, placer dans `1. Data` :
`panel_long.csv`, `panel_court.csv`, les deux dictionnaires, et dans `1. Data/Public data`
`grille_densite_2026.xlsx` et `TD_POP1B_2022.csv` (INSEE).

Les données de l'institut sont déjà nettoyées en amont : tous les répondants ont réussi la
question piège et dépassé la durée minimale (120 s pour le court, 480 s pour le long), et aucun
identifiant n'est en double.

## Les scripts, dans l'ordre

Tous les scripts se lancent depuis la racine du projet (ouvrir le `.Rproj` dans RStudio).

| Script | Rôle | Population |
|---|---|---|
| `1. Redressement.R` | Calage sur âge × sexe × zone (rural / urbain), cible rurale corrigée de la perception. Source les scripts 4 et 2. Ne laisse que `df`. | 3 259 répondants |
| `2. Variable age.R` | Variable d'âge unique et tranches d'âge (script d'Antoine) | |
| `4. Variable zone.R` | Zone d'habitation et cible rurale corrigée (script d'Antoine) | |
| `3. Recodages.R` | Tous les indicateurs, construits une seule fois, avec les conventions du script 7. Enregistre `3. Outputs/df_recode.rds`. | |
| `5a. ACM service public.R` | Conceptions du service public (définition, garants, organisation) | Tous |
| `5b. ACP choix de transition.R` | Les deux classements de la transition (leviers, productions) | Tous |
| `5c. ACM gestes.R` | Les neuf gestes de maîtrise de l'énergie | Longs |
| `5d. ACM marche et offre verte.R` | Fournisseur, offre verte, autoconsommation | Longs |
| `5e. AFM combinaisons des blocs.R` | Les blocs sont-ils liés entre eux ? (RV, AFM, corrélations entre axes) | |
| `6. Regressions.R` | Modèles emboîtés, décomposition du R² (Shapley), robustesse, focus nucléaire | |
| `7. Scores et regressions.R` | Indicateurs et régressions d'Antoine, conservés tels quels | |

L'ordre d'exécution est : `3. Recodages.R` (qui lance lui-même le redressement), puis `5a` à `5e`,
puis `6`. Les scripts 2 et 4 n'ont pas à être lancés seuls. Le script 7 est indépendant : il
relance le redressement s'il ne trouve pas `df`, et écrit dans `3. Outputs/Regressions`
(sans accent), à ne pas confondre avec `3. Outputs/Régressions` (script 6).

Les sorties de chaque analyse factorielle sont dans `3. Outputs/Analyses factorielles/`, celles des
régressions dans `3. Outputs/Régressions/`.

Packages : `tidyverse`, `openxlsx`, `survey`, `FactoMineR`, `factoextra`, `knitr`.

## Les archives

Les anciens scripts sont dans `2. Scripts/Annexes` (dont le diagnostic géographique
`3. Variable geographie.R`, et l'ancienne AFM avec sa CAH), les anciens rapports et dashboards dans
`4. Rapports/Archives`, et les anciennes sorties dans `3. Outputs/Archives`. Ces scripts ne sont plus
maintenus : leurs chemins ne fonctionnent plus depuis le dossier `Annexes`.
