Ce dépôt GitHub s'inscrit dans le cadre du projet de recherche sur "les évolutions du service public de l'énergie" en France, porté par le CERES sous la direction de Marc Fleurbaey et Noé Kabouche.
Il recense les codes rédigés pour analyser les données tirées d'une enquête quantitative. Cette enquête a été réalisée auprès d'un échantillon représentatif de la population française (d'environ 2 000 individus) à propos de leur rapport au service public de l'électricité.

## AFM, CAH et dashboard : un seul script à lancer

Deux variantes avec les gestes G7 sont aussi disponibles, à lancer depuis la
racine : `source("2. Scripts/5a. AFM gestes illustratifs.R")`, puis
`source("2. Scripts/5b. AFM gestes contributifs.R")`. Elles recalculent leur
référence et écrivent dans des dossiers distincts, sans nouvelle CAH.
Leurs recodages et choix méthodologiques figurent dans
[AFM_gestes.md](4.%20Rapports/AFM_gestes.md).

Pour générer le dashboard comparatif des gestes, des représentations et des
sélections thématiques : `source("4. Rapports/generer_dashboard_gestes.R")`.
Il produit `4. Rapports/AFM_gestes_dashboard.html`. Les scripts 5a/5b exportent
également des cartes des individus, modalités actives/illustratives et échelles.

Depuis la racine du projet (ouvrir le fichier `.Rproj` dans RStudio) :

```r
source("4. Rapports/generer_dashboard.R")
```

Ou dans un terminal :

```sh
Rscript --vanilla '4. Rapports/generer_dashboard.R'
```

Le lanceur reconstruit automatiquement la préparation des données, le redressement,
l'AFM, la CAH, les comparaisons et les diagnostics, puis génère
`4. Rapports/AFM_dashboard.html`. Les analyses sont exécutées dans une session R
neuve ; les objets d'une ancienne session ne sont pas réutilisés.

### Fichiers à conserver dans Git

Le dossier `4. Rapports` contient **trois fichiers sources** :

| Fichier | Rôle |
|---|---|
| [generer_dashboard.R](4.%20Rapports/generer_dashboard.R) | Seul script à lancer ; orchestration et diagnostics |
| [AFM_dashboard.Rmd](4.%20Rapports/AFM_dashboard.Rmd) | Texte, graphiques et tableaux du dashboard |
| [Notes_methodologiques.md](4.%20Rapports/Notes_methodologiques.md) | Revue de la méthode, calage, résultats et conclusions |

Le lanceur utilise les scripts statistiques du dossier `2. Scripts`, notamment
les deux scripts 5 : **ils restent nécessaires et doivent aussi être versionnés**.
Le `.gitignore` et ce README font également partie des sources à conserver.
Les anciennes notes et les quatre petits scripts de lancement/diagnostic ont été
regroupés dans ces trois fichiers.

Le HTML est conservé localement pour consultation, mais exclu de Git avec les
sorties du dossier `3. Outputs`. Les données CSV/XLSX sont elles aussi exclues.
Après un clone, placer les données dans `1. Data` avant d'exécuter le lanceur.
Aucune donnée individuelle ni RDS de résultats n'a besoin d'être publié pour
versionner la méthode.

### Dépendances et paramètres

Packages nécessaires : `FactoMineR`, `missMDA`, `ggplot2`, `ggrepel`, `cluster`,
`tidyverse`, `questionr`, `openxlsx`, `survey`, `rmarkdown`, `knitr`, `DT`.
Pandoc est également nécessaire (fourni avec RStudio). Aucune installation
automatique n'est effectuée.

Données attendues : `panel_long.csv`, `panel_court.csv`,
`Public data/grille_densite_2026.xlsx` et `Public data/TD_POP1B_2022.csv`,
sous `1. Data`. Les dictionnaires d'enquête restent utiles à la lecture des questions.

Les paramètres statistiques sont en tête de `2. Scripts/5. AFM et CAH.R`.
Les prolongements comparent une référence fixe à cinq axes et quatre classes.
Les versions de R, des packages et de Pandoc ainsi que les empreintes des sources
sont enregistrées dans `3. Outputs/AFM/reproductibilite/`. Cet enregistrement
n'installe ni ne verrouille les versions des packages.

Pour modifier uniquement la présentation, sans recalculer les analyses :

```sh
Rscript --vanilla '4. Rapports/generer_dashboard.R' --rapport-seul
```

Le R Markdown peut aussi être rendu directement dans RStudio ; son paramètre
`recalculer` vaut `TRUE` par défaut. Les figures sont calculées en R avec
`ggplot2`/`ggrepel`, les tableaux avec `DT`, et le HTML avec `rmarkdown`/Pandoc.
