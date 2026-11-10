# AFM avec gestes : variantes illustratives et contributives

## Exécution

Depuis la racine du projet, dans RStudio :

```r
source("2. Scripts/5a. AFM gestes illustratifs.R")
source("2. Scripts/5b. AFM gestes contributifs.R")
```

Ou dans deux sessions propres :

```sh
Rscript --vanilla '2. Scripts/5a. AFM gestes illustratifs.R'
Rscript --vanilla '2. Scripts/5b. AFM gestes contributifs.R'
```

Les deux scripts utilisent `AFM_gestes_fonctions.R`. Chacun reconstruit le
redressement et l'AFM de référence, puis exécute sa variante. Aucune reprise
d'un ancien objet `df` en mémoire, aucune installation automatique de packages.
Les dépendances sont celles du script 5 et de son amont.

Sorties respectives : `3. Outputs/AFM_gestes_illustratif/` et
`3. Outputs/AFM_gestes_contributif/`. La référence recalculée est enregistrée
dans un sous-dossier `reference/` de chaque variante. La sortie historique
`3. Outputs/AFM/` n'est pas remplacée. Pas de nouvelle CAH à ce stade.

## Variables et recodages

Neuf questions G7Q00001_SQ001–009, chacune qualitative, sans score numérique :

| Item | Contenu | Groupe actif dans 5b |
|---|---|---|
| 001 | Limiter les déplacements thermiques | Sobriété des usages |
| 002 | Réduire chauffage/climatisation/eau chaude | Sobriété des usages |
| 003 | Décaler les usages en heures creuses | Pilotage et information |
| 004 | Suivre la consommation | Pilotage et information |
| 005 | Consacrer du temps à l'information | Pilotage et information |
| 006 | Installer de petits équipements efficaces | Équipements et investissements |
| 007 | Remplacer un gros équipement | Équipements et investissements |
| 008 | Faire des travaux d'isolation | Équipements et investissements |
| 009 | Installer un moyen de production d'énergie | Équipements et investissements |

Les cinq modalités sont conservées : « Déjà fait », « Jamais fait. Prêt·e à
faire », « Jamais fait. Pas prêt·e à faire », « Non concerné·e » et « Ne sait pas /
Ne se prononce pas ». Aucune échelle ordinale 0/1/2 n'est imposée ; aucune
indicatrice redondante n'est ajoutée à l'actif. Le fait d'avoir réalisé un geste
ne renseigne pas son intensité, sa fréquence ni son effet énergétique.

« Non concerné » et NSP restent substantiellement distincts du refus. Leurs
associations peuvent refléter l'éligibilité, la situation matérielle ou
l'incertitude, pas seulement une attitude écologique.

Les cellules NA, vides, « NA », « N/A » sont des absences techniques. Dans 5a,
elles sont représentées comme modalité illustrative « Absence technique » et
ne changent pas la sélection des personnes. Dans 5b, seules les personnes ayant
les neuf gestes techniquement renseignés sont retenues. Aucun geste n'est imputé.
Les NSP et « Non concerné » ne provoquent pas d'exclusion dans 5b.

## Population, imputation et pondération

Point de départ : les longs conservés par le script 5, c'est-à-dire son filtre
de 30 % de manquants sur les 11 variables actives historiques. L'imputation
FAMD régularisée de ces 11 variables, à trois composantes, reste celle du script 5.
L'imputation des représentations est fixée avant toute restriction supplémentaire
de 5b ; elle n'est pas recalculée sur les seuls cas complets G7. Ce choix permet
d'isoler l'ajout des variables, mais ne constitue pas une imputation conjointe
représentations/pratiques. La population diffère entre variantes si G7 présente
des absences techniques : les effectifs sont exportés.

5b reconstruit aussi une référence sans gestes sur exactement sa population,
avec les mêmes représentations imputées, pour comparer les axes sans confondre
ajout de variables et changement de personnes. Les poids du panel sont conservés
et renormalisés à la taille de chaque analyse. Ils ne sont pas recalés aux longs ;
les limites décrites dans `Notes_methodologiques.md` restent applicables.

L'origine long/court est maintenant enregistrée au moment de la concaténation
dans le script 1 (`version_questionnaire`). Cela identifie le format des
questionnaires même si un identifiant Bilendi manque, sans supprimer de réponse
ni affirmer que deux identifiants absents correspondent au même individu.

## Construction des deux AFM

5a : les quatre groupes actifs historiques restent identiques. Les profils
sociaux et les neuf gestes forment deux groupes supplémentaires de type `n`.
Ils n'interviennent pas dans la géométrie active. Le code vérifie l'identité de
l'inertie et la conservation numérique des distances entre les coordonnées des
100 premières personnes avec la référence. FactoMineR 2.17 utilise une SVD
itérative (`irlba`), qui peut produire des écarts numériques minimes : le seuil
est 10⁻⁴ fois la distance maximale (ou 1 si cette distance est inférieure à 1).
L'écart observé et le seuil sont exportés dans `00_controle_projection.csv`.
Une graine fixe rend le calcul de la variante reproductible. Les contributions
actives des gestes sont laissées manquantes.

5b : les quatre groupes historiques et les trois groupes G7 sont actifs ; les
profils sociaux restent supplémentaires. Les trois groupes G7 sont de type `n`.
L'AFM équilibre les groupes selon leur première valeur propre ; l'ajout de trois
groupes change le poids relatif des thèmes dans l'espace. Les petits équipements
restent ici avec les investissements : les isoler constituerait une variante à
quatre groupes de pratiques, donc un autre équilibrage. Aucun groupe ni résultat
n'est choisi selon sa capacité à produire une typologie visuellement convaincante.

Les paramètres `group`, `type`, `num.group.sup` et `row.w` suivent la
[documentation officielle de FactoMineR](https://search.r-project.org/CRAN/refmans/FactoMineR/html/MFA.html).
Cinq axes sont conservés comme dans l'analyse principale.

## Lecture des sorties

- `00_population.csv` et `01_absences_techniques.csv` : sélection et non-réponses.
- `02_groupes.csv` : composition, type et statut des groupes.
- `03_frequences_gestes.csv` : effectifs bruts et parts pondérées des modalités.
- `04_inertie.csv` et `05_contributions_groupes.csv` : structure des axes.
- `06_modalites_gestes_axes.csv` : coordonnées, cos², v-tests lorsqu'ils existent,
  contributions pour 5b seulement, effectifs observés. Les modalités rares restent
  dans les tableaux ; seules les étiquettes des figures exigent au moins 30 cas.
- `07_associations_eta2.csv` : association pondérée entre chaque geste et chaque
  axe ; absences techniques exclues du calcul, NSP et non-concernés conservés.
- `08_individus_coordonnees.csv` : ligne d'origine, poids et coordonnées.
- `09_correlations_axes_reference.csv` : corrélations absolues croisées entre
  axes de référence et de variante. Les signes des axes sont arbitraires et une
  rotation du sous-espace n'est pas résumée par ces seules corrélations.
- Figures des modalités sur les plans 1–2 et 2–3, carte des η² ; RDS de la variante
  et de sa référence, `sessionInfo.txt` et empreintes MD5 des scripts.

Les v-tests et η² sont exploratoires. En 5b, les gestes construisent les axes :
leurs associations aux axes ne constituent pas une validation indépendante.
Une comparaison des pourcentages d'inertie entre tableaux de dimensions différentes
ne permet pas de déclarer qu'une AFM est « meilleure ». Les sorties individuelles
et les RDS restent locaux dans le dossier ignoré par Git.

## Vérification du 3 octobre 2026

Les deux lanceurs ont été exécutés jusqu'au bout avec `Rscript --vanilla`.
Ils retiennent chacun 2 624 personnes : les neuf gestes sont techniquement
renseignés pour toute la population de référence, donc 5b n'exclut aucun cas
supplémentaire dans ces données. L'inertie illustrative est identique à celle
de référence ; l'écart maximal des distances contrôlées est d'environ 0,0000324,
en dessous du seuil numérique d'environ 0,000694. Les fréquences, l'alignement
des modalités sur les cinq axes et le statut des contributions ont été contrôlés.
La variante active n'a pas de nouvelle CAH ; son interprétation reste exploratoire.

## Graphiques et dashboard comparatif

Les scripts 5a et 5b produisent désormais, sur les plans 1–2 et 2–3 :
cartes des individus colorées par cos² du plan, cartes conjointes des modalités
actives/illustratives, superpositions du nuage individuel et des modalités,
cercles des corrélations des échelles actives/illustratives, et cartes des
associations de toutes les variables. Les contributions des groupes sont aussi
tracées sur les cinq axes. Les PDF et leurs objets graphiques RDS sont sauvegardés.
Ces traitements figurent dans `AFM_gestes_graphiques.R`, appelé par les scripts.

Sur les cartes qualitatives, 14 modalités actives sont étiquetées selon leur
contribution cumulée au plan et 14 illustratives selon leur |v-test| maximal.
Le seuil de 30 cas observés s'applique uniquement aux étiquettes ; toutes les
modalités sont tracées. Les absences techniques/non-renseignés n'ont pas
d'étiquette. Une modalité peu représentée peut rester dans le tableau complet.
Les barycentres qualitatifs et les flèches de corrélation ont des coordonnées
de nature différente : ils sont présentés dans des figures distinctes.

`10_toutes_variables_axes.csv` décrit les variables et modalités des deux statuts
sur cinq axes. `11_associations_toutes_variables.csv` donne les η² qualitatifs
et r² quantitatifs pondérés sur les réponses observées, avec dénominateurs ;
les absences techniques/non-renseignés sont exclues de ces associations.
Les NSP explicites et non-concernés G7 restent des réponses dans ces calculs.
Les coordonnées factorielles sont, elles, issues des données actives préparées
et imputées selon la méthode décrite plus haut.

Le nouveau rapport ne remplace pas le dashboard historique :

```r
source("4. Rapports/generer_dashboard_gestes.R")
```

Le lanceur rend `AFM_gestes_dashboard.Rmd` en `AFM_gestes_dashboard.html`, après
recalcul dans un processus R neuf. Pour refaire seulement la présentation :

```sh
Rscript --vanilla '4. Rapports/generer_dashboard_gestes.R' --rapport-seul
```

Le R Markdown peut aussi être rendu directement depuis RStudio ; `recalculer`
vaut TRUE par défaut. Le HTML autonome contient les graphiques et les tableaux
DT filtrables/exportables. Le tableau comparatif, les empreintes du rapport et
les versions de rendu sont exportés dans les sorties contributives.

## Sélection de variables : scénarios explicites

Le script 5b calcule aussi trois scénarios, dans son dossier `selection/`.
Ils utilisent exactement les mêmes individus, poids, modalités et représentations
imputées que la variante contributive ; aucune optimisation automatique n'est
effectuée. Les variables retirées de l'actif passent en illustratif.

| Scénario | Variables actives | Groupes actifs | Raison du choix |
|---|---:|---:|---|
| Service public + neuf gestes | 15 | 5 | Retirer les choix de transition et l'autoconsommation, conserver les représentations du service public et tous les gestes |
| Service public + cinq gestes ciblés | 11 | 4 | Garder chaleur, suivi, petits équipements, remplacement, isolation ; recentrer sur usages quotidiens et logement |
| Pratiques seules | 9 | 3 | Décrire la structure des pratiques, puis projeter toutes les représentations |

La deuxième variante écarte de l'actif la mobilité (périmètre extérieur au logement),
les heures creuses (organisation tarifaire particulière), l'information (proche
de l'intérêt/connaissance, sans être identique) et la production (investissement
spécifique). Ces raisons sont des choix de périmètre, pas la preuve que ces
variables sont de mauvaise qualité. La chaleur concerne plusieurs énergies et
n'est pas une mesure strictement électrique. Les groupes « Pratiques quotidiennes »
et « Investissements logement » ont respectivement 3 et 2 items.

Un choix de groupe modifie aussi l'équilibrage : le gain ne peut donc être attribué
uniquement au nombre de variables. Les trois scénarios sont exploratoires,
définis avant leur propre estimation, après connaissance des analyses initiales ;
ils ne sont pas préenregistrés ni validés sur un échantillon indépendant.
Le maintien de la même imputation isole mieux la sélection de variables, mais
les représentations imputées peuvent encore dépendre des variables historiques
devenues illustratives. Une nouvelle préparation adaptée au périmètre final
constituerait une analyse supplémentaire de sensibilité.

L'inertie du plan 1–2 et le cumul à cinq axes doivent être lus avec le nombre de
groupes/variables, la structure des modalités et le contenu étudié. Retirer des
variables modifie le total d'inertie ; une hausse du pourcentage n'est pas une
preuve de meilleure qualité ni de meilleure représentation des thèmes retirés.
Éviter un retrait guidé seulement par de faibles contributions sur les axes 1–2 :
une variable pertinente peut structurer l'axe 3 ou des dimensions suivantes.

### Spectre complet et résultats de la sélection

Avec FactoMineR 2.17 et sa SVD itérative, `eig` ne contient ici que les cinq
valeurs propres calculées. Les pourcentages utilisent néanmoins l'inertie totale :
leur somme n'est donc pas 100 %. Pour tracer le cumul jusqu'à quinze axes et
compter les axes nécessaires à 50/80 %, `spectre_afm_gestes()` reconstruit la
matrice active centrée/réduite du `global.pca`, applique exactement les poids
de lignes et colonnes, puis diagonalise son produit croisé. Les valeurs propres
positives sont rapportées à la trace totale. Les premières valeurs sont vérifiées
contre les pourcentages de l'AFM (tolérance 0,0001 point). Cela n'altère ni la
géométrie des cinq axes conservés, ni les individus, ni l'imputation.
Les quasi-zéros numériques sont omis ; leur inertie est négligeable.
Le spectre est exporté dans `12_spectre_complet.csv` et dans la comparaison du dashboard.

Les calculs sur 2 624 personnes donnent :

| Variante | Plan 1–2 | Cinq axes |
|---|---:|---:|
| Représentations + neuf gestes, 20 variables actives | 9,28 % | 19,59 % |
| Service public + neuf gestes, 15 variables actives | 12,41 % | 25,47 % |
| Service public + cinq gestes ciblés, 11 variables actives | 14,80 % | 31,96 % |
| Neuf gestes seuls | 14,62 % | 29,52 % |

Les premiers contrôles confirment la présence de tous les individus et modalités,
des associations finies et des contributions illustratives laissées manquantes.
Le gain de concentration est mesuré ; aucune variante n'est déclarée supérieure
sur ce seul critère. Les résultats du HTML sont recalculés depuis les objets RDS.

Le HTML a été généré et contrôlé : 34 graphiques intégrés, tableaux comparatifs
et onglets illustratifs/contributifs/sélection présents. Les contrôles statistiques
confirment 2 624 personnes dans les six analyses comparées, des associations
finies entre 0 et 1 et l'absence de contributions actives attribuées aux
illustratives. Le spectre complet atteint 100 % à l'arrondi numérique.
Atteindre 50 % nécessite 9 axes pour la référence, 19 pour la variante complète,
14 pour service public + neuf gestes, 10 pour service public + cinq gestes et
11 pour les pratiques seules. La lisibilité d'un seul plan reste donc limitée.
