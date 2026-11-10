# Notes méthodologiques — AFM et CAH

Ce document rassemble la revue initiale, le diagnostic de calage et le bilan
scientifique de l’analyse. Le code de référence est `generer_dashboard.R`,
qui appelle les scripts statistiques du dossier 2 et génère `AFM_dashboard.Rmd`.
Les chiffres commentés décrivent les calculs documentés ; les tableaux du
dashboard sont actualisés lors de chaque reconstruction.


## Revue de la méthode

### Avis général

L'AFM est adaptée à une analyse exploratoire articulant quatre thèmes définis
avant l'analyse. Les 11 variables mêlent scores, échelles et choix qualitatifs ;
les groupes `s/m/m/n` sont cohérents avec cette structure. Les propriétés sociales
restent illustratives. Les dictionnaires confirment les recodages des rôles des
acteurs et des cases de connaissance de l'autoconsommation.

La CAH de Ward sur les coordonnées factorielles est également cohérente : elle
résume les proximités dans l'espace retenu. Il ne faut pas restandardiser chaque
axe avant la CAH, car cela annulerait leur pondération par l'inertie. Le passage
à quatre classes sur cinq axes est un choix exploratoire à discuter, pas une
solution validée par la seule existence du dendrogramme.

### Corrections effectuées

| Point | Modification et conséquence |
|---|---|
| Poids de la CAH | `consol = FALSE` dans les deux scripts. HCPC transmet les poids à Ward, mais son étape de consolidation appelle `stats::kmeans` sans poids. La partition finale correspond désormais à la coupure du dendrogramme pondéré. |
| Profils des classes | Fréquences et moyennes calculées sur les réponses observées ; dénominateurs renseignés exportés. L'imputation construit l'espace, mais ne devient plus une réponse observée dans les profils. |
| Modalités rares | Tous les profils restent dans le CSV ; le seuil de 30 personnes s'applique à la sélection graphique. Les fréquences actives portent désormais sur les individus effectivement analysés. |
| Contradictions | L'autoconsommation « NSP + Non » est traitée comme contradictoire, comme les autres combinaisons incompatibles. |
| Sensibilité | Comparaisons AFM/FAMD, AFM resserrée, 3/4/5 classes, 3/5/10 axes et 2/3/4 composantes d'imputation. Les personnes et les poids restent identiques entre variantes. |
| Lisibilité | Commentaires méthodologiques répétés condensés, chemins corrigés, sorties de population et versions logicielles ajoutées. Les contrôles bloquant une erreur de données sont conservés. |
| Figures | Export PDF Unicode adapté à macOS ; rapport HTML à onglets, graphiques intégrés et tableaux filtrables. |

### Ce que montrent les résultats

La population analysée comprend 2 624 répondants, contre 2 770 questionnaires longs
après le nettoyage du redressement. L'ESS de Kish est d'environ 2 458 et 4,85 % des
cellules actives retenues sont imputées. Le plan 1–2 représente 14,49 % de
l'inertie ; cinq axes représentent 32,27 %. Le plan seul ne suffit donc pas à
caractériser les classes.

Avec Ward sans consolidation, les parts pondérées des quatre classes sont
7,33 %, 8,83 %, 59,58 % et 24,25 %. Leur accord avec une partition à quatre classes
sur trois axes est faible (ARI ≈ 0,25), et celui avec la FAMD est très faible
(≈ 0,05). En changeant seulement le nombre de composantes de l'imputation,
l'accord est d'environ 0,30 (deux composantes) et 0,52 (quatre composantes).
Ces valeurs conduisent à présenter la typologie comme **sensible aux choix
analytiques**, et à privilégier les oppositions et profils retrouvés dans plusieurs
variantes avant de donner des noms substantiels définitifs aux classes.
Le dashboard recalcule ses chiffres depuis les sorties et comprend aussi la
comparaison à dix axes.

L'ARI décrit l'accord entre partitions, sans pondération et sans problème de
permutation des étiquettes. Ces variantes constituent une analyse de sensibilité,
pas une étude de stabilité par bootstrap. Des axes peuvent également tourner au
sein d'un sous-espace : les corrélations entre axes sont un diagnostic partiel,
non une mesure exhaustive d'équivalence géométrique.

### Choix scientifiques restant à expliciter

1. **Population et pondération.** Le redressement porte sur le panel long + court.
   Réutiliser ces poids après restriction aux longs ne garantit pas le calage de
   ce sous-échantillon. Les 18–34 ans représentent 17,93 % de l'analyse pondérée,
   contre 25,09 % du panel redressé. Pour estimer des proportions nationales à
   partir des longs, examiner le mécanisme d'affectation long/court puis envisager
   un recalage propre à cette population. Les scripts n'imposent pas ce changement.
2. **Échelles et NSP.** Traiter les échelles 1–5 comme numériques suppose des
   intervalles comparables. Imputer les NSP les traite comme des réponses
   manquantes ; cela peut atténuer une dimension substantielle d'incertitude ou de
   retrait. Une variante avec NSP explicites et une variante ordinale/catégorielle
   seraient utiles si ces dimensions sont au cœur de l'hypothèse.
3. **Imputation.** La FAMD régularisée est une solution pratique pour les groupes
   mixtes, mais ne respecte pas exactement l'équilibrage thématique de l'AFM.
   `completeObs` attribue une catégorie unique aux réponses qualitatives manquantes.
   La sensibilité au rang est maintenant calculée ; elle ne quantifie pas toute
   l'incertitude d'imputation et ne sélectionne pas le rang par validation croisée.
4. **Contenu des variantes.** L'AFM resserrée retire les choix de transition de
   l'actif et ajoute la responsabilité de la garantie d'accès. Elle modifie deux
   aspects à la fois : ses différences ne peuvent être attribuées uniquement au
   retrait d'un thème. La FAMD, elle, garde exactement les mêmes variables imputées.
5. **Amont du projet.** Le script de redressement filtre d'abord les sexes autres
   que Homme/Femme, puis prévoit de les imputer pour le calage : à ce stade, ils
   sont déjà exclus. Le script de zone évoque une cible corrigée de la perception,
   tandis que le redressement final emploie la cible INSEE brute. Ces choix sont
   consignés ici ; le fichier de redressement, déjà modifié dans le dépôt, n'a pas
   été changé pendant cette revue.

L'analyse reste descriptive : ni les v-tests de projection, ni les profils de
classes construits avec les mêmes variables ne constituent une validation
indépendante ou une preuve causale. L'ESS indique la dispersion des poids,
pas la représentativité du recrutement.

### Sources et vérification

- [FactoMineR : MFA, code source](https://github.com/cran/FactoMineR/blob/master/R/MFA.R)
  — groupes, types et poids des individus.
- [FactoMineR : HCPC, code source](https://github.com/cran/FactoMineR/blob/master/R/HCPC.R)
  — transmission des poids à la hiérarchie et consolidation par k-means.
- [Documentation officielle HCPC](https://search.r-project.org/CRAN/refmans/FactoMineR/html/HCPC.html)
  — axes retenus, coupure et consolidation.
- [missMDA : manuel du package](https://cran.r-universe.dev/missMDA/doc/manual.html)
  — imputation régularisée et différence entre tableau disjonctif imputé et
  catégories de `completeObs`.

Les fonctions de la version locale ont aussi été inspectées pour vérifier le
comportement réel des poids. Les versions utilisées sont enregistrées dans
`3. Outputs/AFM/sessionInfo.txt`.


## Calage et interprétation

### Comment le dashboard est produit

Le fichier `AFM_dashboard.Rmd` contient le texte, les paramètres, les traitements
R de présentation et tous les graphiques. `ggplot2` et `ggrepel` tracent les figures,
`DT` crée les tableaux interactifs, `knitr` exécute les blocs R et `rmarkdown`/
Pandoc produit un HTML autonome. Le CSS présent dans le R Markdown règle
uniquement la mise en page. Le HTML est une sortie régénérable.

Le paramètre optionnel `recalculer = FALSE` relit les CSV et RDS des scripts d'analyse.
Avec `recalculer = TRUE` (par défaut), le R Markdown exécute d'abord
`generer_dashboard.R` dans une session `Rscript --vanilla`, qui repart des
fichiers sources et recalcule toutes les analyses. Les commandes figurent dans
le README. Ce processus distinct évite de reprendre un ancien `df` en mémoire.
Les versions et empreintes des sources sont enregistrées dans le dossier de
reproductibilité ; un environnement de packages verrouillé reste une amélioration
possible pour reproduire exactement le même environnement sur une autre machine.

### Ce que fait actuellement le redressement

Le script final réunit questionnaires longs et courts, exclut les personnes sans
âge/sexe/zone exploitable pour le calage, puis utilise `survey::postStratify` sur
les cellules **âge (4 groupes) × sexe (2 catégories de calage) × territoire
(rural/urbain)**. Il s'agit donc d'une post-stratification sur une distribution
jointe, et non d'un simple ajustement séparé de trois marges. La cible provient
de la construction INSEE du script amont.

Avec des poids initiaux uniformes, chaque personne d'une cellule h reçoit un
poids proportionnel à :

\[
w_h = \frac{N_h}{n_h},
\]

où N_h est l'effectif cible de la cellule et n_h son effectif dans le panel
complet nettoyé. La somme des poids dans chaque cellule retrouve donc N_h.
Le code normalise ensuite les poids pour que leur somme égale la taille du panel.
Cela modifie l'échelle des poids, pas les proportions.

### Pourquoi la restriction aux longs change le résultat

L'AFM porte seulement sur les longs, puis sur ceux ayant au plus 30 % de valeurs
manquantes actives. Si m_h personnes restent dans la cellule h, conserver les
poids précédents donne un total proportionnel à :

\[
m_h w_h = N_h\frac{m_h}{n_h}.
\]

Si le taux de conservation m_h/n_h varie selon les cellules, les proportions
cibles ne sont plus retrouvées. Renormaliser tous les poids par une même constante
ne corrige pas cette variation. Cela explique comment un panel complet calé peut
produire un sous-échantillon d'analyse qui ne l'est plus.

Les tableaux `00_calage_etapes.csv` et `00_calage_cellules.csv` distinguent les
effets de la restriction aux longs et de l'exclusion des réponses trop incomplètes.
Ils sont générés par `generer_dashboard.R`, sans modifier aucun poids. La comparaison
finale déjà disponible donne :

| Âge | Panel pondéré | Longs avant filtre AFM | Longs analysés |
|---|---:|---:|---:|
| 18–34 | 25,09 % | 17,62 % | 17,93 % |
| 35–49 | 24,01 % | 25,92 % | 25,96 % |
| 50–64 | 24,49 % | 26,68 % | 26,42 % |
| 65+ | 26,40 % | 29,78 % | 29,69 % |

Le panel nettoyé contient 3 185 personnes : 2 770 longs et 415 courts.
Parmi les 763 personnes de 18–34 ans, 465 ont un questionnaire long (60,9 %),
contre 871 sur 893 parmi les 65 ans et plus (97,5 %). Ces effectifs décrivent
une différence d'affectation ou de composition ; ils ne permettent pas, seuls,
d'en identifier le mécanisme. Le décalage pondéré vient essentiellement du
passage aux longs : le filtre AFM le réduit légèrement pour les 18–34 ans.

Les 16 cellules croisées sont toutes présentes après le filtre AFM, avec des
effectifs compris entre 38 et 308. L'absence de cellule vide rend une
post-stratification propre à cet échantillon techniquement envisageable ;
elle ne suffit pas à valider la cible ni les hypothèses de sélection.

L'écart des 18–34 ans est de **−7,16 points**, soit environ 29 % de moins en part
relative. Les marges sexe et territoire sont plus proches de celles du panel,
mais cela ne garantit pas que les cellules croisées le soient.

### Faut-il recalibrer ? Cela dépend de la population visée

**Si les longs sont une version de questionnaire destinée à représenter les mêmes
adultes que le panel complet**, il est pertinent d'étudier un ajustement spécifique
aux personnes effectivement analysées. Il faut d'abord documenter l'affectation
long/court : quotas, âge, vague, choix aléatoire ou autre mécanisme. Un taux de
sélection connu peut appeler une correction de poids propre au plan de collecte.

**Si les longs définissent au contraire un véritable domaine de population**,
par exemple une catégorie particulière éligible à certaines questions, ils n'ont
pas nécessairement à retrouver les marges de tous les adultes. Il faut alors
annoncer ce domaine et, le cas échéant, utiliser ses propres totaux cibles.
Une différence de marges n'est donc pas automatiquement une erreur de poids.

Pour une même population cible et si toutes les cellules sont suffisamment
remplies, une post-stratification des répondants analysés peut s'écrire :

\[
w_i^{\mathrm{nouveau}} = w_i\,
\frac{N_h}{\sum_{j\in A_h}w_j}, \qquad i\in A_h.
\]

Dans le cas actuel, où les poids sont constants au sein d'une cellule, cela revient
à N_h/m_h, avant une éventuelle normalisation. Une cellule cible vide rend ce
calage exact impossible sans revoir les cellules ou la méthode. Des cellules
très petites peuvent produire des poids importants : examiner effectifs,
quantiles des poids et ESS avant de retenir la variante.

Un recalage ne corrige que les déséquilibres observés sur les variables utilisées.
Il ne garantit pas l'absence de sélection sur les opinions à âge, sexe et zone
identiques, ni la représentativité complète d'un panel non probabiliste.

**Prochaine comparaison utile** : conserver l'analyse actuelle comme référence,
recalculer toute la chaîne (imputation comprise) avec les poids spécifiques aux
longs retenus, puis comparer axes, profils et parts des classes. Changer seulement
les pourcentages finaux laisserait une géométrie construite avec les anciens poids.
Cette note expose cette option ; elle ne l'applique pas automatiquement.

### Comment interpréter les autres remarques

**Présenter aussi l'axe 3.** Sur l'axe 1, intérêt/connaissance et autoconsommation
contribuent respectivement à 41,2 % et 39,5 %. Sur l'axe 2, les choix de transition
contribuent à 48,9 %. Le groupe service public/gouvernance contribue à 3,3 %, 15,1 %
et 29,8 % sur les axes 1, 2 et 3. Le plan 2–3 rend donc plus visible une partie
de la question de recherche. Ces contributions ne suffisent pas à nommer un axe :
il faut aussi regarder les modalités de chaque côté et leurs cos².

**Ne pas assimiler position sociale et explication forte.** Les η² observés sur
les trois premiers axes sont au maximum d'environ 6,35 % (sexe, axe 2), 5,29 %
(âge, axe 2) et 4,20 % (diplôme, axe 1). Une modalité peut être bien représentée
sur un plan et avoir une position lisible tout en étant liée à une faible part
de la dispersion individuelle. Le cos² renseigne la représentation d'un point ;
le η² renseigne l'association de la variable entière avec l'axe. Ces résultats
n'excluent pas des associations sur d'autres axes ou des effets conjoints.

**Les classes restent une proposition descriptive.** Les faibles accords avec
certaines variantes indiquent que les frontières dépendent des choix de méthode,
d'axes et d'imputation. Il est préférable de chercher les oppositions et les
profils récurrents, puis de nommer les classes avec ces éléments. L'ARI compare
les affectations individuelles ; il ne mesure pas à lui seul la proximité des
interprétations sociologiques. Une analyse par rééchantillonnage serait un autre
examen de la stabilité, distinct des comparaisons déjà réalisées.

### Références

- [Documentation officielle de postStratify](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/postStratify.html) : ajustement des poids aux totaux des post-strates.
- [Analyse de domaines avec survey](https://r-survey.r-forge.r-project.org/survey/html/subset.survey.design.html) : distinguer sous-population et restriction de l'échantillon.
- [Plans à deux phases](https://r-survey.r-forge.r-project.org/survey/html/twophase.html) : prise en compte d'une sélection supplémentaire lorsque le plan de collecte la justifie.


## Bilan et conclusions

### Statut

L'analyse dispose maintenant d'une chaîne reproductible depuis les données,
d'une documentation des recodages, d'une lecture de tous les axes retenus,
de profils de classes sur réponses observées et de diagnostics de sensibilité
et de séparation. Elle peut servir de **résultat exploratoire documenté**.
La solution à quatre classes est une proposition de lecture ; les diagnostics
ne justifient pas de la présenter comme une typologie définitive de la population
française.

### Compléments ajoutés en clôture

Tous sont produits par `generer_dashboard.R`, appelé par le mode complet du
R Markdown. Ils relisent les résultats existants sans changer les poids ni les
modèles. Le dossier `3. Outputs/AFM/bilan/` contient :

| Fichier | Utilité |
|---|---|
| `01_dictionnaire_actif.csv` | Correspondance entre les 11 variables, les questions, les groupes, les recodages et les manquants |
| `02_elements_tous_axes.csv` | Coordonnées, contributions, cos² et effectifs observés sur tous les axes retenus |
| `03_poles_axes.csv` | Sélection de lecture par axe, pôle et type de variable, avec au moins 30 observations |
| `04_groupes_tous_axes.csv` | Contributions des quatre thèmes sur les cinq axes utilisés par la CAH |
| `05_separation_decoupages.csv` | Silhouettes pour 2 à 8 classes issues du même arbre pondéré |
| `06_separation_classes.csv` | Séparation de chacune des classes présentées |
| `07_autoconso_classes.csv` | Correspondance des classes avec la connaissance de l'autoconsommation, sur réponses observées |

### Résultats qui précisent l'interprétation

#### Les axes 4 et 5 comptent pour la classification

Le groupe service public/gouvernance contribue à **48,6 % de l'axe 4**.
Sur cet axe, le pôle positif associe notamment les échelles orientées vers la
concurrence et la décentralisation et la définition du service public comme
mission ; le pôle négatif comprend davantage la définition comme organisme et
la préférence pour l'autoconsommation individuelle. Les modalités n'ont pas toutes
une qualité de représentation élevée : les cos² restent nécessaires à cette lecture.

L'axe 5 est construit à **84,0 % par le groupe autoconsommation**. La seule modalité
« Autoconso : collective connue » contribue à **56,6 % de cet axe**, avec un
cos² d'environ 0,62 sur l'axe 5. Elle correspond aux personnes déclarant connaître
la forme collective sans déclarer connaître la forme individuelle ; elle ne doit
pas être confondue avec une préférence pour la forme collective.

Dans la partition actuelle, les **218 répondants observés dans cette modalité
constituent exactement la classe 2**, sans autre répondant dans cette classe.
Ce résultat ne vient pas d'une réponse imputée dans cette modalité : il apparaît
sur les réponses observées. Il établit une correspondance exacte avec un recodage,
mais ne prouve pas que l'axe 5 est à lui seul la cause de cette classe.

Cela aide à interpréter la sensibilité au nombre d'axes. Avant de donner à cette
classe un nom sociologique plus large, il faut vérifier qu'elle exprime plusieurs
caractéristiques concordantes. Pour la présenter dès maintenant, un intitulé
factuel est préférable : « connaissance déclarée de la seule forme collective ».
La distribution d'une question ne doit pas être transformée sans examen en type
social général.

#### Quatre classes : une séparation partielle

La silhouette classique, calculée sur les cinq axes non restandardisés, donne :

| Nombre de classes | Silhouette moyenne | Personnes avec silhouette négative |
|---:|---:|---:|
| 2 | 0,320 | 0,3 % |
| 3 | 0,203 | 10,9 % |
| 4 | 0,202 | 9,7 % |
| 5 | 0,172 | 14,9 % |
| 6 | 0,167 | 15,8 % |
| 7 | 0,156 | 20,1 % |
| 8 | 0,146 | 20,8 % |

Ces calculs réutilisent le même arbre de Ward pondéré : ils ne réestiment pas
l'AFM à chaque k. **Les silhouettes et leurs moyennes sont non pondérées**.
Une silhouette proche de zéro décrit une frontière peu marquée ; une silhouette
négative correspond à une distance moyenne plus faible vers une autre classe.
Ce diagnostic ne désigne pas des personnes « mal classées » au sens substantiel.

Les résultats ne sélectionnent pas nettement quatre classes. Deux classes
maximisent cet indicateur parmi les sept découpages examinés, mais le plus petit
groupe ne comprend alors que 218 personnes : maximiser la séparation ne garantit
pas une typologie riche ni équilibrée. À quatre classes, la classe 2 est la mieux
séparée (moyenne 0,363) et la classe 4 la moins séparée (0,156).

Le découpage à quatre classes peut rester une proposition descriptive si son
contenu apporte une lecture utile ; sa justification doit alors être substantielle
et accompagnée de ces résultats, plutôt que reposer sur un « optimum » statistique.

### Ce qui reste une décision de recherche

1. **Population visée et affectation long/court.** C'est la priorité : savoir si
   les longs doivent représenter tous les adultes permet de décider d'un éventuel
   recalage. La section « Calage et interprétation » ci-dessus distingue la restriction aux longs
   du filtre sur les réponses incomplètes et expose les hypothèses nécessaires.
   Les données seules ne donnent pas le protocole d'affectation.
2. **Périmètre théorique de l'AFM.** L'AFM actuelle porte sur un ensemble large
   de rapports à l'énergie. Une analyse centrée sur la gouvernance du service
   public est une question différente ; la variante resserrée permet de l'explorer.
3. **Statut et noms des classes.** Les classes ne sont pas des catégories
   naturelles. Pour un article, retenir et justifier une solution à partir du
   contenu des profils, de leur séparation et de leur sensibilité. Ne pas étendre
   les parts des classes à la population nationale avant décision sur le calage.

### Où s'arrêter et quand aller plus loin

Il n'est pas nécessaire d'ajouter encore des graphiques génériques à ce stade.
La priorité est l'examen substantiel des résultats déjà disponibles et la décision
sur la population de référence.

Un bootstrap de toute la chaîne deviendrait pertinent pour soutenir une affirmation
de stabilité d'une typologie publiée. Il ne résoudrait toutefois ni une cible de
calage inadéquate ni l'incertitude sur la définition des groupes actifs. De même,
les variantes NSP explicites ou traitement catégoriel des échelles sont à engager
si l'incertitude déclarée ou l'hypothèse d'intervalles égaux sont centrales pour
l'interprétation. Elles restent des prolongements identifiés, pas des validations
prétendument déjà réalisées.

Pour une restitution actuelle, présenter : la population et ses limites de calage,
les principaux axes (en incluant 4 et 5 pour comprendre la CAH), les profils
observés, la séparation partielle des classes et les résultats de sensibilité.

### Référence du diagnostic ajouté

[Documentation officielle de la silhouette, package cluster](https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/silhouette.html).
Les chiffres ci-dessus décrivent le calcul de clôture ; les tables et le dashboard
sont recalculés depuis R en cas de modification des données ou des paramètres.
