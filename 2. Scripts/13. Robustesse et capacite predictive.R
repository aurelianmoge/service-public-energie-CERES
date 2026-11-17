# Robustesse : nos modèles prédisent-ils, et nos mesures tiennent-elles ?
#
# Une analyse de sensibilité sur l'ensemble des modèles du projet, en trois
# parties.
#
# 1. La capacité prédictive. Pour chaque modèle (les positions des blocs A2, B
#    et F, les perceptions P3 et les instruments I3 d'Antoine, l'offre verte du
#    bloc D, le modèle séquentiel des gestes et l'autoconsommation du script
#    11), on compare trois R² :
#      - le R² habituel, sur les répondants qui ont servi à estimer le modèle ;
#      - le R² du même modèle linéaire sur des répondants qu'il n'a pas vus
#        (validation croisée en cinq parts : on estime sur quatre cinquièmes de
#        l'échantillon, on prédit le cinquième restant, et on recommence cinq
#        fois) ;
#      - le R² d'une forêt aléatoire dans les mêmes conditions. La forêt trouve
#        seule les interactions et les effets non linéaires, sans qu'on les
#        écrive.
#    Deux lectures. L'écart entre le premier et le deuxième R² mesure le
#    surajustement : la part du R² qui tient au hasard de l'échantillon. Elle est
#    inévitable quand le lien est faible et les coefficients nombreux. L'écart
#    entre le deuxième et le troisième dit s'il reste un effet caché : si la
#    forêt ne fait pas mieux que la régression, nos modèles n'oublient rien
#    d'important.
#
# 2. Les mêmes conclusions par une autre méthode. Pour le modèle F, on compare
#    le poids de chaque bloc de variables selon la forêt (importance par
#    permutation) et selon la décomposition de Shapley du script 6. Si les deux
#    classent les blocs dans le même ordre, l'interprétation ne dépend pas de la
#    méthode.
#
# 3. La mesure. Les opinions sont-elles peu liées parce que chacune est mesurée
#    par une seule question ? On recalcule leurs corrélations de façon adaptée à
#    des réponses ordinales (corrélations polychoriques), puis on les corrige de
#    l'erreur de mesure. Et le refus des gestes est-il une façon de répondre
#    (cocher la même case partout) ?
#
# Les spécifications des modèles d'Antoine (P3, I3, bloc D) sont reprises de
# ses notes de tableaux dans le document Overleaf. Il faut avoir lancé les
# scripts 3, 5c, 5f et 6.


###############################
### Chargement des packages ###
###############################

library(tidyverse)
library(ranger)
library(polycor)
library(FactoMineR)


###############################
### Importation des données ###
###############################

df <- readRDS("3. Outputs/df_recode.rds") |> filter(champ_commun)

dossier <- "3. Outputs/Robustesse"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

# Une position de 1 à 5 en trois classes et une modalité "ne sait pas", comme
# dans les modèles P3 et I3
en_classes <- function(x) {
  factor(case_when(x <= 2 ~ "1-2", x == 3 ~ "3", x >= 4 ~ "4-5", TRUE ~ "NSP"),
         levels = c("1-2", "3", "4-5", "NSP"))
}

df <- df |>
  mutate(
    # Les positions, en points, comme dans les scripts 6 et 7
    solidarite_pts    = 100 * solidarite,
    pro_nucleaire_pts = 100 * pro_nucleaire,
    malus_pts         = 100 * mesure_malus,
    offre_verte_pts   = 100 * offre_verte,
    gestes_part_pts   = 100 * gestes_part,
    definition_sp     = droplevels(na_if(sp_definition, "NSP")),
    # Les perceptions expliquées par P3 (en points ; "les deux" en référence
    # de la définition)
    def_organisme = if_else(sp_definition == "NSP", NA, 100 * (sp_definition == "Organisme public")),
    def_mission   = if_else(sp_definition == "NSP", NA, 100 * (sp_definition == "Mission d'intérêt général")),
    across(c(sp_acces, sp_controle, sp_egalite, sp_sans_profit, sp_solidarite,
             garant_etat, garant_collectivites, garant_ent_publiques, garant_ent_privees, garant_individus),
           ~ 100 * .x, .names = "{.col}_pts"),
    # Les instruments expliqués par I3 : mesures en points, nombre de mesures,
    # leviers en score de rang de 0 à 100
    across(c(mesure_bonus, mesure_malus, mesure_invest_public, mesure_regulation, mesure_information),
           ~ 100 * .x, .names = "{.col}_pts"),
    nb_mesures = mesure_bonus + mesure_malus + mesure_invest_public + mesure_regulation + mesure_information,
    across(starts_with("lev_"), ~ 20 * .x, .names = "{.col}_score"),
    # Les positions de gouvernance en classes (P3 et I3)
    gestion = en_classes(local),
    marche  = en_classes(concurrence),
    # Les perceptions explicatives de I3 : un "ne sait pas" forme une
    # indicatrice à part, et personne ne sort du modèle
    definition_nsp     = sp_definition,
    nsp_caracteristiques = as.numeric(is.na(sp_sans_profit)),
    nsp_garants          = as.numeric(is.na(garant_etat)),
    sans_profit_0        = coalesce(sp_sans_profit, 0),
    etat_0               = coalesce(garant_etat, 0),
    collectivites_0      = coalesce(garant_collectivites, 0)
  )

# Les blocs de variables
profil <- c("vague", "age_tranche", "sexe", "diplome", "revenu", "difficultes", "logement", "statut", "zone3")
energie <- c("conn_reseau", "interet_energie")
gouvernance <- c("gestion", "marche", "territoires")
perceptions <- c("definition_nsp", "sans_profit_0", "etat_0", "collectivites_0",
                 "nsp_caracteristiques", "nsp_garants")

positions <- c(local = "Gestion décentralisée (1-5)", concurrence = "Concurrence (1-5)",
               solidarite_pts = "Même service partout (pts)", pro_nucleaire_pts = "Pro-nucléaire (0-100)",
               malus_pts = "Malus acceptable (pts)", offre_verte_pts = "Offre verte souscrite (pts)",
               gestes_part_pts = "Gestes (pts)")

# La liste des modèles : un nom, sa variable expliquée et ses variables
# explicatives
modeles <- bind_rows(
  tibble(modele = "A2", y = names(positions), x = list(profil)),
  tibble(modele = "B",  y = names(positions), x = list(c(profil, energie))),
  tibble(modele = "F",  y = names(positions), x = list(c(profil, energie, "definition_sp", "sp_sans_profit"))),
  tibble(modele = "P3", y = c("def_organisme", "def_mission", "sp_acces_pts", "sp_controle_pts", "sp_egalite_pts",
                              "sp_sans_profit_pts", "sp_solidarite_pts", "garant_etat_pts",
                              "garant_collectivites_pts", "garant_ent_publiques_pts",
                              "garant_ent_privees_pts", "garant_individus_pts"),
         x = list(c(profil, energie, gouvernance))),
  tibble(modele = "I3", y = c("mesure_bonus_pts", "mesure_malus_pts", "mesure_invest_public_pts",
                              "mesure_regulation_pts", "mesure_information_pts", "nb_mesures",
                              "lev_sobriete_score", "lev_efficacite_score", "lev_renouvelables_score",
                              "lev_nucleaire_score", "lev_mobilites_score", "lev_reseau_score"),
         x = list(c(profil, energie, perceptions, gouvernance))),
  tibble(modele = "D", y = "offre_verte_pts", x = list(c(profil, "payer_vert", "sceptique")))
)


#####################################################
### 1. La capacité prédictive                     ###
#####################################################

# Le R² pondéré de prévisions
r2 <- function(y, prevision, w) 1 - sum(w * (y - prevision)^2) / sum(w * (y - weighted.mean(y, w))^2)

# Les trois R² d'un modèle. "groupes" désigne le répondant : quand il compte
# plusieurs lignes (les gestes), toutes ses lignes vont dans la même part, pour
# que le modèle ne soit jamais testé sur quelqu'un qu'il a déjà vu.
# L'interaction du bloc D (disposition à payer x scepticisme) est ajoutée au
# modèle linéaire ; la forêt la trouve seule.
trois_r2 <- function(d, y, x, groupes = seq_len(nrow(d)), interaction = NULL) {
  formule_lineaire <- reformulate(c(x, interaction), y)
  formule_foret    <- reformulate(x, y)
  parts <- sample(1:5, n_distinct(groupes), replace = TRUE)[match(groupes, unique(groupes))]
  prevision_lineaire <- prevision_foret <- numeric(nrow(d))
  for (k in 1:5) {
    apprentissage <- d[parts != k, ]
    lineaire <- lm(formule_lineaire, data = apprentissage, weights = poids_norm)
    foret <- ranger(formule_foret, data = apprentissage, case.weights = apprentissage$poids_norm,
                    num.trees = 300, min.node.size = 20, seed = 2026)
    prevision_lineaire[parts == k] <- predict(lineaire, d[parts == k, ])
    prevision_foret[parts == k]    <- predict(foret, d[parts == k, ])$predictions
  }
  tibble(n = nrow(d),
         r2_echantillon   = summary(lm(formule_lineaire, data = d, weights = poids_norm))$r.squared,
         r2_lineaire_test = r2(d[[y]], prevision_lineaire, d$poids_norm),
         r2_foret_test    = r2(d[[y]], prevision_foret, d$poids_norm))
}

set.seed(2026)

### Les modèles à une ligne par répondant
predictif <- pmap_dfr(modeles, function(modele, y, x) {
  d <- df |> drop_na(all_of(c(y, x))) |> droplevels()
  interaction <- if (modele == "D") "payer_vert:sceptique" else NULL
  trois_r2(d, y, x, interaction = interaction) |> mutate(modele = modele, variable = y, .before = 1)
})

### Le modèle séquentiel des gestes (script 11)

gestes <- c(geste_voiture = "quotidien", geste_chauffage = "quotidien",
            geste_heures_creuses = "quotidien", geste_suivi_conso = "quotidien",
            geste_information = "quotidien", geste_petits_equipements = "investissement",
            geste_gros_equipement = "investissement", geste_isolation = "investissement",
            geste_production = "investissement")
x_gestes <- c("geste", "vague", "age_tranche", "sexe", "diplome", "revenu", "difficultes", "zone3",
              "statut", "logement", "interet_energie", "lev_sobriete")
g <- df |>
  filter(long) |>
  mutate(id = row_number()) |>
  pivot_longer(all_of(names(gestes)), names_to = "geste", values_to = "reponse") |>
  filter(!is.na(reponse), reponse != "NSP") |>
  mutate(geste    = factor(geste),
         famille  = gestes[as.character(geste)],
         concerne = 100 * (reponse != "Non concerné"),
         fait     = if_else(reponse == "Non concerné", NA, 100 * (reponse == "Fait")),
         refus    = if_else(reponse %in% c("Prêt", "Pas prêt"), 100 * (reponse == "Pas prêt"), NA))

predictif_gestes <- expand_grid(famille_g = c("quotidien", "investissement"),
                                etape = c("concerne", "fait", "refus")) |>
  pmap_dfr(function(famille_g, etape) {
    d <- g |> filter(famille == famille_g) |> drop_na(all_of(c(etape, x_gestes))) |> droplevels()
    trois_r2(d, etape, x_gestes, groupes = d$id) |>
      mutate(modele = "Gestes", variable = paste(famille_g, etape, sep = " : "), .before = 1)
  })

### L'autoconsommation (script 11)

autoconso <- df |>
  filter(long) |>
  mutate(interesse = 100 * (autoconso_preference %in% c("Individuelle", "Collective")),
         collectif = if_else(autoconso_preference %in% c("Individuelle", "Collective"),
                             100 * (autoconso_preference == "Collective"), NA),
         enc_economies   = as.numeric(G4Q00006_SQ001 == "Oui"),
         enc_cle_en_main = as.numeric(G4Q00006_SQ002 == "Oui"),
         enc_porteur     = as.numeric(G4Q00006_SQ003 == "Oui"),
         enc_souplesse   = as.numeric(G4Q00006_SQ006 == "Oui"),
         enc_regles      = as.numeric(G4Q00006_SQ007 == "Oui"),
         enc_nsp         = as.numeric(G4Q00006_SQ001 == "N/A"))
x_autoconso <- c(profil, "autoconso_connue", "conn_reseau", "interet_energie", "territoires",
                 "enc_economies", "enc_cle_en_main", "enc_porteur", "enc_souplesse", "enc_regles", "enc_nsp")
predictif_autoconso <- map_dfr(c("interesse", "collectif"), function(y) {
  d <- autoconso |> drop_na(all_of(c(y, x_autoconso))) |> droplevels()
  trois_r2(d, y, x_autoconso) |> mutate(modele = "Autoconsommation", variable = y, .before = 1)
})

predictif <- bind_rows(predictif, predictif_gestes, predictif_autoconso) |>
  mutate(surajustement = r2_echantillon - r2_lineaire_test,
         gain_foret    = r2_foret_test - r2_lineaire_test)
predictif |> mutate(across(where(is.numeric) & !n, ~ round(.x, 3))) |> print(n = Inf)
write_csv2(predictif, file.path(dossier, "01_capacite_predictive.csv"))

# Lecture : on retrouve exactement les échantillons et les R² d'Antoine (2 986,
# 2 996 et 2 957 répondants pour P3, 2 923 et 2 927 pour I3, 2 072 pour le bloc
# D), ce qui confirme que ses spécifications sont bien reprises.
#   - La forêt ne fait jamais nettement mieux que le modèle linéaire. Le gain
#     dépasse 0,01 pour 4 modèles sur 54, et 0,02 pour un seul : être concerné
#     par les investissements (0,238 contre 0,218), sans doute parce que le
#     statut et le type de logement se combinent. Il n'y a pas d'effet caché
#     important.
#   - Le surajustement sépare deux familles de modèles. Ceux qui expliquent
#     quelque chose gardent l'essentiel de leur R² sur des répondants nouveaux :
#     le nucléaire (0,16 puis 0,14), les gestes (0,10 puis 0,08), le nombre de
#     mesures acceptées (0,12 puis 0,08), l'offre verte selon la disposition à
#     payer et le scepticisme (0,11 puis 0,08), le modèle séquentiel des gestes
#     (0,20 à 0,23, presque sans perte) et l'autoconsommation (0,19 à 0,21, puis
#     0,15 à 0,18). Les autres, dont le R² ne dépasse pas 0,05, perdent presque
#     tout : 22 modèles sur 54 prédisent moins de 1 % de la variance sur des
#     répondants nouveaux, dont la plupart des perceptions (P3), la gouvernance
#     et le malus.
# Les coefficients significatifs de ces modèles faibles restent des écarts
# réels entre groupes, mais le profil ne permet pas de prédire la position d'une
# personne. C'est à dire dans le texte : ces positions sont peu ancrées
# socialement, pas mal modélisées.


#####################################################
### 2. Les blocs selon la forêt et selon Shapley  ###
#####################################################

# L'importance par permutation d'une variable : de combien la prévision de la
# forêt se dégrade quand on mélange au hasard les valeurs de cette variable. On
# l'additionne par bloc (les blocs de la décomposition de Shapley du script 6),
# et on ramène les blocs à 100 %. La vague n'entre pas dans la décomposition de
# Shapley : on la laisse de côté.
blocs_shapley <- list("Âge" = "age_tranche", "Sexe" = "sexe", "Diplôme" = "diplome", "Revenu" = "revenu",
                      "Conditions de vie" = c("zone3", "logement", "statut", "difficultes"),
                      "Rapport à l'énergie" = energie,
                      "Conception du SP" = c("definition_sp", "sp_sans_profit"))
x_F <- c(profil, energie, "definition_sp", "sp_sans_profit")
shapley <- read_csv2("3. Outputs/Régressions/02_decomposition_shapley.csv", show_col_types = FALSE)

comparaison <- map_dfr(names(positions), function(y) {
  d <- df |> drop_na(all_of(c(y, x_F))) |> droplevels()
  foret <- ranger(reformulate(x_F, y), data = d, case.weights = d$poids_norm, num.trees = 500,
                  min.node.size = 20, importance = "permutation", seed = 2026)
  importance <- pmax(foret$variable.importance, 0)
  par_bloc <- sapply(blocs_shapley, function(v) sum(importance[v]))
  tibble(variable = y, bloc = names(blocs_shapley), part_foret = 100 * par_bloc / sum(par_bloc)) |>
    left_join(shapley |> select(variable, bloc, part_shapley = part), by = c("variable", "bloc"))
})

# Pour chaque position : la corrélation de rang entre les deux classements des
# blocs, et le premier bloc selon chaque méthode
accord <- comparaison |>
  group_by(variable) |>
  summarise(correlation_rangs = cor(part_foret, part_shapley, method = "spearman"),
            premier_foret     = bloc[which.max(part_foret)],
            premier_shapley   = bloc[which.max(part_shapley)])
accord
write_csv2(comparaison, file.path(dossier, "02_blocs_foret_et_shapley.csv"))
write_csv2(accord, file.path(dossier, "03_accord_foret_shapley.csv"))

# Lecture : les deux méthodes désignent le même premier bloc pour 5 positions
# sur 7, et classent les blocs dans un ordre proche (corrélations de rang de
# 0,61 à 0,96). L'accord est presque parfait pour le nucléaire (0,96 : le sexe
# puis l'âge), la concurrence (0,86 : la conception du service public) et les
# gestes (0,82 : la connaissance et l'intérêt). Les deux désaccords portent sur
# des positions très peu expliquées : pour l'offre verte, l'âge et le rapport à
# l'énergie sont à égalité (35 et 30 % selon Shapley, 30 et 31 % selon la
# forêt) ; pour le même service partout, les conditions de vie et l'âge aussi.
# Les conclusions tirées de la décomposition de Shapley ne dépendent donc pas de
# la méthode.


#####################################################
### 3. La mesure                                  ###
#####################################################

### Les opinions, corrigées de l'erreur de mesure

# Les treize questions et les trois blocs du script 5f, sur les mêmes répondants
questions <- readRDS("3. Outputs/Analyses factorielles/6. Cohérence entre blocs/coherence.rds")$questions
d <- df |> drop_na(all_of(questions$variable))
nrow(d)  # 2154

# Pour les corrélations polychoriques, les échelles, les points de classement
# et les 0/1 deviennent des facteurs ordonnés ; le score pro-nucléaire reste
# numérique (corrélation polysériale). Corrélations non pondérées, pour comparer
# les deux méthodes à l'identique.
ordonnees <- d[questions$variable] |>
  mutate(across(-pro_nucleaire, ~ factor(.x, ordered = TRUE)))
r_pearson      <- cor(d[questions$variable])
r_polychorique <- hetcor(as.data.frame(ordonnees), std.err = FALSE)$correlations

# La part de variance partagée en moyenne par deux questions (r² moyen, en %),
# dans un même bloc et entre deux blocs. Si chaque question ne mesure l'opinion
# qu'avec une fiabilité "f", la vraie corrélation est r / f : le r² est divisé
# par f². On essaie plusieurs fiabilités. Repère : les deux classements du
# nucléaire, qui mesurent la même opinion, sont corrélés à 0,68.
au_dessus <- upper.tri(r_pearson)
meme_bloc <- outer(questions$bloc, questions$bloc, "==")

coherence <- expand_grid(methode = c("Pearson", "Polychorique"), fiabilite = c(1, 0.8, 0.7, 0.6, 0.5)) |>
  filter(methode == "Polychorique" | fiabilite == 1) |>
  mutate(r = if_else(methode == "Pearson", list(r_pearson), list(r_polychorique)),
         dans_un_bloc     = map2_dbl(r, fiabilite, ~ 100 * mean((.x / .y)[au_dessus & meme_bloc]^2)),
         entre_les_blocs  = map2_dbl(r, fiabilite, ~ 100 * mean((.x / .y)[au_dessus & !meme_bloc]^2)),
         plus_forte_entre = map2_dbl(r, fiabilite, ~ max(abs(.x / .y)[au_dessus & !meme_bloc]))) |>
  select(-r)
coherence |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv2(coherence, file.path(dossier, "04_coherence_corrigee.csv"))

### Le refus des gestes : une façon de répondre ?

# Ceux qui donnent la même réponse aux neuf gestes
longs <- df |> filter(long)
noms_gestes <- c(geste_voiture = "Limiter la voiture", geste_chauffage = "Baisser le chauffage",
                 geste_heures_creuses = "Heures creuses", geste_suivi_conso = "Suivre sa conso",
                 geste_information = "S'informer", geste_petits_equipements = "Petits équipements",
                 geste_gros_equipement = "Gros équipement", geste_isolation = "Isolation",
                 geste_production = "Produire son énergie")
reponses <- longs[names(noms_gestes)] |> mutate(across(everything(), as.character))
serie <- apply(reponses, 1, n_distinct) == 1
table(reponses$geste_voiture[serie])

# L'ACM du script 5c, avec tous les répondants puis sans les réponses en série.
# On compare l'inertie des axes (taux de Benzécri) et, pour les répondants
# gardés, leur coordonnée sur l'axe du refus dans les deux analyses.
acm_gestes <- function(garder) {
  actives <- map2_dfc(names(noms_gestes), noms_gestes, function(v, nom) {
    x <- droplevels(fct_na_value_to_level(factor(longs[[v]][garder]), "NSP"))
    tibble(!!nom := fct_relabel(x, ~ paste0(nom, " : ", .x)))
  }) |> as.data.frame()
  modalites <- unlist(lapply(actives, levels))
  acm <- MCA(actives, excl = grep(": NSP$", modalites), row.w = longs$poids_norm[garder],
             ncp = 3, graph = FALSE)
  Q <- ncol(actives)
  vp <- acm$eig[acm$eig[, 1] > 1 / Q, 1]
  taux <- ((Q / (Q - 1)) * (vp - 1 / Q))^2
  list(acm = acm, benzecri = 100 * taux[1:3] / sum(taux))
}
tous <- acm_gestes(rep(TRUE, nrow(longs)))
sans_serie <- acm_gestes(!serie)

refus <- tibble(analyse = c("Tous les répondants", "Sans les réponses en série"),
                n = c(nrow(longs), sum(!serie)),
                axe_1 = c(tous$benzecri[1], sans_serie$benzecri[1]),
                axe_2 = c(tous$benzecri[2], sans_serie$benzecri[2]),
                axe_3 = c(tous$benzecri[3], sans_serie$benzecri[3]),
                correlation_axe_1 = c(1, abs(cor(tous$acm$ind$coord[!serie, 1], sans_serie$acm$ind$coord[, 1]))))
refus
write_csv2(refus, file.path(dossier, "05_refus_sans_reponses_en_serie.csv"))

# Les modalités qui font l'axe 1 sans les réponses en série
sort(sans_serie$acm$var$contrib[, 1], decreasing = TRUE)[1:9] |> round(1)

# Lecture :
#   - la mesure explique une partie de la faiblesse des liens entre opinions,
#     mais pas l'essentiel. Les corrélations polychoriques doublent la part de
#     variance partagée (de 1,8 à 3,4 % dans un même bloc, de 0,36 à 0,77 % entre
#     deux blocs). Corrigées en plus de l'erreur de mesure avec une fiabilité
#     pessimiste de 0,6, deux questions de blocs différents partagent 2,2 % de
#     leur variance (une corrélation typique de 0,15). Il vaut mieux écrire
#     "faiblement liés" que "indépendants" ;
#   - les réponses en série sont rares (83 répondants, 3 %), et ce sont surtout
#     des "déjà fait" partout (62) ; seuls 7 ont coché "pas prêt" partout. Sans
#     eux, l'ACM ne change pas : l'axe 1 garde 45,6 % de l'inertie, les
#     coordonnées y sont corrélées à 0,97 à celles de l'analyse complète, et
#     l'axe est toujours fait des "pas prêt". Le refus des gestes est une
#     disposition, pas un artefact de réponse.
