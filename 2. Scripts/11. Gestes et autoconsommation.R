# Les pratiques : affaire de contraintes ou de dispositions ?
#
# Le statut d'occupation est le premier facteur des gestes (ACM du script 5c,
# typologie du script 5g). Mais une partie de ce lien est mécanique : un
# locataire en appartement ne peut pas isoler. Ce script sépare ce qui relève de
# la contrainte (pouvoir agir) et ce qui relève des dispositions (vouloir agir),
# pour les gestes puis pour l'autoconsommation.
#
# 1. Les gestes. Pour chaque répondant i et chacun des neuf gestes g, on
#    construit trois variables, chacune sur un champ plus étroit que la
#    précédente, comme un entonnoir :
#      concerne(i, g) = 100 si i n'est pas "non concerné" par g   (tous)
#      fait(i, g)     = 100 si i a déjà fait g                    (concernés)
#      refus(i, g)    = 100 si i n'est "pas prêt" à faire g       (concernés qui
#                                                                 ne l'ont pas fait)
#    Chacune est expliquée par le même modèle linéaire pondéré :
#      y(i, g) = effet du geste g + profil(i) + logement(i) + dispositions(i)
#    avec le logement = statut d'occupation et type de logement, et les
#    dispositions = intérêt pour l'énergie (1 à 5) et sobriété comme levier de la
#    transition (0 à 5 points). L'effet du geste compare les répondants sur un
#    même geste. On estime ce modèle séparément pour les gestes du quotidien et
#    pour les investissements : six modèles en tout (trois étapes, deux familles).
#    Les données ont une ligne par répondant et par geste ; les écarts-types
#    tiennent compte de ce qu'un répondant compte plusieurs lignes.
#    La question : une fois la contrainte levée (étape 1), le logement joue-t-il
#    encore, ou les dispositions prennent-elles le relais ?
#
# 2. L'autoconsommation. La même logique en deux étapes : être intéressé par un
#    dispositif (individuel ou collectif), puis, parmi les intéressés, préférer
#    le collectif. Préfère-t-on le collectif par contrainte (on ne peut pas
#    installer de panneaux chez soi) ou par valeur (on le trouve plus juste) ?
#
# Questions posées aux titulaires d'un contrat ; les "ne sait pas" sont laissés
# de côté. Coefficients en points de pourcentage. Ce sont des associations :
# l'intérêt peut aussi naître de ce qu'on a agi.


###############################
### Chargement des packages ###
###############################

library(tidyverse)
library(survey)


###############################
### Importation des données ###
###############################

df <- readRDS("3. Outputs/df_recode.rds") |> filter(champ_commun, long)

dossier <- "3. Outputs/Gestes et autoconsommation"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

gestes <- c(geste_voiture = "quotidien", geste_chauffage = "quotidien",
            geste_heures_creuses = "quotidien", geste_suivi_conso = "quotidien",
            geste_information = "quotidien", geste_petits_equipements = "investissement",
            geste_gros_equipement = "investissement", geste_isolation = "investissement",
            geste_production = "investissement")

profil       <- c("vague", "age_tranche", "sexe", "diplome", "revenu", "difficultes", "zone3")
logement     <- c("statut", "logement")
dispositions <- c("interet_energie", "lev_sobriete")

# Une ligne par répondant et par geste, avec les trois étapes (en points)
g <- df |>
  mutate(id = row_number()) |>
  select(id, poids_norm, all_of(c(profil, logement, dispositions)), all_of(names(gestes))) |>
  pivot_longer(all_of(names(gestes)), names_to = "geste", values_to = "reponse") |>
  filter(!is.na(reponse), reponse != "NSP") |>
  mutate(famille  = gestes[geste],
         concerne = 100 * (reponse != "Non concerné"),
         fait     = if_else(reponse == "Non concerné", NA, 100 * (reponse == "Fait")),
         refus    = if_else(reponse %in% c("Prêt", "Pas prêt"), 100 * (reponse == "Pas prêt"), NA))


#################################################
### 1. Les gestes selon le statut            ###
#################################################

# Pour chaque geste, la répartition des réponses chez les propriétaires et les
# locataires : la contrainte se concentre-t-elle sur les investissements ?
repartition <- g |>
  filter(statut %in% c("Propriétaire", "Locataire")) |>
  count(geste, famille, statut, reponse, wt = poids_norm) |>
  group_by(geste, statut) |>
  mutate(part = 100 * n / sum(n)) |>
  select(-n) |>
  pivot_wider(names_from = reponse, values_from = part)
repartition |> mutate(across(where(is.numeric), ~ round(.x, 1))) |> print(n = Inf)
write_csv2(repartition, file.path(dossier, "01_reponses_selon_statut.csv"))

# Lecture : la contrainte se concentre sur les investissements. Les locataires
# sont "non concernés" par l'isolation dans 62 % des cas (13 % des
# propriétaires), par la production dans 59 % (18 %) et par le gros équipement
# dans 38 % (10 %). Pour les gestes du quotidien, les deux groupes répondent
# presque pareil, sauf pour la voiture (20 % de non concernés chez les
# locataires, 8 % chez les propriétaires).


#################################################
### 2. Les gestes : les trois étapes          ###
#################################################

# Un modèle par étape et par famille de gestes. Chaque modèle contient le geste
# (effets fixes), le profil, le logement et les dispositions. On garde les
# coefficients du statut, du logement, du revenu et des dispositions, et les
# tests de Wald des blocs "logement" et "dispositions".
resultats <- list()
tests <- list()

for (famille_g in c("quotidien", "investissement")) {
  for (etape in c("concerne", "fait", "refus")) {
    d <- g |> filter(famille == famille_g) |> drop_na(all_of(c(etape, profil, logement, dispositions))) |>
      droplevels()
    plan <- svydesign(ids = ~id, weights = ~poids_norm, data = d)
    modele <- svyglm(reformulate(c("geste", profil, logement, dispositions), etape), design = plan)

    resultats[[paste(famille_g, etape)]] <- summary(modele)$coefficients |>
      as.data.frame() |>
      rownames_to_column("terme") |>
      as_tibble() |>
      filter(str_detect(terme, "^statut|^logement|^revenu|^interet|^lev_")) |>
      transmute(famille = famille_g, etape, terme, estimation = Estimate, se = `Std. Error`,
                p = `Pr(>|t|)`, n_gestes = nrow(d), n_repondants = n_distinct(d$id))

    tests[[paste(famille_g, etape)]] <- tibble(
      famille = famille_g, etape,
      bloc = c("Logement (statut et type)", "Dispositions"),
      p = c(regTermTest(modele, reformulate(logement), method = "Wald")$p,
            regTermTest(modele, reformulate(dispositions), method = "Wald")$p))
  }
}

resultats <- bind_rows(resultats)
tests <- bind_rows(tests)

resultats |>
  filter(terme %in% c("statutLocataire", "logementAppartement", "revenu4 000 et +",
                      "interet_energie", "lev_sobriete")) |>
  select(famille, etape, terme, estimation, p) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  print(n = Inf)
tests |> mutate(p = signif(p, 2)) |> print(n = Inf)

write_csv2(resultats, file.path(dossier, "02_coefficients_etapes.csv"))
write_csv2(tests, file.path(dossier, "03_tests_blocs.csv"))

# Lecture : le poids du logement et celui des dispositions s'inversent d'une
# étape à l'autre.
#   - Étape 1 (être concerné) : pour les investissements, tout tient au
#     logement. Être locataire retire 27 points, vivre en appartement 11. Les
#     dispositions ne jouent presque pas (p = 0.10).
#   - Étape 2 (avoir fait, parmi les concernés) : les deux jouent pour les
#     investissements. Le logement compte encore (locataire - 5 points,
#     appartement - 11), mais chaque point d'intérêt pour l'énergie (sur 5) en
#     ajoute 5. Pour le quotidien, le logement ne joue plus (p = 0.19) : seuls
#     comptent l'intérêt (+ 5 points par point) et la sobriété (+ 1 point par
#     point de classement).
#   - Étape 3 (refuser, parmi ceux qui ne l'ont pas fait) : le logement ne joue
#     plus du tout, pour aucune famille. Le refus tient aux dispositions : chaque
#     point d'intérêt le réduit de 8 points.
# Le revenu ne joue presque pas, une fois le statut pris en compte. Seule
# exception : les plus modestes (moins de 1 000 €) sont 4 points moins souvent
# concernés par les gestes du quotidien, surtout par la voiture (22 % de non
# concernés, contre 11 % des autres). La
# contrainte décide donc de qui peut agir ; les dispositions, de qui agit et de
# qui refuse. Le refus des gestes est bien une disposition, pas une contrainte.


#################################################
### 3. Les gestes : la conviction passe-t-elle ###
###    aux actes chez les locataires ?        ###
#################################################

# À l'étape 2 (avoir fait, parmi les concernés), on laisse l'effet de la
# sobriété varier selon le statut. Si les locataires convaincus agissent moins
# que les propriétaires convaincus, la contrainte bride aussi les gestes de
# ceux qui peuvent en faire. On estime la pente de la sobriété (en points par
# point de classement, de 0 à 5) chez les propriétaires et chez les locataires.
pentes <- list()
for (famille_g in c("quotidien", "investissement")) {
  d <- g |>
    filter(famille == famille_g, statut %in% c("Propriétaire", "Locataire")) |>
    drop_na(all_of(c("fait", profil, logement, dispositions))) |>
    droplevels()
  plan <- svydesign(ids = ~id, weights = ~poids_norm, data = d)
  modele <- svyglm(reformulate(c("geste", profil, logement, dispositions, "lev_sobriete:statut"), "fait"),
                   design = plan)
  interaction <- grep(":", names(coef(modele)), value = TRUE)   # le terme croisé
  pente_loc <- svycontrast(modele, setNames(c(1, 1), c("lev_sobriete", interaction)))
  pentes[[famille_g]] <- tibble(
    famille = famille_g,
    pente_proprietaires = coef(modele)["lev_sobriete"],
    pente_locataires    = as.numeric(coef(pente_loc)),
    p_interaction       = summary(modele)$coefficients[interaction, 4])
}
pentes <- bind_rows(pentes)
pentes
write_csv2(pentes, file.path(dossier, "04_sobriete_selon_statut.csv"))

# Lecture : pour les gestes du quotidien, la conviction se traduit en actes au
# même rythme chez les propriétaires et les locataires (+ 1,2 et + 1,0 point par
# point de sobriété, p = 0.84). Pour les investissements, la sobriété ne joue
# pas chez les propriétaires (- 0,1) et un peu chez les locataires concernés
# (+ 1,1), sans que la différence soit significative (p = 0.12). Une fois
# concernés, les locataires convaincus ne sont pas bridés.


#####################################################
### L'autoconsommation : préparation              ###
#####################################################

# Les deux étapes : être intéressé par un dispositif, individuel ou collectif
# (plutôt que par aucun, ou "ne sait pas") ; parmi les intéressés, préférer le
# collectif.
#
# Ce qui pourrait encourager à s'engager (G4Q00006, posée avant la préférence).
# Ceux qui ont répondu "ne sait pas" ont toutes les cases à "N/A" : ils ont 0
# partout, et une indicatrice à part.
df <- df |>
  mutate(interesse       = 100 * (autoconso_preference %in% c("Individuelle", "Collective")),
         collectif       = if_else(autoconso_preference %in% c("Individuelle", "Collective"),
                                   100 * (autoconso_preference == "Collective"), NA),
         enc_economies   = as.numeric(G4Q00006_SQ001 == "Oui"),
         enc_cle_en_main = as.numeric(G4Q00006_SQ002 == "Oui"),
         enc_porteur     = as.numeric(G4Q00006_SQ003 == "Oui"),
         enc_souplesse   = as.numeric(G4Q00006_SQ006 == "Oui"),
         enc_regles      = as.numeric(G4Q00006_SQ007 == "Oui"),
         enc_nsp         = as.numeric(G4Q00006_SQ001 == "N/A"))

blocs <- list(
  "Profil"            = c("vague", "age_tranche", "sexe", "diplome", "revenu", "difficultes", "zone3"),
  "Logement"          = c("statut", "logement"),
  "Connaissance et intérêt" = c("autoconso_connue", "conn_reseau", "interet_energie"),
  "Solidarité territoriale" = "territoires",
  "Encouragements"    = c("enc_economies", "enc_cle_en_main", "enc_porteur", "enc_souplesse",
                          "enc_regles", "enc_nsp")
)
variables <- unlist(blocs, use.names = FALSE)

# Les préférences, en % pondérés : 52 % l'individuelle, 28 % la collective, 6 %
# aucune des deux, 14 % ne savent pas
df |> count(autoconso_preference, wt = poids_norm) |> mutate(part = round(100 * n / sum(n), 1))


#####################################################
### 4. L'autoconsommation : les deux étapes       ###
#####################################################

coefficients_autoconso <- list()
tests_autoconso <- list()
for (etape in c("interesse", "collectif")) {
  d <- df |> drop_na(all_of(c(etape, variables))) |> droplevels()
  plan <- svydesign(ids = ~1, weights = ~poids_norm, data = d)
  modele <- svyglm(reformulate(variables, etape), design = plan)

  coefficients_autoconso[[etape]] <- summary(modele)$coefficients |>
    as.data.frame() |>
    rownames_to_column("terme") |>
    as_tibble() |>
    filter(terme != "(Intercept)") |>
    transmute(etape, terme, estimation = Estimate, se = `Std. Error`, p = `Pr(>|t|)`, n = nrow(d))

  tests_autoconso[[etape]] <- tibble(etape, bloc = names(blocs), n = nrow(d),
                                   p = map_dbl(blocs, ~ regTermTest(modele, reformulate(.x), method = "Wald")$p))
}
coefficients_autoconso <- bind_rows(coefficients_autoconso)
tests_autoconso <- bind_rows(tests_autoconso)

tests_autoconso |> mutate(p = signif(p, 2))
coefficients_autoconso |>
  filter(p < 0.05, !str_detect(terme, "^vague")) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  print(n = Inf)

write_csv2(coefficients_autoconso, file.path(dossier, "05_autoconsommation_coefficients.csv"))
write_csv2(tests_autoconso, file.path(dossier, "06_autoconsommation_tests_blocs.csv"))

# Lecture : comme pour les gestes, les deux étapes ne dépendent pas des mêmes
# choses.
#   - Être intéressé ne dépend pas du logement (p = 0.57). Cela dépend de la
#     familiarité avec le sujet et de l'engagement : ne connaître aucune forme
#     d'autoconsommation retire 13 points, chaque point d'intérêt pour l'énergie
#     en ajoute 3, et les encouragements cités vont avec l'intérêt (un
#     accompagnement "clé en main" + 8 points, un porteur de confiance + 6).
#     Ceux qui répondent "ne sait pas" ailleurs le font aussi ici (- 15 et - 26
#     points).
#   - Préférer le collectif tient d'abord au logement : vivre en appartement
#     ajoute 33 points, à statut égal (être locataire n'ajoute rien de plus). Le
#     porteur de confiance compte aussi (+ 8 points). La solidarité joue peu :
#     ceux qui veulent adapter le service à chaque territoire préfèrent le
#     collectif 5 points moins souvent (p = 0.03). Les ruraux et les
#     périurbains le préfèrent un peu moins (- 5 et - 6 points).


#####################################################
### 5. L'autoconsommation : raisons et freins     ###
#####################################################

# Les raisons de préférer le collectif (G4Q00009, posée à ceux qui le
# préfèrent) et les freins à l'engagement (G04Q31, posée à tous), chez les
# propriétaires et les locataires. Parts pondérées, en %, parmi ceux qui ont
# répondu autre chose que "ne sait pas".
raisons <- c(G4Q00009_SQ001 = "Collectif : en bénéficier sans installation chez soi",
             G4Q00009_SQ002 = "Collectif : coûts partagés",
             G4Q00009_SQ003 = "Collectif : moins de démarches",
             G4Q00009_SQ004 = "Collectif : plus juste et solidaire",
             G4Q00009_SQ005 = "Collectif : plus de confiance",
             G4Q00009_SQ006 = "Collectif : adapté à mon logement",
             G4Q00009_SQ009 = "Collectif : démarche collective",
             G04Q31_SQ001   = "Frein : je suis locataire, pas la main",
             G04Q31_SQ002   = "Frein : manque d'informations",
             G04Q31_SQ003   = "Frein : pas confiance",
             G04Q31_SQ004   = "Frein : gain incertain",
             G04Q31_SQ005   = "Frein : investissement inabordable",
             G04Q31_SQ006   = "Frein : pas d'engagement durable",
             G04Q31_SQ008   = "Frein : ne pas changer ses habitudes")

parts <- df |>
  filter(statut %in% c("Propriétaire", "Locataire")) |>
  select(statut, poids_norm, all_of(names(raisons))) |>
  pivot_longer(all_of(names(raisons)), names_to = "question", values_to = "reponse") |>
  filter(reponse %in% c("Oui", "Non")) |>
  group_by(question, statut) |>
  summarise(part = 100 * weighted.mean(reponse == "Oui", poids_norm), n = n(), .groups = "drop") |>
  mutate(libelle = raisons[question]) |>
  pivot_wider(names_from = statut, values_from = c(part, n))
parts |> select(libelle, starts_with("part"), starts_with("n_")) |> mutate(across(where(is.numeric), ~ round(.x, 1)))
write_csv2(parts, file.path(dossier, "07_autoconsommation_raisons_et_freins.csv"))

# Lecture :
#   - ceux qui préfèrent le collectif mettent en avant les coûts partagés (55 à
#     61 %) et le fait d'en bénéficier sans installation chez soi (47 %). La
#     justice et la solidarité viennent après (33 à 37 %), autant chez les
#     propriétaires que chez les locataires ;
#   - les freins diffèrent selon le statut. Les locataires sont freinés par
#     leur statut (77 %). Les propriétaires, par le coût de l'investissement
#     (61 %), l'incertitude du gain (50 %) et le manque de confiance (32 %).
# La préférence pour le collectif est donc surtout une préférence par
# contrainte (le logement) et par confiance (un porteur local), plus qu'une
# préférence par valeur.
