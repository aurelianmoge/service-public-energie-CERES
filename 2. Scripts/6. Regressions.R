# Régressions : qu'est-ce qui explique les positions sur le service public de
# l'électricité et la transition ?
#
# Ce script remplace l'ancien script 6, qui estimait près de 260 modèles. On
# s'en tient ici à une seule démarche, la même pour chaque position :
#
#   1. Quatre modèles emboîtés, estimés sur les mêmes répondants :
#        M1 : profil social (âge, sexe, diplôme, revenu) ;
#        M2 : + conditions de vie (zone, logement, statut, difficultés à payer) ;
#        M3 : + rapport à l'énergie (connaissance des acteurs, intérêt) ;
#        M4 : + conception du service public (définition, absence de profit).
#      La vague d'enquête est contrôlée dans tous les modèles.
#   2. Une décomposition du R² de M4 entre les sept blocs de variables (âge,
#      sexe, diplôme, revenu, conditions de vie, rapport à l'énergie,
#      conception du service public), par la méthode de Shapley.
#   3. Des robustesses (logit, logit ordonné, nombre de gestes sur 9, sans
#      poids) et un focus sur le nucléaire.
#
# Les variables et leurs conventions sont celles du script 7 (Antoine) : mêmes
# positions (pro-nucléaire de 0 à 100 comme mesure principale du nucléaire,
# part des gestes faits hors "non concerné"), mêmes contrôles
# (zone à trois modalités, logement, statut, difficultés), même rapport à
# l'énergie (connaissance du réseau et intérêt pour l'énergie), mêmes
# robustesses. Ce script y ajoute les modèles emboîtés, la décomposition du R²
# et les deux axes factoriels.
#
# Une précaution sur les R² : ils ne sont pas comparables tels quels d'une
# position à l'autre. Une variable binaire (oui / non) a mécaniquement un R² plus
# faible qu'un score continu, à force explicative égale. On compare donc surtout
# la répartition du R² entre les blocs (les parts de Shapley), qui ne dépend pas
# de cette différence de mesure.
#
# L'ordre des blocs suppose une chaîne (le profil social précède les
# représentations) : ce sont des associations, pas des effets causaux. Ajouter la
# conception du service public peut absorber une partie de l'effet du profil.
#
# Il faut avoir lancé le script 3 (recodages) et les scripts 5a et 5b (pour les
# deux axes factoriels utilisés comme positions synthétiques).


###############################
### Chargement des packages ###
###############################

library(tidyverse)
library(survey)


###############################
### Importation des données ###
###############################

df <- readRDS("3. Outputs/df_recode.rds")

# Les deux axes principaux des analyses factorielles : le pôle "marché" du
# service public (script 5a) et l'axe nucléaire (script 5b). On inverse l'axe de
# la transition pour qu'une valeur élevée signifie "plus nucléaire".
axe_sp <- readRDS("3. Outputs/Analyses factorielles/1. Service public/coordonnees.rds")
axe_tr <- readRDS("3. Outputs/Analyses factorielles/2. Transition/coordonnees.rds")
df <- df |>
  mutate(axe_marche    = axe_sp$sp_axe1,
         axe_nucleaire = -axe_tr$tr_axe1)

dossier <- "3. Outputs/Régressions"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)


############################################
### Préparation des variables du modèle  ###
############################################

# Les positions binaires et la part de gestes sont exprimées en points (x 100) :
# un coefficient de 5 se lit "+ 5 points de pourcentage".
df <- df |>
  mutate(solidarite_pts        = 100 * solidarite,
         nucleaire_premier_pts = 100 * nucleaire_premier,
         pro_nucleaire_pts     = 100 * pro_nucleaire,
         malus_pts             = 100 * mesure_malus,
         offre_verte_pts       = 100 * offre_verte,
         gestes_part_pts       = 100 * gestes_part)

# Les "Ne sait pas" de la définition du service public sortent du modèle
df <- df |>
  mutate(definition_sp = droplevels(na_if(sp_definition, "NSP")))

# Les difficultés à payer n'ont été posées qu'aux longs : le script 3 range les
# courts dans "Jamais", et la modalité "Sans contrat" du statut absorbe leur
# écart (comme dans le script 7).

### Les sept blocs de variables explicatives

blocs <- list(
  "Âge"                  = "age_tranche",
  "Sexe"                 = "sexe",
  "Diplôme"              = "diplome",
  "Revenu"               = "revenu",
  "Conditions de vie"    = c("zone3", "logement", "statut", "difficultes"),
  "Rapport à l'énergie"  = c("conn_reseau", "interet_energie"),
  "Conception du SP"     = c("definition_sp", "sp_sans_profit")
)

# Les modèles emboîtés : chaque modèle ajoute des blocs au précédent
modeles_emboites <- list(
  M1 = c("Âge", "Sexe", "Diplôme", "Revenu"),
  M2 = c("Âge", "Sexe", "Diplôme", "Revenu", "Conditions de vie"),
  M3 = c("Âge", "Sexe", "Diplôme", "Revenu", "Conditions de vie", "Rapport à l'énergie"),
  M4 = names(blocs)
)

### Les positions à expliquer

# Les sept positions du script 7, avec leur robustesse (sur la variable "brute") :
# logit ordonné pour les échelles, logit pour les binaires et pour le
# pro-nucléaire (nucléaire en première production), nombre de gestes faits sur 9
# pour les gestes. On y ajoute les deux axes factoriels.
positions <- tribble(
  ~variable,           ~nom,                                       ~longs_seuls, ~robuste,   ~brute,
  "local",             "Gestion décentralisée (1-5)",              FALSE,        "ologit",   "local",
  "concurrence",       "Concurrence (1-5)",                        FALSE,        "ologit",   "concurrence",
  "solidarite_pts",    "Même service partout (pts)",               FALSE,        "logit",    "solidarite",
  "pro_nucleaire_pts", "Pro-nucléaire (0-100)",                    FALSE,        "logit",    "nucleaire_premier",
  "malus_pts",         "Malus acceptable (pts)",                   FALSE,        "logit",    "mesure_malus",
  "offre_verte_pts",   "Offre verte souscrite (pts)",              TRUE,         "logit",    "offre_verte",
  "gestes_part_pts",   "Gestes faits / concernés (pts)",           TRUE,         "mco_sur9", "gestes_faits",
  "axe_marche",        "Axe 1 du SP : pôle marché (ACM)",          FALSE,        "aucune",   NA,
  "axe_nucleaire",     "Axe 1 de la transition : nucléaire (ACP)", FALSE,        "aucune",   NA
)

# L'axe du service public est construit à partir des questions de définition du
# service public : on ne peut pas l'expliquer par elles.
blocs_exclus <- list(axe_marche = "Conception du SP")


##########################################
### Trois outils pour l'estimation     ###
##########################################

# L'échantillon d'une position : les répondants qui ont répondu à la position et
# à toutes les variables du modèle complet. Les huit personnes qui ont répondu
# "Autre" au sexe sortent de ce fait des modèles.
echantillon <- function(y, longs_seuls, noms_blocs) {
  variables <- unname(c(y, "vague", unlist(blocs[noms_blocs])))
  d <- if (longs_seuls) filter(df, long) else df
  d |> drop_na(all_of(variables))
}

# Le R² pondéré d'un modèle linéaire qui ne contient que certains blocs
r2 <- function(d, y, noms_blocs) {
  variables <- c("vague", unlist(blocs[noms_blocs]))
  summary(lm(reformulate(variables, y), data = d, weights = poids_norm))$r.squared
}

# La décomposition de Shapley. La contribution d'un bloc est le gain moyen de R²
# quand on l'ajoute au modèle, en moyenne sur tous les ordres possibles d'entrée
# des blocs. Les contributions s'additionnent exactement : leur somme vaut le R²
# du modèle complet, moins celui du modèle qui ne contient que la vague.
# Avec 7 blocs, cela demande d'estimer 2^7 = 128 modèles par position.
shapley <- function(d, y, noms_blocs) {
  k <- length(noms_blocs)
  # Toutes les combinaisons de blocs : la ligne i contient les blocs inclus
  combinaisons <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), k)))
  r2_combinaisons <- apply(combinaisons, 1, function(inclus) r2(d, y, noms_blocs[inclus]))
  # Avec expand.grid, ajouter le bloc j à la combinaison i mène à la ligne i + 2^(j-1)
  sapply(seq_len(k), function(j) {
    sans_j <- which(!combinaisons[, j])
    taille <- rowSums(combinaisons[sans_j, , drop = FALSE])
    poids_ordre <- factorial(taille) * factorial(k - taille - 1) / factorial(k)
    sum(poids_ordre * (r2_combinaisons[sans_j + 2^(j - 1)] - r2_combinaisons[sans_j]))
  }) |>
    setNames(noms_blocs)
}


#####################################
### Estimation, position par position ###
#####################################

resultats <- list()

for (i in seq_len(nrow(positions))) {
  y <- positions$variable[i]
  noms_blocs <- setdiff(names(blocs), blocs_exclus[[y]])
  d <- echantillon(y, positions$longs_seuls[i], noms_blocs)
  plan <- svydesign(ids = ~1, weights = ~poids_norm, data = d)

  # Les modèles emboîtés (le M4 de l'axe du SP n'a pas le dernier bloc)
  emboites <- lapply(modeles_emboites, intersect, noms_blocs)
  formules <- lapply(emboites, function(b) reformulate(c("vague", unlist(blocs[b])), y))

  resultats[[y]] <- list(
    n        = nrow(d),
    moyenne  = weighted.mean(d[[y]], d$poids_norm),
    r2       = sapply(emboites, function(b) r2(d, y, b)),
    shapley  = shapley(d, y, noms_blocs),
    M1       = svyglm(formules$M1, design = plan),
    M4       = svyglm(formules$M4, design = plan),
    plan     = plan,
    formule  = formules$M4
  )
  message(positions$nom[i], " : n = ", nrow(d))
}


###########################################
### 1. Les R² des modèles emboîtés      ###
###########################################

tableau_r2 <- positions |>
  select(variable, nom) |>
  mutate(n       = map_dbl(resultats, "n"),
         moyenne = map_dbl(resultats, "moyenne"),
         R2_M1   = map_dbl(resultats, ~ .x$r2["M1"]),
         R2_M2   = map_dbl(resultats, ~ .x$r2["M2"]),
         R2_M3   = map_dbl(resultats, ~ .x$r2["M3"]),
         R2_M4   = map_dbl(resultats, ~ .x$r2["M4"]))

tableau_r2 |> mutate(across(where(is.numeric), ~ round(.x, 3)))
write_csv2(tableau_r2, file.path(dossier, "01_R2_modeles_emboites.csv"))

# Lecture :
#   - le nucléaire se détache nettement : R² de 0.161 pour le score
#     pro-nucléaire, et 0.143 pour l'axe nucléaire de l'ACP ;
#   - ce R² vient presque entièrement du profil social : 0.140 dès M1 ;
#   - la part des gestes faits atteint 0.096, mais par le rapport à l'énergie
#     (de 0.038 en M2 à 0.096 en M3) ;
#   - les positions sur la gouvernance (décentralisation, concurrence,
#     solidarité, axe "marché" du SP) restent sous 0.045 : elles sont très peu
#     prévisibles par le profil social ou le rapport à l'énergie ;
#   - pour comparaison, l'ancien script obtenait 0.15 à 0.17 sur le score
#     pro-nucléaire, mais sur 1644 personnes seulement (classements complets
#     exigés) ; on en garde ici 2630.


#############################################
### 2. La décomposition du R² (Shapley)   ###
#############################################

tableau_shapley <- map_dfr(names(resultats), function(y) {
  tibble(variable = y, bloc = names(resultats[[y]]$shapley),
         contribution = resultats[[y]]$shapley)
}) |>
  group_by(variable) |>
  mutate(part = 100 * contribution / sum(contribution)) |>
  ungroup() |>
  left_join(positions |> select(variable, nom), by = "variable")

# En colonnes, la part du R² expliqué qui revient à chaque bloc (en %)
tableau_shapley |>
  select(nom, bloc, part) |>
  mutate(part = round(part)) |>
  pivot_wider(names_from = bloc, values_from = part)

write_csv2(tableau_shapley, file.path(dossier, "02_decomposition_shapley.csv"))

# Lecture des parts :
#   - nucléaire : le sexe pèse 37 % du R² expliqué, l'âge 24 %, le rapport à
#     l'énergie 15 % (connaissance du réseau), le revenu 10 %. Même structure
#     pour l'axe nucléaire de l'ACP (sexe 31 %, âge 25 %). C'est la seule
#     position où le sexe compte ;
#   - gouvernance (décentralisation, solidarité, axe "marché") : l'âge et les
#     conditions de vie font l'essentiel d'un R² très faible ;
#   - concurrence : la conception du service public pèse 58 % ; une définition
#     par la mission plutôt que par l'organisme va avec la concurrence ;
#   - gestes et offre verte : le rapport à l'énergie (l'intérêt surtout) pèse
#     64 % et 30 %, l'âge 16 % et 35 %.

# La figure : la contribution de chaque bloc, en points de R²
tableau_shapley |>
  mutate(nom  = fct_reorder(nom, contribution, sum),
         bloc = factor(bloc, levels = names(blocs))) |>
  ggplot(aes(x = contribution, y = nom, fill = bloc)) +
  geom_col() +
  scale_fill_manual(values = c("#235E86", "#5A8FBF", "#9BBFDD", "#CADCEB",
                               "#E0A458", "#B34E3F", "#6B8F5E")) +
  labs(title = "Ce qui explique chaque position : décomposition du R²",
       subtitle = "Contributions de Shapley ; somme = R² du modèle complet (hors vague)",
       x = "Part de la variance expliquée (R²)", y = NULL, fill = "Bloc") +
  theme_minimal(base_size = 11)
ggsave(file.path(dossier, "figure_1_decomposition_R2.pdf"), width = 11, height = 6)


###################################
### 3. Les coefficients         ###
###################################

# Les coefficients de M1 (profil social seul) et de M4 (modèle complet) : la
# comparaison montre ce que les représentations absorbent de l'effet du profil.
extraire <- function(modele, y, nom_modele) {
  s <- summary(modele)$coefficients
  ic <- confint(modele)
  tibble(variable = y, modele = nom_modele, terme = rownames(s),
         estimation = s[, 1], ecart_type = s[, 2], p = s[, 4],
         ic_bas = ic[, 1], ic_haut = ic[, 2])
}

coefficients <- map_dfr(names(resultats), function(y) {
  bind_rows(extraire(resultats[[y]]$M1, y, "M1"),
            extraire(resultats[[y]]$M4, y, "M4"))
})
write_csv2(coefficients, file.path(dossier, "03_coefficients.csv"))

# Un tableau lisible du modèle complet : une colonne par position, le
# coefficient et ses étoiles
etoiles <- function(p) case_when(p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "")

tableau_M4 <- coefficients |>
  filter(modele == "M4") |>
  mutate(cellule = paste0(formatC(estimation, format = "f", digits = 2), etoiles(p))) |>
  select(terme, variable, cellule) |>
  pivot_wider(names_from = variable, values_from = cellule, values_fill = "")
tableau_M4
write_csv2(tableau_M4, file.path(dossier, "04_tableau_M4.csv"))

# La même chose en LaTeX
knitr::kable(tableau_M4, format = "latex", booktabs = TRUE,
             caption = "Modèle complet (M4), MCO pondérés. * p < 0,05 ; ** p < 0,01 ; *** p < 0,001") |>
  writeLines(file.path(dossier, "04_tableau_M4.tex"))


#################################################
### 4. Tests par bloc et tests multiples      ###
#################################################

# Pour chaque position et chaque bloc, un test de Wald : le bloc est-il associé
# à la position, une fois les autres blocs contrôlés ? On corrige ensuite les
# p-valeurs pour les tests multiples (Benjamini-Hochberg), puisqu'on fait
# 9 positions x 7 blocs = 62 tests.
tests <- map_dfr(names(resultats), function(y) {
  noms_blocs <- setdiff(names(blocs), blocs_exclus[[y]])
  map_dfr(noms_blocs, function(b) {
    test <- regTermTest(resultats[[y]]$M4, reformulate(blocs[[b]]), method = "Wald")
    tibble(variable = y, bloc = b, p = as.numeric(test$p))
  })
}) |>
  mutate(p_BH = p.adjust(p, method = "BH"))

tests |>
  mutate(p_BH = round(p_BH, 3)) |>
  select(-p) |>
  pivot_wider(names_from = bloc, values_from = p_BH)
write_csv2(tests, file.path(dossier, "05_tests_blocs.csv"))

# Après correction, le rapport à l'énergie (8 positions sur 9) et l'âge (7) sont
# les blocs les plus souvent significatifs. Le sexe ne l'est que pour les deux
# mesures du nucléaire. Le diplôme ne l'est pour aucune position, une fois les
# autres blocs contrôlés.


#############################
### 5. Robustesse         ###
#############################

# On réestime le modèle complet autrement, comme dans le script 7 :
#   - en logit pour les positions binaires et pour le pro-nucléaire (le
#     nucléaire en première production) ;
#   - en logit ordonné pour les deux échelles de 1 à 5 ;
#   - en MCO sur le nombre de gestes faits sur 9 pour les gestes ;
#   - et, pour toutes les positions, sans les poids.
# Puis on regarde si les coefficients significatifs gardent leur signe et leur
# significativité. Les signes des coefficients se comparent d'une méthode à
# l'autre, pas leur taille.
robustesse <- map_dfr(seq_len(nrow(positions)), function(i) {
  y <- positions$variable[i]
  r <- resultats[[y]]
  principal <- extraire(r$M4, y, "Principal")

  sans_poids <- lm(r$formule, data = r$plan$variables)
  alternatifs <- list(Sans_poids = tibble(terme = names(coef(sans_poids)),
                                          estimation = coef(sans_poids),
                                          p = summary(sans_poids)$coefficients[, 4]))

  brute <- positions$brute[i]
  if (positions$robuste[i] == "logit") {
    logit <- svyglm(update(r$formule, reformulate(".", response = brute)),
                    design = r$plan, family = quasibinomial())
    s <- summary(logit)$coefficients
    alternatifs$Logit <- tibble(terme = rownames(s), estimation = s[, 1], p = s[, 4])
  }
  if (positions$robuste[i] == "mco_sur9") {
    sur9 <- svyglm(update(r$formule, reformulate(".", response = brute)), design = r$plan)
    s <- summary(sur9)$coefficients
    alternatifs$Gestes_sur_9 <- tibble(terme = rownames(s), estimation = s[, 1], p = s[, 4])
  }
  if (positions$robuste[i] == "ologit") {
    formule_ord <- update(r$formule, reformulate(".", response = paste0("factor(", brute, ")")))
    ordonne <- svyolr(formule_ord, design = r$plan)
    cf <- coef(ordonne)
    se <- sqrt(diag(vcov(ordonne)))[names(cf)]
    alternatifs$Logit_ordonne <- tibble(terme = names(cf), estimation = cf,
                                        p = 2 * pnorm(-abs(cf / se)))
  }

  map_dfr(names(alternatifs), function(a) {
    principal |>
      select(terme, estimation, p) |>
      inner_join(alternatifs[[a]], by = "terme", suffix = c("", "_alt")) |>
      filter(terme != "(Intercept)") |>
      mutate(variable = y, methode = a)
  })
}) |>
  mutate(accord = case_when(
    p < .05 & p_alt < .05 & sign(estimation) == sign(estimation_alt) ~ "les deux, même signe",
    p < .05 & p_alt < .05                                            ~ "signes opposés",
    p < .05                                                          ~ "principal seulement",
    p_alt < .05                                                      ~ "robuste seulement",
    TRUE                                                             ~ "aucun"
  ))

table(robustesse$methode, robustesse$accord)
write_csv2(robustesse, file.path(dossier, "06_robustesse.csv"))

# Aucun coefficient significatif ne change de signe, et les désaccords de
# significativité sont rares (21 sur 457 comparaisons, dont 8 pour les gestes,
# où le nombre sur 9 ne mesure pas la même chose que la part) : les résultats
# ne tiennent ni aux poids, ni au choix du modèle linéaire.


#####################################
### 6. Focus sur le nucléaire     ###
#####################################

### Les moyennes pondérées, par sexe et par âge

# Le résultat le plus net, décrit sans modèle : le score pro-nucléaire moyen
# (de 0 à 100), et la part de répondants qui placent le nucléaire en première
# production
plan_tous <- svydesign(ids = ~1, weights = ~poids_norm,
                       data = df |> filter(!is.na(pro_nucleaire), !is.na(sexe)))
parts_nucleaire <- svyby(~ pro_nucleaire_pts + nucleaire_premier_pts, ~ sexe + AGE_large,
                         plan_tous, svymean)
parts_nucleaire
write_csv2(as.data.frame(parts_nucleaire), file.path(dossier, "07_nucleaire_sexe_age.csv"))

# À tout âge, les hommes sont nettement plus pro-nucléaires que les femmes :
# 46 contre 23 chez les 18-34 ans, 71 contre 50 chez les 65 ans et plus. En
# première production, l'écart est du simple au double : 41 % contre 17 % chez
# les 18-34 ans, 67 % contre 40 % chez les 65 ans et plus.

### L'écart entre hommes et femmes varie-t-il avec l'âge ou le diplôme ?

r <- resultats$pro_nucleaire_pts
interaction_age     <- svyglm(update(r$formule, . ~ . + sexe:age_tranche), design = r$plan)
interaction_diplome <- svyglm(update(r$formule, . ~ . + sexe:diplome), design = r$plan)
regTermTest(interaction_age, ~ sexe:age_tranche)
regTermTest(interaction_diplome, ~ sexe:diplome)
summary(interaction_diplome)$coefficients[grep("sexeFemme", names(coef(interaction_diplome))), ]
# Avec le score pro-nucléaire, l'écart entre femmes et hommes varie avec l'âge
# (p = 0.018) : il est le plus grand chez les 18-34 ans (- 25 points, à autres
# caractéristiques égales) et le plus faible chez les 35-49 ans (- 12 points).
# Il ne varie pas significativement avec le diplôme (p = 0.23), même si l'écart
# se réduit chez les bac+5 (+ 10 points, p = 0.02).
# Attention : avec le nucléaire en première production (ancienne version de ce
# script), c'était l'inverse (âge : p = 0.28 ; diplôme : p = 0.018). Ces
# interactions dépendent donc de la mesure et restent fragiles.


##################################################################
### 7. L'offre verte : la disposition à payer devient-elle un choix ? ###
##################################################################

# Hypothèse H5 (bloc D du script 7) : la disposition à payer plus ne mène à
# souscrire une offre verte que si l'on n'est pas sceptique envers ces offres.
# Les contrôles sont ceux de M2, c'est-à-dire ceux du script 7 (profil, zone,
# logement, statut, difficultés).
d <- df |>
  filter(long) |>
  drop_na(offre_verte_pts, payer_vert, sceptique, vague, all_of(unname(unlist(blocs[modeles_emboites$M2]))))
plan_vert <- svydesign(ids = ~1, weights = ~poids_norm, data = d)
offre_verte <- svyglm(reformulate(c("payer_vert * sceptique", "vague",
                                    unlist(blocs[modeles_emboites$M2])), "offre_verte_pts"),
                      design = plan_vert)
summary(offre_verte)$coefficients[c("payer_vert", "sceptique", "payer_vert:sceptique"), ]

# La même chose en logit, comme dans le script 7. En logit, l'interaction ne
# mesure pas la même chose qu'en probabilité : à taux de base faible, un simple
# décalage de niveau réduit la pente en points.
offre_verte_logit <- svyglm(reformulate(c("payer_vert * sceptique", "vague",
                                          unlist(blocs[modeles_emboites$M2])), "offre_verte"),
                            design = plan_vert, family = quasibinomial())
summary(offre_verte_logit)$coefficients[c("payer_vert", "sceptique", "payer_vert:sceptique"), ]
# Chaque point de disposition à payer va avec 7.0 points de souscription en plus.
# Le scepticisme réduit le niveau (- 5.7 points, p = 0.11 ; nettement en logit,
# p < 0.001) mais pas la pente (interaction : p = 0.08 en probabilité, 0.70 en
# logit) : l'hypothèse n'est pas confirmée.


#######################
### Sauvegarde      ###
#######################

saveRDS(list(positions = positions, resultats = resultats, tableau_r2 = tableau_r2,
             shapley = tableau_shapley, coefficients = coefficients, tests = tests,
             robustesse = robustesse, nucleaire = list(parts = parts_nucleaire,
             interaction_age = interaction_age, interaction_diplome = interaction_diplome),
             offre_verte = offre_verte, offre_verte_logit = offre_verte_logit),
        file.path(dossier, "resultats_regressions.rds"))
