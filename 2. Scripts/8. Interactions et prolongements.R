# Prolongements des régressions : interactions, écart de genre sur le nucléaire,
# gestes en logit fractionnaire, et tableaux de présentation.
#
# Le script 6 estime les mêmes modèles pour toutes les positions. Celui-ci
# reprend quelques résultats pour aller plus loin :
#
#   1. Cinq interactions, choisies à l'avance pour des raisons de fond (et non
#      parce qu'elles sortent significatives), présentées par des valeurs
#      prédites plutôt que par des coefficients croisés ;
#   2. L'écart entre femmes et hommes sur le nucléaire : quelle part passe par
#      la connaissance, l'intérêt et la conception du service public ?
#   3. La part des gestes faits en logit fractionnaire, mieux adapté à une
#      proportion que les MCO du script 6 ;
#   4. Les tableaux (Word et LaTeX) et les figures à présenter.
#
# Les variables, les contrôles et les poids sont ceux du script 6. Comme dans le
# script 6, ce sont des associations, pas des effets causaux.
#
# Il faut avoir lancé le script 3 (recodages). Packages supplémentaires :
# marginaleffects (valeurs prédites), modelsummary et flextable (tableaux).


###############################
### Chargement des packages ###
###############################

library(tidyverse)
library(survey)
library(marginaleffects)
library(modelsummary)
library(flextable)


###############################
### Importation des données ###
###############################

df <- readRDS("3. Outputs/df_recode.rds")

dossier <- "3. Outputs/Régressions/Prolongements"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

# Mêmes préparations que dans le script 6 : les positions binaires et la part de
# gestes en points (x 100), et la définition du service public sans les NSP
df <- df |>
  mutate(pro_nucleaire_pts     = 100 * pro_nucleaire,
         nucleaire_premier_pts = 100 * nucleaire_premier,
         malus_pts             = 100 * mesure_malus,
         offre_verte_pts       = 100 * offre_verte,
         gestes_part_pts       = 100 * gestes_part,
         definition_sp         = droplevels(na_if(sp_definition, "NSP")))

# Les blocs de contrôles du script 6
profil     <- c("age_tranche", "sexe", "diplome", "revenu")
vie        <- c("zone3", "logement", "statut", "difficultes")
energie    <- c("conn_reseau", "interet_energie")
conception <- c("definition_sp", "sp_sans_profit")

# L'ordre dans lequel on affiche les modalités sur les figures (les facteurs ont
# pour première modalité leur référence, pas leur ordre naturel)
ordre_affichage <- c("18-34", "35-49", "50-64", "65+",
                     "< 1 000", "1 000-1 500", "1 500-2 500", "2 500-4 000", "4 000 et +",
                     "Organisme public", "Les deux", "Mission d'intérêt général")

couleurs <- c("#235E86", "#B34E3F", "#E0A458", "#6B8F5E", "#5A8FBF")


#############################################
### 1. Cinq interactions choisies à l'avance ###
#############################################

# Chaque ligne est une hypothèse :
#   A. La connaissance du réseau augmente-t-elle avec l'âge autant chez les
#      femmes que chez les hommes ? (noté dans la première version du script 6)
#   B. L'écart de genre sur le nucléaire varie-t-il avec l'âge ? (script 6)
#   C. Le lien entre connaissance du système et soutien à la concurrence
#      dépend-il de la façon dont on définit le service public ?
#   D. Le refus du malus sur les énergies fossiles est-il plus fort chez les
#      ruraux modestes, qui dépendent de la voiture et du fioul ? (hypothèse
#      "gilets jaunes")
#   E. L'intérêt pour l'énergie se traduit-il en gestes à tous les niveaux de
#      revenu ? (même logique que l'hypothèse de l'offre verte du script 6)
#
# Pour chaque hypothèse : la position expliquée (y), l'interaction, les contrôles,
# la population, puis ce qu'on fait varier sur la figure. La variable "x" est en
# abscisse ; la variable "groupe" donne les couleurs, et prend les valeurs
# indiquées (toutes ses modalités si rien n'est indiqué).
# La connaissance du réseau étant la position expliquée en A, elle n'a que le
# profil et les conditions de vie comme contrôles.
interactions <- tribble(
  ~code, ~y,                  ~interaction,                     ~controles,                             ~longs_seuls, ~x,              ~groupe,           ~valeurs_groupe, ~titre,
  "A",   "conn_reseau",       "sexe * age_tranche",             c(profil, vie),                         FALSE,        "age_tranche",   "sexe",            NULL,            "Connaissance du réseau (0-2)",
  "B",   "pro_nucleaire_pts", "sexe * age_tranche",             c(profil, vie, energie, conception),    FALSE,        "age_tranche",   "sexe",            NULL,            "Score pro-nucléaire (0-100)",
  "C",   "concurrence",       "conn_reseau * definition_sp",    c(profil, vie, energie, conception),    FALSE,        "definition_sp", "conn_reseau",     c(0, 2),         "Soutien à la concurrence (1-5)",
  "D",   "malus_pts",         "revenu * zone3",                 c(profil, vie, energie, conception),    FALSE,        "revenu",        "zone3",           NULL,            "Malus sur les fossiles jugé acceptable (%)",
  "E",   "gestes_part_pts",   "interet_energie * revenu",       c(profil, vie, energie, conception),    TRUE,         "revenu",        "interet_energie", c(2, 5),         "Part des gestes faits, hors « non concerné » (%)"
)

modeles_interaction <- list()
tests_interaction   <- list()
predites            <- list()
ecarts              <- list()

for (i in seq_len(nrow(interactions))) {
  h <- interactions[i, ]
  y <- h$y
  x <- h$x
  groupe <- h$groupe

  # L'échantillon et le plan de sondage
  d <- if (h$longs_seuls) filter(df, long) else df
  d <- d |> drop_na(all_of(c(y, "vague", h$controles[[1]])))
  plan <- svydesign(ids = ~1, weights = ~poids_norm, data = d)

  # Le modèle avec interaction, et le test de Wald de l'interaction
  modele <- svyglm(reformulate(c(h$interaction, "vague", h$controles[[1]]), y), design = plan)
  test <- regTermTest(modele, reformulate(sub(" \\* ", ":", h$interaction)), method = "Wald")
  modeles_interaction[[h$code]] <- modele
  tests_interaction[[h$code]] <- tibble(code = h$code, position = h$titre, interaction = h$interaction,
                                        n = nrow(d), F = as.numeric(test$Ftest), ddl = test$df,
                                        p = as.numeric(test$p))

  # Les valeurs prédites. On attribue à tous les répondants de l'échantillon
  # chaque combinaison de x et du groupe (les NSP du revenu sont laissés de
  # côté), on prédit leur position, et on fait la moyenne pondérée : c'est la
  # position moyenne "si tout le monde était un homme de 18-34 ans", etc., le
  # reste du profil restant celui de l'échantillon.
  valeurs_x <- setdiff(levels(droplevels(d[[x]])), "NSP")
  valeurs_g <- if (is.null(h$valeurs_groupe[[1]])) levels(droplevels(d[[groupe]])) else h$valeurs_groupe[[1]]
  grille <- setNames(list(valeurs_x, valeurs_g), c(x, groupe))
  grille <- do.call(datagrid, c(list(model = modele, grid_type = "counterfactual"), grille))

  predites[[h$code]] <- avg_predictions(modele, newdata = grille, by = c(groupe, x), wts = "(weights)") |>
    as_tibble() |>
    transmute(code = h$code, x = as.character(.data[[x]]), groupe = as.character(.data[[groupe]]),
              estimation = estimate, ic_bas = conf.low, ic_haut = conf.high)

  # Les écarts entre les groupes, à chaque valeur de x (par rapport au premier
  # groupe : les hommes, l'absence de connaissance, l'urbain, l'intérêt faible)
  ecarts[[h$code]] <- avg_predictions(modele, newdata = grille, by = c(groupe, x), wts = "(weights)",
                                      hypothesis = as.formula(paste("difference ~ reference |", x))) |>
    as_tibble() |>
    transmute(code = h$code, x = as.character(.data[[x]]), comparaison = hypothesis,
              ecart = estimate, ic_bas = conf.low, ic_haut = conf.high, p = p.value)
}

tests_interaction <- bind_rows(tests_interaction) |>
  mutate(p_BH = p.adjust(p, method = "BH"))
predites <- bind_rows(predites)
ecarts   <- bind_rows(ecarts)

tests_interaction
ecarts |> mutate(across(where(is.numeric), ~ round(.x, 3))) |> print(n = Inf)

write_csv2(tests_interaction, file.path(dossier, "01_tests_interactions.csv"))
write_csv2(predites, file.path(dossier, "02_valeurs_predites.csv"))
write_csv2(ecarts, file.path(dossier, "03_ecarts.csv"))

# Lecture : voir le bloc de commentaires après les figures.

### Les figures : une par interaction

for (i in seq_len(nrow(interactions))) {
  h <- interactions[i, ]
  donnees <- predites |>
    filter(code == h$code) |>
    mutate(x = factor(x, levels = c(intersect(ordre_affichage, unique(x)), setdiff(unique(x), ordre_affichage))),
           groupe = factor(groupe, levels = unique(groupe)))

  # Des étiquettes lisibles pour les groupes numériques
  if (h$groupe == "conn_reseau")     donnees$groupe <- paste(donnees$groupe, "acteur(s) du réseau connu(s) sur 2")
  if (h$groupe == "interet_energie") donnees$groupe <- paste0("Intérêt pour l'énergie : ", donnees$groupe, "/5")

  ggplot(donnees, aes(x = x, y = estimation, colour = groupe, group = groupe)) +
    geom_line(position = position_dodge(width = 0.3), alpha = 0.5) +
    geom_pointrange(aes(ymin = ic_bas, ymax = ic_haut), position = position_dodge(width = 0.3)) +
    scale_colour_manual(values = couleurs) +
    labs(title = h$titre,
         subtitle = paste0("Valeurs prédites, à autres caractéristiques égales (intervalles à 95 %).\n",
                           "Interaction : p = ", format.pval(tests_interaction$p[i], digits = 2),
                           " (", format.pval(tests_interaction$p_BH[i], digits = 2), " après correction)"),
         x = NULL, y = NULL, colour = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(dossier, paste0("figure_", i, "_interaction_", h$code, ".pdf")), width = 8, height = 5)
}

# Lecture (p-valeurs corrigées pour les cinq tests entre parenthèses) :
#   A. Confirmée, et c'est l'interaction la plus nette (p < 0.001 ; 0.003). La
#      connaissance du réseau augmente avec l'âge chez les hommes (0.81 sur 2
#      chez les 18-34 ans, 1.16 chez les 65 ans et plus), presque pas chez les
#      femmes (de 0.60 à 0.64). L'écart passe de 0.21 chez les 18-34 ans à 0.51
#      chez les 65 ans et plus.
#   B. Confirmée (p = 0.018 ; 0.045), mais avec une autre forme : l'écart de
#      genre sur le nucléaire est le plus grand chez les 18-34 ans (- 25 points)
#      et le plus faible chez les 35-49 ans (- 12). Il ne suit donc pas l'écart
#      de connaissance, qui est le plus faible chez les jeunes.
#   C. Non confirmée (p = 0.11 ; 0.19), mais le profil est suggestif : connaître
#      le réseau va avec moins de soutien à la concurrence (- 0.28 et - 0.38 sur
#      l'échelle de 1 à 5), sauf chez ceux qui définissent le service public par
#      sa mission (0.00).
#   D. Rejetée (p = 0.77) : le malus n'est pas plus refusé par les ruraux
#      modestes. Aucun écart entre zones n'est significatif, à aucun niveau de
#      revenu. L'hypothèse "gilets jaunes" ne se retrouve pas ici.
#   E. Rejetée (p = 0.28) : l'intérêt pour l'énergie va avec plus de gestes à
#      tous les niveaux de revenu (de + 13 à + 17 points entre un intérêt de 2 et
#      de 5 sur 5), un peu plus chez les plus aisés (+ 26), sans que la
#      différence soit significative. Le revenu ne bride pas le passage de
#      l'intérêt aux gestes, au moins pour la part des gestes faits hors
#      "non concerné".


#################################################################
### 2. L'écart entre femmes et hommes sur le nucléaire       ###
#################################################################

# Le sexe pèse 37 % du R² du score pro-nucléaire (script 6). L'écart tient-il à
# ce que les femmes connaissent moins le système électrique, s'y intéressent
# moins, ou le conçoivent autrement ? On compare l'écart de genre :
#   - à profil social et conditions de vie égaux (contrôles de M2) ;
#   - puis en ajoutant la connaissance du réseau, l'intérêt pour l'énergie et la
#     conception du service public (contrôles de M4).
# La différence entre les deux écarts se répartit exactement entre les variables
# ajoutées (décomposition de Gelbach, 2016) : la part d'une variable est l'écart
# entre femmes et hommes sur cette variable (à profil égal), multiplié par son
# coefficient dans le modèle complet. Les intervalles viennent d'un bootstrap
# (500 tirages avec remise des répondants, avec leurs poids).
# On fait la même chose avec le nucléaire en première production, pour la
# robustesse.

# La définition du service public est un facteur à trois modalités : on la
# découpe en deux indicatrices (référence : "Les deux")
df <- df |>
  mutate(def_organisme = as.numeric(definition_sp == "Organisme public"),
         def_mission   = as.numeric(definition_sp == "Mission d'intérêt général"))

mediateurs <- c(conn_reseau     = "Connaissance du réseau",
                interet_energie = "Intérêt pour l'énergie",
                sp_sans_profit  = "SP = absence de profit",
                def_organisme   = "SP défini par l'organisme",
                def_mission     = "SP défini par la mission")
base <- c("vague", profil, vie)

set.seed(2026)
tirages <- list()

for (y in c("pro_nucleaire_pts", "nucleaire_premier_pts")) {
  d <- df |> drop_na(all_of(c(y, base, names(mediateurs))))

  # Le tirage 0 est l'échantillon lui-même, les suivants sont les bootstraps
  for (b in 0:500) {
    d_b <- if (b == 0) d else d[sample(nrow(d), replace = TRUE), ]

    ecart_base <- coef(lm(reformulate(base, y), data = d_b, weights = poids_norm))["sexeFemme"]
    complet    <- lm(reformulate(c(base, names(mediateurs)), y), data = d_b, weights = poids_norm)
    ecart_complet <- coef(complet)["sexeFemme"]

    # Pour chaque variable ajoutée : écart femmes - hommes sur cette variable,
    # multiplié par son coefficient dans le modèle complet
    parts <- sapply(names(mediateurs), function(m) {
      coef(lm(reformulate(base, m), data = d_b, weights = poids_norm))["sexeFemme"] * coef(complet)[m]
    })

    tirages[[paste(y, b)]] <- tibble(position = y, tirage = b,
                                     terme  = c("Écart à profil égal", mediateurs, "Écart restant"),
                                     valeur = c(ecart_base, parts, ecart_complet))
  }
}

decomposition <- bind_rows(tirages) |>
  group_by(position, terme) |>
  summarise(estimation = valeur[tirage == 0],
            ic_bas     = quantile(valeur[tirage > 0], 0.025),
            ic_haut    = quantile(valeur[tirage > 0], 0.975),
            .groups = "drop") |>
  mutate(terme = factor(terme, levels = c("Écart à profil égal", mediateurs, "Écart restant"))) |>
  arrange(position, terme)

decomposition |> mutate(across(where(is.numeric), ~ round(.x, 1))) |> print(n = Inf)
write_csv2(decomposition, file.path(dossier, "04_decomposition_ecart_genre.csv"))

# Vérification : les parts s'additionnent exactement à la différence des deux
# écarts
decomposition |>
  group_by(position) |>
  summarise(difference = estimation[terme == "Écart à profil égal"] - estimation[terme == "Écart restant"],
            somme_parts = sum(estimation[terme %in% mediateurs]))

# Lecture : voir le bloc de commentaires après la figure.

### La figure

decomposition |>
  filter(position == "pro_nucleaire_pts") |>
  mutate(terme = fct_rev(terme),
         type = case_when(terme %in% mediateurs ~ "Part passant par la variable",
                          TRUE ~ "Écart femmes - hommes")) |>
  ggplot(aes(x = estimation, y = terme, fill = type)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(xmin = ic_bas, xmax = ic_haut), width = 0.2, orientation = "y") +
  geom_vline(xintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c("#235E86", "#E0A458")) +
  labs(title = "Ce que l'écart de genre sur le nucléaire doit à la connaissance et aux conceptions",
       subtitle = "Score pro-nucléaire (0-100), décomposition de Gelbach, intervalles bootstrap à 95 %",
       x = "Points de score", y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(file.path(dossier, "figure_6_decomposition_ecart_genre.pdf"), width = 9, height = 5)

# Lecture : à profil égal, les femmes ont un score pro-nucléaire inférieur de
# 20.0 points [16.7 ; 23.0]. Ajouter la connaissance, l'intérêt et la conception
# du service public ne le réduit que de 1.4 point : il en reste 18.6. La
# connaissance du réseau en explique 1.9 point [1.1 ; 2.8] ; l'intérêt pour
# l'énergie joue en sens inverse (+ 0.7), et la conception du service public ne
# joue pas. Même chose avec le nucléaire en première production (de 22.8 à 20.9
# points). L'écart de genre sur le nucléaire n'est donc pas un écart de
# connaissance du système : plus de 90 % demeure à connaissance égale. Il tient à
# autre chose (perception du risque, valeurs), que l'enquête ne mesure pas.


#################################################
### 3. Les gestes en logit fractionnaire      ###
#################################################

# La part des gestes faits est une proportion, entre 0 et 1. Les MCO du script 6
# peuvent prédire des valeurs hors de ces bornes et supposent des effets
# constants, alors qu'il est plus difficile de gagner un geste quand on en fait
# déjà presque tous. Le logit fractionnaire (Papke et Wooldridge, 1996) est un
# logit sur une proportion. On compare ses effets moyens, en points, aux
# coefficients des MCO.
d <- df |>
  filter(long) |>
  drop_na(all_of(c("gestes_part", "vague", profil, vie, energie, conception)))
plan_gestes <- svydesign(ids = ~1, weights = ~poids_norm, data = d)

gestes_mco   <- svyglm(reformulate(c("vague", profil, vie, energie, conception), "gestes_part_pts"),
                       design = plan_gestes)
gestes_logit <- svyglm(reformulate(c("vague", profil, vie, energie, conception), "gestes_part"),
                       design = plan_gestes, family = quasibinomial())

# Les effets moyens : pour une modalité, l'écart moyen avec la référence ; pour
# une variable continue, l'effet d'une unité de plus. Ceux des MCO sont leurs
# coefficients.
gestes_comparaison <- full_join(
  avg_slopes(gestes_mco, wts = "(weights)") |>
    as_tibble() |>
    select(term, contrast, mco = estimate, p_mco = p.value),
  avg_slopes(gestes_logit, wts = "(weights)") |>
    as_tibble() |>
    transmute(term, contrast, logit_fractionnaire = 100 * estimate, p_logit = p.value),
  by = c("term", "contrast")
)

gestes_comparaison |> mutate(across(where(is.numeric), ~ round(.x, 3))) |> print(n = Inf)
write_csv2(gestes_comparaison, file.path(dossier, "05_gestes_logit_fractionnaire.csv"))

# Lecture : les deux modèles donnent les mêmes effets, au dixième de point près
# (intérêt pour l'énergie : + 5.2 et + 5.1 points par degré ; appartement :
# - 4.2 dans les deux cas), et les mêmes p-valeurs. La part moyenne des gestes
# (61 %) est loin des bornes, où les deux modèles divergeraient. Les MCO du
# script 6 sont donc fiables pour les gestes.


#############################################
### 4. Les tableaux et la figure des coefficients ###
#############################################

### Tableau 1 : les positions les mieux expliquées

# Les quatre positions où le profil et le rapport à l'énergie expliquent le plus
# (script 6) : la connaissance du réseau, le nucléaire, les gestes et l'offre
# verte. Mêmes modèles qu'au script 6 (M4, MCO pondérés), sauf la connaissance
# du réseau, qui ne peut pas s'expliquer par elle-même : elle a les contrôles de
# M2 et l'intérêt pour l'énergie.
modeles_tableau <- list()
for (y in c("conn_reseau", "pro_nucleaire_pts", "gestes_part_pts", "offre_verte_pts")) {
  controles <- if (y == "conn_reseau") c(profil, vie, "interet_energie") else c(profil, vie, energie, conception)
  d <- if (y %in% c("gestes_part_pts", "offre_verte_pts")) filter(df, long) else df
  d <- d |> drop_na(all_of(c(y, "vague", controles)))
  modeles_tableau[[y]] <- svyglm(reformulate(c("vague", controles), y),
                                 design = svydesign(ids = ~1, weights = ~poids_norm, data = d))
}
names(modeles_tableau) <- c("Connaissance du réseau (0-2)", "Pro-nucléaire (0-100)",
                            "Gestes faits (%)", "Offre verte (%)")

# Le R² pondéré (svyglm ne le donne pas)
r2_tableau <- sapply(modeles_tableau, function(m) {
  summary(lm(formula(m), data = m$survey.design$variables, weights = poids_norm))$r.squared
})

# Les noms des coefficients, dans l'ordre d'affichage (les références sont
# indiquées dans la note)
noms_coefficients <- c(
  "age_tranche18-34"   = "Âge : 18-34 ans",
  "age_tranche50-64"   = "Âge : 50-64 ans",
  "age_tranche65+"     = "Âge : 65 ans et plus",
  "sexeFemme"          = "Femme",
  "diplomeInfra-bac"   = "Diplôme : infra-bac",
  "diplomeBac+2/3"     = "Diplôme : bac+2/3",
  "diplomeBac+5"       = "Diplôme : bac+5",
  "revenu< 1 000"      = "Revenu : < 1 000 euros",
  "revenu1 000-1 500"  = "Revenu : 1 000-1 500 euros",
  "revenu2 500-4 000"  = "Revenu : 2 500-4 000 euros",
  "revenu4 000 et +"   = "Revenu : 4 000 euros et +",
  "zone3Rural"         = "Zone : rural",
  "zone3Périurbain"    = "Zone : périurbain",
  "logementAppartement" = "Appartement",
  "statutLocataire"    = "Locataire",
  "statutSans contrat" = "Sans contrat d'électricité",
  "difficultesRarement/parfois" = "Difficultés à payer : rarement/parfois",
  "difficultesSouvent/toujours" = "Difficultés à payer : souvent/toujours",
  "conn_reseau"        = "Connaissance du réseau (0-2)",
  "interet_energie"    = "Intérêt pour l'énergie (1-5)",
  "definition_spOrganisme public"          = "SP défini par l'organisme",
  "definition_spMission d'intérêt général" = "SP défini par la mission",
  "sp_sans_profit"     = "SP = absence de profit",
  "(Intercept)"        = "Constante"
)

note_tableau <- paste(
  "MCO pondérés, erreurs-types robustes entre parenthèses. * p < 0,05 ; ** p < 0,01 ; *** p < 0,001.",
  "Références : 35-49 ans, homme, bac, revenu de 1 500 à 2 500 euros, urbain, maison, propriétaire,",
  "jamais de difficultés, SP défini par l'organisme et la mission. Contrôles non affichés : vague,",
  "modalités Autre/NSP. Gestes et offre verte : questionnaire long seulement."
)

lignes_r2 <- data.frame(term = "R²", t(formatC(r2_tableau, format = "f", digits = 3)), check.names = FALSE)
names(lignes_r2) <- c("term", names(modeles_tableau))

# La version Word. modelsummary cherche à calculer une vraisemblance que svyglm
# n'a pas, d'où des avertissements sans conséquence, qu'on masque.
tableau_1 <- suppressWarnings(
  modelsummary(modeles_tableau, output = "flextable", coef_map = noms_coefficients,
               estimate = "{estimate}{stars}", statistic = "({std.error})",
               stars = c("*" = .05, "**" = .01, "***" = .001),
               gof_map = "nobs", add_rows = lignes_r2, fmt = 2, notes = note_tableau,
               title = "Tableau 1. Les positions les mieux expliquées par le profil et le rapport à l'énergie")
)
tableau_1 |> autofit() |> save_as_docx(path = file.path(dossier, "tableau_1_positions.docx"))

# La version LaTeX : un simple tabular, à placer dans un environnement table
# avec son titre et sa note
options(modelsummary_format_numeric_latex = "plain")
suppressWarnings(
  modelsummary(modeles_tableau, output = "latex_tabular", coef_map = noms_coefficients,
               estimate = "{estimate}{stars}", statistic = "({std.error})",
               stars = c("*" = .05, "**" = .01, "***" = .001),
               gof_map = "nobs", add_rows = lignes_r2, fmt = 2)
) |>
  as.character() |>
  writeLines(file.path(dossier, "tableau_1_positions.tex"))

### Tableau 2 : les interactions

# Pour chaque hypothèse, le test de l'interaction et les écarts aux deux bouts
# de la variable en abscisse
tableau_2 <- ecarts |>
  left_join(tests_interaction |> select(code, position, interaction, p_interaction = p, p_BH), by = "code") |>
  mutate(ecart = paste0(formatC(ecart, format = "f", digits = 2), " [",
                        formatC(ic_bas, format = "f", digits = 2), " ; ",
                        formatC(ic_haut, format = "f", digits = 2), "]"),
         p_interaction = format.pval(p_interaction, digits = 2, eps = 0.001),
         p_BH = format.pval(p_BH, digits = 2, eps = 0.001)) |>
  select(Position = position, Interaction = interaction, `p (interaction)` = p_interaction,
         `p (BH)` = p_BH, Comparaison = comparaison, `À` = x, `Écart [IC 95 %]` = ecart)

flextable(tableau_2) |>
  merge_v(j = c("Position", "Interaction", "p (interaction)", "p (BH)")) |>
  set_caption("Tableau 2. Interactions : écarts prédits entre groupes, à autres caractéristiques égales") |>
  add_footer_lines(paste("Écarts de valeurs prédites (même échelle que la position), MCO pondérés.",
                         "p (BH) : p-valeur corrigée pour les cinq tests (Benjamini-Hochberg).")) |>
  theme_booktabs() |>
  autofit() |>
  save_as_docx(path = file.path(dossier, "tableau_2_interactions.docx"))

### Tableau 3 : la décomposition de l'écart de genre

tableau_3 <- decomposition |>
  mutate(valeur = paste0(formatC(estimation, format = "f", digits = 1), " [",
                         formatC(ic_bas, format = "f", digits = 1), " ; ",
                         formatC(ic_haut, format = "f", digits = 1), "]"),
         position = recode(position, pro_nucleaire_pts = "Score pro-nucléaire (0-100)",
                           nucleaire_premier_pts = "Nucléaire en 1re production (%)")) |>
  select(terme, position, valeur) |>
  pivot_wider(names_from = position, values_from = valeur) |>
  rename(` ` = terme)

flextable(tableau_3) |>
  set_caption("Tableau 3. Décomposition de l'écart entre femmes et hommes sur le nucléaire") |>
  add_footer_lines(paste("Décomposition de Gelbach. Écart à profil égal : contrôles d'âge, diplôme,",
                         "revenu, zone, logement, statut, difficultés et vague. Écart restant : avec en plus",
                         "les cinq variables listées. Intervalles bootstrap à 95 % (500 tirages).")) |>
  theme_booktabs() |>
  autofit() |>
  save_as_docx(path = file.path(dossier, "tableau_3_decomposition_genre.docx"))

### Figure des coefficients du tableau 1

# Les échelles des quatre positions diffèrent : un panneau par position
coefficients_tableau <- map_dfr(names(modeles_tableau), function(nom) {
  m <- modeles_tableau[[nom]]
  tibble(modele = nom, terme = names(coef(m)), estimation = coef(m),
         ic_bas = confint(m)[, 1], ic_haut = confint(m)[, 2])
}) |>
  filter(terme %in% names(noms_coefficients), terme != "(Intercept)") |>
  mutate(terme  = factor(noms_coefficients[terme], levels = rev(noms_coefficients)),
         modele = factor(modele, levels = names(modeles_tableau)),
         significatif = ic_bas > 0 | ic_haut < 0)

ggplot(coefficients_tableau, aes(x = estimation, y = terme, colour = significatif)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_pointrange(aes(xmin = ic_bas, xmax = ic_haut), size = 0.25) +
  facet_wrap(~ modele, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = c("FALSE" = "grey65", "TRUE" = "#235E86")) +
  labs(title = "Ce qui va avec chaque position, à autres caractéristiques égales",
       subtitle = "Coefficients des modèles complets (MCO pondérés), intervalles à 95 % ; en bleu, significatifs à 5 %",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")
ggsave(file.path(dossier, "figure_7_coefficients.pdf"), width = 13, height = 7)


#######################
### Sauvegarde      ###
#######################

saveRDS(list(interactions = interactions, modeles_interaction = modeles_interaction,
             tests_interaction = tests_interaction, predites = predites, ecarts = ecarts,
             decomposition = decomposition, gestes_comparaison = gestes_comparaison,
             modeles_tableau = modeles_tableau, coefficients_tableau = coefficients_tableau,
             r2_tableau = r2_tableau),
        file.path(dossier, "resultats_prolongements.rds"))
