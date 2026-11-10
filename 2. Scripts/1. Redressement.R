# Redressement de l'échantillon : calage sur l'âge, le sexe et le type de commune
#
# On attribue à chaque répondant un poids, de sorte que l'échantillon (longs et
# courts réunis) retrouve la structure de la population française de 18 ans et
# plus, selon trois variables croisées :
#   - l'âge en quatre tranches (18-34, 35-49, 50-64, 65 ans et plus) ;
#   - le sexe (homme / femme) ;
#   - le type de commune déclaré, en deux modalités (rural / urbain, zone2).
#
# La cible rurale est celle que recommande le script 4 : une cible "corrigée de
# la perception", qui ne corrige que le vrai déséquilibre de l'échantillon et pas
# la façon dont les gens décrivent leur lieu de vie.
#
# Le script ne laisse que df dans l'environnement, avec deux poids :
#   - poids      : chaque répondant représente un nombre de Français (somme = 53,5 M) ;
#   - poids_norm : le même, ramené à la taille de l'échantillon (moyenne = 1).

rm(list = ls())

# On importe les données préparées par le script 4, qui source lui-même le
# script 2 : df_long et df_court, avec l'âge et les variables de zone.
# Le script 4 laisse aussi deux objets dont on se sert plus bas :
#   - pop_densite     : la grille communale INSEE, avec la taille de chaque commune ;
#   - controle_taille : la part des répondants de chaque taille de commune qui se
#                       disent ruraux.
source("2. Scripts/4. Variable zone.R")


###############################
### Chargement des packages ###
###############################

library(tidyverse)
library(survey)


###############################
### Importation des données ###
###############################

### Les données de l'enquête

# On réunit les questionnaires longs et courts, en gardant leur origine.
df <- bind_rows(list(long = df_long, court = df_court), .id = "version_questionnaire")
nrow(df)  # 3284 répondants

### Les données INSEE

# Population par commune, sexe et âge détaillé (recensement 2022).
# Attention : le fichier contient deux niveaux géographiques, les communes (COM)
# et les arrondissements de Paris, Lyon et Marseille (ARM). Garder les deux
# compterait ces trois villes deux fois : on ne garde que les communes.
# On écarte Mayotte, absente de la grille de densité.
pop_insee <- read.csv("1. Data/Public data/TD_POP1B_2022.csv", sep = ";",
                      colClasses = c(CODGEO = "character")) |>
  filter(NIVGEO == "COM", AGED100 >= 18, !startsWith(CODGEO, "976"))

sum(pop_insee$NB, na.rm = TRUE)  # 53 483 601 adultes

# Chaque commune du recensement doit se retrouver dans la grille de densité,
# sinon sa population disparaîtrait de la cible sans message d'erreur.
stopifnot(all(pop_insee$CODGEO %in% pop_densite$CODGEO))


######################################
### Construction de la cible INSEE ###
######################################

# On reprend le raisonnement du script 4, mais par âge et par sexe, et sur les
# 18 ans et plus (le script 4 travaillait sur la population totale).
# Pour chaque taille de commune, on prend la population INSEE de chaque âge et de
# chaque sexe, et on la répartit entre rural et urbain selon la part des
# répondants de cette taille qui se disent ruraux.
# Hypothèse : à taille de commune égale, les Français décrivent leur zone comme
# nos enquêtés, quels que soient leur âge et leur sexe.

controle_taille  # part rurale déclarée : 100 / 97.4 / 33.8 / 4.8 / 0.0

pop_cible <- pop_insee |>
  left_join(pop_densite |> select(CODGEO, taille_commune), by = "CODGEO") |>
  left_join(controle_taille |> select(taille_commune, rural_declare), by = "taille_commune") |>
  mutate(
    AGE_large = cut(AGED100, c(18, 35, 50, 65, Inf),
                    c("18-34", "35-49", "50-64", "65+"), right = FALSE),
    SEXE      = if_else(SEXE == 1, "Homme", "Femme"),
    Rural     = NB * rural_declare / 100,
    Urbain    = NB - Rural
  ) |>
  pivot_longer(c(Rural, Urbain), names_to = "zone2", values_to = "pop") |>
  group_by(AGE_large, SEXE, zone2) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")

# On vérifie la part rurale de la cible
pop_cible |>
  group_by(zone2) |>
  summarise(part = round(100 * sum(pop) / sum(pop_cible$pop), 1))
# 35.8 % de ruraux, contre 35.9 % dans le script 4 (population totale)


#########################################
### Préparation des données d'enquête ###
#########################################

### L'âge

# Mêmes tranches que dans la cible INSEE
df <- df |>
  mutate(AGE_large = cut(age, c(18, 35, 50, 65, Inf),
                         c("18-34", "35-49", "50-64", "65+"), right = FALSE))

### Le sexe

# On garde toutes les réponses, y compris "Autre" et les non-réponses (10 personnes).
df <- df |> mutate(SEXE = factor(G1Q00001))
table(df$SEXE, useNA = "ifany")

### Le type de commune

# Le script 4 laisse zone2 manquante dans trois cas : la zone n'est pas
# renseignée, elle contredit la taille de commune (hameau "urbain", grande ville
# "rurale"), ou le répondant se dit périurbain sans donner sa taille de commune.
# Plutôt que d'exclure ces personnes de toutes les analyses, on leur attribue une
# zone d'après leur taille de commune, avec la règle du script 4 : hameau et
# village -> rural ; petite ville et au-delà -> urbain.
df <- df |>
  mutate(
    zone2_attribuee = is.na(zone2) & !is.na(taille_commune),
    zone2 = case_when(
      !is.na(zone2)                              ~ as.character(zone2),
      taille_commune %in% c("Hameau", "Village") ~ "Rural",
      !is.na(taille_commune)                     ~ "Urbain"
    ),
    zone2 = factor(zone2, levels = c("Rural", "Urbain"))
  )

sum(df$zone2_attribuee)  # 69 zones attribuées
sum(is.na(df$zone2))     # 19 répondants sans zone ni taille de commune

### Les exclusions

# Sans âge ou sans zone, on ne peut pas placer le répondant dans une cellule de
# calage : on l'exclut, sans imputer.
n_avant <- nrow(df)
df <- df |> filter(!is.na(AGE_large), !is.na(zone2))
n_avant - nrow(df)  # 25 exclus (19 sans zone, 7 sans âge, dont 1 sans les deux)
nrow(df)            # 3259 répondants redressés (contre 3192 avant l'attribution des zones)


####################
### Redressement ###
####################

# Le principe
#------------

# On cale tout le monde sur les cellules âge x zone (8 cellules). Pour le sexe,
# on ne veut pas inventer un sexe aux 10 personnes qui ont répondu "Autre" ou
# "Ne sait pas". On cale donc seulement, dans chaque cellule âge x zone, la part
# des hommes parmi les hommes et les femmes. Les 10 autres personnes reçoivent
# simplement le poids moyen de leur cellule âge x zone.

# Les effectifs cibles par cellule âge x zone, et la part des hommes dans chacune
cellules <- pop_cible |>
  group_by(AGE_large, zone2) |>
  summarise(p_homme = sum(pop[SEXE == "Homme"]) / sum(pop),
            pop     = sum(pop),
            .groups = "drop") |>
  mutate(strate = paste(AGE_large, zone2, sep = " | "))

cellules

df <- df |> mutate(strate = factor(paste(AGE_large, zone2, sep = " | "),
                                   levels = cellules$strate))

# Chaque cellule doit contenir des hommes et des femmes
table(df$strate, df$SEXE)[, c("Homme", "Femme")]  # au moins 69 par case


# La traduction en contraintes de calage
#---------------------------------------

# La fonction calibrate() cale des totaux. Pour caler une proportion, on crée
# dans chaque cellule une variable qui vaut, pour un homme, 1 - p_homme, pour une
# femme, - p_homme, et 0 pour tous les autres. Si la somme pondérée de cette
# variable vaut 0, c'est que la part pondérée des hommes parmi les hommes et les
# femmes de la cellule vaut exactement p_homme.
homme_ou_femme <- df$SEXE %in% c("Homme", "Femme")

for (j in seq_len(nrow(cellules))) {
  dans_cellule <- df$strate == cellules$strate[j] & homme_ou_femme
  df[[paste0("calage_sexe_", j)]] <- ifelse(
    dans_cellule, (df$SEXE == "Homme") - cellules$p_homme[j], 0
  )
}

variables_sexe <- paste0("calage_sexe_", seq_len(nrow(cellules)))
formule_calage <- reformulate(c("strate", variables_sexe), intercept = FALSE)

# Les totaux à atteindre : la population de chaque cellule âge x zone, puis 0
# pour chacune des variables de sexe
totaux_cibles <- c(cellules$pop, rep(0, length(variables_sexe)))


# Le calage en pratique
#----------------------

# On part d'un poids uniforme, à l'échelle de la population française
df$poids_initial <- sum(cellules$pop) / nrow(df)

plan_initial <- svydesign(ids = ~1, weights = ~poids_initial, data = df)

# Calage exponentiel ("raking") : les poids restent toujours positifs
plan_redresse <- calibrate(plan_initial,
                           formula    = formule_calage,
                           population = totaux_cibles,
                           calfun     = "raking",
                           maxit      = 100,
                           epsilon    = 1e-7)

# On vérifie que toutes les contraintes sont respectées
X <- model.matrix(formule_calage, data = df)
ecarts <- colSums(X * weights(plan_redresse)) - totaux_cibles
max(abs(ecarts))  # quasi nul (de l'ordre de 1e-7)
stopifnot(max(abs(ecarts)) < 1)


##############################################
### Vérifications et description des poids ###
##############################################

df$poids <- weights(plan_redresse)

# La distribution des poids
summary(df$poids)
quantile(df$poids, c(0, .01, .05, .25, .50, .75, .95, .99, 1))  # de 10 945 à 28 285

# On regarde qui reçoit les poids les plus forts : les jeunes hommes ruraux,
# les moins nombreux de l'échantillon (69 répondants)
df |>
  arrange(desc(poids)) |>
  select(AGE_large, SEXE, zone2, poids) |>
  head(10)

# Effective Sample Size (ESS) : la précision équivaut à celle d'un échantillon
# non pondéré de cette taille, à cause de la dispersion des poids.
ESS <- sum(df$poids)^2 / sum(df$poids^2)
round(c(n = nrow(df), ESS = ESS, taux_ESS = ESS / nrow(df)), 2)  # ESS = 3090, soit 95 %

# On vérifie que les marges sont retrouvées
df |>
  filter(SEXE %in% c("Homme", "Femme")) |>
  group_by(AGE_large, zone2) |>
  summarise(part_hommes = round(100 * sum(poids[SEXE == "Homme"]) / sum(poids), 1),
            .groups = "drop") |>
  mutate(cible = round(100 * cellules$p_homme, 1))

df |>
  group_by(zone2) |>
  summarise(part_ponderee = round(100 * sum(poids) / sum(df$poids), 1),
            part_brute    = round(100 * n() / nrow(df), 1))
# Ruraux : 38.1 % bruts, 35.8 % pondérés


# Le poids normalisé
#-------------------

df <- df |> mutate(poids_norm = poids * n() / sum(poids))
summary(df$poids_norm)  # chaque répondant compte pour 0.67 à 1.72 répondant "moyen"


# Une précision pour la suite
#----------------------------

# Les questionnaires longs ne représentent pas tous les adultes : ils sont posés
# aux personnes qui ont un contrat d'électricité à leur nom. Les 18-34 ans y sont
# donc moins nombreux (17.5 % des longs pondérés, contre 25 % de l'ensemble).
# Ce n'est pas un défaut des poids : les longs forment une sous-population réelle,
# qu'il faut nommer comme telle dans les analyses qui ne portent que sur eux.
df |>
  group_by(version_questionnaire, AGE_large) |>
  summarise(poids = sum(poids_norm), .groups = "drop_last") |>
  mutate(part = round(100 * poids / sum(poids), 1))


# On ne garde que df, sans les variables techniques du calage
df <- df |> select(-starts_with("calage_sexe_"), -poids_initial)
rm(list = setdiff(ls(), "df"))
