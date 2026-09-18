# Pas de rm(list=ls()) en tête : ce script est fait pour être sourcé par
# d'autres (4. Variable zone.R, le redressement), et effacerait leur environnement.

# Construction d'une variable d'âge unique et des tranches d'âge, pour les
# questionnaires longs comme courts et pour les deux vagues.


###############################
### Chargement des packages ###
###############################

library(dplyr)


###############################
### Importation des données ###
###############################

# On lit tout en texte. La colonne G1Q00002 mélange en effet des nombres
# (vague 2) et du texte (vague 1) : en lecture normale R la traite comme du
# texte, et les comparaisons se font alors dans l'ordre alphabétique.
# "Ne sait pas" y est supérieur à "65" et "0" inférieur à "20", ce qui range
# des répondants sans âge en "65 et plus" ou en "18-19". On convertit donc
# nous-mêmes, explicitement, plus bas.
# fileEncoding = "UTF-8-BOM" retire le marqueur invisible en tête de fichier,
# qui sinon se colle au nom de la première colonne.

lire_panel <- function(fichier) {
  read.csv(file.path("1. Data", fichier), sep = ",",
           colClasses = "character", fileEncoding = "UTF-8-BOM")
}

df_long  <- lire_panel("panel_long.csv")   # 2839 répondants
df_court <- lire_panel("panel_court.csv")  # 445 répondants


##########################################
### Où se trouve l'âge selon la vague  ###
##########################################

# L'âge n'est pas enregistré au même endroit dans les deux vagues :
#   - vague 2 : il est directement dans G1Q00002 ;
#   - vague 1 : la question passait par une modalité "Autre" ouvrant un champ
#     libre, donc G1Q00002 vaut "Autre" et l'âge se trouve dans G1Q00002_other.
# Ne lire que G1Q00002 ferait disparaître toute la vague 1, soit 43 % des
# questionnaires longs, sans aucun message d'erreur.

# On vérifie que la règle est sans exception dans les deux fichiers
table(df_long$vague,  grepl("^[0-9]+$", df_long$G1Q00002))   # vague 1 : aucun nombre ; vague 2 : 1622
table(df_court$vague, grepl("^[0-9]+$", df_court$G1Q00002))  # vague 1 : aucun nombre ; vague 2 : 350
table(df_long$vague,  grepl("^[0-9]+$", df_long$G1Q00002_other))   # vague 1 : 1213 nombres
table(df_court$vague, grepl("^[0-9]+$", df_court$G1Q00002_other))  # vague 1 : 95 nombres


#########################################
### Construction de la variable d'âge ###
#########################################

# Le découpage de l'institut, identique à celui de la variable AgeTranche
BORNES_BILENDI   <- c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf)
LIBELLES_BILENDI <- c("18-24", "25-29", "30-34", "35-39", "40-44",
                      "45-49", "50-54", "55-59", "60-64", "65+")

# Le découpage du recensement, utilisé pour le redressement. L'INSEE va par
# tranches de cinq ans à partir de 15-19 ans : en se limitant aux 18 ans et
# plus, la première tranche devient 18-19.
BORNES_INSEE   <- c(18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf)
LIBELLES_INSEE <- c("18-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                    "45-49", "50-54", "55-59", "60-64", "65 et plus")

ajouter_age <- function(df) {
  df |>
    mutate(
      age = as.numeric(if_else(vague == "1", G1Q00002_other, G1Q00002)),
      # L'enquête vise les 18 ans et plus : un âge hors bornes est une
      # non-réponse. LimeSurvey les avait d'ailleurs classés en "NSPP".
      age = if_else(age < 18 | age > 110, NA_real_, age),
      tranche_bilendi = cut(age, BORNES_BILENDI, LIBELLES_BILENDI, right = FALSE),
      tranche_insee   = cut(age, BORNES_INSEE,   LIBELLES_INSEE,   right = FALSE)
    )
}

df_long  <- ajouter_age(df_long)
df_court <- ajouter_age(df_court)


###############################
### Contrôles de cohérence  ###
###############################

# On compte les répondants sans âge exploitable
sum(is.na(df_long$age))   # 6 : 4 "Ne sait pas" en vague 1, et 2 âges déclarés à 0
sum(is.na(df_court$age))  # 1 : un âge déclaré à 0

# En vague 2, LimeSurvey a calculé lui-même la tranche d'âge (AgeTranche).
# Si nos bornes sont justes, notre tranche doit la reproduire exactement.
controler_tranches <- function(df) {
  df |>
    filter(vague == "2", !is.na(tranche_bilendi), AgeTranche %in% LIBELLES_BILENDI) |>
    summarise(accords    = sum(as.character(tranche_bilendi) == AgeTranche),
              desaccords = sum(as.character(tranche_bilendi) != AgeTranche))
}

controler_tranches(df_long)   # 1620 accords, 0 désaccord
controler_tranches(df_court)  #  349 accords, 0 désaccord


#############################################
### Âges médians par vague et par format  ###
#############################################

# Point de vigilance soulevé en réunion : les écarts d'âge médian entre les
# deux vagues, et entre questionnaires courts et longs.

ages <- bind_rows(
  df_long  |> select(vague, age) |> mutate(questionnaire = "long"),
  df_court |> select(vague, age) |> mutate(questionnaire = "court")
)

ages |>
  group_by(questionnaire, vague) |>
  summarise(n         = n(),
            age_connu = sum(!is.na(age)),
            mediane   = median(age, na.rm = TRUE),
            moyenne   = round(mean(age, na.rm = TRUE), 1),
            q1        = quantile(age, 0.25, na.rm = TRUE),
            q3        = quantile(age, 0.75, na.rm = TRUE),
            .groups   = "drop")

# L'écart entre courts et longs est attendu : le questionnaire court s'adresse
# aux personnes sans contrat d'électricité à leur nom, donc à des jeunes
# logés chez leurs parents ou en résidence.


##################################################
### Comparaison aux cibles d'âge de l'institut ###
##################################################

# L'écart d'âge entre les deux vagues vient-il d'un problème d'encodage, ou du
# terrain ? Si chaque vague s'écarte des cibles mais que leur cumul les
# respecte, c'est que les deux vagues ont été conçues pour se compléter.

part_par_tranche <- function(tranches) {
  effectifs <- table(factor(tranches, levels = LIBELLES_BILENDI))
  round(100 * as.numeric(effectifs) / sum(effectifs), 1)
}

tr <- bind_rows(
  df_long  |> select(vague, tranche_bilendi),
  df_court |> select(vague, tranche_bilendi)
) |>
  filter(!is.na(tranche_bilendi))

comparaison <- data.frame(
  tranche  = LIBELLES_BILENDI,
  cible    = c(10.99, 5.52, 8.51, 7.79, 7.71, 7.70, 8.62, 7.96, 7.65, 27.60),
  vague1   = part_par_tranche(tr$tranche_bilendi[tr$vague == "1"]),
  vague2   = part_par_tranche(tr$tranche_bilendi[tr$vague == "2"]),
  ensemble = part_par_tranche(tr$tranche_bilendi)
) |>
  mutate(ecart_v1  = round(vague1   - cible, 1),
         ecart_v2  = round(vague2   - cible, 1),
         ecart_tot = round(ensemble - cible, 1))

comparaison

# Écart absolu moyen aux dix cibles
comparaison |>
  summarise(vague1   = round(mean(abs(ecart_v1)),  1),   # 2.6 points
            vague2   = round(mean(abs(ecart_v2)),  1),   # 1.7 points
            ensemble = round(mean(abs(ecart_tot)), 1))   # 0 point (0.05 exactement)

# Conclusion : l'écart entre vagues ne vient pas de l'encodage mais du terrain.
# La vague 1 est trop âgée, la vague 2 trop jeune, exactement en miroir, et
# leur cumul retombe sur les cibles. Les deux vagues ont donc été conçues pour
# se compléter : aucune n'est représentative prise séparément, et toute
# comparaison entre vagues serait confondue avec l'âge.
