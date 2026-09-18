# Pas de rm(list=ls()) en tête : ce script est fait pour être sourcé par le
# redressement, et effacerait son environnement.

# Construction de la zone d'habitation : une version à deux modalités (rural /
# urbain) utilisable pour le redressement, et une version à trois modalités
# réservée à l'analyse.
# Suite de la réunion du 18/09 : on reclasse les périurbains d'après la taille
# de commune qu'ils déclarent, puis on compare à la grille de densité INSEE.


###############################
### Chargement des packages ###
###############################

library(dplyr)
library(openxlsx)


###############################
### Importation des données ###
###############################

### Les données de l'enquête

# On part du script sur l'âge : il lit les deux fichiers et ajoute l'âge et les
# tranches d'âge, dont on a besoin plus bas pour le calage croisé.
source("2. Scripts/2. Variable age.R")

### Les données externes (publiques et agrégées)

# Grille de densité 2025 (géographie au 01/01/2026) : une ligne par commune,
# avec sa population municipale 2023 et son degré de densité (3 postes).
pop_densite <- read.xlsx("1. Data/Public data/grille_densite_2026.xlsx",
                         sheet = "Maille communale", startRow = 5) |>
  select(CODGEO, PMUN23, LIBDENS) |>
  mutate(PMUN23 = as.numeric(PMUN23)) |>
  filter(!startsWith(CODGEO, "976"))  # on écarte Mayotte, comme dans le redressement

TAILLES <- c("Hameau", "Village", "Petite ville", "Ville moyenne", "Grande ville")


#################################################
### La règle de reclassement, vue par l'INSEE ###
#################################################

# On applique aux communes les seuils de taille du questionnaire, puis on
# regarde quelle part de la population de chaque taille l'INSEE classe en rural.
pop_densite <- pop_densite |>
  mutate(taille_commune = cut(PMUN23, c(0, 800, 2000, 20000, 100000, Inf),
                              labels = TAILLES, right = FALSE))

rural_insee <- pop_densite |>
  group_by(taille_commune) |>
  summarise(pop         = sum(PMUN23),
            rural_insee = round(100 * sum(PMUN23[LIBDENS == "Rural"]) / sum(PMUN23), 1),
            .groups     = "drop") |>
  mutate(part_insee = round(100 * pop / sum(pop), 1))

rural_insee  # part rurale : 97.6 / 85.5 / 31.2 / 0.4 / 0.0

# D'où la règle : hameau et village -> rural ; petite ville et au-delà -> urbain.
# C'est la classification majoritaire de l'INSEE pour chaque taille.


###########################################
### Construction des variables de zone  ###
###########################################

# On repère les modalités par un mot distinctif plutôt que par le libellé
ajouter_zone <- function(df) {
  df |>
    mutate(
      taille_commune = factor(case_when(
        grepl("hameau",        G2Q00001) ~ "Hameau",
        grepl("village",       G2Q00001) ~ "Village",
        grepl("petite ville",  G2Q00001) ~ "Petite ville",
        grepl("ville moyenne", G2Q00001) ~ "Ville moyenne",
        grepl("grande ville",  G2Q00001) ~ "Grande ville"
      ), levels = TAILLES),
      # "à proximité d'une zone urbaine" contient aussi "urbaine" : on le teste avant
      zone_declaree = case_when(
        grepl("rurale",    G2Q00002) ~ "Rural",
        grepl("proximité", G2Q00002) ~ "Périurbain",
        grepl("urbaine",   G2Q00002) ~ "Urbain"
      ),
      # Les réponses contradictoires sont mises de côté pour l'instant : on ne
      # les reclasse pas et on ne les exclut pas de l'enquête, on neutralise
      # seulement leur zone. Les "village + urbain" n'en font pas partie :
      # l'INSEE classe 14.5 % de la population des villages en urbain, ces
      # réponses sont donc plausibles.
      zone_contradictoire =
        (taille_commune %in% "Hameau"       & zone_declaree %in% "Urbain") |
        (taille_commune %in% "Grande ville" & zone_declaree %in% "Rural"),
      # Trois modalités : la déclaration telle quelle
      zone3 = factor(if_else(zone_contradictoire, NA_character_, zone_declaree),
                     levels = c("Rural", "Périurbain", "Urbain")),
      # Deux modalités, pour le redressement : les périurbains sont reclassés
      # selon la taille de commune qu'ils déclarent
      zone2 = case_when(
        zone_contradictoire | is.na(zone_declaree)                                 ~ NA_character_,
        zone_declaree == "Périurbain" & taille_commune %in% c("Hameau", "Village") ~ "Rural",
        zone_declaree == "Périurbain" & !is.na(taille_commune)                     ~ "Urbain",
        zone_declaree == "Périurbain"                                              ~ NA_character_,
        TRUE                                                                       ~ zone_declaree
      ),
      zone2 = factor(zone2, levels = c("Rural", "Urbain"))
    )
}

df_long  <- ajouter_zone(df_long)
df_court <- ajouter_zone(df_court)


###############################
### Contrôles de cohérence  ###
###############################

# Le redressement porte sur l'ensemble de l'échantillon : on réunit les
# questionnaires longs et courts.
zones <- bind_rows(
  df_long  |> select(taille_commune, zone_declaree, zone_contradictoire, zone2, zone3,
                     tranche_insee, G1Q00001),
  df_court |> select(taille_commune, zone_declaree, zone_contradictoire, zone2, zone3,
                     tranche_insee, G1Q00001)
)

# Les réponses contradictoires mises de côté
sum(zones$zone_contradictoire)  # 40
table(zones$taille_commune[zones$zone_contradictoire],
      zones$zone_declaree[zones$zone_contradictoire], useNA = "no")
# 36 "grande ville + rural" et 4 "hameau + urbain"

# Ce que deviennent les périurbains
periurbains <- zones |> filter(zone_declaree %in% "Périurbain")
table(periurbains$taille_commune, periurbains$zone2, useNA = "ifany")
# 55 passent en rural, 364 en urbain, 9 restent sans zone (taille inconnue)

### 1. À taille de commune égale, la zone colle-t-elle à l'INSEE ?
controle_taille <- zones |>
  filter(!is.na(zone2), !is.na(taille_commune)) |>
  group_by(taille_commune) |>
  summarise(n = n(), rural_declare = round(100 * mean(zone2 == "Rural"), 1),
            .groups = "drop") |>
  left_join(rural_insee |> select(taille_commune, rural_insee), by = "taille_commune")

controle_taille
# Oui : petite ville 33.8 % de ruraux contre 31.2 %, grande ville 0 contre 0.
# Seul écart notable, les villages : 97.4 % se disent ruraux quand l'INSEE en
# classe 85.5 %.

### 2. La marge : part de ruraux dans l'échantillon et dans la population
part_rurale_enquete <- round(100 * mean(zones$zone2 == "Rural", na.rm = TRUE), 1)
part_rurale_insee   <- round(100 * sum(pop_densite$PMUN23[pop_densite$LIBDENS == "Rural"]) /
                               sum(pop_densite$PMUN23), 1)
c(enquete = part_rurale_enquete, insee = part_rurale_insee)  # 38.7 contre 32.1



#####################################################
### La cible pour le redressement : deux versions ###
#####################################################

# Comparer la zone déclarée à la grille INSEE, c'est comparer une perception à
# une définition administrative. Même un échantillon parfaitement représentatif
# déclarerait probablement plus de "rural" que l'INSEE n'en classe ou inversement.
#
# Une partie de l'écart ne vient donc pas de l'échantillon mais de la façon dont
# les gens décrivent leur lieu de vie.
#
# On calcule donc ce qu'aurait déclaré un échantillon parfait : la part des
# Français vivant dans chaque taille de commune (INSEE), multipliée par la part
# des enquêtés de cette taille qui se disent ruraux (enquête). C'est la cible
# "corrigée de la perception".
# Hypothèse : à taille de commune égale, les Français décrivent leur zone
# comme nos enquêtés.

contributions <- rural_insee |>
  select(taille_commune, part_insee) |>
  left_join(controle_taille |> select(taille_commune, rural_declare), by = "taille_commune") |>
  mutate(contribution = round(part_insee * rural_declare / 100, 1))

contributions  # 10.4 + 11.4 + 12.9 + 1.2 + 0

part_rurale_corrigee <- round(sum(contributions$part_insee * contributions$rural_declare / 100), 1)
part_rurale_corrigee  # 35.9 % de ruraux dans un échantillon parfait

# Les deux cibles possibles pour zone2, et les poids qu'elles impliquent en
# ordre de grandeur (cible divisée par la part observée)
cibles_zone2 <- data.frame(
  zone2          = c("Rural", "Urbain"),
  observe        = c(part_rurale_enquete, 100 - part_rurale_enquete),
  cible_insee    = c(part_rurale_insee, 100 - part_rurale_insee),
  cible_corrigee = c(part_rurale_corrigee, 100 - part_rurale_corrigee)
) |>
  mutate(poids_insee    = round(cible_insee / observe, 2),
         poids_corrigee = round(cible_corrigee / observe, 2))

cibles_zone2
# Ruraux : 38.7 % observés, cible 32.1 % (poids 0.83) ou 35.9 % (poids 0.93)
# Urbains : 61.3 % observés, cible 67.9 % (poids 1.11) ou 64.1 % (poids 1.05)

# Lecture de l'écart entre l'échantillon et la cible INSEE brute
c(ecart_total         = round(part_rurale_enquete - part_rurale_insee, 1),
  facon_de_se_decrire = round(part_rurale_corrigee - part_rurale_insee, 1),
  vrai_desequilibre   = round(part_rurale_enquete - part_rurale_corrigee, 1))
# 6.6 points = 3.8 dus à la façon de se décrire (à ne pas corriger)
#            + 2.8 de vrai déséquilibre de l'échantillon (à corriger)

# Pour le redressement, on retient la cible corrigée : elle ne corrige que le
# vrai déséquilibre de l'échantillon.


#########################################
### Pour le calage croisé  ###
#########################################

# Effectifs des cellules âge x zone x sexe sur l'ensemble de l'échantillon
cellules <- zones |>
  filter(!is.na(tranche_insee), !is.na(zone2), G1Q00001 %in% c("Femme", "Homme")) |>
  count(tranche_insee, zone2, G1Q00001, name = "effectif")

nrow(cellules)                     # 44 cellules remplies sur 44
summary(cellules$effectif)         # médiane 63.5 répondants
cellules |> filter(effectif < 5)   # aucune
# La plus petite cellule, les hommes de 18-19 ans en zone rurale, compte 9
# répondants : les jeunes des questionnaires courts remplissent ces cellules.


###############################
### Conclusion              ###
###############################

# La perception de la zone est valide : une fois les périurbains reclassés, à
# taille de commune égale, les enquêtés décrivent leur zone comme l'INSEE.
#
# La marge reste décalée : 38.7 % de ruraux contre 32.1 % à l'INSEE. Mais sur
# ces 6.6 points, 3.8 tiennent à la façon dont les gens décrivent leur lieu de
# vie, et existeraient même avec un échantillon parfait. Seuls 2.8 points
# traduisent un vrai déséquilibre de l'échantillon (trop d'habitants de villages).
#
# La taille de commune n'ayant pas fait l'objet d'un quota, ce
# déséquilibre est donc réel. zone2 est donc utilisable pour
# le redressement, sur la cible corrigée de la perception (35.9 % de ruraux) :
# poids d'environ 0.93 pour les ruraux et 1.05 pour les urbains. Le calage
# croisé âge x zone x sexe est faisable.
#
# zone3 ne sert qu'à l'analyse : ses trois modalités ne correspondent à aucune
# nomenclature INSEE (voir 3. Variable geographie.R).
#
# Limites : les 40 réponses contradictoires sont mises de côté, à examiner ; la
# cible corrigée suppose qu'à taille égale les Français se décrivent comme nos
# enquêtés ; les cibles portent sur la population totale et non sur les 18 ans
# et plus.
