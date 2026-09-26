# Version finale du redressement - avec seulement age*sexe*statut_co (après recodage par Antoine)

# On importe les outputs du script 4 d'Antoine
source("2. Scripts/4. Variable zone.R")

###############################
### Chargement des packages ### 
###############################

library(tidyverse)
library(questionr)
library(openxlsx)
library(survey)


###############################
### Importation des données ###
###############################

# On concatenne le df long et le df long
df <- dplyr::bind_rows(df_long, df_court)

# Les variables qui comptent sont :
# - zone2
# - tranche_insee (âge en 18-19, 20-24...)
# - G1Q00001 (le sexe)

# Grille de densité 2025 (géographie au 01/01/2026) : une ligne par commune,
# avec sa population municipale 2023 et son degré de densité (3 postes).
pop_densite <- read.xlsx("1. Data/Public data/grille_densite_2026.xlsx",
                         sheet = "Maille communale", startRow = 5) |>
  select(CODGEO, PMUN23, LIBDENS) |>
  mutate(PMUN23 = as.numeric(PMUN23)) |>
  filter(!startsWith(CODGEO, "976"))  # on écarte Mayotte, comme dans le redressement

# Population des 15 ans et plus (2022) par commune stratifiée par catégorie d'âge, sexe et statut d'emploi
pop_insee <- read.csv("1. Data/Public data/TD_POP1B_2022.csv", sep=";")
colnames(pop_insee)
pop_insee <- pop_insee |> 
  filter(AGED100 >= 18)

# On fusionne les deux grilles INSEE
pop_densite <- pop_densite |> 
  left_join(pop_insee, by="CODGEO")

# On crée les catégories d'âge
pop_densite <- pop_densite |> 
  mutate(AGE_large = case_when(
    AGED100 < 35 ~ "18-34",
    AGED100 >= 35 & AGED100 < 50 ~ "35-49",
    AGED100 >= 50 & AGED100 < 65 ~ "50-64",
    AGED100 >= 65 ~ "65+",
    TRUE ~ NA))

# De même dans le df
df <- df |> 
  mutate(AGE_large = case_when(
    tranche_bilendi %in% c("18-24", "25-29", "30-34") ~ "18-34",
    tranche_bilendi %in% c("35-39", "40-44", "45-49") ~ "35-49",
    tranche_bilendi %in% c("50-54", "55-59", "60-64") ~ "50-64",
    tranche_bilendi == "65+" ~ "65+"
  ))

# On crée une variable "zone2" comme dans le df recodé en script 2
pop_densite <- pop_densite |> 
  mutate(zone2 =
           case_when(
             LIBDENS == "Rural" ~ "Rural",
             LIBDENS %in% c("Urbain dense", "Urbain intermédiaire") ~ "Urbain"
           ))

# On groupe par sexe, âge et type de commune et on calcule les effectifs croisés
pop_densite_group <- pop_densite |> 
  group_by(AGE_large, SEXE, zone2) |> 
  summarise(pop_strat = sum(NB, na.rm=TRUE))

# On convertit le sexe en facteur dans le df
df <- df |> mutate(SEXE = factor(G1Q00001,
                                 levels = c("Homme", "Femme")))

# On transforme le sexe en facteur (comme dans le df)
pop_densite_group <- pop_densite_group |> 
  mutate(SEXE = factor(
    case_when(
      SEXE == 1 ~ "Homme", 
      SEXE == 2 ~ "Femme"
    ),
    levels = c("Homme", "Femme")
  ))


######################
#### Redressement ####
######################

# Nettoyage préalable
#--------------------

# On supprime (temporairement) les individus mal décrits sur certaines variables
# (par ex les "Autre" sur le sexe, les "NSP" sur le statut...)

# Sur le sexe
table(df$SEXE)
df <- df |> filter(!SEXE %in% c("Ne sait pas / Ne se prononce pas", "Autre"))
df <- df |> filter(!is.na(SEXE))

# Sur le type de commune
table(df$zone2)
df <- df |> filter(!is.na(zone2))

# Sur la catégorie d'âge 
table(df$AGE_large)
sum(is.na(df$AGE_large))
df <- df |> filter(!is.na(AGE_large))

# On nettoie les niveaux non utilisés
df <- droplevels(df)


# Redressement en pratique
#-------------------------

# On crée un poids initial uniforme
df$poids_initial <- 1

# On crée un objet de type "survey"
df_design <- svydesign(
  ids = ~1,
  weights = ~poids_initial,
  data = df
)

### On construit les tableaux avec les distributions marginales

# Sur le type de commune
tab_pop_croisee_typeco <- pop_densite_group |> 
  group_by(zone2) |> 
  summarise(Freq = sum(pop_strat, na.rm = TRUE))

# Sur le sexe
tab_pop_croisee_sexe <- pop_densite_group |> 
  group_by(SEXE) |> 
  summarise(Freq = sum(pop_strat, na.rm = TRUE))

# Sur l'âge
tab_pop_croisee_age <- pop_densite_group |> 
  group_by(AGE_large) |> 
  summarise(Freq = sum(pop_strat, na.rm = TRUE))


## Puis avec les distributions jointes

df <- df |>
  mutate(strate = interaction(
    AGE_large, SEXE, zone2,
    drop = TRUE
  ))

pop_joint <- pop_densite_group |>
  mutate(strate = interaction(
    AGE_large, SEXE, zone2,
    drop = TRUE
  )) |>
  group_by(strate) |>
  summarise(Freq = sum(pop_strat), .groups = "drop")


# On utilise la fonction rake() de survey pour créer les poids
design_post <- postStratify(
  df_design,
  strata = ~strate,
  population = pop_joint
)


# Vérifications et description des poids
#---------------------------------------

# On regarde les poids extrêmes
idx <- order(weights(design_post), decreasing = TRUE)[1:10]
design_post$variables[idx, c(
  "AGE_large",
  "SEXE",
  "zone2"
)]

# On regarde la distribution des poids
w <- weights(design_post)
summary(w)

# On regarde les quantiles
quantile(
  w,
  probs = c(0, .01, .05, .10, .25, .50, .75, .90, .95, .99, 1)
) # de 10 314 à 24 651

# On calcule l'Effective Sample Size (ESS)
ESS <- sum(w)^2 / sum(w^2)
c(
  n = length(w), # 3185 individus dans la base
  ESS = ESS, # ESS = 3125
  # (en termes de précision statistique, c'est "comme si" on avait 2985 individus, à cause de la variance des poids)
  taux_ESS = ESS / length(w) # le taux est de 0,94
)


# On injecte les poids dans le df
#--------------------------------

# On crée une variable avec les poids (l'ordre est le même)
df$poids <- weights(design_post)

# On vérifie la structure de la variable
summary(df$poids) # entre 10 315 et 24 651
sum(df$poids) # on a un total de 53 483 601 d'individus (soit la population française des 18+)

# On crée un poids normalisé
df <- df |> 
  mutate(poids_norm = poids * nrow(df) / sum(poids))

# On vérifie la structure de la variable
summary(df$poids_norm) # chaque individu représente entre 0.61 et 1.47 individu à présent
sum(df$poids_norm) # on a bien un total de 3 185 individus maintenant

