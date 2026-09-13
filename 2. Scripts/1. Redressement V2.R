rm(list=ls())
gc()

# V2 du redressement - avec seulement age*sexe*statut_co

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

### Les données de l'enquête

# La vague longue (pour l'exemple, mais on pourra ajuster en mettant le df final !)
df_long <- read.csv("1. Data/panel_long.csv", sep=",")

### Les données externes (publiques et agrégées)

# Population des 15 ans et plus (2022) par commune stratifiée par catégorie d'âge, sexe et statut d'emploi
pop_insee <- read.csv("1. Data/Public data/TD_ACT1V3_2022.csv", sep=";")
colnames(pop_insee)

# Grille de densité des communes (2023) pour avoir le nombre d'habitants et le type d'habitat (rural, périurbain, rural) 
pop_densite <- openxlsx::read.xlsx("1. Data/Public data/grille_densite_2026.xlsx", sep=";")
colnames(pop_densite) <- pop_densite[4,]
pop_densite <- pop_densite[5:nrow(pop_densite), ]
pop_densite <- pop_densite |> 
  select(CODGEO, DENS, PMUN23) |> 
  mutate(PMUN23 = as.numeric(PMUN23))


# On regarde les divergences entre les deux df sur les communes enregistrées
setdiff(pop_insee$CODGEO, pop_densite$CODGEO) # la grille de densité n'a pas les arrondissements
setdiff(pop_densite$CODGEO, pop_insee$CODGEO) # le fichier insee n'a pas Mayotte

# On regroupe les arrondissements dans le fichier insee
pop_insee <- pop_insee |> 
  mutate(CODGEO = case_when(
    startsWith(CODGEO, "132") ~ "13055",
    startsWith(CODGEO, "6938") ~ "69123",
    startsWith(CODGEO, "751") ~ "75056",
    TRUE ~ CODGEO
  ))

# On supprime les lignes correspondant à Mayotte dans le fichier des densités
pop_densite <- pop_densite |> 
  filter(!startsWith(CODGEO, "976"))

# On vérifie qu'il n'y a plus de divergences
setdiff(pop_insee$CODGEO, pop_densite$CODGEO) # 0
setdiff(pop_densite$CODGEO, pop_insee$CODGEO) # 0

# On joint le fichier des 15 ans et plus à la grille de densité
pop_totale <- pop_insee %>%
  left_join(pop_densite) |> 
  select(CODGEO, SEXE, AGED65, NB, PMUN23) |> 
  rename(pop_stratifiee = NB,
         pop_commune = PMUN23) |> 
  mutate(SEXE = ifelse(
    SEXE == 1, "Homme", "Femme"
  ))


#################################
### Statistiques descriptives ###
#################################

# Part de chaque type de communes
#--------------------------------

### Dans la population totale

# On crée une variable avec la catégorie de communes (à partir de la grille de densité)
pop_totale <- pop_totale |> 
  # On crée la variable de type de commune (comme facteur)
  mutate(type_co = factor(
    case_when(
      pop_commune < 800 ~ "Hameau",
      pop_commune >= 800 & pop_commune < 2000 ~ "Village",
      pop_commune >= 2000 & pop_commune < 20000 ~ "Petite ville",
      pop_commune >= 20000 & pop_commune < 100000 ~ "Ville moyenne",
      pop_commune >= 100000 ~ "Grande ville",
    TRUE ~ NA
  ),
  # On définit l'ordre des niveaux
  levels = c(
    "Hameau",
    "Village",
    "Petite ville",
    "Ville moyenne",
    "Grande ville"
  )
  ))

# On regarde le nombre de lignes non renseignées sur la population non communale
sum(is.na(pop_totale$pop_commune)) # 0

# On regarde s'il y a des NAs dans les populations stratifiées
pop_totale |> 
  summarise(n_NA = sum(is.na(pop_stratifiee))) # 528 NAs

# Ils sont tous parmi des hameaux
pop_totale |> 
  filter(type_co == "Hameau") |> 
  summarise(n_NA = sum(is.na(pop_stratifiee)))

# C'est un problème car on sous-estime probablement la population dans les hameaux !

# On regroupe les communes par catégorie et on somme leur nombre d'individus
tab_pop_commune_densite <- pop_totale |>
  distinct(CODGEO, type_co, pop_commune) |>
  group_by(type_co) |>
  summarise(pop_commune_tot = sum(pop_commune)) |> 
  mutate(pop_commune_prop = pop_commune_tot / sum(pop_commune_tot))


### Dans la population des 15 ans et plus

# On calcule la population par commune
tab_pop_commune_insee <- pop_totale |> 
  group_by(CODGEO, type_co) |> 
  summarise(
    pop_commune = sum(pop_stratifiee, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  group_by(type_co) |> 
  summarise(
    pop_commune_insee = sum(pop_commune, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(
    pop_commune_insee_prop = pop_commune_insee / sum(pop_commune_insee)
  )


### Parmi nos enquêtés

# On renomme les modalités de la variable sur le type de commune
df_long <- df_long |> 
  mutate(
  G2Q00001 = factor(recode(
    G2Q00001,
    "Dans un hameau (moins de 800 habitant·es)" = "Hameau",
    "Dans un village (800-2000 habitant·es)" = "Village",
    "Dans une petite ville (2000-20 000 habitant·es)" = "Petite ville",
    "Dans une ville moyenne (20 000 - 100 000 habitant·es)" = "Ville moyenne",
    "Dans une grande ville (+ de 100 000 habitant·es)" = "Grande ville"),
  levels = c("Hameau", "Village", "Petite ville", "Ville moyenne", "Grande ville", "Ne sait pas / Ne se prononce pas"))) 

# Au passage, on renomme les variables du sexe et du type de commune dans notre df
df_long <- df_long |> 
  rename('type_co' = 'G2Q00001',
         'SEXE' = 'G1Q00001')

# On crée la table pour la population de l'enquête
tab_pop_enquete <- df_long |> 
  group_by(type_co) |> 
  summarise(
    pop_enquete = n(),
    .groups = "drop"
  ) |> 
  mutate(
    pop_enquete_prop = pop_enquete / sum(pop_enquete)
  )

# On regarde les différences entre les modalités pour le type de commune
setdiff(tab_pop_commune_densite$type_co, tab_pop_enquete$type_co)
setdiff(tab_pop_commune_densite$type_co, tab_pop_commune_insee$type_co)
setdiff(tab_pop_enquete$type_co, tab_pop_commune_insee$type_co)
setdiff(tab_pop_enquete$type_co, tab_pop_commune_densite$type_co)
setdiff(tab_pop_commune_insee$type_co, tab_pop_commune_densite$type_co)
setdiff(tab_pop_commune_insee$type_co, tab_pop_enquete$type_co)

# Il y a seulement les "Ne sait pas / Ne se prononce pas" qui diffèrent (présents seulement dans l'enquête)

# On fusionne les trois tables 
tab_pop <- tab_pop_enquete |>
  left_join(tab_pop_commune_insee, by='type_co') |> 
  left_join(tab_pop_commune_densite, by='type_co') |> 
  select("type_co", "pop_enquete", "pop_commune_insee", "pop_commune_tot",
         "pop_enquete_prop", "pop_commune_insee_prop", "pop_commune_prop") |> 
  arrange(type_co)

# On supprime les tables intermédiaires
rm(tab_pop_commune_densite, tab_pop_commune_insee, tab_pop_enquete)

# On exporte la table de population finale
write.csv(tab_pop, "3. Outputs/tab_popV2.csv")


# Croisement Age*sexe*type_commune
#--------------------------------

### Population totale des 18 ans et plus (recensement)

# On se restreint aux 18 ans et plus
pop_totale <- pop_totale |> 
  filter(AGED65 >= 18)

# On renomme les catégories d'âge dans le fichier de la population totale
pop_totale <- pop_totale |> 
  mutate(AGE_cat = case_when(
    AGED65 >= 18 & AGED65 < 20 ~ "18-19",
    AGED65 >= 20 & AGED65 < 25 ~ "20-24",
    AGED65 >= 25 & AGED65 < 30 ~ "25-29",
    AGED65 >= 30 & AGED65 < 35 ~ "30-34",
    AGED65 >= 35 & AGED65 < 40 ~ "35-39",
    AGED65 >= 40 & AGED65 < 45 ~ "40-44",
    AGED65 >= 45 & AGED65 < 50 ~ "45-49",
    AGED65 >= 50 & AGED65 < 55 ~ "50-54",
    AGED65 >= 55 & AGED65 < 60 ~ "55-59",
    AGED65 >= 60 & AGED65 < 65 ~ "60-64",
    AGED65 >= 65 ~ "65 et plus"
  )) |> 
  select(-AGED65)

# On crée les mêmes catégories dans notre df
# (à reprendre quand la variable finale d'âge sera prête)
df_long <- df_long |> 
  mutate(AGE_cat = case_when(
    G1Q00002_other < 20 | G1Q00002 < 20 ~ "18-19",
    (G1Q00002_other >= 20 & G1Q00002_other < 25) | (G1Q00002 >= 20 & G1Q00002 < 25) ~ "20-24",
    (G1Q00002_other >= 25 & G1Q00002_other < 30) | (G1Q00002 >= 25 & G1Q00002< 30) ~ "25-29",
    (G1Q00002_other >= 30 & G1Q00002_other < 35) | (G1Q00002 >= 30 & G1Q00002 < 35) ~ "30-34",
    (G1Q00002_other >= 35 & G1Q00002_other < 40) | (G1Q00002 >= 35 & G1Q00002 < 40) ~ "35-39",
    (G1Q00002_other >= 40 & G1Q00002_other < 45) | (G1Q00002 >= 40 & G1Q00002 < 45) ~ "40-44",
    (G1Q00002_other >= 45 & G1Q00002_other < 50) | (G1Q00002 >= 45 & G1Q00002 < 50) ~ "45-49",
    (G1Q00002_other >= 50 & G1Q00002_other < 55) | (G1Q00002 >= 50 & G1Q00002 < 55) ~ "50-54",
    (G1Q00002_other >= 55 & G1Q00002_other < 60) | (G1Q00002 >= 55 & G1Q00002 < 60) ~ "55-59",
    (G1Q00002_other >= 60 & G1Q00002_other < 65) | (G1Q00002 >= 60 & G1Q00002 < 65) ~ "60-64",
    (G1Q00002_other >= 65) | (G1Q00002 >= 65) ~ "65 et plus",
    TRUE ~ NA
  ))
table(df_long$AGE_cat)

# On crée une table agrégée par sexe*age*type_co dans la population insee
tab_pop_croisee_insee <- pop_totale |> 
  group_by(type_co, SEXE, AGE_cat) |> 
  summarise(
    pop_stratifiee_insee = sum(pop_stratifiee, na.rm = TRUE),
    .groups = "drop"
  )


### Population de l'enquête

# On crée une table agrégée par sexe*age*type_co dans la population enquêtée
tab_pop_croisee_enquete <- df_long |> 
  group_by(type_co, SEXE, AGE_cat) |> 
  summarise(
    pop_stratifiee_enquete = n(),
    .groups = "drop"
  )

### On fusionne les deux tables

# On vérifie les différences dans les modalités pour le type de commune
setdiff(tab_pop_croisee_enquete$type_co, tab_pop_croisee_insee$type_co)
setdiff(tab_pop_croisee_insee$type_co, tab_pop_croisee_enquete$type_co)

# Encore les "Ne sait pas / Ne se prononce pas" qui n'existent que dans l'enquête

# Cependant, il y a forcément des croisements age*sexe*type_co_statut qui manquent dans notre enquête

# Par conséquent, on fait un full_join (pour conserver tous les croisements présents dans la table insee,
# ainsi que la modalité de type_co présente uniquement dans la table d'enquête)

# Fusion
tab_pop_croisee <- tab_pop_croisee_enquete |> full_join(tab_pop_croisee_insee, by=c("AGE_cat", "type_co",
                                                                                     "SEXE")) |> 
  arrange(type_co, SEXE, AGE_cat)

# On observe des NAs dans la population insee ET dans la population de l'enquête
# On les remplace par des zéros dans l'enquête (c'est le cas)
# Cependant, dans la pop insee, ce ne sont pas des "vrais" zéros - il faudra décider de ce qu'on en fait !

# On indique les NAs pour la pop de l'enquête comme des zéros
tab_pop_croisee <- tab_pop_croisee |> 
  mutate(pop_stratifiee_enquete = ifelse(
    is.na(pop_stratifiee_enquete), 0, pop_stratifiee_enquete
  ))

# On vérifie le nombre de NAs sur la pop insee
tab_pop_croisee |> 
  summarise(n_NA = sum(is.na(pop_stratifiee_insee))) # 34 cas

# Ils ne sont pas tous dans les hameaux ! Surtout parmi les "ne sait pas" (logique car cette catégorie n'existe pas...)
tab_pop_croisee |> 
  filter(type_co == "Hameau") |> 
  summarise(n_NA = sum(is.na(pop_stratifiee_insee))) # 3
tab_pop_croisee |> 
  filter(type_co == "Village") |> 
  summarise(n_NA = sum(is.na(pop_stratifiee_insee))) # 3
tab_pop_croisee |> 
  filter(type_co == "Petite ville") |> 
  summarise(n_NA = sum(is.na(pop_stratifiee_insee))) # 4
tab_pop_croisee |> 
  filter(type_co == "Ville moyenne") |> 
  summarise(n_NA = sum(is.na(pop_stratifiee_insee))) # 6
tab_pop_croisee |> 
  filter(type_co == "Grande ville") |> 
  summarise(n_NA = sum(is.na(pop_stratifiee_insee))) # 6
tab_pop_croisee |> 
  filter(type_co == "Ne sait pas / Ne se prononce pas") |> 
  summarise(n_NA = sum(is.na(pop_stratifiee_insee))) # 12

# On supprime les tables intermédiaires
rm(tab_pop_croisee_insee, tab_pop_croisee_enquete)

# On exporte la table croisée finale
write.csv(tab_pop_croisee, "3. Outputs/tab_pop_croiseeV2.csv")


######################
#### Redressement ####
######################

# Nettoyage préalable
#--------------------

# Le tableau croisé qu'on a produit plus haut est très fin par rapport au nombre d'individus dont on dispose
# Par conséquent, beaucoup de cases ont des zéros : les poids risquent d'être très instables

# Dans un premier temps, on va donc se caler sur les distributions marginales (et non jointes)

# On supprime (temporairement) les individus mal décrits sur certaines variables
# (par ex les "Autre" sur le sexe, les "NSP" sur le statut...)

# Sur le sexe (5 individus)
table(df_long$SEXE)
df_long <- df_long |> filter(!SEXE %in% c("Ne sait pas / Ne se prononce pas", "Autre"))

# Sur le type de commune (21 individus)
table(df_long$type_co)
df_long <- df_long |> filter(!type_co == "Ne sait pas / Ne se prononce pas")

# Sur la catégorie d'âge (0 individu)
table(df_long$AGE_cat)

# On nettoie les niveaux non utilisés
df_long <- droplevels(df_long)


# Redressement en pratique
#-------------------------

# On crée un poids initial uniforme
df_long$poids_initial <- 1

# On crée un objet de type "survey"
df_design <- svydesign(
  ids = ~1,
  weights = ~poids_initial,
  data = df_long
)

### On construit les tableaux avec les distributions marginales

# Sur le type de commune
tab_pop_croisee_typeco <- tab_pop_croisee |> 
  group_by(type_co) |> 
  summarise(Freq = sum(pop_stratifiee_insee, na.rm = TRUE))

# Sur le sexe
tab_pop_croisee_sexe <- tab_pop_croisee |> 
  group_by(SEXE) |> 
  summarise(Freq = sum(pop_stratifiee_insee, na.rm = TRUE))

# Sur l'âge
tab_pop_croisee_age <- tab_pop_croisee |> 
  group_by(AGE_cat) |> 
  summarise(Freq = sum(pop_stratifiee_insee, na.rm = TRUE))


# On utilise la fonction rake() de survey pour créer les poids
design_rake <- rake(
  df_design,
  sample.margins = list(
    ~AGE_cat,
    ~SEXE,
    ~type_co
  ),
  population.margins = list(
    tab_pop_croisee_age,
    tab_pop_croisee_sexe,
    tab_pop_croisee_typeco
  )
)


# Vérifications et description des poids
#---------------------------------------

# On regarde les poids extrêmes
idx <- order(weights(design_rake), decreasing = TRUE)[1:10]
df_design$variables[idx, c(
  "AGE_cat",
  "SEXE",
  "type_co"
)] # rien de choquant a priori

# On regarde la distribution des poids
w <- weights(design_rake)
summary(w)

# On regarde les quantiles
quantile(
  w,
  probs = c(0, .01, .05, .10, .25, .50, .75, .90, .95, .99, 1)
)

# On calcule l'Effective Sample Size (ESS)
ESS <- sum(w)^2 / sum(w^2)
c(
  n = length(w), # 2812 individus dans la base
  ESS = ESS, # ESS = 1559
  # (en termes de précision statistique, c'est "comme si" on avait 1559 individus, à cause de la variance des poids)
  taux_ESS = ESS / length(w) # le taux est de 0,55 (plutôt instable mais ça va vu le nombre de marges utilisées)
)

