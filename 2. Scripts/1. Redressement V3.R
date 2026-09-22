rm(list=ls())
gc()

# V3 du redressement - avec seulement age*sexe*statut_co (après recodage par Antoine)

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
pop_insee <- read.csv("1. Data/Public data/TD_ACT1V3_2022.csv", sep=";")
colnames(pop_insee)

# On fusionne les deux grilles INSEE
pop_densite <- pop_densite |> 
  left_join(pop_insee, by="CODGEO")

pop_densite <- pop_densite |> 
  mutate(tranche_bilendi = case_when(
    AGED65 < 25 ~ "18-24",
    AGED65 >= 25 & AGED65 < 30 ~ "25-29",
    AGED65 >= 30 & AGED65 < 35 ~ "30-34",
    AGED65 >= 35 & AGED65 < 40 ~ "35-39",
    AGED65 >= 40 & AGED65 < 45 ~ "40-44",
    AGED65 >= 45 & AGED65 < 50 ~ "45-49",
    AGED65 >= 50 & AGED65 < 55 ~ "50-54",
    AGED65 >= 55 & AGED65 < 60 ~ "55-59",
    AGED65 >= 60 & AGED65 < 65 ~ "60-64",
    AGED65 >= 65 ~ "65+",
    TRUE ~ NA))

# On groupe par sexe, âge et type de commune et on calcule les effectifs croisés
pop_densite_group <- pop_densite |> 
  group_by(tranche_bilendi, SEXE, LIBDENS) |> 
  summarise(pop_strat = sum(NB, na.rm=TRUE))

# On transforme le sexe en facteur (comme dans le df)
pop_densite_group <- pop_densite_group |> 
  mutate(SEXE = factor(
    case_when(
      SEXE == 1 ~ "Homme", 
      SEXE == 2 ~ "Femme"
    ),
    levels = c("Homme", "Femme")
  ))

# On renomme les catégories en urbain / périurbain / rural
pop_densite_group <- pop_densite_group |> 
  mutate(LIBDENS = case_when(
    LIBDENS == "Rural" ~ "Rural",
    LIBDENS == "Urbain intermédiaire" ~ "Périurbain",
    LIBDENS == "Urbain dense" ~ "Urbain"
  )) |> 
  rename("zone3" = "LIBDENS")

# On convertit le sexe en facteur
df <- df |> mutate(SEXE = factor(G1Q00001,
                                 levels = c("Homme", "Femme")))


######################
#### Redressement ####
######################

# Nettoyage préalable
#--------------------

# On supprime (temporairement) les individus mal décrits sur certaines variables
# (par ex les "Autre" sur le sexe, les "NSP" sur le statut...)

# Sur le sexe (5 individus)
table(df$SEXE)
df <- df |> filter(!SEXE %in% c("Ne sait pas / Ne se prononce pas", "Autre"))
df <- df |> filter(!is.na(SEXE))

# Sur le type de commune (21 individus)
table(df$zone3)
df <- df |> filter(!is.na(zone3))

# Sur la catégorie d'âge 
table(df$tranche_bilendi)
sum(is.na(df$tranche_bilendi))
df <- df |> filter(!is.na(tranche_bilendi))

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
  group_by(zone3) |> 
  summarise(Freq = sum(pop_strat, na.rm = TRUE))

# Sur le sexe
tab_pop_croisee_sexe <- pop_densite_group |> 
  group_by(SEXE) |> 
  summarise(Freq = sum(pop_strat, na.rm = TRUE))

# Sur l'âge
tab_pop_croisee_age <- pop_densite_group |> 
  group_by(tranche_bilendi) |> 
  summarise(Freq = sum(pop_strat, na.rm = TRUE))

# On utilise la fonction rake() de survey pour créer les poids
design_rake <- rake(
  df_design,
  sample.margins = list(
    ~tranche_bilendi,
    ~SEXE,
    ~zone3
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
  "AgeTranche",
  "SEXE",
  "zone3"
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
  ESS = ESS, # ESS = 3193
  # (en termes de précision statistique, c'est "comme si" on avait 1720 individus, à cause de la variance des poids)
  taux_ESS = ESS / length(w) # le taux est de 0,54 (plutôt instable mais ça va vu le nombre de marges utilisées)
)



# On regroupe les catégories d'âge (essai 2)
#--------------------------------------------

# Dans le df
df <- df |> 
  mutate(tranche_bilendi2 = case_when(
    tranche_bilendi %in% c("18-24", "25-29", "30-34") ~ "18-34",
    tranche_bilendi %in% c("35-39", "40-44", "45-49") ~ "35-49",
    tranche_bilendi %in% c("50-54", "55-59", "60-64") ~ "50-64",
    tranche_bilendi == "65+" ~ "65+"
  ))

# Dans la table agrégée
pop_densite_group <- pop_densite_group |> 
  mutate(tranche_bilendi2 = case_when(
    tranche_bilendi %in% c("18-24", "25-29", "30-34") ~ "18-34",
    tranche_bilendi %in% c("35-39", "40-44", "45-49") ~ "35-49",
    tranche_bilendi %in% c("50-54", "55-59", "60-64") ~ "50-64",
    tranche_bilendi == "65+" ~ "65+"
  ))
pop_densite_group_bis <- pop_densite_group |> 
  group_by(tranche_bilendi2, SEXE, zone3) |> 
  summarise(pop_strat = sum(pop_strat))

# On reconstruit la table agrégée par âge (tranches plus larges)
tab_pop_croisee_age_bis <- pop_densite_group_bis |> 
  group_by(tranche_bilendi2) |> 
  summarise(Freq = sum(pop_strat, na.rm = TRUE))


### On refait le redressement

# On crée un poids initial uniforme
df$poids_initial_bis <- 1

# On crée un objet de type "survey"
df_design_bis <- svydesign(
  ids = ~1,
  weights = ~poids_initial_bis,
  data = df
)

# On utilise la fonction rake() de survey pour créer les poids
design_rake_bis <- rake(
  df_design_bis,
  sample.margins = list(
    ~tranche_bilendi2,
    ~SEXE,
    ~zone3
  ),
  population.margins = list(
    tab_pop_croisee_age_bis,
    tab_pop_croisee_sexe,
    tab_pop_croisee_typeco
  )
)

# On regarde les poids extrêmes
idx_bis <- order(weights(design_rake_bis), decreasing = TRUE)[1:10]
df_design_bis$variables[idx_bis, c(
  "tranche_bilendi2",
  "SEXE",
  "zone3"
)] 

# On regarde la distribution des poids
w_bis <- weights(design_rake_bis)
summary(w_bis)

# On regarde les quantiles
quantile(
  w_bis,
  probs = c(0, .01, .05, .10, .25, .50, .75, .90, .95, .99, 1)
)

# On calcule l'Effective Sample Size (ESS)
ESS_bis <- sum(w_bis)^2 / sum(w_bis^2)
c(
  n = length(w_bis), # 3193 individus dans la base
  ESS_bis = ESS_bis, # ESS = 1814
  # (en termes de précision statistique, c'est "comme si" on avait 1814 individus, à cause de la variance des poids)
  taux_ESS_bis = ESS_bis / length(w_bis) # le taux est de 0,56 (plutôt instable mais ça va vu le nombre de marges utilisées)
)


### On fait des diagnostics

# On regarde les poids extrêmes
df_w <- df_design_bis$variables |>
  mutate(poids = weights(design_rake_bis))
df_w |>
  group_by(tranche_bilendi2, SEXE, zone3) |>
  summarise(
    n = n(),
    poids_moyen = mean(poids),
    poids_min = min(poids),
    poids_max = max(poids),
    poids_total = sum(poids),
    .groups = "drop"
  ) |>
  arrange(desc(poids_moyen))

# Interprétation : le périurbain pose le plus de problèmes

### On regarde le nombre d'individus par case

# On compte dans le df
sample_cells <- df |>
  count(tranche_bilendi2, SEXE, zone3, name = "n_sample")

# On compte dans la pop insee
pop_cells <- pop_densite_group_bis |>
  group_by(tranche_bilendi2, SEXE, zone3) |>
  summarise(
    n_pop = sum(pop_strat),
    .groups = "drop"
  )

# On compare en calculant le ratio pop / échantillon pour chaque catégorie
diagnostic <- sample_cells |>
  full_join(
    pop_cells,
    by = c("tranche_bilendi2", "SEXE", "zone3")
  ) |>
  mutate(
    ratio_pop_sample = n_pop / n_sample
  ) |>
  arrange(desc(ratio_pop_sample))

# On trouve un problème global chez les moins de 65 ans, et dans le périurbain
diagnostic


# On met le périurbain dans l'urbain (essai 3)
#---------------------------------------------

# Pour le df, on a juste la variable zone2 qui existe déjà
table(df$zone2)
sum(is.na(df$zone2))
df <- df |> 
  filter(!is.na(zone2))

# Pour la table agrégée, on essaie de mettre ensemble urbain et périurbain (ie dense et intermédiaire)
pop_densite_group <- pop_densite_group |> 
  mutate(zone2 = case_when(
    zone3 %in% c("Urbain", "Périurbain") ~ "Urbain",
    zone3 == "Rural" ~ "Rural",
    TRUE ~ NA
  ))

# On recrée une table agrégée sur zone2
pop_densite_group_ter <- pop_densite_group |> 
  group_by(zone2, SEXE, tranche_bilendi2) |> 
  summarise(pop_strat = sum(pop_strat))

# On reconstruit la table agrégée par type de commune (zone2 au lieu de zone3)
tab_pop_croisee_typeco_bis <- pop_densite_group_ter |> 
  group_by(zone2) |> 
  summarise(Freq = sum(pop_strat, na.rm = TRUE))


### On refait le redressement

# On crée un poids initial uniforme
df$poids_initial_ter <- 1

# On crée un objet de type "survey"
df_design_ter <- svydesign(
  ids = ~1,
  weights = ~poids_initial_ter,
  data = df
)

# On utilise la fonction rake() de survey pour créer les poids
design_rake_ter <- rake(
  df_design_ter,
  sample.margins = list(
    ~tranche_bilendi2,
    ~SEXE,
    ~zone2
  ),
  population.margins = list(
    tab_pop_croisee_age_bis,
    tab_pop_croisee_sexe,
    tab_pop_croisee_typeco_bis
  )
)

# On regarde les poids extrêmes
idx_ter <- order(weights(design_rake_ter), decreasing = TRUE)[1:10]
df_design_ter$variables[idx_ter, c(
  "tranche_bilendi2",
  "SEXE",
  "zone2"
)] 

# On regarde la distribution des poids
w_ter <- weights(design_rake_ter)
summary(w_ter)

# On regarde les quantiles
quantile(
  w_ter,
  probs = c(0, .01, .05, .10, .25, .50, .75, .90, .95, .99, 1) # (de 400 à 20 000)
)
# (dans w_bis, on était entre 300 et 44 000, on est plutôt mieux maintenant !)

# On calcule l'Effective Sample Size (ESS)
ESS_ter <- sum(w_ter)^2 / sum(w_ter^2)
c(
  n = length(w_ter), # 3185 individus dans la base
  ESS_ter = ESS_ter, # ESS = 2244
  # (en termes de précision statistique, c'est "comme si" on avait 2244 individus, à cause de la variance des poids)
  taux_ESS_ter = ESS_ter / length(w_ter) # le taux est de 0,70 (beaucoup mieux que le 0,55 initial !)
)

### On fait des diagnostics

# On regarde les poids extrêmes
df_w_ter <- df_design_ter$variables |>
  mutate(poids = weights(design_rake_ter))
df_w_ter |>
  group_by(tranche_bilendi2, SEXE, zone2) |>
  summarise(
    n = n(),
    poids_moyen = mean(poids),
    poids_min = min(poids),
    poids_max = max(poids),
    poids_total = sum(poids),
    .groups = "drop"
  ) |>
  arrange(desc(poids_moyen))

# Interprétation : le périurbain pose le plus de problèmes

### On regarde le nombre d'individus par case

# On compte dans le df
sample_cells_ter <- df |>
  count(tranche_bilendi2, SEXE, zone2, name = "n_sample")

# On compte dans la pop insee
pop_cells_ter <- pop_densite_group_ter |>
  group_by(tranche_bilendi2, SEXE, zone2) |>
  summarise(
    n_pop = sum(pop_strat),
    .groups = "drop"
  )

# On compare en calculant le ratio pop / échantillon pour chaque catégorie
diagnostic_ter <- sample_cells_ter |>
  full_join(
    pop_cells_ter,
    by = c("tranche_bilendi2", "SEXE", "zone2")
  ) |>
  mutate(
    ratio_pop_sample = n_pop / n_sample
  ) |>
  arrange(desc(ratio_pop_sample))

# On trouve un problème global chez les moins de 65 ans, et dans le périurbain
diagnostic_ter


# Dernier essai : zone2 mais âges fins (essai 4)
#-----------------------------------------------

# On recrée une table agrégée sur zone2
pop_densite_group_ter <- pop_densite_group |> 
  group_by(zone2, SEXE, tranche_bilendi2) |> 
  summarise(pop_strat = sum(pop_strat))

# On reconstruit la table agrégée par type de commune (zone2 au lieu de zone3)
tab_pop_croisee_typeco_bis <- pop_densite_group_ter |> 
  group_by(zone2) |> 
  summarise(Freq = sum(pop_strat, na.rm = TRUE))


### On refait le redressement

# On crée un poids initial uniforme
df$poids_initial_quat <- 1

# On crée un objet de type "survey"
df_design_quat <- svydesign(
  ids = ~1,
  weights = ~poids_initial_quat,
  data = df
)

# On utilise la fonction rake() de survey pour créer les poids
design_rake_quat <- rake(
  df_design_quat,
  sample.margins = list(
    ~tranche_bilendi,
    ~SEXE,
    ~zone2
  ),
  population.margins = list(
    tab_pop_croisee_age,
    tab_pop_croisee_sexe,
    tab_pop_croisee_typeco_bis
  )
)

# On regarde les poids extrêmes
idx_quat <- order(weights(design_rake_quat), decreasing = TRUE)[1:10]
df_design_quat$variables[idx_quat, c(
  "tranche_bilendi",
  "SEXE",
  "zone2"
)] 

# On regarde la distribution des poids
w_quat <- weights(design_rake_quat)
summary(w_quat)

# On regarde les quantiles
quantile(
  w_quat,
  probs = c(0, .01, .05, .10, .25, .50, .75, .90, .95, .99, 1) # (de 400 à 26 000)
)
# (dans w_ter, on était entre 400 et 20 000, on est plutôt moins bien ici !)

# On calcule l'Effective Sample Size (ESS)
ESS_quat <- sum(w_quat)^2 / sum(w_quat^2)
c(
  n = length(w_quat), # 3185 individus dans la base
  ESS_quat = ESS_quat, # ESS = 2132
  # (en termes de précision statistique, c'est "comme si" on avait 2132 individus, à cause de la variance des poids)
  taux_ESS_quat = ESS_quat / length(w_quat) # le taux est de 0,67 (mieux que le 0,55 initial, moins bien que le 0,7 final)
)

### On fait des diagnostics

# On regarde les poids extrêmes
df_w_ter <- df_design_ter$variables |>
  mutate(poids = weights(design_rake_ter))
df_w_ter |>
  group_by(tranche_bilendi2, SEXE, zone2) |>
  summarise(
    n = n(),
    poids_moyen = mean(poids),
    poids_min = min(poids),
    poids_max = max(poids),
    poids_total = sum(poids),
    .groups = "drop"
  ) |>
  arrange(desc(poids_moyen))

# Interprétation : le périurbain pose le plus de problèmes

### On regarde le nombre d'individus par case

# On compte dans le df
sample_cells_ter <- df |>
  count(tranche_bilendi2, SEXE, zone2, name = "n_sample")

# On compte dans la pop insee
pop_cells_ter <- pop_densite_group_ter |>
  group_by(tranche_bilendi2, SEXE, zone2) |>
  summarise(
    n_pop = sum(pop_strat),
    .groups = "drop"
  )

# On compare en calculant le ratio pop / échantillon pour chaque catégorie
diagnostic_ter <- sample_cells_ter |>
  full_join(
    pop_cells_ter,
    by = c("tranche_bilendi2", "SEXE", "zone2")
  ) |>
  mutate(
    ratio_pop_sample = n_pop / n_sample
  ) |>
  arrange(desc(ratio_pop_sample))

# On trouve un problème global chez les moins de 65 ans, et dans le périurbain
diagnostic_ter


####################
#### Conclusion ####
####################

# ESS = 0,54 avec les âges semi-détaillés (environ 5 ans) et zone3 (urbain, rural, périurbain)
# ESS = 0,56 avec les âges larges (environ 15 ans) et zone3

# ESS = 0,67 avec les semi-détaillés et zone2 (urbain, rural)
# ESS = 0,7 avec les âges larges et zone2 





