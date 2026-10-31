# Version finale du redressement - avec seulement age*sexe*statut_co (après recodage par Antoine)

rm(list=ls())

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
# Conserver l'origine avant tout filtrage : même sans identifiant Bilendi,
# un questionnaire long/court reste identifiable sans ambiguïté.
df <- dplyr::bind_rows(list(long = df_long, court = df_court),
                       .id = "version_questionnaire")

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
# MODIFICATION : conserver toutes les réponses déclarées, dont "Autre".
df <- df |> mutate(SEXE = factor(G1Q00001))

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

# Exclusion sans imputation si l'âge ou la zone est inconnu.
# "Autre" et les non-réponses sur le sexe sont conservés.
n_avant_redressement <- nrow(df)

# Sur le sexe
table(df$SEXE, useNA = "ifany")

# Sur le type de commune
table(df$zone2)
n_exclus_zone <- sum(is.na(df$zone2))
df <- df |> filter(!is.na(zone2))

# Sur la catégorie d'âge 
table(df$AGE_large)
sum(is.na(df$AGE_large))
n_exclus_age <- sum(is.na(df$AGE_large)) # après l'exclusion pour zone inconnue
df <- df |> filter(!is.na(AGE_large))

# On nettoie les niveaux non utilisés
df <- droplevels(df)


# Redressement en pratique
#-------------------------

# On crée un poids initial uniforme
# Même poids pour tous, à l'échelle de la population.
# Cela stabilise le calage exponentiel, sans changer les poids finaux attendus.
df$poids_initial <- sum(pop_densite_group$pop_strat) / nrow(df)


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

# On cale tout le monde sur âge x zone, sans imputer le sexe.
# Parmi les réponses Homme/Femme, on cale leur proportion conditionnelle
# sur celle de l'INSEE, dans chaque cellule âge x zone.

pop_calage <- pop_densite_group |> filter(pop_strat > 0)
stopifnot(
  nrow(df) > 0,
  !anyNA(pop_calage[c("AGE_large", "SEXE", "zone2", "pop_strat")]),
  all(is.finite(pop_calage$pop_strat)),
  all(pop_calage$SEXE %in% c("Homme", "Femme"))
)

pop_joint <- pop_calage |>
  group_by(AGE_large, zone2) |>
  summarise(
    Freq = sum(pop_strat),
    Freq_homme = sum(pop_strat[SEXE == "Homme"]),
    .groups = "drop"
  ) |>
  mutate(
    strate = paste(AGE_large, zone2, sep = " | "),
    p_homme = Freq_homme / Freq
  )
stopifnot(all(pop_joint$p_homme > 0 & pop_joint$p_homme < 1))

df <- df |>
  mutate(
    strate = factor(paste(AGE_large, zone2, sep = " | "),
                    levels = pop_joint$strate),
    SEXE_redress = factor(
      case_when(
        G1Q00001 == "Homme" ~ "Homme",
        G1Q00001 == "Femme" ~ "Femme",
        TRUE ~ NA_character_
      ),
      levels = c("Homme", "Femme")
    )
  )
stopifnot(!anyNA(df$strate))
if (any(table(df$strate) == 0)) {
  stop("Cellule âge x zone sans répondant : regrouper les catégories concernées ",
       "dans df ET dans les données INSEE avant de relancer.")
}

tab_sexe <- table(df$strate, df$SEXE_redress)
pop_joint$calage_sexe_possible <- tab_sexe[, "Homme"] > 0 &
  tab_sexe[, "Femme"] > 0
if (any(!pop_joint$calage_sexe_possible)) {
  warning("Sexe non calé dans ces cellules, faute de H et F observés : ",
          paste(pop_joint$strate[!pop_joint$calage_sexe_possible], collapse = ", "),
          ". L'âge x zone y reste calé pour tout le monde.")
}

# Pour Autre/NSP, toutes les variables de calage du sexe valent zéro :
# leur poids ne dépend donc que de leur cellule âge x zone.
# Pour H/F, la somme pondérée de (1[Homme] - proportion cible) doit valoir 0.
noms_calage_sexe <- character()
for (j in which(pop_joint$calage_sexe_possible)) {
  nom <- paste0("sexe_centre_", j)
  df[[nom]] <- ifelse(
    df$strate == pop_joint$strate[j] & !is.na(df$SEXE_redress),
    as.numeric(df$SEXE_redress == "Homme") - pop_joint$p_homme[j],
    0
  )
  noms_calage_sexe <- c(noms_calage_sexe, nom)
}

formule_calage <- reformulate(c("strate", noms_calage_sexe), intercept = FALSE)
X_calage <- model.matrix(formule_calage, data = df)
X_age_zone <- model.matrix(~ strate - 1, data = df)
stopifnot(identical(colnames(X_calage)[seq_len(ncol(X_age_zone))],
                    colnames(X_age_zone)))
population_calage <- setNames(
  c(pop_joint$Freq, rep(0, length(noms_calage_sexe))),
  colnames(X_calage)
)

# On crée un objet de type "survey"
df_design <- svydesign(
  ids = ~1,
  weights = ~poids_initial,
  data = df
)

# Calage exponentiel positif avec les contraintes ci-dessus.
# On garde le nom design_post pour faciliter la suite des analyses.
design_post <- calibrate(
  df_design,
  formula = formule_calage,
  population = population_calage,
  calfun = "raking",
  maxit = 100,
  epsilon = 1e-7
)

# Vérifier le respect de TOUTES les contraintes, sans forcer une non-convergence.
ecart_calage <- as.numeric(crossprod(X_calage, weights(design_post))) -
  population_calage
stopifnot(
  all(is.finite(weights(design_post)) & weights(design_post) > 0),
  all(abs(ecart_calage) <= 1e-6 * pmax(1, abs(population_calage)))
)
print(data.frame(cible = names(population_calage),
                 total_cible = as.numeric(population_calage),
                 ecart = as.numeric(ecart_calage)))


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
summary(df$poids) # entre 10 264 et 24 530
sum(df$poids) # on a un total de 53 483 601 d'individus (soit la population française des 18+)

# On crée un poids normalisé
df <- df |> 
  mutate(poids_norm = poids * nrow(df) / sum(poids))

# On vérifie la structure de la variable
summary(df$poids_norm) # chaque individu représente entre 0.61 et 1.46 individu à présent
sum(df$poids_norm) # on a bien un total de 3 192 individus maintenant

design_post$variables <- df
attr(df, "plan_redresse") <- design_post
attr(df, "diagnostic_redressement") <- list(
  n_avant = n_avant_redressement,
  exclus_zone = n_exclus_zone,
  exclus_age_apres_zone = n_exclus_age,
  n_apres = nrow(df),
  cellules = pop_joint,
  contraintes = data.frame(cible = names(population_calage),
                           total_cible = as.numeric(population_calage),
                           ecart = as.numeric(ecart_calage))
)
rm(list = setdiff(ls(all.names = TRUE), "df"))
