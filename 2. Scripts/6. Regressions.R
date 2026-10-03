#################################
### Installation des packages ###
#################################

library(tidyverse)


#################################
#### Importation des données ####
#################################

source("2. Scripts/5. AFM et CAH.R")


# Analyses sur les questionnaires longs, avec les réponses observées.
df <- df |> filter(version == "long")


###########################
### Création des scores ###
###########################


# Intérêt pour l'énergie et connaissance des acteurs du système électrique
#-------------------------------------------------------------------------

### Score d'intérêt

df$interet_energie <- echelle_1_5(df$G3Q00001, "G3Q00001")


### Score de connaissance

roles <- list(
  RTE = "Transporte l’électricité à grande échelle (réseau haute tension)",
  Enedis = "Distribue l’électricité localement jusqu’aux logements/entreprises",
  EDF = "Produit et fournit l’électricité",
  Engie = "Produit et fournit l’électricité"
)
items_roles <- paste0("G3Q00002_SQ00", 1:4)  # RTE, Enedis, EDF, Engie
bons_roles <- lapply(seq_along(roles), function(j) {
  reponse <- as.character(df[[items_roles[j]]])
  # NSP explicite = 0 bonne réponse ; cellule réellement absente = NA.
  ifelse(manquant_technique(reponse), NA_integer_,
         as.integer(trimws(reponse) %in% roles[[j]]))
})
df$score_connaissance <- ifelse(
  Reduce(`|`, lapply(bons_roles, is.na)), NA_real_,
  rowSums(do.call(cbind, bons_roles), na.rm = TRUE)
)


### Score de conception sur le service public

df$service_public <- recode_choix(
  df$G3Q00004,
  c("^Un organisme public", "^Une mission d'intérêt général", "^Les deux"),
  c("Service public : organisme", "Service public : mission",
    "Service public : les deux"), "G3Q00004")
df$solidarite_territoriale <- recode_choix(
  df$G3Q00006,
  c("^Adapter le coût", "^Maintenir la même qualité"),
  c("Territoires : adapter prix/service", "Territoires : égalité/solidarité"),
  "G3Q00006")
df$centralisation <- echelle_1_5(df$G3Q00008, "G3Q00008")
df$concurrence <- echelle_1_5(df$G3Q00009, "G3Q00009")


############################
### Essais de régression ###
############################

# Score de connaissance sur les contrôles
summary(lm(log(score_connaissance+0.01) ~ AGE_large*SEXE, df))

# Score d'intérêt sur les contrôles
summary(lm(log(interet_energie+0.01) ~ AGE_large*SEXE, df))

# Score de connaissance sur l'intérêt
summary(lm(log(score_connaissance+0.01) ~ log(interet_energie+0.01), df))

# Score de connaissance sur la conception du service public
summary(lm(log(score_connaissance+0.01) ~ service_public, df))

# Score d'intérêt sur la conception du service public
summary(lm(log(interet_energie+0.01) ~ service_public, df))


# Scores sur les variables démographiques
#-----------------------------------------

# Diplôme et revenu individuel restent des catégories ; NSP devient manquant.
for (v in c("G1Q00003", "G1Q00005")) {
  x <- trimws(as.character(df[[v]]))
  x[nsp(x)] <- NA_character_
  df[[v]] <- factor(x)
}

# Connaissance : nombre de bonnes réponses, de 0 à 4.
summary(lm(score_connaissance ~ AGE_large*SEXE +
             G1Q00003 + G1Q00005 + zone2 + factor(vague), df))

# Intérêt : de 1 à 5.
summary(lm(interet_energie ~ AGE_large*SEXE +
             G1Q00003 + G1Q00005 + zone2 + factor(vague), df))

# Organisation : 1 = national ; 5 = local (malgré le nom centralisation).
summary(lm(centralisation ~ AGE_large*SEXE, df))
summary(lm(centralisation ~ AGE_large*SEXE +
             G1Q00003 + G1Q00005 + zone2 + factor(vague), df))

# Concurrence : 1 = monopole public ; 5 = concurrence.
summary(lm(concurrence ~ AGE_large*SEXE, df))
summary(lm(concurrence ~ AGE_large*SEXE +
             G1Q00003 + G1Q00005 + zone2 + factor(vague), df))

# Solidarité : 1 = égalité territoriale ; 0 = adaptation des prix/services.
df$solidarite_01 <- ifelse(is.na(df$solidarite_territoriale), NA_real_,
  as.numeric(df$solidarite_territoriale == "Territoires : égalité/solidarité"))

# Modèle linéaire de probabilité : +0,05 correspond à +5 points de pourcentage.
# (les probabilités prédites peuvent sortir de [0,1])
summary(lm(solidarite_01 ~ AGE_large*SEXE +
             G1Q00003 + G1Q00005 + zone2 + factor(vague), df))

# service_public (organisme / mission / les deux) est nominal :
# on le conserve séparément, sans le transformer en score 1/2/3.


# Indice politique synthétique (complément)
#-------------------------------------------

# On essaie de combiner plusieurs variables pour en faire un indice synthétique en matière politique.
# On oppose, d'un côté, solidarité et préférence pour le monopole (score=100) et, de l'autre, adaptation et concurrence (score=0)

df$monopole_01 <- (5 - df$concurrence) / 4
df$indice_solidarite_monopole <- 100 * (df$solidarite_01 + df$monopole_01) / 2

summary(lm(indice_solidarite_monopole ~ AGE_large*SEXE +
             G1Q00003 + G1Q00005 + zone2 + factor(vague), df))

# Commentaire sur cet indice : il augmente chez les personnes âgées (pas vraiment les plus de gauche ?)
# Il diminue par contre chez les urbains : intéressant de voir que les urbains sont moins solidaires et plus pour la concurrence
