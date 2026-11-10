# Recodage de toutes les variables utilisées dans les analyses
#
# Les analyses factorielles (scripts 5) et les régressions (script 6) partent
# toutes de ce fichier : chaque indicateur n'est construit qu'une fois, de la même
# façon pour toutes les analyses.
#
# On repère les modalités par un fragment de début de libellé plutôt que par le
# libellé complet : les libellés contiennent des apostrophes typographiques et des
# points médians, et une comparaison exacte échouerait sans message d'erreur.
# Chaque recodage est suivi d'un tableau croisé avec la variable d'origine.
#
# Conventions :
#   - "Ne sait pas / Ne se prononce pas" devient NA dans les échelles et les
#     indicatrices, mais reste une modalité ("NSP") dans les variables
#     qualitatives, pour que les analyses factorielles puissent la traiter ;
#   - les questions non posées aux questionnaires courts valent NA pour eux ;
#   - les indicatrices valent 1 / 0.
#
# Là où les choix divergeaient, on suit les conventions du script 7 (Antoine) :
# classes de revenu et leurs libellés, statut "Autre", difficultés à payer
# rangées dans "Jamais" pour les courts, part des gestes faits parmi ceux qui ne
# sont pas "non concernés", score pro-nucléaire comme mesure principale du
# soutien au nucléaire. Correspondance des noms avec le script 7 :
#   solidarite = justice_terr, mesure_malus = acc_malus, mesure_bonus = acc_bonus,
#   payer_vert = vert, sceptique = scepticisme, nucleaire_premier = nuc_premier,
#   prod_nucleaire = nuc_production, lev_nucleaire = nuc_levier,
#   gestes_faits = gestes_n, age_tranche = age.
#
# Le résultat est enregistré dans 3. Outputs/df_recode.rds.


###############################
### Chargement des packages ###
###############################

library(tidyverse)


###############################
### Importation des données ###
###############################

# On part du redressement, qui ne laisse que df (3259 répondants pondérés)
source("2. Scripts/1. Redressement.R")

df <- df |> mutate(long = version_questionnaire == "long")
table(df$long)  # 2821 longs, 438 courts

# Deux outils, utilisés tout au long du script

# Une échelle de 1 à 5 ("1 - pas intéressé·e", "2", ..., "5 - très intéressé·e") :
# on garde le chiffre, et les NSP deviennent NA.
echelle_1_5 <- function(x) {
  chiffre <- substr(x, 1, 1)
  chiffre[!chiffre %in% c("1", "2", "3", "4", "5")] <- NA
  as.numeric(chiffre)
}

# Une case à cocher : "Oui" = 1, "Non" = 0. Quand le répondant coche "Ne sait
# pas" (ou "Aucune"), LimeSurvey met les autres cases à "N/A" : elles restent NA.
case_cochee <- function(x) case_when(x == "Oui" ~ 1, x == "Non" ~ 0)


#########################
### 1. Profil social  ###
#########################

### Sexe

# Les 8 réponses "Autre" sont trop peu nombreuses pour être
# analysées comme une catégorie : elles valent NA dans cette variable (mais
# restent dans df, avec leur poids).
df <- df |>
  mutate(sexe = factor(case_when(G1Q00001 == "Homme" ~ "Homme",
                                 G1Q00001 == "Femme" ~ "Femme"),
                       levels = c("Homme", "Femme")))
table(df$sexe, df$G1Q00001, useNA = "ifany")

### Âge

# AGE_large (18-34, 35-49, 50-64, 65+) vient du redressement. On met les 35-49
# ans en référence pour les régressions.
df <- df |> mutate(age_tranche = relevel(AGE_large, ref = "35-49"))
table(df$age_tranche)

### Diplôme

# "Autre" regroupe surtout des diplômes étrangers ou non classables : on le
# garde avec les NSP plutôt que de le ranger arbitrairement.
df <- df |>
  mutate(diplome = factor(case_when(
    G1Q00003 %in% c("Aucun", "CAP-BEP")  ~ "Infra-bac",
    G1Q00003 == "Bac"                    ~ "Bac",
    G1Q00003 %in% c("Bac+2", "Bac+3")    ~ "Bac+2/3",
    G1Q00003 == "Bac+5 et au-delà"       ~ "Bac+5",
    TRUE                                 ~ "Autre/NSP"
  ), levels = c("Bac", "Infra-bac", "Bac+2/3", "Bac+5", "Autre/NSP")))
table(df$G1Q00003, df$diplome)

### Activité

df <- df |>
  mutate(activite = factor(case_when(
    G1Q00004 == "Salarié·e du secteur privé"  ~ "Salarié du privé",
    G1Q00004 == "Salarié·e du secteur public" ~ "Salarié du public",
    G1Q00004 == "Indépendant·e"               ~ "Indépendant",
    G1Q00004 == "Retraité·e"                  ~ "Retraité",
    G1Q00004 == "Étudiant·e"                  ~ "Étudiant",
    G1Q00004 == "Sans emploi"                 ~ "Sans emploi",
    TRUE                                      ~ "Autre/NSP"
  ), levels = c("Salarié du privé", "Salarié du public", "Indépendant",
                "Retraité", "Étudiant", "Sans emploi", "Autre/NSP")))
table(df$G1Q00004, df$activite)

### Revenu individuel

# Les 15 tranches du questionnaire sont trop fines pour les régressions : la
# plupart comptent moins de 150 répondants, et leurs coefficients deviennent
# erratiques. On les regroupe en cinq classes, plus les NSP (313 répondants, qu'on
# ne veut pas perdre). On repère chaque tranche par sa borne basse, suivie d'une
# espace : sans elle, "De 400" attraperait aussi "De 4000 à moins de 6000€".
df <- df |>
  mutate(revenu = factor(case_when(
    grepl("^Moins de 400|^De 400 |^De 600 |^De 800 ", G1Q00005) ~ "< 1 000",
    grepl("^De 1000 |^De 1200 ", G1Q00005)                    ~ "1 000-1 500",
    grepl("^De 1500 |^De 1800 |^De 2000 ", G1Q00005)          ~ "1 500-2 500",
    grepl("^De 2500 |^De 3000 ", G1Q00005)                    ~ "2 500-4 000",
    grepl("^De 4000 |^De 6000 |^10", G1Q00005)               ~ "4 000 et +",
    TRUE                                                     ~ "NSP"
  ), levels = c("1 500-2 500", "< 1 000", "1 000-1 500",
                "2 500-4 000", "4 000 et +", "NSP")))
table(df$G1Q00005, df$revenu)


##############################
### 2. Conditions de vie   ###
##############################

### Territoire

# zone2 (rural / urbain), zone3 (rural / périurbain / urbain) et taille_commune
# viennent du script 4 et du redressement. On met l'urbain en référence.
df <- df |>
  mutate(zone2 = relevel(zone2, ref = "Urbain"),
         zone3 = relevel(zone3, ref = "Urbain"))
table(df$zone3, df$zone2, useNA = "ifany")

### Type de logement

df <- df |>
  mutate(logement = factor(case_when(
    G2Q00003 == "Dans une maison"     ~ "Maison",
    G2Q00003 == "Dans un appartement" ~ "Appartement",
    TRUE                              ~ "Autre/NSP"
  ), levels = c("Maison", "Appartement", "Autre/NSP")))
table(df$G2Q00003, df$logement)

### Statut d'occupation

# Question posée aux longs seulement. Parmi les "Autre", six réponses libres
# désignent en fait des propriétaires (usufruitiers, indivision, "à crédit").
# Les courts n'ont pas de contrat à leur nom : ils forment la catégorie
# "Sans contrat", qui remplace l'indicatrice long / court dans les régressions.
df <- df |>
  mutate(
    precision_statut = tolower(G2Q00004_other),
    statut = case_when(
      !long                                                            ~ "Sans contrat",
      G2Q00004 == "Propriétaire"                                       ~ "Propriétaire",
      G2Q00004 == "Autre" & grepl("usufrui|indivision|crédit", precision_statut) ~ "Propriétaire",
      G2Q00004 == "Locataire"                                          ~ "Locataire",
      TRUE                                                             ~ "Autre"
    ),
    statut = factor(statut, levels = c("Propriétaire", "Locataire", "Autre", "Sans contrat"))
  ) |>
  select(-precision_statut)
table(df$G2Q00004, df$statut, useNA = "ifany")

### Énergie de chauffage

# Utile pour les gestes (heures creuses, baisse du chauffage) et les factures
df <- df |>
  mutate(chauffage = factor(case_when(
    G2Q00008_SQ002 == "Electricité"                 ~ "Électricité",
    G2Q00008_SQ002 == "Gaz"                         ~ "Gaz",
    G2Q00008_SQ002 == "Fioul"                       ~ "Fioul",
    G2Q00008_SQ002 == "Bois et autres combustibles" ~ "Bois",
    TRUE                                            ~ "NSP"
  ), levels = c("Électricité", "Gaz", "Fioul", "Bois", "NSP")))
table(df$G2Q00008_SQ002, df$chauffage)

### Difficultés à payer les factures d'énergie (longs)

# Les courts n'ont pas eu la question. Comme dans le script 7, on les range dans
# la référence ("Jamais") : dans les régressions, la modalité "Sans contrat" du
# statut absorbe leur écart, et les effets des difficultés ne sont estimés que
# sur les longs. Sans le statut, on utilise difficultes_np, où ils forment une
# catégorie "Non posée".
df <- df |>
  mutate(
    difficultes_np = factor(case_when(
      !long                                  ~ "Non posée",
      G2Q00009 == "Jamais"                   ~ "Jamais",
      G2Q00009 %in% c("Rarement", "Parfois") ~ "Rarement/parfois",
      G2Q00009 %in% c("Souvent", "Toujours") ~ "Souvent/toujours",
      TRUE                                   ~ "NSP"
    ), levels = c("Jamais", "Rarement/parfois", "Souvent/toujours", "NSP", "Non posée")),
    difficultes = factor(if_else(long, as.character(difficultes_np), "Jamais"),
                         levels = c("Jamais", "Rarement/parfois", "Souvent/toujours", "NSP"))
  )
table(df$G2Q00009, df$difficultes_np, useNA = "ifany")
table(df$difficultes_np, df$difficultes)

### Fournisseur d'électricité (longs)

# Tous les noms ont été saisis en texte libre (modalité "Autre"). On les regroupe
# en cherchant le nom du fournisseur, fautes de frappe comprises. Les 48
# répondants qui citent Enedis, gestionnaire du réseau et non fournisseur, sont
# gardés à part : leur réponse dit quelque chose de leur connaissance du système.
# Les entreprises locales (régies, SICAE, ES Strasbourg, UEM Metz...) forment une
# catégorie propre, puisqu'elles sont souvent l'ancien monopole local.
df <- df |>
  mutate(
    nom_fournisseur = tolower(trimws(G3Q00003_other)),
    fournisseur = case_when(
      !long                                                  ~ NA_character_,
      G3Q00003 == "Ne sait pas / Ne se prononce pas"         ~ "NSP",
      grepl("enedis|geredis", nom_fournisseur)               ~ "Enedis (confusion)",
      grepl("^e ?\\.? ?d ?\\.? ?f", nom_fournisseur)         ~ "EDF",
      grepl("engie|^engi|^angie|^egie|^enji|^enegie|happ", nom_fournisseur) ~ "Engie",
      grepl("total|direct", nom_fournisseur)                 ~ "TotalEnergies",
      grepl(paste0("sicae|sicap|^es$|^és$|^e\\.s$|^es |^eds$|strasbourg|uem|metz|",
                   "geg|gaz de bordeaux|seolis|sor[eé]g|cesml|ceslm|vialis|r[eé]gie|",
                   "gazelec|synelva|proxelia|communnal|^locale$|seyssuel|coopérative"),
            nom_fournisseur)                                 ~ "Entreprise locale",
      !is.na(nom_fournisseur)                                ~ "Autre fournisseur",
      TRUE                                                   ~ "NSP"
    ),
    fournisseur = factor(fournisseur, levels = c("EDF", "Engie", "TotalEnergies",
                                                 "Autre fournisseur", "Entreprise locale",
                                                 "Enedis (confusion)", "NSP"))
  ) |>
  select(-nom_fournisseur)
table(df$fournisseur, useNA = "ifany")
# EDF 1576, Engie 405, TotalEnergies 316, autres 254, entreprises locales 135,
# Enedis 49, NSP 86


##################################
### 3. Rapport à l'énergie     ###
##################################

### Intérêt pour l'énergie et pour la transition (1 à 5)

df <- df |>
  mutate(interet_energie    = echelle_1_5(G3Q00001),
         interet_transition = echelle_1_5(G4Q00001))
table(df$G3Q00001, df$interet_energie, useNA = "ifany")

### Connaissance du rôle des acteurs

# La seule question à bonne réponse : RTE transporte, Enedis distribue, EDF et
# Engie produisent et fournissent. "Ne sait pas" compte comme une mauvaise
# réponse (0). La question a été posée à tous et personne ne l'a sautée.
df <- df |>
  mutate(
    juste_RTE    = as.numeric(grepl("^Transporte", G3Q00002_SQ001)),
    juste_Enedis = as.numeric(grepl("^Distribue",  G3Q00002_SQ002)),
    juste_EDF    = as.numeric(grepl("^Produit",    G3Q00002_SQ003)),
    juste_Engie  = as.numeric(grepl("^Produit",    G3Q00002_SQ004)),
    # Deux savoirs distincts : les bonnes réponses sur le réseau (RTE, Enedis) et
    # sur les fournisseurs (EDF, Engie) ne sont pas corrélées entre elles (script 7).
    conn_reseau       = juste_RTE + juste_Enedis,
    conn_fournisseurs = juste_EDF + juste_Engie,
    # Le nombre de "Ne sait pas" sur les deux acteurs du réseau, pour séparer le
    # fait de ne pas répondre du fait de se tromper.
    conn_reseau_nsp = grepl("^Ne sait", G3Q00002_SQ001) + grepl("^Ne sait", G3Q00002_SQ002)
  )
table(df$G3Q00002_SQ001, df$juste_RTE)
table(df$conn_reseau, df$conn_fournisseurs)


##########################################
### 4. Conceptions du service public   ###
##########################################

# Toutes ces questions ont été posées à tout le monde, longs et courts.

### Définition du service public (G3Q00004)

df <- df |>
  mutate(sp_definition = factor(case_when(
    grepl("^Un organisme",  G3Q00004) ~ "Organisme public",
    grepl("^Une mission",   G3Q00004) ~ "Mission d'intérêt général",
    grepl("^Les deux",      G3Q00004) ~ "Les deux",
    TRUE                              ~ "NSP"
  ), levels = c("Les deux", "Organisme public", "Mission d'intérêt général", "NSP")))
table(df$G3Q00004, df$sp_definition)

### Ce qui caractérise un service public (G3Q00005, trois choix au plus)

# Attention, l'ordre des items ne suit pas leur numéro. Comme on ne pouvait
# cocher que trois cases, "Oui" signifie "parmi les trois premiers critères".
df <- df |>
  mutate(sp_acces       = case_cochee(G3Q00005_SQ001),
         sp_egalite     = case_cochee(G3Q00005_SQ004),
         sp_solidarite  = case_cochee(G3Q00005_SQ003),
         sp_sans_profit = case_cochee(G3Q00005_SQ002),
         sp_controle    = case_cochee(G3Q00005_SQ005))
table(df$G3Q00005_SQ002, df$sp_sans_profit, useNA = "ifany")  # 137 NSP

### Solidarité territoriale (G3Q00006)

df <- df |>
  mutate(territoires = factor(case_when(
    grepl("^Maintenir", G3Q00006) ~ "Même service partout",
    grepl("^Adapter",   G3Q00006) ~ "Adapter au territoire",
    TRUE                          ~ "NSP"
  ), levels = c("Même service partout", "Adapter au territoire", "NSP")),
  # La même information en indicatrice, pour les régressions
  solidarite = case_when(territoires == "Même service partout"  ~ 1,
                         territoires == "Adapter au territoire" ~ 0))
table(df$G3Q00006, df$territoires)

### Qui doit garantir l'accès à l'énergie (G3Q00007, trois choix au plus)

df <- df |>
  mutate(garant_etat           = case_cochee(G3Q00007_SQ001),
         garant_ent_publiques  = case_cochee(G3Q00007_SQ002),
         garant_ent_privees    = case_cochee(G3Q00007_SQ003),
         garant_collectivites  = case_cochee(G3Q00007_SQ004),
         garant_individus      = case_cochee(G3Q00007_SQ005))
table(df$G3Q00007_SQ001, df$garant_etat, useNA = "ifany")  # 174 NSP

### Organisation du système électrique (1 à 5)

# local : 1 = très centralisée, 5 = très décentralisée
# concurrence : 1 = monopole public, 5 = concurrence
df <- df |>
  mutate(local       = echelle_1_5(G3Q00008),
         concurrence = echelle_1_5(G3Q00009))
table(df$G3Q00009, df$concurrence, useNA = "ifany")


######################################
### 5. Choix pour la transition    ###
######################################

# Deux classements posés à tout le monde : les leviers de la transition
# (G4Q00002, cinq rangs parmi six leviers) et les productions prioritaires
# (G4Q00003, trois rangs parmi six productions). On donne à chaque option des
# points selon son rang : 5 points au premier rang des leviers, 4 au deuxième...,
# et 0 si l'option n'est pas citée (3 points au premier rang des productions).
# On garde ainsi tout le classement, et pas seulement le premier choix. Si le
# premier rang est "Ne sait pas" ou vide, tous les points valent NA.
#
# On n'exige pas un classement complet : plus de 400 personnes n'ont donné
# qu'une production. Les rangs non remplis comptent comme "non cité".

points_classement <- function(prefixe, k, motif) {
  cite <- sapply(1:k, function(j) grepl(motif, df[[paste0(prefixe, j)]]))
  points <- as.numeric(cite %*% (k:1))   # k points au rang 1, ..., 1 point au rang k
  premier <- df[[paste0(prefixe, 1)]]
  points[is.na(premier) | grepl("^Ne sait", premier)] <- NA
  points
}

### Les leviers (0 à 5 points)

df <- df |>
  mutate(lev_sobriete     = points_classement("G4Q00002_", 5, "^Sobriété"),
         lev_efficacite   = points_classement("G4Q00002_", 5, "^Efficacité"),
         lev_renouvelables = points_classement("G4Q00002_", 5, "^Développer les renouvelables"),
         lev_nucleaire    = points_classement("G4Q00002_", 5, "nucléaire"),
         lev_mobilites    = points_classement("G4Q00002_", 5, "^Développer des mobilités"),
         lev_reseau       = points_classement("G4Q00002_", 5, "^Optimisation"))

# On vérifie : chaque répondant a au plus 5 + 4 + 3 + 2 + 1 = 15 points
summary(df$lev_sobriete + df$lev_efficacite + df$lev_renouvelables +
          df$lev_nucleaire + df$lev_mobilites + df$lev_reseau)
table(df$G4Q00002_1, df$lev_nucleaire, useNA = "ifany")

### Les productions (0 à 3 points)

# "Autre" (8 premiers choix) n'est pas retenu.
df <- df |>
  mutate(prod_nucleaire   = points_classement("G4Q00003_", 3, "^Nucléaire"),
         prod_solaire     = points_classement("G4Q00003_", 3, "^Solaire"),
         prod_eolien      = points_classement("G4Q00003_", 3, "^Éolien"),
         prod_hydraulique = points_classement("G4Q00003_", 3, "^Hydro"),
         prod_gaz_vert    = points_classement("G4Q00003_", 3, "^Gaz renouvelables"))
table(df$G4Q00003_1, df$prod_nucleaire, useNA = "ifany")

### Le soutien au nucléaire

# Deux indicateurs :
#   - pro_nucleaire     : la mesure principale (script 7), moyenne des deux
#                         classements ramenés de 0 (absent) à 1 (premier), qui
#                         sont corrélés à 0.68 ;
#   - nucleaire_premier : le nucléaire est la première production citée (1 / 0),
#                         pour la robustesse.
df <- df |>
  mutate(nucleaire_premier = as.numeric(prod_nucleaire == 3),
         pro_nucleaire     = (prod_nucleaire / 3 + lev_nucleaire / 5) / 2)
table(df$nucleaire_premier, useNA = "ifany")

### Les mesures acceptables pour accélérer la transition (G4Q00004)

# "Aucune" (80 répondants) et "Ne sait pas" (232) excluent les autres cases, qui
# valent alors "N/A". Celui qui répond "Aucune" n'accepte aucune mesure : 0
# partout. Celui qui répond "Ne sait pas" reste NA.
aucune_mesure <- df$G4Q00004_SQ006 == "Oui"

df <- df |>
  mutate(mesure_bonus       = case_cochee(G4Q00004_SQ001),
         mesure_malus       = case_cochee(G4Q00004_SQ002),
         mesure_invest_public = case_cochee(G4Q00004_SQ003),
         mesure_regulation  = case_cochee(G4Q00004_SQ004),
         mesure_information = case_cochee(G4Q00004_SQ005)) |>
  mutate(across(starts_with("mesure_"), ~ if_else(aucune_mesure, 0, .x)))
table(df$G4Q00004_SQ002, df$mesure_malus, useNA = "ifany")


#################################
### 6. Autoconsommation (longs) ###
#################################

### Connaissance préalable (G4Q00005)

# Cinq combinaisons seulement apparaissent : individuelle seule (1744), les deux
# (395), collective seule (238), aucune (348) et NSP (96). Aucune n'est
# contradictoire.
df <- df |>
  mutate(autoconso_connue = factor(case_when(
    !long                                                ~ NA_character_,
    G4Q00005_SQ001 == "Oui" & G4Q00005_SQ002 == "Oui"    ~ "Les deux formes",
    G4Q00005_SQ001 == "Oui"                              ~ "Individuelle seule",
    G4Q00005_SQ002 == "Oui"                              ~ "Collective seule",
    G4Q00005_SQ004 == "Oui"                              ~ "Aucune",
    TRUE                                                 ~ "NSP"
  ), levels = c("Individuelle seule", "Les deux formes", "Collective seule", "Aucune", "NSP")))
table(df$autoconso_connue, useNA = "ifany")

### Préférence entre les deux dispositifs (G4Q00007)

df <- df |>
  mutate(autoconso_preference = factor(case_when(
    !long                          ~ NA_character_,
    grepl("individuelle", G4Q00007) ~ "Individuelle",
    grepl("collective",   G4Q00007) ~ "Collective",
    grepl("^Aucun",       G4Q00007) ~ "Aucune",
    TRUE                            ~ "NSP"
  ), levels = c("Individuelle", "Collective", "Aucune", "NSP")))
table(df$G4Q00007, df$autoconso_preference, useNA = "ifany")

### Un porteur local de confiance comme encouragement (G4Q00006_SQ003)

df <- df |> mutate(porteur_local = if_else(long, case_cochee(G4Q00006_SQ003), NA))
table(df$G4Q00006_SQ003, df$porteur_local, useNA = "ifany")


#######################################
### 7. Marché et offre verte (longs) ###
#######################################

# Les trois affirmations G5Q00002 à G5Q00004 ne sont pas recodées : leur énoncé
# manque dans les dictionnaires, et on ne sait donc pas ce qu'elles mesurent.

### Changements de fournisseur au cours de la vie (G5Q00005)

# La modalité "0-3" chevauche "0" : on la lit comme "1 à 3 fois".
df <- df |>
  mutate(changements = factor(case_when(
    !long                            ~ NA_character_,
    G5Q00005 == "0"                  ~ "Jamais",
    G5Q00005 == "0-3"                ~ "1 à 3 fois",
    G5Q00005 %in% c("4-10", "+ de 10") ~ "4 fois ou plus",
    TRUE                             ~ "NSP"
  ), levels = c("Jamais", "1 à 3 fois", "4 fois ou plus", "NSP")))
table(df$G5Q00005, df$changements, useNA = "ifany")

### Offre verte : souscription, disposition à payer et scepticisme

df <- df |>
  mutate(
    offre_verte = case_when(G6Q00001 == "Oui" ~ 1, G6Q00001 == "Non" ~ 0),
    # 1 = certainement pas prêt à payer plus, 5 = certainement prêt
    payer_vert  = echelle_1_5(G6Q00004),
    # "Rien, je reste sceptique" parmi ce qui rendrait une offre verte crédible
    sceptique   = case_cochee(G6Q00005_SQ006)
  )
table(df$G6Q00001, df$offre_verte, useNA = "ifany")
table(df$G6Q00005_SQ006, df$sceptique, useNA = "ifany")

### Par qui souscrire une offre verte (G6Q00006)

df <- df |>
  mutate(offreur_vert = factor(case_when(
    !long                            ~ NA_character_,
    G6Q00006 == "EDF"                ~ "EDF",
    grepl("^Un acteur engagé", G6Q00006)  ~ "Coopérative/engagé",
    grepl("^Un acteur implanté", G6Q00006) ~ "Acteur local",
    G6Q00006 == "Un groupe privé"    ~ "Groupe privé",
    TRUE                             ~ "Autre/NSP"
  ), levels = c("EDF", "Coopérative/engagé", "Acteur local", "Groupe privé", "Autre/NSP")))
table(df$G6Q00006, df$offreur_vert, useNA = "ifany")


#########################
### 8. Gestes (longs) ###
#########################

# Neuf gestes, avec cinq réponses possibles. On garde les cinq : "Non concerné"
# (un locataire ne peut pas isoler son logement) n'est pas un refus, et "Pas prêt"
# n'est pas un oubli.
# Attention à l'ordre des tests : "Jamais fait. Pas prêt·e à faire" contient aussi
# le mot "prêt", il faut donc le tester avant "Prêt·e à faire".
gestes <- c(G7Q00001_SQ001 = "geste_voiture",
            G7Q00001_SQ002 = "geste_chauffage",
            G7Q00001_SQ003 = "geste_heures_creuses",
            G7Q00001_SQ004 = "geste_suivi_conso",
            G7Q00001_SQ005 = "geste_information",
            G7Q00001_SQ006 = "geste_petits_equipements",
            G7Q00001_SQ007 = "geste_gros_equipement",
            G7Q00001_SQ008 = "geste_isolation",
            G7Q00001_SQ009 = "geste_production")

for (question in names(gestes)) {
  reponse <- df[[question]]
  df[[gestes[question]]] <- factor(case_when(
    !df$long                       ~ NA_character_,
    grepl("^Déjà", reponse)        ~ "Fait",
    grepl("Pas prêt", reponse)     ~ "Pas prêt",
    grepl("Prêt", reponse)         ~ "Prêt",
    grepl("^Non concern", reponse) ~ "Non concerné",
    TRUE                           ~ "NSP"
  ), levels = c("Fait", "Prêt", "Pas prêt", "Non concerné", "NSP"))
}
table(df$G7Q00001_SQ008, df$geste_isolation, useNA = "ifany")

# Deux indicateurs synthétiques, définis comme dans le script 7 :
#   - gestes_faits : le nombre de gestes déjà faits, sur 9 (pour la robustesse) ;
#   - gestes_part  : la part des gestes faits parmi ceux qui concernent le
#     répondant, c'est-à-dire tous sauf les "non concerné". Un locataire n'est
#     pas pénalisé pour l'isolation ; un "ne sait pas" compte comme non fait.
fait     <- sapply(gestes, function(g) df[[g]] == "Fait")
concerne <- sapply(gestes, function(g) df[[g]] != "Non concerné")

df <- df |>
  mutate(gestes_faits     = if_else(long, rowSums(fait), NA),
         gestes_concernes = if_else(long, rowSums(concerne), NA),
         gestes_part      = if_else(gestes_concernes > 0, gestes_faits / gestes_concernes, NA))
summary(df$gestes_part[df$long])


#####################
### Sauvegarde    ###
#####################

dir.create("3. Outputs", showWarnings = FALSE)
saveRDS(df, "3. Outputs/df_recode.rds")

rm(list = setdiff(ls(), "df"))
