# Analyse en composantes principales : les choix pour la transition énergétique
#
# Deuxième analyse par bloc. On cherche comment s'organisent les priorités pour
# la transition, à partir des deux classements posés à tout le monde :
#   - les leviers de la transition (G4Q00002, cinq rangs parmi six leviers) ;
#   - les productions prioritaires (G4Q00003, trois rangs parmi six productions).
#
# Le script 3 a converti chaque classement en points : 5 points au levier classé
# premier, 4 au deuxième... et 0 s'il n'est pas cité (de 3 à 0 pour les
# productions). On garde ainsi tout le classement, et pas seulement le premier
# choix comme dans l'ancienne AFM. Les points étant des nombres, on fait une ACP.
#
# Une précaution de lecture : dans un classement, monter une option fait
# forcément descendre les autres. Les options d'un même classement sont donc
# mécaniquement un peu corrélées négativement entre elles. On s'intéresse surtout
# à ce qui relie les deux classements, qui n'ont rien de mécanique.


###############################
### Chargement des packages ###
###############################

library(tidyverse)
library(FactoMineR)
library(factoextra)


###############################
### Importation des données ###
###############################

df <- readRDS("3. Outputs/df_recode.rds")

dossier <- "3. Outputs/Analyses factorielles/2. Transition"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

etiqueter <- function(x, nom) {
  x <- droplevels(fct_na_value_to_level(factor(x), "NSP"))
  fct_relabel(x, ~ paste0(nom, " : ", .x))
}

# On garde les répondants qui ont donné au moins un premier choix dans les deux
# classements (ceux qui ont répondu "Ne sait pas" en premier rang sont exclus).
classe <- !is.na(df$lev_nucleaire) & !is.na(df$prod_nucleaire)
sum(classe)   # 2864 répondants
sum(!classe)  # 395 exclus
tr <- df[classe, ]


####################################
### Les variables actives       ###
####################################

actives <- tr |>
  select(lev_sobriete, lev_efficacite, lev_renouvelables, lev_nucleaire,
         lev_mobilites, lev_reseau,
         prod_nucleaire, prod_solaire, prod_eolien, prod_hydraulique, prod_gaz_vert)

# On regarde les moyennes pondérées des points
round(sapply(actives, weighted.mean, w = tr$poids_norm), 2)
# Leviers : l'efficacité vient en tête (2.80), le nucléaire est dans la moyenne
# (2.19). Productions : solaire (1.56) et nucléaire (1.48) devant l'hydraulique.


####################################
### Les variables illustratives  ###
####################################

# Les variables sociales, et les mesures jugées acceptables pour accélérer la
# transition (question distincte, qu'on laisse en illustratif)
trois_positions <- function(x, bas, haut) {
  case_when(x <= 2 ~ bas, x == 3 ~ "milieu", x >= 4 ~ haut)
}

illustratives <- data.frame(
  sexe         = etiqueter(tr$sexe, "Sexe"),
  age          = etiqueter(tr$AGE_large, "Âge"),
  diplome      = etiqueter(tr$diplome, "Diplôme"),
  revenu       = etiqueter(tr$revenu, "Revenu"),
  activite     = etiqueter(tr$activite, "Activité"),
  zone         = etiqueter(tr$zone3, "Zone"),
  statut       = etiqueter(tr$statut, "Statut"),
  connaissance = etiqueter(tr$conn_reseau, "Réseau (bonnes réponses)"),
  interet      = etiqueter(trois_positions(tr$interet_transition, "faible", "fort"),
                           "Intérêt transition"),
  bonus        = etiqueter(if_else(tr$mesure_bonus == 1, "oui", "non"), "Bonus"),
  malus        = etiqueter(if_else(tr$mesure_malus == 1, "oui", "non"), "Malus"),
  invest       = etiqueter(if_else(tr$mesure_invest_public == 1, "oui", "non"), "Invest. public"),
  regulation   = etiqueter(if_else(tr$mesure_regulation == 1, "oui", "non"), "Régulation"),
  information  = etiqueter(if_else(tr$mesure_information == 1, "oui", "non"), "Information")
)


#############
### L'ACP ###
#############

# ACP normée : chaque variable compte autant, quel que soit l'écart entre 5 et 3
# points au maximum
base <- cbind(actives, illustratives)

acp <- PCA(base,
           quali.sup = (ncol(actives) + 1):ncol(base),
           row.w     = tr$poids_norm,
           ncp       = 11,  # tous les axes, pour avoir le spectre complet
           graph     = FALSE)


######################################
### Combien d'axes interpréter ?   ###
######################################

inertie <- data.frame(axe = 1:6,
                      valeur_propre = round(acp$eig[1:6, 1], 3),
                      inertie       = round(acp$eig[1:6, 2], 1))
inertie
# Axe 1 : 25.6 %, nettement au-dessus des suivants (11.9, 11.0, 10.7, 9.9) :
# un seul axe se détache vraiment.

write_csv2(inertie, file.path(dossier, "01_inertie.csv"))

fviz_screeplot(acp, ncp = 10, addlabels = TRUE,
               title = "Choix de transition : inertie des axes")
ggsave(file.path(dossier, "figure_1_inertie.pdf"), width = 8, height = 5)


###########################
### Lecture des axes    ###
###########################

# Corrélations des variables avec les axes, et contributions
lecture_axes <- tibble(variable = rownames(acp$var$cor),
                       correlation_axe1  = acp$var$cor[, 1],
                       correlation_axe2  = acp$var$cor[, 2],
                       correlation_axe3  = acp$var$cor[, 3],
                       contribution_axe1 = acp$var$contrib[, 1],
                       contribution_axe2 = acp$var$contrib[, 2]) |>
  arrange(correlation_axe1)

lecture_axes |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv2(lecture_axes, file.path(dossier, "02_correlations_variables.csv"))

# Lecture :
#   - axe 1 (25.6 %) : l'axe "nucléaire contre renouvelables". D'un côté, le
#     nucléaire en tête des deux classements (corrélations de -0.82 et -0.84) ;
#     de l'autre, le solaire, l'éolien, le développement des renouvelables et les
#     mobilités vertes. Les deux classements disent la même chose : ce n'est pas
#     un effet mécanique.
#   - axe 2 (11.9 %) : il oppose la réduction de la demande et les productions
#     pilotables "douces" (efficacité, sobriété, gaz renouvelable, hydraulique)
#     au développement de l'offre (renouvelables, solaire, éolien, nucléaire).
#     Les axes suivants ont des inerties trop proches pour être distingués.


##################################
### Les figures               ###
##################################

fviz_pca_var(acp, axes = c(1, 2), repel = TRUE, col.var = "#235E86",
             title = "Choix de transition : cercle des corrélations (plan 1-2)")
ggsave(file.path(dossier, "figure_2_cercle_correlations.pdf"), width = 8, height = 8)

# Les modalités illustratives dans le plan des individus
fviz_pca_ind(acp, axes = c(1, 2), geom = "point", col.ind = "grey80",
             alpha.ind = .3, col.quali.sup = "#B34E3F", repel = TRUE,
             title = "Choix de transition : variables illustratives (plan 1-2)")
ggsave(file.path(dossier, "figure_3_modalites_illustratives.pdf"), width = 11, height = 8)


##########################################
### Les variables illustratives       ###
##########################################

eta2 <- as.data.frame(acp$quali.sup$eta2) |>
  rownames_to_column("variable") |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  arrange(desc(`Dim.1`))
eta2
write_csv2(eta2, file.path(dossier, "03_eta2_illustratives.csv"))

# Contrairement aux conceptions du service public (script 5a, eta² < 0.012),
# l'axe nucléaire / renouvelables est socialement structuré : l'âge (0.054), le
# sexe (0.052), l'activité et l'intérêt pour la transition y pèsent cinq fois
# plus. Les hommes, les 65 ans et plus, les retraités, les propriétaires et ceux
# qui connaissent les acteurs du réseau sont du côté du nucléaire ; les femmes,
# les 18-34 ans, les revenus modestes et ceux qui acceptent bonus et malus, du
# côté des renouvelables. Le diplôme et la zone n'y jouent presque aucun rôle.

vtests <- as.data.frame(acp$quali.sup$v.test[, 1:2]) |>
  rownames_to_column("modalite") |>
  arrange(desc(abs(`Dim.1`)))
head(vtests, 15)
write_csv2(vtests, file.path(dossier, "04_vtests_illustratives.csv"))


#######################
### Sauvegarde      ###
#######################

# Les coordonnées sur les trois premiers axes, alignées sur les lignes de df
# (NA pour les répondants exclus)
coordonnees <- tibble(ligne = seq_len(nrow(df)),
                      tr_axe1 = NA_real_, tr_axe2 = NA_real_, tr_axe3 = NA_real_)
coordonnees[classe, c("tr_axe1", "tr_axe2", "tr_axe3")] <- acp$ind$coord[, 1:3]
saveRDS(coordonnees, file.path(dossier, "coordonnees.rds"))
saveRDS(acp, file.path(dossier, "acp.rds"))
