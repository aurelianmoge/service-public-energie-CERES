# Analyse des correspondances multiples : les gestes de maîtrise de l'énergie
#
# Troisième analyse par bloc. On cherche comment s'organisent les pratiques
# déclarées, à partir des neuf gestes de G7Q00001 (posés aux longs seulement).
#
# On garde les cinq réponses possibles : "Déjà fait", "Prêt à faire", "Pas prêt",
# "Non concerné" et "Ne sait pas". Ne pas être concerné (un locataire ne peut pas
# isoler) n'est pas la même chose que refuser, et ces deux situations doivent
# pouvoir construire des axes différents. Les "Ne sait pas" sont passifs (ACM
# spécifique, comme dans le script 5a).
#
# Population : les questionnaires longs, c'est-à-dire les personnes qui ont un
# contrat d'électricité à leur nom. Ce n'est pas l'ensemble des adultes : les
# 18-34 ans y sont moins nombreux (voir le script 1).


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

dossier <- "3. Outputs/Analyses factorielles/3. Gestes"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

etiqueter <- function(x, nom) {
  x <- droplevels(fct_na_value_to_level(factor(x), "NSP"))
  fct_relabel(x, ~ paste0(nom, " : ", .x))
}

longs <- df |> filter(long)
nrow(longs)  # 2821


####################################
### Les variables actives       ###
####################################

actives <- data.frame(
  voiture          = etiqueter(longs$geste_voiture, "Limiter la voiture"),
  chauffage        = etiqueter(longs$geste_chauffage, "Baisser le chauffage"),
  heures_creuses   = etiqueter(longs$geste_heures_creuses, "Heures creuses"),
  suivi_conso      = etiqueter(longs$geste_suivi_conso, "Suivre sa conso"),
  information      = etiqueter(longs$geste_information, "S'informer"),
  petits_equip     = etiqueter(longs$geste_petits_equipements, "Petits équipements"),
  gros_equip       = etiqueter(longs$geste_gros_equipement, "Gros équipement"),
  isolation        = etiqueter(longs$geste_isolation, "Isolation"),
  production       = etiqueter(longs$geste_production, "Produire son énergie")
)

# Les effectifs : "Déjà fait" domine pour les gestes d'usage, "Non concerné"
# est fréquent pour les investissements
summary(actives, maxsum = 6)


####################################
### Les variables illustratives  ###
####################################

trois_positions <- function(x, bas, haut) {
  case_when(x <= 2 ~ bas, x == 3 ~ "milieu", x >= 4 ~ haut)
}

illustratives <- data.frame(
  sexe         = etiqueter(longs$sexe, "Sexe"),
  age          = etiqueter(longs$AGE_large, "Âge"),
  diplome      = etiqueter(longs$diplome, "Diplôme"),
  revenu       = etiqueter(longs$revenu, "Revenu"),
  activite     = etiqueter(longs$activite, "Activité"),
  zone         = etiqueter(longs$zone3, "Zone"),
  logement     = etiqueter(longs$logement, "Logement"),
  statut       = etiqueter(longs$statut, "Statut"),
  chauffage_el = etiqueter(longs$chauffage, "Chauffage"),
  difficultes  = etiqueter(longs$difficultes, "Difficultés à payer"),
  interet      = etiqueter(trois_positions(longs$interet_energie, "faible", "fort"),
                           "Intérêt énergie")
)


#############
### L'ACM ###
#############

base <- cbind(actives, illustratives)

modalites_actives <- unlist(lapply(actives, levels))
nsp <- grep(": NSP$", modalites_actives)

acm <- MCA(base,
           quali.sup = (ncol(actives) + 1):ncol(base),
           excl      = nsp,
           row.w     = longs$poids_norm,
           ncp       = 5,
           graph     = FALSE)


######################################
### Combien d'axes interpréter ?   ###
######################################

# Taux modifiés de Benzécri (voir le script 5a)
Q <- ncol(actives)
valeurs_propres <- acm$eig[, 1]
retenues <- valeurs_propres[valeurs_propres > 1 / Q]
taux_modifies <- ((Q / (Q - 1)) * (retenues - 1 / Q))^2

inertie <- data.frame(
  axe            = seq_along(retenues),
  valeur_propre  = round(retenues, 3),
  inertie_brute  = round(acm$eig[seq_along(retenues), 2], 1),
  taux_benzecri  = round(100 * taux_modifies / sum(taux_modifies), 1)
)
inertie
# Deux axes presque à égalité (45 % et 40 % en taux modifiés), puis un troisième
# (13 %) : on interprète les trois.

write_csv2(inertie, file.path(dossier, "01_inertie.csv"))

fviz_screeplot(acm, ncp = 10, addlabels = TRUE,
               title = "Gestes : inertie des axes (brute)")
ggsave(file.path(dossier, "figure_1_inertie.pdf"), width = 8, height = 5)


###########################
### Lecture des axes    ###
###########################

contribution_moyenne <- 100 / (length(modalites_actives) - length(nsp))

lecture_axes <- map_dfr(1:3, function(k) {
  tibble(axe          = k,
         modalite     = rownames(acm$var$coord),
         coordonnee   = acm$var$coord[, k],
         contribution = acm$var$contrib[, k],
         cos2         = acm$var$cos2[, k]) |>
    filter(contribution > contribution_moyenne) |>
    arrange(desc(contribution))
})

lecture_axes |> mutate(across(where(is.numeric), ~ round(.x, 2))) |> print(n = 40)
write_csv2(lecture_axes, file.path(dossier, "02_contributions_modalites.csv"))

# Lecture :
#   - axe 1 (45 % en taux modifiés) : le refus. Toutes les modalités "Pas prêt"
#     se rassemblent d'un côté, quel que soit le geste. Ce n'est pas un geste
#     particulier qu'on refuse, c'est une disposition générale.
#   - axe 2 (40 %) : la capacité d'agir. Les "Non concerné" (gros équipement,
#     production, isolation, petits équipements) s'opposent aux investissements
#     faits ou envisagés.
#   - axe 3 (13 %) : l'intention contre la réalisation. "Prêt à faire" d'un
#     côté, "Déjà fait" (gros équipement, production, isolation) de l'autre.


##################################
### Les figures               ###
##################################

fviz_mca_var(acm, axes = c(1, 2), choice = "var.cat",
             select.var = list(contrib = 25), invisible = "quali.sup",
             repel = TRUE, col.var = "#235E86",
             title = "Gestes : modalités actives (plan 1-2)")
ggsave(file.path(dossier, "figure_2_modalites_actives_1_2.pdf"), width = 11, height = 8)

fviz_mca_var(acm, axes = c(2, 3), choice = "var.cat",
             select.var = list(contrib = 25), invisible = "quali.sup",
             repel = TRUE, col.var = "#235E86",
             title = "Gestes : modalités actives (plan 2-3)")
ggsave(file.path(dossier, "figure_3_modalites_actives_2_3.pdf"), width = 11, height = 8)

fviz_mca_var(acm, axes = c(1, 2), choice = "var.cat",
             invisible = "var", repel = TRUE, col.quali.sup = "#B34E3F",
             title = "Gestes : variables illustratives (plan 1-2)")
ggsave(file.path(dossier, "figure_4_modalites_illustratives.pdf"), width = 11, height = 8)


##########################################
### Les variables illustratives       ###
##########################################

eta2 <- as.data.frame(acm$quali.sup$eta2) |>
  rownames_to_column("variable") |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))
eta2
write_csv2(eta2, file.path(dossier, "03_eta2_illustratives.csv"))

# Trois logiques différentes :
#   - le refus (axe 1) n'est presque pas social (eta² < 0.01) : il est surtout
#     lié au faible intérêt pour l'énergie (0.039) ;
#   - la capacité d'agir (axe 2) est très structurée par la condition matérielle :
#     le statut d'occupation (0.174) et le type de logement (0.134). Les
#     locataires, en appartement, en ville, sont "non concernés" ;
#   - l'intention contre la réalisation (axe 3) suit le cycle de vie : l'âge
#     (0.058) et l'activité (0.044). Les 18-34 ans sont "prêts", les retraités
#     et les propriétaires ont "déjà fait".

vtests <- as.data.frame(acm$quali.sup$v.test[, 1:3]) |>
  rownames_to_column("modalite")
write_csv2(vtests, file.path(dossier, "04_vtests_illustratives.csv"))

# Les modalités les plus liées à chaque axe
vtests |> arrange(desc(abs(`Dim 1`))) |> head(10)
vtests |> arrange(desc(abs(`Dim 2`))) |> head(10)
vtests |> arrange(desc(abs(`Dim 3`))) |> head(10)


#######################
### Sauvegarde      ###
#######################

coordonnees <- tibble(ligne = seq_len(nrow(df)),
                      gestes_axe1 = NA_real_, gestes_axe2 = NA_real_, gestes_axe3 = NA_real_)
coordonnees[df$long, c("gestes_axe1", "gestes_axe2", "gestes_axe3")] <- acm$ind$coord[, 1:3]
saveRDS(coordonnees, file.path(dossier, "coordonnees.rds"))
saveRDS(acm, file.path(dossier, "acm.rds"))
