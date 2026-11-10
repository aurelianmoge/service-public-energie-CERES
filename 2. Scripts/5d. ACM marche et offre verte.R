# Analyse des correspondances multiples : le rapport au marché de l'électricité
#
# Quatrième analyse par bloc. On cherche comment s'organise le rapport des
# consommateurs au marché : leur fournisseur, leurs changements de fournisseur,
# l'offre verte (souscrite, disposition à payer, scepticisme, auprès de qui) et
# l'autoconsommation (connaissance, préférence, porteur local).
#
# Ces questions n'ont été posées qu'aux longs (personnes avec un contrat à leur
# nom). Les "Ne sait pas" sont passifs (ACM spécifique, voir le script 5a).
# Les trois affirmations G5Q00002 à G5Q00004 ne sont pas utilisées : leur énoncé
# manque dans les dictionnaires.


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

dossier <- "3. Outputs/Analyses factorielles/4. Marché et offre verte"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

etiqueter <- function(x, nom) {
  x <- droplevels(fct_na_value_to_level(factor(x), "NSP"))
  fct_relabel(x, ~ paste0(nom, " : ", .x))
}

trois_positions <- function(x, bas, haut) {
  case_when(x <= 2 ~ bas, x == 3 ~ "milieu", x >= 4 ~ haut)
}

longs <- df |> filter(long)


####################################
### Les variables actives       ###
####################################

actives <- data.frame(
  fournisseur  = etiqueter(longs$fournisseur, "Fournisseur"),
  changements  = etiqueter(longs$changements, "Changements"),
  offre_verte  = etiqueter(if_else(longs$offre_verte == 1, "oui", "non"), "Offre verte souscrite"),
  payer_vert   = etiqueter(trois_positions(longs$payer_vert, "non", "oui"), "Payer plus pour le vert"),
  sceptique    = etiqueter(if_else(longs$sceptique == 1, "oui", "non"), "Sceptique offre verte"),
  offreur_vert = etiqueter(longs$offreur_vert, "Offre verte auprès de"),
  auto_connue  = etiqueter(longs$autoconso_connue, "Autoconso connue"),
  auto_prefere = etiqueter(longs$autoconso_preference, "Autoconso préférée"),
  porteur      = etiqueter(if_else(longs$porteur_local == 1, "oui", "non"), "Porteur local")
)

summary(actives, maxsum = 8)

# "Offre verte auprès de : Autre/NSP" regroupe surtout des NSP (1094 sur 1145) :
# on le traite aussi comme passif.


####################################
### Les variables illustratives  ###
####################################

illustratives <- data.frame(
  sexe         = etiqueter(longs$sexe, "Sexe"),
  age          = etiqueter(longs$AGE_large, "Âge"),
  diplome      = etiqueter(longs$diplome, "Diplôme"),
  revenu       = etiqueter(longs$revenu, "Revenu"),
  activite     = etiqueter(longs$activite, "Activité"),
  zone         = etiqueter(longs$zone3, "Zone"),
  logement     = etiqueter(longs$logement, "Logement"),
  statut       = etiqueter(longs$statut, "Statut"),
  connaissance = etiqueter(longs$conn_reseau, "Réseau (bonnes réponses)"),
  interet      = etiqueter(trois_positions(longs$interet_energie, "faible", "fort"),
                           "Intérêt énergie")
)


#############
### L'ACM ###
#############

base <- cbind(actives, illustratives)

modalites_actives <- unlist(lapply(actives, levels))
passives <- grep(": NSP$|: Autre/NSP$", modalites_actives)
modalites_actives[passives]

acm <- MCA(base,
           quali.sup = (ncol(actives) + 1):ncol(base),
           excl      = passives,
           row.w     = longs$poids_norm,
           ncp       = 5,
           graph     = FALSE)


######################################
### Combien d'axes interpréter ?   ###
######################################

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

write_csv2(inertie, file.path(dossier, "01_inertie.csv"))

fviz_screeplot(acm, ncp = 10, addlabels = TRUE,
               title = "Marché et offre verte : inertie des axes (brute)")
ggsave(file.path(dossier, "figure_1_inertie.pdf"), width = 8, height = 5)


###########################
### Lecture des axes    ###
###########################

contribution_moyenne <- 100 / (length(modalites_actives) - length(passives))

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
#   - axe 1 (74 % en taux modifiés) : l'entrée dans le marché. D'un côté, l'offre
#     verte souscrite, un fournisseur alternatif, des changements fréquents de
#     fournisseur, une préférence pour une coopérative et la disposition à payer
#     plus. De l'autre, EDF, aucun changement de fournisseur et le scepticisme
#     envers les offres vertes. Un seul axe domine nettement.
#   - axe 2 (16 %) : la confiance. Le scepticisme, TotalEnergies et le refus de
#     payer plus s'opposent au porteur local de confiance et à EDF.


##################################
### Les figures               ###
##################################

fviz_mca_var(acm, axes = c(1, 2), choice = "var.cat",
             select.var = list(contrib = 25), invisible = "quali.sup",
             repel = TRUE, col.var = "#235E86",
             title = "Marché et offre verte : modalités actives (plan 1-2)")
ggsave(file.path(dossier, "figure_2_modalites_actives.pdf"), width = 11, height = 8)

fviz_mca_var(acm, axes = c(1, 2), choice = "var.cat",
             invisible = "var", repel = TRUE, col.quali.sup = "#B34E3F",
             title = "Marché et offre verte : variables illustratives (plan 1-2)")
ggsave(file.path(dossier, "figure_3_modalites_illustratives.pdf"), width = 11, height = 8)


##########################################
### Les variables illustratives       ###
##########################################

eta2 <- as.data.frame(acm$quali.sup$eta2) |>
  rownames_to_column("variable") |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))
eta2
write_csv2(eta2, file.path(dossier, "03_eta2_illustratives.csv"))

# L'entrée dans le marché (axe 1) est modérément sociale : l'âge (0.032), le
# diplôme (0.027) et l'intérêt pour l'énergie (0.023). Les jeunes, les diplômés
# d'un bac+5, les urbains en appartement y entrent ; les retraités et les 65 ans
# et plus restent chez EDF. La confiance (axe 2) suit surtout l'intérêt pour
# l'énergie (0.040) et le diplôme (0.025).

vtests <- as.data.frame(acm$quali.sup$v.test[, 1:3]) |>
  rownames_to_column("modalite")
write_csv2(vtests, file.path(dossier, "04_vtests_illustratives.csv"))

vtests |> arrange(desc(abs(`Dim 1`))) |> head(10)
vtests |> arrange(desc(abs(`Dim 2`))) |> head(10)


#######################
### Sauvegarde      ###
#######################

coordonnees <- tibble(ligne = seq_len(nrow(df)),
                      marche_axe1 = NA_real_, marche_axe2 = NA_real_, marche_axe3 = NA_real_)
coordonnees[df$long, c("marche_axe1", "marche_axe2", "marche_axe3")] <- acm$ind$coord[, 1:3]
saveRDS(coordonnees, file.path(dossier, "coordonnees.rds"))
saveRDS(acm, file.path(dossier, "acm.rds"))
