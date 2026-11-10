# Analyse des correspondances multiples : les conceptions du service public
#
# Première analyse par bloc. On cherche comment s'organisent les représentations
# du service public de l'électricité : ce qui le définit, qui doit le garantir,
# comment le système doit être organisé.
#
# Toutes ces questions ont été posées à tout le monde : l'analyse porte donc sur
# l'ensemble des 3259 répondants redressés, et pas seulement sur les longs.
#
# Choix de méthode :
#   - ACM "spécifique" : les "Ne sait pas" restent dans l'analyse mais sont
#     passifs (ils ne construisent pas les axes). Ils ne sont ni imputés ni
#     supprimés, et on évite qu'un axe entier oppose ceux qui répondent à ceux
#     qui ne répondent pas ;
#   - les variables sociales sont illustratives : elles ne construisent pas
#     l'espace, on regarde seulement où elles s'y placent ;
#   - les individus sont pondérés par le redressement.


###############################
### Chargement des packages ###
###############################

library(tidyverse)
library(FactoMineR)
library(factoextra)


###############################
### Importation des données ###
###############################

# Les données recodées par le script 3
df <- readRDS("3. Outputs/df_recode.rds")

dossier <- "3. Outputs/Analyses factorielles/1. Service public"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

# Dans une ACM, chaque modalité doit avoir un nom unique : on préfixe donc chaque
# modalité par le nom court de sa question ("Accès : oui", "Garant État : non").
# Les valeurs manquantes deviennent "NSP".
etiqueter <- function(x, nom) {
  x <- droplevels(fct_na_value_to_level(factor(x), "NSP"))
  fct_relabel(x, ~ paste0(nom, " : ", .x))
}


####################################
### Les variables actives       ###
####################################

# Quatorze variables, en trois thèmes :
#   - la définition du service public (G3Q00004) et ses caractéristiques
#     (G3Q00005, trois cases au plus) ;
#   - les garants de l'accès à l'énergie (G3Q00007, trois cases au plus) ;
#   - l'organisation du système : solidarité territoriale (G3Q00006),
#     centralisation (G3Q00008) et concurrence (G3Q00009).
#
# Les deux échelles de 1 à 5 sont ramenées à trois modalités : traiter "3" comme
# une position intermédiaire évite de supposer des écarts égaux entre les notes.

trois_positions <- function(x, bas, haut) {
  case_when(x <= 2 ~ bas, x == 3 ~ "milieu", x >= 4 ~ haut)
}

actives <- data.frame(
  definition  = etiqueter(df$sp_definition, "Définition"),
  acces       = etiqueter(if_else(df$sp_acces == 1, "oui", "non"), "Accès garanti"),
  egalite     = etiqueter(if_else(df$sp_egalite == 1, "oui", "non"), "Égalité"),
  solidarite  = etiqueter(if_else(df$sp_solidarite == 1, "oui", "non"), "Solidarité"),
  sans_profit = etiqueter(if_else(df$sp_sans_profit == 1, "oui", "non"), "Sans profit"),
  controle    = etiqueter(if_else(df$sp_controle == 1, "oui", "non"), "Contrôle public"),
  etat        = etiqueter(if_else(df$garant_etat == 1, "oui", "non"), "Garant État"),
  ent_pub     = etiqueter(if_else(df$garant_ent_publiques == 1, "oui", "non"), "Garant ent. publiques"),
  ent_priv    = etiqueter(if_else(df$garant_ent_privees == 1, "oui", "non"), "Garant ent. privées"),
  collectiv   = etiqueter(if_else(df$garant_collectivites == 1, "oui", "non"), "Garant collectivités"),
  individus   = etiqueter(if_else(df$garant_individus == 1, "oui", "non"), "Garant individus"),
  territoires = etiqueter(df$territoires, "Territoires"),
  gestion     = etiqueter(trois_positions(df$local, "centralisée", "décentralisée"), "Gestion"),
  marche      = etiqueter(trois_positions(df$concurrence, "monopole", "concurrence"), "Organisation")
)

# On vérifie les effectifs : une modalité rare peut tirer un axe à elle seule
summary(actives, maxsum = 5)


####################################
### Les variables illustratives  ###
####################################

illustratives <- data.frame(
  sexe        = etiqueter(df$sexe, "Sexe"),
  age         = etiqueter(df$AGE_large, "Âge"),
  diplome     = etiqueter(df$diplome, "Diplôme"),
  revenu      = etiqueter(df$revenu, "Revenu"),
  activite    = etiqueter(df$activite, "Activité"),
  zone        = etiqueter(df$zone3, "Zone"),
  statut      = etiqueter(df$statut, "Statut"),
  connaissance = etiqueter(df$conn_reseau, "Réseau (bonnes réponses)"),
  interet     = etiqueter(trois_positions(df$interet_energie, "faible", "fort"), "Intérêt énergie")
)


#############
### L'ACM ###
#############

base <- cbind(actives, illustratives)

# Les numéros des modalités "NSP" parmi les modalités actives : elles deviennent
# passives
modalites_actives <- unlist(lapply(actives, levels))
nsp <- grep(": NSP$", modalites_actives)
modalites_actives[nsp]

acm <- MCA(base,
           quali.sup = (ncol(actives) + 1):ncol(base),
           excl      = nsp,
           row.w     = df$poids_norm,
           ncp       = 5,
           graph     = FALSE)


######################################
### Combien d'axes interpréter ?   ###
######################################

# En ACM, les pourcentages d'inertie bruts sont toujours faibles et sous-estiment
# l'importance des premiers axes : le découpage en modalités crée beaucoup de
# dimensions artificielles. On utilise donc aussi les taux modifiés de Benzécri,
# qui ne gardent que les axes dont la valeur propre dépasse 1 / Q (Q = nombre de
# variables actives) et les corrigent.
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
# Axe 1 : 9 % brut, 56 % en taux modifiés. Axe 2 : 18 %. Axe 3 : 9 %.
# On interprète les deux premiers axes, le troisième avec prudence.

write_csv2(inertie, file.path(dossier, "01_inertie.csv"))

fviz_screeplot(acm, ncp = 10, addlabels = TRUE,
               title = "Conceptions du service public : inertie des axes (brute)")
ggsave(file.path(dossier, "figure_1_inertie.pdf"), width = 8, height = 5)


###########################
### Lecture des axes    ###
###########################

# Pour chaque axe, les modalités qui contribuent le plus, avec le côté de l'axe
# où elles se trouvent (signe de la coordonnée). La contribution moyenne vaut
# 100 / nombre de modalités actives : au-delà, la modalité compte pour l'axe.
contribution_moyenne <- 100 / (length(modalites_actives) - length(nsp))
round(contribution_moyenne, 1)  # 3.3 %

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
#   - axe 1 (56 % en taux modifiés) : il oppose un pôle "public et national"
#     (l'État garant, une gestion centralisée, le monopole public, le même
#     service partout) à un pôle "marché et décentralisation" (l'État n'est pas
#     cité comme garant, au profit des entreprises privées et des individus ;
#     gestion décentralisée, concurrence, prix adaptés aux territoires).
#     C'est l'axe principal, et de loin.
#   - axe 2 (18 %) : il oppose une définition étroite et statutaire du service
#     public (un organisme public, sans profit, sans référence à l'accès ni à
#     l'égalité) à une définition large et plus consensuelle (les deux à la
#     fois, accès garanti, égalité, collectivités et entreprises publiques
#     garantes, positions "milieu" sur les deux échelles). Les réponses "milieu"
#     invitent à y voir aussi une manière de répondre.
#   - axe 3 (9 %) : le rôle des entreprises publiques, associé au monopole,
#     contre la concurrence. À lire avec prudence.


##################################
### Les figures               ###
##################################

# Les 25 modalités qui contribuent le plus au plan 1-2
fviz_mca_var(acm, axes = c(1, 2), choice = "var.cat",
             select.var = list(contrib = 25), invisible = "quali.sup",
             repel = TRUE, col.var = "#235E86",
             title = "Conceptions du service public : modalités actives (plan 1-2)")
ggsave(file.path(dossier, "figure_2_modalites_actives.pdf"), width = 11, height = 8)

# Les variables illustratives dans le même plan
fviz_mca_var(acm, axes = c(1, 2), choice = "var.cat",
             invisible = "var", repel = TRUE, col.quali.sup = "#B34E3F",
             title = "Conceptions du service public : variables illustratives (plan 1-2)")
ggsave(file.path(dossier, "figure_3_modalites_illustratives.pdf"), width = 11, height = 8)


##########################################
### Les variables illustratives       ###
##########################################

# Le rapport de corrélation (eta²) mesure la part de la dispersion d'un axe liée
# à une variable illustrative, de 0 à 1. C'est la bonne mesure pour dire si une
# propriété sociale "structure" un axe ; les v-tests disent seulement si une
# modalité s'écarte significativement du centre.
eta2 <- as.data.frame(acm$quali.sup$eta2) |>
  rownames_to_column("variable") |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))
eta2
write_csv2(eta2, file.path(dossier, "03_eta2_illustratives.csv"))

# Résultat important : aucune variable sociale ne dépasse 0.012 sur les deux
# premiers axes. Les conceptions du service public sont très peu structurées
# socialement. Le seul gradient net est l'âge sur l'axe 1 : les 18-34 ans et
# les salariés du privé penchent vers le pôle "marché", les retraités, les 65 ans
# et plus et les propriétaires vers le pôle "public et national" (v-tests
# ci-dessous). Ceux qui connaissent les acteurs du réseau sont aussi du côté
# public.

# Les modalités illustratives les plus éloignées du centre sur les deux axes
vtests <- as.data.frame(acm$quali.sup$v.test[, 1:2]) |>
  rownames_to_column("modalite") |>
  arrange(desc(abs(`Dim 1`)))
head(vtests, 15)
write_csv2(vtests, file.path(dossier, "04_vtests_illustratives.csv"))


#######################
### Sauvegarde      ###
#######################

# On garde les coordonnées des répondants sur les trois premiers axes, pour les
# relier aux autres analyses et aux régressions (script 6).
coordonnees <- tibble(ligne = seq_len(nrow(df)),
                      sp_axe1 = acm$ind$coord[, 1],
                      sp_axe2 = acm$ind$coord[, 2],
                      sp_axe3 = acm$ind$coord[, 3])
saveRDS(coordonnees, file.path(dossier, "coordonnees.rds"))
saveRDS(acm, file.path(dossier, "acm.rds"))
