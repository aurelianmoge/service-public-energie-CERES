# Analyses factorielles multiples : les blocs sont-ils liés entre eux ?
#
# Les scripts 5a à 5d analysent chaque bloc de questions séparément. Ici, on se
# demande si ces blocs partagent une structure commune : par exemple, ceux qui
# défendent un service public national sont-ils aussi ceux qui priorisent le
# nucléaire, ou ceux qui entrent dans le marché ?
#
# L'AFM est faite pour cela : elle équilibre les blocs (aucun ne domine du seul
# fait de son nombre de variables) et mesure leur ressemblance par le coefficient
# RV, de 0 (aucun lien) à 1 (même structure).
#
# Deux combinaisons :
#   A. Les positions politiques sur l'énergie (tous les répondants) : les trois
#      volets du service public et les deux classements de la transition ;
#   B. Représentations et pratiques (longs seulement) : service public,
#      transition, gestes et rapport au marché.
# Puis, plus simplement, les corrélations entre les axes de chaque bloc (C).
#
# Ce script relit les résultats des scripts 5a à 5d : il faut les lancer avant.
#
# Pourquoi pas de CAH ? L'ancienne AFM réunissait des groupes presque
# indépendants (RV < 0.02) ; ses classes reproduisaient alors des modalités
# isolées (la classe 2 était exactement les 218 personnes "autoconsommation
# collective connue"). Une typologie n'a de sens que si les blocs partagent une
# structure : c'est précisément ce que ce script vérifie d'abord.


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

dossier <- "3. Outputs/Analyses factorielles/5. AFM combinées"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

### Les tableaux actifs des quatre blocs

# Chaque objet FactoMineR garde le tableau qu'on lui a donné ($call$X) : on y
# reprend les variables actives, déjà recodées et étiquetées.
racine <- "3. Outputs/Analyses factorielles"
acm_sp     <- readRDS(file.path(racine, "1. Service public", "acm.rds"))
acp_tr     <- readRDS(file.path(racine, "2. Transition", "acp.rds"))
acm_gestes <- readRDS(file.path(racine, "3. Gestes", "acm.rds"))
acm_marche <- readRDS(file.path(racine, "4. Marché et offre verte", "acm.rds"))

service_public <- acm_sp$call$X[, 1:14]       # tous les répondants
transition     <- acp_tr$call$X[, 1:11]       # ceux qui ont rempli les classements
gestes         <- acm_gestes$call$X[, 1:9]    # les longs
marche         <- acm_marche$call$X[, 1:9]    # les longs

# Qui est dans quel tableau
classe <- !is.na(readRDS(file.path(racine, "2. Transition", "coordonnees.rds"))$tr_axe1)
long   <- df$long
c(tous = nrow(df), classements = sum(classe), longs = sum(long), les_deux = sum(classe & long))

# Une limite : l'AFM n'a pas d'équivalent de l'ACM spécifique. Les "NSP" des
# variables qualitatives restent donc des modalités ordinaires ici. Pour les deux
# questions à cases (caractéristiques du SP et garants), c'est gênant : celui qui
# coche "Ne sait pas" a "NSP" aux cinq cases à la fois, ce qui crée à lui seul
# une dimension artificielle et fausse les RV. On écarte donc ces répondants des
# AFM (265). Les NSP des questions simples restent des modalités.
cases_repondues <- !is.na(df$sp_acces) & !is.na(df$garant_etat)
sum(!cases_repondues)
classe_sp <- classe & cases_repondues


###########################################################
### A. Les positions politiques sur l'énergie (tous)    ###
###########################################################

base_A <- cbind(service_public[classe_sp, ], transition[cases_repondues[classe], ])
poids_A <- df$poids_norm[classe_sp]
nrow(base_A)

afm_A <- MFA(base_A,
             group      = c(6, 5, 3, 6, 5),
             type       = c("n", "n", "n", "s", "s"),
             name.group = c("Définition du SP", "Garants", "Organisation",
                            "Leviers", "Productions"),
             row.w      = poids_A,
             ncp        = 5,
             graph      = FALSE)

### Les liens entre blocs

rv_A <- round(afm_A$group$RV, 2)
rv_A
write_csv2(as.data.frame(rv_A) |> rownames_to_column("groupe"),
           file.path(dossier, "A_01_coefficients_RV.csv"))

# Lecture :
#   - le seul lien net est à l'intérieur de la transition : les leviers et les
#     productions se ressemblent (RV = 0.19). Le nucléaire est cohérent d'un
#     classement à l'autre ;
#   - les trois volets du service public se ressemblent très peu (RV <= 0.02) :
#     ils partagent un axe (le pôle "public et national" contre le pôle "marché"
#     du script 5a), mais chacun a aussi ses propres dimensions ;
#   - entre service public et transition, aucun lien (RV <= 0.01). Les
#     positions sur la gouvernance du service public et sur le mix énergétique
#     sont indépendantes.

### Les axes de l'AFM

inertie_A <- data.frame(axe = 1:5,
                        valeur_propre = round(afm_A$eig[1:5, 1], 2),
                        inertie       = round(afm_A$eig[1:5, 2], 1))
inertie_A

contributions_A <- round(afm_A$group$contrib[, 1:3], 1)
contributions_A
write_csv2(as.data.frame(contributions_A) |> rownames_to_column("groupe"),
           file.path(dossier, "A_02_contributions_groupes.csv"))

# L'axe 1 est fait par la transition (leviers et productions, 86 %) : c'est
# l'axe nucléaire / renouvelables du script 5b. L'axe 2 est fait par le service
# public (garants 50 %, organisation 34 %) : c'est l'axe "public et national"
# contre "marché" du script 5a. Aucun axe ne mêle les deux blocs : l'AFM les
# juxtapose, elle ne trouve pas de dimension commune.

fviz_mfa_var(afm_A, choice = "group", repel = TRUE,
             title = "A. Positions politiques : les groupes sur les axes 1 et 2")
ggsave(file.path(dossier, "A_figure_groupes.pdf"), width = 8, height = 7)


#########################################################
### B. Représentations et pratiques (longs)           ###
#########################################################

dans_B <- classe_sp & long
sum(dans_B)

base_B <- cbind(service_public[dans_B, ],
                transition[dans_B[classe], ],
                gestes[dans_B[long], ],
                marche[dans_B[long], ])
poids_B <- df$poids_norm[dans_B]

afm_B <- MFA(base_B,
             group      = c(14, 11, 9, 9),
             type       = c("n", "s", "n", "n"),
             name.group = c("Service public", "Transition", "Gestes", "Marché"),
             row.w      = poids_B,
             ncp        = 5,
             graph      = FALSE)

rv_B <- round(afm_B$group$RV, 2)
rv_B
write_csv2(as.data.frame(rv_B) |> rownames_to_column("groupe"),
           file.path(dossier, "B_01_coefficients_RV.csv"))

contributions_B <- round(afm_B$group$contrib[, 1:3], 1)
contributions_B
write_csv2(as.data.frame(contributions_B) |> rownames_to_column("groupe"),
           file.path(dossier, "B_02_contributions_groupes.csv"))

# Lecture : les quatre blocs sont presque indépendants deux à deux (RV <= 0.04).
# L'axe 1 mêle surtout le marché (39 %), la transition (27 %) et les gestes
# (23 %) : c'est le seul axe un peu transversal, que les corrélations ci-dessous
# permettent de lire plus précisément. Le service public construit plutôt les
# axes 2 et 3.

fviz_mfa_var(afm_B, choice = "group", repel = TRUE,
             title = "B. Représentations et pratiques : les groupes sur les axes 1 et 2")
ggsave(file.path(dossier, "B_figure_groupes.pdf"), width = 8, height = 7)


#########################################################
### C. Les corrélations entre les axes des blocs      ###
#########################################################

# Plus directement : on corrèle les coordonnées des répondants sur les premiers
# axes de chaque bloc (scripts 5a à 5d), avec quelques indicateurs simples.
axes <- readRDS(file.path(racine, "1. Service public", "coordonnees.rds")) |>
  left_join(readRDS(file.path(racine, "2. Transition", "coordonnees.rds")), by = "ligne") |>
  left_join(readRDS(file.path(racine, "3. Gestes", "coordonnees.rds")), by = "ligne") |>
  left_join(readRDS(file.path(racine, "4. Marché et offre verte", "coordonnees.rds")), by = "ligne") |>
  mutate(connaissance_reseau = df$conn_reseau,
         interet_energie     = df$interet_energie,
         payer_vert          = df$payer_vert,
         poids               = df$poids_norm) |>
  select(sp_axe1, sp_axe2, tr_axe1, tr_axe2, gestes_axe1, gestes_axe2, gestes_axe3,
         marche_axe1, marche_axe2, connaissance_reseau, interet_energie, payer_vert, poids)

# Corrélations pondérées, sur les répondants qui ont tous les axes (longs avec
# classements)
complets <- axes |> drop_na()
nrow(complets)
correlations <- cov.wt(select(complets, -poids), wt = complets$poids, cor = TRUE)$cor
round(correlations, 2)
write_csv2(as.data.frame(round(correlations, 3)) |> rownames_to_column("axe"),
           file.path(dossier, "C_correlations_axes.csv"))

# Rappel des sens des axes :
#   sp_axe1     : + marché et décentralisation / - public et national
#   tr_axe1     : + renouvelables / - nucléaire
#   gestes_axe1 : + refus ("pas prêt")
#   gestes_axe2 : + non concerné
#   gestes_axe3 : + prêt à faire / - déjà fait
#   marche_axe1 : + entrée dans le marché (offre verte, alternatif) / - EDF, fidélité
#
# Lecture : aucune corrélation entre axes de blocs ne dépasse 0.25. Les liens
# les plus nets sont :
#   - renouvelables et entrée dans le marché (tr_axe1 / marche_axe1 : 0.23),
#     renouvelables et disposition à payer pour une offre verte (0.28) ;
#   - la connaissance du réseau va avec le nucléaire (tr_axe1 : -0.17) ;
#   - le refus des gestes va avec le nucléaire (-0.13) et le faible intérêt
#     pour l'énergie (-0.22) ;
#   - les tenants du "marché" dans le service public entrent un peu plus dans le
#     marché (sp_axe1 / marche_axe1 : 0.14), mais cette cohérence reste faible.
# Il n'y a donc pas de "vision du monde" énergétique unifiée : on a plusieurs
# dimensions qui varient presque indépendamment les unes des autres.


#######################
### Sauvegarde      ###
#######################

saveRDS(list(A = afm_A, B = afm_B, correlations = correlations),
        file.path(dossier, "afm_combinees.rds"))
