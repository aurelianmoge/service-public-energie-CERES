rm(list=ls())
gc()

# La zone déclarée (rural / périurbain / urbain) peut-elle remplacer
# l'agglomération dans le redressement ?
# On regarde d'abord si les enquêtés répondent de façon cohérente, puis si leur
# répartition correspond à une référence INSEE.


###############################
### Chargement des packages ###
###############################

library(dplyr)
library(openxlsx)


###############################
### Importation des données ###
###############################

### Les données de l'enquête

# On lit en texte : les modalités sont des libellés, et on veut maîtriser les
# recodages. fileEncoding retire le marqueur invisible en tête de fichier.
lire_panel <- function(fichier) {
  read.csv(file.path("1. Data", fichier), sep = ",",
           colClasses = "character", fileEncoding = "UTF-8-BOM")
}

df_long  <- lire_panel("panel_long.csv")   # 2839 répondants
df_court <- lire_panel("panel_court.csv")  #  445 répondants

### Les données externes (publiques et agrégées)

# Grille de densité 2025 (géographie au 01/01/2026) : une ligne par commune,
# avec sa population municipale 2023 et son degré de densité croisé avec les
# aires d'attraction des villes. L'en-tête est en ligne 5 du fichier.
pop_densite <- read.xlsx("1. Data/Public data/grille_densite_2026.xlsx",
                         sheet = "Maille communale", startRow = 5) |>
  select(CODGEO, PMUN23, LIBDENS_AAV) |>
  mutate(PMUN23 = as.numeric(PMUN23)) |>
  filter(!startsWith(CODGEO, "976"))  # on écarte Mayotte, comme dans
                                      # le redressement d'Aurélian

nrow(pop_densite)  # 34858 communes


#########################################
### Recodage des variables d'enquête  ###
#########################################

# On repère les modalités par un mot distinctif plutôt que par le libellé

TAILLES <- c("Hameau", "Village", "Petite ville", "Ville moyenne", "Grande ville")
ZONES   <- c("Rural", "Périurbain", "Urbain")

enquete <- bind_rows(
  df_long  |> select(G2Q00001, G2Q00002),
  df_court |> select(G2Q00001, G2Q00002)
) |>
  mutate(
    taille = case_when(
      grepl("hameau",        G2Q00001) ~ "Hameau",
      grepl("village",       G2Q00001) ~ "Village",
      grepl("petite ville",  G2Q00001) ~ "Petite ville",
      grepl("ville moyenne", G2Q00001) ~ "Ville moyenne",
      grepl("grande ville",  G2Q00001) ~ "Grande ville"
    ),
    # Attention à l'ordre : "à proximité d'une zone urbaine" contient aussi le
    # mot "urbaine", il faut donc le tester avant.
    zone = case_when(
      grepl("rurale",    G2Q00002) ~ "Rural",
      grepl("proximité", G2Q00002) ~ "Périurbain",
      grepl("urbaine",   G2Q00002) ~ "Urbain"
    ),
    # On ordonne les modalités du plus rural au plus urbain, sans quoi les
    # tableaux s'affichent par ordre alphabétique.
    taille = factor(taille, levels = TAILLES),
    zone   = factor(zone,   levels = ZONES)
  )

# Seules les non-réponses ne sont pas recodées : on le vérifie
table(enquete$G2Q00001[is.na(enquete$taille)])  # 32 "Ne sait pas"
table(enquete$G2Q00002[is.na(enquete$zone)])    # 39 "Ne sait pas"

# Répartition observée, non-réponses écartées
part <- function(x, niveaux) {
  effectifs <- table(factor(x[!is.na(x)], levels = niveaux))
  round(100 * as.numeric(effectifs) / sum(effectifs), 1)
}

part(enquete$zone, ZONES)  # 37.5 / 13.2 / 49.3


##################################################
### 1. Les deux déclarations sont-elles d'accord ?
##################################################

# Les enquêtés ont répondu à deux questions de localisation. Si la zone était
# comprise de façon erratique, les deux réponses ne concorderaient pas.
tab_croise <- table(enquete$taille, enquete$zone)
round(100 * prop.table(tab_croise, 1), 1)  # 96 % des hameaux se disent ruraux

# V de Cramér : intensité de la liaison entre les deux variables, entre 0 et 1
v_cramer <- function(tab) {
  chi2 <- suppressWarnings(chisq.test(tab)$statistic)
  sqrt(as.numeric(chi2) / (sum(tab) * (min(dim(tab)) - 1)))
}
round(v_cramer(tab_croise), 3)  # 0.603, liaison forte

# Les combinaisons franchement contradictoires : un hameau déclaré urbain, ou
# une grande ville déclarée rurale. Les "village + urbain" n'en font pas partie :
# l'INSEE classe 14.5 % de la population des villages en urbain (voir le script 4).
incoherences <- enquete |>
  filter((taille == "Hameau"       & zone == "Urbain") |
         (taille == "Grande ville" & zone == "Rural"))
nrow(incoherences)  # 40 répondants

# Conclusion : la variable est bien remplie et cohérente.


##################################################
### 2. Est-elle calibrée sur une référence INSEE ?
##################################################

# La grille INSEE distingue quatre postes, le questionnaire trois modalités :
# il faut donc une table de correspondance. Comme ce choix commande le
# résultat, on en teste trois au lieu d'en imposer une.
correspondances <- list(
  "A - urbain = dense + intermédiaire" = c("Rural non périurbain" = "Rural",
                                           "Rural périurbain"     = "Périurbain",
                                           "Urbain intermédiaire" = "Urbain",
                                           "Urbain dense"         = "Urbain"),
  "B - urbain = dense seulement"       = c("Rural non périurbain" = "Rural",
                                           "Rural périurbain"     = "Périurbain",
                                           "Urbain intermédiaire" = "Périurbain",
                                           "Urbain dense"         = "Urbain"),
  "C - rural = tout le rural INSEE"    = c("Rural non périurbain" = "Rural",
                                           "Rural périurbain"     = "Rural",
                                           "Urbain intermédiaire" = "Périurbain",
                                           "Urbain dense"         = "Urbain")
)

# Indice de dissimilarité : la part de l'échantillon qu'il faudrait déplacer
# pour retrouver la répartition réelle. Il s'interprète directement, et reste
# comparable entre des variables qui n'ont pas le même nombre de modalités.
dissimilarite <- function(observe, cible) round(sum(abs(observe - cible)) / 2, 1)

observe_zone <- part(enquete$zone, ZONES)

resultats <- sapply(correspondances, function(corr) {
  cible <- pop_densite |>
    mutate(zone_insee = corr[LIBDENS_AAV]) |>
    group_by(zone_insee) |>
    summarise(pop = sum(PMUN23), .groups = "drop") |>
    mutate(part = round(100 * pop / sum(pop), 1))
  cible <- cible$part[match(ZONES, cible$zone_insee)]
  c(cible, dissimilarite(observe_zone, cible))
})
rownames(resultats) <- c(paste("cible", ZONES), "dissimilarite")

observe_zone  # 37.5 / 13.2 / 49.3
resultats     # dissimilarité : 24.1 / 36.2 / 17.5

# Aucune correspondance ne descend sous 17 points : il faudrait déplacer un
# répondant sur six pour retrouver la réalité. Le motif est toujours le même,
# le périurbain est massivement sous-déclaré : 13.2 % des enquêtés se disent y appartenir,
# contre 18.7 % ou 30.7 % de la population selon la définition retenue.
# Les gens ne se décrivent pas comme périurbains, ils disent rural ou urbain.


###################################################
### 3. Est-ce mieux que la taille de commune ?  ###
###################################################

# Même mesure sur l'autre variable déclarée, en appliquant les seuils du
# questionnaire aux populations communales.
cible_taille <- pop_densite |>
  mutate(taille_insee = cut(PMUN23, c(0, 800, 2000, 20000, 100000, Inf),
                            labels = TAILLES, right = FALSE)) |>
  group_by(taille_insee) |>
  summarise(pop = sum(PMUN23), .groups = "drop") |>
  mutate(part = round(100 * pop / sum(pop), 1))

comparaison_taille <- data.frame(
  modalite = TAILLES,
  observe  = part(enquete$taille, TAILLES),
  cible    = cible_taille$part[match(TAILLES, cible_taille$taille_insee)]
) |>
  mutate(ecart = round(observe - cible, 1))

comparaison_taille
dissimilarite(comparaison_taille$observe, comparaison_taille$cible)  # 21.0

# Sur la tranche "plus de 100 000 habitants", trois chiffres coexistent :
#   15.2 %  la réalité, si l'on raisonne en communes
#   28.5 %  ce que déclarent les enquêtés
#   44.3 %  la cible de Bilendi, qui raisonne en agglomérations
# Le résultat est situé entre les deux


###############################
### Conclusion              ###
###############################

# La zone déclarée est une bonne variable d'analyse : bien remplie (1.2 % de
# non-réponses), cohérente avec la taille de commune déclarée, et intéressante
# en soi puisqu'elle dit comment les gens se situent.
#
# Mais telle quelle, à trois modalités, elle ne peut pas servir au redressement.
# Caler suppose une cible de population à atteindre ; or aucune correspondance
# avec la nomenclature INSEE ne rapproche la répartition déclarée de la
# répartition réelle. On imposerait donc à l'échantillon une structure qu'aucune
# référence ne valide, et comme les poids sont globaux, l'erreur se propagerait
# à toutes les estimations.
#
# Suite : le script 4 (4. Variable zone.R) construit une version à deux
# modalités, en reclassant les périurbains selon leur taille de commune. Cette
# version-là est utilisable pour le redressement, sur une cible corrigée de la
# perception.
#
# Une limite à garder en tête :
#   - la cible est calculée sur la population totale, pas sur les 18 ans et
#     plus ; avec le fichier du recensement elle bougerait légèrement.
