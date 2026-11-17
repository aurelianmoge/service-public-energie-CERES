# Préférer une option, est-ce rejeter son opposée ?
#
# Antoine a montré que le nucléaire et les renouvelables sont à la fois parmi les
# leviers les plus souvent classés premiers et les plus souvent écartés. On
# teste ici si ce sont les mêmes personnes : mettre le nucléaire en premier
# conduit-il à écarter les renouvelables, et inversement ? Plus généralement,
# chaque préférence s'accompagne-t-elle d'un rejet de l'option socialement
# perçue comme opposée ? C'est une autre façon de regarder la polarisation que
# l'ACP du script 5b résume par un axe.
#
# Leviers (G4Q00002) : six options, cinq rangs. On garde les 1921 répondants qui
# ont rempli les cinq rangs : chacun écarte exactement une option. Productions
# (G4Q00003) : cinq options, trois rangs. On garde les 2473 répondants qui en
# ont cité trois (sans "autre") : chacun en laisse deux de côté.
#
# Le test : si l'option écartée ne dépendait pas de l'option préférée, ceux qui
# mettent A en premier écarteraient B avec la fréquence générale de B, rapportée
# aux options encore disponibles. Avec m options écartées par répondant et r_X la
# part de répondants qui écartent X, cette part attendue vaut
# m × r_B / (m - r_A). On compare la part observée à cette part attendue : un
# rapport de 2 veut dire que B est écarté deux fois plus souvent que si la
# préférence pour A ne disait rien du rejet.


###############################
### Chargement des packages ###
###############################

library(tidyverse)
library(survey)


###############################
### Importation des données ###
###############################

df <- readRDS("3. Outputs/df_recode.rds") |> filter(champ_commun)

dossier <- "3. Outputs/Goûts et dégoûts"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

classements <- list(
  leviers = c(lev_nucleaire = "Nucléaire", lev_renouvelables = "Renouvelables",
              lev_sobriete = "Sobriété", lev_efficacite = "Efficacité",
              lev_mobilites = "Mobilités vertes", lev_reseau = "Optimiser le réseau"),
  productions = c(prod_nucleaire = "Nucléaire", prod_solaire = "Solaire", prod_eolien = "Éolien",
                  prod_hydraulique = "Hydraulique", prod_gaz_vert = "Gaz renouvelables")
)
# Le nombre de points du premier rang, et le total des points d'un classement
# complet : 5 et 15 pour les leviers, 3 et 6 pour les productions
premier <- c(leviers = 5, productions = 3)
total   <- c(leviers = 15, productions = 6)

# Pour chaque classement, les répondants qui l'ont rempli en entier, avec leur
# option préférée et les options qu'ils écartent (0 point)
complets <- map(names(classements), function(k) {
  options <- classements[[k]]
  df |>
    filter(rowSums(across(all_of(names(options)))) == total[k]) |>
    mutate(prefere = options[apply(across(all_of(names(options))), 1, function(x) which(x == premier[k]))])
}) |> setNames(names(classements))
map_int(complets, nrow)  # 1921 et 2473


#######################################################
### 1. Les options préférées et les options écartées ###
#######################################################

# Pour chaque option : la part des répondants qui la classent première, et la
# part de ceux qui l'écartent (parts pondérées, en %)
frequences <- map_dfr(names(classements), function(k) {
  d <- complets[[k]]
  map_dfr(names(classements[[k]]), function(v) {
    tibble(classement = k, option = classements[[k]][[v]],
           premiere = 100 * weighted.mean(d[[v]] == premier[k], d$poids_norm),
           ecartee  = 100 * weighted.mean(d[[v]] == 0, d$poids_norm))
  })
})
frequences |> mutate(across(where(is.numeric), ~ round(.x, 1)))
write_csv2(frequences, file.path(dossier, "01_preferees_et_ecartees.csv"))

# Lecture : parmi les leviers, seul le nucléaire est à la fois très souvent
# premier (20 %) et le plus souvent écarté (32 %, loin devant la sobriété, 19
# %). Les renouvelables sont souvent premiers (17 %) mais peu écartés (14 %).
# Pour les productions, le nucléaire est premier pour 41 % et écarté par 41 % ;
# l'éolien et le gaz renouvelable sont surtout écartés (51 et 57 %).


#######################################################
### 2. Préférer A, est-ce écarter B ?                ###
#######################################################

# Pour chaque paire (A préférée, B écartée) : la part observée, la part attendue
# si la préférence ne disait rien du rejet, et leur rapport
paires <- map_dfr(names(classements), function(k) {
  d <- complets[[k]]
  options <- classements[[k]]
  m <- length(options) - (premier[k])                      # options écartées par répondant
  r <- sapply(names(options), function(v) weighted.mean(d[[v]] == 0, d$poids_norm))
  expand_grid(a = names(options), b = names(options)) |>
    filter(a != b) |>
    mutate(classement = k,
           prefere = options[a], ecarte = options[b],
           n = map_int(a, ~ sum(d$prefere == options[.x])),
           observe = map2_dbl(a, b, ~ 100 * weighted.mean(d[[.y]][d$prefere == options[.x]] == 0,
                                                          d$poids_norm[d$prefere == options[.x]])),
           attendu = 100 * m * r[b] / (m - r[a]),
           rapport = observe / attendu) |>
    select(classement, prefere, ecarte, n, observe, attendu, rapport)
})
paires |> mutate(across(where(is.numeric), ~ round(.x, 2))) |> print(n = Inf)
write_csv2(paires, file.path(dossier, "02_paires.csv"))

### La figure : les rapports pour les leviers

paires |>
  filter(classement == "leviers") |>
  mutate(prefere = factor(prefere, levels = classements$leviers),
         ecarte  = factor(ecarte, levels = rev(classements$leviers))) |>
  ggplot(aes(x = prefere, y = ecarte, fill = log2(rapport))) +
  geom_tile(colour = "white") +
  geom_text(aes(label = format(round(rapport, 1), decimal.mark = ","))) +
  scale_fill_gradient2(low = "#235E86", high = "#B34E3F", mid = "white", limits = log2(c(0.5, 2)),
                       breaks = log2(c(0.5, 1, 2)), labels = c("÷ 2", "1", "× 2")) +
  labs(title = "Préférer un levier, écarter un autre",
       subtitle = "Part de ceux qui écartent l'option (en ligne) parmi ceux qui préfèrent l'option (en colonne),\nrapportée à la part attendue si la préférence ne disait rien du rejet",
       x = "Levier classé premier", y = "Levier écarté", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(dossier, "figure_1_leviers_preferes_et_ecartes.pdf"), width = 8, height = 6)

# Lecture :
#   - le rejet ciblé existe, mais il est modéré. Ceux qui mettent le nucléaire
#     en premier écartent les renouvelables 1,4 fois plus souvent qu'attendu (29
#     % contre 21 %) ; ceux qui mettent les renouvelables en premier écartent le
#     nucléaire 1,2 fois plus souvent (47 % contre 38 %). Pour les productions,
#     l'opposition est plus nette : préférer le solaire va avec le rejet du
#     nucléaire (72 % contre 46 % attendus, 1,6 fois), préférer le nucléaire avec
#     celui du solaire (1,4 fois). Les autres renouvelables écartent aussi le
#     nucléaire plus qu'attendu (1,2 à 1,3 fois) ;
#   - le rejet du nucléaire est général plus que propre à un camp. Il est écarté
#     par 30 à 47 % de ceux qui préfèrent un autre levier, et ceux qui préfèrent
#     la sobriété ne l'écartent pas plus qu'attendu (1,0) ;
#   - les affinités pèsent autant que les rejets. Préférer les renouvelables va
#     avec le fait de garder les mobilités vertes (0,6 fois le rejet attendu), et
#     inversement ; préférer le nucléaire va avec le fait de garder l'optimisation
#     du réseau (0,6) et l'efficacité (0,8). Le solaire et l'éolien se protègent
#     l'un l'autre (0,6). On retrouve deux familles : une famille "verte"
#     (renouvelables, mobilités) et une famille "technique" (nucléaire, réseau,
#     efficacité), qui se repoussent l'une l'autre sans s'exclure.


#######################################################
### 3. À profil égal                                ###
#######################################################

# Le lien entre préférer A et écarter B pourrait venir de ce que les mêmes
# groupes sociaux font les deux (les hommes âgés pour le nucléaire, par
# exemple). Pour les paires opposées de la partie 2, on compare ceux qui
# préfèrent A aux autres (sauf ceux qui préfèrent B), sans puis avec les
# contrôles du bloc A2. L'écart brut contient une part mécanique (les autres
# écartent souvent A lui-même, ce qui laisse moins de place pour écarter B) : on
# n'interprète donc que ce qui en reste à profil égal. Si l'écart reste le même,
# la polarisation n'est pas un effet de composition sociale.
controles <- c("vague", "age_tranche", "sexe", "diplome", "revenu", "difficultes",
               "logement", "statut", "zone3")
opposees <- tribble(
  ~classement,   ~a,                  ~b,
  "leviers",     "lev_nucleaire",     "lev_renouvelables",
  "leviers",     "lev_renouvelables", "lev_nucleaire",
  "productions", "prod_nucleaire",    "prod_solaire",
  "productions", "prod_solaire",      "prod_nucleaire"
)

profil_egal <- pmap_dfr(opposees, function(classement, a, b) {
  d <- complets[[classement]] |>
    filter(prefere != classements[[classement]][[b]]) |>
    mutate(prefere_a = as.numeric(.data[[a]] == premier[classement]),
           ecarte_b  = 100 * (.data[[b]] == 0)) |>
    drop_na(all_of(controles))
  plan <- svydesign(ids = ~1, weights = ~poids_norm, data = d)
  brut    <- svyglm(ecarte_b ~ prefere_a, design = plan)
  controle <- svyglm(reformulate(c("prefere_a", controles), "ecarte_b"), design = plan)
  tibble(classement, prefere = classements[[classement]][[a]], ecarte = classements[[classement]][[b]],
         n = nrow(d),
         ecart_brut = coef(brut)["prefere_a"],
         ecart_profil_egal = coef(controle)["prefere_a"],
         se = SE(controle)["prefere_a"],
         part_conservee = 100 * ecart_profil_egal / ecart_brut)
})
profil_egal |> mutate(across(where(is.numeric), ~ round(.x, 1)))
write_csv2(profil_egal, file.path(dossier, "03_paires_a_profil_egal.csv"))

# Lecture : le profil social n'explique presque rien de ces rejets. Entre 90 et
# 100 % de l'écart demeure à profil égal (de 16,1 à 14,7 points pour le
# nucléaire et les renouvelables, de 18,2 à 18,7 pour le nucléaire et le
# solaire). La polarisation n'est donc pas un effet de composition sociale :
# c'est une cohérence des préférences elles-mêmes.
