# La cohérence des opinions : pourquoi les blocs sont-ils si peu liés ?
#
# Le script 5e montre que les blocs de questions sont presque indépendants
# (coefficients RV de l'ordre de 0.01). Ce script prend ce résultat au sérieux et
# le creuse, question par question plutôt que bloc par bloc :
#
#   1. Les corrélations entre treize questions d'opinion, rangées en trois
#      blocs : la gouvernance du service public, les priorités de la transition,
#      les instruments de la transition (mesures jugées acceptables) ;
#   2. Une mesure simple de la cohérence : la part de variance que deux
#      questions partagent en moyenne (r²), à l'intérieur d'un bloc et entre
#      deux blocs ;
#   3. Cette cohérence varie-t-elle selon la connaissance, l'intérêt, le diplôme
#      et l'âge ? C'est l'idée de "contrainte" des systèmes de croyances
#      (Converse, 1964) : les personnes les plus informées ont des opinions plus
#      liées entre elles, parce qu'elles les rattachent à des principes communs.
#   4. Les paires de questions qui se lient davantage chez les plus diplômés.
#
# Les questions retenues ont été posées à tous. On ne garde qu'une ou deux
# options par classement : les options d'un même classement sont liées
# mécaniquement (en monter une fait descendre les autres, voir le script 5b).
#
# Une limite : le questionnaire n'a pas de question d'orientation politique. On
# ne peut donc pas tester si une position gauche-droite relierait les blocs.


###############################
### Chargement des packages ###
###############################

library(tidyverse)


###############################
### Importation des données ###
###############################

df <- readRDS("3. Outputs/df_recode.rds")

dossier <- "3. Outputs/Analyses factorielles/6. Cohérence entre blocs"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)


#####################################
### Les questions et leurs blocs  ###
#####################################

# Toutes sont numériques : des échelles de 1 à 5, des points de classement ou
# des 0/1
questions <- tribble(
  ~variable,              ~bloc,                     ~nom,
  "local",                "Gouvernance du SP",       "Gestion décentralisée (1-5)",
  "concurrence",          "Gouvernance du SP",       "Concurrence (1-5)",
  "solidarite",           "Gouvernance du SP",       "Même service partout",
  "garant_etat",          "Gouvernance du SP",       "État garant",
  "garant_ent_privees",   "Gouvernance du SP",       "Entreprises privées garantes",
  "sp_sans_profit",       "Gouvernance du SP",       "SP = absence de profit",
  "pro_nucleaire",        "Priorités de la transition", "Pro-nucléaire",
  "lev_sobriete",         "Priorités de la transition", "Levier : sobriété",
  "lev_renouvelables",    "Priorités de la transition", "Levier : renouvelables",
  "mesure_malus",         "Instruments de la transition", "Malus sur les fossiles",
  "mesure_bonus",         "Instruments de la transition", "Bonus aux renouvelables",
  "mesure_regulation",    "Instruments de la transition", "Régulation",
  "mesure_invest_public", "Instruments de la transition", "Investissement public"
)

# Les répondants qui ont répondu à toutes ces questions, avec les variables de
# groupe. On regroupe l'intérêt et le diplôme pour avoir des groupes d'au moins
# 450 personnes ; les 34 diplômes "Autre/NSP" sont laissés de côté pour ce
# découpage.
d <- df |>
  drop_na(all_of(questions$variable)) |>
  mutate(connaissance = factor(conn_reseau, labels = c("0 acteur connu", "1 acteur connu", "2 acteurs connus")),
         interet = cut(interet_energie, c(0, 3, 4, 5), labels = c("Intérêt 1 à 3", "Intérêt 4", "Intérêt 5")),
         diplome_3 = fct_collapse(diplome, "Bac ou moins" = c("Infra-bac", "Bac")),
         diplome_3 = factor(na_if(as.character(diplome_3), "Autre/NSP"),
                            levels = c("Bac ou moins", "Bac+2/3", "Bac+5")))
nrow(d)  # 2196


#######################################
### 1. La matrice des corrélations  ###
#######################################

# Corrélations pondérées entre les treize questions
correlations <- cov.wt(d[questions$variable], wt = d$poids_norm, cor = TRUE)$cor
round(correlations, 2)
write_csv2(as.data.frame(round(correlations, 3)) |> rownames_to_column("question"),
           file.path(dossier, "01_correlations.csv"))

# La figure : si les blocs étaient liés, on verrait des couleurs hors des carrés
# de la diagonale
noms <- setNames(questions$nom, questions$variable)
as.data.frame(as.table(correlations)) |>
  mutate(Var1 = factor(noms[as.character(Var1)], levels = questions$nom),
         Var2 = factor(noms[as.character(Var2)], levels = rev(questions$nom)),
         Freq = if_else(Var1 == Var2, NA_real_, Freq)) |>
  ggplot(aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = if_else(is.na(Freq), "", formatC(Freq, format = "f", digits = 2))), size = 2.6) +
  annotate("rect", xmin = c(0.5, 6.5, 9.5), xmax = c(6.5, 9.5, 13.5),
           ymin = c(7.5, 4.5, 0.5), ymax = c(13.5, 7.5, 4.5),
           fill = NA, colour = "grey30", linewidth = 0.6) +
  scale_fill_gradient2(low = "#B34E3F", mid = "white", high = "#235E86", midpoint = 0,
                       limits = c(-0.4, 0.4), na.value = "grey95") +
  labs(title = "Corrélations entre les questions d'opinion",
       subtitle = "Encadrés : les trois blocs (gouvernance du SP, priorités, instruments de la transition)",
       x = NULL, y = NULL, fill = "r") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), panel.grid = element_blank())
ggsave(file.path(dossier, "figure_1_correlations.pdf"), width = 10, height = 8)

# Lecture : les corrélations sont faibles partout, y compris à l'intérieur des
# blocs (la plus forte, hors classement, est de 0.23 entre décentralisation et
# concurrence). Le bloc de la gouvernance du service public est isolé : aucune
# corrélation avec la transition ne dépasse 0.08, sauf "même service partout"
# avec l'investissement public (0.14). Les priorités et les instruments de la
# transition se répondent davantage : le pro-nucléaire va avec le rejet du malus
# et du bonus (- 0.13 et - 0.12), les renouvelables avec le bonus (0.16), la
# sobriété avec la régulation (0.13).


###########################################
### 2. Une mesure de la cohérence       ###
###########################################

# La cohérence d'un ensemble de questions : la part de variance que deux
# questions partagent en moyenne, c'est-à-dire la moyenne des r² (en %).
# Même sans aucun lien réel, un r² n'est jamais nul dans un échantillon : il
# vaut en moyenne 1 / (n - 1). On retire donc cette valeur, ce qui permet de
# comparer des groupes de tailles différentes. Avec des poids, n est la taille
# "effective" de l'échantillon (formule de Kish).
paires_blocs <- outer(questions$bloc, questions$bloc, paste, sep = " / ")
au_dessus    <- upper.tri(paires_blocs)

coherence <- function(donnees) {
  r <- cov.wt(donnees[questions$variable], wt = donnees$poids_norm, cor = TRUE)$cor
  n_effectif <- sum(donnees$poids_norm)^2 / sum(donnees$poids_norm^2)
  r2 <- 100 * (r^2 - 1 / (n_effectif - 1))
  meme_bloc <- outer(questions$bloc, questions$bloc, "==")
  c("À l'intérieur des blocs" = mean(r2[au_dessus & meme_bloc]),
    "Entre les blocs"         = mean(r2[au_dessus & !meme_bloc]),
    "SP / priorités"          = mean(r2[au_dessus & paires_blocs %in% c("Gouvernance du SP / Priorités de la transition")]),
    "SP / instruments"        = mean(r2[au_dessus & paires_blocs %in% c("Gouvernance du SP / Instruments de la transition")]),
    "Priorités / instruments" = mean(r2[au_dessus & paires_blocs %in% c("Priorités de la transition / Instruments de la transition")]))
}

round(coherence(d), 2)

# Lecture : deux questions d'un même bloc partagent en moyenne 1.75 % de leur
# variance, deux questions de blocs différents 0.31 %. Entre la gouvernance du
# service public et la transition, c'est presque rien (0.04 % avec les
# priorités, 0.22 % avec les instruments) ; entre les priorités et les
# instruments de la transition, c'est 0.91 %.
# La faible cohérence n'est donc pas propre aux liens entre blocs : même à
# l'intérieur d'un bloc, les opinions sont peu liées.


###########################################################
### 3. La cohérence selon la connaissance et le diplôme ###
###########################################################

# On calcule la même mesure dans chaque groupe, avec un intervalle de confiance
# par bootstrap (300 tirages avec remise dans le groupe). Le bootstrap gonfle
# lui-même un peu les r² (les répondants tirés deux fois créent des liens) : on
# n'en garde donc que l'écart-type, et l'intervalle est la mesure ± 1.96
# écart-type.
set.seed(2026)
par_groupe <- list()

for (g in c("connaissance", "interet", "diplome_3", "AGE_large")) {
  for (modalite in levels(d[[g]])) {
    groupe <- d |> filter(.data[[g]] == modalite)
    mesure <- coherence(groupe)
    tirages <- replicate(300, coherence(groupe[sample(nrow(groupe), replace = TRUE), ]))
    par_groupe[[paste(g, modalite)]] <- tibble(
      variable = g, groupe = modalite, n = nrow(groupe),
      type = names(mesure), coherence = mesure,
      ic_bas  = mesure - 1.96 * apply(tirages, 1, sd),
      ic_haut = mesure + 1.96 * apply(tirages, 1, sd)
    )
  }
}
par_groupe <- bind_rows(par_groupe)

par_groupe |>
  filter(type %in% c("À l'intérieur des blocs", "Entre les blocs")) |>
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  print(n = Inf)
write_csv2(par_groupe, file.path(dossier, "02_coherence_par_groupe.csv"))

# La figure
par_groupe |>
  filter(type %in% c("À l'intérieur des blocs", "Entre les blocs")) |>
  mutate(variable = recode(variable, connaissance = "Connaissance du réseau",
                           interet = "Intérêt pour l'énergie", diplome_3 = "Diplôme",
                           AGE_large = "Âge"),
         variable = factor(variable, levels = c("Connaissance du réseau", "Intérêt pour l'énergie",
                                                "Diplôme", "Âge")),
         groupe = fct_inorder(groupe)) |>
  ggplot(aes(x = coherence, y = fct_rev(groupe), colour = type)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_pointrange(aes(xmin = ic_bas, xmax = ic_haut), position = position_dodge(width = 0.5)) +
  facet_grid(variable ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = c("#235E86", "#E0A458")) +
  labs(title = "Les opinions sont-elles plus cohérentes chez les plus informés ?",
       subtitle = "Part de variance partagée en moyenne par deux questions (r² corrigé, en %), intervalles à 95 %",
       x = "r² moyen (%)", y = NULL, colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(file.path(dossier, "figure_2_coherence_par_groupe.pdf"), width = 9, height = 8)

# Lecture : voir le bloc de commentaires après la section 4.


##################################################################
### 4. Quelles paires se lient chez les plus diplômés ?        ###
##################################################################

# Les corrélations entre blocs, chez les bac+5 et chez les autres
correlations_bac5   <- cov.wt(filter(d, diplome_3 == "Bac+5")[questions$variable],
                              wt = filter(d, diplome_3 == "Bac+5")$poids_norm, cor = TRUE)$cor
correlations_autres <- cov.wt(filter(d, diplome_3 != "Bac+5")[questions$variable],
                              wt = filter(d, diplome_3 != "Bac+5")$poids_norm, cor = TRUE)$cor

paires <- which(au_dessus & outer(questions$bloc, questions$bloc, "!="), arr.ind = TRUE)
paires_diplome <- tibble(question_1 = noms[paires[, 1]],
                         question_2 = noms[paires[, 2]],
                         blocs      = paires_blocs[paires],
                         r_bac5     = correlations_bac5[paires],
                         r_autres   = correlations_autres[paires]) |>
  arrange(desc(abs(r_bac5)))

paires_diplome |> mutate(across(where(is.numeric), ~ round(.x, 2))) |> head(10)
write_csv2(paires_diplome, file.path(dossier, "03_paires_selon_diplome.csv"))

# Lecture des sections 3 et 4 :
#   - à l'intérieur des blocs, la cohérence augmente avec la connaissance du
#     réseau : de 1.36 % [1.06 ; 1.66] sans acteur connu à 2.26 % [1.73 ; 2.80]
#     avec les deux. Entre les blocs, elle augmente à peine (de 0.26 à 0.38 %,
#     intervalles qui se recouvrent). L'intérêt et l'âge ne font pas de
#     différence nette ;
#   - le diplôme est le seul facteur net pour les liens entre blocs : 0.68 %
#     chez les bac+5, contre 0.22 % pour les autres diplômes. Ce surcroît vient
#     des liens entre priorités et instruments de la transition : chez les
#     bac+5, prioriser les renouvelables va avec le bonus (0.26 contre 0.14), la
#     sobriété avec la régulation (0.24 contre 0.09), et le pro-nucléaire contre
#     la régulation (- 0.17 contre - 0.07). Les plus diplômés relient davantage
#     les fins et les moyens de la transition ;
#   - la gouvernance du service public reste isolée dans tous les groupes, y
#     compris chez les plus diplômés et les plus informés.
#
# Conclusion : l'indépendance des blocs n'est pas un défaut de mesure. Les
# opinions sur l'organisation du service public ne sont rattachées ni aux choix
# énergétiques, ni à un principe commun, même chez les plus informés. Le
# nucléaire, lui, structure les opinions sur la transition, et ce lien entre
# fins et moyens est le seul qui se renforce avec le diplôme.


#######################
### Sauvegarde      ###
#######################

saveRDS(list(questions = questions, correlations = correlations,
             coherence = coherence(d), par_groupe = par_groupe, paires_diplome = paires_diplome),
        file.path(dossier, "coherence.rds"))
