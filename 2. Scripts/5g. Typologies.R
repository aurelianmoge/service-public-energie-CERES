# Typologies : les conceptions du service public et les rapports aux gestes
#
# Les ACM des scripts 5a et 5c sont des ACM spécifiques (les "Ne sait pas" y
# sont passifs). On en tire ici deux typologies, par une classification
# ascendante hiérarchique (CAH, méthode de Ward) sur les trois premiers axes de
# chaque ACM, consolidée par les k-means :
#   1. quatre conceptions du service public (tous les répondants) ;
#   2. quatre rapports aux gestes de maîtrise de l'énergie (les longs).
# Puis on décrit le profil social des classes, et on croise les deux
# typologies entre elles et avec les positions sur la transition.
#
# Pourquoi des typologies par bloc, et pas une typologie d'ensemble ? Les blocs
# sont presque indépendants (scripts 5e et 5f) : une classification sur tous les
# blocs à la fois produirait des classes faites de modalités isolées (c'était
# le cas de l'ancienne AFM). Croiser deux typologies construites séparément
# permet au contraire de mesurer ce lien.
#
# Pourquoi trois axes ? Ce sont les axes interprétés dans les scripts 5a et 5c
# (plus de 85 % de l'inertie en taux de Benzécri). Les axes suivants sont du
# bruit, qui rendrait les classes instables. Pourquoi quatre classes ? Le gain
# d'inertie diminue ensuite régulièrement, sans nouveau palier, et quatre
# classes restent lisibles ; avec quatre classes, on retrouve la même partition
# en relançant les k-means depuis d'autres points de départ.
#
# Il faut avoir lancé les scripts 5a et 5c.


###############################
### Chargement des packages ###
###############################

library(tidyverse)
library(FactoMineR)
library(survey)
library(flextable)


###############################
### Importation des données ###
###############################

df <- readRDS("3. Outputs/df_recode.rds")

racine  <- "3. Outputs/Analyses factorielles"
dossier <- file.path(racine, "7. Typologies")
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

acm_sp     <- readRDS(file.path(racine, "1. Service public", "acm.rds"))
acm_gestes <- readRDS(file.path(racine, "3. Gestes", "acm.rds"))


###############################################
### 1. Les deux classifications             ###
###############################################

# Pour classer sur les trois premiers axes seulement, on refait une ACP non
# normée sur les coordonnées des répondants : elle respecte exactement les
# distances de l'ACM sur ces trois axes. La CAH (HCPC) tient compte des poids.
classer <- list()
for (nom in c("sp", "gestes")) {
  acm <- if (nom == "sp") acm_sp else acm_gestes
  acp_axes <- PCA(as.data.frame(acm$ind$coord[, 1:3]), scale.unit = FALSE,
                  row.w = acm$call$row.w, ncp = 3, graph = FALSE)
  classer[[nom]] <- HCPC(acp_axes, nb.clust = 4, consol = TRUE, graph = FALSE)
}

# La part de l'inertie des trois axes que la partition en quatre classes
# résume (inertie entre les classes / inertie totale)
for (nom in names(classer)) {
  h <- classer[[nom]]
  gain <- h$call$t$inert.gain
  message(nom, " : ", round(100 * sum(gain[1:3]) / sum(h$call$t$res$eig[, 1]), 1),
          " % de l'inertie des trois axes")
}
# Service public : 49 %. Gestes : 54 %.

### Les noms des classes

# Ils ont été donnés après lecture des descriptions ci-dessous (section 2). Si
# l'on change les scripts 5a ou 5c, il faut vérifier que les numéros des
# classes correspondent toujours à ces noms.
noms_sp <- c("1" = "Monopole public national",
             "2" = "État garant, mais concurrence",
             "3" = "Service public élargi",
             "4" = "En dehors de l'État")
noms_gestes <- c("1" = "Non concernés",
                 "2" = "Déjà engagés",
                 "3" = "Prêts à faire",
                 "4" = "Réfractaires")

df <- df |>
  mutate(classe_sp = factor(noms_sp[as.character(classer$sp$data.clust$clust)], levels = noms_sp),
         classe_gestes = NA_character_)
df$classe_gestes[df$long] <- noms_gestes[as.character(classer$gestes$data.clust$clust)]
df$classe_gestes <- factor(df$classe_gestes, levels = noms_gestes)

# La taille des classes (en % pondérés)
round(100 * prop.table(xtabs(poids_norm ~ classe_sp, df)), 1)
round(100 * prop.table(xtabs(poids_norm ~ classe_gestes, df)), 1)


##############################################
### 2. Ce qui caractérise chaque classe    ###
##############################################

# Pour chaque classe, les modalités sur-représentées (actives et
# illustratives) : % de la classe qui a la modalité (Mod/Cla), % de l'ensemble
# (Global), et v-test. On ne garde que les v-tests supérieurs à 3.
descriptions <- list()
for (nom in c("sp", "gestes")) {
  acm <- if (nom == "sp") acm_sp else acm_gestes
  noms_classes <- if (nom == "sp") noms_sp else noms_gestes
  donnees <- cbind(acm$call$X, classe = classer[[nom]]$data.clust$clust)
  # catdes affiche des messages sans intérêt ("See the Viewer window") : on les
  # capture pour qu'ils n'encombrent pas la console
  invisible(capture.output(
    description <- catdes(donnees, num.var = ncol(donnees), row.w = acm$call$row.w, proba = 0.001)
  ))

  descriptions[[nom]] <- map_dfr(names(description$category), function(k) {
    x <- description$category[[k]]
    tibble(typologie = nom, classe = noms_classes[k], modalite = sub("^.*=", "", rownames(x)),
           pct_classe = x[, "Mod/Cla"], pct_ensemble = x[, "Global"], v_test = x[, "v.test"])
  }) |>
    filter(v_test > 3)
}
descriptions <- bind_rows(descriptions)

descriptions |>
  group_by(typologie, classe) |>
  slice_max(v_test, n = 8) |>
  mutate(across(where(is.numeric), ~ round(.x, 1))) |>
  print(n = Inf)
write_csv2(descriptions, file.path(dossier, "01_description_classes.csv"))

# Lecture, service public :
#   1. Monopole public national (27 %) : monopole, gestion centralisée, sans
#      profit, défini par l'organisme public, même service partout, l'État
#      garant et pas les entreprises privées ;
#   2. État garant, mais concurrence (30 %) : l'État garant mais ni les
#      entreprises publiques ni les collectivités ; concurrence, prix adaptés
#      aux territoires, défini par la mission ;
#   3. Service public élargi (26 %) : les entreprises publiques et les
#      collectivités garantes avec l'État, accès et égalité, "les deux" pour la
#      définition, positions médianes sur la gestion et la concurrence ;
#   4. En dehors de l'État (17 %) : l'État n'est pas cité comme garant (76 %
#      contre 17 % en moyenne), au profit des individus et des entreprises
#      privées ; gestion décentralisée, concurrence.
#
# Lecture, gestes :
#   1. Non concernés (22 %) : non concernés par les gros équipements,
#      l'isolation et la production ; locataires, en appartement ;
#   2. Déjà engagés (40 %) : ont déjà isolé, changé de gros équipement, suivent
#      leur consommation ; propriétaires de maison ;
#   3. Prêts à faire (25 %) : prêts pour les investissements (production,
#      gros équipement, isolation) qu'ils n'ont pas encore faits ;
#   4. Réfractaires (13 %) : pas prêts, y compris pour les gestes du quotidien
#      (heures creuses, chauffage, suivi de la consommation).

### La figure des profils

# Pour chaque classe, les six modalités actives les plus caractéristiques : la
# barre donne le % dans la classe, le trait le % dans l'ensemble
for (nom in c("sp", "gestes")) {
  modalites_actives <- unlist(lapply(if (nom == "sp") acm_sp$call$X[, 1:14] else acm_gestes$call$X[, 1:9], levels))
  descriptions |>
    filter(typologie == nom, modalite %in% modalites_actives) |>
    group_by(classe) |>
    slice_max(v_test, n = 6) |>
    ungroup() |>
    mutate(classe = factor(classe, levels = if (nom == "sp") noms_sp else noms_gestes),
           # un même libellé peut apparaître dans deux classes : on le rend unique
           modalite = fct_reorder(paste0(classe, "___", modalite), pct_classe)) |>
    ggplot(aes(y = modalite)) +
    geom_col(aes(x = pct_classe), fill = "#235E86", width = 0.7) +
    geom_errorbar(aes(xmin = pct_ensemble, xmax = pct_ensemble), colour = "#E0A458", linewidth = 1, width = 0.8) +
    facet_wrap(~ classe, scales = "free_y", ncol = 2) +
    scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
    labs(title = if (nom == "sp") "Quatre conceptions du service public" else "Quatre rapports aux gestes",
         subtitle = "Modalités les plus caractéristiques : % dans la classe (barre) et dans l'ensemble (trait)",
         x = "%", y = NULL) +
    theme_minimal(base_size = 10)
  ggsave(file.path(dossier, paste0("figure_", if (nom == "sp") 1 else 2, "_profils_", nom, ".pdf")),
         width = 11, height = 7)
}


############################################
### 3. Le profil social des classes      ###
############################################

# Les classes se distinguent-elles socialement ? Pour chaque variable sociale,
# le V de Cramér pondéré avec la classe (de 0, aucun lien, à 1), et le test de
# Rao-Scott (le chi² adapté aux données pondérées).
variables_sociales <- c(sexe = "Sexe", AGE_large = "Âge", diplome = "Diplôme", revenu = "Revenu",
                        zone3 = "Zone", statut = "Statut", conn_reseau = "Connaissance du réseau",
                        interet_energie = "Intérêt pour l'énergie")

liens_sociaux <- list()
for (classe in c("classe_sp", "classe_gestes")) {
  for (v in names(variables_sociales)) {
    # Les modalités absentes (le statut "Sans contrat" chez les longs, par
    # exemple) sont retirées
    d <- df |>
      drop_na(all_of(c(classe, v))) |>
      mutate(across(all_of(v), ~ if (is.factor(.x)) droplevels(.x) else .x))
    plan <- svydesign(ids = ~1, weights = ~poids_norm, data = d)
    tableau <- prop.table(svytable(reformulate(c(classe, v)), plan))
    attendu <- outer(rowSums(tableau), colSums(tableau))
    v_cramer <- sqrt(sum((tableau - attendu)^2 / attendu) / (min(dim(tableau)) - 1))
    test <- svychisq(reformulate(c(classe, v)), plan)
    liens_sociaux[[paste(classe, v)]] <- tibble(typologie = classe, variable = variables_sociales[v],
                                                v_cramer = v_cramer, p = test$p.value)
  }
}
liens_sociaux <- bind_rows(liens_sociaux)
liens_sociaux |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  pivot_wider(names_from = typologie, values_from = c(v_cramer, p))
write_csv2(liens_sociaux, file.path(dossier, "02_liens_sociaux.csv"))

# Le tableau des profils : la répartition de chaque classe (en %, pondérée)
profils <- map_dfr(c("classe_sp", "classe_gestes"), function(nom_classe) {
  map_dfr(names(variables_sociales), function(v) {
    d <- df |>
      drop_na(all_of(c(nom_classe, v))) |>
      mutate(modalite = as.character(.data[[v]]))
    bind_rows(count(d, classe = as.character(.data[[nom_classe]]), modalite, wt = poids_norm),
              count(d, modalite, wt = poids_norm) |> mutate(classe = "Ensemble")) |>
      group_by(classe) |>
      mutate(pct = 100 * n / sum(n)) |>
      ungroup() |>
      mutate(typologie = nom_classe, variable = variables_sociales[v])
  })
})

# Lecture : voir le bloc de commentaires après les tableaux (section 5).


##########################################################
### 4. Les typologies croisées entre elles et avec la transition ###
##########################################################

### Les deux typologies entre elles (les longs)

d <- df |> filter(long)
plan_longs <- svydesign(ids = ~1, weights = ~poids_norm, data = d)
croisement <- svytable(~ classe_sp + classe_gestes, plan_longs)
round(100 * prop.table(croisement, 1), 1)  # en % des lignes (classes du SP)

tableau <- prop.table(croisement)
attendu <- outer(rowSums(tableau), colSums(tableau))
v_croisement <- sqrt(sum((tableau - attendu)^2 / attendu) / (min(dim(tableau)) - 1))
round(v_croisement, 3)
svychisq(~ classe_sp + classe_gestes, plan_longs)

write_csv2(as.data.frame(round(100 * prop.table(croisement, 1), 1)) |>
             pivot_wider(names_from = classe_gestes, values_from = Freq),
           file.path(dossier, "03_croisement_typologies.csv"))

### Les positions sur la transition, par classe

# Pour chaque classe, la moyenne pondérée de quelques positions, en points (%)
positions <- c(pro_nucleaire = "Pro-nucléaire (0-100)", mesure_malus = "Malus acceptable (%)",
               mesure_bonus = "Bonus acceptable (%)", mesure_invest_public = "Investissement public (%)",
               offre_verte = "Offre verte souscrite (%)", payer_vert = "Prêt à payer plus (1-5)")

positions_classes <- map_dfr(c("classe_sp", "classe_gestes"), function(nom_classe) {
  map_dfr(names(positions), function(p) {
    d <- df |> drop_na(all_of(c(nom_classe, p)))
    plan <- svydesign(ids = ~1, weights = ~poids_norm, data = d)
    moyennes <- svyby(reformulate(p), reformulate(nom_classe), plan, svymean)
    # Test d'égalité des moyennes entre les classes
    test <- regTermTest(svyglm(reformulate(nom_classe, p), design = plan), reformulate(nom_classe))
    facteur <- if (p == "payer_vert") 1 else 100
    tibble(typologie = nom_classe, position = positions[p], classe = as.character(moyennes[[nom_classe]]),
           moyenne = facteur * coef(moyennes), ecart_type = facteur * SE(moyennes),
           p_classes = as.numeric(test$p))
  })
})

positions_classes |>
  mutate(cellule = formatC(moyenne, format = "f", digits = 1)) |>
  select(typologie, position, classe, cellule, p_classes) |>
  pivot_wider(names_from = classe, values_from = cellule) |>
  mutate(p_classes = signif(p_classes, 2)) |>
  print(width = 200)
write_csv2(positions_classes, file.path(dossier, "04_positions_par_classe.csv"))

# Lecture : voir le bloc de commentaires après les tableaux (section 5).


###################################
### 5. Les tableaux Word        ###
###################################

### Tableau : profil social des classes

for (nom_classe in c("classe_sp", "classe_gestes")) {
  ordre_classes <- c(levels(df[[nom_classe]]), "Ensemble")
  tableau_profils <- profils |>
    filter(typologie == nom_classe) |>
    mutate(classe = factor(classe, levels = ordre_classes),
           pct = formatC(pct, format = "f", digits = 0)) |>
    arrange(classe) |>
    select(variable, modalite, classe, pct) |>
    pivot_wider(names_from = classe, values_from = pct) |>
    left_join(liens_sociaux |>
                filter(typologie == nom_classe) |>
                transmute(variable, `V de Cramér` = formatC(v_cramer, format = "f", digits = 2)),
              by = "variable") |>
    rename(Variable = variable, Modalité = modalite)

  titre <- if (nom_classe == "classe_sp") "Profil social des quatre conceptions du service public" else
    "Profil social des quatre rapports aux gestes (questionnaire long)"
  flextable(tableau_profils) |>
    merge_v(j = c("Variable", "V de Cramér")) |>
    set_caption(titre) |>
    add_footer_lines("Répartition de chaque classe, en % pondérés. V de Cramér pondéré entre la variable et la classe.") |>
    theme_booktabs() |>
    autofit() |>
    save_as_docx(path = file.path(dossier, paste0("tableau_profils_", sub("classe_", "", nom_classe), ".docx")))
}

### Tableau : positions par classe

tableau_positions <- positions_classes |>
  mutate(cellule = formatC(moyenne, format = "f", digits = 1),
         typologie = recode(typologie, classe_sp = "Conceptions du SP", classe_gestes = "Rapports aux gestes"),
         p_classes = format.pval(p_classes, digits = 2, eps = 0.001)) |>
  select(Typologie = typologie, Position = position, classe, cellule, `p (classes)` = p_classes)

for (typo in unique(tableau_positions$Typologie)) {
  tableau_positions |>
    filter(Typologie == typo) |>
    pivot_wider(names_from = classe, values_from = cellule) |>
    select(-Typologie) |>
    flextable() |>
    set_caption(paste("Positions sur la transition selon les", tolower(typo))) |>
    add_footer_lines("Moyennes pondérées. p : test d'égalité des moyennes entre les classes (Wald).") |>
    theme_booktabs() |>
    autofit() |>
    save_as_docx(path = file.path(dossier, paste0("tableau_positions_", if (typo == "Conceptions du SP") "sp" else "gestes", ".docx")))
}

# Lecture des sections 3 à 5 :
#   - les conceptions du service public ne sont presque pas sociales : aucun V
#     de Cramér ne dépasse 0.07 (connaissance du réseau et intérêt), et le sexe,
#     le revenu et le statut n'y sont pas liés. Ce sont des opinions, pas des
#     positions sociales ;
#   - les rapports aux gestes, eux, le sont : le statut d'occupation (V = 0.30),
#     l'intérêt (0.15), la zone (0.14) et l'âge (0.11). Être "non concerné" ou
#     "déjà engagé" dépend d'abord du logement qu'on occupe ;
#   - les deux typologies sont indépendantes (V = 0.03, p = 0.52) : chaque
#     conception du service public compte la même proportion de réfractaires,
#     de prêts à faire, etc. ;
#   - avec la transition, les conceptions du service public ne se distinguent
#     que sur ce qui touche à l'État : l'investissement public est jugé
#     acceptable par 69-70 % des classes "monopole" et "élargi", contre 51-52 %
#     des deux autres (p < 0.001). Aucune différence sur le nucléaire (p = 0.18).
#     La classe "en dehors de l'État" souscrit plus souvent une offre verte
#     (24 % contre 15 % pour le monopole public) ;
#   - les réfractaires aux gestes sont les plus pro-nucléaires (55 contre 42 à
#     52), les moins favorables au malus (12 % contre 22 à 27 %) et les moins
#     prêts à payer pour une offre verte. On retrouve l'axe nucléaire / sobriété
#     du script 5e, du côté des pratiques.


#######################
### Sauvegarde      ###
#######################

# Les classes de chaque répondant, pour les réutiliser (régressions, synthèse)
saveRDS(df |> select(IdBilendi, classe_sp, classe_gestes), file.path(dossier, "classes.rds"))
saveRDS(list(classifications = classer, descriptions = descriptions, liens_sociaux = liens_sociaux,
             profils = profils, croisement = croisement, positions_classes = positions_classes),
        file.path(dossier, "typologies.rds"))
