#### 0. Objets nécessaires et paramètres ####

attendus <- c("df_afm", "base_active", "actives_imp", "base_afm",
              "res_afm", "poids", "ligne_df", "sup", "groupes_actifs",
              "types_actifs", "ncp_imputation", "pdf_afm")
absents <- attendus[!vapply(attendus, exists, logical(1), inherits = TRUE)]
if (length(absents)) stop("Exécuter d'abord l'AFM principale. Objets absents : ",
                          paste(absents, collapse = ", "))
if (!requireNamespace("FactoMineR", quietly = TRUE) ||
    !requireNamespace("missMDA", quietly = TRUE) ||
    !requireNamespace("ggplot2", quietly = TRUE) ||
    !requireNamespace("ggrepel", quietly = TRUE))
  stop("Packages nécessaires : FactoMineR, missMDA, ggplot2, ggrepel.")

dossier_sens <- file.path("3. Outputs", "AFM",
                          "sensibilite")
dir.create(dossier_sens, recursive = TRUE, showWarnings = FALSE)
export_sens <- function(x, nom) utils::write.csv2(
  x, file.path(dossier_sens, nom), row.names = FALSE,
  na = "", fileEncoding = "UTF-8")
figure_sens <- function(p, nom, l = 9, h = 7) ggplot2::ggsave(
  file.path(dossier_sens, nom), plot = p, width = l, height = h, units = "in", device = pdf_afm)
theme_sens <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                 panel.grid.minor = ggplot2::element_blank())
ncp_compare <- 5L
if (nrow(df_afm) != length(poids) || nrow(base_active) != length(poids))
  stop("Les objets de l'AFM principale n'ont pas la même population.")

# Si le script principal avait conservé un autre nombre d'axes, reconstruire
# localement la référence à 5 axes sans modifier l'objet res_afm de la session.
res_ref <- if (ncol(res_afm$ind$coord) == ncp_compare) res_afm else
  FactoMineR::MFA(base = base_afm,
                  group = c(lengths(groupes_actifs), ncol(sup)),
                  type = c("s", "m", "m", "n", "n"),
                  name.group = c(names(groupes_actifs), "Profils illustratifs"),
                  num.group.sup = 5L, row.w = poids, ncp = ncp_compare, graph = FALSE)

#### 1. Variante de méthode : FAMD sur les mêmes 11 variables ####

# La FAMD pondère les variables numériques et catégorielles sans équilibrer
# les quatre thèmes. C'est un test de sensibilité du CHOIX DES GROUPES.
res_famd <- FactoMineR::FAMD(actives_imp, row.w = poids,
                             ncp = ncp_compare, graph = FALSE)
inertie_methodes <- rbind(
  data.frame(methode = "AFM initiale", axe = 1:ncp_compare,
             inertie_pct = res_ref$eig[1:ncp_compare, 2]),
  data.frame(methode = "FAMD sans groupes", axe = 1:ncp_compare,
             inertie_pct = res_famd$eig[1:ncp_compare, 2])
)
export_sens(inertie_methodes, "01_inertie_AFM_FAMD.csv")
# Les pourcentages d'inertie ne sont pas directement équivalents entre méthodes.
# Les ressemblances d'axes sont mesurées sur les coordonnées des mêmes personnes.

#### 2. Variante théorique : responsabilité du service public ####

# G3Q00007 : cinq acteurs cochables (État, entreprises publiques, entreprises
# privées, collectivités, individus), puis une case NSP. « N/A » et « Non »
# indiquent une case non cochée ; une cellule vide/NA est une absence technique.
# Le texte « Autre » n'est pas forcé dans l'une des cinq catégories.
cases <- paste0("G3Q00007_SQ00", 1:6)
if (!all(cases %in% names(df_afm)))
  stop("G3Q00007 incomplète : ", paste(setdiff(cases, names(df_afm)),
                                       collapse = ", "))
rep_cases <- as.data.frame(lapply(df_afm[cases], function(x)
  trimws(as.character(x))), check.names = FALSE)
valeurs_inconnues <- setdiff(unique(unlist(rep_cases, use.names = FALSE)),
                             c("Oui", "Non", "N/A", "", NA_character_))
if (length(valeurs_inconnues)) stop("Codage inattendu de G3Q00007 : ",
                                    paste(valeurs_inconnues, collapse = " | "))
oui <- as.matrix(rep_cases == "Oui")
oui[is.na(oui)] <- FALSE
national <- oui[, 1] | oui[, 2]
local <- oui[, 4]
prive_ou_individuel <- oui[, 3] | oui[, 5]
absent_technique <- rowSums(sapply(rep_cases[1:5], function(x)
  is.na(x) | x == "")) > 0
autre_texte <- rep(FALSE, nrow(df_afm))
if ("G3Q00007_other" %in% names(df_afm)) {
  a <- trimws(as.character(df_afm$G3Q00007_other))
  autre_texte <- !is.na(a) & !a %in% c("", "N/A", "NA")
}

responsabilite <- rep(NA_character_, nrow(df_afm))
responsabilite[national & !local & !prive_ou_individuel] <-
  "Garantie : public national seul"
responsabilite[!national & local & !prive_ou_individuel] <-
  "Garantie : collectivités seules"
responsabilite[national & local & !prive_ou_individuel] <-
  "Garantie : national et collectivités"
responsabilite[!national & !local & prive_ou_individuel] <-
  "Garantie : privé/individus seuls"
responsabilite[(national | local) & prive_ou_individuel] <-
  "Garantie : public et privé/individus"
responsabilite[absent_technique | oui[, 6] | autre_texte] <- NA_character_
responsabilite <- factor(responsabilite)

modalites_resp <- levels(responsabilite)
frequences_resp <- data.frame(
  modalite = c(modalites_resp, "Non classée"),
  n = c(vapply(modalites_resp, function(z) sum(responsabilite == z,
                                               na.rm = TRUE), integer(1)),
        sum(is.na(responsabilite))),
  pct_pondere = c(vapply(modalites_resp, function(z)
    100 * sum(poids[!is.na(responsabilite) & responsabilite == z]) /
      sum(poids), numeric(1)),
    100 * sum(poids[is.na(responsabilite)]) / sum(poids))
)
export_sens(frequences_resp, "02_responsabilite_recodage.csv")

# Une modalité très rare ne doit pas être interprétée comme un pôle stable.
# Au-delà de 20 % de non-classés, le recodage est trop fragile pour être actif.
variante_possible <- mean(is.na(responsabilite)) <= .20 &&
  nlevels(responsabilite) >= 2L
res_resseree <- NULL
if (variante_possible) {
  groupes_ress <- list(
    "Intérêt et connaissance" = c("interet_energie", "score_connaissance"),
    "Service public et gouvernance" = c(
      "service_public", "solidarite_territoriale", "centralisation",
      "concurrence", "responsabilite_garantie"),
    "Autoconsommation" = c("connaissance_autoconso", "preference_autoconso")
  )
  # Reconstruire l'ordre des trois groupes ; l'imputation ne voit QUE l'actif.
  actives_ress <- base_active[setdiff(unlist(groupes_ress, use.names = FALSE),
                                      "responsabilite_garantie")]
  actives_ress$responsabilite_garantie <- responsabilite
  actives_ress <- actives_ress[unlist(groupes_ress, use.names = FALSE)]
  set.seed(20260927)
  imp_ress <- missMDA::imputeFAMD(actives_ress, ncp = ncp_imputation,
                                  method = "Regularized", row.w = poids)
  actif_ress_imp <- as.data.frame(imp_ress$completeObs)
  qual_ress <- names(actives_ress)[vapply(actives_ress, is.factor, logical(1))]
  actif_ress_imp[qual_ress] <- lapply(actif_ress_imp[qual_ress], factor)
  
  # Les deux premiers choix de transition et l'offre verte deviennent
  # illustratifs ; le profil social reste dans son groupe supplémentaire.
  transition <- c("levier_transition", "production_prioritaire")
  base_ress <- cbind(actif_ress_imp[unlist(groupes_ress, use.names = FALSE)],
                     actives_imp[transition],
                     actives_imp["disposition_offre_verte"], sup)
  res_resseree <- FactoMineR::MFA(
    base = base_ress,
    group = c(lengths(groupes_ress), 2L, 1L, ncol(sup)),
    type = c("s", "m", "n", "n", "s", "n"),
    name.group = c(names(groupes_ress), "Transition (illustrative)",
                   "Offre verte (illustrative)", "Profils illustratifs"),
    num.group.sup = 4:6, row.w = poids, ncp = ncp_compare, graph = FALSE
  )
  exporter_groupes <- data.frame(
    groupe = rep(rownames(res_resseree$group$contrib), ncp_compare),
    axe = rep(seq_len(ncp_compare), each = nrow(res_resseree$group$contrib)),
    contribution_pct = as.vector(res_resseree$group$contrib[, 1:ncp_compare])
  )
  exporter_groupes <- exporter_groupes[
    exporter_groupes$groupe %in% names(groupes_ress), ]
  export_sens(exporter_groupes, "03_groupes_AFM_resserree.csv")
  
  tableaux_ress <- function(objet, type) {
    do.call(rbind, lapply(1:3, function(j) data.frame(
      type = type, axe = j, element = rownames(objet$coord),
      coordonnee = objet$coord[, j],
      contribution_pct = objet$contrib[, j],
      cos2 = objet$cos2[, j]
    )))
  }
  export_sens(tableaux_ress(res_resseree$quali.var, "modalité"),
              "03b_modalites_AFM_resserree.csv")
  export_sens(tableaux_ress(res_resseree$quanti.var, "échelle/score"),
              "03c_variables_numeriques_AFM_resserree.csv")
  
  # Carte courte : actives les plus contributives et choix de transition
  # projetés, pour voir s'ils se regroupent du même côté du plan 1-2.
  m <- res_resseree$quali.var
  act_map <- data.frame(modalite = rownames(m$coord),
                        x = m$coord[, 1], y = m$coord[, 2],
                        contribution = rowSums(m$contrib[, 1:2, drop = FALSE]))
  freq_act <- do.call(rbind, lapply(qual_ress, function(v) {
    x <- as.character(actives_ress[[v]])
    data.frame(modalite = levels(actives_ress[[v]]),
               n = vapply(levels(actives_ress[[v]]), function(z)
                 sum(x == z, na.rm = TRUE), integer(1)))
  }))
  act_map$n <- vapply(act_map$modalite, function(z) {
    i <- which(endsWith(z, freq_act$modalite))
    if (length(i) == 1L) freq_act$n[i] else NA_integer_
  }, integer(1))
  act_map <- act_map[!is.na(act_map$n) & act_map$n >= 30L, ]
  act_map <- head(act_map[order(-act_map$contribution), ], 12L)
  mi <- res_resseree$quali.var.sup
  sup_map <- data.frame(modalite = rownames(mi$coord),
                        x = mi$coord[, 1], y = mi$coord[, 2],
                        force = apply(abs(mi$v.test[, 1:2, drop = FALSE]), 1, max))
  sup_map <- sup_map[grepl("Levier :|Production :", sup_map$modalite), ]
  freq_trans <- do.call(rbind, lapply(transition, function(v) {
    x <- as.character(base_active[[v]])
    data.frame(modalite = levels(actives_imp[[v]]),
               n = vapply(levels(actives_imp[[v]]), function(z)
                 sum(x == z, na.rm = TRUE), integer(1)))
  }))
  sup_map$n <- vapply(sup_map$modalite, function(z) {
    i <- which(endsWith(z, freq_trans$modalite))
    if (length(i) == 1L) freq_trans$n[i] else NA_integer_
  }, integer(1))
  sup_map <- sup_map[!is.na(sup_map$n) & sup_map$n >= 30L, ]
  sup_map <- head(sup_map[order(-sup_map$force), ], 7L)
  p_ress <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = "grey75") +
    ggplot2::geom_vline(xintercept = 0, colour = "grey75") +
    ggplot2::geom_point(data = act_map, ggplot2::aes(x, y),
                        colour = "#B34E3F", size = 2) +
    ggrepel::geom_text_repel(data = act_map,
                             ggplot2::aes(x, y, label = modalite), colour = "#9B382A",
                             size = 3, seed = 20260927, max.overlaps = Inf) +
    ggplot2::geom_point(data = sup_map, ggplot2::aes(x, y),
                        colour = "#235E86", shape = 18, size = 3) +
    ggrepel::geom_text_repel(data = sup_map,
                             ggplot2::aes(x, y, label = modalite), colour = "#235E86",
                             size = 3, seed = 20260927, max.overlaps = Inf) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "AFM resserrée : service public et gouvernance",
                  subtitle = "Rouge : actif ; bleu : choix de transition illustratif",
                  x = "Axe 1", y = "Axe 2") + theme_sens
  figure_sens(p_ress, "figure_1_AFM_resserree.pdf", 12, 8)
} else {
  message("Variante resserrée écartée : lire 02_responsabilite_recodage.csv ",
          "(trop de réponses non classées ou une seule modalité).")
}

#### 3. Comparer les positions des mêmes répondants ####

cor_ponderee <- function(x, y, w) {
  x <- x - stats::weighted.mean(x, w)
  y <- y - stats::weighted.mean(y, w)
  sum(w * x * y) / sqrt(sum(w * x^2) * sum(w * y^2))
}
alternatives <- list(FAMD = res_famd)
if (!is.null(res_resseree)) alternatives$AFM_resserree <- res_resseree
correlations_axes <- do.call(rbind, lapply(names(alternatives), function(nom) {
  a <- alternatives[[nom]]$ind$coord
  b <- res_ref$ind$coord
  do.call(rbind, lapply(1:3, function(i)
    do.call(rbind, lapply(1:3, function(j) {
      r <- cor_ponderee(b[, i], a[, j], poids)
      data.frame(variante = nom, axe_initial = i, axe_variante = j,
                 correlation = r, correlation_absolue = abs(r))
    }))))
}))
export_sens(correlations_axes, "04_correlations_entre_axes.csv")
# Signe et ordre des axes arbitraires : lire toute la matrice de corrélations.
p_cor <- ggplot2::ggplot(correlations_axes,
                         ggplot2::aes(x = factor(axe_initial), y = factor(axe_variante),
                                      fill = correlation_absolue)) +
  ggplot2::geom_tile(colour = "white") +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%+.2f", correlation)),
                     size = 3.5) +
  ggplot2::facet_wrap(~ variante) +
  ggplot2::scale_fill_gradient(limits = c(0, 1), low = "#F4F6F9",
                               high = "#235E86", name = "|r|") +
  ggplot2::labs(title = "Correspondance des axes entre modèles",
                subtitle = "Corrélation pondérée signée dans les cases ; couleur = valeur absolue",
                x = "Axe de l'AFM initiale", y = "Axe de la variante") + theme_sens
figure_sens(p_cor, "figure_2_correspondance_axes.pdf", 10, 5)

#### 4. CAH : 3/4/5 classes et 3/5 axes ####

# On réestime l'AFM à 3 axes sur la MÊME base et avec les MÊMES poids.
# HCPC conserve la coupure de Ward pondérée, sans k-means non pondéré.
res_afm_3 <- FactoMineR::MFA(base = base_afm,
                             group = c(lengths(groupes_actifs), ncol(sup)),
                             type = c("s", "m", "m", "n", "n"),
                             name.group = c(names(groupes_actifs), "Profils illustratifs"),
                             num.group.sup = 5L, row.w = poids, ncp = 3L, graph = FALSE)

classe_hcpc <- function(res, k) {
  h <- FactoMineR::HCPC(res, nb.clust = k, consol = FALSE,
                        description = FALSE, graph = FALSE)
  identifiants <- rownames(res$ind$coord)
  c <- as.character(h$data.clust[
    match(identifiants, rownames(h$data.clust)), "clust"])
  if (anyNA(c)) stop("Ordre des répondants perdu dans une CAH.")
  c
}
parts <- list()
modele_de_part <- list()
for (n_axes in c(3L, 5L)) {
  modele <- if (n_axes == 3L) res_afm_3 else res_ref
  for (k in 3:5) {
    nom <- paste0("AFM_", n_axes, "axes_", k, "classes")
    parts[[nom]] <- classe_hcpc(modele, k)
    modele_de_part[[nom]] <- modele
  }
}
if (!is.null(res_resseree)) {
  parts$AFM_resserree_5axes_4classes <- classe_hcpc(res_resseree, 4L)
  modele_de_part$AFM_resserree_5axes_4classes <- res_resseree
}
parts$FAMD_5axes_4classes <- classe_hcpc(res_famd, 4L)
modele_de_part$FAMD_5axes_4classes <- res_famd

# Dix axes : examiner une troncature moins forte de l'espace factoriel.
res_afm_10 <- FactoMineR::MFA(base_afm,
  group = c(lengths(groupes_actifs), ncol(sup)), type = c(types_actifs, "n"),
  name.group = c(names(groupes_actifs), "Profils illustratifs"),
  num.group.sup = length(groupes_actifs) + 1L, row.w = poids, ncp = 10L, graph = FALSE)
parts$AFM_10axes_4classes <- classe_hcpc(res_afm_10, 4L)
modele_de_part$AFM_10axes_4classes <- res_afm_10

# Sensibilité à l'imputation : mêmes personnes, poids, variables, axes et k.
for (q in c(2L, 4L)) {
  set.seed(20260927)
  imp_q <- missMDA::imputeFAMD(base_active, ncp = q,
                              method = "Regularized", row.w = poids)
  base_q <- cbind(as.data.frame(imp_q$completeObs)[unlist(groupes_actifs)], sup)
  mod_q <- FactoMineR::MFA(base_q,
    group = c(lengths(groupes_actifs), ncol(sup)),
    type = c(types_actifs, "n"), num.group.sup = length(groupes_actifs) + 1L,
    name.group = c(names(groupes_actifs), "Profils illustratifs"),
    row.w = poids, ncp = ncp_compare, graph = FALSE)
  nom <- paste0("AFM_imputation", q, "_5axes_4classes")
  parts[[nom]] <- classe_hcpc(mod_q, 4L)
  modele_de_part[[nom]] <- mod_q
}

# Indice de Rand ajusté (ARI) : 1 = mêmes partitions à une permutation des
# noms de classes près ; ~0 = accord comparable au hasard. L'ARI n'utilise
# pas les poids ; les tailles pondérées sont fournies séparément ci-dessous.
choisir2 <- function(x) x * (x - 1) / 2
ari <- function(a, b) {
  tab <- table(a, b)
  n2 <- choisir2(sum(tab))
  joint <- sum(choisir2(tab))
  lignes <- sum(choisir2(rowSums(tab)))
  colonnes <- sum(choisir2(colSums(tab)))
  attendu <- lignes * colonnes / n2
  maximum <- (lignes + colonnes) / 2
  if (abs(maximum - attendu) < 1e-12) return(1)
  (joint - attendu) / (maximum - attendu)
}
noms_parts <- names(parts)
comparaisons_cah <- do.call(rbind, lapply(noms_parts, function(a)
  do.call(rbind, lapply(noms_parts, function(b)
    data.frame(partition_1 = a, partition_2 = b,
               ARI = ari(parts[[a]], parts[[b]]))))))
export_sens(comparaisons_cah, "05_stabilite_partitions_ARI.csv")

# Part d'inertie interclasses calculée dans l'espace propre à chaque modèle.
# Elle augmente mécaniquement avec k ; elle sert à décrire, pas à choisir k.
qualite_partition <- do.call(rbind, lapply(noms_parts, function(nom) {
  x <- as.matrix(modele_de_part[[nom]]$ind$coord)
  g <- parts[[nom]]
  centre_global <- colSums(x * poids) / sum(poids)
  total <- sum(poids * rowSums(sweep(x, 2, centre_global, "-")^2))
  intra <- sum(vapply(split(seq_along(g), g), function(i) {
    centre <- colSums(x[i, , drop = FALSE] * poids[i]) / sum(poids[i])
    sum(poids[i] * rowSums(sweep(x[i, , drop = FALSE], 2,
                                 centre, "-")^2))
  }, numeric(1)))
  data.frame(partition = nom, n_classes = length(unique(g)),
             inertie_inter_pct = 100 * (1 - intra / total),
             plus_petite_classe_n = min(tabulate(as.integer(factor(g)))),
             plus_petite_classe_ponderee_pct = 100 *
               min(vapply(split(seq_along(g), g), function(i) sum(poids[i]),
                          numeric(1))) / sum(poids))
}))
export_sens(qualite_partition, "06_tailles_et_inertie_classes.csv")

identifiants <- data.frame(ligne_df = ligne_df,
                           IdBilendi = if ("IdBilendi" %in% names(df_afm))
                             as.character(df_afm$IdBilendi) else NA_character_, poids = poids)
classements <- cbind(identifiants, as.data.frame(parts, check.names = FALSE))
export_sens(classements, "07_classes_toutes_variantes.csv")

p_ari <- ggplot2::ggplot(comparaisons_cah,
                         ggplot2::aes(x = partition_1, y = partition_2, fill = ARI)) +
  ggplot2::geom_tile(colour = "white") +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", ARI)), size = 2.7) +
  ggplot2::scale_fill_gradient2(limits = c(-1, 1), midpoint = 0,
                                low = "#CD704A", mid = "#F4F6F9",
                                high = "#235E86", name = "ARI") +
  ggplot2::labs(title = "Sensibilité des classes aux choix d’analyse",
                subtitle = "La comparaison décisive pour les axes : 3 et 5 axes à 4 classes",
                x = NULL, y = NULL) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
figure_sens(p_ari, "figure_3_stabilite_CAH.pdf", 12, 10)

saveRDS(list(FAMD = res_famd, AFM_resserree = res_resseree,
             AFM_reference_5axes = res_ref, AFM_3axes = res_afm_3, AFM_10axes = res_afm_10,
             partitions = parts,
             recodage_responsabilite = responsabilite),
        file.path(dossier_sens, "resultats_sensibilite.rds"))
message("Comparaisons terminées : ", dossier_sens,
        ". Lire 02 et 04, puis 05 et 06 avant de retenir une typologie.")
