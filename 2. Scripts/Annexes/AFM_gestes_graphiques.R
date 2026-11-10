# Figures et tableaux réutilisés par les scripts 5a/5b et le dashboard.
# Aucune catégorie n'est éliminée du calcul pour alléger les étiquettes.
spectre_afm_gestes <- function(res) {
  # FactoMineR 2.17 ne restitue que ncp valeurs propres avec sa SVD itérative.
  # Retrouver le spectre complet du MÊME tableau actif pondéré, sans changer
  # les coordonnées ni recalculer l'AFM ou son imputation.
  p <- res$global.pca$call
  x <- as.matrix(p$X[, seq_along(p$col.w), drop = FALSE])
  x <- sweep(sweep(x, 2, p$centre, "-"), 2, p$ecart.type, "/")
  x <- sweep(sweep(x, 1, sqrt(p$row.w), "*"), 2, sqrt(p$col.w), "*")
  valeurs <- pmax(eigen(crossprod(x), symmetric = TRUE, only.values = TRUE)$values, 0)
  total <- sum(valeurs)
  valeurs <- valeurs[valeurs > 1e-10 * max(valeurs)]
  pourcentage <- 100 * valeurs / total
  k <- nrow(res$eig)
  if (max(abs(pourcentage[seq_len(k)] - res$eig[,2])) > 1e-4)
    stop("Le spectre reconstruit ne correspond pas aux axes de l'AFM.")
  data.frame(axe = seq_along(valeurs), valeur_propre = valeurs,
             pourcentage = pourcentage, cumul = cumsum(pourcentage))
}

graphiques_afm_gestes <- function(res, base, poids, sortie, titre,
                                  pdf_device, observee = base, seuil = 30L) {
  dir.create(sortie, recursive = TRUE, showWarnings = FALSE)
  export <- function(x, nom) write.csv2(x, file.path(sortie, nom),
                                        row.names = FALSE, na = "", fileEncoding = "UTF-8")
  quali <- names(base)[vapply(base, is.factor, logical(1))]
  freqs <- do.call(rbind, lapply(quali, function(v) {
    x <- as.character(observee[[v]])
    data.frame(variable = v, modalite = levels(base[[v]]),
      n_observe = vapply(levels(base[[v]]), function(z) sum(x == z, na.rm = TRUE), integer(1)))
  }))
  axes <- seq_len(ncol(res$ind$coord))
  extrait <- function(obj, statut, nature) {
    if (is.null(obj) || is.null(obj$coord)) return(NULL)
    do.call(rbind, lapply(axes, function(j) {
      elements <- rownames(obj$coord)
      f <- match(elements, freqs$modalite)
      data.frame(element = elements, variable = if (nature == "Modalité") freqs$variable[f] else elements,
        statut = statut, nature = nature, axe = j, coordonnee = obj$coord[, j],
        contribution_pct = if (is.null(obj$contrib)) NA_real_ else obj$contrib[, j],
        cos2 = if (is.null(obj$cos2)) NA_real_ else obj$cos2[, j],
        v_test = if (is.null(obj$v.test)) NA_real_ else obj$v.test[, j],
        n_observe = if (nature == "Modalité") freqs$n_observe[f] else
          vapply(elements, function(v) sum(!is.na(observee[[v]])), integer(1)))
    }))
  }
  table <- rbind(extrait(res$quali.var, "Active", "Modalité"),
                 extrait(res$quali.var.sup, "Illustrative", "Modalité"),
                 extrait(res$quanti.var, "Active", "Échelle"),
                 extrait(res$quanti.var.sup, "Illustrative", "Échelle"))
  export(table, "10_toutes_variables_axes.csv")
  # Association de toute variable aux axes : η² qualitative, r² numérique.
  association <- do.call(rbind, lapply(names(base), function(v) do.call(rbind, lapply(axes, function(j) {
    y <- observee[[v]]; ok <- !is.na(y)
    if (is.factor(y)) ok <- ok & !grepl(" : (Non renseigné|Absence technique)$", as.character(y))
    x <- res$ind$coord[ok, j]; w <- poids[ok]; y <- y[ok]
    mu <- weighted.mean(x, w); total <- sum(w * (x - mu)^2)
    if (is.factor(y)) {
      entre <- sum(vapply(split(seq_along(x), droplevels(y)), function(i)
        sum(w[i]) * (weighted.mean(x[i], w[i]) - mu)^2, numeric(1)))
      valeur <- entre / total
    } else {
      ym <- weighted.mean(y, w)
      valeur <- sum(w * (x - mu) * (y - ym))^2 /
        (total * sum(w * (y - ym)^2))
    }
    statut <- unique(table$statut[table$variable == v])
    data.frame(variable = v, statut = if (length(statut)) statut[1] else "Inconnu",
      axe = j, association = valeur, mesure = if (is.factor(y)) "η²" else "r²",
      n_renseigne = sum(ok))
  }))))
  export(association, "11_associations_toutes_variables.csv")
  export(spectre_afm_gestes(res), "12_spectre_complet.csv")
  plots <- list()
  theme <- ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold"))
  contributions <- do.call(rbind, lapply(axes, function(j)
    data.frame(groupe = rownames(res$group$contrib), axe = j,
               contribution = res$group$contrib[, j])))
  plots$groupes_contributions <- ggplot2::ggplot(contributions,
    ggplot2::aes(factor(axe), contribution, fill = groupe)) + ggplot2::geom_col() +
    ggplot2::labs(title = paste(titre, "— contributions des groupes actifs"),
                  x = "Axe", y = "Contribution (%)", fill = "Groupe") + theme
  for (plan in list(c(1L, 2L), c(2L, 3L))) {
    nom <- paste(plan, collapse = "_")
    label_axe <- function(j) sprintf("Axe %d (%.2f %%)", j, res$eig[j, 2])
    xy <- data.frame(x = res$ind$coord[, plan[1]], y = res$ind$coord[, plan[2]])
    cos <- rowSums(res$ind$cos2[, plan, drop = FALSE])
    xy$cos2 <- cos
    plots[[paste0("individus_", nom)]] <- ggplot2::ggplot(xy, ggplot2::aes(x, y, colour = cos2)) +
      ggplot2::geom_point(alpha = .35, size = .7) +
      ggplot2::scale_colour_gradient(low = "#BCCDDD", high = "#173F63") +
      ggplot2::labs(title = paste(titre, "— individus"), colour = "Cos² du plan",
                    subtitle = "Un point par répondant ; couleur = qualité de représentation",
                    x = label_axe(plan[1]), y = label_axe(plan[2])) + theme
    a <- table[table$axe == plan[1] & table$nature == "Modalité", ]
    b <- table[table$axe == plan[2] & table$nature == "Modalité", ]
    a$y <- b$coordonnee[match(a$element, b$element)]
    a$cos2_plan <- a$cos2 + b$cos2[match(a$element, b$element)]
    a$contrib_plan <- a$contribution_pct + b$contribution_pct[match(a$element, b$element)]
    a$force <- pmax(abs(a$v_test), abs(b$v_test[match(a$element, b$element)]), na.rm = TRUE)
    eligible <- !is.na(a$n_observe) & a$n_observe >= seuil &
      !grepl(" : (Non renseigné|Absence technique)$", a$element)
    actifs <- which(eligible & a$statut == "Active")
    illustratifs <- which(eligible & a$statut == "Illustrative")
    etiquettes <- c(head(actifs[order(-a$contrib_plan[actifs])], 14),
                   head(illustratifs[order(-a$force[illustratifs])], 14))
    plots[[paste0("modalites_conjointes_", nom)]] <- ggplot2::ggplot(a,
      ggplot2::aes(coordonnee, y, colour = statut, shape = statut)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey75") +
      ggplot2::geom_vline(xintercept = 0, colour = "grey75") +
      ggplot2::geom_point(alpha = .65, size = 2) +
      ggrepel::geom_text_repel(data = a[etiquettes, ], ggplot2::aes(label = element),
        size = 2.8, max.overlaps = Inf, seed = 20261003) +
      ggplot2::scale_colour_manual(values = c(Active = "#B34E3F", Illustrative = "#235E86")) +
      ggplot2::labs(title = paste(titre, "— modalités actives et illustratives"),
        subtitle = "Étiquettes : 14 actives contributives et 14 illustratives associées ; effectif ≥ 30",
        x = label_axe(plan[1]), y = label_axe(plan[2])) + theme
    # Même plan factoriel : nuage + barycentres qualitatifs, sans flèches numériques.
    plots[[paste0("individus_modalites_", nom)]] <-
      plots[[paste0("modalites_conjointes_", nom)]] +
      ggplot2::geom_point(data = xy, ggplot2::aes(x, y), inherit.aes = FALSE,
                          colour = "grey65", alpha = .12, size = .5) +
      ggplot2::labs(title = paste(titre, "— individus et modalités"))
    if (!is.null(res$quanti.var) || !is.null(res$quanti.var.sup)) {
      correlations <- function(obj, statut) {
        if (is.null(obj)) return(NULL)
        data.frame(variable = rownames(obj$cor), statut = statut,
          x = obj$cor[, plan[1]], y = obj$cor[, plan[2]])
      }
      q <- rbind(correlations(res$quanti.var, "Active"),
                 correlations(res$quanti.var.sup, "Illustrative"))
      angles <- seq(0, 2*pi, length.out = 200)
      plots[[paste0("echelles_", nom)]] <- ggplot2::ggplot(q, ggplot2::aes(x, y)) +
        ggplot2::geom_path(data = data.frame(x = cos(angles), y = sin(angles)), colour = "grey70") +
        ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = x, yend = y, colour = statut),
          arrow = grid::arrow(length = grid::unit(.15, "cm"))) +
        ggrepel::geom_text_repel(ggplot2::aes(label = variable, colour = statut), seed = 20261003) +
        ggplot2::scale_colour_manual(values = c(Active = "#B34E3F", Illustrative = "#235E86")) +
        ggplot2::coord_equal(xlim = c(-1.15, 1.15), ylim = c(-1.15, 1.15)) +
        ggplot2::labs(title = paste(titre, "— échelles actives et illustratives"),
          subtitle = "Cercle des corrélations : coordonnées différentes de celles des modalités",
          x = label_axe(plan[1]), y = label_axe(plan[2])) + theme
    }
    # Comparaison de toutes les variables sur une échelle commune [0,1].
    ass <- association[association$axe %in% plan, ]
    plots[[paste0("associations_variables_", nom)]] <- ggplot2::ggplot(ass,
      ggplot2::aes(factor(axe), variable, fill = association)) + ggplot2::geom_tile() +
      ggplot2::facet_wrap(~statut, scales = "free_y", ncol = 2) +
      ggplot2::scale_fill_gradient(low = "white", high = "#235E86", limits = c(0,1)) +
      ggplot2::labs(title = paste(titre, "— toutes les variables"),
        subtitle = "η² pour les variables qualitatives ; r² pour les échelles, réponses observées",
        x = "Axe", y = NULL, fill = "Association") + theme
  }
  for (nom in names(plots)) ggplot2::ggsave(file.path(sortie, paste0("figure_", nom, ".pdf")),
    plots[[nom]], device = pdf_device, width = 13,
    height = if (grepl("associations", nom)) 9 else 8)
  saveRDS(plots, file.path(sortie, "graphiques.rds"))
  invisible(plots)
}

selection_afm_gestes <- function(actifs, gestes, sup, poids, groupes_origine,
                                  types_origine, sortie, pdf_device,
                                  observees_representations = actifs) {
  # Scénarios définis par leur contenu, jamais par un classement des contributions.
  q <- names(gestes)
  scenarios <- list(
    "service_public_pratiques" = list(
      groupes = c(groupes_origine[1:2], list("Sobriété" = q[1:2],
        "Pilotage et information" = q[3:5], "Équipements et investissements" = q[6:9])),
      types = c(types_origine[1:2], rep("n", 3)),
      justification = "Retirer transition et autoconsommation de l'actif ; conserver tous les gestes et le service public."),
    "electricite_quotidien_logement" = list(
      groupes = c(groupes_origine[1:2], list("Pratiques quotidiennes" = q[c(2,4,6)],
        "Investissements logement" = q[c(7,8)])),
      types = c(types_origine[1:2], "n", "n"),
      justification = "Centrer sur chaleur, suivi, petits équipements, remplacement et isolation ; autres pratiques illustratives."),
    "pratiques_seules" = list(
      groupes = list("Sobriété" = q[1:2], "Pilotage et information" = q[3:5],
                     "Équipements et investissements" = q[6:9]),
      types = rep("n", 3),
      justification = "Décrire les pratiques seules ; toutes les représentations sont illustratives.")
  )
  toutes <- cbind(actifs, gestes, sup)
  observees <- cbind(observees_representations, gestes, sup)
  resultats <- list()
  for (nom in names(scenarios)) {
    s <- scenarios[[nom]]; act <- unlist(s$groupes, use.names = FALSE)
    autres <- setdiff(names(toutes), act)
    # Types des groupes supplémentaires séparés (mixte pour les représentations).
    rep_sup <- intersect(autres, names(actifs)); gestes_sup <- intersect(autres, names(gestes))
    blocs <- c(s$groupes, if (length(rep_sup)) list("Représentations illustratives" = rep_sup),
      if (length(gestes_sup)) list("Gestes illustratifs" = gestes_sup),
      list("Profils illustratifs" = names(sup)))
    types <- c(s$types, if (length(rep_sup)) "m", if (length(gestes_sup)) "n", "n")
    base <- toutes[unlist(blocs, use.names = FALSE)]
    set.seed(20261003)
    r <- FactoMineR::MFA(base, group = lengths(blocs), type = types,
      name.group = names(blocs), num.group.sup = seq.int(length(s$groupes)+1, length(blocs)),
      row.w = poids, ncp = 5, graph = FALSE)
    dossier <- file.path(sortie, "selection", nom)
    graphiques_afm_gestes(r, base, poids, dossier, nom, pdf_device,
                          observee = observees[names(base)])
    resultats[[nom]] <- list(afm = r, groupes = s$groupes, variables_actives = act,
      justification = s$justification, n = nrow(base))
  }
  saveRDS(resultats, file.path(sortie, "selection", "resultats_selection.rds"))
  bilan <- do.call(rbind, lapply(names(resultats), function(nom) {
    a <- resultats[[nom]]
    data.frame(variante = nom, n = a$n, n_variables_actives = length(a$variables_actives),
      n_groupes_actifs = length(a$groupes), plan_12_pct = sum(a$afm$eig[1:2,2]),
      cinq_axes_pct = sum(a$afm$eig[1:5,2]),
      variables_actives = paste(a$variables_actives, collapse = " | "),
      justification = a$justification)
  }))
  write.csv2(bilan, file.path(sortie, "selection", "comparaison_selection.csv"), row.names = FALSE)
  invisible(resultats)
}
