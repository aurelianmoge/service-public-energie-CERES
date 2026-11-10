# Fonctions communes aux deux lanceurs ; pas de dépendance aux objets RStudio.
sys.source("2. Scripts/AFM_gestes_graphiques.R", envir = environment())
executer_afm_gestes <- function(mode = c("illustratif", "contributif")) {
  mode <- match.arg(mode)
  sortie <- file.path("3. Outputs", paste0("AFM_gestes_", mode))
  dir.create(sortie, recursive = TRUE, showWarnings = FALSE)
  export <- function(x, nom) utils::write.csv2(x, file.path(sortie, nom),
                     row.names = FALSE, na = "", fileEncoding = "UTF-8")
  ancien <- options(energie.afm.sortie = file.path(sortie, "reference"),
                    energie.afm.cah = FALSE)
  on.exit(options(ancien), add = TRUE)
  ref <- new.env(parent = globalenv())
  # Le redressement est sourcé explicitement dans l'environnement de référence.
  sys.source("2. Scripts/1. Redressement.R", envir = ref)
  sys.source("2. Scripts/5. AFM et CAH.R", envir = ref)
  questions <- paste0("G7Q00001_SQ00", 1:9)
  if (!all(questions %in% names(ref$df_afm))) stop("Questions G7 absentes.")
  libelles <- c("Mobilité thermique", "Chaleur", "Heures creuses",
                "Suivi consommation", "Information", "Petits équipements",
                "Gros équipement", "Isolation", "Production énergie")
  modalites <- c("Déjà fait", "Jamais fait. Prêt·e à faire",
                "Jamais fait. Pas prêt·e à faire", "Non concerné·e",
                "Ne sait pas / Ne se prononce pas")
  technique <- function(x) is.na(x) | trimws(as.character(x)) %in% c("", "NA", "N/A")
  brut <- ref$df_afm[questions]
  gestes <- as.data.frame(lapply(seq_along(questions), function(j) {
    x <- trimws(as.character(brut[[j]]))
    absent <- technique(x)
    inconnues <- unique(x[!absent & !x %in% modalites])
    if (length(inconnues)) stop("Modalités inconnues : ", questions[j], " : ",
                                paste(inconnues, collapse = " | "))
    # Aucun NSP ni non-concerné n'est imputé ou assimilé à un refus.
    x[absent] <- "Absence technique"
    factor(paste0(libelles[j], " : ", x))
  }))
  names(gestes) <- questions
  complet <- !Reduce(`|`, lapply(brut, technique))
  garder <- if (mode == "illustratif") rep(TRUE, nrow(gestes)) else complet
  if (sum(garder) < 100L) stop("Moins de 100 répondants avec les neuf gestes renseignés.")
  export(data.frame(etape = c("Référence AFM", "Gestes tous renseignés", "Variante"),
    n = c(nrow(gestes), sum(complet), sum(garder))), "00_population.csv")
  export(data.frame(variable = questions, libelle = libelles,
    absences_techniques = vapply(brut, function(x) sum(technique(x)), integer(1))),
    "01_absences_techniques.csv")
  gestes <- droplevels(gestes[garder, , drop = FALSE])
  if (any(vapply(gestes, nlevels, integer(1)) < 2L)) stop("Geste constant.")
  w <- ref$poids[garder]; w <- w * length(w) / sum(w)
  actifs <- droplevels(ref$actives_imp[garder, , drop = FALSE])
  sup <- droplevels(ref$sup[garder, , drop = FALSE])
  sup <- sup[, vapply(sup, nlevels, integer(1)) > 1L, drop = FALSE]
  groupes <- ref$groupes_actifs
  types <- ref$types_actifs
  # Pour comparer à population identique ; les représentations déjà imputées
  # restent fixes. On n'impute pas les gestes et on ne relance pas l'imputation.
  reference <- if (all(garder)) ref$res_afm else FactoMineR::MFA(
    cbind(actifs, sup), group = c(lengths(groupes), ncol(sup)),
    type = c(types, "n"), name.group = c(names(groupes), "Profils illustratifs"),
    num.group.sup = length(groupes) + 1L, row.w = w, ncp = ref$ncp_afm, graph = FALSE)
  if (mode == "illustratif") {
    base <- cbind(actifs, sup, gestes)
    tailles <- c(lengths(groupes), ncol(sup), ncol(gestes))
    noms <- c(names(groupes), "Profils illustratifs", "Gestes illustratifs")
    types <- c(types, "n", "n")
    supplementaires <- c(length(groupes) + 1L, length(groupes) + 2L)
  } else {
    # Trois groupes : le petit équipement reste avec les investissements.
    # Une séparation en quatre groupes serait une autre pondération thématique.
    g7 <- list("Sobriété des usages" = questions[1:2],
               "Pilotage et information" = questions[3:5],
               "Équipements et investissements" = questions[6:9])
    groupes <- c(groupes, g7)
    base <- cbind(actifs, gestes, sup)
    tailles <- c(lengths(groupes), ncol(sup))
    noms <- c(names(groupes), "Profils illustratifs")
    types <- c(types, rep("n", 3), "n")
    supplementaires <- length(groupes) + 1L
  }
  set.seed(20261003) # SVD itérative de FactoMineR : résultats reproductibles
  resultat <- FactoMineR::MFA(base, group = tailles, type = types,
    name.group = noms, num.group.sup = supplementaires,
    row.w = w, ncp = ref$ncp_afm, graph = FALSE)
  if (mode == "illustratif") {
    # Les variables illustratives ne doivent modifier ni inertie ni distances.
    stopifnot(isTRUE(all.equal(reference$eig, resultat$eig, tolerance = 1e-8)))
    i <- seq_len(min(100L, nrow(base)))
    distances_ref <- as.matrix(dist(reference$ind$coord[i, ]))
    ecart <- max(abs(distances_ref - as.matrix(dist(resultat$ind$coord[i, ]))))
    # FactoMineR 2.17 utilise irlba : de faibles écarts numériques sont possibles.
    tolerance <- 1e-4 * max(1, max(distances_ref))
    export(data.frame(ecart_max_distances = ecart, tolerance = tolerance,
                      inertie_identique = TRUE), "00_controle_projection.csv")
    if (ecart > tolerance) stop("Distances modifiées par la projection : écart = ", ecart)
  }
  export(data.frame(groupe = noms, taille = tailles, type = types,
                   statut = ifelse(seq_along(noms) %in% supplementaires,
                                   "illustratif", "actif")), "02_groupes.csv")
  frequences <- do.call(rbind, lapply(questions, function(v) {
    x <- as.character(gestes[[v]])
    lv <- levels(gestes[[v]])
    data.frame(variable = v, modalite = lv,
      n = vapply(lv, function(z) sum(x == z), integer(1)),
      part_ponderee_pct = vapply(lv, function(z) 100 * sum(w[x == z]) / sum(w), numeric(1)))
  }))
  export(frequences, "03_frequences_gestes.csv")
  export(data.frame(axe = seq_len(nrow(resultat$eig)), resultat$eig,
                   check.names = FALSE), "04_inertie.csv")
  export(data.frame(groupe = rownames(resultat$group$contrib),
                    resultat$group$contrib, check.names = FALSE), "05_contributions_groupes.csv")
  objet <- if (mode == "illustratif") resultat$quali.var.sup else resultat$quali.var
  index <- rownames(objet$coord) %in% frequences$modalite
  axes <- seq_len(ncol(resultat$ind$coord))
  modalites_axes <- do.call(rbind, lapply(axes, function(j) {
    elements <- rownames(objet$coord)[index]
    data.frame(axe = j, modalite = elements,
      coordonnee = objet$coord[index, j], cos2 = objet$cos2[index, j],
      contribution_pct = if (mode == "contributif") objet$contrib[index, j] else NA_real_,
      v_test = if (!is.null(objet$v.test)) objet$v.test[index, j] else NA_real_,
      n_observe = frequences$n[match(elements, frequences$modalite)])
  }))
  if (nrow(modalites_axes) != sum(lengths(lapply(gestes, levels))) * length(axes))
    stop("Alignement des modalités G7 à vérifier.")
  export(modalites_axes, "06_modalites_gestes_axes.csv")
  eta <- do.call(rbind, lapply(questions, function(v) do.call(rbind, lapply(axes, function(j) {
    # Association calculée sur les réponses observées ; absences exclues.
    ok <- !grepl(" : Absence technique$", as.character(gestes[[v]]))
    x <- resultat$ind$coord[ok, j]; p <- w[ok]; g <- droplevels(gestes[[v]][ok])
    mu <- weighted.mean(x, p)
    inter <- sum(vapply(split(seq_along(x), g), function(i)
      sum(p[i]) * (weighted.mean(x[i], p[i]) - mu)^2, numeric(1)))
    data.frame(variable = v, axe = j, eta2_pondere = inter / sum(p * (x - mu)^2),
               n_renseigne = sum(ok))
  }))))
  export(eta, "07_associations_eta2.csv")
  export(data.frame(ligne_df = ref$ligne_df[garder], poids = w,
                    resultat$ind$coord, check.names = FALSE), "08_individus_coordonnees.csv")
  comparaison <- abs(cor(reference$ind$coord, resultat$ind$coord))
  export(data.frame(axe_reference = rownames(comparaison), comparaison,
                   check.names = FALSE), "09_correlations_axes_reference.csv")
  # Les signes peuvent changer, les axes peuvent tourner : corrélation partielle.
  p <- ggplot2::ggplot(eta, ggplot2::aes(factor(axe), variable, fill = eta2_pondere)) +
    ggplot2::geom_tile() + ggplot2::scale_fill_gradient(low = "white", high = "#235E86") +
    ggplot2::labs(title = paste("Gestes", mode, "— associations aux axes"),
                  x = "Axe", y = NULL, fill = "η² pondéré") + ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(sortie, "figure_associations.pdf"), p,
                  device = ref$pdf_afm, width = 9, height = 6)
  for (plan in list(c(1L, 2L), c(2L, 3L))) {
    a <- modalites_axes[modalites_axes$axe == plan[1], ]
    b <- modalites_axes[modalites_axes$axe == plan[2], ]
    a$y <- b$coordonnee[match(a$modalite, b$modalite)]
    visible <- a$n_observe >= ref$seuil_effectif_modalite &
      !grepl(" : Absence technique$", a$modalite)
    p <- ggplot2::ggplot(a, ggplot2::aes(coordonnee, y)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey75") +
      ggplot2::geom_vline(xintercept = 0, colour = "grey75") +
      ggplot2::geom_point(colour = "#235E86") +
      ggrepel::geom_text_repel(data = a[visible, ], ggplot2::aes(label = modalite),
                              size = 2.6, max.overlaps = Inf, seed = 20261003) +
      ggplot2::labs(title = paste("Modalités des gestes —", mode),
                    x = paste("Axe", plan[1]), y = paste("Axe", plan[2])) +
      ggplot2::theme_minimal()
    ggplot2::ggsave(file.path(sortie, paste0("figure_modalites_", paste(plan, collapse = "_"), ".pdf")),
                    p, device = ref$pdf_afm, width = 14, height = 10)
  }
  observees <- if (mode == "illustratif")
    cbind(ref$base_active[garder, , drop = FALSE], sup, gestes) else
    cbind(ref$base_active[garder, , drop = FALSE], gestes, sup)
  graphiques_afm_gestes(resultat, base, w, sortie,
                        paste("AFM gestes", mode), ref$pdf_afm, observee = observees)
  if (mode == "contributif") selection_afm_gestes(actifs, gestes, sup, w,
    ref$groupes_actifs, ref$types_actifs, sortie, ref$pdf_afm,
    observees_representations = ref$base_active[garder, , drop = FALSE])
  saveRDS(list(afm = resultat, reference = reference, mode = mode,
    gestes_observes = gestes, ligne_df = ref$ligne_df[garder], poids = w,
    groupes = noms, statut_groupes = supplementaires,
    base = base, base_observee = observees), file.path(sortie, "resultat_afm_gestes.rds"))
  sources <- c("2. Scripts/1. Redressement.R", "2. Scripts/2. Variable age.R",
    "2. Scripts/4. Variable zone.R", "2. Scripts/5. AFM et CAH.R",
    "2. Scripts/AFM_gestes_fonctions.R", "2. Scripts/AFM_gestes_graphiques.R",
    paste0("2. Scripts/", if (mode == "illustratif")
      "5a. AFM gestes illustratifs.R" else "5b. AFM gestes contributifs.R"))
  export(data.frame(fichier = sources, md5 = unname(tools::md5sum(sources))), "sources.csv")
  writeLines(capture.output(sessionInfo()), file.path(sortie, "sessionInfo.txt"))
  message("AFM gestes terminée : ", sortie)
  invisible(resultat)
}
