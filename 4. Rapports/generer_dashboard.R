# LANCEUR UNIQUE — AFM, CAH ET DASHBOARD
# Depuis la racine du projet, dans R :
#   source("4. Rapports/generer_dashboard.R")
# Ou dans un terminal :
#   Rscript --vanilla '4. Rapports/generer_dashboard.R'
#
# Le mode par défaut reconstruit toutes les analyses depuis les données.
# Les deux scripts statistiques du dossier 2 restent les sources de référence.
# Le R Markdown appelle ce même fichier dans un processus R neuf avec
# --analyses : ce mode interne évite toute reprise d'objets d'une ancienne session.
# --rapport-seul permet de refaire uniquement la présentation des sorties existantes.

local({
  arguments <- commandArgs(trailingOnly = TRUE)
  mode_analyses <- "--analyses" %in% arguments
  if (mode_analyses) {
    position <- match("--analyses", arguments)
    if (length(arguments) > position) setwd(arguments[position + 1L])
  }
  if (!file.exists("service-public-energie.Rproj"))
    stop("Ouvrir le projet RStudio ou lancer ce fichier depuis la racine du dépôt.")

  if (mode_analyses) {
    # 1. Reconstruction : ces scripts historiques utilisent l'environnement
    # global, isolé ici dans un processus Rscript --vanilla créé par le rapport.
    source("2. Scripts/5. AFM et CAH.R", local = .GlobalEnv, encoding = "UTF-8")
    source("2. Scripts/5. CAH prolongements.R", local = .GlobalEnv, encoding = "UTF-8")

    # 2. Diagnostic de la population : conservation des poids actuels.
    local({
      # Diagnostic descriptif après l'AFM : aucun poids ni résultat n'est modifié.
      # On distingue l'affectation long/court du filtre sur les réponses actives.
      # Les objets df, df_afm, est_long, poids et pop_joint viennent des scripts amont.

      populations <- list(
        "Panel complet" = df,
        "Longs avant filtre AFM" = df[est_long, , drop = FALSE],
        "Longs analysés" = df_afm
      )
      poids_populations <- list(df$poids_norm, df$poids_norm[est_long], poids)

      marges_etapes <- do.call(rbind, lapply(seq_along(populations), function(j) {
        population <- populations[[j]]
        ponderation <- poids_populations[[j]]
        do.call(rbind, lapply(c("AGE_large", "SEXE", "zone2"), function(variable) {
          modalites <- sort(unique(as.character(df[[variable]])))
          do.call(rbind, lapply(modalites, function(modalite) {
            selection <- as.character(population[[variable]]) == modalite
            data.frame(etape = names(populations)[j], variable = variable,
              modalite = modalite, n = sum(selection), n_total = nrow(population),
              part_brute_pct = 100 * mean(selection),
              part_ponderee_pct = 100 * sum(ponderation[selection]) / sum(ponderation))
          }))
        }))
      }))
      exporter(marges_etapes, "00_calage_etapes.csv")

      # Post-strates : âge × sexe de calage × territoire, identiques à la cible.
      # Un taux de conservation variable selon les cellules suffit à déséquilibrer
      # le sous-échantillon lorsqu'on conserve les poids du panel complet.
      cellules <- as.data.frame(pop_joint)
      cellules$strate <- as.character(cellules$strate)
      for (j in seq_along(populations)) {
        effectifs <- table(factor(as.character(populations[[j]]$strate),
                                 levels = cellules$strate))
        cellules[[c("n_panel", "n_longs", "n_analyse")[j]]] <- as.integer(effectifs)
      }
      cellules$taux_longs_pct <- 100 * cellules$n_longs / cellules$n_panel
      cellules$taux_conservation_afm_pct <- 100 * cellules$n_analyse / cellules$n_longs
      cellules$part_cible_pct <- 100 * cellules$Freq / sum(cellules$Freq)
      cellules$part_analyse_ponderee_pct <- vapply(cellules$strate, function(strate)
        100 * sum(poids[as.character(df_afm$strate) == strate]) / sum(poids), numeric(1))
      exporter(cellules, "00_calage_cellules.csv")
    })

    # 3. Documentation de l’actif et séparation des classes.
    local({
      # Compléments de clôture : documentation de l'actif, lecture de tous les axes
      # retenus et séparation des classes. Exécutable depuis la racine du projet,
      # après l'AFM/CAH ; aucun modèle ni poids n'est modifié.
      if (!requireNamespace("cluster", quietly = TRUE)) stop("Installer le package cluster.")
      dossier <- "3. Outputs/AFM"
      bilan <- file.path(dossier, "bilan")
      dir.create(bilan, recursive = TRUE, showWarnings = FALSE)
      ecrire <- function(x, nom) utils::write.csv2(x, file.path(bilan, nom),
        row.names = FALSE, na = "", fileEncoding = "UTF-8")
      a <- readRDS(file.path(dossier, "resultat_afm.rds"))
      h <- readRDS(file.path(dossier, "resultat_cah.rds"))

      # 1. Dictionnaire synthétique : l'ordre est celui des variables actives de MFA.
      # Les libellés détaillés des choix restent dans les dictionnaires d'enquête.
      dictionnaire <- data.frame(
        groupe = rep(names(a$groupes_actifs), lengths(a$groupes_actifs)),
        variable = unlist(a$groupes_actifs, use.names = FALSE),
        question = c("G3Q00001", "G3Q00002_SQ001–SQ004", "G3Q00004", "G3Q00006",
          "G3Q00008", "G3Q00009", "G4Q00002_1", "G4Q00003_1", "G6Q00004",
          "G4Q00005_SQ001–SQ004", "G4Q00007"),
        traitement = c(
          "Échelle numérique 1–5 : intérêt croissant",
          "Score numérique 0–4 : nombre de rôles correctement identifiés ; NSP = 0",
          "Facteur : organisme / mission / les deux",
          "Facteur : adaptation du prix/service / égalité et solidarité",
          "Échelle numérique : 1 centralisé ; 5 local",
          "Échelle numérique : 1 monopole public ; 5 concurrence",
          "Facteur : premier choix parmi six leviers",
          "Facteur : premier choix parmi six modes de production",
          "Échelle numérique 1–5 : disposition croissante à payer plus",
          "Facteur : inconnue / individuelle / collective / deux formes connues",
          "Facteur : aucune / collective / individuelle"
        ),
        manquants = c("NSP et absences", "Absence technique à au moins un des quatre items",
          rep("NSP et absences", 7), "NSP, absence, combinaison contradictoire ou non classable",
          "NSP et absences")
      )
      stopifnot(identical(dictionnaire$variable, names(a$base_active_observee)))
      ecrire(dictionnaire, "01_dictionnaire_actif.csv")

      # 2. Axes : les coordonnées quantitatives sont des corrélations, tandis que
      # les coordonnées qualitatives sont des positions de modalités. Garder le type
      # explicite ; classer les éléments par contribution au sein de chaque type.
      axes <- seq_len(ncol(a$afm$ind$coord))
      extraire_axes <- function(objet, type) do.call(rbind, lapply(axes, function(j)
        data.frame(axe = j, type = type, element = rownames(objet$coord),
          coordonnee = objet$coord[, j], contribution_pct = objet$contrib[, j],
          cos2 = objet$cos2[, j])))
      elements <- rbind(extraire_axes(a$afm$quali.var, "Modalité"),
                         extraire_axes(a$afm$quanti.var, "Échelle ou score"))
      frequences <- read.csv2(file.path(dossier, "02_frequences_modalites_observees.csv"))
      elements$n_observe <- vapply(seq_len(nrow(elements)), function(i) {
        if (elements$type[i] == "Échelle ou score")
          return(sum(!is.na(a$base_active_observee[[elements$element[i]]])))
        j <- which(endsWith(elements$element[i], frequences$modalite))
        if (length(j) != 1L) stop("Modalité non appariée : ", elements$element[i])
        frequences$n_observe[j]
      }, integer(1))
      elements$pole <- ifelse(elements$coordonnee < 0, "Négatif", "Positif")
      ecrire(elements, "02_elements_tous_axes.csv")
      selection <- elements[elements$n_observe >= 30, ]
      selection <- do.call(rbind, lapply(split(selection,
        list(selection$axe, selection$type, selection$pole), drop = TRUE), function(x)
          head(x[order(-x$contribution_pct), ], 4)))
      ecrire(selection, "03_poles_axes.csv")
      groupes <- extraire_axes(a$afm$group, "Groupe")
      groupes <- groupes[groupes$element %in% names(a$groupes_actifs), ]
      ecrire(groupes, "04_groupes_tous_axes.csv")

      # 3. Même arbre pondéré, mêmes coordonnées non restandardisées : seul k change.
      # La silhouette classique utilise des moyennes de distances NON pondérées.
      # Elle complète l'inertie pondérée sans constituer un critère automatique de k.
      coordonnees <- a$afm$ind$coord
      identifiants <- rownames(coordonnees)
      arbre <- h$call$t$tree
      stopifnot(setequal(identifiants, arbre$labels))
      distances <- stats::dist(coordonnees)
      qualite <- do.call(rbind, lapply(2:8, function(k) {
        partition <- stats::cutree(arbre, k = k)[identifiants]
        silhouette <- cluster::silhouette(as.integer(factor(partition)), distances)[, "sil_width"]
        data.frame(n_classes = k, n_axes = ncol(coordonnees),
          silhouette_moyenne = mean(silhouette), silhouette_mediane = median(silhouette),
          silhouettes_negatives_pct = 100 * mean(silhouette < 0),
          plus_petite_classe_n = min(table(partition)))
      }))
      ecrire(qualite, "05_separation_decoupages.csv")

      # Conserver ici les numéros des classes du rapport principal (HCPC peut
      # réordonner les numéros produits par cutree sans changer les appartenances).
      classes <- as.character(h$data.clust[match(identifiants, rownames(h$data.clust)), "clust"])
      stopifnot(!anyNA(classes))
      silhouette <- cluster::silhouette(as.integer(factor(classes)), distances)[, "sil_width"]
      par_classe <- do.call(rbind, lapply(sort(unique(classes)), function(k) {
        valeurs <- silhouette[classes == k]
        data.frame(classe = k, n = length(valeurs), silhouette_moyenne = mean(valeurs),
          silhouette_mediane = median(valeurs),
          silhouettes_negatives_pct = 100 * mean(valeurs < 0))
      }))
      ecrire(par_classe, "06_separation_classes.csv")
      message("Bilan documentaire et séparation des classes : ", bilan)

      # 4. Vérifier la correspondance entre une classe et une modalité structurante.
      # Le croisement porte sur les réponses observées, avec les absences explicites.
      autoconso <- as.character(a$base_active_observee$connaissance_autoconso)
      autoconso[is.na(autoconso)] <- "Non renseigné"
      croisement <- do.call(rbind, lapply(sort(unique(classes)), function(k) {
        do.call(rbind, lapply(sort(unique(autoconso)), function(modalite) {
          dans <- classes == k
          cible <- dans & autoconso == modalite
          data.frame(classe = k, modalite = modalite, n = sum(cible),
            part_classe_brute_pct = 100 * sum(cible) / sum(dans),
            part_classe_ponderee_pct = 100 * sum(a$poids[cible]) / sum(a$poids[dans]))
        }))
      }))
      ecrire(croisement, "07_autoconso_classes.csv")
    })

    # Versions et empreintes des sources : identifier les données et le code
    # effectivement utilisés, sans copier les données individuelles dans le rapport.
    dossier_trace <- file.path("3. Outputs", "AFM", "reproductibilite")
    dir.create(dossier_trace, recursive = TRUE, showWarnings = FALSE)
    fichiers <- c(
      "1. Data/panel_long.csv", "1. Data/panel_court.csv",
      "1. Data/Public data/grille_densite_2026.xlsx",
      "1. Data/Public data/TD_POP1B_2022.csv",
      list.files("2. Scripts", pattern = "\\.R$", full.names = TRUE),
      list.files("4. Rapports", pattern = "\\.(R|Rmd)$", full.names = TRUE)
    )
    utils::write.csv2(data.frame(fichier = fichiers,
      md5 = unname(tools::md5sum(fichiers))),
      file.path(dossier_trace, "empreintes_sources.csv"), row.names = FALSE)
    writeLines(capture.output(sessionInfo()), file.path(dossier_trace, "sessionInfo_analyses.txt"))
    writeLines(format(Sys.time(), tz = "UTC", usetz = TRUE),
               file.path(dossier_trace, "date_recalcul.txt"))
  } else {
    # 4. Rendu : le R Markdown orchestre le recalcul avant de créer le HTML.
    for (package in c("rmarkdown", "knitr", "DT", "ggplot2", "ggrepel")) {
      if (!requireNamespace(package, quietly = TRUE))
        stop("Installer le package manquant : ", package)
    }
    if (!rmarkdown::pandoc_available()) {
      # RStudio fournit Pandoc ; ce chemin complète sa détection sur macOS.
      candidats <- Sys.glob("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/*/pandoc")
      if (length(candidats)) Sys.setenv(RSTUDIO_PANDOC = dirname(candidats[1]))
    }
    if (!rmarkdown::pandoc_available())
      stop("Installer Pandoc ou lancer le rendu depuis RStudio.")
    rmarkdown::render("4. Rapports/AFM_dashboard.Rmd",
      params = list(recalculer = !"--rapport-seul" %in% arguments),
      quiet = TRUE, envir = new.env(parent = globalenv()))
    message("Dashboard généré : 4. Rapports/AFM_dashboard.html")
  }
})
