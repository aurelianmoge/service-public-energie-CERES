# Depuis la racine : source("4. Rapports/generer_dashboard_gestes.R")
# Terminal : Rscript --vanilla '4. Rapports/generer_dashboard_gestes.R'
# --rapport-seul : rendu des sorties existantes ; --analyses : recalcul seul.
local({
  args <- commandArgs(trailingOnly = TRUE)
  if ("--analyses" %in% args) {
    i <- match("--analyses", args)
    if (length(args) > i) setwd(args[i+1])
  }
  if (!file.exists("service-public-energie.Rproj")) stop("Lancer depuis la racine du projet.")
  if ("--analyses" %in% args) {
    source("2. Scripts/5a. AFM gestes illustratifs.R", encoding = "UTF-8")
    source("2. Scripts/5b. AFM gestes contributifs.R", encoding = "UTF-8")
  } else {
    for (p in c("rmarkdown", "knitr", "DT", "ggplot2", "ggrepel"))
      if (!requireNamespace(p, quietly = TRUE)) stop("Installer ", p)
    if (!rmarkdown::pandoc_available()) {
      candidats <- Sys.glob("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/*/pandoc")
      if (length(candidats)) Sys.setenv(RSTUDIO_PANDOC = dirname(candidats[1]))
    }
    if (!rmarkdown::pandoc_available()) stop("Pandoc manquant : utiliser RStudio ou installer Pandoc.")
    rmarkdown::render("4. Rapports/AFM_gestes_dashboard.Rmd",
      params = list(recalculer = !"--rapport-seul" %in% args),
      envir = new.env(parent = globalenv()), quiet = TRUE)
    message("Dashboard généré : 4. Rapports/AFM_gestes_dashboard.html")
  }
})
