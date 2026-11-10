# Depuis la racine : source("2. Scripts/5b. AFM gestes contributifs.R")
# Représentations + trois groupes actifs G7 ; profils sociaux illustratifs.
# Documentation : 4. Rapports/AFM_gestes.md.
local({
  e <- new.env(parent = globalenv())
  sys.source("2. Scripts/AFM_gestes_fonctions.R", envir = e)
  e$executer_afm_gestes("contributif")
})
