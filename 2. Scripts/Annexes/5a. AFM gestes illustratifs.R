# Depuis la racine : source("2. Scripts/5a. AFM gestes illustratifs.R")
# Recalcule la référence dans un dossier séparé, puis projette G7 en illustratif.
# Documentation : 4. Rapports/AFM_gestes.md.
local({
  e <- new.env(parent = globalenv())
  sys.source("2. Scripts/AFM_gestes_fonctions.R", envir = e)
  e$executer_afm_gestes("illustratif")
})
