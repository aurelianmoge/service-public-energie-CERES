# Relancer toute l'analyse et produire la synthèse des résultats
#
# Ce script ne fait aucun calcul lui-même : il lance les scripts dans l'ordre,
# puis tricote la synthèse (4. Rapports/Synthese_resultats.Rmd). Il suffit donc
# de le lancer pour tout reproduire à partir des données brutes de "1. Data".
#
# Chaque script tourne dans son propre environnement, comme s'il était lancé
# seul : aucun objet ne passe d'un script à l'autre, tout passe par les
# fichiers enregistrés dans "3. Outputs".
#
# Le script 7 (Antoine) n'est pas relancé : il est indépendant et la synthèse
# n'utilise pas ses sorties. Les scripts 1, 2 et 4 sont lancés par le script 3.
#
# À lancer depuis la racine du projet (ouvrir le .Rproj dans RStudio). Compter
# deux à trois minutes.


#################################
### Les scripts, dans l'ordre ###
#################################

scripts <- c("3. Recodages.R",
             "5a. ACM service public.R",
             "5b. ACP choix de transition.R",
             "5c. ACM gestes.R",
             "5d. ACM marche et offre verte.R",
             "5e. AFM combinaisons des blocs.R",
             "5f. Coherence entre blocs.R",
             "5g. Typologies.R",
             "6. Regressions.R",
             "8. Interactions et prolongements.R")

for (s in scripts) {
  message("--- ", s)
  source(file.path("2. Scripts", s), local = new.env(), encoding = "UTF-8")
}


###############################
### La synthèse             ###
###############################

# Le document complet, à ouvrir dans un navigateur
rmarkdown::render("4. Rapports/Synthese_resultats.Rmd", quiet = TRUE)

# La même chose sans en-tête HTML, pour la version en ligne (artifact Claude) :
# c'est ce fichier qu'on publie
rmarkdown::render("4. Rapports/Synthese_resultats.Rmd",
                  output_format = rmarkdown::html_fragment(self_contained = TRUE),
                  output_file = "Synthese_resultats_en_ligne.html", quiet = TRUE)

# Les deux fichiers HTML ne sont pas versionnés (.gitignore) : ils se
# régénèrent avec ce script.
#
# Hors de RStudio (Rscript dans un terminal), rmarkdown a besoin de pandoc. Sur
# ce Mac, on peut utiliser celui de RStudio en définissant d'abord :
# Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64")
