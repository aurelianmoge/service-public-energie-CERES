
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
