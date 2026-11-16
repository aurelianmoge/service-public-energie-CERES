# Fonctions de mise en forme des tableaux LaTeX
#
# Ces fonctions reprennent les conventions des tableaux d'Antoine dans le
# document Overleaf ("Enquête Quanti - Énergie - Doc partagé"), pour que tous les
# tableaux du projet se ressemblent :
#   - nombres à la française : virgule décimale, espace fine entre les milliers
#     (2\,759), vrai signe moins ($-$0,13) ;
#   - étoiles de significativité avec la commande \sym{} du préambule, qui ne
#     décale pas l'alignement des nombres ;
#   - coefficient sur une ligne, écart-type entre parenthèses sur la suivante ;
#   - p-valeurs à trois décimales, "< 0,001" en dessous ;
#   - tableau dans un environnement threeparttable, avec une note complète
#     (méthode, champ, références, seuils) en \footnotesize.
#
# Le préambule du document doit contenir booktabs, threeparttable et la commande
# \newcommand{\sym}[1]{\rlap{$^{#1}$}} (c'est le cas du main.tex d'Antoine).
#
# Ce script ne fait que définir des fonctions : on le charge au début des
# scripts qui écrivent des tableaux, avec source("2. Scripts/0. Mise en forme LaTeX.R").


#########################
### Les nombres       ###
#########################

# Un nombre à la française, avec "dec" décimales. Les valeurs manquantes
# deviennent des cellules vides. Un nombre qui s'arrondit à zéro n'a pas de signe
# moins (on écrit 0,00 et non -0,00).
nombre <- function(x, dec = 2) {
  arrondi <- round(x, dec)
  texte <- formatC(abs(arrondi), format = "f", digits = dec)
  entier <- sub("\\..*$", "", texte)
  entier <- gsub("(\\d)(?=(\\d{3})+$)", "\\1\\\\,", entier, perl = TRUE)
  texte <- if (dec > 0) paste0(entier, ",", sub("^[^.]*\\.", "", texte)) else entier
  texte <- paste0(ifelse(!is.na(arrondi) & arrondi < 0, "$-$", ""), texte)
  texte[is.na(x)] <- ""
  texte
}

# Un effectif : un nombre entier (les effectifs pondérés sont arrondis)
effectif <- function(n) nombre(n, 0)

# Une p-valeur
p_valeur <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "$<$\\,0,001", nombre(p, 3)))
}

# Une p-valeur dans une phrase : "$p$ = 0,046" ou "$p$ < 0,001"
p_texte <- function(p) {
  ifelse(p < 0.001, "$p$ $<$\\,0,001", paste0("$p$ = ", nombre(p, 3)))
}

# Les étoiles, aux seuils de 5 %, 1 % et 0,1 %
etoiles <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "\\sym{***}", ifelse(p < 0.01, "\\sym{**}",
                                                           ifelse(p < 0.05, "\\sym{*}", ""))))
}

# Un coefficient et ses étoiles, son écart-type entre parenthèses
coefficient <- function(estimation, p, dec = 2) paste0(nombre(estimation, dec), etoiles(p))
ecart_type  <- function(se, dec = 2) ifelse(is.na(se), "", paste0("(", nombre(se, dec), ")"))

# Un intervalle de confiance entre crochets
intervalle <- function(bas, haut, dec = 2) {
  ifelse(is.na(bas), "", paste0("[", nombre(bas, dec), " ; ", nombre(haut, dec), "]"))
}

# La phrase qui termine les notes des tableaux de régression
LEGENDE_ETOILES <- "$^{*}\\,p<0{,}05$ ; $^{**}\\,p<0{,}01$ ; $^{***}\\,p<0{,}001$."


#########################
### Le texte          ###
#########################

# Les caractères spéciaux de LaTeX dans un libellé (modalités, noms de classes)
echapper <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%#_$])", "\\\\\\1", x)
  x <- gsub("²", "\\textsuperscript{2}", x, fixed = TRUE)
  x <- gsub("(\\d) (\\d{3})", "\\1\\\\,\\2", x)   # 1 500 -> 1\,500
  x <- gsub(" €", "\\,€", x, fixed = TRUE)
  x
}

# En italique (les p-valeurs des tableaux descriptifs, les "ne sait pas")
italique <- function(x) ifelse(x == "", "", paste0("\\textit{", x, "}"))


#########################
### Les lignes        ###
#########################

# Une ligne de tableau : les cellules séparées par &, terminée par \\
ligne <- function(...) paste0(paste(c(...), collapse = " & "), " \\\\")

# Une ligne de titre de groupe, sur toute la largeur ("Profil", "Axe 1"...)
groupe <- function(titre, n_colonnes) {
  paste0("\\multicolumn{", n_colonnes, "}{l}{", titre, "} \\\\")
}

# Un en-tête de colonne sur deux lignes ("Conditions" / "de vie")
deux_lignes <- function(haut, bas) {
  paste0("\\begin{tabular}[b]{@{}c@{}}", haut, "\\\\", bas, "\\end{tabular}")
}

# Un libellé en retrait, sous un titre de groupe
retrait <- function(x) paste0("\\quad ", x)

# Un peu d'espace entre deux groupes de lignes
espace <- function(taille = "3pt") paste0("\\addlinespace[", taille, "]")

# Deux lignes pour une variable d'un tableau de régression : le coefficient
# (avec ses étoiles), puis l'écart-type entre parenthèses
lignes_coefficients <- function(libelle, estimations, p, se, dec = 2) {
  c(ligne(libelle, coefficient(estimations, p, dec)),
    ligne("", ecart_type(se, dec)))
}


#########################
### Le tableau        ###
#########################

# Assemble un tableau complet et l'écrit dans un fichier .tex, à inclure dans le
# document avec \input{}.
#   colonnes : la spécification des colonnes du tabular ("l*{4}{c}"...) ;
#   entete   : la ou les lignes d'en-tête (avant le \midrule) ;
#   corps    : les lignes du tableau ;
#   note     : le texte de la note (une seule chaîne) ;
#   taille   : "footnotesize" (par défaut) ou "scriptsize" pour les grands
#              tableaux ;
#   espacement : l'écart entre colonnes (\tabcolsep), si on veut le réduire.
# La fonction renvoie aussi les lignes, pour les relire dans la console.
ecrire_tableau <- function(fichier, titre, etiquette, colonnes, entete, corps, note,
                           taille = "footnotesize", espacement = NULL) {
  lignes <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", titre, "}"),
    paste0("\\label{tab:", etiquette, "}"),
    paste0("\\", taille),
    if (!is.null(espacement)) paste0("\\setlength{\\tabcolsep}{", espacement, "}"),
    "\\begin{threeparttable}",
    paste0("\\begin{tabular}{", colonnes, "}"),
    "\\toprule",
    entete,
    "\\midrule",
    corps,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste0("\\item ", note),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  writeLines(lignes, fichier, useBytes = FALSE)
  invisible(lignes)
}
