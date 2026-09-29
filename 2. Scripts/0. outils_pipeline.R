# ==============================================================================
# outils_pipeline.R  --  Les fonctions du pipeline de donnees
# ==============================================================================
# Enquete ENS / CERES - Enedis.
#
# Ce fichier ne fait RIEN tout seul : il ne contient que des definitions de
# fonctions appelée par pipeline.R, dans l'ordre.
#
# PLAN
#   A. Lecture des fichiers et dictionnaire de variables
#   B. Audit de qualite et nettoyage
#   C. Comparaison aux references de population
#   D. Recodages des reponses
#   E. Indice d'engagement
# ==============================================================================

library(readr)     # lire des fichiers        (read_csv)
library(dplyr)     # manipuler des tableaux   (filter, mutate, select...)
library(stringr)   # manipuler du texte       (str_replace, str_detect...)
library(tibble)    # le type de tableau moderne de R


# ##############################################################################
# A. LECTURE DES FICHIERS ET DICTIONNAIRE
# ##############################################################################

# --- lire_vague() -------------------------------------------------------------
# Lit un CSV brut de Bilendi et renvoie DEUX choses : les donnees avec des noms
# de colonnes utilisables, et le dictionnaire qui conserve le texte complet des
# questions.
#
# TROIS PRECAUTIONS DE LECTURE, chacune correspond a un vrai piege du fichier :
#
#   locale(encoding = "UTF-8")
#       Le fichier contient des accents et commence par un "BOM" (3 octets
#       invisibles). Sans ca, la premiere colonne s'appellerait
#       "i>>?IdBilendi" et les accents seraient illisibles.
#
#   col_types = cols(.default = col_character())
#       On interdit a readr de deviner le type des colonnes : TOUT est lu comme
#       du texte. C'est volontaire. Les reponses melangent chiffres et mots
#       ("1 - pas interesse", "3", "Ne sait pas"), donc laisser R deviner
#       produirait des NA silencieux. La conversion sera effectuée manuellement par la suite
#
#   name_repair = "minimal"
#       Empeche readr de "nettoyer" les noms de colonnes tout seul.
#
# (Les sauts de ligne a l'interieur des reponses libres sont geres tout seuls
#  par read_csv, du moment que les champs sont entre guillemets, ce qui est le cas.)
#
# VALEUR RENVOYEE : une liste a deux elements, qu'on recupere avec $
#     v1 <- lire_vague("../Data/COMPLETE1.csv")
#     v1$donnees   le tableau
#     v1$dico      le dictionnaire

lire_vague <- function(chemin, nom = basename(chemin)) {
  
  # stop()
  # interrompt avec un message clair si le chemin n'est pas correct au lieu d'une erreur obscure.
  if (!file.exists(chemin)) {
    stop("Fichier introuvable : ", normalizePath(chemin, mustWork = FALSE))
  }
  
  brut <- read_csv(
    chemin,
    locale      = locale(encoding = "UTF-8"),
    col_types   = cols(.default = col_character()),
    name_repair = "minimal"
  )
  
  # --- Separer le code du texte de la question ---
  # Les noms de colonnes ont tous la meme forme :
  #     "G1Q00002[SQ001]. Chez vous, quel est le nombre de : [Enfants]"
  #      \______________/  \_________________________________________/
  #           le code                 le texte de la question
  #
  # La coupe s'effectue au premier ". ". sub() remplace la 1re occurrence d'un motif.
  noms_bruts <- names(brut)
  
  code     <- str_squish(sub("^([^.]*)\\..*$", "\\1", noms_bruts))
  question <- str_squish(sub("^[^.]*\\.",      "",    noms_bruts))
  
  # "G1Q00007[SQ001]" devient "G1Q00007_SQ001".
  nom_court <- code |>
    str_replace_all("\\[", "_") |>            # [ devient _
    str_replace_all("\\]", "")  |>            # ] disparait
    str_replace_all("[^A-Za-z0-9_]", "_")
  
  # make.unique() ajoute un suffixe si deux colonnes finissaient avec le meme
  # nom.
  nom_court <- make.unique(nom_court, sep = "_")
  
  donnees <- brut
  names(donnees) <- nom_court
  
  cat(nom, ":", nrow(donnees), "lignes x", ncol(donnees), "colonnes |",
      n_distinct(donnees$IdBilendi), "identifiants uniques\n")
  
  list(
    donnees = donnees,
    dico    = tibble(
      variable = nom_court,   # le nom qu'on utilisera dans le code
      code     = code,        # le code d'origine LimeSurvey
      question = question     # le texte integral de la question
    )
  )
}

# --- enrichir_dico() ----------------------------------------------------------
# Ajoute deux colonnes au dictionnaire :
#
#   item      : ce qui etait entre crochets a la fin du libelle. Dans ce
#               questionnaire, les crochets contiennent la ligne d'un tableau
#               ("[Enfants]", "[RTE]") ou le champ libre "[Autre]". Les separer
#               permet de regrouper les colonnes d'une meme question.
#
#   modalites : les reponses effectivement presentes dans les donnees, sous
#               forme de VECTEUR. Une colonne qui contient des vecteurs plutot
#               que des valeurs simples s'appelle une "list-column", chaque
#               case pouvant contenir 2, 5 ou 7 elements.
#
# L'argument sans_liste permet d'ajouter, vague par vague, des colonnes qui ne
# proposaient pas de liste de choix.G1Q00002 (l'age) est une
# liste en vague 1 ("Autre" / "Ne sait pas") mais une saisie au clavier en
# vague 2 (72 valeurs). Une meme variable peut donc changer de nature d'une
# vague a l'autre.

enrichir_dico <- function(dico, donnees, sans_liste = character(0)) {
  
  # --- 0. Garde-fou : refuser d'enrichir deux fois ---
  # Cette fonction n'est PAS repetable. Au 1er passage elle retire les crochets
  # de "question", au 2e elle chercherait des crochets qui n'existent plus, ne
  # trouverait rien;
  if ("item" %in% names(dico)) {
    stop("Ce dictionnaire a deja ete enrichi (colonne 'item' presente).\n",
         "  Relancez pipeline.R depuis le debut plutot que ce bloc seul.")
  }
  
  # --- 1. Separer le contenu des crochets ---
  # str_match() renvoie une matrice : la colonne 1 contient tout ce qui a ete
  # trouve, la colonne 2 le contenu du groupe entre parentheses. D'ou le [, 2].
  # Le motif "\\[([^\\]]*)\\]\\s*$" se lit : un crochet ouvrant, puis tout ce
  # qui n'est pas un crochet fermant (et qu'on memorise), puis un crochet
  # fermant, en FIN de chaine.
  # Les libelles sans crochets renvoient NA.
  dico <- dico |>
    mutate(
      item     = str_match(question, "\\[([^\\]]*)\\]\\s*$")[, 2],
      question = str_squish(str_remove(question, "\\s*\\[[^\\]]*\\]\\s*$"))
    )
  
  # --- 2. Relever les modalites proposees au repondant ---
  # REGLE : on n'inventorie que les listes de choix, c'est-a-dire les questions
  # ou la personne n'avait qu'a CLIQUER. Tout ce qui a ete tape au clavier
  # recoit NA : une saisie libre n'a pas de modalites.
  #
  # Les champs concernes se reconnaissent a leur nom :
  #   - tout ce qui finit par "_other" : les "[Autre]" du questionnaire ;
  #   - G8Q00001 : le commentaire final ;
  #   - IdBilendi, STARTTIME, DUREE : des colonnes techniques, jamais vues
  #     par le repondant.
  # Plus, le cas echeant, ce que l'appelant ajoute via sans_liste.
  
  champs_libres <- c("IdBilendi", "STARTTIME", "DUREE", "G8Q00001", sans_liste)
  
  dico$modalites <- lapply(dico$variable, function(v) {
    
    if (!v %in% names(donnees))     return(NA_character_)
    if (str_detect(v, "_other$"))   return(NA_character_)
    if (v %in% champs_libres)       return(NA_character_)
    
    # sort(unique(...)) : les valeurs distinctes, triees, sans les NA (sort()
    # les ecarte par defaut). L'ordre est alphabetique (donc "10 000EUR" arrive
    # avant "400EUR", c'est un inventaire).
    # Une liste de choix restée sans reponse renvoie un vecteur vide.
    sort(unique(donnees[[v]]))
  })
  
  dico
}


# --- aplatir_modalites() et compter_modalites() -------------------------------
# Un CSV est une grille plate : une seule valeur par case. Or "modalites" est
# une list-column, ou chaque case contient un vecteur entier. Il faut donc
# APLATIR la colonne avant d'exporter.
#
#   vecteur : "Autre"  "Femme"  "Homme"  "Ne sait pas..."
#   chaine  : "Autre | Femme | Homme | Ne sait pas..."

aplatir_modalites <- function(x) {
  if (length(x) == 0)             return("")            # liste sans reponse
  if (length(x) == 1 && is.na(x)) return(NA_character_) # saisie libre
  paste(x, collapse = " | ")                            # liste de choix
}

# Le nombre de modalites est plus commode à filtrer dans Excel que la chaine.
compter_modalites <- function(x) {
  if (length(x) == 1 && is.na(x)) NA_integer_ else length(x)
}


# --- exporter_dico() ----------------------------------------------------------
# Ecrit un dictionnaire en CSV, apres aplatissement.
#
# write_excel_csv() plutot que write_csv() : il ajoute le "BOM" en tete du
# fichier, ce qui evite qu'Excel sous Windows affiche "A(c)" a la place de "e".

exporter_dico <- function(dico, chemin) {
  dico |>
    mutate(
      n_modalites = sapply(modalites, compter_modalites),
      modalites   = sapply(modalites, aplatir_modalites)
    ) |>
    write_excel_csv(chemin)
}

# ##############################################################################
# B. AUDIT DE QUALITE ET NETTOYAGE
# ##############################################################################
# REGLE DE CONSERVATION:
#   Un questionnaire est valide s'il remplit LES DEUX conditions :
#     - duree superieure au seuil de sa version : 120 s si le questionnaire est
#       court, 480 s s'il est long ;
#     - bonne reponse a la question piege ("4").
#   Quand une personne a repondu plusieurs fois, on conserve son dernier
#   passage validé.


# --- auditer() ----------------------------------------------------------------
# Ajoute cinq colonnes de diagnostic. Elle ne SUPPRIME rien : on calcule des
# indicateurs, le filtrage vient apres, dans le pipeline. Separer les deux
# permet de compter ce qu'on ecarte avant de l'ecarter.

auditer <- function(donnees, seuil_court = 120, seuil_long = 480) {
  
  donnees |>
    mutate(
      
      # --- Quelle version du questionnaire ? ---
      # L'embranchement se fait sur G1Q00009 ("Vous vivez actuellement :") :
      #   "Dans un logement AVEC contrat d'electricite a votre nom"  -> longue
      #   "Dans un logement SANS contrat d'electricite a votre nom"  -> courte
      version = case_when(
        str_detect(G1Q00009, "^Dans un logement avec contrat") ~ "long",
        str_detect(G1Q00009, "^Dans un logement sans contrat") ~ "court",
        .default = NA_character_
      ),
      
      # --- La duree de passation est-elle suffisante ? ---
      # DUREE a ete lue en texte : on la convertit.
      duree_sec = as.numeric(DUREE),
      
      # On stocke le seuil dans une COLONNE plutot que de l'ecrire en dur dans
      # le test.
      seuil_duree = case_when(
        version == "court" ~ seuil_court,
        version == "long"  ~ seuil_long,
        .default = NA_real_
      ),
      
      # Le test lui-meme. Resultat : TRUE, FALSE, ou NA si une valeur manque.
      duree_ok = duree_sec > seuil_duree,
      
      # --- La question piege est-elle reussie ? ---
      # La question demande explicitement de selectionner l'option numero
      # quatre. La bonne reponse est donc le libelle "4".
      
      piege_ok = !is.na(TRAP) & TRAP == "4"
    )
}


# --- a_repondu() --------------------------------------------------------------
# La personne a-t-elle reellement repondu a cette colonne ?
#
# LimeSurvey marque "question non posee" de
# DEUX facons selon le type de question :
#   - une vraie valeur manquante (NA) pour les questions a choix unique ;
#   - la CHAINE DE CARACTERES "N/A" pour les questions a cases a cocher.
#
# La seconde est traitre : is.na("N/A") vaut FALSE. Un simple is.na()
# compterait donc ces colonnes comme renseignees. Sur ce panel, 93 colonnes sur
# 234 contiennent la chaine "N/A", et l'ignorer faisait tomber la detection des
# questions sautees par la version courte de 82 variables a 22.

a_repondu <- function(x) !is.na(x) & x != "N/A"


# --- dedoublonner() -----------------------------------------------------------
# DECISION : quand une personne a repondu plusieurs fois, on conserve son
# DERNIER passage.
#
# ATTENTION AU PIEGE : group_by() considere TOUS les NA comme un seul et meme
# groupe. Applique tel quel a IdBilendi, il verrait les lignes sans identifiant
# comme des doublons les unes des autres et n'en garderait qu'une, alorrs que
# ce sont des personnes differentes. On traite donc les deux cas separement.
#
# L'ORDRE compte aussi, et c'est le pipeline qui s'en charge : on dedoublonne
# APRES les filtres de qualite.

dedoublonner <- function(donnees) {
  
  # Les lignes identifiees : on trie par date et on garde la derniere de
  # chaque identifiant.
  avec_id <- donnees |>
    filter(!is.na(IdBilendi)) |>
    arrange(as.numeric(STARTTIME)) |>
    group_by(IdBilendi) |>
    slice_tail(n = 1) |>
    ungroup()
  
  # Les lignes sans identifiant : conservees telles quelles. Leur sort est
  # traite separement, par controler_sans_id().
  sans_id <- donnees |>
    filter(is.na(IdBilendi))
  
  bind_rows(avec_id, sans_id)
}


# --- accord_lignes() et controler_sans_id() -----------------------------------
# Trois lignes du panel n'ont pas d'identifiant (2 en vague 1, 1 en vague 2).
# Faute d'identifiant, la seule facon de savoir si elles dupliquent un
# repondant deja enregistre est de comparer les REPONSES.
#
# Un score de ressemblance brut ne veut rien dire : deux inconnus se
# ressemblent deja beaucoup, parce que la plupart des questions ont une reponse
# majoritaire. On calcule donc d'abord la ressemblance entre deux repondants
# tires au hasard, qui sert de BRUIT DE FOND, et on regarde si les lignes sans
# identifiant en sortent.

# Proportion de cases identiques entre deux lignes. Deux NA comptent comme
# identiques.
accord_lignes <- function(a, b) {
  mean((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
}

controler_sans_id <- function(donnees, dico, nom) {
  
  cols <- intersect(setdiff(dico$variable,
                            c("IdBilendi", "STARTTIME", "DUREE", "TRAP")),
                    names(donnees))
  M <- as.matrix(donnees[, cols])
  
  # set.seed() fige le tirage aleatoire.
  set.seed(1)
  i1 <- sample(nrow(M), 2000, replace = TRUE)
  i2 <- sample(nrow(M), 2000, replace = TRUE)
  ok <- i1 != i2
  hasard <- mapply(function(x, y) accord_lignes(M[x, ], M[y, ]),
                   i1[ok], i2[ok])
  
  cat("\nVague", nom,
      "- reference : deux repondants au hasard se ressemblent a",
      sprintf("%.0f %%", 100 * median(hasard)), "(median),",
      sprintf("%.0f %%", 100 * max(hasard)), "au maximum\n")
  
  for (i in which(is.na(donnees$IdBilendi))) {
    sim <- apply(M, 1, function(r) accord_lignes(M[i, ], r))
    sim[i] <- NA
    cat("  ligne", donnees$STARTTIME[i], ": meilleure correspondance",
        sprintf("%.0f %%", 100 * max(sim, na.rm = TRUE)),
        ifelse(max(sim, na.rm = TRUE) > max(hasard),
               "<-- A EXAMINER", "(dans le bruit de fond)"), "\n")
  }
  
  invisible(NULL)
}

# ##############################################################################
# C. COMPARAISON AUX REFERENCES DE POPULATION
# ##############################################################################

# --- comparer() ---------------------------------------------------------------
# Met cote a cote la part observee dans le panel et la part theorique dans la
# population francaise, puis en deduit le poids de redressement.
#   poids > 1 : la categorie est sous-representee, il faut la "gonfler"
#   poids < 1 : elle est sur-representee, on la reduit
# Note: à retravailer avec les données d'Aurélian (chemin à créer)

comparer <- function(donnees, variable, reference, col_categorie, col_pct) {
  
  observe <- donnees |>
    filter(!is.na(.data[[variable]])) |>
    count(categorie = .data[[variable]], name = "n_panel", .drop = FALSE) |>
    mutate(pct_panel = round(100 * n_panel / sum(n_panel), 1))
  
  manquants <- setdiff(as.character(observe$categorie),
                       as.character(reference[[col_categorie]]))
  if (length(manquants) > 0) {
    stop("Categories absentes de la reference : ",
         paste(manquants, collapse = ", "),
         "\n  Verifiez que les libelles concordent des deux cotes.")
  }
  
  reference |>
    transmute(categorie  = .data[[col_categorie]],
              pct_france = .data[[col_pct]]) |>
    left_join(observe, by = "categorie") |>
    mutate(ecart = round(pct_panel - pct_france, 1),
           poids = round(pct_france / pct_panel, 3))
}


# ##############################################################################
# D. RECODAGES DES REPONSES
# ##############################################################################
# UNE PRECAUTION COMMUNE AUX QUATRE FONCTIONS:
#
# Les libelles du questionnaire contiennent des apostrophes TYPOGRAPHIQUES
# ("l'electricite") et non des apostrophes droites. Ce sont deux
# caracteres differents pour R : "d'accord" == "d'accord" renvoie FALSE si l'un
# est typographique et l'autre droit. Certains libelles contiennent en plus des
# accents et le point median de l'ecriture inclusive ("pret.e").
#
# Ecrire la reponse complete en dur dans le code, c'est prendre le risque que
# la comparaison echoue SILENCIEUSEMENT - tout le monde a zero au score, aucun
# message d'erreur.
#
# La parade, appliquee partout : ne comparer que sur un fragment de DEBUT sans
# apostrophe ni accent, choisi assez long pour etre unique. Et faire suivre
# chaque recodage, dans le pipeline, d'un tableau de controle libelle par
# libelle.


# --- role_acteur() ------------------------------------------------------------
# G3Q00002 demande, pour quatre acteurs, quel est leur role principal. C'est la
# seule question du questionnaire ou il existe une BONNE REPONSE.
#     "Transporte l'electricite a grande echelle..."  -> ^Transporte
#     "Distribue l'electricite localement..."         -> ^Distribue
#     "Produit et fournit l'electricite"              -> ^Produit

role_acteur <- function(x) {
  case_when(
    str_detect(x, "^Transporte") ~ "transport",
    str_detect(x, "^Distribue")  ~ "distribution",
    str_detect(x, "^Produit")    ~ "production",
    str_detect(x, "^Ne sait")    ~ "nsp",
    .default = NA_character_     # vraie non-reponse
  )
}


# --- milieu_tranche() ---------------------------------------------------------
# Convertit une tranche de revenu en un nombre : son POINT MILIEU.
# "De 1200 a moins de 1500EUR" -> 1350.
#
# C'est une approximation assumee : on ne connait pas le revenu exact.
#
# Deux tranches n'ont pas de milieu, parce qu'elles sont ouvertes :
#   "Moins de 400EUR"   -> pas de borne basse
#   "10 000EUR ou plus" -> pas de borne haute
# Conventions retenues :
#   tranche basse ouverte : 0,75 x la borne connue  ->  300
#   tranche haute ouverte : 1,5  x la borne connue  ->  15 000
#
# Note: fonction désormais inutile et à retravailler. Suivant la réunion du 24/09:
# on évite les choix arbitraires de nombres pour garder les résultats en intervalles.
# En conséquence, le niveau de vie sera aussi déterminé selon un intervalle.
#
# Effet secondaire : "Ne sait pas" et "Menage individuel" ne
# contiennent aucun chiffre, donc ils deviennent NA tout seuls.
# Question : devrait-on traiter "ménage individuel" comme NA ?

milieu_tranche <- function(x) {
  
  # "10 000EUR" -> "10000EUR". Le motif "(\\d)\\s(\\d)" cible un espace situe
  # ENTRE deux chiffres ; \\s couvre l'espace normal comme l'insecable.
  propre <- str_replace_all(x, "(\\d)\\s(\\d)", "\\1\\2")
  
  # str_extract_all renvoie une LISTE.
  nombres <- str_extract_all(propre, "\\d+")
  
  premier <- function(v, i) if (length(v) >= i) v[i] else NA
  
  borne_1 <- as.numeric(sapply(nombres, premier, 1))
  borne_2 <- as.numeric(sapply(nombres, premier, 2))
  
  case_when(
    !is.na(borne_2)       ~ (borne_1 + borne_2) / 2,  # tranche fermee
    is.na(borne_1)        ~ NA_real_,                 # aucun chiffre
    str_detect(x, "plus") ~ borne_1 * 1.5,            # "ou plus"
    .default              = borne_1 * 0.75            # "moins de"
  )
}


# --- en_accord() --------------------------------------------------------------
# Convertit une echelle d'accord en note de 1 a 5.
#
# Seule subtilite : "Plutot d'accord" et "Plutot pas d'accord" commencent
# pareil. On les separe par la presence du mot "pas", et l'ordre des lignes de
# case_when() compte.
#
# Deux modalites restent volontairement sans note :
#   "Ne sait pas"                 -> non-reponse
#   "Je ne recois pas la facture" -> personne non concernee
# Ce ne sont pas les memes NA, et le pipeline les compte donc séparement.

en_accord <- function(x) {
  case_when(
    str_detect(x, "^Tout")                        ~ 5,
    str_detect(x, "^Plut") & str_detect(x, "pas") ~ 2,
    str_detect(x, "^Plut")                        ~ 4,
    str_detect(x, "^Ni ")                         ~ 3,
    str_detect(x, "^Pas du tout")                 ~ 1,
    .default = NA_real_
  )
}


# --- type_geste() -------------------------------------------------------------
# G7Q00001 propose neuf gestes, avec cinq reponses possibles. On les ramene a
# cinq etiquettes courtes.
#   "^D"                 -> "Deja fait"   (seul libelle commencant par D)
#   "^Jamais fait. Pas"  -> refus
#   "^Jamais"            -> pret a faire  (apres le test precedent)
#   "^Non"               -> non concerne
#   "^Ne sait"           -> ne sait pas
#
# Distinguer "fait", "pret" et "non_concerne" est ce qui permettra de separer
# la volonte d'agir de la capacite d'agir : un locataire ne peut pas isoler son
# logement, il repondra "non concerne" ou "pret a faire", jamais "deja fait".

type_geste <- function(x) {
  case_when(
    str_detect(x, "^D")                ~ "fait",
    str_detect(x, "^Jamais fait. Pas") ~ "refus",
    str_detect(x, "^Jamais")           ~ "pret",
    str_detect(x, "^Non")              ~ "non_concerne",
    str_detect(x, "^Ne sait")          ~ "nsp",
    .default = NA_character_
  )
}

# ##############################################################################
# E. INDICE D'ENGAGEMENT
# ##############################################################################
# Ces mesures ne portent sur aucun theme du questionnaire : elles decrivent la
# MANIERE dont la personne y a repondu. Elles servent de controle.
#
# REGLE : variables de CONTROLE, jamais criteres de tri. Le nettoyage a deja eu
# lieu (question piege et seuil de duree).


# --- taux_remplissage() -------------------------------------------------------
# Pour chaque batterie de cases a cocher : quelle part de ses places la
# personne a-t-elle utilisee ?
#
# Le plafond change d'une question a l'autre (2 pour G6Q00005, 5 pour
# G4Q00004). Un nombre brut de cases n'est donc pas comparable entre batteries :
# on rapporte au plafond.
#
# Le plafond est LU DANS LES DONNEES (le maximum observe) plutot qu'ecrit a la
# main. Avec 2839 repondants, quelqu'un a forcement utilise toutes ses places.
#
# Renvoie une MATRICE : une ligne par repondant, une colonne par batterie.

taux_remplissage <- function(donnees, prefixes) {
  sapply(prefixes, function(p) {
    items <- grep(paste0("^", p, "_SQ"), names(donnees), value = TRUE)
    if (length(items) == 0) stop("Batterie introuvable : ", p)
    m  <- as.matrix(donnees[items])
    nb <- rowSums(m == "Oui", na.rm = TRUE)
    nb / max(nb)
  })
}


# --- profondeur_classements() -------------------------------------------------
# Combien de rangs la personne a-t-elle renseignes, rapporte au maximum
# autorise ?
#
# G4Q00002 accepte 5 rangs, G4Q00003 en accepte 3. Les colonnes au-dela sont
# entierement vides et doivent etre ecartees.
#
# NUANCE A GARDER EN TETE : une personne qui repond "ne sait pas" au rang 1 n'a
# aucun rang suivant, donc une profondeur de 1. L'indice ne fait pas la
# difference: c'est une limite a assumer.

profondeur_classements <- function(donnees, prefixes) {
  sapply(prefixes, function(p) {
    cols <- grep(paste0("^", p, "_[0-9]$"), names(donnees), value = TRUE)
    if (length(cols) == 0) stop("Classement introuvable : ", p)
    m  <- as.matrix(donnees[cols])
    m[!a_repondu(m)] <- NA          # neutralise NA, "" et la chaine "N/A"
    nb <- rowSums(!is.na(m))
    nb / max(nb)
  })
}


# --- engagement() -------------------------------------------------------------
# L'indice lui-meme : la moyenne de trois composantes, toutes ramenees entre 0
# et 1 pour qu'aucune ne pese plus qu'une autre du simple fait de son echelle.
#     - le remplissage moyen des batteries
#     - la profondeur moyenne des classements
#     - l'inverse du taux de "ne sait pas"
#
# L'ARGUMENT `sauf` EST LE COEUR DE LA FONCTION. Pour analyser les
# mono-cocheurs d'une batterie, on veut savoir s'ils sont par ailleurs engages. Mais
# si l'indice contient deja le remplissage de CETTE batterie, on expliquerait
# le nombre de cases cochees par une variable qui le contient.
#
#   engagement(tx, pr, nsp)                     l'indice complet
#   engagement(tx, pr, nsp, sauf = "G3Q00005")  le meme, sans cette batterie
#
# LA DUREE DE REPONSE N'ENTRE PAS DANS L'INDICE. La
# validation utile passe par G8Q00002, ou les repondants notent le
# questionnaire.
#
# A ASSUMER : indicateur composite utile comme controle.

engagement <- function(taux_batteries, profondeur, taux_nsp, sauf = NULL) {
  
  if (!is.null(sauf)) {
    inconnues <- setdiff(sauf, colnames(taux_batteries))
    if (length(inconnues) > 0) {
      stop("Batterie inconnue : ", paste(inconnues, collapse = ", "),
           "\n  Attendu : ", paste(colnames(taux_batteries), collapse = ", "))
    }
  }
  
  gardees <- setdiff(colnames(taux_batteries), sauf)
  
  rowMeans(
    cbind(rowMeans(taux_batteries[, gardees, drop = FALSE], na.rm = TRUE),
          rowMeans(profondeur, na.rm = TRUE),
          1 - taux_nsp),
    na.rm = TRUE
  )
}
