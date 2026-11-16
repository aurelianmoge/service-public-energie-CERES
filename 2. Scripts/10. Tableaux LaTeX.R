# Les tableaux LaTeX des analyses factorielles, des typologies et des
# prolongements des régressions, au format du document Overleaf d'Antoine
#
# Ce script ne refait aucune analyse : il relit les sorties enregistrées par les
# scripts 5a à 5g, 6 et 8 dans "3. Outputs", et les met en forme avec les
# fonctions du script 0. Il faut donc avoir lancé ces scripts avant (le script 9
# les relance tous).
#
# Les tableaux sont écrits dans "5. Overleaf/tableaux Aurélian", un fichier .tex
# par tableau.
#
# Le champ est celui de tout le projet : les 3185 répondants dont le sexe et la
# zone sont connus (script 3), dont 2770 titulaires d'un contrat ("longs").


###############################
### Chargement des packages ###
###############################

library(tidyverse)
library(survey)

source("2. Scripts/0. Mise en forme LaTeX.R", encoding = "UTF-8")


###############################
### Importation des données ###
###############################

sorties <- "3. Outputs"
af      <- file.path(sorties, "Analyses factorielles")
reg     <- file.path(sorties, "Régressions")
lire    <- function(...) read_csv2(file.path(...), show_col_types = FALSE)

dossier <- "5. Overleaf/tableaux Aurélian"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
tex <- function(nom) file.path(dossier, paste0(nom, ".tex"))

df <- readRDS(file.path(sorties, "df_recode.rds")) |> filter(champ_commun)

acm_sp     <- readRDS(file.path(af, "1. Service public", "acm.rds"))
acp_tr     <- readRDS(file.path(af, "2. Transition", "acp.rds"))
acm_gestes <- readRDS(file.path(af, "3. Gestes", "acm.rds"))
acm_marche <- readRDS(file.path(af, "4. Marché et offre verte", "acm.rds"))

# Le pourcentage d'inertie de chaque axe : taux modifiés de Benzécri pour les ACM
# (comme dans les scripts 5a, 5c et 5d), inertie brute pour l'ACP
inertie <- list(
  sp     = lire(af, "1. Service public", "01_inertie.csv")$taux_benzecri,
  tr     = lire(af, "2. Transition", "01_inertie.csv")$inertie,
  gestes = lire(af, "3. Gestes", "01_inertie.csv")$taux_benzecri,
  marche = lire(af, "4. Marché et offre verte", "01_inertie.csv")$taux_benzecri
)
pct_axe <- function(analyse, k) paste0(nombre(inertie[[analyse]][k], 0), "~\\%")


###########################################
### 1. Les quatre analyses factorielles ###
###########################################

### Tableau : les axes retenus

analyses <- tribble(
  ~code,    ~nom,                                   ~methode, ~champ,   ~objet,
  "sp",     "Conceptions du service public",        "ACM",    "tous",   acm_sp,
  "tr",     "Choix pour la transition",             "ACP",    "tous*",  acp_tr,
  "gestes", "Gestes de maîtrise de l'énergie",      "ACM",    "longs",  acm_gestes,
  "marche", "Marché et offre verte",                "ACM",    "longs",  acm_marche
)
n_actives <- function(objet) if (inherits(objet, "MCA")) length(objet$call$quali) else nrow(objet$var$coord)

corps <- map_chr(seq_len(nrow(analyses)), function(i) {
  a <- analyses[i, ]
  ligne(a$nom, a$methode, a$champ, effectif(nrow(a$objet[[1]]$call$X)), n_actives(a$objet[[1]]),
        nombre(inertie[[a$code]][1:3], 1))
})

ecrire_tableau(
  tex("af_axes"),
  titre = "Les quatre analyses factorielles",
  etiquette = "af_axes",
  colonnes = "lllrr*{3}{c}",
  entete = c("& & & & & \\multicolumn{3}{c}{Inertie (\\%)} \\\\",
             "\\cmidrule(lr){6-8}",
             ligne("", "Méthode", "Champ", "$n$", "Variables", "Axe 1", "Axe 2", "Axe 3")),
  corps = corps,
  note = paste(
    "ACM : analyses des correspondances multiples spécifiques, où les réponses \\og ne sait pas\\fg{}",
    "sont passives. Leur inertie est donnée en taux modifiés de Benzécri, qui corrigent la sous-estimation",
    "des premiers axes propre à l'ACM. ACP : analyse en composantes principales normée sur les points",
    "des deux classements (inertie brute). Individus pondérés. Champ : tous les répondants du champ commun,",
    "ou les titulaires d'un contrat (\\og longs\\fg{}). * Ceux qui ont donné un premier choix dans les deux classements.",
    "Axes interprétés : 1 à 3 pour le service public et les gestes, 1 et 2 pour la transition et le marché."
  )
)

### Tableaux : les modalités qui font chaque axe (les trois ACM)

# Le sens de chaque axe, d'après la lecture des scripts 5a, 5c et 5d : ce qui
# est du côté négatif, puis du côté positif
sens_axes <- list(
  sp = c("Public et national (--) ou marché et décentralisation (+)",
         "Définition large et consensuelle (--) ou étroite et statutaire (+)",
         "Concurrence (--) ou entreprises publiques et monopole (+)"),
  gestes = c("Le refus : \\og pas prêt\\fg{}, quel que soit le geste (+)",
             "Investissements faits ou envisagés (--) ou \\og non concerné\\fg{} (+)",
             "Déjà fait (--) ou prêt à faire (+)"),
  marche = c("Fidélité et scepticisme (--) ou entrée dans le marché (+)",
             "Défiance (--) ou confiance (+)")
)

tableau_modalites <- function(code, dossier_af, titre, champ) {
  contributions <- lire(af, dossier_af, "02_contributions_modalites.csv")
  axes <- seq_along(sens_axes[[code]])
  corps <- unlist(map(axes, function(k) {
    m <- contributions |> filter(axe == k) |> arrange(coordonnee)
    c(if (k > 1) espace("4pt"),
      groupe(paste0("\\textbf{Axe ", k, " (", pct_axe(code, k), ")} : ", sens_axes[[code]][k]), 4),
      pmap_chr(m, function(modalite, coordonnee, contribution, cos2, ...) {
        ligne(retrait(echapper(modalite)), nombre(coordonnee, 2), nombre(contribution, 1), nombre(cos2, 2))
      }))
  }))
  ecrire_tableau(
    tex(paste0("af_", code)),
    titre = titre,
    etiquette = paste0("af_", code),
    colonnes = "l*{3}{c}",
    entete = ligne("", "Coordonnée", "Contribution (\\%)", "cos\\textsuperscript{2}"),
    corps = corps,
    note = paste0(
      "ACM spécifique pondérée (", champ, "). Modalités dont la contribution à l'axe dépasse la contribution ",
      "moyenne, rangées selon leur coordonnée : les coordonnées négatives d'un côté de l'axe, les positives de ",
      "l'autre. Inertie de l'axe en taux modifiés de Benzécri. Le cos\\textsuperscript{2} mesure la qualité de ",
      "représentation de la modalité sur l'axe."
    )
  )
}

tableau_modalites("sp", "1. Service public",
                  "Les axes de l'ACM des conceptions du service public",
                  "3\\,185 répondants")
tableau_modalites("gestes", "3. Gestes",
                  "Les axes de l'ACM des gestes de maîtrise de l'énergie",
                  "2\\,770 titulaires d'un contrat")
tableau_modalites("marche", "4. Marché et offre verte",
                  "Les axes de l'ACM du rapport au marché et à l'offre verte",
                  "2\\,770 titulaires d'un contrat")

### Tableau : l'ACP des choix pour la transition

noms_options <- c(
  lev_efficacite = "Efficacité énergétique", lev_sobriete = "Sobriété",
  lev_renouvelables = "Développer les renouvelables", lev_nucleaire = "Relancer le nucléaire",
  lev_mobilites = "Mobilités vertes", lev_reseau = "Optimiser le réseau",
  prod_nucleaire = "Nucléaire", prod_solaire = "Solaire", prod_eolien = "Éolien",
  prod_hydraulique = "Hydraulique", prod_gaz_vert = "Gaz renouvelables"
)
actives_tr <- acp_tr$call$X[, names(noms_options)]
moyennes_tr <- sapply(actives_tr, weighted.mean, w = acp_tr$call$row.w)
correlations_tr <- acp_tr$var$coord[names(noms_options), 1:2]

lignes_options <- function(variables) {
  map_chr(variables, function(v) {
    ligne(retrait(noms_options[v]), nombre(moyennes_tr[v], 2), nombre(correlations_tr[v, 1], 2),
          nombre(correlations_tr[v, 2], 2))
  })
}
ecrire_tableau(
  tex("af_tr"),
  titre = "L'ACP des choix pour la transition",
  etiquette = "af_tr",
  colonnes = "l*{3}{c}",
  entete = c(ligne("", "", "\\multicolumn{2}{c}{Corrélation avec l'axe}"),
             "\\cmidrule(lr){3-4}",
             ligne("", "Points (moyenne)", paste0("1 (", pct_axe("tr", 1), ")"), paste0("2 (", pct_axe("tr", 2), ")"))),
  corps = c(groupe("Leviers de la transition (0 à 5 points)", 4),
            lignes_options(grep("^lev_", names(noms_options), value = TRUE)),
            espace("4pt"),
            groupe("Productions prioritaires (0 à 3 points)", 4),
            lignes_options(grep("^prod_", names(noms_options), value = TRUE))),
  note = paste(
    "ACP normée pondérée sur les 2\\,806 répondants qui ont donné un premier choix dans les deux classements.",
    "Chaque option reçoit des points selon son rang : 5 au levier classé premier, 4 au deuxième, et 0 s'il",
    "n'est pas cité (de 3 à 0 pour les productions). Axe 1 : nucléaire (--) contre renouvelables (+). Axe 2 :",
    "développement de l'offre (--) contre réduction de la demande et productions pilotables douces (+).",
    "Les axes suivants ont des inerties trop proches pour être distingués."
  )
)

### Tableau : ce que les variables sociales disent des axes (eta²)

noms_illustratives <- c(
  age = "Âge", sexe = "Sexe", diplome = "Diplôme", revenu = "Revenu", activite = "Activité principale",
  zone = "Zone", statut = "Statut d'occupation", logement = "Logement", chauffage_el = "Énergie de chauffage",
  difficultes = "Difficultés à payer", connaissance = "Connaissance du réseau",
  interet_energie = "Intérêt pour l'énergie", interet_transition = "Intérêt pour la transition"
)
eta2 <- bind_rows(
  lire(af, "1. Service public", "03_eta2_illustratives.csv") |> mutate(analyse = "sp"),
  lire(af, "2. Transition", "03_eta2_illustratives.csv") |> rename_with(~ sub("\\.", " ", .x)) |>
    mutate(analyse = "tr", variable = if_else(variable == "interet", "interet_transition", variable)),
  lire(af, "3. Gestes", "03_eta2_illustratives.csv") |> mutate(analyse = "gestes"),
  lire(af, "4. Marché et offre verte", "03_eta2_illustratives.csv") |> mutate(analyse = "marche")
) |>
  mutate(variable = if_else(variable == "interet", "interet_energie", variable))

colonnes_eta2 <- tribble(~analyse, ~axe,
                         "sp", 1, "sp", 2, "sp", 3, "tr", 1, "tr", 2,
                         "gestes", 1, "gestes", 2, "gestes", 3, "marche", 1, "marche", 2)
corps <- map_chr(names(noms_illustratives), function(v) {
  valeurs <- pmap_chr(colonnes_eta2, function(analyse, axe) {
    x <- eta2[[paste("Dim", axe)]][eta2$variable == v & eta2$analyse == analyse]
    if (length(x) == 0) "" else nombre(100 * x, 1)
  })
  ligne(noms_illustratives[v], valeurs)
})
ecrire_tableau(
  tex("af_social"),
  titre = "Les axes selon le profil des répondants",
  etiquette = "af_social",
  colonnes = "l*{10}{c}",
  espacement = "4pt",
  entete = c(ligne("", "\\multicolumn{3}{c}{Service public}", "\\multicolumn{2}{c}{Transition}",
                   "\\multicolumn{3}{c}{Gestes}", "\\multicolumn{2}{c}{Marché}"),
             "\\cmidrule(lr){2-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-9}\\cmidrule(lr){10-11}",
             ligne("Axe", 1, 2, 3, 1, 2, 1, 2, 3, 1, 2)),
  corps = corps,
  note = paste(
    "Rapport de corrélation ($\\eta^2$), en \\%, entre chaque variable illustrative et chaque axe : la part",
    "de la dispersion des répondants sur l'axe qui tient à la variable. Les variables illustratives ne",
    "construisent pas les axes. Une case vide signale une variable absente de l'analyse. Gestes et marché :",
    "titulaires d'un contrat seulement, d'où l'absence du statut \\og sans contrat\\fg{}."
  ),
  taille = "scriptsize"
)


####################################
### 2. Les liens entre les blocs ###
####################################

### Tableau : les coefficients RV des deux AFM

rv_A <- lire(af, "5. AFM combinées", "A_01_coefficients_RV.csv")
rv_B <- lire(af, "5. AFM combinées", "B_01_coefficients_RV.csv")

# Le triangle inférieur d'une matrice de RV, complété par des cellules vides
# jusqu'à cinq colonnes
triangle <- function(rv) {
  groupes <- setdiff(rv$groupe, "MFA")
  map_chr(seq_along(groupes), function(i) {
    valeurs <- map_chr(1:5, function(j) {
      if (j < i) nombre(rv[[groupes[j]]][i], 2) else if (j == i) "1" else ""
    })
    ligne(retrait(paste0("(", i, ") ", groupes[i])), valeurs)
  })
}
ecrire_tableau(
  tex("afm_rv"),
  titre = "Les ressemblances entre blocs de questions (coefficients RV)",
  etiquette = "afm_rv",
  colonnes = "l*{5}{c}",
  entete = ligne("", paste0("(", 1:5, ")")),
  corps = c(groupe("\\textbf{A.} Positions sur l'énergie (tous les répondants)", 6),
            triangle(rv_A),
            espace("4pt"),
            groupe("\\textbf{B.} Représentations et pratiques (titulaires d'un contrat)", 6),
            triangle(rv_B)),
  note = paste(
    "Coefficients RV des analyses factorielles multiples (AFM), de 0 (aucune structure commune) à 1 (même",
    "structure). L'AFM équilibre les blocs, pour qu'aucun ne domine du seul fait de son nombre de questions.",
    "En A, la définition, les garants et l'organisation forment les trois volets du service public,",
    "les leviers et les productions les deux classements de la transition. En B, chaque bloc est",
    "l'une des quatre analyses du tableau~\\ref{tab:af_axes}."
  )
)

### Tableau : les corrélations entre les axes des blocs

noms_axes <- c(sp_axe1 = "SP 1 : marché", sp_axe2 = "SP 2 : déf. étroite",
               tr_axe1 = "Transition 1 : renouvelables", tr_axe2 = "Transition 2 : demande",
               gestes_axe1 = "Gestes 1 : refus", gestes_axe2 = "Gestes 2 : non concerné",
               gestes_axe3 = "Gestes 3 : prêt à faire", marche_axe1 = "Marché 1 : entrée",
               marche_axe2 = "Marché 2 : confiance", connaissance_reseau = "Connaissance du réseau",
               interet_energie = "Intérêt pour l'énergie", payer_vert = "Payer plus le vert")
correlations_axes <- lire(af, "5. AFM combinées", "C_correlations_axes.csv") |> column_to_rownames("axe")
correlations_axes <- as.matrix(correlations_axes)[names(noms_axes), names(noms_axes)]
k <- length(noms_axes)
corps <- map_chr(seq_len(k), function(i) {
  valeurs <- map_chr(seq_len(k - 1), function(j) if (j < i) nombre(correlations_axes[i, j], 2) else "")
  ligne(paste0("(", i, ") ", noms_axes[i]), valeurs)
})
ecrire_tableau(
  tex("afm_axes"),
  titre = "Les corrélations entre les axes des quatre analyses",
  etiquette = "afm_axes",
  colonnes = paste0("l*{", k - 1, "}{c}"),
  espacement = "2.5pt",
  entete = ligne("", paste0("(", seq_len(k - 1), ")")),
  corps = c(corps[1:9], espace("3pt"), corps[10:12]),
  note = paste(
    "Corrélations pondérées entre les coordonnées des répondants sur les axes, chaque fois que les deux",
    "analyses les concernent (titulaires d'un contrat pour les gestes et le marché). Le nom de chaque axe",
    "désigne son côté positif. Les trois dernières lignes sont des indicateurs simples, pour situer les axes."
  ),
  taille = "scriptsize"
)


####################################
### 3. La cohérence des opinions ###
####################################

coherence <- readRDS(file.path(af, "6. Cohérence entre blocs", "coherence.rds"))

### Tableau : les corrélations entre les treize questions

q <- coherence$questions
r <- coherence$correlations[q$variable, q$variable]
noms_courts <- c(local = "Gestion décentralisée (1-5)", concurrence = "Concurrence (1-5)",
                 solidarite = "Même service partout", garant_etat = "État garant",
                 garant_ent_privees = "Entreprises privées garantes", sp_sans_profit = "Absence de profit",
                 pro_nucleaire = "Pro-nucléaire (0-100)", lev_sobriete = "Levier : sobriété",
                 lev_renouvelables = "Levier : renouvelables", mesure_malus = "Malus",
                 mesure_bonus = "Bonus", mesure_regulation = "Régulation",
                 mesure_invest_public = "Investissement public")
k <- nrow(q)
corps <- c()
for (i in seq_len(k)) {
  if (i == 1 || q$bloc[i] != q$bloc[i - 1]) {
    corps <- c(corps, if (i > 1) espace("3pt"), groupe(paste0("\\textit{", q$bloc[i], "}"), k))
  }
  valeurs <- map_chr(seq_len(k - 1), function(j) if (j < i) nombre(r[i, j], 2) else "")
  corps <- c(corps, ligne(retrait(paste0("(", i, ") ", noms_courts[q$variable[i]])), valeurs))
}
ecrire_tableau(
  tex("coherence_correlations"),
  titre = "Les corrélations entre treize questions d'opinion",
  etiquette = "coherence_correlations",
  colonnes = paste0("l*{", k - 1, "}{c}"),
  espacement = "2pt",
  entete = ligne("", paste0("(", seq_len(k - 1), ")")),
  corps = corps,
  note = paste(
    "Corrélations pondérées, réponses \\og ne sait pas\\fg{} exclues, sur les 3\\,185 répondants du champ",
    "commun. Une seule ou deux options par classement : les options d'un même classement sont liées",
    "mécaniquement (en monter une fait descendre les autres)."
  ),
  taille = "scriptsize"
)

### Tableau : la cohérence selon le groupe

types <- c("À l'intérieur des blocs", "Entre les blocs", "SP / priorités", "SP / instruments",
           "Priorités / instruments")
noms_variables <- c(connaissance = "Connaissance du réseau", interet = "Intérêt pour l'énergie",
                    diplome_3 = "Diplôme", AGE_large = "Âge")
corps <- c(ligne("Ensemble", nombre(coherence$coherence[types], 2)), espace("4pt"))
for (v in names(noms_variables)) {
  g <- coherence$par_groupe |> filter(variable == v)
  corps <- c(corps, groupe(noms_variables[v], 6))
  for (modalite in unique(g$groupe)) {
    gg <- g |> filter(groupe == modalite) |> slice(match(types, type))
    corps <- c(corps, ligne(retrait(echapper(modalite)), nombre(gg$coherence, 2)),
               ligne("", paste0("{\\tiny ", intervalle(gg$ic_bas, gg$ic_haut, 2), "}")))
  }
  corps <- c(corps, espace("3pt"))
}
corps <- head(corps, -1)
ecrire_tableau(
  tex("coherence_groupes"),
  titre = "La cohérence des opinions selon le groupe",
  etiquette = "coherence_groupes",
  colonnes = "l*{5}{>{\\centering\\arraybackslash}p{1.9cm}}",
  espacement = "3pt",
  entete = c(ligne("", "\\multicolumn{2}{c}{Toutes les paires}", "\\multicolumn{3}{c}{Paires entre deux blocs}"),
             "\\cmidrule(lr){2-3}\\cmidrule(lr){4-6}",
             ligne("", "Dans un même bloc", "Entre deux blocs", "Service public et priorités",
                   "Service public et instruments", "Priorités et instruments")),
  corps = corps,
  note = paste(
    "Part de variance partagée en moyenne par deux questions du tableau~\\ref{tab:coherence_correlations}",
    "(r\\textsuperscript{2} moyen, en \\%), corrigée de sa valeur attendue sans aucun lien (1/($n-1$)) pour",
    "comparer des groupes de tailles différentes. Intervalles à 95~\\% par bootstrap (en dessous). Les trois",
    "blocs : la gouvernance du service public, les priorités et les instruments de la transition."
  ),
  taille = "scriptsize"
)


##############################
### 4. Les deux typologies ###
##############################

typologies <- readRDS(file.path(af, "7. Typologies", "typologies.rds"))
classes <- readRDS(file.path(af, "7. Typologies", "classes.rds"))
# Les classes sont enregistrées dans l'ordre des lignes de df (script 5g). On les
# rattache par position, et non par identifiant : trois répondants n'ont pas
# d'identifiant Bilendi.
stopifnot(identical(df$IdBilendi, classes$IdBilendi))
df <- bind_cols(df, select(classes, -IdBilendi))

ordre_classes <- list(
  classe_sp = c("Monopole public national", "État garant, mais concurrence",
                "Service public élargi", "En dehors de l'État"),
  classe_gestes = c("Non concernés", "Déjà engagés", "Prêts à faire", "Réfractaires")
)
part_classes <- map(names(ordre_classes), function(cl) {
  d <- df |> filter(!is.na(.data[[cl]]))
  x <- tapply(d$poids_norm, as.character(d[[cl]]), sum)
  100 * x[ordre_classes[[cl]]] / sum(x)
}) |> setNames(names(ordre_classes))
n_classes <- map(names(ordre_classes), function(cl) {
  table(as.character(df[[cl]]))[ordre_classes[[cl]]]
}) |> setNames(names(ordre_classes))

# Un en-tête avec le numéro et le nom des classes, sur deux lignes
entete_classes <- function(cl, avant = "", apres = NULL) {
  noms <- paste0("\\textbf{", seq_along(ordre_classes[[cl]]), ".} ", ordre_classes[[cl]])
  ligne(avant, paste0("\\multicolumn{1}{>{\\centering\\arraybackslash}p{2cm}}{", noms, "}"), apres)
}

### Tableaux : ce qui caractérise chaque classe

tableau_classes <- function(cl, code, titre, champ, n_modalites = 6) {
  desc <- typologies$descriptions |> filter(typologie == code)
  corps <- unlist(map(seq_along(ordre_classes[[cl]]), function(i) {
    nom <- ordre_classes[[cl]][i]
    m <- desc |> filter(classe == nom, v_test > 0) |> arrange(desc(v_test)) |> head(n_modalites)
    c(if (i > 1) espace("4pt"),
      groupe(paste0("\\textbf{", i, ". ", nom, "} (", nombre(part_classes[[cl]][i], 1), "~\\%)"), 4),
      pmap_chr(m, function(modalite, pct_classe, pct_ensemble, v_test, ...) {
        ligne(retrait(echapper(modalite)), nombre(pct_classe, 1), nombre(pct_ensemble, 1), nombre(v_test, 1))
      }))
  }))
  ecrire_tableau(
    tex(paste0("typo_", code)),
    titre = titre,
    etiquette = paste0("typo_", code),
    colonnes = "l*{3}{c}",
    entete = ligne("", "Dans la classe (\\%)", "Ensemble (\\%)", "Valeur-test"),
    corps = corps,
    note = paste0(
      "Classification ascendante hiérarchique (méthode de Ward, consolidée par les k-means) sur les trois ",
      "premiers axes de l'ACM, ", champ, ". Pour chaque classe, entre parenthèses sa part pondérée, puis les ",
      n_modalites, " modalités les plus surreprésentées : leur part dans la classe, dans l'ensemble, et la ",
      "valeur-test (au-delà de 2, l'écart est significatif)."
    )
  )
}
tableau_classes("classe_sp", "sp", "Quatre conceptions du service public",
                "sur les 3\\,185 répondants (tableau~\\ref{tab:af_sp})")
tableau_classes("classe_gestes", "gestes", "Quatre rapports aux gestes de maîtrise de l'énergie",
                "sur les 2\\,770 titulaires d'un contrat (tableau~\\ref{tab:af_gestes})")

### Tableau : le profil social des classes

ordre_modalites <- c("Homme", "Femme", "18-34", "35-49", "50-64", "65+",
                     "Infra-bac", "Bac", "Bac+2/3", "Bac+5", "Autre/NSP",
                     "< 1 000", "1 000-1 500", "1 500-2 500", "2 500-4 000", "4 000 et +", "NSP",
                     "Urbain", "Périurbain", "Rural", "Propriétaire", "Locataire", "Autre", "Sans contrat",
                     "0", "1", "2", as.character(1:5))
libelles_modalites <- c("18-34" = "18-34 ans", "35-49" = "35-49 ans", "50-64" = "50-64 ans",
                        "65+" = "65 ans et plus", "< 1 000" = "moins de 1 000 €",
                        "1 000-1 500" = "1 000 à 1 500 €", "1 500-2 500" = "1 500 à 2 500 €",
                        "2 500-4 000" = "2 500 à 4 000 €", "4 000 et +" = "4 000 € et plus",
                        "NSP" = "non déclaré", "Autre/NSP" = "autre ou ne sait pas")

tableau_profils <- function(cl, code, titre, champ) {
  p <- typologies$profils |> filter(typologie == cl)
  liens <- typologies$liens_sociaux |> filter(typologie == cl)
  colonnes_classes <- c(ordre_classes[[cl]], "Ensemble")
  corps <- c()
  for (v in unique(p$variable)) {
    l <- liens |> filter(variable == v)
    corps <- c(corps, paste0(v, " & \\multicolumn{5}{r}{", italique(paste0("V = ", nombre(l$v_cramer, 2),
                                                                         " ; ", p_texte(l$p))), "} \\\\"))
    pv <- p |> filter(variable == v)
    modalites <- intersect(ordre_modalites, pv$modalite)
    modalites <- c(modalites, setdiff(unique(pv$modalite), modalites))
    for (m in modalites) {
      valeurs <- map_chr(colonnes_classes, function(cc) {
        x <- pv$pct[pv$classe == cc & pv$modalite == m]
        if (length(x) == 0) "" else nombre(x, 1)
      })
      libelle <- if (m %in% names(libelles_modalites)) libelles_modalites[[m]] else tolower(m)
      if (v == "Connaissance du réseau") libelle <- paste(m, "sur 2")
      if (v == "Intérêt pour l'énergie") libelle <- paste(m, "sur 5")
      corps <- c(corps, ligne(retrait(echapper(libelle)), valeurs))
    }
    corps <- c(corps, espace("4pt"))
  }
  corps <- c(ligne("Part pondérée", nombre(part_classes[[cl]], 1), "100,0"),
             ligne("$n$", effectif(n_classes[[cl]]), effectif(sum(n_classes[[cl]]))),
             espace("4pt"), head(corps, -1))
  ecrire_tableau(
    tex(paste0("typo_profil_", code)),
    titre = titre,
    etiquette = paste0("typo_profil_", code),
    colonnes = "l*{5}{c}",
    espacement = "4pt",
    entete = entete_classes(cl, apres = "Ensemble"),
    corps = corps,
    note = paste0(
      "Répartition pondérée de chaque classe, en \\%, ", champ, ". En italique, sur la ligne de chaque ",
      "variable : V de Cramér pondéré entre la variable et la classe (de 0, aucun lien, à 1), et $p$ du ",
      "test d'indépendance de Rao-Scott."
    ),
    taille = "scriptsize"
  )
}
tableau_profils("classe_sp", "sp", "Le profil social des conceptions du service public",
                "sur les 3\\,185 répondants")
tableau_profils("classe_gestes", "gestes", "Le profil social des rapports aux gestes",
                "sur les 2\\,770 titulaires d'un contrat")

### Tableau : les positions selon la classe

positions <- typologies$positions_classes
noms_positions <- c("Pro-nucléaire (0-100)" = "Pro-nucléaire (0-100)",
                    "Malus acceptable (%)" = "Malus acceptable (\\%)",
                    "Bonus acceptable (%)" = "Bonus acceptable (\\%)",
                    "Investissement public (%)" = "Investissement public acceptable (\\%)",
                    "Offre verte souscrite (%)" = "Offre verte souscrite (\\%)",
                    "Prêt à payer plus (1-5)" = "Prêt à payer plus le vert (1-5)")
lignes_positions <- function(cl) {
  pos <- positions |> filter(typologie == cl)
  map_chr(names(noms_positions), function(np) {
    x <- pos |> filter(position == np) |> slice(match(ordre_classes[[cl]], classe))
    dec <- if (grepl("1-5", np)) 2 else 1
    ligne(retrait(noms_positions[[np]]), nombre(x$moyenne, dec), p_valeur(x$p_classes[1]))
  })
}

# Les deux typologies croisées (titulaires d'un contrat), en % des lignes
croisement <- typologies$croisement
croisement <- 100 * prop.table(croisement, 1)
croisement <- croisement[ordre_classes$classe_sp, ordre_classes$classe_gestes]
plan_longs <- svydesign(ids = ~1, weights = ~poids_norm, data = filter(df, long))
test_croisement <- svychisq(~ classe_sp + classe_gestes, plan_longs)
lignes_croisement <- map_chr(seq_len(nrow(croisement)), function(i) {
  ligne(retrait(paste0(i, ". ", rownames(croisement)[i])), nombre(croisement[i, ], 1), "")
})

ecrire_tableau(
  tex("typo_positions"),
  titre = "Les positions sur la transition selon la classe",
  etiquette = "typo_positions",
  colonnes = "l*{5}{c}",
  espacement = "4pt",
  entete = NULL,
  corps = c(entete_classes("classe_sp", avant = "\\textbf{A.} Conceptions du service public", apres = "$p$"),
            "\\midrule",
            lignes_positions("classe_sp"),
            "\\midrule",
            entete_classes("classe_gestes", avant = "\\textbf{B.} Rapports aux gestes", apres = "$p$"),
            "\\midrule",
            lignes_positions("classe_gestes"),
            "\\midrule",
            entete_classes("classe_gestes", avant = "\\textbf{C.} Les deux typologies croisées", apres = ""),
            "\\midrule",
            lignes_croisement),
  note = paste0(
    "A et B : moyennes pondérées (score de 0 à 100, parts en \\%, échelle de 1 à 5) ; $p$ du test d'égalité ",
    "entre les classes. L'offre verte et la disposition à payer ne sont posées qu'aux titulaires d'un contrat. ",
    "C : répartition de chaque conception du service public entre les rapports aux gestes, en \\% des lignes, ",
    "chez les 2\\,770 titulaires d'un contrat. Test de Rao-Scott : ", p_texte(test_croisement$p.value), "."
  ),
  taille = "scriptsize"
)
# ecrire_tableau met un \midrule après l'en-tête : sans en-tête, on retire ce
# premier \midrule
lignes <- readLines(tex("typo_positions"))
premier <- which(lignes == "\\midrule")[1]
writeLines(lignes[-premier], tex("typo_positions"))


############################################
### 5. Les prolongements des régressions ###
############################################

### Tableau : la décomposition du R² du modèle complet (Shapley)

ordre_positions <- c(local = "Gestion décentralisée (1-5)", concurrence = "Concurrence (1-5)",
                     solidarite_pts = "Même service partout (pts)", pro_nucleaire_pts = "Pro-nucléaire (0-100)",
                     malus_pts = "Malus acceptable (pts)", offre_verte_pts = "Offre verte souscrite (pts)",
                     gestes_part_pts = "Gestes (pts)", axe_marche = "Axe 1 de l'ACM du SP")
blocs <- c("Âge", "Sexe", "Diplôme", "Revenu", "Conditions de vie", "Rapport à l'énergie", "Conception du SP")

r2 <- lire(reg, "01_R2_modeles_emboites.csv")
shapley <- lire(reg, "02_decomposition_shapley.csv")
tests_blocs <- lire(reg, "05_tests_blocs.csv")

corps <- map_chr(names(ordre_positions), function(y) {
  s <- shapley |> filter(variable == y)
  t <- tests_blocs |> filter(variable == y)
  parts <- map_chr(blocs, function(b) {
    if (!b %in% s$bloc) return("")
    paste0(nombre(s$part[s$bloc == b], 0), etoiles(t$p_BH[t$bloc == b]))
  })
  ligne(ordre_positions[[y]], effectif(r2$n[r2$variable == y]), nombre(r2$R2_M4[r2$variable == y], 3), parts)
})
ecrire_tableau(
  tex("reg_shapley"),
  titre = "Ce que chaque bloc apporte au modèle complet (décomposition du R\\textsuperscript{2})",
  etiquette = "reg_shapley",
  colonnes = "lrc*{4}{>{\\centering\\arraybackslash}p{0.95cm}}*{3}{>{\\centering\\arraybackslash}p{1.6cm}}",
  espacement = "3pt",
  entete = c(ligne("", "", "", "\\multicolumn{7}{c}{Part du R\\textsuperscript{2} (\\%)}"),
             "\\cmidrule(lr){4-10}",
             ligne("", "$n$", "R\\textsuperscript{2}", "Âge", "Sexe", "Diplôme", "Revenu",
                   deux_lignes("Conditions", "de vie"), deux_lignes("Connaissance", "et intérêt"),
                   deux_lignes("Conception", "du SP"))),
  corps = corps,
  note = paste(
    "Modèle du bloc F (profil, zone, connaissance du réseau, intérêt pour l'énergie, conceptions du service",
    "public), moindres carrés pondérés. Les coefficients sont ceux du tableau du bloc F. Part de chaque bloc",
    "de variables dans le R\\textsuperscript{2} pondéré (hors vague), par la décomposition de Shapley : le gain",
    "moyen de R\\textsuperscript{2} quand on ajoute le bloc, sur tous les ordres d'entrée possibles. Les parts",
    "somment à 100. Conditions de vie : zone, logement, statut et difficultés à payer. Les R\\textsuperscript{2}",
    "ne se comparent pas d'une ligne à l'autre (une variable binaire a mécaniquement un R\\textsuperscript{2} plus",
    "faible) ; les parts, si. L'axe du service public est construit à partir des conceptions du service public,",
    "qui ne peuvent donc pas l'expliquer. L'axe nucléaire de l'ACP n'est pas repris : il est corrélé à 0,91 avec",
    "le score pro-nucléaire. Étoiles : test de Wald de nullité jointe des variables du bloc, $p$ corrigés pour",
    "les tests multiples (Benjamini-Hochberg).", LEGENDE_ETOILES
  ),
  taille = "scriptsize"
)

### Tableau : les cinq interactions, en valeurs prédites

tests_interactions <- lire(reg, "Prolongements", "01_tests_interactions.csv")
predites <- lire(reg, "Prolongements", "02_valeurs_predites.csv")

panneaux <- tribble(
  ~code, ~titre,                                                         ~ordre_x,                                                        ~etiquettes_groupe,                                       ~dec,
  "A",   "Connaissance du réseau (0-2), selon le sexe et l'âge",         c("18-34", "35-49", "50-64", "65+"),                             c(Homme = "Hommes", Femme = "Femmes"),                    2,
  "B",   "Pro-nucléaire (0-100), selon le sexe et l'âge",                c("18-34", "35-49", "50-64", "65+"),                             c(Homme = "Hommes", Femme = "Femmes"),                    1,
  "C",   "Concurrence (1-5), selon la connaissance et la définition du SP", c("Organisme public", "Les deux", "Mission d'intérêt général"), c("0" = "Aucun acteur du réseau connu", "2" = "Les deux connus"), 2,
  "D",   "Malus acceptable (\\%), selon le revenu et la zone",           c("< 1 000", "1 000-1 500", "1 500-2 500", "2 500-4 000", "4 000 et +"), c(Urbain = "Urbains", Périurbain = "Périurbains", Rural = "Ruraux"), 1,
  "E",   "Gestes (\\%), selon le revenu et l'intérêt pour l'énergie",   c("< 1 000", "1 000-1 500", "1 500-2 500", "2 500-4 000", "4 000 et +"), c("2" = "Intérêt de 2 sur 5", "5" = "Intérêt de 5 sur 5"), 1
)
libelles_x <- c("18-34" = "18-34 ans", "35-49" = "35-49 ans", "50-64" = "50-64 ans", "65+" = "65 ans et +",
                "< 1 000" = "$<$ 1\\,000\\,€", "1 000-1 500" = "1\\,000-1\\,500\\,€",
                "1 500-2 500" = "1\\,500-2\\,500\\,€", "2 500-4 000" = "2\\,500-4\\,000\\,€",
                "4 000 et +" = "4\\,000\\,€ et +", "Organisme public" = "Organisme",
                "Les deux" = "Les deux", "Mission d'intérêt général" = "Mission")

corps <- c()
for (i in seq_len(nrow(panneaux))) {
  h <- panneaux[i, ]
  t <- tests_interactions |> filter(code == h$code)
  x_vals <- h$ordre_x[[1]]
  vides <- rep("", 5 - length(x_vals))
  corps <- c(corps,
             if (i > 1) "\\midrule",
             groupe(paste0("\\textbf{", h$code, ".} ", h$titre, " \\textit{(interaction : ",
                           p_texte(t$p), ", corrigé ", p_valeur(t$p_BH), " ; $n$ = ", effectif(t$n), ")}"), 6),
             ligne("", paste0("\\textit{", libelles_x[x_vals], "}"), vides))
  for (g in names(h$etiquettes_groupe[[1]])) {
    v <- predites |> filter(code == h$code, groupe == g) |> slice(match(x_vals, x))
    corps <- c(corps,
               ligne(retrait(h$etiquettes_groupe[[1]][[g]]), nombre(v$estimation, h$dec), vides),
               ligne("", paste0("{\\tiny ", intervalle(v$ic_bas, v$ic_haut, h$dec), "}"), vides))
  }
}
ecrire_tableau(
  tex("reg_interactions"),
  titre = "Cinq interactions choisies à l'avance : valeurs prédites",
  etiquette = "reg_interactions",
  colonnes = "l*{5}{>{\\centering\\arraybackslash}p{1.75cm}}",
  espacement = "3pt",
  entete = NULL,
  corps = corps,
  note = paste(
    "Valeurs prédites à autres caractéristiques égales : position moyenne si tous les répondants avaient la",
    "modalité de la colonne et celle de la ligne, le reste de leur profil inchangé. Intervalles à 95~\\% en",
    "dessous. Moindres carrés pondérés ; contrôles du bloc F (du bloc A2 et de l'intérêt pour l'énergie en A).",
    "Gestes : titulaires d'un contrat. $p$ du test de Wald de l'interaction, puis corrigé pour les cinq tests",
    "(Benjamini-Hochberg). Les hypothèses : A, la connaissance croît-elle avec l'âge chez les deux sexes ? B,",
    "l'écart de genre sur le nucléaire varie-t-il avec l'âge ? C, le lien entre connaissance et concurrence",
    "dépend-il de la définition du service public ? D, le malus est-il plus refusé par les ruraux modestes ?",
    "E, l'intérêt se traduit-il en gestes à tous les niveaux de revenu ?"
  ),
  taille = "scriptsize"
)
lignes <- readLines(tex("reg_interactions"))
premier <- which(lignes == "\\midrule")[1]
writeLines(lignes[-premier], tex("reg_interactions"))

### Tableau : la décomposition de l'écart entre femmes et hommes sur le nucléaire

decomposition <- lire(reg, "Prolongements", "04_decomposition_ecart_genre.csv")
termes <- c("Écart à profil égal", "Connaissance du réseau", "Intérêt pour l'énergie",
            "SP = absence de profit", "SP défini par l'organisme", "SP défini par la mission", "Écart restant")
libelles_termes <- c("Écart à profil égal" = "\\textbf{Écart à profil égal}",
                     "Connaissance du réseau" = retrait("Connaissance du réseau"),
                     "Intérêt pour l'énergie" = retrait("Intérêt pour l'énergie"),
                     "SP = absence de profit" = retrait("SP : absence de profit"),
                     "SP défini par l'organisme" = retrait("SP défini par l'organisme"),
                     "SP défini par la mission" = retrait("SP défini par la mission"),
                     "Écart restant" = "\\textbf{Écart restant}")
corps <- c()
for (te in termes) {
  v <- decomposition |> filter(terme == te) |>
    slice(match(c("pro_nucleaire_pts", "nucleaire_premier_pts"), position))
  corps <- c(corps,
             if (te == "Connaissance du réseau") c(espace("2pt"), groupe("\\textit{dont part passant par\\ldots}", 3)),
             if (te == "Écart restant") espace("4pt"),
             ligne(libelles_termes[[te]], nombre(v$estimation, 1)),
             ligne("", paste0("{\\tiny ", intervalle(v$ic_bas, v$ic_haut, 1), "}")))
}
ecrire_tableau(
  tex("reg_ecart_genre"),
  titre = "Ce que l'écart de genre sur le nucléaire doit à la connaissance et aux conceptions",
  etiquette = "reg_ecart_genre",
  colonnes = "lcc",
  entete = ligne("", "Score pro-nucléaire (0-100)", "Nucléaire en 1\\textsuperscript{re} production (pts)"),
  corps = corps,
  note = paste(
    "Écart entre femmes et hommes, en points. Écart à profil égal : moindres carrés pondérés avec l'âge, le",
    "diplôme, le revenu, la zone, le logement, le statut, les difficultés à payer et la vague (bloc A2). Écart",
    "restant : avec en plus les cinq variables listées (bloc F). Décomposition de Gelbach (2016) : la part",
    "d'une variable est l'écart entre femmes et hommes sur cette variable, à profil égal, multipliée par son",
    "coefficient dans le modèle complet ; les parts somment exactement à la différence des deux écarts.",
    "Intervalles à 95~\\% par bootstrap (500 tirages), en dessous."
  )
)

message("Tableaux écrits dans ", dossier, " : ", paste(list.files(dossier), collapse = ", "))
