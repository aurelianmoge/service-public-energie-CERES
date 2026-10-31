#################################
### Chargement des packages   ###
#################################

library(tidyverse)
library(survey)


#################################
#### Importation des données ####
#################################

# Ce script supprime ses objets intermédiaires et ne conserve que df (pondéré !)
source("2. Scripts/1. Redressement.R", encoding = "UTF-8")

plan_complet <- attr(df, "plan_redresse")
diagnostic_redressement <- attr(df, "diagnostic_redressement")
if (is.null(plan_complet)) stop("Le plan de redressement manque dans df.")
stopifnot(nrow(df) == nrow(plan_complet$variables),
          isTRUE(all.equal(as.numeric(weights(plan_complet)), as.numeric(df$poids))))
attr(df, "plan_redresse") <- NULL
attr(df, "diagnostic_redressement") <- NULL

# On conserve les longs ET les courts ; chaque modèle choisit ensuite son domaine.
df$long <- df$version_questionnaire == "long"
stopifnot(!anyNA(df$long))

dossier_sortie <- "3. Outputs/Régressions"
dir.create(dossier_sortie, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dossier_sortie)) stop("Impossible de créer le dossier de sortie.")

# À COMPLÉTER si tu souhaites choisir une tranche de revenu de référence précise.
# Sinon, la tranche observée la plus fréquente en poids sert de référence.
reference_revenu <- NULL

# À VÉRIFIER : TRUE suppose que tous les rangs demandés doivent être renseignés.
# Mettre FALSE seulement si un classement partiel est une réponse valide.
classements_complets <- TRUE

# À COMPLÉTER si le questionnaire confirme le nom de cette colonne, par exemple
# "G4Q00004_SQ007". Ne pas deviner son numéro : NULL laisse les N/A en NA.
colonne_nsp_acceptabilite <- NULL


#################################
### Fonctions de recodage      ###
#################################

texte <- function(x) {
  z <- as.character(x)
  z <- gsub("[\u00a0\u202f]", " ", z)
  z <- gsub("\u2019", "'", z, fixed = TRUE)
  trimws(gsub("[[:space:]]+", " ", z))
}

manquant_technique <- function(x) {
  z <- texte(x)
  is.na(z) | z %in% c("", "N/A", "NA", "Sans réponse", "Non renseigné")
}

nsp <- function(x) {
  z <- tolower(texte(x))
  !is.na(z) & z %in% c("ne sait pas / ne se prononce pas", "ne sait pas",
                       "ne se prononce pas", "nsp")
}

refus <- function(x) {
  z <- tolower(texte(x))
  !is.na(z) & z %in% c("refus de répondre", "refuse de répondre",
                       "préfère ne pas répondre")
}

manquant <- function(x) manquant_technique(x) | nsp(x) | refus(x)

echelle_1_5 <- function(x, variable) {
  z <- texte(x)
  z[manquant(x)] <- NA_character_
  mauvais <- !is.na(z) & !grepl("^[1-5]($|[[:space:]])", z)
  if (any(mauvais)) {
    stop(variable, " : compléter le recodage de ",
         paste(unique(z[mauvais]), collapse = " / "))
  }
  as.numeric(substr(z, 1, 1))
}

recode_choix <- function(x, motifs, valeurs, variable) {
  z <- texte(x)
  resultat <- rep(NA_character_, length(z))
  nombre <- rep(0L, length(z))
  for (j in seq_along(motifs)) {
    i <- !manquant(x) & grepl(motifs[j], z)
    resultat[i] <- valeurs[j]
    nombre[i] <- nombre[i] + 1L
  }
  mauvais <- !manquant(x) & nombre != 1L
  if (any(mauvais)) {
    stop(variable, " : compléter le recodage de ",
         paste(unique(z[mauvais]), collapse = " / "))
  }
  resultat
}

coche <- function(x, variable) {
  z <- texte(x)
  mauvais <- !manquant(x) & !z %in% c("Oui", "Non")
  if (any(mauvais)) stop(variable, " : libellés Oui/Non à vérifier.")
  ifelse(manquant(x), NA_real_, as.numeric(z == "Oui"))
}

facteur_controle <- function(x, reference = NULL) {
  z <- texte(x)
  z[manquant(x)] <- "NSP/manquant"
  f <- factor(z)
  valides <- setdiff(levels(f), "NSP/manquant")
  if (is.null(reference)) {
    if (length(valides)) {
      masses <- tapply(df$poids, z, sum)
      reference <- valides[which.max(masses[valides])]
    } else reference <- levels(f)[1]
  }
  if (!reference %in% levels(f)) stop("Référence absente : ", reference)
  relevel(f, ref = reference)
}

ecrire_csv <- function(x, nom) {
  write.csv2(x, file.path(dossier_sortie, nom), row.names = FALSE,
             na = "", fileEncoding = "UTF-8")
}

# Export des libellés AVANT les recodages : permet de corriger un arrêt du script.
variables_auditees <- names(df)[grepl("^G[1-7]Q|^zone|^taille_commune", names(df))]
variables_auditees <- setdiff(variables_auditees,
                              variables_auditees[grepl("_other$", variables_auditees)])
audit_libelles <- bind_rows(lapply(variables_auditees, function(v) {
  z <- texte(df[[v]])
  z[is.na(z)] <- "<NA réel>"
  tab <- table(z)
  data.frame(variable = v, libelle = names(tab), n = as.integer(tab))
}))
ecrire_csv(audit_libelles, "audit_libelles.csv")


###########################
### Création des scores ###
###########################

# Intérêt pour l'énergie et connaissance des acteurs du système électrique
#-------------------------------------------------------------------------

### Score d'intérêt

df$interet_energie <- echelle_1_5(df$G3Q00001, "G3Q00001")
df$interet_transition <- echelle_1_5(df$G4Q00001, "G4Q00001")


### Scores de connaissance

# On distingue deux domaines et on conserve aussi le total.
# NSP explicite = 0 ; absence technique ou refus = NA.
# À VÉRIFIER : les réponses justes ci-dessous sont reprises de ton script.
roles <- list(
  RTE = "Transporte l’électricité à grande échelle (réseau haute tension)",
  Enedis = "Distribue l’électricité localement jusqu’aux logements/entreprises",
  EDF = "Produit et fournit l’électricité",
  Engie = "Produit et fournit l’électricité"
)
items_roles <- paste0("G3Q00002_SQ00", 1:4)
bons_roles <- sapply(seq_along(roles), function(j) {
  reponse <- df[[items_roles[j]]]
  ifelse(manquant_technique(reponse) | refus(reponse), NA_real_,
         as.numeric(texte(reponse) %in% texte(roles[[j]])))
})
colnames(bons_roles) <- names(roles)

df$conn_reseau <- rowSums(bons_roles[, c("RTE", "Enedis"), drop = FALSE])
df$conn_fournisseurs <- rowSums(bons_roles[, c("EDF", "Engie"), drop = FALSE])
df$score_connaissance <- rowSums(bons_roles)
df$connaissance_moyenne <- df$score_connaissance / 4

# On conserve les items pour les modèles complémentaires.
for (a in names(roles)) df[[paste0("juste_", a)]] <- bons_roles[, a]

# Deux scores séparés OU le total ; jamais les trois simultanément :
# score_connaissance = conn_reseau + conn_fournisseurs (colinéarité exacte).

### Non-savoir déclaré et erreurs parmi ceux qui tentent de répondre

tentatives <- sapply(1:2, function(j) {
  x <- df[[items_roles[j]]]
  ifelse(manquant_technique(x) | refus(x), NA_real_, as.numeric(!nsp(x)))
})
df$conn_nb_nsp <- 2 - rowSums(tentatives)
df$conn_juste_si_repond <- ifelse(rowSums(tentatives) > 0,
                                  100 * df$conn_reseau / rowSums(tentatives), NA_real_)
# Ce dernier score porte sur une sous-population sélectionnée, pas sur tous.


### Conceptions du service public et positions de gouvernance

df$service_public <- factor(recode_choix(
  df$G3Q00004,
  c("^Un organisme public", "^Une mission", "^Les deux"),
  c("Organisme", "Mission", "Les deux"), "G3Q00004"),
  levels = c("Les deux", "Organisme", "Mission"))

df$solidarite_territoriale <- recode_choix(
  df$G3Q00006, c("^Adapter le coût", "^Maintenir la même qualité"),
  c("Adaptation", "Égalité/solidarité"), "G3Q00006")
df$solidarite_01 <- ifelse(is.na(df$solidarite_territoriale), NA_real_,
                           as.numeric(df$solidarite_territoriale == "Égalité/solidarité"))
df$solidarite_pts <- 100 * df$solidarite_01

# 1 = national ; 5 = local (l'ancien nom était centralisation).
df$local <- echelle_1_5(df$G3Q00008, "G3Q00008")
# 1 = monopole public ; 5 = concurrence.
df$concurrence <- echelle_1_5(df$G3Q00009, "G3Q00009")
df$sp_sans_profit <- coche(df$G3Q00005_SQ002, "G3Q00005_SQ002")
df$sp_sans_profit_pts <- 100 * df$sp_sans_profit
df$sp_organisme_pts <- 100 * as.numeric(df$service_public == "Organisme")
df$sp_mission_pts <- 100 * as.numeric(df$service_public == "Mission")


# Indicateurs complémentaires du script de ton collègue
#-------------------------------------------------------

### Acceptabilité : on garde les mesures séparées

acc_items <- sapply(1:5, function(j) {
  v <- paste0("G4Q00004_SQ00", j)
  coche(df[[v]], v)
})
colnames(acc_items) <- c("bonus", "malus", "invest_public", "regulation", "information")
aucune <- texte(df$G4Q00004_SQ006) %in% "Oui"
contradiction <- aucune & rowSums(acc_items == 1, na.rm = TRUE) > 0
acc_items[aucune, ] <- 0
acc_items[contradiction, ] <- NA_real_
if (any(contradiction)) warning("Acceptabilité : Aucune + mesure cochée, mises à NA.")
if (!is.null(colonne_nsp_acceptabilite)) {
  stopifnot(colonne_nsp_acceptabilite %in% names(df))
  acc_items[texte(df[[colonne_nsp_acceptabilite]]) %in% "Oui", ] <- NA_real_
}
for (v in colnames(acc_items)) {
  df[[paste0("acc_", v)]] <- acc_items[, v]
  df[[paste0("acc_", v, "_pts")]] <- 100 * acc_items[, v]
}


### Gestes réalisés : part parmi les concernés et nombre absolu

gestes <- sapply(1:9, function(j) texte(df[[paste0("G7Q00001_SQ00", j)]]))
fait <- concerne <- matrix(NA_real_, nrow(df), 9)
for (j in 1:9) {
  x <- gestes[, j]
  deja <- x %in% "Déjà fait"
  jamais <- !is.na(x) & grepl("^Jamais", x)
  non_concerne <- x %in% c("Non concerné·e", "Non concerné", "Non concernée")
  inconnu <- df$long & !(deja | jamais | non_concerne | manquant(x))
  if (any(inconnu)) stop("Compléter le recodage de G7Q00001_SQ00", j,
                         " : ", paste(unique(x[inconnu]), collapse = " / "))
  fait[deja, j] <- 1
  fait[jamais | non_concerne, j] <- 0
  concerne[deja | jamais, j] <- 1
  concerne[non_concerne, j] <- 0
}
fait[!df$long, ] <- NA_real_
concerne[!df$long, ] <- NA_real_
df$gestes_n <- rowSums(fait)
df$gestes_concernes <- rowSums(concerne)
df$gestes_part <- ifelse(df$gestes_concernes > 0,
                         df$gestes_n / df$gestes_concernes, NA_real_)
df$gestes_part_pts <- 100 * df$gestes_part
# Pas de na.rm = TRUE : une réponse inconnue ne devient ni zéro ni "non concerné".


### Offre verte : disposition à payer, souscription et scepticisme

x <- df$G6Q00004
x[!df$long] <- NA
df$vert <- echelle_1_5(x, "G6Q00004")
df$vert_c <- df$vert - 3 # centré sur le milieu de l'échelle
x <- df$G6Q00001
x[!df$long] <- NA
df$offre_verte <- coche(x, "G6Q00001")
df$offre_verte_pts <- 100 * df$offre_verte
x <- df$G6Q00005_SQ006
x[!df$long] <- NA
df$scepticisme <- coche(x, "G6Q00005_SQ006")
df$scepticisme_pts <- 100 * df$scepticisme


### Priorité relative du nucléaire dans deux classements

rang_nucleaire <- function(prefixe, k, motif) {
  choix <- sapply(1:k, function(j) texte(df[[paste0(prefixe, j)]]))
  absents <- matrix(manquant(as.vector(choix)), nrow(df), k)
  nucleaire <- matrix(grepl(motif, as.vector(choix), ignore.case = TRUE), nrow(df), k)
  doublons <- apply(choix, 1, function(z) anyDuplicated(z[!manquant(z)]) > 0)
  valide <- !absents[, 1] & !doublons & rowSums(nucleaire) <= 1
  if (classements_complets) valide <- valide & rowSums(absents) == 0
  resultat <- rep(NA_real_, nrow(df))
  for (i in which(valide)) {
    rang <- which(nucleaire[i, ])
    resultat[i] <- if (length(rang)) k + 1 - rang[1] else 0
  }
  resultat
}
# À VÉRIFIER : ces motifs reprennent la logique du collègue ; les libellés
# correspondants doivent bien désigner une priorité positive du nucléaire.
df$nuc_production <- rang_nucleaire("G4Q00003_", 3, "^Nucléaire")
df$nuc_levier <- rang_nucleaire("G4Q00002_", 5, "nucléaire")
df$pro_nucleaire <- (df$nuc_production / 3 + df$nuc_levier / 5) / 2
df$pro_nucleaire_pts <- 100 * df$pro_nucleaire
df$nuc_premier_pts <- 100 * as.numeric(df$nuc_production == 3)


### Ancrage local : indicateurs séparés, sans score forcé

df$anc_collectivites <- coche(df$G3Q00007_SQ004, "G3Q00007_SQ004")
x <- df$G4Q00006_SQ003
x[!df$long] <- NA
df$anc_porteur_local <- coche(x, "G4Q00006_SQ003")
df$anc_auto_collective <- ifelse(
  !df$long | manquant(df$G4Q00007), NA_real_,
  as.numeric(texte(df$G4Q00007) == "Autoconsommation collective"))
for (v in c("anc_collectivites", "anc_porteur_local", "anc_auto_collective")) {
  df[[paste0(v, "_pts")]] <- 100 * df[[v]]
}


### Indice solidarité / monopole : complément exploratoire

df$monopole_01 <- (5 - df$concurrence) / 4
df$indice_solidarite_monopole <- 100 * (df$solidarite_01 + df$monopole_01) / 2
# Les composantes restent analysées séparément dans les modèles principaux.


#######################################
### Variables de contrôle et plan   ###
#######################################

# Diplôme et revenu restent des catégories, comme dans ton script.
# Les NSP sont identifiés dans l'analyse principale ; on les exclut ensuite
# dans une sensibilité. Une catégorie NSP ne remplace pas une imputation.
df$diplome <- facteur_controle(df$G1Q00003, "Bac")
df$revenu <- facteur_controle(df$G1Q00005, reference_revenu)
df$AGE_large <- relevel(factor(df$AGE_large), ref = "35-49")

# "Autre" est une réponse observée, distincte de NSP et des absences.
df$SEXE <- facteur_controle(df$G1Q00001, "Homme")
df$questionnaire <- factor(ifelse(df$long, "Long", "Court"),
                           levels = c("Long", "Court"))
df$vague <- factor(df$vague)
df$zone2 <- relevel(factor(df$zone2), ref = "Urbain")
variable_zone <- if ("zone3" %in% names(df)) "zone3" else "zone2"
df$zone <- facteur_controle(df[[variable_zone]], "Urbain")
if ("taille_commune" %in% names(df)) {
  reference_taille <- if ("Grande ville" %in% texte(df$taille_commune)) "Grande ville" else NULL
  df$taille <- facteur_controle(df$taille_commune, reference_taille)
}
df$logement <- facteur_controle(df$G2Q00003)

statut <- texte(df$G2Q00004)
if ("G2Q00004_other" %in% names(df)) {
  precision <- tolower(texte(df$G2Q00004_other))
  corriger <- statut %in% "Autre" & grepl("usufrui|indivision|crédit", precision)
  statut[corriger] <- "Propriétaire"
}
statut[df$long & manquant(statut)] <- "NSP/manquant"
statut[!df$long] <- "Non posé (court)"
df$statut <- factor(statut)
if ("Propriétaire" %in% levels(df$statut)) {
  df$statut <- relevel(df$statut, ref = "Propriétaire")
}

x <- df$G2Q00009
x[!df$long] <- NA
difficultes <- recode_choix(
  x, c("^Jamais$", "^Rarement$", "^Parfois$", "^Souvent$", "^Toujours$"),
  c("Jamais", "Rarement/parfois", "Rarement/parfois", "Souvent/toujours",
    "Souvent/toujours"), "G2Q00009")
difficultes[df$long & is.na(difficultes)] <- "NSP/manquant"
difficultes[!df$long] <- "Non posé (court)"
df$difficultes <- factor(difficultes)

# Codage conditionnel : seuls les longs renseignent les difficultés.
# Zéro chez les courts signifie "pas de contribution à cette indicatrice".
# Le niveau Non posé du statut (ou questionnaire) représente leur intercept.
df$diff_parfois <- as.numeric(df$long & difficultes == "Rarement/parfois")
df$diff_souvent <- as.numeric(df$long & difficultes == "Souvent/toujours")
df$diff_nsp <- as.numeric(df$long & difficultes == "NSP/manquant")

df$controles_observes <- !(
  manquant(df$G1Q00001) | manquant(df$G1Q00003) | manquant(df$G1Q00005) |
    manquant(df$G2Q00003) | manquant(df[[variable_zone]]) | is.na(df$vague) |
    (df$long & (manquant(df$G2Q00004) | manquant(df$G2Q00009)))
)
# Les réponses "Autre" au sexe satisfont controles_observes si le reste est connu.

controles_profil <- c("AGE_large", "SEXE", "diplome", "revenu", "vague", "questionnaire")
controles_complets <- c("AGE_large", "SEXE", "diplome", "revenu", "vague",
                        "logement", "statut", "diff_parfois", "diff_souvent", "diff_nsp")
controles_sans_logement <- c(controles_profil, "diff_parfois", "diff_souvent", "diff_nsp")
# Statut et questionnaire ne sont pas inclus ensemble : leur indicatrice
# Court/Non posé serait redondante.

# Aucun filtre ni tri de df n'a été effectué depuis le redressement.
stopifnot(nrow(df) == nrow(plan_complet$variables),
          identical(as.character(df$strate), as.character(plan_complet$variables$strate)))
plan_complet$variables <- df
plan_non_pondere <- svydesign(ids = ~1, data = df)
# Sensibilité à la convention de calage du sexe : âge x zone seulement,
# pour tout le monde, avec les mêmes effectifs cibles et sans exclure Autre.
plan_age_zone <- postStratify(
  svydesign(ids = ~1, weights = ~poids_initial, data = df),
  strata = ~strate,
  population = diagnostic_redressement$cellules[, c("strate", "Freq")]
)


#################################################
### Fonctions pour estimer à échantillon commun ###
#################################################

preparer_plan <- function(formules, longs = FALSE, hf = FALSE,
                          observes = FALSE, base = plan_complet) {
  variables <- unique(unlist(lapply(formules, all.vars)))
  absentes <- setdiff(variables, names(df))
  if (length(absentes)) stop("Colonnes absentes : ", paste(absentes, collapse = ", "))
  inclusion <- complete.cases(df[, variables, drop = FALSE])
  if (longs) inclusion <- inclusion & df$long
  if (hf) inclusion <- inclusion & as.character(df$SEXE) %in% c("Homme", "Femme")
  if (observes) inclusion <- inclusion & df$controles_observes
  inclusion[is.na(inclusion)] <- FALSE
  subset(base, inclusion)
}

ajuster <- function(formule, plan) {
  tryCatch({
    # Un plan calé conserve les lignes hors domaine avec un poids nul.
    # Les contrôles portent sur les observations qui contribuent au modèle.
    w <- as.numeric(weights(plan))
    if (any(!is.finite(w) | w < 0)) stop("Poids du plan invalides.")
    d <- plan$variables[w > 0, , drop = FALSE]
    if (nrow(d) < 10) stop("Moins de 10 observations complètes.")
    if (anyNA(d[, all.vars(formule), drop = FALSE])) {
      stop("Valeurs manquantes dans le domaine : vérifier preparer_plan().")
    }
    reponse <- all.vars(formule)[1]
    if (length(unique(d[[reponse]])) < 2) {
      stop("Réponse constante dans cet échantillon.")
    }
    # Retirer seulement les termes constants dans le domaine : par exemple
    # questionnaire dans une analyse exclusivement sur les longs.
    termes <- attr(terms(formule), "term.labels")
    garder <- vapply(termes, function(t) {
      variables <- all.vars(as.formula(paste("~", t)))
      all(vapply(variables, function(v) {
        length(unique(d[[v]])) > 1
      }, logical(1)))
    }, logical(1))
    retires <- termes[!garder]
    f <- reformulate(if (any(garder)) termes[garder] else "1",
                     response = reponse, env = baseenv())
    
    # NA retire les lignes de poids nul de l'ajustement et des niveaux des facteurs.
    # Le plan conserve ces lignes pour calculer la variance liée au calage.
    plan$variables[[reponse]][w == 0] <- NA_real_
    modele <- svyglm(f, design = plan, family = gaussian(), na.action = na.omit)
    attr(modele, "termes_constants_retires") <- retires
    if (anyNA(modele$coefficients)) {
      warning("Coefficients non identifiés : vérifier la table diagnostic_modeles.csv.")
    }
    modele
  }, error = function(e) {
    warning(paste(deparse(formule), collapse = " "), " : ", conditionMessage(e))
    structure(list(formule = formule, erreur = conditionMessage(e)),
              class = "modele_non_estime")
  })
}

formule_modele <- function(y, variables) {
  reformulate(variables, response = y, env = baseenv())
}

modele_valide <- function(m) inherits(m, "svyglm")

tests_globaux <- list()
ajouter_test <- function(m, termes, bloc, identifiant, famille = "Exploratoire") {
  if (!modele_valide(m)) return(invisible(NULL))
  resultat <- tryCatch(
    regTermTest(m, as.formula(paste("~", paste(termes, collapse = " + "))),
                method = "Wald"),
    error = function(e) NULL
  )
  tests_globaux[[length(tests_globaux) + 1L]] <<- data.frame(
    bloc = bloc, modele = identifiant, termes = paste(termes, collapse = " + "),
    famille = famille, p = if (is.null(resultat)) NA_real_ else as.numeric(resultat$p),
    test_estimable = !is.null(resultat)
  )
}


##################################
### Régressions : blocs A à F   ###
##################################

# A0 : profil sociodémographique ; A1 : + logement/statut/difficultés ;
# A2 : + territoire ; B1 : + réseau et intérêt ; B2 : total et intérêt ;
# B3 : les deux domaines séparés et intérêt ;
# C : conceptions du service public ; F : B3 et C réunis.
#
# Pour chaque réponse, les huit modèles utilisent les MÊMES individus.
# Les scores de connaissance ne sont jamais inclus sous une forme colinéaire.

positions <- data.frame(
  variable = c("local", "concurrence", "solidarite_pts", "pro_nucleaire_pts",
               "acc_malus_pts", "offre_verte_pts", "gestes_part_pts"),
  nom = c("Organisation locale (1-5)", "Concurrence (1-5)",
          "Solidarité territoriale (points)", "Priorité nucléaire (0-100)",
          "Acceptation du malus (points)", "Offre verte souscrite (points)",
          "Part des gestes réalisés (points)"),
  longs = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
)

modeles_positions <- list()
formules_positions <- list()
plans_positions <- list()

for (i in seq_len(nrow(positions))) {
  y <- positions$variable[i]
  fA0 <- formule_modele(y, controles_profil)
  fA1 <- formule_modele(y, controles_complets)
  fA2 <- update(fA1, . ~ . + zone)
  fB1 <- update(fA2, . ~ . + conn_reseau + interet_energie)
  fB2 <- update(fA2, . ~ . + score_connaissance + interet_energie)
  fB3 <- update(fA2, . ~ . + conn_reseau + conn_fournisseurs + interet_energie)
  fC <- update(fA2, . ~ . + service_public + sp_sans_profit)
  fF <- update(fB3, . ~ . + service_public + sp_sans_profit)
  formules <- list(A0 = fA0, A1 = fA1, A2 = fA2, B1_Reseau = fB1,
                   B2_Total = fB2, B3_Deux_domaines = fB3, C = fC, F = fF)
  plan <- preparer_plan(formules, longs = positions$longs[i])
  modeles <- lapply(formules, ajuster, plan = plan)
  modeles_positions[[y]] <- modeles
  formules_positions[[y]] <- formules
  plans_positions[[y]] <- plan
  for (v in c("AGE_large", "SEXE", "diplome", "revenu", "zone")) {
    ajouter_test(modeles$A2, v, "A2", y)
  }
  ajouter_test(modeles$B1_Reseau, "conn_reseau", "B1", y, "B1_reseau_7_positions")
  ajouter_test(modeles$B1_Reseau, "interet_energie", "B1", y, "B1_interet_7_positions")
  ajouter_test(modeles$B3_Deux_domaines, c("conn_reseau", "conn_fournisseurs"),
               "B3", y, "B3_connaissance_7_positions")
  ajouter_test(modeles$C, "service_public", "C", y, "C_service_public_7_positions")
}

# Exemples de lecture, comme dans ton script :
if (modele_valide(modeles_positions$concurrence$B3_Deux_domaines)) {
  print(summary(modeles_positions$concurrence$B3_Deux_domaines))
}
if (modele_valide(modeles_positions$solidarite_pts$F)) {
  print(summary(modeles_positions$solidarite_pts$F))
}


# Bloc D : disposition à payer, scepticisme et souscription
#---------------------------------------------------------

fD0 <- formule_modele("offre_verte_pts", c(controles_complets, "zone"))
fD1 <- update(fD0, . ~ . + vert_c + scepticisme)
fD2 <- update(fD1, . ~ . + vert_c:scepticisme)
plan_D <- preparer_plan(list(fD0, fD1, fD2), longs = TRUE)
modeles_D <- list(
  Profil = ajuster(fD0, plan_D),
  Additif = ajuster(fD1, plan_D),
  Interaction = ajuster(fD2, plan_D)
)
ajouter_test(modeles_D$Interaction, "vert_c:scepticisme", "D", "offre_verte_pts")
# vert_c = 0 correspond à une disposition à payer égale à 3.
# Le coefficient d'interaction est la différence de pente en points.


# Bloc E : profils associés aux variables explicatives
#-----------------------------------------------------

explicatives <- c("conn_reseau", "conn_fournisseurs", "score_connaissance",
                  "interet_energie", "interet_transition", "sp_organisme_pts",
                  "sp_mission_pts", "sp_sans_profit_pts", "vert", "scepticisme_pts")
modeles_E <- list()
for (y in explicatives) {
  f1 <- formule_modele(y, controles_profil)
  f2 <- formule_modele(y, c(controles_complets, "zone"))
  plan <- preparer_plan(list(f1, f2), longs = y %in% c("vert", "scepticisme_pts"))
  modeles_E[[y]] <- list(Profil = ajuster(f1, plan), Complet = ajuster(f2, plan))
}
# Les deux modèles Organisme/Mission décrivent des probabilités linéaires.
# Ils ne constituent pas un modèle multinomial conjoint.


##########################################
### Questions de ton script initial    ###
##########################################

# On conserve les analyses des longs, et on ajoute l'ensemble long/court pour
# les questions communes. Les contrôles sont sociodémographiques et territoriaux.
modeles_questions <- list()
for (population in c("Tous", "Longs")) {
  for (y in c("conn_reseau", "conn_fournisseurs", "score_connaissance")) {
    f1 <- formule_modele(y, c(controles_profil, "zone", "interet_energie"))
    f2 <- formule_modele(y, c(controles_profil, "zone", "service_public"))
    f3 <- update(f1, . ~ . + service_public)
    plan <- preparer_plan(list(f1, f2, f3), longs = population == "Longs")
    nom <- paste(population, y, sep = "_")
    modeles_questions[[nom]] <- list(
      Interet = ajuster(f1, plan),
      Service_public = ajuster(f2, plan),
      Les_deux = ajuster(f3, plan)
    )
    ajouter_test(modeles_questions[[nom]]$Service_public, "service_public", "Questions", nom)
  }
  f <- formule_modele("interet_energie", c(controles_profil, "zone", "service_public"))
  plan <- preparer_plan(list(f), longs = population == "Longs")
  nom <- paste0(population, "_interet_service_public")
  modeles_questions[[nom]] <- list(Interet = ajuster(f, plan))
  ajouter_test(modeles_questions[[nom]]$Interet, "service_public", "Questions", nom)
}


####################################
### Régressions complémentaires   ###
####################################

### Interactions âge x sexe

# Les modèles principaux ci-dessus gardent tout le monde, dont Autre.
# On tente également l'interaction complète, en signalant toute non-identification.
# Une seconde analyse H/F est une annexe explicitement limitée à H/F ;
# elle ne remplace jamais les modèles principaux et ne modifie aucun poids.
modeles_interactions <- list()
for (y in c("conn_reseau", "score_connaissance", "interet_energie", "local",
            "concurrence", "solidarite_pts", "indice_solidarite_monopole")) {
  f0 <- formule_modele(y, c(controles_profil, "zone"))
  f1 <- update(f0, . ~ . + AGE_large:SEXE)
  plan <- preparer_plan(list(f0, f1), longs = TRUE)
  plan_HF <- preparer_plan(list(f0, f1), longs = TRUE, hf = TRUE)
  modeles_interactions[[y]] <- list(
    Additif_tous_sexes = ajuster(f0, plan),
    Interaction_tous_sexes = ajuster(f1, plan),
    Additif_HF = ajuster(f0, plan_HF),
    Interaction_HF = ajuster(f1, plan_HF)
  )
  ajouter_test(modeles_interactions[[y]]$Interaction_tous_sexes,
               "AGE_large:SEXE", "Interaction_tous_sexes", y)
  ajouter_test(modeles_interactions[[y]]$Interaction_HF,
               "AGE_large:SEXE", "Interaction_HF", y)
}


### Sensibilités : poids, population, non-réponse, intérêt pour la transition

modeles_sensibilite <- list()
for (i in seq_len(nrow(positions))) {
  y <- positions$variable[i]
  f <- formules_positions[[y]]$B3_Deux_domaines
  plan <- plans_positions[[y]]
  # Réutiliser exactement le masque du plan principal pour l'analyse non pondérée.
  variables <- unique(unlist(lapply(formules_positions[[y]], all.vars)))
  inclusion <- complete.cases(df[, variables, drop = FALSE])
  if (positions$longs[i]) inclusion <- inclusion & df$long
  plan_sans_poids <- subset(plan_non_pondere, inclusion)
  plan_sans_calage_sexe <- subset(plan_age_zone, inclusion)
  f_transition <- update(f, . ~ . + interet_transition)
  plan_transition <- preparer_plan(c(formules_positions[[y]], list(f_transition)),
                                   longs = positions$longs[i])
  plan_observe <- preparer_plan(formules_positions[[y]],
                                longs = positions$longs[i], observes = TRUE)
  modeles_sensibilite[[y]] <- list(
    Principal_pondere = ajuster(f, plan),
    Sans_poids = ajuster(f, plan_sans_poids),
    Calage_age_zone_seulement = ajuster(f, plan_sans_calage_sexe),
    Controles_observes = ajuster(f, plan_observe),
    Transition_reference = ajuster(f, plan_transition),
    Transition_ajoutee = ajuster(f_transition, plan_transition)
  )
  if (!positions$longs[i]) {
    plan_longs <- preparer_plan(formules_positions[[y]], longs = TRUE)
    modeles_sensibilite[[y]]$Longs <- ajuster(f, plan_longs)
  }
  # La comparaison principale / contrôles observés ou longs change de population.
  # La paire Transition_reference/ajoutée utilise exactement les mêmes personnes.
}


### Territoire : avec et sans logement/statut, zone2/zone3/taille

modeles_territoire <- list()
territoires <- c("zone", if (variable_zone != "zone2") "zone2",
                 if ("taille" %in% names(df)) "taille")
for (i in seq_len(nrow(positions))) {
  y <- positions$variable[i]
  formules <- list()
  for (v in territoires) {
    formules[[paste0(v, "_Complet")]] <- formule_modele(y, c(controles_complets, v))
    formules[[paste0(v, "_Sans_logement_statut")]] <- formule_modele(
      y, c(controles_sans_logement, v))
  }
  plan <- preparer_plan(formules, longs = positions$longs[i])
  modeles_territoire[[y]] <- lapply(formules, ajuster, plan = plan)
}


### Autres définitions et indicateurs

modeles_complements <- list()
f <- formules_positions$gestes_part_pts$B3_Deux_domaines
plan <- preparer_plan(c(formules_positions$gestes_part_pts,
                        list(update(f, gestes_n ~ .))), longs = TRUE)
modeles_complements$Gestes <- list(
  Part = ajuster(f, plan),
  Nombre = ajuster(update(f, gestes_n ~ .), plan)
)
f <- formules_positions$pro_nucleaire_pts$B3_Deux_domaines
formules <- list(
  Composite = f,
  Production = update(f, nuc_production ~ .),
  Levier = update(f, nuc_levier ~ .),
  Premier = update(f, nuc_premier_pts ~ .)
)
plan <- preparer_plan(c(formules_positions$pro_nucleaire_pts, formules))
modeles_complements$Nucleaire <- lapply(formules, ajuster, plan = plan)

for (y in c("acc_bonus_pts", "acc_invest_public_pts", "acc_regulation_pts",
            "acc_information_pts", "anc_collectivites_pts", "anc_porteur_local_pts",
            "anc_auto_collective_pts", "indice_solidarite_monopole")) {
  f <- formule_modele(y, c(controles_complets, "zone", "conn_reseau",
                           "conn_fournisseurs", "interet_energie"))
  plan <- preparer_plan(list(f), longs = y %in% c("anc_porteur_local_pts",
                                                  "anc_auto_collective_pts"))
  modeles_complements[[y]] <- list(Complet = ajuster(f, plan))
}

# Connaissance : NSP et erreurs parmi les répondants sont deux issues distinctes.
formules <- lapply(c("conn_reseau", "conn_nb_nsp"), function(y) {
  formule_modele(y, c(controles_complets, "zone"))
})
plan <- preparer_plan(formules)
f_repondants <- formule_modele("conn_juste_si_repond", c(controles_complets, "zone"))
plan_repondants <- preparer_plan(c(formules, list(f_repondants)))
modeles_complements$Connaissance_NSP <- list(
  Score_reseau = ajuster(formules[[1]], plan),
  Nombre_NSP = ajuster(formules[[2]], plan),
  Juste_si_repond = ajuster(f_repondants, plan_repondants)
)
for (a in names(roles)) {
  f <- formule_modele(paste0("juste_", a), c(controles_complets, "zone"))
  plan <- preparer_plan(list(f))
  modeles_complements[[paste0("Connaissance_", a)]] <- list(Item = ajuster(f, plan))
}


####################################
### Description et vérifications  ###
####################################

correlation_ponderee <- function(x, y) {
  i <- is.finite(x) & is.finite(y)
  if (sum(i) < 3 || length(unique(x[i])) < 2 || length(unique(y[i])) < 2) {
    return(NA_real_)
  }
  cov.wt(cbind(x[i], y[i]), wt = df$poids[i], cor = TRUE)$cor[1, 2]
}
correlations <- data.frame(
  paire = c("RTE / Enedis", "EDF / Engie", "Réseau / fournisseurs",
            "Intérêt énergie / transition", "Solidarité / monopole"),
  Pearson_pondere = c(
    correlation_ponderee(bons_roles[, "RTE"], bons_roles[, "Enedis"]),
    correlation_ponderee(bons_roles[, "EDF"], bons_roles[, "Engie"]),
    correlation_ponderee(df$conn_reseau, df$conn_fournisseurs),
    correlation_ponderee(df$interet_energie, df$interet_transition),
    correlation_ponderee(df$solidarite_01, df$monopole_01))
)
ecrire_csv(correlations, "correlations_scores.csv")
# Pour les deux items binaires, il s'agit de phi pondéré.
# Pas de changement automatique vers une corrélation tétrachorique.

indicateurs <- unique(c(positions$variable, explicatives, "connaissance_moyenne",
                        "gestes_n", "gestes_concernes",
                        "indice_solidarite_monopole"))
descriptifs <- bind_rows(lapply(indicateurs, function(v) {
  bind_rows(lapply(c("Tous", "Longs", "Courts"), function(population) {
    eligible <- switch(population, Tous = rep(TRUE, nrow(df)),
                       Longs = df$long, Courts = !df$long)
    i <- eligible & !is.na(df[[v]])
    data.frame(
      indicateur = v, population = population, n_eligible = sum(eligible),
      n_observe = sum(i),
      moyenne_ponderee = if (any(i)) weighted.mean(df[[v]][i], df$poids[i]) else NA_real_
    )
  }))
}))
ecrire_csv(descriptifs, "descriptifs.csv")
ecrire_csv(as.data.frame(table(df$AGE_large, df$SEXE, useNA = "ifany")),
           "effectifs_age_sexe.csv")
ecrire_csv(as.data.frame(table(df$version_questionnaire, df$SEXE, useNA = "ifany")),
           "effectifs_questionnaire_sexe.csv")
ecrire_csv(diagnostic_redressement$cellules, "diagnostic_cellules_calage.csv")
ecrire_csv(diagnostic_redressement$contraintes, "diagnostic_contraintes_calage.csv")
ecrire_csv(data.frame(
  n_avant = diagnostic_redressement$n_avant,
  exclus_zone = diagnostic_redressement$exclus_zone,
  exclus_age_apres_zone = diagnostic_redressement$exclus_age_apres_zone,
  n_apres = diagnostic_redressement$n_apres
), "diagnostic_exclusions_redressement.csv")
references <- bind_rows(lapply(
  c("AGE_large", "SEXE", "diplome", "revenu", "logement", "statut", "zone",
    "questionnaire", "service_public", "vague"), function(v) {
      data.frame(variable = v, reference = levels(df[[v]])[1],
                 modalites = paste(levels(df[[v]]), collapse = " / "))
    }))
ecrire_csv(references, "references_facteurs.csv")
denominateurs_gestes <- bind_rows(lapply(levels(df$statut), function(s) {
  i <- df$long & df$statut == s & !is.na(df$gestes_concernes)
  data.frame(statut = s, n = sum(i), nombre_concerne_moyen =
               if (any(i)) weighted.mean(df$gestes_concernes[i], df$poids[i]) else NA_real_)
}))
ecrire_csv(denominateurs_gestes, "denominateurs_gestes.csv")


#################################################
### Sorties des régressions : CSV et LaTeX     ###
#################################################

# Aucune dépendance supplémentaire pour les tables.
# Chaque groupe de modèles reçoit :
# - un CSV numérique long, avec coefficient, SE, IC95 %, p et n ;
# - un CSV large pour comparer les modèles ;
# - un fichier .tex (longtable), à importer avec \input{...}.
# Dans le préambule LaTeX : \usepackage{longtable}.

ddl_modele <- function(m) {
  ddl <- m$df.residual
  if (length(ddl) != 1 || is.na(ddl) || ddl <= 0) NA_real_ else ddl
}

# Une seule source pour les données, les poids et l'effectif de chaque modèle.
# prior.weights est aligné sur m$model, même après suppression des NA.
donnees_modele <- function(m) {
  d <- m$model
  w <- as.numeric(m$prior.weights)
  y <- as.numeric(m$y)
  prediction <- as.numeric(m$fitted.values)
  if (is.null(d) || nrow(d) != length(w) ||
      length(y) != length(w) || length(prediction) != length(w)) {
    stop("Données et poids non alignés dans le modèle.")
  }
  if (any(!is.finite(w) | w < 0)) stop("Poids du modèle invalides.")
  
  d$.poids <- w
  d$.reponse <- y
  d$.prediction <- prediction
  d <- d[w > 0, , drop = FALSE]
  if (!nrow(d)) stop("Aucune observation de poids positif.")
  if (any(!is.finite(d$.reponse)) || any(!is.finite(d$.prediction))) {
    stop("Réponse ou prédiction non finie dans le modèle.")
  }
  d
}

extraire_coefficients <- function(m, nom, groupe) {
  if (!modele_valide(m)) return(data.frame())
  d <- donnees_modele(m)
  s <- summary(m)$coefficients
  ddl <- ddl_modele(m)
  q <- qt(.975, df = ddl)
  data.frame(
    groupe = groupe, modele = nom, terme = rownames(s),
    estimation = s[, 1], ecart_type = s[, 2],
    IC95_inf = s[, 1] - q * s[, 2], IC95_sup = s[, 1] + q * s[, 2],
    p = s[, 4], n = nrow(d), ddl = ddl, row.names = NULL
  )
}

diagnostic_modele <- function(m, nom, groupe) {
  if (!modele_valide(m)) {
    return(data.frame(
      groupe = groupe, modele = nom, n = NA_real_, ESS_Kish = NA_real_,
      R2_pondere = NA_real_, nb_coefficients_non_identifies = NA_integer_,
      formule = paste(deparse(m$formule), collapse = " "),
      termes_constants_retires = "", erreur = m$erreur
    ))
  }
  d <- donnees_modele(m)
  w <- d$.poids
  y <- d$.reponse
  moyenne <- weighted.mean(y, w)
  erreur_residuelle <- sum(w * (y - d$.prediction)^2)
  dispersion_totale <- sum(w * (y - moyenne)^2)
  
  data.frame(
    groupe = groupe, modele = nom, n = nrow(d),
    ESS_Kish = sum(w)^2 / sum(w^2),
    R2_pondere = if (dispersion_totale > 0) {
      1 - erreur_residuelle / dispersion_totale
    } else NA_real_,
    nb_coefficients_non_identifies = sum(is.na(m$coefficients)),
    formule = paste(deparse(formula(m)), collapse = " "),
    termes_constants_retires = paste(attr(m, "termes_constants_retires"), collapse = " / "),
    erreur = ""
  )
}

echapper_latex <- function(x) {
  caracteres <- c("\\", "&", "%", "$", "#", "_", "{", "}", "~", "^", "<", ">")
  remplacements <- c("\\textbackslash{}", "\\&", "\\%", "\\$", "\\#", "\\_",
                     "\\{", "\\}", "\\textasciitilde{}", "\\textasciicircum{}",
                     "\\textless{}", "\\textgreater{}")
  dictionnaire <- setNames(remplacements, caracteres)
  x <- as.character(x)
  x[is.na(x)] <- ""
  vapply(x, function(z) {
    lettres <- strsplit(gsub("[\r\n\t]", " ", z), "", fixed = TRUE)[[1]]
    paste(vapply(lettres, function(a) {
      if (a %in% names(dictionnaire)) dictionnaire[[a]] else a
    }, character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

formater <- function(x, dec = 3) {
  ifelse(is.finite(x), formatC(x, digits = dec, format = "f", decimal.mark = ","), "")
}
etoiles <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**",
                                                      ifelse(p < .05, "*", ""))))
}

exporter_table <- function(modeles, nom_fichier, titre = nom_fichier) {
  # Le CSV numérique long reste la référence pour comparer les calculs.
  longs <- bind_rows(lapply(names(modeles), function(nom) {
    extraire_coefficients(modeles[[nom]], nom, nom_fichier)
  }))
  diagnostics <- bind_rows(lapply(names(modeles), function(nom) {
    diagnostic_modele(modeles[[nom]], nom, nom_fichier)
  }))
  ecrire_csv(longs, paste0(nom_fichier, "_coefficients.csv"))
  ecrire_csv(diagnostics, paste0(nom_fichier, "_diagnostics.csv"))
  if (!nrow(longs)) return(invisible(NULL))
  
  noms <- names(modeles)
  termes <- unique(longs$terme)
  lignes <- c(termes, "N non pondéré", "R2 pondéré", "ESS Kish (indicatif)")
  large <- data.frame(terme = lignes, stringsAsFactors = FALSE)
  cellules_tex <- matrix("", length(lignes), length(noms))
  
  for (j in seq_along(noms)) {
    m <- modeles[[noms[j]]]
    colonne <- rep("", length(lignes))
    if (modele_valide(m)) {
      cc <- longs[longs$modele == noms[j], , drop = FALSE]
      for (i in seq_along(termes)) {
        k <- match(termes[i], cc$terme)
        if (!is.na(k)) {
          estimation <- paste0(formater(cc$estimation[k]), etoiles(cc$p[k]))
          se <- paste0("(", formater(cc$ecart_type[k]), ")")
          colonne[i] <- paste(estimation, se)
          cellules_tex[i, j] <- paste0("\\shortstack{", estimation, "\\\\", se, "}")
        }
      }
      dd <- diagnostics[diagnostics$modele == noms[j], , drop = FALSE]
      supplement <- c(as.character(dd$n), formater(dd$R2_pondere), formater(dd$ESS_Kish, 1))
      colonne[length(termes) + 1:3] <- supplement
      cellules_tex[length(termes) + 1:3, j] <- supplement
    } else {
      colonne[] <- "Non estimé"
      cellules_tex[, j] <- "\\textit{Non estimé}"
    }
    large[[noms[j]]] <- colonne
  }
  ecrire_csv(large, paste0(nom_fichier, "_table.csv"))
  
  # Colonnes à largeur fixe : retour à la ligne des noms longs et tables
  # pouvant se poursuivre sur la page suivante grâce à longtable.
  largeur <- formatC(.62 / length(noms), format = "f", digits = 4, decimal.mark = ".")
  definition <- paste0("p{0.30\\textwidth}", paste0(
    rep(paste0("p{", largeur, "\\textwidth}"), length(noms)), collapse = ""))
  entete <- paste(c("Variable", echapper_latex(gsub("_", " ", noms, fixed = TRUE))),
                  collapse = " & ")
  latex <- c(
    "% Généré par 2_Regressions_lineaires.R ; UTF-8 ; requiert longtable.",
    "\\begingroup", "\\scriptsize", "\\setlength{\\tabcolsep}{2pt}",
    paste0("\\begin{longtable}{", definition, "}"),
    paste0("\\caption{", echapper_latex(titre), "}\\\\"),
    "\\hline", paste0(entete, " \\\\"), "\\hline", "\\endfirsthead",
    "\\hline", paste0(entete, " \\\\"), "\\hline", "\\endhead"
  )
  for (i in seq_along(lignes)) {
    latex <- c(latex, paste0(paste(c(echapper_latex(lignes[i]), cellules_tex[i, ]),
                                   collapse = " & "), " \\\\"))
  }
  note <- paste(
    "Régressions linéaires ; erreurs-types survey entre parenthèses.",
    "Poids et calage selon la colonne ; Sans poids désigne l'annexe non pondérée.",
    "* p < 0,05 ; ** p < 0,01 ; *** p < 0,001.",
    "IC95 % et p exacts dans le CSV numérique. ESS : dispersion des poids seulement.",
    "Une colonne vide ou non estimée doit être vérifiée dans les diagnostics."
  )
  latex <- c(latex, "\\hline", "\\end{longtable}",
             paste0("\\noindent\\parbox{\\textwidth}{", echapper_latex(note), "}"),
             "\\endgroup")
  writeLines(latex, file.path(dossier_sortie, paste0(nom_fichier, ".tex")), useBytes = FALSE)
  invisible(longs)
}

groupes_modeles <- list()
for (y in names(modeles_positions)) groupes_modeles[[paste0("Positions_", y)]] <- modeles_positions[[y]]
groupes_modeles$Bloc_D_offre_verte <- modeles_D
for (y in names(modeles_E)) groupes_modeles[[paste0("Bloc_E_", y)]] <- modeles_E[[y]]
for (y in names(modeles_questions)) groupes_modeles[[paste0("Questions_", y)]] <- modeles_questions[[y]]
for (y in names(modeles_interactions)) groupes_modeles[[paste0("Interactions_", y)]] <- modeles_interactions[[y]]
for (y in names(modeles_sensibilite)) groupes_modeles[[paste0("Sensibilite_", y)]] <- modeles_sensibilite[[y]]
for (y in names(modeles_territoire)) groupes_modeles[[paste0("Territoire_", y)]] <- modeles_territoire[[y]]
for (y in names(modeles_complements)) groupes_modeles[[paste0("Complement_", y)]] <- modeles_complements[[y]]

for (nom in names(groupes_modeles)) {
  exporter_table(groupes_modeles[[nom]], nom, gsub("_", " ", nom, fixed = TRUE))
}
coefficients_tous <- bind_rows(lapply(names(groupes_modeles), function(g) {
  bind_rows(lapply(names(groupes_modeles[[g]]), function(nom) {
    extraire_coefficients(groupes_modeles[[g]][[nom]], nom, g)
  }))
}))
diagnostics_tous <- bind_rows(lapply(names(groupes_modeles), function(g) {
  bind_rows(lapply(names(groupes_modeles[[g]]), function(nom) {
    diagnostic_modele(groupes_modeles[[g]][[nom]], nom, g)
  }))
}))
ecrire_csv(coefficients_tous, "coefficients_tous_modeles.csv")
ecrire_csv(diagnostics_tous, "diagnostic_modeles.csv")

# BH sur les familles de tests indiquées, pas sur toutes les analyses à la fois.
tests <- bind_rows(tests_globaux)
tests$p_BH <- NA_real_
for (famille in setdiff(unique(tests$famille), "Exploratoire")) {
  i <- tests$famille == famille & is.finite(tests$p)
  tests$p_BH[i] <- p.adjust(tests$p[i], method = "BH")
}
ecrire_csv(tests, "tests_globaux_et_BH.csv")

# Contrastes de l'interaction D : deux pentes et leur différence.
contraste_lineaire <- function(m, termes, poids, nom) {
  if (!modele_valide(m) || anyNA(m$coefficients)) return(data.frame())
  if (!all(termes %in% names(coef(m)))) return(data.frame())
  a <- setNames(rep(0, length(coef(m))), names(coef(m)))
  a[termes] <- poids
  estimation <- sum(a * coef(m))
  se <- sqrt(max(0, as.numeric(t(a) %*% vcov(m) %*% a)))
  ddl <- ddl_modele(m)
  q <- qt(.975, df = ddl)
  data.frame(contraste = nom, estimation = estimation, ecart_type = se,
             IC95_inf = estimation - q * se, IC95_sup = estimation + q * se,
             p = if (se > 0) 2 * pt(-abs(estimation / se), df = ddl) else NA_real_)
}
contrastes_D <- bind_rows(
  contraste_lineaire(modeles_D$Interaction, "vert_c", 1, "Pente non-sceptiques"),
  contraste_lineaire(modeles_D$Interaction, c("vert_c", "vert_c:scepticisme"),
                     c(1, 1), "Pente sceptiques"),
  contraste_lineaire(modeles_D$Interaction, "vert_c:scepticisme", 1,
                     "Différence de pente")
)
ecrire_csv(contrastes_D, "contrastes_interaction_offre_verte.csv")

# Le modèle avec le total impose la même pente à une bonne réponse de réseau
# et de fournisseur. B3 permet de tester cette restriction, réponse par réponse.
# Ce test porte sur les associations avec la réponse, pas sur l'unidimensionnalité
# du score de connaissance.
contrastes_domaines <- bind_rows(lapply(names(modeles_positions), function(y) {
  z <- contraste_lineaire(
    modeles_positions[[y]]$B3_Deux_domaines,
    c("conn_reseau", "conn_fournisseurs"), c(1, -1),
    "Coefficient réseau moins coefficient fournisseurs"
  )
  if (nrow(z)) z$reponse <- y
  z
}))
if (nrow(contrastes_domaines)) {
  contrastes_domaines$p_BH <- p.adjust(contrastes_domaines$p, method = "BH")
}
ecrire_csv(contrastes_domaines, "contrastes_reseau_fournisseurs.csv")

# Comparer aussi les amplitudes par un écart-type pondéré du score.
# La standardisation est calculée dans l'échantillon commun de chaque réponse.
comparaison_scores <- list()
for (y in names(modeles_positions)) {
  correspondance <- c(B1_Reseau = "conn_reseau", B2_Total = "score_connaissance")
  for (nom in names(correspondance)) {
    v <- correspondance[[nom]]
    m <- modeles_positions[[y]][[nom]]
    if (!modele_valide(m) || !v %in% names(coef(m))) next
    d <- donnees_modele(m)
    w <- d$.poids
    moyenne_score <- weighted.mean(d[[v]], w)
    sd_score <- sqrt(weighted.mean((d[[v]] - moyenne_score)^2, w))
    cc <- extraire_coefficients(m, nom, y)
    cc <- cc[cc$terme == v, , drop = FALSE]
    comparaison_scores[[paste(y, nom)]] <- data.frame(
      reponse = y, modele = nom, score = v, n = nrow(d), SD_score = sd_score,
      effet_par_1_SD = cc$estimation * sd_score,
      IC95_inf = cc$IC95_inf * sd_score, IC95_sup = cc$IC95_sup * sd_score
    )
  }
}
ecrire_csv(bind_rows(comparaison_scores), "comparaison_scores_par_1_SD.csv")

saveRDS(list(
  modeles = groupes_modeles, coefficients = coefficients_tous,
  diagnostics = diagnostics_tous, tests = tests, contrastes_D = contrastes_D,
  contrastes_domaines = contrastes_domaines,
  correlations = correlations, descriptifs = descriptifs,
  diagnostic_redressement = diagnostic_redressement,
  parametres = list(variable_zone = variable_zone, reference_revenu = levels(df$revenu)[1],
                    classements_complets = classements_complets,
                    colonne_nsp_acceptabilite = colonne_nsp_acceptabilite)
), file.path(dossier_sortie, "resultats_regressions_lineaires.rds"))
capture.output(sessionInfo(), file = file.path(dossier_sortie, "sessionInfo.txt"))
writeLines(c(
  "Tables générées par 2_Regressions_lineaires.R.",
  "Tous les modèles : gaussiens à lien identité, y compris les issues binaires.",
  "Pour LaTeX : ajouter \\usepackage{longtable} au préambule, puis \\input{chemin.tex}.",
  "Chaque .tex a un CSV large (_table.csv) et un CSV numérique (_coefficients.csv).",
  "coefficients_tous_modeles.csv contient les estimations, SE, IC95 %, p et n.",
  "Les comparaisons A0/A1/A2/B1/B2/B3/C/F sont à échantillon identique par réponse.",
  "En sensibilité, Sans_poids et Calage_age_zone_seulement reprennent cet échantillon.",
  "Les sous-domaines Longs et Contrôles observés changent explicitement d'échantillon.",
  "Les coefficients des issues _pts sont en points sur une échelle 0-100.",
  "Les probabilités linéaires peuvent être prédites hors de [0,100].",
  "Les intervalles de confiance sont de Wald-Student avec les ddl survey.",
  "BH ne porte que sur les familles indiquées dans tests_globaux_et_BH.csv.",
  "Autre est conservé dans les modèles principaux ; les modèles HF sont une annexe.",
  "Vérifier les coefficients non identifiés, surtout les interactions tous sexes.",
  "Une colonne vide / non estimée n'est pas un résultat nul.",
  "Vérifier audit_libelles.csv, références et diagnostics avant interprétation."
), file.path(dossier_sortie, "LIRE_LES_SORTIES.txt"), useBytes = FALSE)

# Réattacher le plan à df pour permettre un nettoyage ultérieur sans le perdre.
attr(df, "plan_redresse") <- plan_complet
attr(df, "diagnostic_redressement") <- diagnostic_redressement

cat("\nLes tables CSV et LaTeX sont dans :", dossier_sortie, "\n")
cat("Vérifier en priorité diagnostic_modeles.csv et audit_libelles.csv.\n")
# Pour ne conserver que df après consultation :
# rm(list = setdiff(ls(all.names = TRUE), "df"))

