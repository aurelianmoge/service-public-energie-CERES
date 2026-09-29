# Exécuter depuis la racine du projet. Réutiliser le redressement s'il est chargé.
if (!exists("df") || !"poids_norm" %in% names(df))
  source("2. Scripts/1. Redressement V4 (final).R")

# ============================================================================
# AFM DES RAPPORTS AU SERVICE PUBLIC DE L'ENERGIE
# Le redressement est chargé automatiquement si nécessaire.
# L'analyse porte sur les répondants du questionnaire long déjà redressés.
#
# Question : comment s'articulent connaissance du secteur, conception du
# service public, préférences de transition et intérêt pour l'autoconsommation ?
# Les propriétés sociales et matérielles décrivent l'espace sans le construire.
#
# Installation préalable, une seule fois :
# install.packages(c("FactoMineR", "missMDA", "ggplot2", "ggrepel"))
#
# L'AFM équilibre quatre thèmes définis sociologiquement. Une FAMD sans groupes
# laisserait chaque question peser seule ; une ACM demanderait de discrétiser
# les échelles 1-5. La CAH est une typologie exploratoire, dépendante des axes
# conservés, du nombre de classes et des réponses imputées.
# ============================================================================

#### 0. Paramètres et contrôles ####

dossier_sortie <- "3. Outputs/AFM"
ncp_imputation <- 3L       # composantes de l'imputation régularisée
ncp_afm <- 5L              # axes conservés dans l'AFM et utilisés dans la CAH
max_manquants_variable <- 0.15
max_manquants_individu <- 0.30
seuil_effectif_modalite <- 30L  # pour les tableaux et graphiques interprétatifs
faire_cah <- TRUE          # produire aussi les cartes et profils des classes
nombre_classes <- 4L       # choix exploratoire, à discuter sur le dendrogramme

for (pkg in c("FactoMineR", "missMDA", "ggplot2", "ggrepel")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Installer le package manquant : ", pkg)
}
if (!exists("df") || !is.data.frame(df) || !"poids_norm" %in% names(df))
  stop("Exécuter d'abord le redressement : df et df$poids_norm sont requis.")
dir.create(dossier_sortie, recursive = TRUE, showWarnings = FALSE)
exporter <- function(x, fichier) {
  utils::write.csv2(x, file.path(dossier_sortie, fichier),
                    row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

# PDF Unicode : Quartz sur macOS, Cairo sur les autres plateformes.
pdf_afm <- function(filename, width = 10, height = 7, ...) {
  if (identical(Sys.info()[["sysname"]], "Darwin"))
    grDevices::quartz(type = "pdf", file = filename, width = width, height = height, ...)
  else grDevices::cairo_pdf(filename, width = width, height = height, ...)
}

#### 1. Population d'analyse : questionnaire long ####

# Identifier la version par son étiquette ou par les identifiants de df_long.
version <- intersect(c("version_questionnaire", "version"), names(df))
if (length(version)) {
  est_long <- grepl("long", as.character(df[[version[1]]]), ignore.case = TRUE)
} else if (exists("df_long") && is.data.frame(df_long) &&
           "IdBilendi" %in% names(df) && "IdBilendi" %in% names(df_long)) {
  # Si les identifiants se répètent entre deux vagues, la vague les distingue.
  cle_long <- if (anyDuplicated(df$IdBilendi) &&
                  "vague" %in% names(df) && "vague" %in% names(df_long))
    c("vague", "IdBilendi") else "IdBilendi"
  cle <- function(x) do.call(paste, c(lapply(x[cle_long], as.character), sep = "||"))
  k_df <- cle(df)
  k_long <- cle(df_long)
  if (anyNA(df[cle_long]) || anyNA(df_long[cle_long]) ||
      anyDuplicated(k_df) || anyDuplicated(k_long))
    stop("Identifiants long/court ambigus : renseigner df$version_questionnaire.")
  est_long <- k_df %in% k_long
} else {
  stop("Impossible d'identifier sans ambiguïté les questionnaires longs. ",
       "Ajouter df$version_questionnaire ('long'/'court') avant ce script, ",
       "ou conserver df_long et des IdBilendi uniques après redressement.")
}
if (anyNA(est_long) || sum(est_long) < 100L)
  stop("Sélection du questionnaire long à vérifier (moins de 100 cas).")
df_afm <- df[est_long, , drop = FALSE]
ligne_df <- which(est_long)
poids <- as.numeric(df_afm$poids_norm)
if (anyNA(poids) || any(!is.finite(poids)) || any(poids <= 0))
  stop("Les poids doivent être numériques, finis et strictement positifs.")
poids <- poids * length(poids) / sum(poids)
message(nrow(df_afm), " questionnaires longs avant contrôle des réponses.")

#### 2. Recodages : questions et modalités des deux dictionnaires ####

manquant_technique <- function(x) {
  y <- trimws(as.character(x))
  is.na(y) | y %in% c("", "NA", "N/A")
}
nsp <- function(x) {
  manquant_technique(x) |
    trimws(as.character(x)) == "Ne sait pas / Ne se prononce pas"
}
echelle_1_5 <- function(x, nom) {
  y <- trimws(as.character(x))
  y[nsp(x)] <- NA_character_
  valide <- is.na(y) | grepl("^[1-5](\\D|$)", y)
  if (!all(valide)) stop("Valeur non reconnue dans ", nom, " : ",
                         paste(unique(y[!valide]), collapse = " | "))
  as.numeric(sub("^([1-5]).*$", "\\1", y))
}
# Chaque réponse substantielle doit rencontrer exactement un motif.
recode_choix <- function(x, motifs, libelles, nom) {
  y <- trimws(as.character(x))
  y[nsp(x)] <- NA_character_
  nb <- rep(0L, length(y))
  z <- rep(NA_character_, length(y))
  for (i in seq_along(motifs)) {
    ok <- !is.na(y) & grepl(motifs[i], y, ignore.case = TRUE)
    nb[ok] <- nb[ok] + 1L
    z[ok] <- libelles[i]
  }
  if (any(!is.na(y) & nb != 1L))
    stop("Modalité non reconnue ou ambiguë dans ", nom, " : ",
         paste(unique(y[!is.na(y) & nb != 1L]), collapse = " | "))
  factor(z, levels = libelles)
}

necessaires <- c("G3Q00001", paste0("G3Q00002_SQ00", 1:4),
                 "G3Q00004", "G3Q00006", "G3Q00008", "G3Q00009",
                 "G4Q00002_1", "G4Q00003_1", paste0("G4Q00005_SQ00", 1:4),
                 "G4Q00007", "G6Q00004")
absentes <- setdiff(necessaires, names(df_afm))
if (length(absentes)) stop("Questions actives absentes : ", paste(absentes, collapse = ", "))

# 2.1. Intérêt pour l'énergie et connaissance des acteurs du système électrique.
df_afm$interet_energie <- echelle_1_5(df_afm$G3Q00001, "G3Q00001")
roles <- c("Transporte l’électricité", "Distribue l’électricité",
           "Produit et fournit", "Produit et fournit")
items_roles <- paste0("G3Q00002_SQ00", 1:4)  # RTE, Enedis, EDF, Engie
bons_roles <- lapply(seq_along(roles), function(j) {
  reponse <- as.character(df_afm[[items_roles[j]]])
  # NSP explicite = 0 bonne réponse ; cellule réellement absente = NA.
  ifelse(manquant_technique(reponse), NA_integer_,
         as.integer(startsWith(trimws(reponse), roles[j])))
})
df_afm$score_connaissance <- ifelse(
  Reduce(`|`, lapply(bons_roles, is.na)), NA_real_,
  rowSums(do.call(cbind, bons_roles), na.rm = TRUE)
)

# 2.2. Définition du service public, égalité territoriale et organisation.
df_afm$service_public <- recode_choix(
  df_afm$G3Q00004,
  c("^Un organisme public", "^Une mission d'intérêt général", "^Les deux"),
  c("Service public : organisme", "Service public : mission",
    "Service public : les deux"), "G3Q00004")
df_afm$solidarite_territoriale <- recode_choix(
  df_afm$G3Q00006,
  c("^Adapter le coût", "^Maintenir la même qualité"),
  c("Territoires : adapter prix/service", "Territoires : égalité/solidarité"),
  "G3Q00006")
df_afm$centralisation <- echelle_1_5(df_afm$G3Q00008, "G3Q00008")
df_afm$concurrence <- echelle_1_5(df_afm$G3Q00009, "G3Q00009")
# 1 = centralisé ou monopole public ; 5 = local ou concurrence.

# 2.3. Premier choix pour la transition et consentement à payer une offre verte.
# Seul le premier rang est retenu : les rangs 2 à 7 dépendent du premier choix.
df_afm$levier_transition <- recode_choix(
  df_afm$G4Q00002_1,
  c("^Développer des mobilités", "^Développer les renouvelables",
    "^Efficacité énergétique", "^Maintenir ou développer le nucléaire",
    "^Optimisation du système", "^Sobriété"),
  paste0("Levier : ", c("mobilités", "renouvelables", "efficacité",
                        "nucléaire", "réseau", "sobriété")), "G4Q00002_1")
df_afm$production_prioritaire <- recode_choix(
  df_afm$G4Q00003_1,
  c("^Autre", "^Éolien", "^Gaz renouvelables", "^Hydroélectricité",
    "^Nucléaire", "^Solaire"),
  paste0("Production : ", c("autre", "éolien", "gaz renouvelable",
                            "hydraulique", "nucléaire", "solaire")), "G4Q00003_1")
df_afm$disposition_offre_verte <- echelle_1_5(df_afm$G6Q00004, "G6Q00004")
# 1 = certainement pas payer plus ; 5 = certainement prêt·e.

# 2.4. Connaissance préalable et préférence pour l'autoconsommation.
a <- lapply(paste0("G4Q00005_SQ00", 1:4), function(v)
  trimws(as.character(df_afm[[v]])))
oui <- lapply(a, function(x) !is.na(x) & x == "Oui")
sans_reponse <- Reduce(`&`, lapply(a, manquant_technique))
contradictoire <- ((oui[[3]] | oui[[4]]) & (oui[[1]] | oui[[2]])) |
  (oui[[3]] & oui[[4]])
connaissance <- ifelse(
  sans_reponse | contradictoire | oui[[3]], NA_character_,
  ifelse(oui[[4]], "Autoconso : inconnue",
         ifelse(oui[[1]] & oui[[2]], "Autoconso : deux formes connues",
                ifelse(oui[[1]], "Autoconso : individuelle connue",
                       ifelse(oui[[2]], "Autoconso : collective connue", NA_character_)))))
df_afm$connaissance_autoconso <- factor(connaissance)
df_afm$preference_autoconso <- recode_choix(
  df_afm$G4Q00007,
  c("^Aucun des deux", "^Autoconsommation collective",
    "^Autoconsommation individuelle"),
  paste0("Préférence : ", c("aucune", "collective", "individuelle")),
  "G4Q00007")

#### 3. Variables actives et supplémentaires ####

# Groupes théoriques : s = numérique standardisé, n = qualitatif, m = mixte.
groupes_actifs <- list(
  "Intérêt et connaissance" = c("interet_energie", "score_connaissance"),
  "Service public et gouvernance" = c("service_public", "solidarite_territoriale",
                                      "centralisation", "concurrence"),
  "Choix de transition" = c("levier_transition", "production_prioritaire",
                            "disposition_offre_verte"),
  "Autoconsommation" = c("connaissance_autoconso", "preference_autoconso")
)
types_actifs <- c("s", "m", "m", "n")
actives <- unlist(groupes_actifs, use.names = FALSE)
base_active <- as.data.frame(df_afm[, actives, drop = FALSE])

# Variables illustratives ; la vague permet de repérer un effet de terrain.
sup_souhaitees <- c("SEXE", "AGE_large", "G1Q00003", "zone2",
                    "G1Q00009", "G2Q00009", "G5Q00005", "vague")
sup_presentes <- intersect(sup_souhaitees, names(df_afm))

#### 4. Fréquences, valeurs manquantes et poids ####

diagnostic <- data.frame(
  groupe = rep(names(groupes_actifs), lengths(groupes_actifs)),
  variable = actives,
  manquants = vapply(base_active, function(x) sum(is.na(x)), integer(1)),
  pourcentage_manquant = 100 * vapply(base_active, function(x) mean(is.na(x)), numeric(1))
)
exporter(diagnostic, "01_diagnostic_variables.csv")
if (any(diagnostic$pourcentage_manquant > 100 * max_manquants_variable))
  stop("Plus de 15 % de manquants pour : ",
       paste(diagnostic$variable[diagnostic$pourcentage_manquant >
                                   100 * max_manquants_variable], collapse = ", "),
       ". Vérifier le filtre long, les routages et les recodages.")

manquants_individu <- rowMeans(is.na(base_active))
garder <- manquants_individu <= max_manquants_individu
if (sum(garder) < 100L) stop("Moins de 100 questionnaires suffisamment renseignés.")
base_active <- droplevels(base_active[garder, , drop = FALSE])
df_afm <- df_afm[garder, , drop = FALSE]
ligne_df <- ligne_df[garder]
poids <- poids[garder]
poids <- poids * length(poids) / sum(poids)
if (any(vapply(base_active, function(x) all(is.na(x)) ||
               (is.factor(x) && nlevels(x) < 2L) ||
               (is.numeric(x) && stats::sd(x, na.rm = TRUE) == 0), logical(1))))
  stop("Variable active constante ou entièrement manquante : revoir la sélection.")
message(nrow(base_active), " personnes conservées ; ESS = ",
        round(sum(poids)^2 / sum(poids^2)), " (poids renormalisés).")

# Les modalités rares peuvent créer des points très éloignés dans l'espace.
# Leur fréquence est conservée et doit être lue avant toute interprétation.
quali_actives <- names(base_active)[vapply(base_active, is.factor, logical(1))]
freq_avant <- do.call(rbind, lapply(quali_actives, function(v) {
  x <- as.character(base_active[[v]])
  lv <- levels(base_active[[v]])
  data.frame(variable = v, modalite = lv,
             n_observe = vapply(lv, function(z) sum(x == z, na.rm = TRUE), integer(1)),
             pct_pondere_observe = vapply(lv, function(z)
               100 * sum(poids[!is.na(x) & x == z]) / sum(poids[!is.na(x)]), numeric(1)))
}))
exporter(freq_avant, "02_frequences_modalites_observees.csv")

# Traçabilité de la sélection et de la dispersion des poids.
exporter(data.frame(etape = c("Longs après redressement", "Longs analysés"),
                    n = c(sum(est_long), nrow(base_active))), "00_population.csv")
exporter(data.frame(n = length(poids), ESS_Kish = sum(poids)^2 / sum(poids^2),
                    poids_min = min(poids), poids_max = max(poids),
                    cellules_imputees_pct = 100 * mean(is.na(base_active))),
         "00_indicateurs.csv")
# Comparer les marges du sous-échantillon long aux marges du panel redressé.
marges <- do.call(rbind, lapply(intersect(c("SEXE", "AGE_large", "zone2"), names(df)), function(v) {
  do.call(rbind, lapply(unique(as.character(df[[v]])), function(z) {
    data.frame(variable = v, modalite = z,
      panel_pondere_pct = 100 * sum(df$poids_norm[which(df[[v]] == z)]) / sum(df$poids_norm),
      longs_ponderes_pct = 100 * sum(poids[which(df_afm[[v]] == z)]) / sum(poids))
  }))
}))
exporter(marges, "00_marges_population.csv")

#### 5. Imputation, puis analyse factorielle multiple ####

# Imputation FAMD pondérée pour les groupes mixtes ; catégorie la plus probable.
# Sensibilité à ncp = 2/3/4 dans le second script ; profils sur réponses observées.
set.seed(20260927)
imp <- missMDA::imputeFAMD(base_active, ncp = ncp_imputation,
                           method = "Regularized", row.w = poids)
actives_imp <- as.data.frame(imp$completeObs)
if (anyNA(actives_imp)) stop("Imputation incomplète.")
actives_imp[quali_actives] <- lapply(actives_imp[quali_actives], factor)

# Groupe supplémentaire : NSP explicites et retrait des colonnes constantes.
nettoyer_sup <- function(x, nom) {
  y <- trimws(as.character(x))
  y[nsp(x)] <- "Non renseigné"
  factor(paste0(nom, " : ", y))
}
sup <- as.data.frame(lapply(sup_presentes, function(v)
  nettoyer_sup(df_afm[[v]], v)), check.names = FALSE)
names(sup) <- sup_presentes
sup <- sup[, vapply(sup, nlevels, integer(1)) > 1L, drop = FALSE]
if (ncol(sup) == 0L) stop("Aucune variable supplémentaire exploitable.")

base_afm <- cbind(actives_imp[, actives, drop = FALSE], sup)
res_afm <- FactoMineR::MFA(
  base = base_afm,
  group = c(lengths(groupes_actifs), ncol(sup)),
  type = c(types_actifs, "n"),
  name.group = c(names(groupes_actifs), "Profils illustratifs"),
  num.group.sup = length(groupes_actifs) + 1L,
  row.w = poids, ncp = ncp_afm, graph = FALSE
)

#### 6. Lire les axes : poids des groupes, variables, modalités ####

axes <- seq_len(min(3L, ncol(res_afm$ind$coord)))
inertie <- data.frame(axe = seq_len(nrow(res_afm$eig)),
                      valeur_propre = res_afm$eig[, 1],
                      pourcentage = res_afm$eig[, 2],
                      pourcentage_cumule = res_afm$eig[, 3])
exporter(inertie, "03_inertie.csv")

# Chaque contribution est un pourcentage propre à UN axe : ne pas comparer
# directement contribution de groupe, de variable et de modalité.
table_axes <- function(coord, contrib, cos2 = NULL, vtest = NULL,
                       categorie = "", axes = 1:3) {
  do.call(rbind, lapply(axes, function(j) data.frame(
    type = categorie, axe = j, element = rownames(coord),
    coordonnee = coord[, j], contribution_pct = if (is.null(contrib))
      NA_real_ else contrib[, j],
    cos2 = if (is.null(cos2)) NA_real_ else cos2[, j],
    v_test = if (is.null(vtest)) NA_real_ else vtest[, j]
  )))
}
groupes <- table_axes(res_afm$group$coord, res_afm$group$contrib,
                      res_afm$group$cos2, categorie = "groupe", axes = axes)
groupes <- groupes[groupes$element %in% names(groupes_actifs), ]
exporter(groupes[order(groupes$axe, -groupes$contribution_pct), ],
         "04_contributions_groupes.csv")

quant <- table_axes(res_afm$quanti.var$coord, res_afm$quanti.var$contrib,
                    res_afm$quanti.var$cos2, categorie = "échelle/score", axes = axes)
quali <- table_axes(res_afm$quali.var$coord, res_afm$quali.var$contrib,
                    res_afm$quali.var$cos2, res_afm$quali.var$v.test,
                    categorie = "modalité active", axes = axes)

# Effectifs observés dans la population retenue ; seuil pour le classement court.
freq <- freq_avant
chercher_modalite <- function(libelle) {
  candidats <- which(endsWith(libelle, freq$modalite))
  if (length(candidats) == 1L) candidats else NA_integer_
}
jointure <- vapply(quali$element, chercher_modalite, integer(1))
quali$variable <- freq$variable[jointure]
quali$n_observe <- freq$n_observe[jointure]
quali$pct_pondere_observe <- freq$pct_pondere_observe[jointure]
if (anyNA(jointure)) warning("Appariement incomplet des modalités : vérifier 05.")
exporter(quali[order(quali$axe, -quali$contribution_pct), ],
         "05_toutes_modalites_actives.csv")
clivantes <- quali[!is.na(quali$n_observe) &
                     quali$n_observe >= seuil_effectif_modalite, ]
clivantes <- do.call(rbind, lapply(axes, function(j)
  head(clivantes[clivantes$axe == j, ][order(
    -clivantes$contribution_pct[clivantes$axe == j]), ], 12L)))
exporter(clivantes, "06_modalites_clivantes.csv")
exporter(quant[order(quant$axe, -quant$contribution_pct), ],
         "07_variables_numeriques.csv")

illustratives <- table_axes(res_afm$quali.var.sup$coord, NULL,
                            res_afm$quali.var.sup$cos2,
                            res_afm$quali.var.sup$v.test,
                            categorie = "modalité illustrative", axes = axes)
exporter(illustratives[order(illustratives$axe,
                             -abs(illustratives$v_test)), ],
         "08_modalites_illustratives.csv")

# Fréquence des propriétés projetées : écarter « Non renseigné » et les petits
# effectifs des étiquettes des figures, sans supprimer ces cas de l'analyse.
freq_sup <- do.call(rbind, lapply(names(sup), function(v) {
  x <- as.character(sup[[v]])
  lv <- levels(sup[[v]])
  data.frame(variable = v, modalite = lv,
             n = vapply(lv, function(z) sum(x == z), integer(1)),
             pct_pondere = vapply(lv, function(z)
               100 * sum(poids[x == z]) / sum(poids), numeric(1)))
}))
exporter(freq_sup, "08b_frequences_illustratives.csv")

# Une variable illustrative qualitative n'a pas une seule coordonnée signée.
# η² mesure, pour chaque axe, l'association entre ses catégories et les scores
# des individus. Le calcul pondéré écarte les réponses non renseignées.
eta2 <- do.call(rbind, lapply(names(sup), function(v) {
  categorie <- as.character(sup[[v]])
  ok <- !endsWith(categorie, " : Non renseigné")
  do.call(rbind, lapply(axes, function(j) {
    x <- res_afm$ind$coord[ok, j]
    w <- poids[ok]
    g <- categorie[ok]
    mu <- stats::weighted.mean(x, w)
    inter <- sum(vapply(split(seq_along(g), g), function(i)
      sum(w[i]) * (stats::weighted.mean(x[i], w[i]) - mu)^2, numeric(1)))
    total <- sum(w * (x - mu)^2)
    data.frame(variable = v, axe = j,
               eta2_pondere = if (total > 0) inter / total else NA_real_,
               n_renseigne = sum(ok))
  }))
}))
exporter(eta2, "08c_variables_illustratives_eta2.csv")
# Lecture des axes : coordonnées, contributions, cos² et effectifs ; η² pour le social.

individus <- data.frame(
  ligne_df = ligne_df,
  IdBilendi = if ("IdBilendi" %in% names(df_afm))
    as.character(df_afm$IdBilendi) else NA_character_,
  poids = poids,
  n_manquants_avant_imputation = rowSums(is.na(base_active)),
  res_afm$ind$coord, check.names = FALSE
)
exporter(individus, "09_individus_coordonnees.csv")
saveRDS(list(afm = res_afm, groupes_actifs = groupes_actifs, poids = poids,
             ligne_df = ligne_df, base_active_observee = base_active,
             base_active_imputee = actives_imp, sup = sup),
        file.path(dossier_sortie, "resultat_afm.rds"))

#### 7. Figures de l'espace factoriel ####

pdf_afm(file.path(dossier_sortie, "figure_1_inertie_groupes.pdf"),
               width = 11, height = 5)
op <- par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))
barplot(head(inertie$pourcentage, 8), names.arg = seq_len(min(8, nrow(inertie))),
        col = "#4169A0", border = NA, xlab = "Axe", ylab = "Inertie (%)",
        main = "Part d'inertie par axe")
barplot(t(res_afm$group$contrib[names(groupes_actifs), 1:2, drop = FALSE]),
        beside = TRUE, col = c("#4169A0", "#CD704A"), las = 2,
        cex.names = .7, ylab = "Contribution (%)",
        main = "Contribution des groupes aux axes 1 et 2")
legend("topright", legend = c("Axe 1", "Axe 2"),
       fill = c("#4169A0", "#CD704A"), bty = "n", cex = .8)
par(op)
grDevices::dev.off()

pdf_afm(file.path(dossier_sortie, "figure_2_modalites.pdf"),
               width = 11, height = 8)
op <- par(mfrow = c(1, 2), mar = c(11, 15, 3, 1))
for (j in 1:2) {
  a <- head(clivantes[clivantes$axe == j, ], 10L)
  if (nrow(a)) barplot(rev(a$contribution_pct), horiz = TRUE,
                       names.arg = rev(a$element), las = 1,
                       col = if (j == 1) "#4169A0" else "#CD704A",
                       border = NA, cex.names = .62,
                       xlab = "Contribution à l'axe (%)",
                       main = paste("Axe", j, "— modalités actives"))
}
par(op)
grDevices::dev.off()

pdf_afm(file.path(dossier_sortie, "figure_3_individus.pdf"),
               width = 9, height = 7)
xy <- res_afm$ind$coord[, 1:2, drop = FALSE]
plot(xy, pch = 16, cex = .5, col = grDevices::adjustcolor("#4169A0", .25),
     xlab = paste0("Axe 1 (", round(inertie$pourcentage[1], 1), " %)") ,
     ylab = paste0("Axe 2 (", round(inertie$pourcentage[2], 1), " %)") ,
     main = "Individus et centres des groupes d'âge")
abline(h = 0, v = 0, col = "grey70", lty = 2)
if ("AGE_large" %in% names(df_afm)) {
  age <- as.character(df_afm$AGE_large)
  for (modalite in unique(age[!is.na(age)])) {
    i <- which(age == modalite)
    if (length(i) < seuil_effectif_modalite) next
    centre <- colSums(xy[i, , drop = FALSE] * poids[i]) / sum(poids[i])
    points(centre[1], centre[2], pch = 21, bg = "#CD704A", cex = 1.3)
    text(centre[1], centre[2], labels = modalite, pos = 3, cex = .8)
  }
}
grDevices::dev.off()

# 7.1. Cercle des corrélations : seules les échelles/score actifs ont ici des
# flèches. Les variables qualitatives illustratives figurent sur la figure 6.
theme_afm <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                 panel.grid.minor = ggplot2::element_blank())
enregistrer <- function(p, nom, largeur = 10, hauteur = 7) {
  ggplot2::ggsave(file.path(dossier_sortie, nom), plot = p,
                  width = largeur, height = hauteur, units = "in", device = pdf_afm)
}
angles <- seq(0, 2 * pi, length.out = 240)
cercle <- data.frame(x = cos(angles), y = sin(angles))
correlations <- data.frame(
  variable = rownames(res_afm$quanti.var$cor),
  x = res_afm$quanti.var$cor[, 1],
  y = res_afm$quanti.var$cor[, 2],
  contribution_12 = rowSums(res_afm$quanti.var$contrib[, 1:2, drop = FALSE])
)
libelles_numeriques <- c(
  interet_energie = "Intérêt pour l'énergie",
  score_connaissance = "Connaissance des acteurs",
  centralisation = "Décentralisation (5)",
  concurrence = "Concurrence (5)",
  disposition_offre_verte = "Payer plus pour le vert (5)"
)
correlations$libelle <- unname(libelles_numeriques[correlations$variable])
p_cor <- ggplot2::ggplot() +
  ggplot2::geom_path(data = cercle, ggplot2::aes(x, y), colour = "grey70") +
  ggplot2::geom_hline(yintercept = 0, colour = "grey75") +
  ggplot2::geom_vline(xintercept = 0, colour = "grey75") +
  ggplot2::geom_segment(data = correlations,
                        ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
                        colour = "#4169A0",
                        arrow = grid::arrow(length = grid::unit(0.15, "cm"))) +
  ggrepel::geom_text_repel(data = correlations,
                           ggplot2::aes(x, y, label = libelle),
                           size = 3.5, max.overlaps = Inf, seed = 20260927) +
  ggplot2::coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
  ggplot2::labs(title = "Échelles actives et axes de l'AFM",
                subtitle = "Direction = corrélation ; la contribution exacte figure dans 07.",
                x = "Axe 1", y = "Axe 2") + theme_afm
enregistrer(p_cor, "figure_4_variables_numeriques.pdf", 9, 8)

# 7.2. Projection conjointe des modalités actives et illustratives.
# Les étiquettes actives sont choisies par contribution cumulée aux axes 1-2 ;
# les illustratives par |v-test| maximal et par effectif. Les autres modalités
# actives sont visibles en gris. Les deux types de points ont des statuts
# analytiques distincts, précisés dans la légende du graphique.
plan_actif <- data.frame(
  element = rownames(res_afm$quali.var$coord),
  x = res_afm$quali.var$coord[, 1], y = res_afm$quali.var$coord[, 2],
  contribution_12 = rowSums(res_afm$quali.var$contrib[, 1:2, drop = FALSE]),
  n_observe = freq$n_observe[vapply(rownames(res_afm$quali.var$coord),
                                    chercher_modalite, integer(1))]
)
actives_a_nommer <- plan_actif[!is.na(plan_actif$n_observe) &
                                 plan_actif$n_observe >= seuil_effectif_modalite, ]
actives_a_nommer <- head(actives_a_nommer[
  order(-actives_a_nommer$contribution_12), ], 12L)

plan_sup <- data.frame(
  element = rownames(res_afm$quali.var.sup$coord),
  x = res_afm$quali.var.sup$coord[, 1],
  y = res_afm$quali.var.sup$coord[, 2],
  force_association = apply(abs(res_afm$quali.var.sup$v.test[, 1:2,
                                                             drop = FALSE]), 1, max)
)
indice_sup <- vapply(plan_sup$element, function(z) {
  i <- which(endsWith(z, freq_sup$modalite))
  if (length(i) == 1L) i else NA_integer_
}, integer(1))
plan_sup$n <- freq_sup$n[indice_sup]
plan_sup$variable <- freq_sup$variable[indice_sup]
plan_sup <- plan_sup[!is.na(plan_sup$n) &
                       plan_sup$n >= seuil_effectif_modalite &
                       !endsWith(plan_sup$element, " : Non renseigné") &
                       plan_sup$variable %in% c("SEXE", "AGE_large", "G1Q00003", "zone2",
                                                "G2Q00009", "vague"), ]
plan_sup <- head(plan_sup[order(-plan_sup$force_association), ], 10L)
plan_sup$etiquette <- plan_sup$element
plan_sup$etiquette <- sub("^G1Q00003 : ", "Diplôme : ", plan_sup$etiquette)
plan_sup$etiquette <- sub("^G2Q00009 : ", "Factures : ", plan_sup$etiquette)
plan_sup$etiquette <- sub("^AGE_large : ", "Âge : ", plan_sup$etiquette)

p_modalites <- ggplot2::ggplot() +
  ggplot2::geom_hline(yintercept = 0, colour = "grey75") +
  ggplot2::geom_vline(xintercept = 0, colour = "grey75") +
  ggplot2::geom_point(data = plan_actif, ggplot2::aes(x, y),
                      colour = "grey80", size = 1.6) +
  ggplot2::geom_point(data = actives_a_nommer, ggplot2::aes(x, y),
                      colour = "#B34E3F", size = 2.5) +
  ggrepel::geom_text_repel(data = actives_a_nommer,
                           ggplot2::aes(x, y, label = element),
                           colour = "#9B382A", size = 3,
                           max.overlaps = Inf, seed = 20260927) +
  ggplot2::geom_point(data = plan_sup, ggplot2::aes(x, y),
                      colour = "#235E86", shape = 18, size = 3) +
  ggrepel::geom_text_repel(data = plan_sup,
                           ggplot2::aes(x, y, label = etiquette),
                           colour = "#235E86", size = 3,
                           max.overlaps = Inf, seed = 20260927) +
  ggplot2::coord_equal() +
  ggplot2::labs(title = "Modalités structurantes et propriétés des répondants",
                subtitle = paste("Rouge : actives contributives ; bleu : illustratives",
                                 "associées (≥ 30 répondants). Gris : autres actives."),
                x = paste0("Axe 1 (", round(inertie$pourcentage[1], 1), " %)"),
                y = paste0("Axe 2 (", round(inertie$pourcentage[2], 1), " %)")) +
  theme_afm
enregistrer(p_modalites, "figure_5_modalites_actives_illustratives.pdf", 13, 9)

# 7.3. Intensité de l'association des variables qualitatives illustratives.
libelles_sup <- c(
  SEXE = "Sexe", AGE_large = "Âge", G1Q00003 = "Diplôme",
  zone2 = "Territoire", G1Q00009 = "Gestion du contrat",
  G2Q00009 = "Difficulté à payer", G5Q00005 = "Changement de fournisseur",
  vague = "Vague d'enquête"
)
eta2$libelle <- unname(libelles_sup[eta2$variable])
p_eta <- ggplot2::ggplot(eta2,
                         ggplot2::aes(x = factor(axe), y = libelle, fill = eta2_pondere)) +
  ggplot2::geom_tile(colour = "white") +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", eta2_pondere)),
                     size = 3.3) +
  ggplot2::scale_fill_gradient(low = "#EEF3F8", high = "#235E86",
                               na.value = "white", name = "η²") +
  ggplot2::labs(title = "Quelles propriétés illustratives sont liées aux axes ?",
                subtitle = "η² pondéré : part de la dispersion de l'axe associée à la variable",
                x = "Axe", y = NULL) + theme_afm
enregistrer(p_eta, "figure_6_variables_illustratives.pdf", 8, 5)

#### 8. Deuxième temps : CAH sur les coordonnées de l'AFM ####

if (faire_cah) {
  # Ward pondéré, sans consolidation : le k-means interne de HCPC ignore les poids.
  # La coupure à quatre classes est comparée à 3/5 classes dans les prolongements.
  res_cah <- FactoMineR::HCPC(res_afm, nb.clust = nombre_classes,
                              consol = FALSE, nb.par = 5,
                              description = FALSE, graph = FALSE)
  classes <- as.character(res_cah$data.clust[
    match(rownames(res_afm$ind$coord), rownames(res_cah$data.clust)), "clust"])
  if (anyNA(classes)) stop("Alignement des individus de la CAH à vérifier.")
  classement <- data.frame(ligne_df = ligne_df, classe = classes,
                           poids = poids, xy, check.names = FALSE)
  exporter(classement, "10_classes_individus.csv")
  taille <- aggregate(poids, list(classe = classes), sum)
  names(taille)[2] <- "effectif_pondere"
  taille$effectif_brut <- as.integer(table(classes)[taille$classe])
  taille$part_ponderee_pct <- 100 * taille$effectif_pondere / sum(poids)
  exporter(taille, "11_taille_classes.csv")
  
  # Parangons : cinq personnes les plus proches du centre pondéré sur tous les axes.
  coord_cah <- as.matrix(res_afm$ind$coord)
  centres <- do.call(rbind, lapply(unique(classes), function(k) {
    i <- which(classes == k)
    m <- colSums(coord_cah[i, , drop = FALSE] * poids[i]) / sum(poids[i])
    data.frame(classe = k, t(m), check.names = FALSE)
  }))
  parangons <- do.call(rbind, lapply(unique(classes), function(k) {
    i <- which(classes == k)
    centre <- as.numeric(unlist(centres[centres$classe == k,
                                        -1, drop = FALSE], use.names = FALSE))
    distance <- sqrt(rowSums(sweep(coord_cah[i, , drop = FALSE], 2,
                                   centre, FUN = "-")^2))
    proches <- order(distance)[seq_len(min(5L, length(i)))]
    data.frame(classe = k, rang = seq_along(proches),
               ligne_df = ligne_df[i[proches]],
               IdBilendi = if ("IdBilendi" %in% names(df_afm))
                 as.character(df_afm$IdBilendi[i[proches]]) else NA_character_,
               distance_centre = distance[proches], poids = poids[i[proches]])
  }))
  exporter(centres, "12_centres_classes.csv")
  exporter(parangons, "13_parangons_classes.csv")
  saveRDS(res_cah, file.path(dossier_sortie, "resultat_cah.rds"))
  pdf_afm(file.path(dossier_sortie, "figure_7_dendrogramme.pdf"),
                 width = 10, height = 6)
  plot(res_cah, choice = "tree", ind.names = FALSE,
       title = "CAH sur les axes de l'AFM")
  grDevices::dev.off()
  
  # Deux cartes des individus : la première montre la séparation habituelle,
  # la seconde révèle d'éventuelles différences masquées sur les axes 1 et 2.
  for (plan in list(c(1L, 2L), c(2L, 3L))) {
    pts <- data.frame(x = coord_cah[, plan[1]], y = coord_cah[, plan[2]],
                      classe = factor(classes))
    pc <- data.frame(x = centres[[plan[1] + 1L]],
                     y = centres[[plan[2] + 1L]], classe = centres$classe)
    p_classes <- ggplot2::ggplot(pts,
                                 ggplot2::aes(x, y, colour = classe)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey80") +
      ggplot2::geom_vline(xintercept = 0, colour = "grey80") +
      ggplot2::geom_point(alpha = .23, size = 1) +
      ggplot2::geom_point(data = pc, ggplot2::aes(x, y),
                          inherit.aes = FALSE, shape = 21,
                          fill = "white", colour = "black", size = 4) +
      ggrepel::geom_label_repel(data = pc,
                                ggplot2::aes(x, y, label = paste("Classe", classe)),
                                inherit.aes = FALSE, max.overlaps = Inf,
                                seed = 20260927, size = 3.5) +
      ggplot2::labs(title = "Classes de la CAH dans l'espace de l'AFM",
                    subtitle = "Points : individus ; cercles : centres pondérés",
                    x = paste0("Axe ", plan[1], " (",
                               round(inertie$pourcentage[plan[1]], 1), " %)"),
                    y = paste0("Axe ", plan[2], " (",
                               round(inertie$pourcentage[plan[2]], 1), " %)"),
                    colour = "Classe") + theme_afm
    enregistrer(p_classes,
                paste0("figure_", if (plan[1] == 1L) "8" else "9",
                       "_classes_axes_", plan[1], "_", plan[2], ".pdf"), 10, 8)
  }
  
  # Profils sur réponses observées : dénominateur = réponses renseignées à la question.
  # Un ratio > 1 signifie que la modalité est surreprésentée dans la classe.
  profils <- cbind(base_active[quali_actives],
                   df_afm[intersect(c("SEXE", "AGE_large", "G1Q00003",
                                      "zone2", "G2Q00009"), names(df_afm))])
  profil <- do.call(rbind, lapply(names(profils), function(v) {
    x <- as.character(profils[[v]])
    x[nsp(x)] <- NA_character_
    do.call(rbind, lapply(unique(x[!is.na(x)]), function(modalite) {
      cible <- !is.na(x) & x == modalite
      global <- sum(poids[cible]) / sum(poids[!is.na(x)])
      do.call(rbind, lapply(unique(classes), function(k) {
        dans <- classes == k & !is.na(x)
        p <- sum(poids[dans & cible]) / sum(poids[dans])
        data.frame(classe = k, variable = v, modalite = modalite,
                   effectif_brut = sum(dans & cible),
                   n_renseigne_classe = sum(dans),
                   poids_renseigne_classe = sum(poids[dans]),
                   part_classe_pct = 100 * p,
                   part_globale_pct = 100 * global,
                   ratio = p / global)
      }))
    }))
  }))
  exporter(profil[order(profil$classe, -profil$ratio), ],
           "14_profils_classes.csv")
  
  # Moyennes pondérées des réponses observées, avec dénominateurs par question.
  variables_num <- names(actives_imp)[vapply(actives_imp, is.numeric, logical(1))]
  moyennes <- do.call(rbind, lapply(variables_num, function(v)
    do.call(rbind, lapply(unique(classes), function(k) {
      i <- which(classes == k)
      data.frame(classe = k, variable = v,
                 moyenne_ponderee = stats::weighted.mean(base_active[[v]][i],
                                                         poids[i], na.rm = TRUE),
                 n_renseigne = sum(!is.na(base_active[[v]][i])))
    }))))
  exporter(moyennes, "15_moyennes_classes.csv")
  
  # Vue synthétique : cinq modalités surreprésentées par classe, avec au moins
  # 30 personnes et 5 % de la classe. Les chiffres complets sont dans 14.
  profils_visibles <- profil[profil$effectif_brut >= seuil_effectif_modalite &
                               profil$part_classe_pct >= 5 &
                               is.finite(profil$ratio), ]
  profils_visibles <- do.call(rbind, lapply(unique(classes), function(k)
    head(profils_visibles[profils_visibles$classe == k, ][
      order(-profils_visibles$ratio[profils_visibles$classe == k]), ], 5L)))
  if (nrow(profils_visibles)) {
    profils_visibles$libelle <- paste(profils_visibles$variable,
                                      profils_visibles$modalite, sep = " : ")
    profils_visibles$libelle <- ifelse(nchar(profils_visibles$libelle) > 56,
                                       paste0(substr(profils_visibles$libelle, 1, 55), "…"),
                                       profils_visibles$libelle)
    p_profils <- ggplot2::ggplot(profils_visibles,
                                 ggplot2::aes(x = ratio, y = stats::reorder(libelle, ratio))) +
      ggplot2::geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
      ggplot2::geom_col(fill = "#4169A0") +
      ggplot2::facet_wrap(~ classe, scales = "free_y", ncol = 2) +
      ggplot2::labs(title = "Modalités distinctives des classes",
                    subtitle = "Rapport : part dans la classe / part dans l'ensemble ; 1 = moyenne",
                    x = "Surreprésentation (rapport)", y = NULL) + theme_afm
    enregistrer(p_profils, "figure_10_profils_classes.pdf", 13, 9)
  }
}

message("AFM terminée : ", dossier_sortie,
        ". Lire d'abord 03–08, puis 09 ; si CAH activée, 11–15.")

# Comparaisons : « 5. CAH prolongements.R » ; présentation : « 4. Rapports/AFM_dashboard.Rmd ».
writeLines(capture.output(sessionInfo()), file.path(dossier_sortie, "sessionInfo.txt"))
