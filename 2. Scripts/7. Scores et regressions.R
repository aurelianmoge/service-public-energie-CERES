# Indicateurs et régressions pondérées.
#
# On construit les indicateurs, on vérifie que ceux à plusieurs items tiennent
# (leurs items doivent être corrélés), puis on les organise selon leur rôle
# 7 positions expliquées, 6 variables explicatives liées
# chacune à une hypothèse, et des indicateurs secondaires seulement décrits. Les
# régressions sont réparties en blocs A à F (voir la partie 4). Modèle principal :
# MCO ou modèle linéaire de probabilité pondérés (svyglm) ; robustesse : logit ou
# logit ordonné.
#
# À lancer depuis la racine du dépôt. Packages : tidyverse et survey (déjà
# chargés par le redressement) ; polycor est facultatif (corrélations
# tétrachoriques de la partie 2).


###############################
### Chargement des données ###
###############################

if (!exists("df") || !"poids_norm" %in% names(df))
  source("2. Scripts/1. Redressement.R")
library(survey)

dossier_sortie <- getOption("energie.reg.sortie", "3. Outputs/Regressions")
dir.create(dossier_sortie, recursive = TRUE, showWarnings = FALSE)

df$long <- df$version_questionnaire == "long"
w <- df$poids_norm

txt <- function(x) trimws(as.character(x))
est_nsp <- function(x) is.na(x) | txt(x) %in% c("", "N/A", "Ne sait pas / Ne se prononce pas")
# Échelle 1-5 : NSP -> NA
echelle <- function(x) {
  y <- txt(x); y[est_nsp(x)] <- NA
  stopifnot(all(is.na(y) | grepl("^[1-5]( |$)", y)))
  as.numeric(substr(y, 1, 1))
}
# Case à cocher : Oui = 1, Non = 0, N/A (a coché NSP ou "Aucune") -> NA
coche <- function(x) ifelse(txt(x) == "Oui", 1, ifelse(txt(x) == "Non", 0, NA))


#################################
### 1. Construction des scores ###
#################################

# Connaissance du réseau (G3Q00002) : RTE + Enedis, de 0 à 2, NSP = faux.
# Le total sur 4 mélangeait deux savoirs sans lien entre eux : ceux qui
# connaissent RTE connaissent Enedis, ceux qui connaissent EDF connaissent
# Engie, mais les deux blocs ne sont pas corrélés (voir partie 2). On garde le
# réseau, qui correspond à la question (comment l'électricité arrive-t-elle ?).
bonnes <- c(SQ001 = "^Transporte", SQ002 = "^Distribue",   # RTE, Enedis
            SQ003 = "^Produit",    SQ004 = "^Produit")     # EDF, Engie
conn_items <- sapply(names(bonnes), function(s)
  as.numeric(grepl(bonnes[s], txt(df[[paste0("G3Q00002_", s)]]))))
colnames(conn_items) <- c("RTE", "Enedis", "EDF", "Engie")
df$conn_reseau <- conn_items[, "RTE"] + conn_items[, "Enedis"]

# Intérêt pour l'énergie (G3Q00001) et pour la transition (G4Q00001), de 1 à 5,
# corrélés à 0,63 mais gardés séparés. Seul l'intérêt pour l'énergie sert
# de variable explicative : l'intérêt pour la transition se comporte comme
# une adhésion (les pro-nucléaires s'intéressent plus à l'énergie mais moins à
# "la transition") ; il est seulement décrit.
df$interet_energie    <- echelle(df$G3Q00001)
df$interet_transition <- echelle(df$G4Q00001)

# Gestion décentralisée (G3Q00008) et concurrence (G3Q00009), de 1 à 5
df$local       <- echelle(df$G3Q00008)   # 1 très centralisée, 5 très décentralisée
df$concurrence <- echelle(df$G3Q00009)   # 1 monopole public, 5 concurrence

# Justice territoriale (G3Q00006) : 1 = même service partout, 0 = adapter le coût
df$justice_terr <- ifelse(est_nsp(df$G3Q00006), NA,
                          as.numeric(grepl("^Maintenir", txt(df$G3Q00006))))

# Bloc « local » à trois niveaux, sans score : les items ne sont pas corrélés.
#   principe    : gestion décentralisée (G3Q00008, ci-dessus)
#   institution : les collectivités parmi les garants (G3Q00007_SQ004, tout le monde)
#   pratique    : porteur local de confiance (G4Q00006_SQ003) et préférence pour
#                 l'autoconsommation collective (G4Q00007), longs seulement
# G3Q00007 et G4Q00006 plafonnent à 3 cases : "Oui" veut dire "parmi ses 3 choix".
df$local_decentralise  <- ifelse(is.na(df$local), NA, as.numeric(df$local >= 4))
df$anc_collectivites   <- coche(df$G3Q00007_SQ004)
df$anc_porteur_local   <- ifelse(df$long, coche(df$G4Q00006_SQ003), NA)
g47 <- txt(df$G4Q00007)
df$anc_auto_collective <- ifelse(!df$long | est_nsp(g47), NA,
                                 as.numeric(g47 == "Autoconsommation collective"))

# Acceptabilité (G4Q00004) : mesure par mesure, pas de total. Accepter un bonus
# ne dit rien de l'acceptation d'un malus. "Aucune" = 0 partout ; NSP = NA.
acc_items <- sapply(1:5, function(i) coche(df[[paste0("G4Q00004_SQ00", i)]]))
colnames(acc_items) <- c("Bonus", "Malus", "Invest_public", "Regulation", "Information")
acc_items[txt(df$G4Q00004_SQ006) == "Oui", ] <- 0
df$acc_bonus <- acc_items[, "Bonus"]
df$acc_malus <- acc_items[, "Malus"]
df$carotte_baton <- factor(
  ifelse(df$acc_bonus == 1, ifelse(df$acc_malus == 1, "Les deux", "Carotte seule"),
                            ifelse(df$acc_malus == 1, "Bâton seul", "Ni l'un ni l'autre")),
  levels = c("Carotte seule", "Les deux", "Bâton seul", "Ni l'un ni l'autre"))

# Gestes (G7Q00001, longs) : part des gestes déjà faits parmi ceux qui concernent
# le répondant. Un locataire ne peut souvent ni isoler ni produire : compter sur 9
# le pénaliserait. Le compte sur 9 sert de robustesse.
gestes_lib <- c("Voiture", "Chauffage", "Heures_creuses", "Suivi_conso", "S_informer",
                "Equip_efficaces", "Gros_equip", "Isolation", "Production")
g7 <- sapply(1:9, function(i) txt(df[[paste0("G7Q00001_SQ00", i)]]))
colnames(g7) <- gestes_lib
fait     <- (g7 == "Déjà fait") * 1
concerne <- matrix(g7 != "Non concerné·e", nrow(g7))
fait[!df$long, ] <- NA; concerne[!df$long, ] <- NA
df$gestes_part <- ifelse(rowSums(concerne) > 0, rowSums(fait) / rowSums(concerne), NA)
df$gestes_n    <- rowSums(fait)
gestes_items <- ifelse(g7 == "Déjà fait", 1, ifelse(grepl("^Jamais", g7), 0, NA))
gestes_items[!df$long, ] <- NA

# Disposition à payer pour une offre verte crédible (G6Q00004, longs), de 1 à 5
df$vert <- echelle(df$G6Q00004)

# Offre verte effectivement souscrite (G6Q00001, longs) et scepticisme envers les
# offres vertes (G6Q00005_SQ006 "rien ne me convainc", longs ; le N/A de cet item
# correspond aux NSP). Ajoutés le 11/10 pour tester H5 : la disposition à payer
# ne devient un choix que si l'offre est jugée crédible.
df$offre_verte <- ifelse(!df$long | est_nsp(df$G6Q00001), NA,
                         as.numeric(txt(df$G6Q00001) == "Oui"))
df$scepticisme <- ifelse(df$long, coche(df$G6Q00005_SQ006), NA)

# Préférence pour le nucléaire (axe nucléaire / renouvelables). Le nucléaire apparaît dans deux classements posés à tout le monde :
# la production prioritaire (G4Q00003, 3 rangs) et les leviers de la transition
# (G4Q00002, 5 rangs). Ses rangs dans les deux questions sont corrélés à 0,68 :
# le score est la moyenne des deux rangs ramenés de 0 (absent) à 1 (premier).
rang <- function(prefixe, k, motif) {
  r <- sapply(1:k, function(j) grepl(motif, txt(df[[paste0(prefixe, j)]])))
  s <- apply(r, 1, function(z) if (any(z)) k + 1 - which(z)[1] else 0)
  ifelse(est_nsp(df[[paste0(prefixe, 1)]]), NA, s)
}
df$nuc_production <- rang("G4Q00003_", 3, "^Nucléaire")   # 3 = premier, 0 = absent
df$nuc_levier     <- rang("G4Q00002_", 5, "nucléaire")    # 5 = premier, 0 = absent
df$pro_nucleaire  <- (df$nuc_production / 3 + df$nuc_levier / 5) / 2
df$nuc_premier    <- as.numeric(df$nuc_production == 3)   # pour la robustesse

# Conception du service public. G3Q00004 : un organisme
# public / une mission d'intérêt général / les deux. G3Q00005 (3 choix au plus) :
# seule "l'absence d'objectif de profit" (SQ002, l'ordre des items ne suit pas
# leur numéro) distingue nettement les positions de gouvernance.
def_sp <- txt(df$G3Q00004)
df$sp_organisme <- ifelse(est_nsp(df$G3Q00004), NA, as.numeric(grepl("^Un organisme public", def_sp)))
df$sp_mission   <- ifelse(est_nsp(df$G3Q00004), NA, as.numeric(grepl("^Une mission", def_sp)))
df$sp_sans_profit <- coche(df$G3Q00005_SQ002)


#############################################
### 2. Les scores à plusieurs items tiennent-ils ? ###
#############################################

wcor <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y)
  cov.wt(cbind(x[ok], y[ok]), w[ok], cor = TRUE)$cor[1, 2]
}
# Alpha de Cronbach pondéré (= KR-20 sur des items binaires), cas complets
alpha_w <- function(M, w) {
  ok <- complete.cases(M); M <- M[ok, , drop = FALSE]; w <- w[ok]
  v <- function(x) cov.wt(cbind(x), w)$cov[1, 1]
  ncol(M) / (ncol(M) - 1) * (1 - sum(apply(M, 2, v)) / v(rowSums(M)))
}
# Pour deux items binaires
assoc <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y)
  if (!requireNamespace("polycor", quietly = TRUE)) return(wcor(x, y, w))
  suppressWarnings(polycor::polychor(xtabs(w[ok] ~ x[ok] + y[ok])))
}

coherence <- c(
  interet_r            = wcor(df$interet_energie, df$interet_transition, w),  # 0,63
  reseau_RTE_Enedis    = assoc(conn_items[, 1], conn_items[, 2], w),          # 0,61
  fourn_EDF_Engie      = assoc(conn_items[, 3], conn_items[, 4], w),          # 0,64
  reseau_x_fourn_r     = wcor(df$conn_reseau, conn_items[, 3] + conn_items[, 4], w),  # 0,01
  gestes_alpha         = alpha_w(gestes_items, w),                            # 0,67
  bonus_malus          = assoc(df$acc_bonus, df$acc_malus, w),                # 0,06
  local_principe_inst  = assoc(df$local_decentralise, df$anc_collectivites, w),   # 0,07
  local_inst_porteur   = assoc(df$anc_collectivites, df$anc_porteur_local, w),    # 0,11
  local_porteur_auto   = assoc(df$anc_porteur_local, df$anc_auto_collective, w),  # 0,20
  nuc_prod_x_levier    = wcor(df$nuc_production, df$nuc_levier, w),           # 0,68
  sp_organisme_x_sans_profit = assoc(df$sp_organisme, df$sp_sans_profit, w))  # ~0
print(round(coherence, 3))


###########################################
### 3. Contrôles, références fixées à la main ###
###########################################

dip <- txt(df$G1Q00003)
df$diplome <- factor(ifelse(dip %in% c("Aucun", "CAP-BEP"), "Infra-bac",
  ifelse(dip == "Bac", "Bac", ifelse(dip %in% c("Bac+2", "Bac+3"), "Bac+2/3",
  ifelse(dip == "Bac+5 et au-delà", "Bac+5", "Autre/NSP")))),
  levels = c("Bac", "Infra-bac", "Bac+2/3", "Bac+5", "Autre/NSP"))

# Revenu individuel : borne basse de la tranche (premier nombre une fois les
# espaces retirés, "10 000" pouvant contenir un espace insécable)
revenu_brut <- txt(df$G1Q00005)
sans_esp <- gsub("[[:space:]\u00a0\u202f]", "", revenu_brut)
borne <- rep(NA_real_, length(revenu_brut))
a_nombre <- grepl("[0-9]", sans_esp)
borne[a_nombre] <- as.numeric(regmatches(sans_esp, regexpr("[0-9]+", sans_esp)))
borne[grepl("^Moins", revenu_brut)] <- 0
df$revenu <- factor(ifelse(est_nsp(revenu_brut), "NSP",
  ifelse(borne < 1000, "< 1 000", ifelse(borne < 1500, "1 000-1 500",
  ifelse(borne < 2500, "1 500-2 500", ifelse(borne < 4000, "2 500-4 000", "4 000 et +"))))),
  levels = c("1 500-2 500", "< 1 000", "1 000-1 500", "2 500-4 000", "4 000 et +", "NSP"))
stopifnot(!anyNA(df$revenu))

df$age  <- relevel(factor(df$AGE_large), ref = "35-49")
df$sexe <- factor(as.character(df$SEXE), levels = c("Homme", "Femme"))
lg <- txt(df$G2Q00003)
df$logement <- factor(ifelse(lg == "Dans une maison", "Maison",
  ifelse(lg == "Dans un appartement", "Appartement", "Autre/NSP")),
  levels = c("Maison", "Appartement", "Autre/NSP"))

# Statut d'occupation (G2Q00004, posé aux longs). Six "Autre" sont en fait
# propriétaires (usufruitiers, indivision, "à crédit"). Les courts n'ont pas de
# contrat à leur nom : la catégorie "Sans contrat" remplace l'indicatrice
# long/court, qui lui serait identique.
st  <- txt(df$G2Q00004)
pre <- tolower(txt(df$G2Q00004_other))
st[st == "Autre" & grepl("usufrui|indivision|crédit", pre)] <- "Propriétaire"
st[df$long & !st %in% c("Propriétaire", "Locataire")] <- "Autre"
st[!df$long] <- "Sans contrat"
df$statut <- factor(st, levels = c("Propriétaire", "Locataire", "Autre", "Sans contrat"))

df$zone   <- relevel(factor(as.character(df$zone3)), ref = "Urbain")
df$taille <- relevel(factor(as.character(df$taille_commune)), ref = "Grande ville")
df$vague  <- factor(df$vague)

# Difficultés à payer ses factures d'énergie (G2Q00009, posée aux longs). Les courts n'ont pas eu la question. Quand le statut est dans le
# modèle, on les range dans la référence : l'indicatrice "Sans contrat" absorbe
# leur écart, et les effets des difficultés ne sont estimés que sur les longs.
# Sans le statut, ils forment une catégorie "Non posée".
dif <- txt(df$G2Q00009)
dif <- ifelse(dif %in% c("Rarement", "Parfois"), "Rarement/parfois",
       ifelse(dif %in% c("Souvent", "Toujours"), "Souvent/toujours",
       ifelse(dif == "Jamais", "Jamais", "NSP")))
df$difficultes <- factor(ifelse(df$long, dif, "Jamais"),
                         levels = c("Jamais", "Rarement/parfois", "Souvent/toujours", "NSP"))
df$difficultes_np <- factor(ifelse(df$long, dif, "Non posée"),
                            levels = c("Jamais", "Rarement/parfois", "Souvent/toujours", "NSP", "Non posée"))

controles <- c("age", "sexe", "diplome", "revenu", "difficultes", "logement", "statut", "vague")
controles_sans_logement <- c("age", "sexe", "diplome", "revenu", "difficultes_np", "vague")


#############################################
### 4. Régressions : le découpage du 22/10 ###
#############################################

# Les indicateurs ont trois rôles :
#   - 7 positions expliquées : gestion décentralisée, concurrence, même service
#     partout, pro-nucléaire, malus, offre verte souscrite, gestes ;
#   - 6 variables explicatives : connaissance du
#     réseau et intérêt pour l'énergie, définition du service public et
#     absence de profit, disposition à payer et scepticisme ;
#   - des indicateurs secondaires, seulement décrits : bonus, collectivités
#     garantes, porteur local, autoconsommation collective, intérêt pour la
#     transition.
# L'intérêt pour la transition n'est pas une variable explicative : il se comporte
# comme une adhésion à la transition (voir la variante à la fin de cette partie).
#
# Blocs : A1 position = profil ; A2 = + zone ; B = + connaissance du
# réseau + intérêt pour l'énergie ; C = profil + zone + conceptions du service
# public ; D = offre souscrite selon la disposition à payer et le scepticisme ; E = qui porte chaque variable explicative ;
# F = B et C réunis.

# Binaires et part de gestes en points de pourcentage (x 100)
for (v in c("justice_terr", "anc_collectivites", "anc_porteur_local", "anc_auto_collective",
            "acc_bonus", "acc_malus", "gestes_part", "pro_nucleaire", "sp_organisme",
            "sp_mission", "sp_sans_profit", "offre_verte", "scepticisme"))
  df[[paste0(v, "_pts")]] <- 100 * df[[v]]

positions <- data.frame(
  nom = c("Gestion décentralisée (1-5)", "Concurrence (1-5)", "Même service partout (pts)",
          "Pro-nucléaire (0-100)", "Malus acceptable (pts)", "Offre verte souscrite (pts)",
          "Gestes faits / concernés (pts)"),
  var = c("local", "concurrence", "justice_terr_pts", "pro_nucleaire_pts", "acc_malus_pts",
          "offre_verte_pts", "gestes_part_pts"),
  brut = c("local", "concurrence", "justice_terr", "nuc_premier", "acc_malus", "offre_verte", "gestes_n"),
  robust = c("ologit", "ologit", "logit", "logit", "logit", "logit", "mco_sur9"),
  long_seul = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE)

# Plan de sondage sur un sous-échantillon : poids seuls. Le calage n'est pas repris
# dans le plan, mais c'est sans conséquence : avec le plan post-stratifié, les
# coefficients sont identiques et les écarts-types plus petits de 0,2 %.
plan <- function(d) svydesign(ids = ~1, weights = ~poids, data = droplevels(d))

etoiles <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
coef_txt <- function(m, terme, dec = 2) {
  s <- summary(m)$coefficients
  if (!terme %in% rownames(s)) return("")
  paste0(formatC(s[terme, 1], format = "f", digits = dec), etoiles(s[terme, 4]))
}
coef_p <- function(m) {
  if (inherits(m, "svyolr")) {
    cf <- coef(m); se <- sqrt(diag(vcov(m)))[names(cf)]
    return(cbind(cf, 2 * pnorm(-abs(cf / se))))
  }
  summary(m)$coefficients[, c(1, 4)]
}
dec_de <- function(nom) if (grepl("pts|0-100", nom)) 1 else 2


### Blocs A et B : profil, territoire, connaissance et intérêt ###

# Même échantillon pour A1, A2 et B : un écart d'une étape à l'autre ne vient que
# des variables ajoutées.
h4 <- c("conn_reseau", "interet_energie")
estimer <- function(i) {
  ind <- positions[i, ]
  d <- df[if (ind$long_seul) df$long else TRUE, ]
  d <- d[complete.cases(d[c(ind$var, controles, "zone", h4)]), ]
  des <- plan(d)
  fA1 <- reformulate(controles, ind$var)
  fA2 <- update(fA1, . ~ . + zone)
  fB  <- update(fA2, . ~ . + conn_reseau + interet_energie)
  m <- lapply(list(A1 = fA1, A2 = fA2, B = fB), function(f) svyglm(f, design = des))
  # Robustesse sur B : logit, logit ordonné, ou nombre de gestes sur 9
  rob <- switch(ind$robust,
    logit    = svyglm(update(fB, as.formula(paste(ind$brut, "~ ."))), design = des,
                      family = quasibinomial()),
    mco_sur9 = svyglm(update(fB, gestes_n ~ .), design = des),
    ologit   = svyolr(update(fB, as.formula(paste0("factor(", ind$brut, ") ~ ."))), design = des))
  # H6 : tests de Wald par bloc de variables, sur A2
  wald <- sapply(c("age", "diplome", "revenu", "difficultes", "sexe", "zone"), function(b)
    regTermTest(m$A2, as.formula(paste("~", b)))$p)
  list(ind = ind, n = nrow(d), m = m, rob = rob, wald = wald,
       moy = weighted.mean(d[[ind$var]], d$poids))
}
res <- lapply(seq_len(nrow(positions)), estimer)

# Robustesse et tests multiples sur les coefficients clés de B
cles <- c("age18-34", "age50-64", "age65+", "sexeFemme", "diplomeInfra-bac", "diplomeBac+5",
          "zoneRural", "zonePériurbain", "conn_reseau", "interet_energie")
robustesse <- do.call(rbind, lapply(res, function(r) {
  a <- coef_p(r$m$B); b <- coef_p(r$rob)
  k <- intersect(cles, intersect(rownames(a), rownames(b)))
  data.frame(position = r$ind$nom, terme = k, principal = a[k, 1], p_principal = a[k, 2],
             robuste = b[k, 1], p_robuste = b[k, 2], row.names = NULL)
}))
robustesse$accord <- with(robustesse, ifelse(
  p_principal < .05 & p_robuste < .05, ifelse(sign(principal) == sign(robuste), "les deux, même signe", "signes opposés"),
  ifelse(p_principal < .05, "principal seulement", ifelse(p_robuste < .05, "robuste seulement", "aucun"))))
robustesse$p_bh <- p.adjust(robustesse$p_principal, "BH")   # Benjamini-Hochberg


### Variantes du territoire : deux versions, avec et sans logement et statut ###

# Le logement et le statut absorbent une partie de ce qui fait le territoire
# (89 % des ruraux vivent en maison) : on présente les deux versions, avec la
# zone déclarée et avec la taille de commune (où se distinguent les hameaux).
variantes_h7 <- function(i) {
  ind <- positions[i, ]
  d <- df[if (ind$long_seul) df$long else TRUE, ]
  sapply(list(zone_complet = c("zone", controles), zone_sans_log = c("zone", controles_sans_logement),
              taille_complet = c("taille", controles), taille_sans_log = c("taille", controles_sans_logement)),
         function(x) {
           dd <- d[complete.cases(d[c(ind$var, x)]), ]
           s <- summary(svyglm(reformulate(x, ind$var), design = plan(dd)))$coefficients
           k <- grep("^(zone|taille)", rownames(s))
           setNames(paste0(formatC(s[k, 1], format = "f", digits = 2), etoiles(s[k, 4])), rownames(s)[k])
         }, simplify = FALSE)
}
h7 <- lapply(seq_len(nrow(positions)), variantes_h7)


### Bloc C : les conceptions du service public orientent-elles les positions ? ###

# La définition du service public (organisme public, mission, les deux à la fois)
# entre par deux indicatrices, avec "les deux à la fois" en référence ; l'absence
# de profit citée est une troisième indicatrice.
conceptions <- c("sp_organisme", "sp_mission", "sp_sans_profit")
bloc_c <- lapply(setNames(nm = c("concurrence", "local", "justice_terr_pts", "pro_nucleaire_pts")), function(y) {
  x <- c(conceptions, controles, "zone")
  d <- df[complete.cases(df[c(y, x)]), ]
  svyglm(reformulate(x, y), design = plan(d))
})
registres <- sapply(bloc_c, function(m) sapply(conceptions, function(t) coef_txt(m, t)))


### Bloc F : les variables de B et de C réunies ###

# Les 7 positions, avec à la fois les conceptions du service public, la
# connaissance du réseau et l'intérêt pour l'énergie, sur les cas complets. Les
# coefficients bougent à peine par rapport à B et C estimés séparément.
bloc_f <- lapply(seq_len(nrow(positions)), function(i) {
  ind <- positions[i, ]
  d <- df[if (ind$long_seul) df$long else TRUE, ]
  x <- c(controles, "zone", conceptions, h4)
  d <- d[complete.cases(d[c(ind$var, x)]), ]
  list(ind = ind, n = nrow(d), m = svyglm(reformulate(x, ind$var), design = plan(d)))
})


### Bloc D : la disposition à payer devient-elle un choix quand l'offre est crédible ? ###

# Offre souscrite (en points) selon la disposition à payer, chez les sceptiques
# et chez les autres. H5 attend une souscription plus faible chez les sceptiques.
d <- df[df$long & complete.cases(df[c("offre_verte_pts", "vert", "scepticisme",
                                       controles, "zone")]), ]
h5 <- svyglm(reformulate(c("vert * scepticisme", controles, "zone"), "offre_verte_pts"),
             design = plan(d))
h5_logit <- svyglm(reformulate(c("vert * scepticisme", controles, "zone"), "offre_verte"),
                   design = plan(d), family = quasibinomial())
# En logit, l'interaction ne mesure pas la même chose qu'en probabilité : à taux
# de base faible, un simple décalage de niveau réduit la pente en points.


### Bloc E : qui porte les variables explicatives ###

# Chaque variable explicative, et l'intérêt pour la transition, régressés sur le
# profil et la zone. La définition du service public, à trois modalités, est
# estimée par deux modèles (organisme public ; mission).
explicatives <- data.frame(
  nom = c("Connaissance du réseau (0-2)", "Intérêt pour l'énergie (1-5)",
          "SP : un organisme public (pts)", "SP : une mission (pts)", "SP : absence de profit (pts)",
          "Payer le vert (1-5)", "Scepticisme, offres vertes (pts)", "Intérêt pour la transition (1-5)"),
  var = c("conn_reseau", "interet_energie", "sp_organisme_pts", "sp_mission_pts", "sp_sans_profit_pts",
          "vert", "scepticisme_pts", "interet_transition"),
  long_seul = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE)
bloc_e <- lapply(seq_len(nrow(explicatives)), function(i) {
  ind <- explicatives[i, ]
  d <- df[if (ind$long_seul) df$long else TRUE, ]
  d <- d[complete.cases(d[c(ind$var, controles, "zone")]), ]
  list(ind = ind, n = nrow(d), m = svyglm(reformulate(c(controles, "zone"), ind$var), design = plan(d)))
})

# Connaissance du réseau : séparer le fait de répondre "ne sait pas" et le fait de
# se tromper (les femmes répondent bien plus souvent "ne sait pas" : 62 % contre
# 28 % pour RTE).
reponses <- cbind(RTE = !est_nsp(df$G3Q00002_SQ001), Enedis = !est_nsp(df$G3Q00002_SQ002))
df$conn_nb_nsp <- 2 - rowSums(reponses)
df$conn_juste_si_repond <- ifelse(rowSums(reponses) > 0,
  100 * rowSums(conn_items[, 1:2] * reponses) / rowSums(reponses), NA)
d <- df[complete.cases(df[c("conn_nb_nsp", controles, "zone")]), ]
conn_rob <- list(
  principal     = svyglm(reformulate(c(controles, "zone"), "conn_reseau"), design = plan(d)),
  nb_nsp        = svyglm(reformulate(c(controles, "zone"), "conn_nb_nsp"), design = plan(d)),
  juste_si_rep  = svyglm(reformulate(c(controles, "zone"), "conn_juste_si_repond"),
                         design = plan(d[!is.na(d$conn_juste_si_repond), ])))


### Variante : pourquoi l'intérêt pour la transition n'est pas une variable explicative ###

# Bloc B avec l'intérêt pour la transition en plus. L'intérêt pour l'énergie y perd
# son effet sur le malus et l'offre souscrite, tout passe par l'intérêt pour la
# transition, et les deux vont en sens contraire sur le nucléaire.
variante_transition <- t(sapply(seq_len(nrow(positions)), function(i) {
  ind <- positions[i, ]
  d <- df[if (ind$long_seul) df$long else TRUE, ]
  x <- c(controles, "zone", h4, "interet_transition")
  d <- d[complete.cases(d[c(ind$var, x)]), ]
  m <- svyglm(reformulate(x, ind$var), design = plan(d))
  c(energie = coef_txt(m, "interet_energie", dec_de(ind$nom)),
    transition = coef_txt(m, "interet_transition", dec_de(ind$nom)))
}))
rownames(variante_transition) <- positions$nom


###################
### 5. Tableaux ###
###################

options(width = 250)
pond <- function(x, cond = TRUE) { i <- cond & !is.na(x); sum(x[i] * w[i]) / sum(w[i]) }

cat("\n=== Indicateurs, par rôle (n et moyenne pondérée) ===\n")
resume <- data.frame(
  role = c(rep("position", 7), rep("explicative", 7), rep("secondaire", 5)),
  indicateur = c(positions$nom, explicatives$nom[1:7], "Bonus acceptable (pts)", "Collectivités garantes (pts)",
                 "Porteur local (pts)", "Autoconso. collective (pts)", "Intérêt pour la transition (1-5)"),
  var = c(positions$var, explicatives$var[1:7], "acc_bonus_pts", "anc_collectivites_pts",
          "anc_porteur_local_pts", "anc_auto_collective_pts", "interet_transition"))
resume$n <- sapply(resume$var, function(v) sum(!is.na(df[[v]])))
resume$moyenne <- round(sapply(resume$var, function(v) pond(df[[v]])), 2)
print(resume[, c("role", "indicateur", "n", "moyenne")], row.names = FALSE)

tabAB <- function(etape, termes, noms) {
  out <- t(sapply(res, function(r) sapply(termes, function(t) coef_txt(r$m[[etape]], t, dec_de(r$ind$nom)))))
  out <- cbind(n = sapply(res, `[[`, "n"), out)
  dimnames(out) <- list(positions$nom, c("n", noms)); noquote(out)
}
cat("\n=== Bloc A2 (profil + zone) : âge, sexe, diplôme, revenu ===\n")
print(tabAB("A2", c("age18-34", "age50-64", "age65+", "sexeFemme", "diplomeInfra-bac", "diplomeBac+2/3",
                    "diplomeBac+5", "revenu< 1 000", "revenu4 000 et +", "revenuNSP"),
            c("18-34", "50-64", "65+", "Femme", "Infra-bac", "Bac+2/3", "Bac+5", "<1000", "4000+", "Rev. NSP")))
cat("\n=== Bloc A2 : difficultés, logement, territoire ===\n")
print(tabAB("A2", c("difficultesRarement/parfois", "difficultesSouvent/toujours", "logementAppartement",
                    "statutLocataire", "statutSans contrat", "zoneRural", "zonePériurbain"),
            c("Diff. parfois", "Diff. souvent", "Appart.", "Locataire", "Sans contrat", "Rural", "Périurb.")))
cat("\n=== Tests de Wald par bloc, A2 (p-valeurs) ===\n")
wald <- t(sapply(res, `[[`, "wald")); rownames(wald) <- positions$nom
print(round(wald, 4))
cat("\n=== Bloc B (H4) ===\n")
print(tabAB("B", c("conn_reseau", "interet_energie"), c("Réseau", "Intérêt énergie")))
r2 <- function(m) { y <- m$y; p <- weights(m$survey.design); f <- fitted(m)
  1 - sum(p * (y - f)^2) / sum(p * (y - weighted.mean(y, p))^2) }
r2_tab <- t(sapply(res, function(r) sapply(r$m, r2)))
dimnames(r2_tab) <- list(positions$nom, c("A1", "A2", "B"))
cat("\n=== R² pondéré ===\n"); print(round(r2_tab, 3))

cat("\n=== Robustesse des coefficients clés de B ===\n")
print(table(robustesse$accord))
print(robustesse[robustesse$accord %in% c("principal seulement", "robuste seulement", "signes opposés"),
                 c("position", "terme", "p_principal", "p_robuste", "accord")], digits = 2, row.names = FALSE)
cat(sprintf("Tests multiples : %d coefficients clés, %d significatifs à 5 %%, %d après correction de Benjamini-Hochberg\n",
            nrow(robustesse), sum(robustesse$p_principal < .05), sum(robustesse$p_bh < .05)))

cat("\n=== H7, deux versions : zone déclarée et taille de commune (réf. urbain / grande ville) ===\n")
for (i in seq_along(h7)) {
  cat("\n", positions$nom[i], "\n")
  cat("  zone, avec logement et statut  :", paste(names(h7[[i]]$zone_complet), h7[[i]]$zone_complet), "\n")
  cat("  zone, sans logement ni statut  :", paste(names(h7[[i]]$zone_sans_log), h7[[i]]$zone_sans_log), "\n")
  cat("  taille, avec logement et statut:", paste(sub("taille", "", names(h7[[i]]$taille_complet)), h7[[i]]$taille_complet), "\n")
  cat("  taille, sans logement ni statut:", paste(sub("taille", "", names(h7[[i]]$taille_sans_log)), h7[[i]]$taille_sans_log), "\n")
}

cat("\n=== Bloc C (H3) : positions selon les conceptions du service public ===\n")
print(noquote(registres))

cat("\n=== Bloc F : B et C réunis ===\n")
out <- t(sapply(bloc_f, function(b) sapply(c(conceptions, h4), function(t) coef_txt(b$m, t, dec_de(b$ind$nom)))))
dimnames(out) <- list(positions$nom, c("Organisme", "Mission", "Sans profit", "Réseau", "Intérêt énergie"))
print(noquote(cbind(n = sapply(bloc_f, `[[`, "n"), out,
                    R2_F = sprintf("%.3f", sapply(bloc_f, function(b) r2(b$m))), R2_B = sprintf("%.3f", r2_tab[, "B"]))))

cat("\n=== Bloc D (H5) : offre souscrite (pts) ~ disposition à payer x scepticisme ===\n")
print(round(summary(h5)$coefficients[c("vert", "scepticisme", "vert:scepticisme"), ], 3))
cat("Logit :\n")
print(round(summary(h5_logit)$coefficients[c("vert", "scepticisme", "vert:scepticisme"), ], 3))

cat("\n=== Bloc E (annexe) : qui porte les variables explicatives ===\n")
te <- c("age18-34", "age65+", "sexeFemme", "diplomeInfra-bac", "diplomeBac+5", "revenu4 000 et +", "zoneRural")
out <- t(sapply(bloc_e, function(b) sapply(te, function(t) coef_txt(b$m, t, dec_de(b$ind$nom)))))
dimnames(out) <- list(explicatives$nom, c("18-34", "65+", "Femme", "Infra-bac", "Bac+5", "4000+", "Rural"))
print(noquote(cbind(n = sapply(bloc_e, `[[`, "n"), out)))
cat("\nConnaissance du réseau : principal, nombre de NSP, part juste si répond\n")
print(noquote(sapply(conn_rob, function(m) sapply(c("age50-64", "age65+", "sexeFemme", "diplomeBac+5"),
                                                   function(t) coef_txt(m, t)))))

cat("\n=== Variante : bloc B avec l'intérêt pour la transition en plus ===\n")
print(noquote(variante_transition))

# Sauvegarde : coefficients de tous les modèles
une_table <- function(m, bloc, variable, n) {
  s <- summary(m)$coefficients
  data.frame(bloc = bloc, variable_expliquee = variable, terme = rownames(s), estimation = s[, 1],
             ecart_type = s[, 2], p = s[, 4], n = n, row.names = NULL)
}
coefs <- rbind(
  do.call(rbind, lapply(res, function(r) do.call(rbind, lapply(names(r$m), function(e)
    une_table(r$m[[e]], e, r$ind$nom, r$n))))),
  do.call(rbind, lapply(names(bloc_c), function(y) une_table(bloc_c[[y]], "C", y, length(bloc_c[[y]]$y)))),
  une_table(h5, "D", "Offre verte souscrite (pts)", length(h5$y)),
  do.call(rbind, lapply(bloc_e, function(b) une_table(b$m, "E", b$ind$nom, b$n))),
  do.call(rbind, lapply(bloc_f, function(b) une_table(b$m, "F", b$ind$nom, b$n))))
write.csv2(coefs, file.path(dossier_sortie, "coefficients.csv"), row.names = FALSE, fileEncoding = "UTF-8")
saveRDS(list(res = res, h7 = h7, bloc_c = bloc_c, h5 = h5, h5_logit = h5_logit, bloc_e = bloc_e, bloc_f = bloc_f,
             conn_rob = conn_rob, robustesse = robustesse, variante_transition = variante_transition,
             coherence = coherence, resume = resume),
        file.path(dossier_sortie, "resultats.rds"))
