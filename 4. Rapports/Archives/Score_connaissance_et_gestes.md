# Script 5 : connaissance des acteurs et intégration des gestes

## Vérification du barème — 3 octobre 2026

Le questionnaire demande le rôle principal de chaque acteur et propose trois
rôles substantiels, plus NSP. Dans le système électrique français, RTE assure
le transport, Enedis la distribution, EDF et Engie la production et la fourniture.
La réponse « Produit et fournit l’électricité » est donc correcte pour DEUX
acteurs. Le script l'acceptait déjà pour chacun : il n'exigeait pas une attribution
exclusive d'un rôle à un acteur.

Sources officielles consultées :
- [RTE, transport et distinction avec la distribution](https://www.rte-france.com/bases-electricite/systeme-electrique/comment-fonctionne-transport-electricite).
- [Enedis, organisation du marché](https://www.enedis.fr/lorganisation-du-marche-de-lelectricite).
- [EDF, production et commercialisation](https://www.edf.fr/groupe-edf/edf-en-bref).
- [Engie, production renouvelable](https://www.engie.com/nos-activites/production-renouvelable-et-flexibilite/)
  et [fourniture d'énergie](https://www.engie.com/nos-activites/gestion-fourniture-energie-clients/).

Le périmètre est celui des acteurs nommés et de leur rôle principal en France,
pas celui de toutes les activités historiques ou internationales de leur groupe.
L'appartenance d'Enedis au groupe EDF ne rend pas automatiquement correcte la
réponse « distribue » pour EDF dans cette question distinguant les acteurs.
Cette ambiguïté de perception mérite d'être signalée dans l'interprétation du
score, sans transformer arbitrairement plusieurs réponses en bonnes réponses.

Modification du script : remplacement du test par préfixe par une liste explicite
de réponses admises pour chaque acteur et un test d'appartenance. Le code permet
plusieurs réponses admises par acteur si un autre questionnaire le justifie.
Un acteur vaut au maximum un point. Score = somme des quatre indicatrices, 0–4 ;
NSP = 0 ; absence technique sur un item = score manquant, puis imputation pour
l'espace factoriel selon la procédure existante. Pas de nouvelle réponse admise
sans justification. Les réponses observées conformes aux dictionnaires conservent
donc exactement le même score. Le chemin vers le script 1 renommé est corrigé.

## Gestes : intégration possible et recommandation

G7Q00001_SQ001–009 distingue déjà réalisation, intention, refus, NSP et non-concerné.
Conserver ces variables comme facteurs permet une AFM qualitative/mixte ; une
ACM est également adaptée si les seules variables actives sont qualitatives.
Ne pas coder refus = 0, intention = 1, réalisation = 2 comme une distance régulière.
Ne pas introduire simultanément la réponse complète et ses indicatrices dans
l'actif : cela compterait plusieurs fois la même information.

Première extension recommandée : les neuf gestes en un groupe illustratif
qualitatif, avec les quatre groupes actifs actuels conservés. La géométrie active
reste celle des représentations ; les gestes permettent de la décrire. Utiliser
les coordonnées et associations des modalités, pas des contributions actives
pour ce groupe. Comparer les réponses observées avec leurs dénominateurs.

Une AFM conjointe peut ensuite ajouter trois ou quatre groupes actifs :

| Groupe | Items G7Q00001 | Contenu |
|---|---|---|
| Sobriété | 001–002 | Mobilité thermique et usages de chaleur |
| Pilotage/information | 003–005 | Heures creuses, suivi, information |
| Petits équipements | 006 | Thermostat, LED, coupe-veille |
| Investissements | 007–009 | Gros équipement, isolation, production |

Comparer le regroupement 006–009 au découpage ci-dessus : chaque groupe actif
est équilibré par l'AFM, et créer plusieurs groupes de pratiques leur donne plus
de place dans la construction de l'espace. Le regroupement n'est pas neutre.
Pour une analyse centrée sur les gestes, inverser les statuts : pratiques actives,
représentations illustratives. Une nouvelle CAH constituerait une autre typologie.

« Non concerné » est une situation substantielle distincte de « pas prêt ».
Le conserver explicitement peut structurer des axes d'opportunités matérielles ;
le traiter comme manquant puis l'imputer comme une intention ou une réalisation
est injustifié. NSP peut également rester explicite dans cette variante.
Les absences techniques restent à diagnostiquer ; distinguer routages et non-réponse.
Les seuils et l'imputation actuels du script 5 ne doivent pas être appliqués
automatiquement à ces nouvelles variables sans examiner ces causes.

Les réalisations sont déclarées comme « déjà fait » : aucune fréquence, date,
ampleur ou économie d'énergie n'est mesurée. Une intention ne constitue pas
un comportement futur observé. Contrôler les effectifs des modalités, comparer
le logement et le statut d'occupation en illustratif, puis examiner la sensibilité
aux populations retenues et aux groupes avant de nommer les axes ou classes.

Cette intervention vérifie et explicite le score. Elle propose le protocole G7,
sans modifier les variables actives ni exécuter une nouvelle AFM.
