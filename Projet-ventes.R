# Projet Analyse Statistiques Ventes

# Installation des packages
install.packages("Hmisc")
install.packages("VIM")
install.packages("digest")
install.packages("zoo")

# Chargement des packages
library(Hmisc)
library(VIM)
library(e1071)

#  Tâche 1 : Importation des données

ventes_db <- read.table(file=file.choose(), header=TRUE, sep=";")
View(ventes_db)
names(ventes_db)
dim(ventes_db)
str(ventes_db)  # Vérifie les types de données

# Tâche 2 : Pré-traitement des données

# 1. Valeurs aberantes 

# Détection des valeurs aberrantes graphiquement avec boxplot
boxplot(ventes_db$Quantite, main="Boxplot Quantité")
boxplot(ventes_db$PrixUnitaire, main="Boxplot Prix Unitaire")
boxplot(ventes_db$MontantVente, main="Boxplot Montant Vente")

# Détection des valeurs aberrantes numériquement par IQR

# Fonction pour détecter les outliers
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  borne_inf <- Q1 - 1.5 * IQR
  borne_sup <- Q3 + 1.5 * IQR
  outliers <- which(x < borne_inf | x > borne_sup)
  return(outliers)
}

# Outliers pour Quantite
outliers_Quantite <- detect_outliers(ventes_db$Quantite)
outliers_Quantite  # Affiche les lignes

# Outliers pour PrixUnitaire
outliers_PrixUnitaire <- detect_outliers(ventes_db$PrixUnitaire)
outliers_PrixUnitaire  # Affiche les lignes

# Outliers pour MontantVente
outliers_MontantVente <- detect_outliers(ventes_db$MontantVente)
outliers_MontantVente  # Affiche les lignes

# Interpretation : Seule la variable 'MontantVente' contient des valeurs aberrantes aux lignes (Lignes : 286, 610, 768, 804, 816, 968)

# Afficher les valeurs aberrantes
ventes_db$MontantVente[outliers_MontantVente]

# Traitement des valeurs aberantes

sum(is.na(ventes_db$MontantVente))
ventes_db$MontantVente[outliers_MontantVente] <- NA
sum(is.na(ventes_db$MontantVente))
ventes_db$MontantVente <- impute(ventes_db$MontantVente, fun = median)
sum(is.na(ventes_db$MontantVente))

summary(ventes_db$MontantVente)

#  2. Valeurs manquantes 

summary(ventes_db)

sum(is.na(ventes_db$Vendeur))
sum(is.na(ventes_db$Region))
sum(is.na(ventes_db$Produit))

# Interpretation : Pas des valeurs manquantes

#  3. Détection des doublons

sum(duplicated(ventes_db))

# Interpretation : Pas des doublons

# 4. Création de nouvelles variables 

names(ventes_db)
ventes_db$Chiffre_affaires <- ventes_db$PrixUnitaire*ventes_db$Quantite
names(ventes_db)

# Tâche 3 : Analyse univariée


str(ventes_db)
summary(ventes_db)


# 1️- Variables quantitatives

# Les mesures de position et de dispersion

stats_uni <- function(x) {
  cat("Moyenne :", mean(x, na.rm = TRUE), "\n")
  cat("Médiane :", median(x, na.rm = TRUE), "\n")
  cat("Écart-type :", sd(x, na.rm = TRUE), "\n")
  cat("Variance :", var(x, na.rm = TRUE), "\n")
  cat("Q1 :", quantile(x,0.25), "\n")
  cat("Q3 :", quantile(x,0.75), "\n")
  cat("Étendue :", max(x, na.rm = TRUE) - min(x, na.rm = TRUE), "\n\n")
}


cat("Quantité :\n")
stats_uni(ventes_db$Quantite)

cat("Prix unitaire :\n")
stats_uni(ventes_db$PrixUnitaire)

cat("Chiffre d'affaires :\n")
stats_uni(ventes_db$Chiffre_affaires)

# Les mesures de forme

for (v in c("Quantite", "PrixUnitaire", "Chiffre_affaires")) {
  cat("\n", v, "\n")
  cat("Asymétrie :", skewness(ventes_db[[v]]), "\n")
  cat("Aplatissement :", kurtosis(ventes_db[[v]]), "\n")
}

# - Résumé :
# Quantité et PrixUnitaire : distributions symétriques et aplaties
# Chiffre d’Affaires : légèrement asymétrique à droite, présence de valeurs élevées


# Interprétation :
# - Quantité est une variable quantitative discrète (int)
# - PrixUnitaire et ChiffreAffaires sont des variables quantitatives continues (num)

# Visualisations

barplot(
  table(ventes_db$Quantite),
  main = "Diagramme en barres des Quantités",
  col = "lightblue",
  xlab = "Quantité",
  ylab = "Effectif"
)

hist(ventes_db$PrixUnitaire, 
     main = "Histogramme des Prix Unitaires", 
     col = "lightgreen",
     xlab = "Prix unitaire")

hist(ventes_db$ChiffreAffaires, 
     main = "Histogramme du Chiffre d'Affaires", 
     col = "lightcoral",
     xlab = "Chiffre d'affaires")

# 2️- Variables qualitatives

# Calculs : effectifs et fréquences


table(ventes_db$Vendeur)
prop.table(table(ventes_db$Vendeur))

table(ventes_db$Region)
prop.table(table(ventes_db$Region))

table(ventes_db$Produit)
prop.table(table(ventes_db$Produit))

# Vendeur
barplot(
  table(ventes_db$Vendeur),
  main = "Répartition des Vendeurs",
  col = "lightblue",
  xlab = "Vendeur",
  ylab = "Effectif"
)

pie(
  table(ventes_db$Vendeur),
  main = "Camembert des Vendeurs",
  col = rainbow(length(unique(ventes_db$Vendeur)))
)

# Région
barplot(
  table(ventes_db$Region),
  main = "Répartition des Régions",
  col = "lightgreen",
  xlab = "Région",
  ylab = "Effectif"
)

# Produit
barplot(
  table(ventes_db$Produit),
  main = "Répartition des Produits",
  col = "lightcoral",
  xlab = "Produit",
  ylab = "Effectif"
)

# Tache 4 : Analyse bivariée

str(ventes_db)

## Relation entre deux variables quantitatives
# Exemple : Quantité vendue vs Chiffre d'affaires

# Nuage de points
plot(ventes_db$Quantite, ventes_db$Chiffre_affaires,
     main="Quantité vs Chiffre d'affaires")

# Test de normalite
shapiro.test(ventes_db$Quantite)
# La variable Quantite ne suit pas une distribution normale

# Test de dependence
cor.test(ventes_db$Quantite, ventes_db$Chiffre_affaires, method = "spearman")
# On rejette H0 ,il existe une corrélation monotone significative entre la quantité vendue et le chiffre d’affaires.

## Relation entre variable qualitative et quantitative
# Exemple : Chiffre d'affaires par Région

# Convertir la variable Region en facteur (obligatoire pour les tests statistiques)
ventes_db$Region <- as.factor(ventes_db$Region)

# Boxplot du chiffre d'affaires par région
boxplot(Chiffre_affaires ~ Region, data = ventes_db,
        main = "Chiffre d'affaires par Région",
        xlab = "Région",
        ylab = "Chiffre d'affaires",
        col = "lightblue",
        border = "darkblue")

# Test non paramétrique
# Attention : wilcox.test fonctionne uniquement si Region a 2 niveaux
if (nlevels(ventes_db$Region) == 2) {
  test_result <- wilcox.test(Chiffre_affaires ~ Region, data = ventes_db)
  print(test_result)
} else {
  # Pour plus de 2 niveaux, utiliser Kruskal-Wallis
  test_result <- kruskal.test(Chiffre_affaires ~ Region, data = ventes_db)
  print(test_result)
}

# On ne rejette pas H0, il n’y a pas de différence statistiquement significative du chiffre d’affaires entre les régions.


# Relation entre deux variables qualitatives
# Exemple : Produit vs Région (effectifs)
# Tableau croisé
table(ventes_db$Produit, ventes_db$Region)

# Diagramme en mosaïque
mosaicplot(table(ventes_db$Produit, ventes_db$Region))

chisq.test(table(ventes_db$Produit, ventes_db$Region))
# On ne rejette pas H0, il n’y a pas de relation statistiquement significative entre le type de produit et la région.

#  Tache 5 : Conclusions et recommandations 

# Région la plus performante
tapply(ventes_db$Chiffre_affaires, ventes_db$Region, mean)

# Produits les plus vendus / rentables
tapply(ventes_db$Chiffre_affaires, ventes_db$Produit, sum)

# Corrélation Quantité vs Chiffre d'affaires
cor(ventes_db$Quantite, ventes_db$Chiffre_affaires)

# Synthèse :
# - Région la plus performante : Nord
# - Produit le plus rentable : Produit B
# - Relation Quantité-Chiffre d’affaires : positive et forte (~0.70)
