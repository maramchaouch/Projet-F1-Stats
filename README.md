# Analyse statistique de la Formule 1 (1950–2024)

Projet académique réalisé en équipe de trois (Maram Chaouch, Eya Touati, Shahd Kdaiem) dans le cadre de la licence Big Data et Analyse de Données (ISAMM & ESCT, Université de la Manouba), avril – mai 2026.

L'objectif est d'appliquer les méthodes statistiques vues en cours sur un jeu de données réel : 74 ans de courses de Formule 1.

## Données

Dataset **Formula 1 World Championship (1950–2024)**, disponible sur Kaggle : 14 fichiers CSV (résultats, pilotes, écuries, courses, circuits, qualifications, arrêts aux stands, temps au tour…).

Après fusion de `results`, `races`, `drivers` et `constructors` :

| Élément | Valeur |
|---|---|
| Résultats de course | 26 499 |
| Courses | 1 112 |
| Pilotes | 859 |
| Écuries | 211 |

## Méthodes

- **Préparation :** fusion des fichiers, remplacement des valeurs manquantes (`\N` → `NA`), conversion des types, création des variables `victoire` et `podium`.
- **Analyse univariée :** histogrammes, densités, statistiques descriptives, test de Shapiro-Wilk, boxplots, diagrammes en barres et en secteurs.
- **Analyse bivariée :** matrice de corrélation, régression linéaire simple, intervalles de confiance, tableaux de contingence, tests du khi-deux et de Fisher, AFC.
- **Analyse multivariée :** ACP, régression linéaire multiple, clustering K-means (K = 3) évalué par le score de silhouette.
- **Visualisation :** `ggplot2` (barres, heatmap, violin plots, biplot ACP).

## Principaux résultats

- 85,7 % des résultats valent 0 point : la distribution des points est très asymétrique.
- Le calendrier passe de 7 courses en 1950 à 24 en 2024.
- Corrélation de −0,65 entre les points et la position finale, et de 0,64 entre la position de départ et la position finale.
- Le test du khi-deux indique un lien entre la nationalité du pilote et les victoires (p-value < 2,2e-16), à interpréter avec prudence.
- Régression multiple `points ~ grid + laps + year` : toutes les variables sont significatives, R² = 0,29.
- K-means : 3 profils de pilotes sur 376.

| Profil | Pilotes | Points moyens | Victoires moyennes |
|---|---|---|---|
| Dominants | 7 | 2 436 | 58,9 |
| Intermédiaires | 93 | 296 | 7,1 |
| Réguliers | 276 | 25 | 0,1 |

## Contenu du dépôt

```
analyse_f1.R      # code R complet, organisé par section du rapport
Project_F1.pdf    # rapport complet : méthodes, graphiques, interprétations
README.md
data/             # à créer : y placer les 14 fichiers CSV (non inclus)
```

## Exécution

1. Télécharger le dataset sur Kaggle (« Formula 1 World Championship ») et placer les fichiers CSV dans un dossier `data/` à la racine du projet.
2. Installer les packages R :

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "plotrix", "FactoMineR",
                   "factoextra", "corrplot", "ggcorrplot", "cluster"))
```

3. Ouvrir le dossier du projet dans RStudio et exécuter `analyse_f1.R`.

## Limites

- Beaucoup de valeurs manquantes (positions finales des abandons, données absentes des premières saisons).
- Les changements du système de points au fil des décennies limitent les comparaisons historiques.
- Le modèle linéaire n'explique qu'une partie de la variance (R² = 0,29).

---

## In short (English)

Statistical analysis of Formula 1 results from 1950 to 2024 (Kaggle dataset, 14 CSV files, 26,499 race results, 859 drivers), written in R. It covers data cleaning, univariate, bivariate and multivariate analysis (PCA, CA, multiple regression) and K-means clustering of 376 drivers into 3 profiles. Team project (3 students). See `analyse_f1.R` for the code and `Project_F1.pdf` for the full report (in French).
