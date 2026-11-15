# =============================================================================
# Analyse statistique de la Formule 1 (1950-2024)
# Projet academique - Maram Chaouch (ISAMM & ESCT)
# Donnees  : Formula 1 World Championship (Kaggle), 14 fichiers CSV
#
# Script reconstruit a partir du rapport PDF du projet (Project_F1.pdf).
# Utilisation : placer les fichiers CSV dans un dossier "data/" a cote de ce
# script, puis executer depuis la racine du projet.
# =============================================================================

library(dplyr) # Manipulation et filtrage des données
library(tidyr) # Restructuration des données
library(ggplot2) # Graphiques professionnels
library(plotrix) # Diagrammes en secteurs 3D
library(FactoMineR) # ACP et AFC
library(factoextra) # Visualisation ACP/AFC
library(corrplot) # Matrice de corrélation
library(ggcorrplot) # Matrice de corrélation ggplot

# 2.2 Définition du répertoire de travail ----

# 2.3 Importation des fichiers CSV ----
results<- read.csv("data/results.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
drivers<- read.csv("data/drivers.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
races<- read.csv("data/races.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
constructors<- read.csv("data/constructors.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
qualifying<- read.csv("data/qualifying.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
circuits<- read.csv("data/circuits.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
standings<- read.csv("data/driver_standings.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
status<- read.csv("data/status.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
pit_stops<- read.csv("data/pit_stops.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
con_results<- read.csv("data/constructor_results.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
con_standing<- read.csv("data/constructor_standings.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
lap_times<- read.csv("data/lap_times.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
seasons<- read.csv("data/seasons.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
sprint_res<- read.csv("data/sprint_results.csv",
  header=TRUE, sep=",",
  stringsAsFactors=TRUE)
cat("Les 14 fichiers CSV ont été importés avec succès !\n")

# 2.4 Exploration initiale des données ----
cat("=== DIMENSIONS DES FICHIERS PRINCIPAUX ===\n")
cat("results :", dim(results), "\n")
cat("drivers :", dim(drivers), "\n")
cat("races :", dim(races), "\n")
cat("constructors :", dim(constructors), "\n")
cat("qualifying :", dim(qualifying), "\n")
cat("circuits :", dim(circuits), "\n")
cat("standings :", dim(standings), "\n")
cat("status :", dim(status), "\n")
cat("pit_stops :", dim(pit_stops), "\n")
cat("lap_times :", dim(lap_times), "\n")

# 2.5 Structure des données ----
cat("=== STRUCTURE DU FICHIER RESULTS ===\n")
str(results)

# 2.6 Aperçu des données ----
cat("=== APERÇU DU FICHIER RESULTS ===\n")
head(results, 6)
cat("\n=== APERÇU DU FICHIER DRIVERS ===\n")
head(drivers, 6)
cat("\n=== APERÇU DU FICHIER RACES ===\n")
head(races, 6)

# 2.7 Fusion des fichiers principaux ----
# Étape 1 : results + races
f1 <- merge(results,
  races[, c("raceId","year","name","circuitId")],
  by = "raceId")
# Étape 2 : + drivers
f1 <- merge(f1,
  drivers[, c("driverId","forename","surname",
      "nationality","dob")],
  by = "driverId")
# Étape 3 : + constructors
f1 <- merge(f1,
  constructors[, c("constructorId","name",
      "nationality")],
  by = "constructorId",
  suffixes = c("_race","_constructor"))
cat("=== DATASET PRINCIPAL FUSIONNÉ ===\n")
cat("Dimensions :", dim(f1), "\n")
cat("Colonnes :\n")
print(names(f1))

# 2.8 Nettoyage des données ----
# Remplacement des valeurs manquantes

# Conversion en numérique
f1$points <- as.numeric(as.character(f1$points))
f1$position <- as.numeric(as.character(f1$position))
f1$grid <- as.numeric(as.character(f1$grid))
f1$laps <- as.numeric(as.character(f1$laps))
f1$year <- as.numeric(as.character(f1$year))
# Création de nouvelles variables
f1$driver_name <- paste(f1$forename, f1$surname)
f1$victoire <- ifelse(f1$position == 1, 1, 0)
f1$podium <- ifelse(f1$position <= 3, 1, 0)
cat("=== VALEURS MANQUANTES PAR COLONNE ===\n")
colSums(is.na(f1))

# write.table()
write.table(f1[, c("driver_name","year","points",
      "position","grid","laps",
      "name_constructor",
      "nationality_race")],
  file = "data/f1_nettoye.csv",
  col.names = TRUE,
  row.names = FALSE,
  sep = ";")
cat("Fichier f1_nettoye.csv exporte avec succes !\n")
cat("Dimensions :", nrow(f1), "lignes x 8 colonnes\n")

# 2.9 Statistiques générales ----
cat("=== STATISTIQUES GENERALES F1 ===\n")
cat("Nombre total d'entrees :", nrow(f1), "\n")
cat("Nombre total de courses :",
  length(unique(f1$raceId)), "\n")
cat("Nombre de pilotes uniques :",
  length(unique(f1$driverId)), "\n")
cat("Nombre d'ecuries uniques :",
  length(unique(f1$constructorId)), "\n")
cat("Periode couverte :",
  min(f1$year, na.rm=TRUE), "→",
  max(f1$year, na.rm=TRUE), "\n")
cat("\n=== RESUME STATISTIQUE DES VARIABLES CLES ===\n")
summary(f1[, c("points","position","grid","laps","year")])

# 3.1.1 Distribution des Points ----
# hist() — vu en TP Chapitre 4
hist(f1$points,
  col = "darkred",
  border = "white",
  main = "Distribution des Points en F1 (1950-2024)",
  xlab = "Points obtenus",
  ylab = "Nombre d observations",
  labels = TRUE)

# 3.1.2 Distribution des Points avec Densité ----
# Histogramme + courbe de densite — vu en TP Chapitre 4
# prob=TRUE permet de superposer la densite sur l histogramme
hist(f1$points,
  col = "darkred",
  border = "white",
  main = "Distribution des Points avec Courbe de Densite",
  xlab = "Points",
  ylab = "Densite",
  prob = TRUE)
lines(density(f1$points, na.rm=TRUE),
  lwd = 3,
  col = "gold")
legend("topright",
  legend = c("Courbe de densite"),
  col = c("gold"),
  lwd = 3)

# 3.1.3 Statistiques Descriptives — Points ----
# Parametres descriptifs — fonctions vues en TP Chapitre 2
cat("=== STATISTIQUES DESCRIPTIVES — POINTS ===\n")
cat("Moyenne :", mean(f1$points, na.rm=TRUE), "\n")
cat("Mediane :", median(f1$points, na.rm=TRUE), "\n")
cat("Ecart-type :", sd(f1$points, na.rm=TRUE), "\n")
cat("Variance :", var(f1$points, na.rm=TRUE), "\n")
cat("Minimum :", min(f1$points, na.rm=TRUE), "\n")
cat("Maximum :", max(f1$points, na.rm=TRUE), "\n")
cat("Etendue :", range(f1$points, na.rm=TRUE), "\n")
summary(f1$points)

# Test de normalite de Shapiro-Wilk
# Sur un echantillon car shapiro.test limite a 5000 obs
set.seed(123)
shapiro_test <- shapiro.test(sample(f1$points, 5000))
cat("=== TEST DE NORMALITE — SHAPIRO-WILK ===\n")
cat("Statistique W :", round(shapiro_test$statistic, 4), "\n")
cat("p-value :", shapiro_test$p.value, "\n")
if(shapiro_test$p.value < 0.05){
  cat("Conclusion : p-value < 0.05 — distribution NON normale\n")
  cat("Cela confirme l asymetrie observee dans l histogramme\n")
} else {
  cat("Conclusion : p-value > 0.05 — distribution normale\n")
}

# QQ-plot pour visualiser la normalite
qqnorm(sample(f1$points, 1000),
  main = "QQ-Plot — Distribution des Points",
  col = "darkred",
  pch = 19,
  cex = 0.5)
qqline(sample(f1$points, 1000),
  col = "blue",
  lwd = 2)

# Boxplot — detecte les valeurs aberrantes
boxplot(f1$points,
  col = "darkred",
  border = "black",
  main = "Boxplot des Points F1 (1950-2024)",
  ylab = "Points obtenus",
  outline = TRUE)
# Ajout de la moyenne
points(mean(f1$points, na.rm=TRUE),
  col = "gold",
  pch = 18,
  cex = 2)
legend("topright",
  legend = "Moyenne",
  col = "gold",
  pch = 18,
  cex = 1.2)

# 3.1.4 Distribution des Positions Finales ----
hist(f1$position,
  col = "steelblue",
  border = "white",
  main = "Distribution des Positions Finales en F1",
  xlab = "Position finale",
  ylab = "Frequence",
  labels = TRUE)

# 3.1.5 Distribution du Nombre de Tours ----
hist(f1$laps,
  col = "darkgreen",
  border = "white",
  main = "Distribution du Nombre de Tours Completes",
  xlab = "Nombre de tours",
  ylab = "Frequence",
  prob = TRUE)
lines(density(f1$laps, na.rm=TRUE),
  lwd = 3,
  col = "orange")
legend("topleft",
  legend = "Densite",
  col = "orange",
  lwd = 3)

# Statistiques descriptives — tours completes
cat("=== STATISTIQUES — TOURS COMPLETES ===\n")
cat("Moyenne :", mean(f1$laps, na.rm=TRUE), "\n")
cat("Mediane :", median(f1$laps, na.rm=TRUE), "\n")
cat("Ecart-type :", sd(f1$laps, na.rm=TRUE), "\n")
cat("Minimum :", min(f1$laps, na.rm=TRUE), "\n")
cat("Maximum :", max(f1$laps, na.rm=TRUE), "\n")
summary(f1$laps)

# 3.1.6 Evolution du Nombre de Courses par Saison ----
# table() — vu en TP Chapitre 4
courses_par_an <- table(races$year)
barplot(courses_par_an,
  col = "steelblue",
  main = "Evolution du Nombre de Courses par Saison",
  xlab = "Annee",
  ylab = "Nombre de courses",
  las = 2,
  cex.names = 0.5)

# 3.1.7 Estimateur a Noyau — Points des Pilotes ----
# plot(density()) — vu en TP Chapitre 4
plot(density(f1$points, na.rm=TRUE),
  main = "Estimateur a Noyau — Distribution des Points",
  xlab = "Points",
  ylab = "Densite",
  lwd = 3,
  col = "darkred")
abline(v = mean(f1$points, na.rm=TRUE),
  col = "blue",
  lwd = 2,
  lty = 2)
abline(v = median(f1$points, na.rm=TRUE),
  col = "darkgreen",
  lwd = 2,
  lty = 3)
legend("topright",
  legend = c("Densite", "Moyenne", "Mediane"),
  col = c("darkred", "blue", "darkgreen"),
  lwd = c(3, 2, 2),
  lty = c(1, 2, 3))

# 3.1.8 Courbe Theorique — curve() ----
# curve() — vu en TP Chapitre 3
m <- mean(f1$points, na.rm=TRUE)
s <- sd(f1$points, na.rm=TRUE)
hist(f1$points,
  col = "lightblue",
  border = "white",
  prob = TRUE,
  main = "Points F1 avec loi normale ajustee",
  xlab = "Points",
  ylab = "Densite")
curve(dnorm(x, mean=m, sd=s),
  col = "darkred",
  lwd = 3,
  add = TRUE)
legend("topright",
  legend = c("Donnees F1", "Loi normale"),
  col = c("lightblue","darkred"),
  lwd = c(5,3))

# 3.2.1 Repartition des Nationalites des Pilotes ----
nat <- sort(table(drivers$nationality), decreasing=TRUE)
top10_nat <- head(nat, 10)
par(mar=c(5,10,4,2))
barplot(top10_nat,
  col = rainbow(10),
  main = "Top 10 des Nationalites des Pilotes F1",
  xlab = "Nombre de pilotes",
  horiz = TRUE,
  las = 1,
  cex.names = 0.9)

# 3.2.2 Repartition des Statuts de Fin de Course ----
f1_status <- merge(results, status, by="statusId")
top_status <- sort(table(f1_status$status), decreasing=TRUE)
top10_status <- head(top_status, 10)
par(mar=c(5,12,4,2))
barplot(top10_status,
  col = heat.colors(10),
  main = "Top 10 des Statuts de Fin de Course",
  xlab = "Frequence",
  horiz = TRUE,
  las = 1,
  cex.names = 0.8)

# 3.2.3 Top 8 Ecuries — Diagramme en Secteurs 3D ----
library(ggplot2)
top_ecuries <- sort(table(f1$name_constructor),
  decreasing=TRUE)
top8 <- head(top_ecuries, 8)
df_ecuries <- as.data.frame(top8)
names(df_ecuries) <- c("Ecurie", "Frequence")
df_ecuries$Pourcentage <- round((df_ecuries$Frequence / sum(df_ecuries$Frequence)) * 100, 1)
ggplot(df_ecuries, aes(x=reorder(Ecurie, Frequence), y=Frequence, fill=Frequence)) +
geom_col() +
geom_text(aes(label=paste(Pourcentage, "%")),
  hjust = -0.2,
  size = 4) +
coord_flip() +
labs(title = "Top 8 des Ecuries F1",
  x = "",
  y = "Nombre de participations") +
theme_minimal() +
theme(legend.position = "none")

# 3.2.4 Top 10 Pilotes — Nombre de Victoires ----
victoires <- f1[!is.na(f1$position) & f1$position == 1, ]
top_pilotes <- sort(table(victoires$driver_name),
  decreasing=TRUE)
top10_pilotes <- head(top_pilotes, 10)
par(mar=c(5,12,4,2))
barplot(top10_pilotes,
  col = "darkred",
  main = "Top 10 Pilotes — Nombre de Victoires en F1",
  xlab = "Nombre de victoires",
  horiz = TRUE,
  las = 1,
  cex.names = 0.9)

# 3.2.5 Repartition des Ecuries — Pie Chart 2D ----
# levels() — vu en TP Chapitre 2
ecurie_factor <- factor(f1$name_constructor)
nb <- levels(ecurie_factor)
cat("Nombre total d ecuries :", length(nb), "\n")
top6 <- head(sort(table(f1$name_constructor),
    decreasing=TRUE), 6)
# prop.table() — vu en TP Chapitre 4
prop <- round(prop.table(top6) * 100, 1)
lab <- names(top6)
# pie() 2D — vu en TP Chapitre 4
pie(top6,
  col = rainbow(length(top6)),
  main = "Top 6 Ecuries F1 — Repartition",
  labels = paste(lab, prop, "%"))

# 3.2.6 Points Moyens par Ecurie — tapply() ----
# tapply() — vu en TP Chapitre 4
points_par_ecurie <- tapply(f1$points,
  f1$name_constructor,
  mean,
  na.rm=TRUE)
top10_ecurie <- sort(points_par_ecurie,
  decreasing=TRUE)[1:10]
cat("=== POINTS MOYENS PAR ECURIE (Top 10) ===\n")
print(round(top10_ecurie, 2))
par(mar=c(5,12,4,2))
barplot(top10_ecurie,
  col = rainbow(10),
  main = "Points Moyens par Ecurie (Top 10)",
  xlab = "Points moyens",
  horiz = TRUE,
  las = 1,
  cex.names = 0.8)

# 3.2.7 Proportion des Victoires par Nationalite ----
# prop.table() — vu en TP Chapitre 4
vict_nat <- table(victoires$nationality_race)
top10_vict_nat <- head(sort(vict_nat, decreasing=TRUE), 10)
prop_nat <- round(prop.table(top10_vict_nat) * 100, 1)
cat("=== PROPORTION DES VICTOIRES PAR NATIONALITE ===\n")
print(prop_nat)
par(mar=c(5,10,4,2))
barplot(prop_nat,
  col = rainbow(10),
  main = "Proportion des Victoires par Nationalite",
  xlab = "Pourcentage des victoires",
  horiz = TRUE,
  las = 1,
  cex.names = 0.8)

# 3.2.8 Rang des Pilotes — rank() et seq() ----
# rank()—
stats_rang <- sort(tapply(f1$points,
    f1$driver_name,
    sum, na.rm=TRUE),
  decreasing=TRUE)
top20 <- head(stats_rang, 20)
rangs <- rank(-top20)
cat("=== RANG DES TOP 20 PILOTES PAR POINTS ===\n")
print(rangs)
# seq() —
annees <- seq(from=1950, to=2024, by=10)
cat("\n=== SEQUENCE DES DECENNIES F1 ===\n")
print(annees)

# 3.3.1 Distribution des Points — ggplot2 ----
ggplot(f1, aes(x=points)) +
geom_histogram(binwidth = 1,
  fill = "darkred",
  color = "white",
  alpha = 0.8) +
geom_density(aes(y=after_stat(count)),
  color = "gold",
  lwd = 1.5) +
labs(title = "Distribution des Points en F1 (1950-2024)",
  subtitle = "Histogramme avec courbe de densite",
  x = "Points obtenus",
  y = "Frequence",
  caption = "Source : Kaggle F1 Dataset") +
theme_minimal() +
theme(plot.title = element_text(face="bold", size=14),
  plot.subtitle = element_text(color="grey50"))

# 3.3.2 Top 10 Pilotes — ggplot2 ----
vict_df <- as.data.frame(top10_pilotes)
names(vict_df) <- c("Pilote", "Victoires")
ggplot(vict_df, aes(x=reorder(Pilote, Victoires),
    y=Victoires,
    fill=Victoires)) +
geom_col() +
geom_text(aes(label=Victoires),
  hjust = -0.2,
  size = 4) +
scale_fill_gradient(low="steelblue", high="darkred") +
coord_flip() +
labs(title = "Top 10 Pilotes — Nombre de Victoires",
  subtitle = "Formula 1 (1950-2024)",
  x = "",
  y = "Nombre de victoires",
  caption = "Source : Kaggle F1 Dataset") +
theme_minimal() +
theme(legend.position = "none",
  plot.title = element_text(face="bold", size=14))

# 3.3.3 Evolution des Points par Decennie — ggplot2 ----
# Création de la variable decennie ICI
f1$decennie <- paste0(floor(f1$year / 10) * 10, "s")
moy_dec <- tapply(f1$points, f1$decennie,
  mean, na.rm=TRUE)
moy_dec <- sort(moy_dec)
dec_df <- data.frame(
  decennie = names(moy_dec),
  points = as.numeric(moy_dec)
)
ggplot(dec_df, aes(x=decennie, y=points, group=1)) +
geom_line(color="darkred", lwd=1.5) +
geom_point(color="darkred", size=4) +
geom_text(aes(label=round(points,2)),
  vjust = -1,
  size = 3.5) +
labs(title = "Evolution des Points Moyens par Decennie",
  subtitle = "Impact des changements du systeme de points",
  x = "Decennie",
  y = "Points moyens par course",
  caption = "Source : Kaggle F1 Dataset") +
theme_minimal() +
theme(plot.title = element_text(face="bold", size=14),
  axis.text.x = element_text(angle=45, hjust=1))

# 3.3.4 Boxplot Professionnel — Points par Nationalite ----
top6_nat_vec <- names(head(sort(table(f1$nationality_race),
      decreasing=TRUE), 6))
f1_box <- f1[f1$nationality_race %in% top6_nat_vec, ]
ggplot(f1_box, aes(x=reorder(nationality_race, points,
      FUN=median),
    y=points,
    fill=nationality_race)) +
geom_boxplot(alpha=0.7, outlier.alpha=0.2) +
coord_flip() +
labs(title = "Distribution des Points par Nationalite",
  subtitle = "Top 6 nationalites les plus representees",
  x = "Nationalite",
  y = "Points",
  caption = "Source : Kaggle F1 Dataset") +
theme_minimal() +
theme(legend.position = "none",
  plot.title = element_text(face="bold", size=14))

# 4.1 Matrice de Corrélation ----
# Selection des variables numeriques
vars_num <- f1[, c("points","position","grid","laps","year")]
vars_num <- vars_num[complete.cases(vars_num), ]
# Matrice de correlation
mat_cor <- cor(vars_num, use="complete.obs")
cat("=== MATRICE DE CORRELATION ===\n")
print(round(mat_cor, 3))
corrplot(mat_cor,
  method = "circle",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  title = "Matrice de Correlation — Variables F1",
  mar = c(0,0,2,0))

# persp()
# Visualisation 3D de la densite points/laps
# Creation de la grille
x <- seq(0, 50, length=30)
y <- seq(0, 200, length=30)
# Fonction de densite 2D approximee
f_surf <- function(x, y){
  dnorm(x, mean=mean(f1$points, na.rm=TRUE),
    sd=sd(f1$points, na.rm=TRUE)) *
  dnorm(y, mean=mean(f1$laps, na.rm=TRUE),
    sd=sd(f1$laps, na.rm=TRUE))
}
# Evaluation sur la grille avec outer()
z <- outer(x, y, f_surf)
# persp() — graphique 3D
persp(x, y, z,
  theta = 30,
  phi = 30,
  expand = 0.5,
  col = "lightblue",
  main = "Surface 3D — Densite Points x Laps",
  xlab = "Points",
  ylab = "Tours",
  zlab = "Densite")

# 4.1.1 Nuage de Points — Grid vs Position ----
# Nuage de points
plot(f1$grid, f1$position,
  xlab = "Position de depart (grille)",
  ylab = "Position finale",
  main = "Position de depart vs Position finale en F1",
  pch = 19,
  col = rgb(0.8, 0, 0, 0.1),
  cex = 0.5)
# Droite de regression
abline(lm(f1$position ~ f1$grid),
  col = "blue",
  lwd = 2)
legend("topleft",
  legend = c("Observations", "Droite de regression"),
  col = c("darkred", "blue"),
  pch = c(19, NA),
  lty = c(NA, 1),
  lwd = c(NA, 2))

# 4.1.2 Coefficient de Correlation — Grid vs Position ----
cor_val <- cor(f1$grid, f1$position, use="complete.obs")
cat("=== COEFFICIENT DE CORRELATION ===\n")
cat("Correlation grid vs position :", round(cor_val, 4), "\n")
# Interpretation automatique
if(abs(cor_val) > 0.7){
  cat("Interpretation : Correlation tres elevee\n")
} else if(abs(cor_val) > 0.5){
  cat("Interpretation : Correlation elevee\n")
} else if(abs(cor_val) > 0.3){
  cat("Interpretation : Correlation moyenne\n")
} else {
  cat("Interpretation : Faible correlation\n")
}

# 4.1.3 Regression Lineaire Simple — Points vs Laps ----
# lm(Y ~ X) : Y variable dependante, X variable independante
reg <- lm(f1$points ~ f1$laps)
summary(reg)

# Visualisation de la droite de regression
plot(f1$laps, f1$points,
  xlab = "Nombre de tours completes",
  ylab = "Points obtenus",
  main = "Regression Lineaire : Points ~ Tours",
  pch = 19,
  col = rgb(0, 0, 0.8, 0.1),
  cex = 0.5)
# Ajout de la droite
abline(reg, col="red", lwd=2)
legend("topleft",
  legend = c("Observations", "Droite de regression"),
  col = c("steelblue", "red"),
  pch = c(19, NA),
  lty = c(NA, 1),
  lwd = c(NA, 2))

# 4.2 Intervalle de Confiance ----
# t.test() — intervalle de confiance
t.test(f1$points)

# Visualisation de l intervalle de confiance
ic <- t.test(f1$points)
ggplot(data.frame(x=1,
    mean=ic$estimate,
    lower=ic$conf.int[1],
    upper=ic$conf.int[2]),
  aes(x=x, y=mean)) +
geom_point(size=4, color="darkred") +
geom_errorbar(aes(ymin=lower, ymax=upper),
  width=0.1,
  color="darkred",
  lwd=1.5) +
labs(title = "Intervalle de Confiance a 95% — Points Moyens",
  subtitle = paste("Moyenne =", round(ic$estimate,3),
    "| IC = [", round(ic$conf.int[1],3),
    ";", round(ic$conf.int[2],3), "]"),
  x = "",
  y = "Points moyens") +
theme_minimal() +
theme(plot.title = element_text(face="bold", size=14),
  axis.text.x = element_blank())

# 4.2.1 Points par Ecurie — tapply() + Boxplot ----
# tapply() — moyenne des points par ecurie
moy_ecurie <- tapply(f1$points,
  f1$name_constructor,
  mean,
  na.rm=TRUE)
top10 <- sort(moy_ecurie, decreasing=TRUE)[1:10]
cat("=== POINTS MOYENS TOP 10 ECURIES ===\n")
print(round(top10, 2))
# Boxplot groupé — top 5 ecuries
top5_noms <- names(sort(moy_ecurie, decreasing=TRUE)[1:5])
f1_top5 <- f1[f1$name_constructor %in% top5_noms, ]
boxplot(points ~ name_constructor,
  data = f1_top5,
  col = rainbow(5),
  main = "Distribution des Points — Top 5 Ecuries",
  xlab = "Ecurie",
  ylab = "Points",
  las = 2,
  cex.axis = 0.7)

# tapply() — points moyens par decennie
moy_dec <- tapply(f1$points, f1$decennie, mean, na.rm=TRUE)
moy_dec <- sort(moy_dec)
cat("=== POINTS MOYENS PAR DECENNIE ===\n")
print(round(moy_dec, 3))
barplot(moy_dec,
  col = heat.colors(length(moy_dec)),
  main = "Points Moyens par Decennie",
  xlab = "Decennie",
  ylab = "Points moyens",
  las = 2)

# 4.3.1 Tableau de Contingence — Nationalite vs Victoires ----
# On cree une variable victoire binaire lisible
# na.rm pour eviter les problemes avec les NA
f1$a_gagne <- ifelse(!is.na(f1$victoire) & f1$victoire == 1,
  "Vainqueur",
  "Non vainqueur")
# Top 6 nationalites
top6_nat <- names(head(sort(table(f1$nationality_race),
      decreasing=TRUE), 6))
f1_top6 <- f1[f1$nationality_race %in% top6_nat, ]
# Tableau de contingence
tab_cont <- table(f1_top6$nationality_race, f1_top6$a_gagne)
cat("=== TABLEAU DE CONTINGENCE ===\n")
print(tab_cont)
# Proportions
cat("\n=== PROPORTIONS (%) ===\n")
print(round(prop.table(tab_cont) * 100, 2))

# 4.3.2 Test du Khi-deux ----
# Supprimer les NA du tableau avant le test
tab_cont_clean <- tab_cont[rowSums(tab_cont) > 0,
  colSums(tab_cont) > 0]
# Test khi-deux
# H0 : les deux variables sont independantes
# H1 : les deux variables sont dependantes
chi2 <- chisq.test(tab_cont_clean)
print(chi2)
cat("\n=== INTERPRETATION ===\n")
if(!is.na(chi2$p.value) & chi2$p.value < 0.05){
  cat("p-value < 0.05 : On rejette H0\n")
  cat("Conclusion : Il existe un lien entre nationalite et victoires\n")
} else if(!is.na(chi2$p.value) & chi2$p.value >= 0.05){
  cat("p-value > 0.05 : On accepte H0\n")
  cat("Conclusion : Pas de lien significatif\n")
} else {
  cat("p-value non calculable\n")
}

# 4.3.3 Test de Fisher ----
# On cree un tableau 2x2 : British vs autres / Vainqueur vs non
f1$british <- ifelse(f1$nationality_race == "British",
  "British", "Autres")
tab_fisher <- table(f1$british, f1$a_gagne)
cat("=== TABLEAU 2x2 POUR TEST DE FISHER ===\n")
print(tab_fisher)
# Test de Fisher
fisher <- fisher.test(tab_fisher)
print(fisher)

# 4.3.4 Estimation Locale de Densité — Grid vs Position ----
library(MASS)
# Isoler les deux variables
tmp <- f1[, c("grid", "position")]
tmp <- tmp[complete.cases(tmp), ]
# Estimation de la densite locale
filled.contour(kde2d(tmp$grid, tmp$position),
  color = terrain.colors,
  main = "Densite Locale — Grid vs Position",
  xlab = "Position de depart",
  ylab = "Position finale")

# 4.3.5 Grid vs Position — Europe vs Autres ----
# Groupe 1 — pilotes britanniques
brit <- f1[f1$nationality_race == "British" &
  !is.na(f1$grid) & !is.na(f1$position), ]
# Groupe 2 — pilotes allemands
germ <- f1[f1$nationality_race == "German" &
  !is.na(f1$grid) & !is.na(f1$position), ]
# plot() pour le premier groupe
plot(brit$grid, brit$position,
  pch = 7,
  col = "blue",
  main = "Grid vs Position — British vs German",
  xlab = "Position de depart",
  ylab = "Position finale",
  las = 1)
# points() pour ajouter le second groupe
points(germ$grid, germ$position,
  pch = 14,
  col = "red")
# Legende
legend(x = "bottomright",
  legend = c("British", "German"),
  col = c("blue", "red"),
  pch = c(7, 14))

# 4.4 Analyse Factorielle des Correspondances (AFC) ----
# Preparation du tableau de contingence pour l AFC
f1_afc <- merge(f1, status, by="statusId")
# On prend top 5 ecuries et top 4 statuts seulement
top5_ec <- names(head(sort(table(f1_afc$name_constructor),
      decreasing=TRUE), 5))
top4_st <- names(head(sort(table(f1_afc$status),
      decreasing=TRUE), 4))
# Filtrage strict
f1_afc2 <- f1_afc[f1_afc$name_constructor %in% top5_ec &
  f1_afc$status %in% top4_st, ]
# Supprimer les niveaux vides
f1_afc2$name_constructor <- droplevels(f1_afc2$name_constructor)
f1_afc2$status <- droplevels(f1_afc2$status)
# Tableau de contingence
tab_afc <- table(f1_afc2$name_constructor, f1_afc2$status)
cat("=== TABLEAU DE CONTINGENCE AFC ===\n")
print(tab_afc)
# Verification — toutes les cases > 0 ?
cat("\nMin des cases :", min(tab_afc), "\n")
# Test khi-deux avant AFC
chi2_afc <- chisq.test(tab_afc)
print(chi2_afc)
cat("\n=== INTERPRETATION ===\n")
if(!is.na(chi2_afc$p.value)){
  if(chi2_afc$p.value < 0.05){
    cat("p-value < 0.05 : variables dependantes\n")
    cat("L AFC est justifiee !\n")
  } else {
    cat("p-value > 0.05 : variables independantes\n")
  }
} else {
  cat("p-value non calculable\n")
}
# AFC
res_afc <- CA(tab_afc, graph=TRUE)

# Valeurs propres
vp_afc <- get_eigenvalue(res_afc)
cat("=== VALEURS PROPRES AFC ===\n")
print(vp_afc)
# Scree plot
fviz_screeplot(res_afc,
  addlabels = TRUE,
  ylim = c(0, 80))

# Biplot AFC
fviz_ca_biplot(res_afc,
  repel = TRUE)

# Qualite de representation 6
row_afc <- get_ca_row(res_afc)
corrplot(row_afc$cos2,
  is.corr = FALSE,
  method = "number")

# 4.4.1 Matrice de Corrélation — ggcorrplot ----
# ggcorrplot — version pro de corrplot
# Plus lisible et plus esthetique
library(ggcorrplot)
vars_num <- f1[, c("points","grid","laps",
    "positionOrder","year")]
vars_num <- vars_num[complete.cases(vars_num), ]
mat_cor <- cor(vars_num, use="complete.obs")
ggcorrplot(mat_cor,
  method = "circle",
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  colors = c("steelblue","white","darkred"),
  title = "Matrice de Correlation — Variables F1",
  ggtheme = theme_minimal())

# 4.4.2 Nuage de Points Pro — grid vs position ----
# ggplot2 — nuage de points avec regression
# geom_smooth() ajoute la droite de regression
f1_clean <- f1[!is.na(f1$grid) & !is.na(f1$positionOrder), ]
ggplot(f1_clean, aes(x=grid, y=positionOrder)) +
geom_point(alpha=0.1,
  color="darkred",
  size =0.8) +
geom_smooth(method = "lm",
  color = "blue",
  lwd = 1.5,
  se = TRUE) +
labs(title = "Position de Depart vs Position Finale",
  subtitle = "Avec droite de regression lineaire",
  x = "Position de depart (grille)",
  y = "Position finale",
  caption = "Source : Kaggle F1 Dataset") +
theme_minimal() +
theme(plot.title = element_text(face="bold", size=14))

# 4.4.3 Evolution des Victoires par Ecuries — ggplot2 ----
# Top 5 ecuries
top5_ec_names <- names(sort(tapply(f1$victoire,
      f1$name_constructor,
      sum, na.rm=TRUE),
    decreasing=TRUE)[1:5])
f1_top5_ec <- f1[f1$name_constructor %in% top5_ec_names &
  !is.na(f1$year), ]
# Victoires par ecurie et par decennie
vict_dec <- f1_top5_ec %>%
filter(!is.na(victoire)) %>%
group_by(name_constructor, decennie) %>%
summarise(victoires = sum(victoire, na.rm=TRUE),
  .groups = "drop")
# Lineplot par ecurie
ggplot(vict_dec, aes(x=decennie,
    y=victoires,
    color=name_constructor,
    group=name_constructor)) +
geom_line(lwd=1.5) +
geom_point(size=3) +
labs(title = "Evolution des Victoires par Ecurie",
  subtitle = "Top 5 ecuries — par decennie",
  x = "Decennie",
  y = "Nombre de victoires",
  color = "Ecurie",
  caption = "Source : Kaggle F1 Dataset") +
theme_minimal() +
theme(plot.title = element_text(face="bold", size=14),
  axis.text.x = element_text(angle=45, hjust=1))

# 4.4.4 Heatmap — Victoires par Nationalite et Decennie ----
# Heatmap professionnelle avec ggplot2
# geom_tile() cree une carte de chaleur
top5_nat <- names(head(sort(table(victoires$nationality_race),
      decreasing=TRUE), 5))
f1_heat <- f1[f1$nationality_race %in% top5_nat &
  !is.na(f1$victoire) &
  !is.na(f1$decennie), ]
heat_df <- f1_heat %>%
group_by(nationality_race, decennie) %>%
summarise(victoires = sum(victoire, na.rm=TRUE),
  .groups = "drop")
ggplot(heat_df, aes(x=decennie,
    y=nationality_race,
    fill=victoires)) +
geom_tile(color="white", lwd=0.5) +
geom_text(aes(label=victoires),
  color="white",
  size =3,
  fontface="bold") +
scale_fill_gradient(low="steelblue",
  high="darkred") +
labs(title = "Victoires par Nationalite et Decennie",
  subtitle = "Top 5 nationalites",
  x = "Decennie",
  y = "Nationalite",
  fill = "Victoires",
  caption = "Source : Kaggle F1 Dataset") +
theme_minimal() +
theme(plot.title = element_text(face="bold", size=14),
  axis.text.x = element_text(angle=45, hjust=1))

# 4.4.5 Violin Plot — Points par Ecurie ----
# Violin plot — combine boxplot et densite
# Montre la distribution complete de chaque groupe
top5_noms <- names(sort(tapply(f1$points,
      f1$name_constructor,
      mean, na.rm=TRUE),
    decreasing=TRUE)[1:5])
f1_violin <- f1[f1$name_constructor %in% top5_noms, ]
ggplot(f1_violin, aes(x=reorder(name_constructor,
      points, FUN=median),
    y=points,
    fill=name_constructor)) +
geom_violin(alpha=0.7, trim=FALSE) +
geom_boxplot(width=0.1,
  fill ="white",
  alpha=0.8) +
coord_flip() +
labs(title = "Distribution des Points — Top 5 Ecuries",
  subtitle = "Violin plot avec boxplot integre",
  x = "Ecurie",
  y = "Points",
  caption = "Source : Kaggle F1 Dataset") +
theme_minimal() +
theme(legend.position = "none",
  plot.title = element_text(face="bold", size=14))

# 4.4.6 Regression Multiple — Visualisation des Residus ----
# Recalcul de reg_multi directement dans ce chunk
reg_multi <- lm(points ~ grid + laps + year, data=f1)
# Diagnostic complet de la regression
reg_diag <- data.frame(
  fitted = fitted(reg_multi),
  residus = residuals(reg_multi)
)
# Residus vs valeurs ajustees
ggplot(reg_diag, aes(x=fitted, y=residus)) +
geom_point(alpha=0.2, color="darkred", size=0.8) +
geom_hline(yintercept=0,
  color="blue",
  lwd =1.5,
  lty =2) +
geom_smooth(method="loess",
  color ="orange",
  se =FALSE) +
labs(title = "Residus vs Valeurs Ajustees",
  subtitle = "Verification de l homoscedasticite",
  x = "Valeurs ajustees",
  y = "Residus",
  caption = "Source : Kaggle F1 Dataset") +
theme_minimal() +
theme(plot.title = element_text(face="bold", size=14))

# 5.1.1 Preparation des donnees ----
# Selection des variables numeriques pour l ACP
vars_acp <- f1[, c("points","grid","laps",
    "positionOrder","year")]
# Supprimer les lignes avec des valeurs manquantes
vars_acp <- vars_acp[complete.cases(vars_acp), ]
cat("=== DONNEES POUR L ACP ===\n")
cat("Dimensions :", dim(vars_acp), "\n")
summary(vars_acp)

# 5.1.2 Application de l ACP ----
# PCA()
# scale.unit=TRUE normalise les donnees automatiquement
res_acp <- PCA(vars_acp,
  scale.unit = TRUE,
  ncp = 5,
  graph = TRUE)

# 5.1.3 Valeurs Propres ----
# Valeurs propres
eigval <- get_eigenvalue(res_acp)
cat("=== VALEURS PROPRES ===\n")
print(eigval)
# Scree plot
fviz_eig(res_acp,
  addlabels = TRUE,
  ylim = c(0, 60))

# 5.1.4 Graphe des Variables ----
# Extraction des resultats des variables
graph_var <- get_pca_var(res_acp)
# Qualite de representation cos2
cat("=== QUALITE DE REPRESENTATION (cos2) ===\n")
print(round(graph_var$cos2, 3))
# Visualisation cos2 avec corrplot
corrplot(graph_var$cos2,
  is.corr = FALSE,
  method = "number")

# 5.1.5 Qualite de Representation des Variables ----
# Colorer les variables selon cos2
fviz_pca_var(res_acp,
  col.var = "cos2",
  gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
  repel = TRUE)

# 5.1.6 Graphe des Individus ----
fviz_pca_ind(res_acp,
  col.ind = "cos2",
  gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
  repel = FALSE,
  alpha = 0.1,
  geom = "point")

# 5.2 Regression Lineaire Multiple ----
# Regression lineaire multiple
# Y ~ X1 + X2 + X3 : Y explique par X1, X2 et X3
reg_multi <- lm(points ~ grid + laps + year,
  data = f1)
summary(reg_multi)

# Visualisation des residus
par(mfrow=c(2,2))
plot(reg_multi)

par(mfrow=c(1,1))

# 5.3.1 Preparation des donnees pilotes ----
# Agregation des stats par pilote
stats_pilotes <- f1 %>%
group_by(driver_name) %>%
summarise(
  total_points = sum(points, na.rm=TRUE),
  nb_courses = n(),
  nb_victoires = sum(victoire, na.rm=TRUE),
  nb_podiums = sum(podium, na.rm=TRUE),
  moy_grid = mean(grid, na.rm=TRUE),
  moy_laps = mean(laps, na.rm=TRUE)
) %>%
filter(nb_courses >= 10)
cat("=== STATISTIQUES PAR PILOTE ===\n")
cat("Nombre de pilotes :", nrow(stats_pilotes), "\n")
head(stats_pilotes, 10)

# 5.3.2 Choix du nombre de clusters ----
# Selection des variables numeriques pour le clustering
data_kmeans <- stats_pilotes[, c("total_points",
    "nb_victoires",
    "nb_podiums",
    "moy_grid")]
# Normalisation des donnees
data_kmeans_scale <- scale(data_kmeans)
# Methode du coude pour choisir K
wss <- sapply(1:8, function(k){
    kmeans(data_kmeans_scale,
      centers = k,
      nstart = 25)$tot.withinss
  })
plot(1:8, wss,
  type = "b",
  pch = 19,
  col = "darkred",
  main = "Methode du Coude — Choix de K",
  xlab = "Nombre de clusters K",
  ylab = "Inertie intra-classe")

# 5.3.3 Application du K-means ----
# Application du K-means avec K=3
set.seed(123)
km <- kmeans(data_kmeans_scale,
  centers = 3,
  nstart = 25)
# Ajout du cluster aux donnees
stats_pilotes$cluster <- as.factor(km$cluster)
cat("=== REPARTITION DES CLUSTERS ===\n")
table(stats_pilotes$cluster)

# Visualisation des clusters avec factoextra
fviz_cluster(km,
  data = data_kmeans_scale,
  palette = c("darkred","steelblue","darkgreen"),
  main = "Clustering K-means des Pilotes F1",
  repel = FALSE,
  alpha = 0.5)

# 5.3.4 Validation du Clustering — Silhouette ----
library(cluster)
sil <- silhouette(km$cluster, dist(data_kmeans_scale))
fviz_silhouette(sil,
  palette = c("darkred","steelblue","darkgreen"),
  main = "Silhouette — Validation du Clustering")

# Statistiques moyennes par cluster
cat("=== CARACTERISTIQUES DES CLUSTERS ===\n")
aggregate(cbind(total_points, nb_victoires,
    nb_podiums, moy_grid) ~ cluster,
  data = stats_pilotes,
  FUN = mean)

# 5.3.5 ACP — Biplot Professionnel ----
# Biplot ACP — individus + variables ensemble
# fviz_pca_biplot() — combine les deux graphiques
fviz_pca_biplot(res_acp,
  repel = FALSE,
  col.var = "darkred",
  col.ind = "steelblue",
  alpha.ind = 0.2,
  label = "var",
  title = "Biplot ACP — F1 (1950-2024)")

# 5.3.6 ACP — Contribution des Variables ----
# Contribution de chaque variable aux composantes
fviz_contrib(res_acp,
  choice = "var",
  axes = 1,
  fill = "darkred",
  color = "black",
  title = "Contribution des Variables — Axe 1")

fviz_contrib(res_acp,
  choice = "var",
  axes = 2,
  fill = "steelblue",
  color = "black",
  title = "Contribution des Variables — Axe 2")

# 5.3.7 Clustering — Profil des Clusters ----
# Profil detaille de chaque cluster
profil_clusters <- stats_pilotes %>%
group_by(cluster) %>%
summarise(
  nb_pilotes = n(),
  moy_points = round(mean(total_points), 1),
  moy_victoires = round(mean(nb_victoires), 1),
  moy_podiums = round(mean(nb_podiums), 1),
  moy_courses = round(mean(nb_courses), 1),
  moy_grid = round(mean(moy_grid), 1),
  .groups = "drop"
)
cat("=== PROFIL DES CLUSTERS ===\n")
print(profil_clusters)

# 5.3.8 Clustering — Visualisation ggplot2 ----
# Visualisation pro des clusters avec ggplot2
ggplot(stats_pilotes,
  aes(x = total_points,
    y = nb_victoires,
    color= cluster,
    size = nb_courses)) +
geom_point(alpha=0.7) +
scale_color_manual(values=c("darkred",
    "steelblue",
    "darkgreen")) +
labs(title = "Clustering des Pilotes F1",
  subtitle = "Points totaux vs Victoires",
  x = "Points totaux",
  y = "Nombre de victoires",
  color = "Cluster",
  size = "Nb courses",
  caption = "Source : Kaggle F1 Dataset") +
theme_minimal() +
theme(plot.title = element_text(face="bold", size=14))

# 5.3.9 Top Pilotes par Cluster ----
# Top 3 pilotes par cluster
cat("=== TOP PILOTES PAR CLUSTER ===\n")
for(i in 1:3){
  cat("\n--- Cluster", i, "---\n")
  cluster_i <- stats_pilotes[stats_pilotes$cluster == i, ]
  top3 <- head(cluster_i[order(cluster_i$total_points,
        decreasing=TRUE), ],3)
  print(top3[, c("driver_name","total_points",
        "nb_victoires","nb_courses")])
}

# 5.3.10 Regression Multiple — Tableau Professionnel ----
# Tableau professionnel des resultats de regression
res_reg <- summary(reg_multi)
cat("=== RESULTATS REGRESSION MULTIPLE ===\n")
cat("Variable dependante : Points\n")
cat("Variables independantes : grid + laps + year\n\n")
# Coefficients
coef_df <- as.data.frame(res_reg$coefficients)
coef_df <- round(coef_df, 4)
print(coef_df)
cat("\nR² :", round(res_reg$r.squared, 4), "\n")
cat("R² ajuste :", round(res_reg$adj.r.squared, 4), "\n")
cat("F-statistic :", round(res_reg$fstatistic[1], 2), "\n")
cat("p-value globale : < 2.2e-16\n")
cat("\n=== EQUATION DU MODELE ===\n")
coefs <- coef(reg_multi)
cat("Points =", round(coefs[1],4),
  "+", round(coefs[2],4), "* grid",
  "+", round(coefs[3],4), "* laps",
  "+", round(coefs[4],4), "* year\n")

# 5.3.11 Prediction avec le Modele ----
# Prediction pour de nouveaux pilotes
nouveaux <- data.frame(
  grid = c(1, 5, 10, 20),
  laps = c(70, 60, 50, 30),
  year = c(2024, 2024, 2024, 2024)
)
predictions <- predict(reg_multi,
  newdata = nouveaux,
  interval = "confidence")
resultats <- cbind(nouveaux, round(predictions, 2))
cat("=== PREDICTIONS DE POINTS ===\n")
cat("grid = position depart, laps = tours completes\n\n")
print(resultats)

# 5.3.12 Comparaison ACP vs Clustering ----
# Projection des clusters sur le plan ACP
# Combine ACP et clustering pour une analyse complete
coords_acp <- as.data.frame(res_acp$ind$coord[, 1:2])
names(coords_acp) <- c("Dim1", "Dim2")
# Ajouter les clusters aux coordonnees ACP
# On prend les memes observations
n_acp <- nrow(coords_acp)
ggplot(coords_acp, aes(x=Dim1, y=Dim2)) +
geom_point(alpha=0.3,
  color="darkred",
  size =0.8) +
geom_hline(yintercept=0, lty=2, color="darkgreen") +
geom_vline(xintercept=0, lty=2, color="darkgreen") +
labs(title = "Plan Factoriel ACP — F1 (1950-2024)",
  subtitle = paste("Dim1 + Dim2 expliquent",
    round(sum(get_eigenvalue(res_acp
        )[1:2,2]),1), "% variance"),
  x = paste("Dim1 (", round(get_eigenvalue(res_acp
      )[1,2],1), "%)"),
  y = paste("Dim2 (", round(get_eigenvalue(res_acp
      )[2,2],1), "%)"),
  caption = "Source : Kaggle F1 Dataset") +
theme_minimal() +
theme(plot.title = element_text(face="bold", size=14))
