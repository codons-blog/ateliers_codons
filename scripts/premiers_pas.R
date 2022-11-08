# Ateliers codons
# Premiers pas avec R
# 2022-11-07

# Importer les donnees ----

pingouins <- read.csv("raw/pingouins.csv", check.names = FALSE)

# Nettoyer et manipuler les donnees ----

# Créer une copie de travail
pingouins_propre <- pingouins

# Renommer les colonnes
nouveaux_noms <- gsub("[()]", "", names(pingouins_propre))
nouveaux_noms <- gsub(" ", "_", nouveaux_noms)
nouveaux_noms <- tolower(nouveaux_noms)
names(pingouins_propre) <- nouveaux_noms
rm(nouveaux_noms)

# Selectionner les colonnes
pingouins_propre <- pingouins_propre[, c("espece", "ile", "longueur_bec_mm",
                                         "hauteur_bec_mm", "longueur_aile_mm", 
                                         "masse_corporelle_g")]

# Modifier la variable espece 
pingouins_propre$espece <- gsub(" .*$", "", pingouins_propre$espece)

# Calculer le rapport entre la longueur et la hauteur du bec
pingouins_propre$ratio_bec <- pingouins_propre$longueur_bec_mm / pingouins_propre$hauteur_bec_mm
pingouins_propre$ratio_bec <- round(pingouins_propre$ratio_bec, digits = 2)

# Supprimer les donnees manquantes
pingouins_propre <- pingouins_propre[complete.cases(pingouins_propre), ]

# Calculer la moyenne de la masse corporelle par ile et par espece
aggregate(masse_corporelle_g ~ espece + ile, data = pingouins_propre, FUN = mean)

# Exporter les donnees nettoyees
write.csv(pingouins_propre, "data/pingouins_propre.csv", row.names = FALSE)

# Representer les donnees ----

# Barplot : nombre d'individus par espece
png("figs/nombre_pingouins.png")
barplot(sort(table(pingouins_propre$espece), decreasing = TRUE),
        col = c("Adelie" = "darkorange",
                "Chinstrap" = "cyan4",
                "Gentoo" = "purple"),
        border = NA,
        main = "Pingouins de l'archipel Palmer",
        ylab = "Nombre d'individus par espèce",
        ylim = c(0, 200))
dev.off()

# Boxplot : ratio du bec en fonction de l'espece
png("figs/ratio_bec.png")
boxplot(pingouins_propre$ratio_bec ~ pingouins_propre$espece,
        col = NA,
        border = c("Adelie" = "darkorange",
                   "Chinstrap" = "cyan4",
                   "Gentoo" = "purple"),
        pch = 20,
        main = "Rapport entre la longueur et la hauteur du bec",
        xlab = "",
        ylab = "Ratio")
dev.off()

# Histogramme : distribution de la masse corporelle
png("figs/distribution_masse.png")
hist(pingouins_propre$masse_corporelle_g / 1000,
     breaks = 20,
     col = "lightblue",
     border = "darkblue",
     ylim = c(0, 50),
     main = "Distribution de la masse corporelle",
     xlab = "Masse (kgs)",
     ylab = "Fréquence")
dev.off()

# Scatter plot : longueur des ailes vs longueur du bec
pingouins_propre$couleur[pingouins_propre$espece == "Adelie"] <- "darkorange"
pingouins_propre$couleur[pingouins_propre$espece == "Chinstrap"] <- "purple"
pingouins_propre$couleur[pingouins_propre$espece == "Gentoo"] <- "cyan4"
png("figs/ailes_vs_bec.png")
plot(x = pingouins_propre$longueur_aile_mm, y = pingouins_propre$longueur_bec_mm,
     col = pingouins_propre$couleur,
     pch = 20,
     main = "Relation entre la longueur des ailes et la longueur du bec",
     xlab = "Longueur des ailes (mm)",
     ylab = "Longueur du bec (mm)")
legend(legend = sort(unique(pingouins_propre$espece)),
       x = 215,
       y = 40,
       col = c("darkorange", "purple", "cyan4"),
       pch = 19)
dev.off()
