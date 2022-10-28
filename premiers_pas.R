# Ateliers codons
# Premiers pas avec R et RStudio
# 2022-11-07

# Importer le jeu de donnees ----

pingouins <- read.csv("raw/pingouins.csv", check.names = FALSE)

# Nettoyer et manipuler les donnees ----

# Selectionner les colonnes d'interet
pingouins_propre <- pingouins[, c("Espece", "Longueur Bec (mm)",
                                  "Hauteur Bec (mm)", "Longueur Aile (mm)",
                                  "Masse corporelle (g)")]

# Renommer les colonnes
nouveaux_noms <- gsub("[()]", "", names(pingouins_propre))
nouveaux_noms <- gsub(" ", "_", nouveaux_noms)
nouveaux_noms <- tolower(nouveaux_noms)
names(pingouins_propre) <- nouveaux_noms
rm(nouveaux_noms)

# Modifier la variable espece
pingouins_propre$espece <- gsub(" .*$", "", pingouins_propre$espece)

# Calculer le ratio longueur du bec / hauteur du bec
pingouins_propre$ratio_bec <- round(pingouins_propre$longueur_bec_mm / pingouins_propre$hauteur_bec_mm,
                                    digits = 2)


# Supprimer les donnees manquantes
pingouins_propre <- pingouins_propre[complete.cases(pingouins_propre), ]

# Transformer la variable espece en un facteur
pingouins_propre$espece <- as.factor(pingouins_propre$espece)

# Exporter les donnees nettoyees
write.csv(pingouins_propre, "data/pingouins_propre.csv")

# Representer les donnees ----

# Ouvrir l'outil graphique
png("figs/pingouins_palmer.png",
    width = 3840, height = 1920)

par(mfrow = c(2, 2))

# Barplot
png("figs/nombre_pingouins.png")
barplot(sort(table(pingouins_propre$espece),
             decreasing = TRUE),
        main = "Pingouins de l'archipel Palmer",
        ylab = "Nombre d'individus par espèce",
        ylim = c(0, 200),
        border = NA,
        col = c("darkorange", "cyan4", "purple"))
dev.off()

# Boxplot
png("figs/ratio_bec.png")
boxplot(pingouins_propre$ratio_bec ~ pingouins_propre$espece,
        col = NA,
        border = c("darkorange", "purple", "cyan4"),
        pch = 20,
        main = "Rapport entre la longueur et la hauteur du bec",
        xlab = "",
        ylab = "Ratio")
dev.off()

# Histogramme
png("figs/distribution_masse.png")
hist(pingouins_propre$masse_corporelle_g / 1000,
     breaks = 20,
     col = "lightblue",
     border = "darkblue",
     main = "Distribution de la masse corporelle",
     xlab = "Masse (kgs)",
     ylab = "Fréquence")
dev.off()

# Scatterplot
pingouins_propre$couleur[pingouins_propre$espece == "Adelie"] <- "darkorange"
pingouins_propre$couleur[pingouins_propre$espece == "Chinstrap"] <- "purple"
pingouins_propre$couleur[pingouins_propre$espece == "Gentoo"] <- "cyan4"

png("figs/relation_aile_bec.png")
plot(x = pingouins_propre$longueur_aile_mm,
     y = pingouins_propre$longueur_bec_mm,
     col = pingouins_propre$couleur,
     pch = 20,
     main = "Relation entre la longueur des ailes et la longueur du bec",
     xlab = "Longueur des ailes (mm)",
     ylab = "Longueur du bec (mm)")
legend(legend = unique(pingouins_propre$espece),
       x = 220, y = 40, col = c("darkorange", "cyan4", "purple"), border = NA, pch = 16,
       bty = "n", pt.cex = 1.5, cex = 0.75)
dev.off()
