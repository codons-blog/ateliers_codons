# Ateliers codons
# Introduction a R Markdown
# 2022-11-14

# Importer les donnees ----

pingouins <- read.csv("data/pingouins_propre.csv")

# Explorer les donnees ----

str(pingouins)

# Representer les donnees ----

# Barplot : nombre d'individus par espece
barplot(sort(table(pingouins$espece), decreasing = TRUE),
        col = c("Adelie" = "darkorange",
                "Chinstrap" = "cyan4",
                "Gentoo" = "purple"),
        border = NA,
        main = "Pingouins de l'archipel Palmer",
        ylab = "Nombre d'individus par espèce",
        ylim = c(0, 200))


# Boxplot : ratio du bec en fonction de l'espece
pingouins$ratio_bec <- round(pingouins$longueur_bec_mm / pingouins$hauteur_bec_mm,
                             digits = 2)
boxplot(pingouins$ratio_bec ~ pingouins$espece,
        col = NA,
        border = c("Adelie" = "darkorange",
                   "Chinstrap" = "cyan4",
                   "Gentoo" = "purple"),
        pch = 20,
        main = "Rapport entre la longueur et la hauteur du bec",
        xlab = "",
        ylab = "Ratio")

# Histogramme : distribution de la masse corporelle
hist(pingouins$masse_corporelle_g / 1000,
     breaks = 20,
     col = "lightblue",
     border = "darkblue",
     ylim = c(0, 50),
     main = "Distribution de la masse corporelle",
     xlab = "Masse (kgs)",
     ylab = "Fréquence")

# Scatter plot : longueur des ailes vs longueur du bec
pingouins$couleur[pingouins$espece == "Adelie"] <- "darkorange"
pingouins$couleur[pingouins$espece == "Chinstrap"] <- "purple"
pingouins$couleur[pingouins$espece == "Gentoo"] <- "cyan4"
plot(x = pingouins$longueur_aile_mm, y = pingouins$longueur_bec_mm,
     col = pingouins$couleur,
     pch = 20,
     main = "Relation entre la longueur des ailes et la longueur du bec",
     xlab = "Longueur des ailes (mm)",
     ylab = "Longueur du bec (mm)")
legend(legend = sort(unique(pingouins$espece)),
       x = 215,
       y = 40,
       col = c("darkorange", "purple", "cyan4"),
       pch = 19)
