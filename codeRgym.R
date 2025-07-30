install.packages("stargazer")
install.packages("car")
install.packages("ggplot2")
install.packages("questionr")
install.packages("BioStatR")
install.packages("FactoMineR")
install.packages("corrplot")


library(stargazer)
library(car)
library(ggplot2)
library(questionr)
library(BioStatR)
library(FactoMineR)
library(corrplot)
library(cluster)
library(factoextra)

dataset <- read.csv("updated_dataset.csv")


# Analyse univariée sous forme d’indicateurs statistiques des variables quantitatives
summary(dataset)

# Affichage de l'écart type pour chaque variable
stargazer(dataset,
          
          summary.stat = c("sd"),
          type = "text")



# Création d'un diagramme en barres pour la variable Exercise_Type



exercise_counts <- table(dataset$Exercise_Type)

ggplot(data = as.data.frame(exercise_counts), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Répartition des types d'exercices (Force vs Cardio)",
    x = "Type d'exercice",
    y = "Nombre d'individus"
  ) +
  scale_fill_manual(values = c("skyblue", "orange")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 14)
  )


Experience_Level <- dataset$Experience_Level


frequence <- freq(Experience_Level)
print(frequence)

# Tracé du diagramme en barres de Experience_Level
barplot(frequence$`%`,
        ylab = "Fréquence relative (%)",
        names.arg = rownames(frequence),
        col = "skyblue",
        main = "Diagramme en barres des niveaux d'expérience")

# Boîtes à moustaches Muscle_Mass et Exercise_Type
Boxplot(Muscle_Mass~Exercise_Type, data = dataset)

## Calcul du eta2 entre Muscle_Mass et Exercise_Type
eta2(dataset$Muscle_Mass,dataset$Exercise_Type)

## Test de l'association
anova(lm(dataset$Muscle_Mass~dataset$Exercise_Type))



# On effectue l'ACP avec les colonnes spécifiques pour les variables supplémentaires
res.pca <- PCA(dataset,
               quanti.sup = c(3, 4),  # Colonnes Poids et Taille comme variables quantitatives supplémentaires
               quali.sup = c(2, 17),  # Colonnes Genre et Type d'exercice comme variables qualitatives supplémentaires
               graph = FALSE)


write.infile(res.pca, file = "resultats_ACP.csv")



res.pca$eig_rounded <- round(res.pca$eig, 5)
 

print(res.pca$eig_rounded)



barplot(res.pca$eig[, 1], 
        las = 2,                      
        main = "Valeurs propres",    
        ylab = "Valeurs propres")




# Nuage de points des individus 1er et 2nd plan
plot.PCA(res.pca, choix = "ind", label = "quali")

plot.PCA(res.pca, choix = "ind", label = "quali", axes = c(3, 4))

# Graphes des variables 1er et 2nd plan
plot.PCA(res.pca, choix = "var")

plot.PCA(res.pca, choix = "var", axes = c(3, 4))




# Sélectionner uniquement les colonnes numériques du jeu de données
numeric_data <- dataset[sapply(dataset, is.numeric)]

# Calculer la matrice de corrélation avec les colonnes numériques
cor_matrix <- cor(numeric_data, use = "complete.obs", method = "pearson")

# Afficher la matrice de corrélation
print(cor_matrix)

# Afficher le graphique de la matrice de corrélation

corrplot(cor_matrix, 
         method = "circle", 
         type = "full", 
         tl.col = "black", 
         tl.srt = 45, 
         addCoef.col = "black", 
         number.cex = 0.7, 
         diag = FALSE)


#CAH

#dendogramme

res.agnes <- agnes(res.pca$ind$coord, method = "ward")

plot(res.agnes, main = "Dendogramme", xlab = "Individus", which.plots = 2, labels = FALSE)



# Sélectionner les 10 plus grands gains d'inertie
heights <- sort(res.agnes$height, decreasing = TRUE)[1:16]

barplot(
  heights,
  las = 2,                              
  names.arg = 1:16,                     
  xlab = "Nombre de classes",
  ylab = "Gains d’inertie",
  main = "Diagramme des gains d’inertie",
  col = ifelse(1:length(heights) <= 4, "red", "gray")  
)

# Découper la CAH en 4 classes
classe_finale <- cutree(res.agnes, k = 4)
 
# Ajouter la nouvelle variable "classe" dans le jeu de données
dataset$classe <- as.factor(classe_finale)
 
# Résumé du jeu de données avec la nouvelle variable "classe"
summary(dataset)

#Classes 
p <- fviz_pca_ind(res.pca, 
             col.ind = dataset$classe,  
             palette = "jco",            
             addEllipses = TRUE,       
             label = "centroids",         
             pointshape = 16,            
             legend.title = "Classe")    

p + geom_text(data = centroids, aes(x = Dim.1, y = Dim.2, label = label), color = "black", size = 5, vjust = -1)

#valeurs test
catdes(dataset, num.var = 18)

