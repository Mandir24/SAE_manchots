#' ---
#' title : "Analyse de données sur un échantillon de manchots"
#' author : " DIOP Mandir, Yon Anthony, Prieur Noam"
#' date : "24 avril 2024"
#' ---

# ============================================================================= #
# IUT GON - (SSTI203) Régressions sur données réelles
# Données : données d'une station dans les archipels Palmer en Antarctique
# source : Palmerpenguins
# ============================================================================= #


# ====================================================================

# ================= Chargement des données =================

# visualisation de la structure des données

readLines(con = "../Data/donnees_manchos.txt", 
          n=10)

read.table(file = "../Data/donnees_manchos.txt", 
           sep = '\t',
           header = TRUE ) -> dataset

# Le séparateur c'est le "\t" 
# On note la présence du nom des champs 

View(dataset)

head(dataset)

# le jeu de données comprend 344 observations et 8 variables

str(dataset)

dataset[complete.cases(dataset$body_mass_g, dataset$sex,dataset$bill_length_mm,dataset$bill_depth_mm,dataset$flipper_length_mm), ] -> dataset2 
summary(dataset2)

data <- na.omit(dataset2)

# ========================================================================== #

# ================= Préparation des données =================

dataset <- within(dataset2,
                    {
                     year = factor(year)
                     sex = factor(dataset2$sex)
                     island = factor(dataset2$island)
                     species = factor(dataset2$species)
                    })
str(dataset)


# Isolons les variables d'intérêts que nous voulons croiser 

subset(dataset,
       select = c(body_mass_g,bill_length_mm,bill_depth_mm,flipper_length_mm))-> data



# ========================================================================== #

# ================= Analyse exploratoire du jeu de données =================

# Visualisons la matrice des corrélations

pairs(data,
      panel = panel.smooth,
      pch = 20,
      cex = 1.3,
      col = "blue",
      lwd = 2,
      main = "Matrice des nuages de dispersion",
      las = 1)


# ================= Étape 1 =================

# Création de la fonction pour visualiser la distribution des variables
# des variables body_mass_g, bill_length_mm, bill_depth_mm et flipper_length_mm

distribution <- function(var){
  
  moy <- mean(data[[var]], na.rm = TRUE)
  mediane <- median(data[[var]], na.rm = TRUE)
  
  hist(data[[var]], 
       col = "lightblue",
       probability = TRUE,
       main = paste0("Distribution de la variable ", var),
       xlab = ifelse(var == "bill_length_mm",
                     "Longueur de la crête supérieure du bec",
                     ifelse(var == "bill_depth_mm",
                            "Profondeur de la crête supérieure du bec", 
                            ifelse(var == "flipper_length_mm",
                                   "Longueur des nageoires",
                                   "Poids des manchots"))),
       ylab = "Densité")
  
  lines(density(data[[var]]), col = "red", lwd = 2)
  
  abline(v = moy, col = "green", lty = 2, lwd = 1)
  abline(v = mediane, col = "purple", lty = 2, lwd = 1)
  
  legend("topright", 
         legend = c("Moyenne", "Médiane"), 
         col = c("green", "purple"), 
         lty = 2, 
         lwd = 1, 
         cex = 0.8)
}


# Réalisons les distributions 

variable <- c("body_mass_g", "bill_length_mm", "bill_depth_mm", "flipper_length_mm")


# Appel de la focntion distribution
par(mfrow = c(2, 2))

distribution(var = "body_mass_g")
distribution(var = "bill_length_mm")
distribution(var = "bill_depth_mm")
distribution(var = "flipper_length_mm")

# Représentation des nuages de dispersion et de la régression lissée

# Création de la fonction pour la réalisation des nuages de dispersion

NuageDispersion <- function(var){
  plot(data[[var]], data$body_mass_g, 
       pch = 21, col = "black", bg = "blue",
       xlab = ifelse(var == "bill_length_mm",
                     "Longueur de la crête supérieure du bec",
                     ifelse(var == "bill_depth_mm",
                            "Profondeur de la crête supérieure du bec",
                            "Longueur des nageoires")),
       ylab = "Poids du manchot",
       main = paste0("Nuage de dispersion : body_mass_g versus ", var))
  
  # on ordonne les données de la variable explicative
  # avant de tracer la droite de régrssion lissée
  sorted_indices <- order(data[[var]])
  sorted_var <- data[[var]][sorted_indices]
  sorted_body_mass_g <- data$body_mass_g[sorted_indices]
  
  loess_fit <- loess(sorted_body_mass_g ~ sorted_var)
  loess_pred <- predict(loess_fit, data.frame(x = sorted_var))
  lines(sorted_var, loess_pred, col = "red", lwd = 1)
}

# Appel de la fonction Nuage de dispersion

par(mfrow = c(2, 2))

NuageDispersion(var = "bill_length_mm")
NuageDispersion(var = "flipper_length_mm")
NuageDispersion(var = "bill_depth_mm")

par(mfrow = c(1,1))


# ========================================================================== #

# ================= Étape 2 : Modélisation =================

# Création de modèle de régression linéaire qui traduit l'association linéaire 
# entre la variable "body_mass_g" et les variables explicatives


# Création d'une fonction pour réaliser la modélisation entre les variables
model <- function(var){
  lm(formula = body_mass_g ~ data[[var]],
     data = data)
}

# Regroupons les modèle dans un seul objet pour ensuite les comparer

modele <- list(bill_length = model("bill_length_mm"),
               bill_depth = model("bill_depth_mm"),
               flipper_length = model("flipper_length_mm"))


# ================= Comparaison des modèles =================
lapply(X = modele,
       FUN = summary) -> extractmodels

print(extractmodels)


sapply(X = seq_along(extractmodels),
       FUN = function(x) round(100*extractmodels[[x]]$r.squared,2)) -> R.Squared

setNames(object = R.Squared,
         nm = paste0("model",1:3)) -> R.Squared

print(x = R.Squared)


# le modéle poids versus longueur des nageoires a un coefficient de détermination
# R = 76.21. c'est à dire 76.21 % de la variation observée pour le poids peut être 
#expliquer par la variation de la longueur des nageoires 
# dans notre modèle de régression


# Création d'un tableau récapitulatif des coefficients

table <- data.frame(
  variable = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm"),
  beta_0 = c(
    modele$bill_length$coefficients[1],
    modele$bill_depth$coefficients[1],
    modele$flipper_length$coefficients[1]
  ),
  beta_1 = c(
    modele$bill_length$coefficients[2],
    modele$bill_depth$coefficients[2],
    modele$flipper_length$coefficients[2]
  ),
  R2 = c(
    summary(modele$bill_length)$r.squared,
    summary(modele$bill_depth)$r.squared,
    summary(modele$flipper_length)$r.squared
  ),
  row.names = NULL
)

table$beta_0 <- round(table$beta_0, 2)
table$beta_1 <- round(table$beta_1, 2)

# Formatting R2 as percentage
table$R2 <- paste0(format(table$R2 * 100, digits = 2), "%")

# Renaming columns
colnames(table) <- c(
  "Variable explicave",
  "beta 0",
  "beta 1",
  "R2"
)

print(table)

flextable::flextable(table) # Interprétation des coefficients sur le tableau 

# Création de la fonction pour la  représentation des 
# nuages de dispersion droite des moindres carrés et regression lissée

NuageDispersion2 <- function(var) {
  plot(dataset[[var]], dataset$body_mass_g,
       pch = 21, col = "blue", bg = "black",
       xlab = ifelse(var == "bill_length_mm",
                     "Longueur de la crête supérieure du bec",
                     ifelse(var == "bill_depth_mm",
                            "Profondeur de la crête supérieure du bec",
                            "Longueur des nageoires")),
       ylab = "Poids du manchot",
       main = paste0("Nuage de dispersion : body_mass_g versus ", var))
  
  # Ordonner les données
  sorted_indices <- order(dataset[[var]])
  sorted_var <- dataset[[var]][sorted_indices]
  sorted_body_mass_g <- data$body_mass_g[sorted_indices]
  
  # Ajout de la courbe de lissage loess
  loess_fit <- loess(sorted_body_mass_g ~ sorted_var)
  loess_pred <- predict(loess_fit, data.frame(x = sorted_var))
  lines(sorted_var, loess_pred, col = "red", lwd = 1)
  
  # Ajout de la courbe de régression linéaire
  lm_fit <- lm(sorted_body_mass_g ~ sorted_var)
  abline(lm_fit, col = "green1")
  
  legend(x = "topleft", 
         legend = c("Régression lissée", "droite des moindres carrés"), 
         col = c("red","green1"), 
         lty = c(1,1),
         bty = "n",
         lwd = c(1,1), 
         cex = 0.4
         )
}

variable_explicative <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm")


# Appel de la fonction Nuage de dispersion

par(mfrow = c(2, 2))

NuageDispersion2( "bill_length_mm")
NuageDispersion2( "flipper_length_mm")
NuageDispersion2("bill_depth_mm")

par(mfrow = c(1, 1))


# Création du modéle pour la variable explicative avec le meilleur modéle

within(data,{
  std.residuals = rstandard(model("flipper_length_mm"));
}) -> data_resid

data_resid = data.frame(data_resid,
                  predict = predict(model("flipper_length_mm"),
                                    interval = "prediction"))
head(x = data_resid,
     n = 10)

with(data_resid,
     loess(formula =  body_mass_g ~ flipper_length_mm)) -> smooth

data_resid = data.frame(data_resid,
                  loess = fitted(smooth))

data_resid[order(data$flipper_length_mm),] -> data_resid


fit1 = model("flipper_length_mm")
summary(fit1)

with(data_resid,
     plot(x = flipper_length_mm,
          y = body_mass_g,
          pch = 21,
          bg = "red2",
          main = "Nuage de dispersion 'poids' versus 'long_nag' ",
          xlab = "Longueur des nageoires",
          ylab = "Poids du manchot",
          las = 1))

with(data_resid,
     lines(x = flipper_length_mm,
           y = loess,
           col = "blue",
           lty = 2,
           lwd = 2))

with(data_resid,
     lines(x = flipper_length_mm,
           y = predict.fit,
           col = "purple3",
           lty = 1,
           lwd = 3))

with(data_resid,
     lines(x = flipper_length_mm,
           y = predict.lwr,
           col = "orange2",
           lty = 4,
           lwd = 3))

with(data_resid,
     lines(x = flipper_length_mm,
           y = predict.upr,
           col = "orange2",
           lty = 4,
           lwd = 3))

text(x = 180,
     y = 6000,
     labels = paste0("poids =", round(coef(fit1)[1],2),
                                            ifelse(sign(coef(fit1)[2]),"+","-"),
                                                        round(abs(coef(fit1)[2]),2)," * Longueur des nageoires"))

text(x = 178,
     y = 5700,
     labels = paste0("R-Squared = ", round(100*summary(fit1)$r.squared,2),"%"))

# ================= Étape 3 : Sexe  =================

# ================= Préparation des données 2 =================

# Création du dataset male 
data_male <- subset(dataset, sex == "male", 
                    select = c(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm))

str(data_male)

# Création du dataset female
data_female <- subset(dataset, sex == "female",
                      select = c(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm))

str(data_female)

# ========================================================================== #

# ================= Analyse exploratoire du jeu de données =================

# Visualisons la matrice des corrélations

pairs(data_male,
      panel = panel.smooth,
      pch = 20,
      cex = 1.3,
      col = "blue",
      lwd = 2,
      main = "Matrice des nuages de dispersion : (Male) ",
      las = 1)

pairs(data_female,
      panel = panel.smooth,
      pch = 20,
      cex = 1.3,
      col = "blue",
      lwd = 2,
      main = "Matrice des nuages de dispersion : (Female)",
      las = 1)

# ================= Étape 1 =================

# Représentation des nuages de dispersion et de la régression lissée

Graph1 <- function(varx, table){
  # Sélectionner la table appropriée
  if(table == "male") {
    selected_data <- data_male
  } else if(table == "female") {
    selected_data <- data_female
  } else {
    stop("Le paramètre 'table' doit être 'male' ou 'female'")
  }
  
  # Ajuster le modèle Loess
  Loess <- loess(selected_data$body_mass_g ~ selected_data[[varx]])
  
  # Créer un dataframe temporaire avec les données ajustées
  temp <- data.frame(selected_data[, c("body_mass_g", varx)],
                     Loess = fitted(Loess))
  
  # Ordonner les données en fonction de la variable x
  temp <- temp[order(temp[[varx]]),]
  
  # Déterminer le sexe pour le titre
  sex_title <- ifelse(table == "male", "Male", "Female")
  
  # Tracer le nuage de points et la courbe Loess
  plot(selected_data$body_mass_g ~ selected_data[[varx]],
       pch = 20,
       main = paste("Nuage de dispersion : body_mass_g vs ", varx, " (", sex_title, ")", sep = ""),
       xlab = varx,
       ylab = "body_mass_g",
       las = 1)
  
  lines(temp$Loess ~ temp[[varx]],
        col = "red",
        lwd = 2)
}


par(mfrow = c(2,3))

# Appel de la fonction pour représenter les nuages de dispersion male et female
Graph1("bill_length_mm","male")
Graph1("bill_depth_mm","male")
Graph1("flipper_length_mm","male")


Graph1("bill_length_mm","female")
Graph1("bill_depth_mm","female")
Graph1("flipper_length_mm","female")

par(mfrow = c(1,1))

# ================= Étape 2 : Modélisation ================= 

# Création de modèle de régression linéaire qui traduit l'association linéaire 
# entre la variable "body_mass_g" et les variables explicatives

Graph2 <- function(varx, table){
  # Sélectionner la table appropriée
  if(table == "male") {
    selected_data <- data_male
  } else if(table == "female") {
    selected_data <- data_female
  } else {
    stop("Le paramètre 'table' doit être 'male' ou 'female'")
  }
  
  # Ajuster le modèle Loess
  Loess <- loess(selected_data$body_mass_g ~ selected_data[[varx]])
  
  # Ajuster le modèle de régression linéaire
  fit_lm <- lm(selected_data$body_mass_g ~ selected_data[[varx]])
  
  # Créer un dataframe temporaire avec les données ajustées
  temp <- data.frame(selected_data[, c("body_mass_g", varx)],
                     Loess = fitted(Loess))
  
  # Ordonner les données en fonction de la variable x
  temp <- temp[order(temp[[varx]]),]
  
  # Déterminer le sexe pour le titre
  sex_title <- ifelse(table == "male", "Male", "Female")
  
  # Tracer le nuage de points et la courbe Loess
  plot(selected_data$body_mass_g ~ selected_data[[varx]],
       pch = 20,
       main = paste("Nuage de dispersion : body_mass_g vs ", varx, " (", sex_title, ")", sep = ""),
       xlab = varx,
       ylab = "body_mass_g",
       las = 1)
  
  # Tracer la droite des moindres carrés
  abline(fit_lm, col = "blue", lwd = 2)
  
  # Tracer la courbe Loess
  lines(temp$Loess ~ temp[[varx]],
        col = "red",
        lwd = 2)
  
  legend(x = "topleft", 
         legend = c("Régression lissée", "droite des moindres carrés"), 
         col = c("red","blue"), 
         lty = c(1,1),
         bty = "n",
         lwd = c(1,1), 
         cex = 0.4
  )
}

par(mfrow = c(2,3))

# Appel de la fonction pour représenter les nuages de dispersion male et female
Graph2("bill_length_mm","male")
Graph2("bill_depth_mm","male")
Graph2("flipper_length_mm","male")


Graph2("bill_length_mm","female")
Graph2("bill_depth_mm","female")
Graph2("flipper_length_mm","female")

par(mfrow = c(1,1))


# Création d'une fonction pour réaliser la modélisation entre les variables en spécifiant le dataset

model <- function(var, dataset){
  if(dataset == "male") {
    selected_data <- data_male
  } else if(dataset == "female") {
    selected_data <- data_female
  } else {
    stop("Le paramètre 'dataset' doit être 'male' ou 'female'")
  }
  
  lm(formula = body_mass_g ~ selected_data[[var]],
     data = selected_data)
  
}


# Regroupons les modèle (male) dans un seul objet pour ensuite les comparer

modele_male <- list(bill_length = model("bill_length_mm","male"),
               bill_depth = model("bill_depth_mm","male"),
               flipper_length = model("flipper_length_mm","male"))


# ================= Comparaison des modèles : male =================

lapply(X = modele_male,
       FUN = summary) -> extract_modele_male


print(extract_modele_male)


sapply(X = seq_along(extract_modele_male),
       FUN = function(x) round(100*extract_modele_male[[x]]$r.squared,2)) -> R.Squared_male

setNames(object = R.Squared_male,
         nm = paste0("model",1:3)) -> R.Squared_male

print(x = R.Squared_male) 

# le modéle longueur nageoire et le poids est toujours meilleur pour les males

# Regroupons les modèle (female) dans un seul objet pour ensuite les comparer

modele_female <- list(bill_length = model("bill_length_mm","female"),
                    bill_depth = model("bill_depth_mm","female"),
                    flipper_length = model("flipper_length_mm","female"))


# ================= Comparaison des modèles : female  =================

lapply(X = modele_female,
       FUN = summary) -> extract_modele_female

print(extract_modele_female)


sapply(X = seq_along(extract_modele_female),
       FUN = function(x) round(100*extract_modele_female[[x]]$r.squared,2)) -> R.Squared_female

setNames(object = R.Squared_female,
         nm = paste0("model",1:3)) -> R.Squared_female

print(x = R.Squared_female) 

# le modéle longueur nageoire et le poids toujours meilleur pour les females



# Création d'une fonction pour créer le modéle pour 
#la variable explicative avec le meilleur modéle

NuageDispersion4 <- function(dataset, var, table_name){
  # Trier les données en fonction de la variable explicative
  dataset <- dataset[order(dataset[[var]]), ]
  var <- as.name(var)
  
  # Calcul des résidus standardisés
  std_res <- rstandard(lm(body_mass_g ~ dataset[[var]], data = dataset))
  
  # Ajout des résidus standardisés au dataset
  dataset$std_residuals <- std_res
  
  # Calcul de la régression linéaire
  fit <- lm(body_mass_g ~ dataset[[var]], data = dataset)
  
  # Calcul de la régression lissée
  smooth <- loess(formula = body_mass_g ~ dataset[[var]], data = dataset)
  
  # Création du graphique
  plot(x = dataset[[var]],
       y = dataset$body_mass_g,
       pch = 21,
       bg = "red2",
       main = paste("Nuage de dispersion 'body_mass_g'","vs",var,":",table_name ),
       xlab = "Longueur des nageoires",
       ylab = "Poids du manchot",
       las = 1)
  
  # Ajout de la régression lissée
  lines(x = dataset[[var]], y = smooth$fitted, col = "blue", lty = 2, lwd = 2)
  
  # Ajout de la droite de régression linéaire
  abline(fit, col = "purple3", lty = 1, lwd = 3)
  
  # Ajout des intervalles de prédiction
  pred <- predict(fit, interval = "prediction")
  lines(x = dataset[[var]], y = pred[, "lwr"], col = "orange2", lty = 4, lwd = 3)
  lines(x = dataset[[var]], y = pred[, "upr"], col = "orange2", lty = 4, lwd = 3)
  
  # Ajout des annotations
  text(x = min(dataset[[var]]) + 7, y = max(dataset$body_mass_g) +1,
       labels = paste("R-Squared =", round(summary(fit)$r.squared * 100, 2), "%"))
  text(x = min(dataset[[var]]) +21, y = max(dataset$body_mass_g) - 200,
       labels = paste("body_mass_g=", round(coef(fit)[1], 2),
                      ifelse(sign(coef(fit)[2]), "+", "-"),
                      round(abs(coef(fit)[2]), 2), " *",var))
}
  
# appel de la fonction
  
par(mfrow = c(1,2))
NuageDispersion4(data_male,"flipper_length_mm","male")
NuageDispersion4(data_female,"flipper_length_mm","female")
par(mfrow = c(1,1))

# ================= Etape 3 : Espèces =========================#

# ================= Préparation des données ===================#

# Création du dataset  pour l'espèce Adelie
Adelie <- subset(dataset2, species == "Adelie", 
                 select = c(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm))


# Création du dataset  pour l'espèce Gentoo
Gentoo <- subset(dataset2, species == "Gentoo",
                 select = c(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm))


# Création du dataset  pour l'espèce Chinstrap
Chinstrap <- subset(dataset2, species == "Chinstrap",
                    select = c(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm))

# ================= Analyse exploratoire ====================#

# Visualisons la matrice des corrélations

pairs(Adelie,
      panel = panel.smooth,
      pch = 20,
      cex = 1.3,
      col = "blue",
      lwd = 2,
      main = "Matrice des nuages de dispersion - Adelie",
      las = 1)

pairs(Gentoo,
      panel = panel.smooth,
      pch = 20,
      cex = 1.3,
      col = "blue",
      lwd = 2,
      main = "Matrice des nuages de dispersion - Gentoo",
      las = 1)

pairs(Chinstrap,
      panel = panel.smooth,
      pch = 20,
      cex = 1.3,
      col = "blue",
      lwd = 2,
      main = "Matrice des nuages de dispersion - Chinstrap",
      las = 1)

# Création de la fonction pour une visualisation de la distribution des variables en fonction de l'espéce

distribution3 <- function(data, var){
  
  dataset_name <- deparse(substitute(data))
  
  hist(data[[var]],
       col ="grey",
       probability = TRUE,
       main = paste("Distribution",var, dataset_name, sep= " - "),
       xlab = var,
       las = 1)
  
  
  lines(density(data[[var]]),
        col = "blue",
        lwd = 2)
}

par(mfrow = c(3,4))

distribution3(Adelie, "body_mass_g") 
distribution3(Adelie, "bill_length_mm")
distribution3(Adelie, "bill_depth_mm")
distribution3(Adelie, "flipper_length_mm")
distribution3(Chinstrap, "body_mass_g")
distribution3(Chinstrap, "bill_length_mm")
distribution3(Chinstrap, "bill_depth_mm")
distribution3(Chinstrap, "flipper_length_mm")
distribution3(Gentoo, "body_mass_g")
distribution3(Gentoo, "bill_length_mm")
distribution3(Gentoo, "bill_depth_mm")
distribution3(Gentoo, "flipper_length_mm")


# ================= Modélisation =================

# création de la fonction analyse pour réaliser la modélisation en fonction des varibales et des espèces

analyse <- function(data, var){
  
  var <- as.name(var)
  
  model <- lm(formula = formula(paste("body_mass_g", "~", var)), data = data)
  
  return(model)
  
}

# Regroupons les modèle dans un seul objet pour ensuite les comparer


models = list(Model1 = analyse(Adelie, "bill_length_mm"),
              Model2 = analyse(Adelie, "bill_depth_mm"),
              Model3 = analyse(Adelie, "flipper_length_mm"),
              Model4 = analyse(Chinstrap, "bill_length_mm"),
              Model5 = analyse(Chinstrap, "bill_depth_mm"),
              Model6 = analyse(Chinstrap, "flipper_length_mm"),
              Model7 = analyse(Gentoo, "bill_length_mm"),
              Model8 = analyse(Gentoo, "bill_depth_mm"),
              Model9 = analyse(Gentoo, "flipper_length_mm"))

# ================= Comparaison des modèles =================

lapply(X = models,
       FUN = summary) -> extractmodels

print(extractmodels)


sapply(X = seq_along(extractmodels),
       FUN = function(x) round(100*extractmodels[[x]]$r.squared,2)) -> R.Squared

setNames(object = R.Squared,
         nm = paste0("Model",1:9)) -> R.Squared

print(x = R.Squared) # meilleur explicative prof_crete en fonction de l'espèce

# Pour l'espèce Adelie la profondeur de la crête a un coefficient de détermination
#plus élevé R = 33.66, pour l'espèce Chinstrap la longueur des nageoires
# a un coefficient de détermination de 41.16 et pour l'espèce Gentoo
#la profondeur de la crête a un coefficient de détermination
#plus élevé R = 52

# Représentation des nuages de dispersion et de la régression lissée

# Création de la fonction pour la réalisation des nuages de dispersion

NuageDispersion3 <- function(dataset, var){
  plot(dataset[[var]], dataset$body_mass_g, 
       pch = 21, col = "black", bg = "purple",
       xlab = ifelse(var == "long_crete",
                     "Longueur de la crête supérieure du bec",
                     ifelse(var == "prof_crete",
                            "Profondeur de la crête supérieure du bec",
                            "Longueur des nageoires")),
       ylab = "Poids du manchot",
       main = paste0("Nuage de dispersion : body_mass_g versus ", var, " (", deparse(substitute(dataset)), ")"))
  
  # On ordonne les données de la variable explicative
  # avant de tracer la droite de régression lissée
  sorted_indices <- order(dataset[[var]])
  sorted_var <- dataset[[var]][sorted_indices]
  sorted_poids <- dataset$body_mass_g[sorted_indices]
  
  loess_fit <- loess(sorted_poids ~ sorted_var)
  loess_pred <- predict(loess_fit, data.frame(x = sorted_var))
  lines(sorted_var, loess_pred, col = "red", lwd = 1)
  
  # Ajout de la courbe de régression linéaire
  lm_fit <- lm(sorted_poids ~ sorted_var)
  abline(lm_fit, col = "green1")
  
  legend(x = "topleft", 
         legend = c("Régression lissée", "droite des moindres carrés"), 
         col = c("red","green1"), 
         lty = c(1,1),
         bty = "n",
         lwd = c(1,1), 
         cex = 0.4
  )
}

# Appel de la fonction Nuage de Dispersion 
par(mfrow = c(2,2))

NuageDispersion3(Adelie, "bill_length_mm")
NuageDispersion3(Adelie, "bill_depth_mm")
NuageDispersion3(Adelie, "flipper_length_mm")
NuageDispersion3(Chinstrap, "bill_length_mm")

par(mfrow = c(2,2))
NuageDispersion3(Chinstrap, "bill_depth_mm")
NuageDispersion3(Chinstrap, "flipper_length_mm")

NuageDispersion3(Gentoo, "bill_length_mm")
NuageDispersion3(Gentoo, "bill_depth_mm")

par(mfrow = c(2,2))
NuageDispersion3(Gentoo, "flipper_length_mm")


par(mfrow = c(1,1))


# Nuages de dispersion, regression lissée et droite des moindres carrés des modéle le plus
# pertinent pour chaque éspécé

# Création de la fonction pour une adaption avec l'échelle

NuageDispersion5 <- function(dataset, var, table_name){
  # Trier les données en fonction de la variable explicative
  dataset <- dataset[order(dataset[[var]]), ]
  var <- as.name(var)
  
  # Calcul des résidus standardisés
  std_res <- rstandard(lm(body_mass_g ~ dataset[[var]], data = dataset))
  
  # Ajout des résidus standardisés au dataset
  dataset$std_residuals <- std_res
  
  # Calcul de la régression linéaire
  fit <- lm(body_mass_g ~ dataset[[var]], data = dataset)
  
  # Calcul de la régression lissée
  smooth <- loess(formula = body_mass_g ~ dataset[[var]], data = dataset)
  
  # Création du graphique
  plot(x = dataset[[var]],
       y = dataset$body_mass_g,
       pch = 21,
       bg = "red2",
       main = paste("Nuage de dispersion 'body_mass_g'","vs",var,":",table_name ),
       xlab = "profondeur de la crête suprieur du bec",
       ylab = "Poids du manchot",
       las = 1)
  
  # Ajout de la régression lissée
  lines(x = dataset[[var]], y = smooth$fitted, col = "blue", lty = 2, lwd = 2)
  
  # Ajout de la droite de régression linéaire
  abline(fit, col = "purple3", lty = 1, lwd = 3)
  
  # Ajout des intervalles de prédiction
  pred <- predict(fit, interval = "prediction")
  lines(x = dataset[[var]], y = pred[, "lwr"], col = "orange2", lty = 4, lwd = 3)
  lines(x = dataset[[var]], y = pred[, "upr"], col = "orange2", lty = 4, lwd = 3)
  
  # Ajout des annotations
  text(x = min(dataset[[var]]) + 1, y = max(dataset$body_mass_g) +1,
       labels = paste("R-Squared =", round(summary(fit)$r.squared * 100, 2), "%"))
  text(x = min(dataset[[var]]) +1.5, y = max(dataset$body_mass_g) - 200,
       labels = paste("body_mass_g=", round(coef(fit)[1], 2),
                      ifelse(sign(coef(fit)[2]), "+", "-"),
                      round(abs(coef(fit)[2]), 2), " *",var))
}


NuageDispersion4(Chinstrap,"flipper_length_mm","Chinstrap")
NuageDispersion5(Gentoo,"bill_depth_mm","Gentoo")
NuageDispersion5(Adelie,"bill_depth_mm","Adelie")




