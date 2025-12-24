# Analyse et Modélisation Statistique : Population de Manchots 🐧

Ce projet, réalisé dans le cadre de la **SAE 2.03** (Science des Données), porte sur l'analyse exploratoire et la modélisation par régression linéaire d'une population de 344 manchots situés dans les archipels Palmer en Antarctique.

## 📋 Présentation du projet
L'objectif est d'étudier l'association entre le **poids** des manchots et plusieurs variables physiques afin d'aider un ornithologue à comprendre les facteurs influençant la masse corporelle des individus.

### Variables étudiées
Le jeu de données comprend les informations suivantes :
* **Espèce** (Adélie, Chinstrap, Gentoo)
* **Sexe** (Mâle, Femelle)
* **Longueur des nageoires** (en mm)
* **Dimensions du bec** (Longueur et profondeur de la crête supérieure en mm)
* **Masse corporelle** (en g)

---

## 📊 Méthodologie et Résultats

### 1. Analyse Exploratoire
L'analyse initiale via une matrice des nuages de dispersion a révélé une association linéaire positive entre le poids et la longueur du bec ainsi que la longueur des nageoires. En revanche, une absence d'association linéaire globale a été constatée avec la profondeur de la crête, suggérant la présence de variables de confusion comme l'espèce ou le sexe.



### 2. Modélisation Globale
Le modèle de régression linéaire simple le plus performant au niveau global repose sur la **longueur des nageoires** :
* **Coefficient de détermination ($R^2$)** : 76,21 %.
* **Interprétation** : Une augmentation de 1 mm de la longueur des nageoires entraîne une hausse moyenne de **50,15 grammes** du poids.
* **Équation** : $Poids = -5872,09 + 50,15 \times Longueur\_Nageoires$.



### 3. Analyse Segmentée (Espèce & Sexe)
L'étude démontre que l'espèce est la variable qui détermine le mieux la variation du poids :
* **Par Sexe** : La longueur de la nageoire reste la variable la plus impactante pour les mâles ($R^2 = 74,91\%$) comme pour les femelles ($R^2 = 78,29\%$).
* **Par Espèce** : 
    * Pour les **Gentoos**, la **profondeur du bec** est le facteur prédominant ($R^2 = 52,27\%$).
    * Pour les **Adélies**, la profondeur du bec explique 33,66 % de la variation du poids.
    * Pour les **Chinstraps**, la longueur des nageoires demeure la variable la plus importante ($R^2 = 41,16\%$).

---

## 🛠️ Outils utilisés
* **Langage R** : Traitement de données et modélisation.
* **Matrice de dispersion & Régression lissée** : Visualisation des corrélations.
* **Méthode des moindres carrés** : Calcul des droites de régression.

---
*Année universitaire 2023-2024 - IUT Grand Ouest Normandie (Campus de Lisieux)*