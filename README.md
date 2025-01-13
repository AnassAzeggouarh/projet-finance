---
title: "Projet technique de programmation"
author: "Anass Azeggouarh-Wallen et Mamadou Giuliano"
date: "2025-01-09"
---

# Construction d’un portefeuille optimisé en R 


## 1. Présentation du sujet
### 1.1 Choix du sujet et problématique

Nous avons choisi ce sujet, car nous nous intéressons à la finance, et plus particulièrement à la finance quantitative. L'objectif est de comprendre comment optimiser les rendements tout en minimisant les risques liés à l'allocation des actifs, en appliquant des concepts d'optimisation quadratique et en utilisant le critère de moyenne/variance développé par Markowitz, que nous avons un peu abordé dans le cours de comportements et incitations avec monsieur Donze.

### 1.2 Choix des variables à étudier

Nous avons choisi Apple, Walmart, Meta et Nvidia pour cette étude, car ces entreprises reflètent des secteurs variés et issues du S&P 500. Apple, avec son innovation constante dans la tech. Walmart, leader de la grande distribution américaine, offre une stabilité liée à la consommation quotidienne. Meta, qui domine les réseaux sociaux et la publicité en ligne, incarne la croissance rapide du numérique. Enfin, Nvidia, acteur clé des semi-conducteurs et de l’intelligence artificielle, illustre le potentiel des technologies d’avenir. 
Apple se classe au deuxième rang mondial en termes de capitalisation boursière, juste derrière Nvidia. 
En étudiant ces entreprises sur la période 2024-2025, nous souhaitons mieux comprendre comment ces différents secteurs interagissent et se comportent face aux opportunités et aux risques. Cette période est également marquée par une forte hausse de 24/27 % du S&P 500 en 2025.


## 2. Le modèle

### 2.1 Librairies et importation des données

Dans un premier temps, nous téléchargeons les librairies nécessaires et importons les données directement depuis Yahoo Finance pour constituer une base contenant les prix ajustés des actifs. Ensuite, nous renommons les colonnes afin d'utiliser les noms réels des entreprises, plutôt que les tickers Yahoo Finance.

```{r, message=FALSE}
library(quantmod)
library(quadprog)

#Télécharger les données des actifs via une commande qui importe tout de yahoo finance

tickers <- c("AAPL", "WMT", "META", "NVDA")
getSymbols(tickers, src = "yahoo", from = "2024-01-01", to = "2025-01-01")

#Extraction des prix ajustés et on les stock dans une base "prices"

prices <- merge(
  AAPL[, "AAPL.Adjusted"],
  WMT[, "WMT.Adjusted"],
  META[, "META.Adjusted"],
  NVDA[, "NVDA.Adjusted"]
)

#on renomme les colonnes avec des noms respectifs pour que ça soit plus clair

colnames(prices) <- c("apple", "walmart", "meta", "nvidia")
```
### 2.2 La Base 100
Nous utilisons la base 100 pour comparer facilement l'évolution des actifs sur la même échelle. Cela permet de ramener toutes les entreprises à un point de départ commun (100) et de visualiser leurs performances relatives dans le temps, peu importe leur prix initial.
Cela va permettre de voir quel actif a le mieux performé en 2024 du point de vue de la base 100.
Les lignes Apple_100, Walmart_100, Meta_100 et Nvidia_100 normalisent les prix des actions respectives à une base 100, en divisant chaque valeur par le prix initial (première ligne de la colonne correspondante) et en multipliant par 100, ce qui permet ainsi de comparer leur évolution relative. Ensuite, le graphique représente l'évolution des prix normalisés pour chaque action avec des lignes colorées, et la légende associée identifie chaque courbe.
```{r}
Apple_100 <- prices[, "apple"] / as.vector(prices[1, "apple"]) * 100
Walmart_100 <- prices[, "walmart"] / as.vector(prices[1, "walmart"]) * 100
Meta_100 <- prices[, "meta"] / as.vector(prices[1, "meta"]) * 100
Nvidia_100 <- prices[, "nvidia"] / as.vector(prices[1, "nvidia"]) * 100
 
plot(
  Apple_100, type = "l", col = "blue", lwd = 2,
  xlab = "Date", ylab = "Base 100", main = "Évolution des actions (Base 100)"
)
lines(Walmart_100, col = "purple", lwd = 2)
lines(Meta_100, col = "cyan", lwd = 2)
lines(Nvidia_100, col = "yellow", lwd = 2)

addLegend("topleft",on=1,
          legend.names=c("Apple","Walmart",'Meta','Nvidia'),
          col=c("blue","purple","cyan","yellow"),
          lty = 1:1, cex=0.8)
```
On voit que NVIDIA est l'actif le plus performant en 2024 du point de vue sa base 100 ce qui n'est pas surprenant. 
En 2024, NVIDIA a réalisé une performance exceptionnelle, enregistrant des résultats financiers records, portés par une demande mondiale croissante en intelligence artificielle. L'entreprise s'est également hissée à la deuxième place mondiale en termes de capitalisation boursière, consolidant ainsi sa position de leader dans le secteur des semi-conducteurs.

### 2.3 Calcul des rendements et de la volatilité
Nous allons maintenant calculer les rendements journaliers (la moyenne des rendements) des actions ainsi que leur volatilité (écart-type des rendements) afin d'analyser statistiquement le comportement de ces actifs en 2024. Ensuite, nous représenterons graphiquement les rendements pour mieux visualiser leur évolution au cours de l'année.
Le code calcule les rendements des actions (`returns`) en utilisant la méthode des rendements discrets via la fonction `ROC` et supprime les valeurs manquantes avec `na.omit`, avant de renommer les colonnes pour chaque action. Le graphique affiche l'évolution des rendements des actions dans le temps avec des lignes colorées, et les légendes associés.

```{r}
returns <- na.omit(ROC(prices, type = "discrete"))
colnames(returns) <- c("apple", "walmart", "meta", "nvidia")
summary(returns)

plot(
  returns[, "apple"], type = "l", col = "blue", ylim = range(returns),
  xlab = "Temps", ylab = "Rendements", main = "Rendements des actions"
)
lines(returns[, "walmart"], col = "purple")
lines(returns[, "meta"], col = "cyan")
lines(returns[, "nvidia"], col = "yellow")

addLegend("topleft",on=1,
          legend.names=c("Apple","Walmart",'Meta','Nvidia'),
          col=c("blue","purple","cyan","yellow"),
          lty = 1:1, cex=0.8)
```
On observe à travers le graphique que Nvidia est l’actif le plus volatile


### 2.4 Ratio de sharpe
Nous allons maintenant calculer la moyenne des rendements de chaque actif ainsi que leur volatilité. Ces calculs nous permettront de déterminer le ratio de Sharpe (moyenne/écart-type), qui évalue le rapport entre le risque et le rendement de chaque actif. Ce ratio indique quel actif offre la meilleure performance compte tenu du risque.

```{r}
mean_returns <- colMeans(returns)  
std_returns <- apply(returns, 2, sd)  

cat("Moyennes annualisées des rendements :\n")
print(mean_returns)
cat("Écarts-types annualisés des rendements :\n")
print(std_returns)

ratio_sharpe <- mean_returns/std_returns
ratio_sharpe
```
Les résultats montrent que Walmart possède le meilleur ratio risque/rendement. En effet, bien que ses rendements soient moins élevés que ceux de Nvidia, Walmart a affiché l'une des plus faibles volatilités en 2024. Cela explique son ratio élevé, car le risque joue un rôle tout aussi important que le rendement dans le calcul du ratio de Sharpe.


## 3. Programme d'optimisation

### 3.1 Histoire du modèle d'optimisation mean/variance
Le modèle d'optimisation moyenne-variance, créé par Harry Markowitz en 1952, est à la base de la théorie moderne du portefeuille. Il propose de construire des portefeuilles optimaux en maximisant le rendement attendu pour un risque donné, ou en minimisant le risque pour un rendement souhaité. Ce modèle a introduit l'idée clé de la diversification basée sur les corrélations entre actifs, permettant de réduire le risque global. Cette approche quantitative a révolutionné la gestion de portefeuille et valu à Markowitz le prix Nobel d'économie en 1990.


### 3.2 Hypothèses du modèle 
- Rendements et risque : Les rendements des actifs sont supposés normaux, et le risque est mesuré par l'écart-type des rendements.

- Diversification : Les actifs ne sont pas parfaitement corrélés, ce qui permet de réduire le risque global grâce à la diversification.

- Optimisation : Le modèle minimise la variance du portefeuille sous contraintes : la somme des poids doit être égale à 1 et les poids doivent être positifs (pas de ventes à découvert).

- Frontière efficiente : Le portefeuille calculé maximise l’efficacité en termes de rapport rendement/risque.

Ce modèle se base sur des données historiques pour estimer les rendements futurs et les interactions entre actifs.


### 3.3 Optimisation

Dans le programme d'optimisation, l'objectif était de minimiser le risque, mesuré par la variance du portefeuille, tout en respectant un rendement cible qui est la moyenne des rendements passés de tous les actifs. 
Pour cela, nous avons d'abord fixé un rendement cible qui est la moyenne des rendements moyens du vecteur des rendements des actifs, ensuite la matrice de variance-covariance des rendements des actifs, qui mesure les interactions entre eux. 
Ensuite, nous avons défini un vecteur objectif nul, car l'objectif est uniquement de minimiser le risque, ainsi qu'une matrice de contraintes garantissant que la somme des poids des actifs est égale à 1, que les poids sont positifs (pas de ventes à découvert), et que le rendement du portefeuille atteint ou dépasse le seuil cible. 
L'optimisation quadratique a été réalisée à l'aide de la fonction `solve.QP`, qui minimise la variance du portefeuille sous ces contraintes. 
Enfin, les poids optimaux des actifs ont été calculés, permettant de déterminer les rendements attendus et le risque total du portefeuille optimisé.

```{r}
rendement_ciblé <- mean(mean_returns)

cov_matrix <- cov(returns)
cor_matrix <- cor(returns)

cat("Matrice de variance-covariance :\n")
print(cov_matrix)
cat("Matrice de corrélation :\n")
print(cor_matrix)

n_assets <- ncol(returns)
Dmat <- cov_matrix  
dvec <- rep(0, n_assets)  
Amat <- cbind(rep(1, n_assets), mean_returns , diag(n_assets))  
bvec <- c(1,rendement_ciblé, rep(0, n_assets))  
opt <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)  

weights_min_var <- opt$solution # Poids du portefeuille à variance minimale
round(weights_min_var * 100, 2)

portfolio_return_min_var <- sum(weights_min_var * mean_returns)
portfolio_risk_min_var <- sqrt(t(weights_min_var) %*% cov_matrix %*% weights_min_var)

cat("Portefeuille à variance minimale :\n")
cat("Rendement attendu :", portfolio_return_min_var, "\n")
cat("Risque attendu :", portfolio_risk_min_var, "\n")

barplot(
  weights_min_var * 100,
  names.arg = c("apple", "walmart", "meta", "nvidia"),
  col = rainbow(length(tickers)),
  main = "Répartition des poids (Variance Minimale)",
  ylab = "Poids (%)"
)

#ratio de sharpe du portefeuille 
sharpe_ratio <- (portfolio_return_min_var) / portfolio_risk_min_var
sharpe_ratio
```
le code de la fonction `barplot` crée un graphique en barres coloré qui montre comment les actions Apple, Walmart, Meta et Nvidia sont réparties dans un portefeuille optimisé pour minimiser le risque, en affichant leurs poids en pourcentage.
l'allocation optimale est 4.86% pour Apple, 72.55% Walmart, 4.09% pour Meta et 18.51% Nvidia. 
Le rendement attendu du portefeuille est de 0.00263478, ce qui dépasse celui de toutes les actions, à l'exception de Nvidia. Par ailleurs, le risque du portefeuille, mesuré par une volatilité de 0.01063863, est inférieur à celle de tous les actifs pris individuellement. 
De plus, le ratio de Sharpe du portefeuille, évalué à 0.2476616, est nettement supérieur à tous les ratios individuels. Cela démontre l’efficacité de l’optimisation, qui a permis d’améliorer à la fois le rendement ajusté au risque et la diversification. 

### 3.4 Comparaison des ratios de sharpe avec optimisation et sans optimisation

L’objectif est de comparer le ratio de Sharpe d’un portefeuille équipondéré avec celui d’un portefeuille optimisé. 
Pour le portefeuille équipondéré, on calcule le rendement moyen en répartissant également les investissements entre les actifs, et la volatilité en se basant uniquement sur les variances individuelles, sans considérer les corrélations. 
On cherche à mettre en évidence l’intérêt d’une optimisation par rapport à une simple répartition égale.

```{r}
sharpe_equal_weights <- mean(colMeans(returns)) / sqrt(mean(diag(cov_matrix)))
cat("Ratio de Sharpe avec poids égaux :", round(sharpe_equal_weights, 4), "\n")
```
Le ratio de Sharpe équipondéré (0.1195) est nettement inférieur à celui du portefeuille optimisé (0.2477). Cela montre que l’optimisation améliore significativement la performance ajustée au risque en ajustant les pondérations des actifs pour maximiser le rendement tout en minimisant la volatilité. 
En comparaison, la répartition égale ne profite pas pleinement des caractéristiques individuelles des actifs ni de leurs corrélations, ce qui explique son ratio de Sharpe plus faible.


### 3.5 Annualisation des résultats 

On va annualiser les résultats pour pouvoir comparer les performances des actifs et du portefeuille sur une base annuelle, ce qui est plus pratique. Pour cela, on transforme les rendements journaliers en rendements annuels en prenant en compte l’effet composé avec la formule ((1+Rendementsquotidien)^{252} - 1), et on annualise la volatilité en la multipliant par sqrt{252}, qui correspond au nombre moyen de jours de bourse par an.

Ensuite, on consrtuit un tableau récapitulatif. Ce tableau contiendra les rendements et volatilités annualisés pour chaque actif, ainsi que leurs poids optimaux dans le portefeuille. 
On y ajoutera aussi une ligne pour présenter les performances globales du portefeuille optimisé, ce qui permettra de comparer directement le portefeuille aux actifs individuels. 
Cela nous donne une vision plus claire et synthétique des performances.

```{r}
mean_returns_annualized <- (1 + mean_returns)^252 - 1  
std_returns_annualized <- std_returns * sqrt(252)  

portfolio_return_annualized <- (1 + portfolio_return_min_var)^252 - 1  # Rendement annuel du portefeuille
portfolio_risk_annualized <- portfolio_risk_min_var * sqrt(252)  # Volatilité annuelle du portefeuille
sharpe_ratio_annualized <- portfolio_return_annualized / portfolio_risk_annualized  # Ratio de Sharpe annuel


results <- data.frame(
  Actif = c(tickers, "Portefeuille"),
  Poids = c(round(weights_min_var * 100, 2), 100),
  Rendements_Annuel = c(round(mean_returns_annualized, 4), round(portfolio_return_annualized, 4)),
  Volatilité_Annuel = c(round(std_returns_annualized, 4), round(portfolio_risk_annualized, 4)),
  Ratio_Sharpe = c(round(mean_returns_annualized / std_returns_annualized, 4), round(sharpe_ratio_annualized, 4))
)

print(results)
```
L’optimisation a conduit à une forte pondération de Walmart (72,55%) en raison de sa faible volatilité (0.1770) et de son rendement élevé (0.7525), en faisant l’actif le plus défensif. Nvidia suit avec 18,50%, porté par son rendement très élevé (2.2059), malgré une volatilité plus importante (0.5251). Apple et Meta, moins performants en termes de rapport rendement/risque, occupent des poids plus modestes. Le portefeuille global atteint un rendement annuel de 94,08% avec une volatilité réduite à 16,89%, montrant l’effet bénéfique de la diversification. Avec un ratio de Sharpe exceptionnel de 5.5706, le portefeuille surpasse largement les ratios des autres actifs pris individuellement, ce qui montre l’efficacité de cette approche optimisée.


### 3.6 Frontière efficiente du portefeuille

```{r}
# Étape 4 : Calcul de la frontière efficiente
portfolio_returns <- seq(min(mean_returns), max(mean_returns), length.out = 100)  # Rendements cibles
portfolio_risks <- sapply(portfolio_returns, function(target_return) {
  weights <- solve.QP(
    Dmat = cov_matrix,
    dvec = rep(0, n_assets),
    Amat = cbind(rep(1, n_assets), mean_returns, diag(n_assets)),
    bvec = c(1, target_return, rep(0, n_assets)),
    meq = 2
  )$solution
  sqrt(t(weights) %*% cov_matrix %*% weights)  # Risque (écart-type)
})

# Calcul des coordonnées du portefeuille optimisé
risk_optimized <- portfolio_risk_min_var
return_optimized <- portfolio_return_min_var

# Étape 5 : Tracé de la frontière efficiente
plot(
  portfolio_risks, portfolio_returns, type = "l", col = "red", lwd = 2,
  xlab = "Risque (Écart-Type)", ylab = "Rendement", main = "Frontière efficiente"
)

# Ajout du portefeuille optimisé
points(risk_optimized, return_optimized, col = "blue", pch = 19, cex = 1.5)
text(risk_optimized, return_optimized, labels = "Portefeuille Optimisé", pos = 4, col = "blue")

# Ajout d'une légende
legend(
  "bottomright", legend = c("Frontière efficiente", "Portefeuille Optimisé"),
  col = c("red", "blue"), lty = 1, pch = c(NA, 19), lwd = 2
)

```

Ce graphique illustre la frontière efficiente selon la théorie de Markowitz. Tous les points situés sur la courbe rouge de la frontière efficiente sont considérés comme optimaux, car ils offrent le meilleur compromis possible entre le risque et le rendement pour un portefeuille donné. 
Ces portefeuilles maximisent soit le rendement attendu pour un niveau de risque donné, soit minimisent le risque pour un rendement cible.

Le point bleu correspond à notre portefeuille optimisé, à variance minimale, qui minimise le risque tout en restant sur la frontière.


### 3.7 Allocation par actif pour un budget de 500 euros
```{r}
allocation <- round(weights_min_var * 500, 2)
names(allocation) <- c("apple", "walmart", "meta", "nvidia")
print(allocation)
```
Ce code calcule l'allocation en dollars pour chaque action dans un portefeuille de 500 unités monétaires basé sur les poids à variance minimale. Les montants sont arrondis à deux décimales, puis associés aux noms des actions Apple, Walmart, Meta et Nvidia avant d'être affichés.
Avec un budget de 500 euros, la majorité est investie dans Walmart (362,73 €) en raison de sa faible volatilité, suivi de Nvidia (92,53 €) pour son rendement élevé. Apple (24,31 €) et Meta (20,43 €) ont des allocations plus faibles, reflétant leur profil rendement/risque.



## 4. Limites du modèle et de la présentation 

Le modèle de Markowitz est un pilier incontournable de l’optimisation de portefeuille en finance, grâce à son approche du rapport rendement/risque. 
Cependant, il présente plusieurs limites à prendre en compte. 

Il repose sur des hypothèses simplifiées, comme la normalité des rendements et la stabilité des corrélations entre actifs, alors qu’en réalité, les marchés financiers sont souvent imprévisibles, avec des comportements asymétriques et des corrélations qui évoluent rapidement, surtout en période de crise.

Une autre faiblesse majeure du modèle est qu’il utilise des données historiques pour estimer les rendements et les risques. 
Ces données, ne garantissent pas la précision des prévisions futures. En effet, les marchés changent constamment, et les performances passées ne sont pas toujours un bon indicateur des résultats futurs. 

De plus, l’optimisation peut parfois conduire à des portefeuilles peu diversifiés, car certains actifs, perçus comme très performants historiquement, sont surpondérés.

Le modèle accorde également le même poids aux rendements positifs et négatifs, ce qui ne reflète pas toujours les priorités des investisseurs, souvent plus soucieux de limiter leurs pertes que de maximiser leurs gains.

L’une des limites du modèle de Markowitz est qu’il peut favoriser la concentration des investissements sur quelques actifs, en particulier lorsqu’on cherche à maximiser le ratio de Sharpe. Cela peut conduire à l’exclusion totale de certains actifs, réduisant ainsi l'objectif de diversification.

Dans notre cas, le portefeuille choisi, représenté par le point bleu sur la frontière efficiente, n’est pas le portefeuille tangent, qui maximise le ratio de Sharpe. Nous avons délibérément évité d’utiliser le portefeuille tangent pour éviter que certains actifs ne soient complètement exclus de l’optimisation, privilégiant ainsi une meilleure diversification. Ce portefeuille reste optimal, car il minimise le risque pour un rendement donné tout en assurant une répartition plus équilibrée des actifs, mais n'est pas tangeant.

Malgré ces limites, le modèle de Markowitz a ouvert la voie à des approches plus modernes. Par exemple, le modèle de Black-Litterman intègre non seulement les données historiques, mais aussi les prévisions des investisseurs, offrant ainsi des allocations plus équilibrées et réalistes. D’autres avancées, comme les simulations Monte Carlo ou les optimisations prenant en compte les risques extrêmes, répondent aux défis des marchés actuels. 
Ces évolutions montrent que, bien que perfectible, le modèle de Markowitz reste une base solide pour comprendre et gérer les portefeuilles.

