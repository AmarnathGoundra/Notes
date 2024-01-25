setwd("D:/Batch37/CSE7302c/Day04")

# Chemical analysis of Wine
# These data are the results of a chemical analysis of wines grown 
# in the same region in Italy but derived from three different cultivars. 
# The analysis determined the quantities of 13 constituents found in each 
# of the three types of wines. 

WineData <- read.csv("wine.csv")
str(WineData)

# PCA analysis is done only on the predictors
wine.predictors <- WineData[,-1]

# Since the predictors are of completely different magnitude, 
# we need scale them before the analysis.
scaled.Predictors <- scale(wine.predictors)
scaled.Predictors
# compute PCs
pca.out = princomp(scaled.Predictors)
# princomp(wine.predictors, cor=TRUE) would
# automatically scale
names(pca.out)
summary(pca.out)
pca.out$loadings
plot(pca.out)

#If we choose 80% explanatory power for variances, we need only first 5 components of PC.

compressed_features = pca.out$scores[,1:5] 
compressed_features

library(nnet)
multout.pca <- multinom(WineData$WineClass ~ compressed_features)
summary(multout.pca)
#Gives us AIC value of 24

multout.full <- multinom(WineClass ~ scaled.Predictors, data=WineData)
summary(multout.full)  #Gives us AIC of 56

#Visualizing the spread in the dataset using only the first 2 components.
#
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(pca.out, obs.scale = 1, var.scale = 1, 
              groups = WineData$WineClass, ellipse = TRUE, circle = TRUE) 
  g  + scale_color_discrete(name = '')
  