# Import the libraries
library(htmlTable)
library(tidyverse)
library(ggplot2)
library(rvest)
library(naniar)
library(readr)
library(rpart)
library(MASS)
library(klaR)
library(car)

# Import the Wine dataset from library "rattle"
data(wine, package = 'rattle')
attach(wine)

# Check the Structure of the dataset
str(wine)

# Plot the visuals
scatterplotMatrix(wine[2:8])

# Build the Linear Discriminatory Analysis (LDA) model
wine.lda <-  lda(Type~., data = wine)
wine.lda

# Visualize the Wine data using Histogram
wine.lda.values <- predict(wine.lda)
ldahist(wine.lda.values$x[,1],g=Type)

# Visualize the Wine data using Histogram using Second Discriminant Function
ldahist(wine.lda.values$x[,2],g=Type)

# Scatter Plot 
plot(wine.lda.values$x[,1], wine.lda.values$x[,2])
text(wine.lda.values$x[,1], wine.lda.values$x[,2],Type, col = 'orange')


# Visualize it with GGPlot
newdata <-  data.frame(type =wine[,1], lda = wine.lda.values$x)

ggplot(newdata) + geom_point(aes(lda.LD1,lda.LD2, colour = type), data = newdata, size = 2.8)


# Check the Accuracy
library(caret)
lda_pred <-  train(Type~.,methods='lda', data = wine)
confusionMatrix(wine$Type, predict(lda_pred, wine))
