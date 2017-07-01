# @Encode: utf-8
# @Title: Linear Discriminant Analysis
# @Description:
# - Implementation of LDA by 'lda' funciton in 'MASS' package
# - Visualization the result of lda in 2-dimensional

library(MASS)
library(kknn) # This package contain the dataset 'miete'
library(ggplot2)

# Get the data of house
data(miete)
# Only use the data with 'nmkat'(price of house) in 1 and 2 level
# and select 'nm' 'wfl' as the feature
miete <- miete[miete$nmkat %in% c("1","2"),which(names(miete) %in% c("nm", "wfl", "nmkat"))] 

#### Modeling Start ####
# The model with no 'piror'
model <- lda(as.factor(as.character(nmkat)) ~ nm + wfl, data = miete)

#### Modeling End ####

# Visualization
# Transform sample to 1-dimensional
miete$LDA1 <- t(matrix(c(as.numeric(model$scaling)), ncol = 2, nrow = 1) %*% t(as.matrix(miete[,1:2])))
# Plot in 1-dim
# Two vertical line are the center of each class
miete$Y <- rep(3, nrow(miete))
p <- ggplot(data = miete, aes(x=LDA1, y=Y))+
      geom_point(aes(color=as.factor(as.character(nmkat))), alpha=0.3, size=6)+
      geom_vline(xintercept = matrix(as.numeric(model$scaling), nrow = 1, ncol = 2) %*% 
                   matrix(model$means[1,], nrow=2, ncol=1), size=1.2)+
      geom_vline(xintercept = matrix(as.numeric(model$scaling), nrow = 1, ncol = 2) %*% 
                   matrix(model$means[2,], nrow=2, ncol=1), size=1.2)
      
p
# Plot original data
# Data of center
dataCenter <- as.data.frame(model$means)
# Add label to data
dataCenter$Label <- factor(c(1,2))
p2 <- ggplot()+
        geom_point(data = miete, aes(x= nm, y=wfl, color=as.factor(nmkat)), size=1.5, alpha=0.3)+
        geom_point(data = dataCenter, aes(x = nm, y=wfl, color=Label), size=6, alpha=1)
p2
