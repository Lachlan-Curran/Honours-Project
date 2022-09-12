rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
#Import Enzyme Data 
Enzyme <- read.csv("Raw_Data/Enzyme Assay Data.csv")
Enzyme$Transect <- factor(Enzyme$Transect, levels=c("Forest", "Forest_Edge_Interior", "Forest_Edge_Exterior", "Pioneer_Near", "Pioneer_Far", "Grass_Near", "Grass_Far", "UMNR"))
#Look at spread of data per transect with boxplot 
Phosphotase <- ggplot(data = Enzyme, aes(Transect, Phopspotase..mg.Kg.FW., fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)
BG <- ggplot(data = Enzyme, aes(Transect, BG..mg.Kg.FW., fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)
NAG <- ggplot(data = Enzyme, aes(Transect, NAG..mg.Kg.FW., fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)
pdf(file = "outputs/Enzymes.pdf")
par(2,2)
plot(Phosphotase)
plot(BG)
plot(NAG)
dev.off()


