rm(list=ls())
library(lme4)
library(lmerTest)
library(nlme)
library(tidyverse)
library(dplyr)
library(MuMIn)
#Import Distance Data 
Distance <-read.csv("Raw_Data/Plot_Coordinates_MC.csv")

#Import AGV 
AGV <- read.csv("Cleaned Up Data/AGV_Data.csv")
#convert first column to row names and remove rows containing UMNR data - this is an outgroup 
AGV <- as.data.frame(AGV, row.names = AGV$X)
AGV <- as.data.frame(AGV[c(1:21),c(2:5)])
#Perform a PCA 
AGV_PCA <- princomp(AGV, cor = TRUE)
#Plot PCA 
plot(AGV_PCA)
biplot(AGV_PCA, cex = 0.5)
#Comp1 and Comp2 explain most od the variance, lets do a biplot and save it as a PDF 
pdf(file = "Outputs/Figures/AGV_PCA_Biplot.pdf", height = 6, width = 6)
biplot(AGV_PCA, cex = 0.5)
dev.off()
#there are two clear relationships between the 4 variables that are explained by comp.1 and comp.2: Sites with higher woody richness tend to have lower grass cover and sites with higher median stem DBH tend to have lower other herb cover. We will rename them and use them in our model 
AGV_PCA_Scores <- as.data.frame(AGV_PCA$scores[,c(1,2)])
#Rename first 2 components
colnames(AGV_PCA_Scores)[1] <- "Woody_Richness_vs_Grass_Cover"
colnames(AGV_PCA_Scores)[2] <- "DBH_vs_Other_Herb"
#Remove last two columns 
#Create new dataframe that joins togehter the AGV PCA socres and distance 
#Join by plot 
AGV_PCA_Scores$Plot <- Distance$Plot
Distance_AGV <- left_join(Distance, AGV_PCA_Scores, by = "Plot")

#Plot each component against distance from forest edge 
Woody_Grass <- ggplot(data = Distance_AGV, aes(x = Distance_From_Edge, y = Woody_Richness_vs_Grass_Cover, col = Transect, cex = 3)) + geom_point()
plot(Woody_Grass)
DBH_Herb <- Woody_Grass <- ggplot(data = Distance_AGV, aes(x = Distance_From_Edge, y =DBH_vs_Other_Herb, col = Transect, cex = 3)) + geom_point()
plot(DBH_Herb)

pdf(file = "Outputs/Figures/AGV_Distance.pdf")
par(1,2)
plot(Woody_Grass)
plot(DBH_Herb)
dev.off()

#Do the same for soil data 
#import soil data 
Soil <- read.csv("Raw_Data/Soil_Nutrients.csv")
#remove empty rows
Soil <- as.data.frame(Soil[,c(2:6)])
#Create new dataframe with soil and distance data 
Distance_Soil <- left_join(Distance, Soil, by = "Plot")
#Create boxploit of TOC by transect 
#Reorder x axis
Distance_Soil$Transect <- factor(Distance_Soil$Transect, levels=c("Forest", "Forest_Edge_Interior", "Forest_Edge_Exterior", "Pioneer_Near", "Pioneer_Far", "Grass_Near", "Grass_Far"))
TOC <-  ggplot(data = Distance_Soil, aes(x = Transect, y = TOC, fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)
plot(TOC)
#plot soil TOC against distance 
TOC_Distance <- ggplot(data = Distance_Soil, aes(x = Distance_From_Edge, y = TOC)) + geom_point(aes(col = Transect), cex = 3)  
plot(TOC_Distance)

#Repeat for Phosphorous 
#Create boxploit of TP by transect 
TP <-  ggplot(data = Distance_Soil, aes(x = Transect, y = TP, fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)
plot(TP)
#plot soil TP against distance 
TP_Distance <- ggplot(data = Distance_Soil, aes(x = Distance_From_Edge, y = TP)) + geom_point(aes(col = Transect), cex = 3) 
plot(TP_Distance)

#Repeat for N 
#Create boxploit of TN by transect 
TN <-  ggplot(data = Distance_Soil, aes(x = Transect, y = TN, fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)
plot(TN)
#plot soil TN against distance 
TN_Distance <- ggplot(data = Distance_Soil, aes(x = Distance_From_Edge, y = TN)) + geom_point(aes(col = Transect), cex = 3) 
plot(TN_Distance)

pdf(file = "Outputs/Figures/Soil_Nutrients_Distance.pdf")
par(2,2)
plot(TOC)
plot(TOC_Distance)
plot(TP)
plot(TP_Distance)
plot(TN)
plot(TN_Distance)
dev.off()

#Repeat for leaf litter nutrients 
#import litter data 
Litter <- read.csv("Raw_Data/Leaf_Litter_N_P .csv")
#Join to distance data in new dataframe 
Distance_Litter <- left_join(Distance, Litter, by = "Plot")
#Plot each litter variable against transect and distance from edge 
#Reorder x axis
Distance_Litter$Transect <- factor(Distance_Litter$Transect, levels=c("Forest", "Forest_Edge_Interior", "Forest_Edge_Exterior", "Pioneer_Near", "Pioneer_Far", "Grass_Near", "Grass_Far"))
#Litter P
Litter_P <- ggplot(data = Distance_Litter, aes(x = Transect, y = Total_P_., fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)
plot(Litter_P)
Litter_P_Distance <- ggplot(data = Distance_Litter, aes(x = Distance_From_Edge, y = Total_P_.,)) + geom_point(aes(col = Transect), cex = 3)
plot(Litter_P_Distance)
#Litter N 
Litter_N <- ggplot(data = Distance_Litter, aes(x = Transect, y = Total_N_., fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)
plot(Litter_N)
Litter_N_Distance <- ggplot(data = Distance_Litter, aes(x = Distance_From_Edge, y = Total_N_.,)) + geom_point(aes(col = Transect), cex = 3)
plot(Litter_N_Distance)
#Litter NH4
Litter_NH4 <- ggplot(data = Distance_Litter, aes(x = Transect, y = Total_NH4.N_mg.kg, fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)
plot(Litter_NH4)
Litter_NH4_Distance <- ggplot(data = Distance_Litter, aes(x = Distance_From_Edge, y = Total_NH4.N_mg.kg)) + geom_point(aes(col = Transect), cex =3)
plot(Litter_NH4_Distance)
#Litter NO3
Litter_NO3 <- ggplot(data = Distance_Litter, aes(x = Transect, y = Total.NO3.N_mg.kg, fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)
plot(Litter_NO3)
Litter_NO3_Distance <- ggplot(data = Distance_Litter, aes(x = Distance_From_Edge, y = Total.NO3.N_mg.kg)) + geom_point(aes(col = Transect), cex =3)
plot(Litter_NO3_Distance)

pdf(file = "Outputs/Figures/Litter_Nutrients_Distance.pdf")
par(2,2)
plot(Litter_P)
plot(Litter_P_Distance)
plot(Litter_N)
plot(Litter_N_Distance)
plot(Litter_NH4)
plot(Litter_NH4_Distance)
plot(Litter_NO3)
plot(Litter_NO3_Distance)
dev.off()

