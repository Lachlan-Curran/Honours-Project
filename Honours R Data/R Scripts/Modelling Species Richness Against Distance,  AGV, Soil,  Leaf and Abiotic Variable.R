rm(list=ls())
library(MuMIn)
library(lme4)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DHARMa)
library(lmerTest)
library(vegan)

#Source script for fitting mixed effects models to plot 
source("Functions/Fitting Mixed Effects Models Fucntion.R")

#Import community data at sample level
Community <- read.csv("Cleaned Up Data/Fungi_by_Sample.csv")
#Rename column X to sample
names(Community)[1] <- "Sample"
#Remove UMNR data
Community <- as.data.frame(Community[-c(37:42),])
#Add a column of species richness using function specnumber from package vegan
Community$Species_Richness <- specnumber(Community[,c(2:5138)])
#Add  Transect Column 
Transect <- c(rep("Forest", 6), rep("Forest_Edge_Interior", 6), rep("Forest_Edge_Exterior", 6), rep("Pioneer_Near", 6), rep("Pioneer_Far", 6), rep("Grass_Near", 6), rep("Grass_Far", 5))
Community$Transect <- Transect
#Assign row names as transects 
Community <- as.data.frame(Community, row.names = Community$Transect)
#Add column containing distance of each transect from forest edge 

#Model Species richness vs Transect/Distance From Edge 

#Import Distance data
Distance <- read.csv("Raw_Data/Distance_edge.csv")
#Remove NA column 
Distance$X <- NULL
#Assign distance to community dataframe 
Community$Distance <- c(rep(-47,6), rep(-3,6), rep(3,6), rep(18,6), rep(83,6), rep(23,6), rep(152,5))
#create box plot of species richness per transect 
Community$Transect <- factor(Community$Transect, levels=c("Forest", "Forest_Edge_Interior", "Forest_Edge_Exterior", "Pioneer_Near", "Pioneer_Far", "Grass_Near", "Grass_Far"))
#Define colours 
my_colours <- c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2",  "#CC6677","#AA4499")
#Construct ggplot
Species_Richness <- ggplot(data = Community, aes(x = Transect, y = Species_Richness, fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect") + theme(axis.text.x = element_text(angle = 45, hjust=1)) + xlab("Transect") + ylab("Species Richness")
#Plot
Species_Richness + scale_colour_manual(values = my_colours) + scale_fill_manual(values=my_colours)

pdf(file = "Outputs/Figures/Species_Richness.pdf")
par(1,1)
plot(Species_Richness)
dev.off()

#Plot species richness against distance from forest edge
#Define Model
#Create Mixed effects models for species richness vs distance 
#define Model
SR_vs_Distance_Model <- lmer(Species_Richness~Distance + (1|Transect), data = Community)
#Summary
summary(SR_vs_Distance_Model)
#Check Residuals
plot(simulateResiduals(SR_vs_Distance_Model))
#Check the constrained variance contirbuted by distance to the model
r.squaredGLMM(SR_vs_Distance_Model)

#Create new dataframe that stores Distance in 100 even units from smallest to lowest values 
SR_Distance_Predict<-data.frame(int = 1, x = seq_func(Community$Distance))
#Create a new predictive model from this data and our previous model
Distance_Predict<-lmer.predict(mod = SR_vs_Distance_Model, newdat=SR_Distance_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against distance
with(Community, plot(Species_Richness ~ Distance, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "Distance From Forest Edge (m)"))
#Change margins to fit legend 
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community$Distance), pred = Distance_Predict$y, 
             upper = Distance_Predict$phi, lower=Distance_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
             col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0))



#Model Species Richness Vs AGV
#Import community data at plot level - this is the scale for which we have data 
Community_by_Plot <- read.csv("Cleaned Up Data/Community_Fungi_By_Plot.csv")
#Assign row names, remove first column and UMNR data 
Community_by_Plot <- as.data.frame(Community_by_Plot[c(1:21),])
Community_by_Plot <- as.data.frame(Community_by_Plot[,c(2:5138)], row.names = Community_by_Plot$X)
#Calculate species richness for this scale 
Community_by_Plot$Species_Richness <- specnumber(Community_by_Plot[,c(1:5137)])
#Add Transect Column 
Transect <- c(rep("Forest", 3), rep("Forest_Edge_Interior", 3), rep("Forest_Edge_Exterior", 3), rep("Pioneer_Near", 3), rep("Pioneer_Far", 3), rep("Grass_Near", 3), rep("Grass_Far", 3))
Community_by_Plot$Transect <- Transect 

#Import AGV 
AGV <- read.csv("Cleaned Up Data/AGV_Data.csv")
#convert first column to row names 
AGV <- as.data.frame(AGV, row.names = AGV$X)
AGV <- as.data.frame(AGV[,c(2,3,4)])


#We can capture most of the variance within this group by using the firs principle component of a PCA
#Perform PCA, check this is appropriate 
AGV_PCA <- princomp(AGV, cor = TRUE)
#Plot PCA 
plot(AGV_PCA)
biplot(AGV_PCA, cex = 0.5)
pdf(file = "Outputs/Figures/AGV_PCA_Biplot.pdf", height = 6, width = 6)
biplot(AGV_PCA, cex = 0.5)
dev.off()
#Comp1 explains most of the variance, lets use this as our explanatory variable
#extract the scores
AGV_PCA_Scores <- as.data.frame(AGV_PCA$scores)
#rename first column 
colnames(AGV_PCA_Scores) <- c("AGV_COMP1", "AGV_COMP2", "AGV_COMP3") 
#Join AGV_COMP1 to Community_by_Plot data frame 
Community_by_Plot$AGV_COMP1 <- AGV_PCA_Scores$AGV_COMP1

#Construct Model for Species richness vs AGV
SR_vs_AGV <- lmer(Species_Richness ~ AGV_COMP1 + (1|Transect), data = Community_by_Plot)
#Summary
summary(SR_vs_AGV)
#Check Residuals
plot(simulateResiduals(SR_vs_AGV))
#Check the constrained variance contirbuted by distance to the model
r.squaredGLMM(SR_vs_AGV)

#Create new dataframe that stores AGV_COMP1 in 100 even units from smallest to lowest values 
SR_AGV_Predict<-data.frame(int = 1, x = seq_func(Community_by_Plot$AGV_COMP1))
#Create a new predictive model from this data and our previous model
AGV_Predict<-lmer.predict(mod = SR_vs_AGV, newdat=SR_AGV_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against distance
with(Community_by_Plot, plot(Species_Richness ~ AGV_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "AGV Principle Component 1"))
#Change margins to fit legend 
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community_by_Plot$AGV_COMP1), pred = AGV_Predict$y, 
             upper = AGV_Predict$phi, lower=AGV_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0))

#Repeat Process for Soil Nutrients
#Import Soil 
Soil <- read.csv("Cleaned Up Data/Soil_Nutrients_MC.csv")
#assign rownames
rownames(Soil) <- rownames(Community_by_Plot)
#Perform PCA 
Soil_PCA <- princomp(Soil)
#Check the components
plot(Soil_PCA)
biplot(Soil_PCA, cex = 0.5)
#Save biplot
pdf(file = "Outputs/Figures/AGV_PCA_Biplot.pdf", height = 6, width = 6)
biplot(Soil_PCA, cex = 0.5)
dev.off()
#Comp1 explains most of the variance, lets use this as our explanatory variable
#extract the scores
Soil_PCA_Scores <- as.data.frame(Soil_PCA$scores)
#rename first column 
colnames(Soil_PCA_Scores) <- c("SOIL_COMP1", "SOIL_COMP2", "SOIL_COMP3") 
#Join AGV_COMP1 to Community_by_Plot data frame 
Community_by_Plot$SOIL_COMP1 <- Soil_PCA_Scores$SOIL_COMP1

#Construct Model for Species richness vs SOIL
SR_vs_SOIL <- lmer(Species_Richness ~ AGV_COMP1 + (1|Transect), data = Community_by_Plot)
#Summary
summary(SR_vs_SOIL)
#Check Residuals
plot(simulateResiduals(SR_vs_SOIL))
#Check the proportion of unique variance explained by this variable 
r.squaredGLMM(SR_vs_SOIL)

#Create new dataframe that stores SOIL_COMP1 in 100 even units from smallest to lowest values 
SR_SOIL_Predict<-data.frame(int = 1, x = seq_func(Community_by_Plot$SOIL_COMP1))
#Create a new predictive model from this data and our previous model
SOIL_Predict<-lmer.predict(mod = SR_vs_SOIL, newdat=SR_SOIL_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against distance
with(Community_by_Plot, plot(Species_Richness ~ SOIL_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "SOIL Principle Component 1"))
#Change margins to fit legend 
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community_by_Plot$SOIL_COMP1), pred = SOIL_Predict$y, 
             upper = SOIL_Predict$phi, lower=SOIL_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0))


#Repeat for Litter Nutrients 
#Import Litter Data
Litter <- read.csv("Cleaned Up Data/Litter_MC.csv")
Litter <- as.data.frame(Litter[,c(2,3,4)], row.names = Litter$X)
#Perform PCA 
Litter_PCA <- princomp(Litter)
#Check the components
plot(Litter_PCA)
biplot(Litter_PCA, cex = 0.5)
#Save biplot
pdf(file = "Outputs/Figures/Litter_PCA_Biplot.pdf", height = 6, width = 6)
biplot(Litter_PCA, cex = 0.5)
dev.off()
#Comp1 explains most of the variance, lets use this as our explanatory variable
#extract the scores
Litter_PCA_Scores <- as.data.frame(Litter_PCA$scores)
#rename first column 
colnames(Litter_PCA_Scores) <- c("LITTER_COMP1", "LITTER_COMP2", "LITTER_COMP3") 
#Join AGV_COMP1 to Community_by_Plot data frame 
Community_by_Plot$LITTER_COMP1 <- Litter_PCA_Scores$LITTER_COMP1


#Construct Model for Species richness vs LITTER
SR_vs_LITTER <- lmer(Species_Richness ~ LITTER_COMP1+ (1|Transect), data = Community_by_Plot)
#Check Residuals 
plot(simulateResiduals(SR_vs_LITTER))
#Summary
summary(SR_vs_LITTER)
#Check the proportion of unique variance explained by this variable
r.squaredGLMM(SR_vs_SOIL)

#Create new dataframe that stores LITTER_COMP1 in 100 even units from smallest to lowest values 
SR_LITTER_Predict<-data.frame(int = 1, x = seq_func(Community_by_Plot$LITTER_COMP1))
#Create a new predictive model from this data and our previous model
LITTER_Predict<-lmer.predict(mod = SR_vs_LITTER, newdat=SR_LITTER_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against distance
with(Community_by_Plot, plot(Species_Richness ~ LITTER_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "LITTER Principle Component 1"))
#Change margins to fit legend 
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community_by_Plot$LITTER_COMP1), pred = LITTER_Predict$y, 
             upper = LITTER_Predict$phi, lower=LITTER_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0))


#Repeat for ABIOTIC variables 
#Import Abiotic 
Abiotic <- read.csv("Cleaned Up Data/Abiotc_Variables_MC.csv")
Abiotic <- as.data.frame(Abiotic[,c(3:4)], row.names = Abiotic$X)
#Perform PCA 
Abiotic_PCA <- princomp(Abiotic)
#Check the components
plot(Abiotic_PCA)
biplot(Abiotic_PCA, cex = 0.5)
#Save biplot
pdf(file = "Outputs/Figures/Litter_PCA_Biplot.pdf", height = 6, width = 6)
biplot(Litter_PCA, cex = 0.5)
dev.off()
#Comp1 and 2 explains most of the variance, lets use these as our explanatory variable
#extract the scores
Abiotic_PCA_Scores <- as.data.frame(Abiotic_PCA$scores)
#rename first column 
colnames(Abiotic_PCA_Scores) <- c("ABIOTIC_COMP1", "ABIOTIC_COMP2") 
#Join AGV_COMP1 and 2 to Community_by_Plot data frame 
Community_by_Plot$ABIOTIC_COMP1 <- Abiotic_PCA_Scores$ABIOTIC_COMP1
Community_by_Plot$ABIOTIC_COMP2 <- Abiotic_PCA_Scores$ABIOTIC_COMP2

#Construct model for Species Richness vs  Abiotic COMP1
SR_vs_ABIOTIC1 <- lmer(Species_Richness ~ ABIOTIC_COMP1 + (1|Transect), data = Community_by_Plot)
#Check Residuals
plot(simulateResiduals(SR_vs_ABIOTIC1))
#Summary
summary(SR_vs_ABIOTIC1)
#Check the proportion of unique variance explained by this variable 
r.squaredGLMM(SR_vs_ABIOTIC1)


#Create new dataframe that stores ABIOTIC_COMP1 in 100 even units from smallest to lowest values 
SR_ABIOTIC_COMP1_Predict<-data.frame(int = 1, x = seq_func(Community_by_Plot$ABIOTIC_COMP1))
#Create a new predictive model from this data and our previous model
ABIOTIC_COMP1_Predict<-lmer.predict(mod = SR_vs_ABIOTIC1, newdat=SR_ABIOTIC_COMP1_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against distance
with(Community_by_Plot, plot(Species_Richness ~ ABIOTIC_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "ABIOTIC Principle Component 1"))
#Change margins to fit legend 
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community_by_Plot$ABIOTIC_COMP1), pred = ABIOTIC_COMP1_Predict$y, 
             upper = ABIOTIC_COMP1_Predict$phi, lower=ABIOTIC_COMP1_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0))


#Repeat for ABIOTIC COMP2 
#Construct model for Species Richness vs  Abiotic COMP2
SR_vs_ABIOTIC2 <- lmer(Species_Richness ~ ABIOTIC_COMP2 + (1|Transect), data = Community_by_Plot)
#Check Residuals
plot(simulateResiduals(SR_vs_ABIOTIC2))
#Summary
summary(SR_vs_ABIOTIC2)
#Check the proportion of unique variance explained by this variable 
r.squaredGLMM(SR_vs_ABIOTIC2)


#Create new dataframe that stores ABIOTIC_COMP1 in 100 even units from smallest to lowest values 
SR_ABIOTIC_COMP2_Predict<-data.frame(int = 1, x = seq_func(Community_by_Plot$ABIOTIC_COMP2))
#Create a new predictive model from this data and our previous model
ABIOTIC_COMP2_Predict<-lmer.predict(mod = SR_vs_ABIOTIC2, newdat=SR_ABIOTIC_COMP2_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against distance
with(Community_by_Plot, plot(Species_Richness ~ ABIOTIC_COMP2, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "ABIOTIC Principle Component 1"))
#Change margins to fit legend 
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community_by_Plot$ABIOTIC_COMP2), pred = ABIOTIC_COMP2_Predict$y, 
             upper = ABIOTIC_COMP2_Predict$phi, lower=ABIOTIC_COMP2_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0))




#Save all scatter plots to pdf
pdf(file = "Outputs/Figures/Species_Richness_vs_Environment.pdf")
par(mar=c(5.1, 4.1, 4.1, 8.1))
with(Community, plot(Species_Richness ~ Distance, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "Distance From Forest Edge (m)"))
#Change margins to fit legend 
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community$Distance), pred = Distance_Predict$y, 
             upper = Distance_Predict$phi, lower=Distance_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0),xpd = TRUE)
with(Community_by_Plot, plot(Species_Richness ~ AGV_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "AGV Principle Component 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community_by_Plot$AGV_COMP1), pred = AGV_Predict$y, 
             upper = AGV_Predict$phi, lower=AGV_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0), xpd = TRUE)
with(Community_by_Plot, plot(Species_Richness ~ SOIL_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "SOIL Principle Component 1"), xpd = TRUE)
#Change margins to fit legend 
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community_by_Plot$SOIL_COMP1), pred = SOIL_Predict$y, 
             upper = SOIL_Predict$phi, lower=SOIL_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0), xpd = TRUE)
with(Community_by_Plot, plot(Species_Richness ~ LITTER_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "LITTER Principle Component 1"))
#Change margins to fit legend 
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community_by_Plot$LITTER_COMP1), pred = LITTER_Predict$y, 
             upper = LITTER_Predict$phi, lower=LITTER_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0), xpd = TRUE)
with(Community_by_Plot, plot(Species_Richness ~ ABIOTIC_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "ABIOTIC Principle Component 1"))
#Change margins to fit legend 
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community_by_Plot$ABIOTIC_COMP1), pred = ABIOTIC_COMP1_Predict$y, 
             upper = ABIOTIC_COMP1_Predict$phi, lower=ABIOTIC_COMP1_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0), xpd = TRUE)
with(Community_by_Plot, plot(Species_Richness ~ ABIOTIC_COMP2, col=my_colours[factor(Transect)], pch = 16, ylab = "Species Richness", xlab = "ABIOTIC Principle Component 1"))
#Change margins to fit legend 
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Community_by_Plot$ABIOTIC_COMP2), pred = ABIOTIC_COMP2_Predict$y, 
             upper = ABIOTIC_COMP2_Predict$phi, lower=ABIOTIC_COMP2_Predict$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.35,0), xpd = TRUE)
dev.off()



