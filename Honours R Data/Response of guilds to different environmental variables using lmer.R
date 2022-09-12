
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
source("Fitting Mixed Effects Models Function.R")
#Define colours to be used in plots
my_colours <- c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2",  "#CC6677","#AA4499")

#Import Guild Rel Abundance Data
Guilds <- read.csv("Cleaned Up Data/Guilds_by_Plots_MC.csv")
#Rename first column, make rownames plot 
Guilds <- as.data.frame(Guilds, row.names = Guilds$X)
colnames(Guilds)[1] <- "Plot"
#Calculate the relative abundance for each guild
Total_Abundance <- rowSums(Guilds[,c(2:6)])
#calculate total counts per row
Guilds$Total <- Total_Abundance
#Calculate relative abundance 
Guilds$Rel_Saprobe_Abundance <- (Guilds$Saprobe_Abundance/Guilds$Total) * 100
Guilds$Rel_Endomycorrhizae_Abundance <- (Guilds$Endomycorrhizae/Guilds$Total) * 100
Guilds$Rel_Ectomycorrhizae_Abundance <- (Guilds$Ectomycorrhizae/Guilds$Total) * 100
Guilds$Rel_Plant_Pathogen_Abundance <- (Guilds$Plant_Pathogen_Abundance/Guilds$Total) * 100
Guilds$Rel_Other_Abundance <- (Guilds$Other_Abundance/Guilds$Total) * 100

#Import Distance Data 
Distance <-read.csv("Raw_Data/Plot_Coordinates_MC.csv")
#here we are interested only in distance from the forest edge, remove all other columns bar Transect and make rownames plot 
Distance <- as.data.frame(Distance[,c(1,2,5)], row.names = Distance$Plot)

#Merge the two dataframes by Plot 
Guilds_Environment <- left_join(Guilds, Distance, by = "Plot")

#Transform the distance from edge column by adding -42 (distance from forest transect to edge) to each entry then sqrt transforming it
Guilds_Environment$Transformed_Distance <- sqrt(Guilds_Environment$Distance_From_Edge + 42)


#Import AGV Data - we will use the main pricniple component for our explanatory variables, if appropriate
AGV <- read.csv("Cleaned Up Data/AGV_Data.csv")
#convert first column to row names 
AGV <- as.data.frame(AGV, row.names = AGV$X)
AGV <- as.data.frame(AGV[,c(2,3,4)])
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
#Join AGV_COMP1 to Guilds_Environment  
Guilds_Environment$AGV_COMP1 <- AGV_PCA_Scores$AGV_COMP1

#Repeat for Soil 
#Import Soil 
Soil <- read.csv("Cleaned Up Data/Soil_Nutrients_MC.csv")
#assign rownames
rownames(Soil) <- rownames(Guilds_Environment)
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
Guilds_Environment$SOIL_COMP1 <- Soil_PCA_Scores$SOIL_COMP1

#Repeat for Litter 
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
Guilds_Environment$LITTER_COMP1 <- Litter_PCA_Scores$LITTER_COMP1

#Repeat for Abiotic
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
Guilds_Environment$ABIOTIC_COMP1 <- Abiotic_PCA_Scores$ABIOTIC_COMP1
Guilds_Environment$ABIOTIC_COMP2 <- Abiotic_PCA_Scores$ABIOTIC_COMP2


#Now we can model the response of each guild (excluding Other as it's too broad to be ABIOTIC_COMP1ingful) against these variables 

#Model the relative abundance of each guild against the transformed distance from forest edge using the lmer function from package lme4
#Saprobes 
Saprobe_Distance <- lmer(Rel_Saprobe_Abundance ~ Transformed_Distance + (1|Transect), data = Guilds_Environment)
#check residuals 
plot(simulateResiduals(Saprobe_Distance))
#Call Model Summary 
summary(Saprobe_Distance)
#Assess the proportion of variance explained by Explanatory Variable 
r.squaredGLMM(Saprobe_Distance)


#Create new dataframe that stores Distance in 100 even units from smallest to lowest values 
Saprobe_Distance_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$Transformed_Distance))
#Create a new predictive model from this data and our previous model
Distance_Predict_Saprobe<-lmer.predict(mod = Saprobe_Distance, newdat=Saprobe_Distance_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against distance

#Endo
Endo_Distance <- lmer(Rel_Endomycorrhizae_Abundance ~ Transformed_Distance + (1|Transect), data = Guilds_Environment)
#Plot Residuals 
plot(simulateResiduals(Endo_Distance))
#Call Model Summary 
summary(Endo_Distance)
#Assess the proportion of variance explained by Explatory Variable 
r.squaredGLMM(Endo_Distance)

#Create new dataframe that stores Distance in 100 even units from smallest to lowest values 
Endo_Distance_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$Transformed_Distance))
#Create a new predictive model from this data and our previous model
Distance_Predict_Endo<-lmer.predict(mod = Endo_Distance, newdat=Endo_Distance_Predict , se.mult=1.96, binom=F, poisson=F)



#Ecto
Ecto_Distance <- lmer(Rel_Ectomycorrhizae_Abundance ~ Transformed_Distance + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Ecto_Distance))
#Call Model Summary 
summary(Ecto_Distance)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Ecto_Distance)

#Create new dataframe that stores Distance in 100 even units from smallest to lowest values 
Ecto_Distance_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$Transformed_Distance))
#Create a new predictive model from this data and our previous model
Distance_Predict_Ecto<-lmer.predict(mod = Ecto_Distance, newdat=Ecto_Distance_Predict , se.mult=1.96, binom=F, poisson=F)



#Plant Path 
Path_Distance <- lmer(Rel_Plant_Pathogen_Abundance ~ Transformed_Distance + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Path_Distance))
#Call Model Summary 
summary(Path_Distance)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Path_Distance)

#Create new dataframe that stores Distance in 100 even units from smallest to lowest values 
Path_Distance_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$Transformed_Distance))
#Create a new predictive model from this data and our previous model
Distance_Predict_Path<-lmer.predict(mod = Path_Distance, newdat=Path_Distance_Predict , se.mult=1.96, binom=F, poisson=F)


#Repeat for AGV
#Model the relative abundance of each guild against AGV_COMP1  the lmer function from package lme4
#Saprobes 
Saprobe_AGV <- lmer(Rel_Saprobe_Abundance ~ AGV_COMP1 + (1|Transect), data = Guilds_Environment)
#check residuals 
plot(simulateResiduals(Saprobe_AGV))
#Call Model Summary 
summary(Saprobe_AGV)
#Assess the proportion of variance explained by Explanatory Variable 
r.squaredGLMM(Saprobe_AGV)


#Create new dataframe that stores AGV_COMP1 in 100 even units from smallest to lowest values 
Saprobe_AGV_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$AGV_COMP1))
#Create a new predictive model from this data and our previous model
AGV_Predict_Saprobe<-lmer.predict(mod = Saprobe_AGV, newdat=Saprobe_AGV_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against AGV

#Endo
Endo_AGV <- lmer(Rel_Endomycorrhizae_Abundance ~ AGV_COMP1 + (1|Transect), data = Guilds_Environment)
#Plot Residuals 
plot(simulateResiduals(Endo_AGV))
#Call Model Summary 
summary(Endo_AGV)
#Assess the proportion of variance explained by Explatory Variable 
r.squaredGLMM(Endo_AGV)

#Create new dataframe that stores AGV in 100 even units from smallest to lowest values 
Endo_AGV_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$AGV_COMP1))
#Create a new predictive model from this data and our previous model
AGV_Predict_Endo<-lmer.predict(mod = Endo_AGV, newdat=Endo_AGV_Predict , se.mult=1.96, binom=F, poisson=F)


#Ecto
Ecto_AGV <- lmer(Rel_Ectomycorrhizae_Abundance ~ AGV_COMP1 + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Ecto_AGV))
#Call Model Summary 
summary(Ecto_AGV)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Ecto_AGV)

#Create new dataframe that stores AGV in 100 even units from smallest to lowest values 
Ecto_AGV_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$AGV_COMP1))
#Create a new predictive model from this data and our previous model
AGV_Predict_Ecto<-lmer.predict(mod = Ecto_AGV, newdat=Ecto_AGV_Predict , se.mult=1.96, binom=F, poisson=F)


#Path
Path_AGV <- lmer(Rel_Plant_Pathogen_Abundance ~ AGV_COMP1 + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Path_AGV))
#Call Model Summary 
summary(Path_AGV)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Path_AGV)

#Create new dataframe that stores AGV in 100 even units from smallest to lowest values 
Path_AGV_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$AGV_COMP1))
#Create a new predictive model from this data and our previous model
AGV_Predict_Path<-lmer.predict(mod = Path_AGV, newdat=Path_AGV_Predict , se.mult=1.96, binom=F, poisson=F)

#Repeat for SOIL
#Model the relative abundance of each guild against SOIL_COMP1  the lmer function from package lme4
#Saprobes 
Saprobe_SOIL <- lmer(Rel_Saprobe_Abundance ~ SOIL_COMP1 + (1|Transect), data = Guilds_Environment)
#check residuals 
plot(simulateResiduals(Saprobe_SOIL))
#Call Model Summary 
summary(Saprobe_SOIL)
#Assess the proportion of variance explained by Explanatory Variable 
r.squaredGLMM(Saprobe_SOIL)


#Create new dataframe that stores SOIL_COMP1 in 100 even units from smallest to lowest values 
Saprobe_SOIL_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$SOIL_COMP1))
#Create a new predictive model from this data and our previous model
SOIL_Predict_Saprobe<-lmer.predict(mod = Saprobe_SOIL, newdat=Saprobe_SOIL_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against SOIL
with(Guilds_Environment, plot(Rel_Saprobe_Abundance ~ SOIL_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Saprobe Abundance", xlab = "SOIL COMP 1"))
#  
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$SOIL_COMP1), pred = SOIL_Predict_Saprobe$y, 
             upper = SOIL_Predict_Saprobe$phi, lower=SOIL_Predict_Saprobe$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)

#Endo
Endo_SOIL <- lmer(Rel_Endomycorrhizae_Abundance ~ SOIL_COMP1 + (1|Transect), data = Guilds_Environment)
#Plot Residuals 
plot(simulateResiduals(Endo_SOIL))
#Call Model Summary 
summary(Endo_SOIL)
#Assess the proportion of variance explained by Explatory Variable 
r.squaredGLMM(Endo_SOIL)

#Create new dataframe that stores SOIL in 100 even units from smallest to lowest values 
Endo_SOIL_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$SOIL_COMP1))
#Create a new predictive model from this data and our previous model
SOIL_Predict_Endo<-lmer.predict(mod = Endo_SOIL, newdat=Endo_SOIL_Predict , se.mult=1.96, binom=F, poisson=F)


#Ecto
Ecto_SOIL <- lmer(Rel_Ectomycorrhizae_Abundance ~ SOIL_COMP1 + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Ecto_SOIL))
#Call Model Summary 
summary(Ecto_SOIL)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Ecto_SOIL)

#Create new dataframe that stores SOIL in 100 even units from smallest to lowest values 
Ecto_SOIL_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$SOIL_COMP1))
#Create a new predictive model from this data and our previous model
SOIL_Predict_Ecto<-lmer.predict(mod = Ecto_SOIL, newdat=Ecto_SOIL_Predict , se.mult=1.96, binom=F, poisson=F)

#Path
Path_Soil <- lmer(Rel_Plant_Pathogen_Abundance ~ SOIL_COMP1 + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Path_Soil))
#Call Model Summary 
summary(Path_Soil)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Path_Soil)

#Create new dataframe that stores SOIL in 100 even units from smallest to lowest values 
Path_Soil_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$SOIL_COMP1))
#Create a new predictive model from this data and our previous model
SOIL_Predict_Path<-lmer.predict(mod = Path_Soil, newdat=Path_Soil_Predict , se.mult=1.96, binom=F, poisson=F)

#Repeat For LITTER
#Model the relative abundance of each guild against LITTER_COMP1  the lmer function from package lme4
#Saprobes 
Saprobe_LITTER <- lmer(Rel_Saprobe_Abundance ~ LITTER_COMP1 + (1|Transect), data = Guilds_Environment)
#check residuals 
plot(simulateResiduals(Saprobe_LITTER))
#Call Model Summary 
summary(Saprobe_LITTER)
#Assess the proportion of variance explained by Explanatory Variable 
r.squaredGLMM(Saprobe_LITTER)


#Create new dataframe that stores LITTER_COMP1 in 100 even units from smallest to lowest values 
Saprobe_LITTER_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$LITTER_COMP1))
#Create a new predictive model from this data and our previous model
LITTER_Predict_Saprobe<-lmer.predict(mod = Saprobe_LITTER, newdat=Saprobe_LITTER_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against LITTER
with(Guilds_Environment, plot(Rel_Saprobe_Abundance ~ LITTER_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Saprobe Abundance", xlab = "LITTER COMP 1"))

#Endo
Endo_LITTER <- lmer(Rel_Endomycorrhizae_Abundance ~ LITTER_COMP1 + (1|Transect), data = Guilds_Environment)
#Plot Residuals 
plot(simulateResiduals(Endo_LITTER))
#Call Model Summary 
summary(Endo_LITTER)
#Assess the proportion of variance explained by Explatory Variable 
r.squaredGLMM(Endo_LITTER)

#Create new dataframe that stores LITTER in 100 even units from smallest to lowest values 
Endo_LITTER_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$LITTER_COMP1))
#Create a new predictive model from this data and our previous model
LITTER_Predict_Endo<-lmer.predict(mod = Endo_LITTER, newdat=Endo_LITTER_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against LITTER

#Ecto
Ecto_LITTER <- lmer(Rel_Ectomycorrhizae_Abundance ~ LITTER_COMP1 + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Ecto_LITTER))
#Call Model Summary 
summary(Ecto_LITTER)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Ecto_LITTER)

#Create new dataframe that stores LITTER in 100 even units from smallest to lowest values 
Ecto_LITTER_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$LITTER_COMP1))
#Create a new predictive model from this data and our previous model
LITTER_Predict_Ecto<-lmer.predict(mod = Ecto_LITTER, newdat=Ecto_LITTER_Predict , se.mult=1.96, binom=F, poisson=F)


#Path
Path_LITTER <- lmer(Rel_Plant_Pathogen_Abundance ~ LITTER_COMP1 + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Path_LITTER))
#Call Model Summary 
summary(Path_LITTER)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Path_LITTER)

#Create new dataframe that stores LITTER in 100 even units from smallest to lowest values 
Path_LITTER_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$LITTER_COMP1))
#Create a new predictive model from this data and our previous model
LITTER_Predict_Path<-lmer.predict(mod = Path_LITTER, newdat=Path_LITTER_Predict , se.mult=1.96, binom=F, poisson=F)


#Repeat for ABIOTIC_1
#Saprobes 
Saprobe_ABIOTIC1 <- lmer(Rel_Saprobe_Abundance ~ ABIOTIC_COMP1 + (1|Transect), data = Guilds_Environment)
#check residuals 
plot(simulateResiduals(Saprobe_ABIOTIC1))
#Call Model Summary 
summary(Saprobe_ABIOTIC1)
#Assess the proportion of variance explained by Explanatory Variable 
r.squaredGLMM(Saprobe_ABIOTIC1)


#Create new dataframe that stores ABIOTIC_COMP1 in 100 even units from smallest to lowest values 
Saprobe_ABIOTIC1_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$ABIOTIC_COMP1))
#Create a new predictive model from this data and our previous model
ABIOTIC_Predict_Saprobe<-lmer.predict(mod = Saprobe_ABIOTIC1, newdat=Saprobe_ABIOTIC1_Predict , se.mult=1.96, binom=F, poisson=F)
with(Guilds_Environment, plot(Rel_Saprobe_Abundance ~ ABIOTIC_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = ("Relative Saprobe Abundance"), xlab = "ABIOTIC COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$ABIOTIC_COMP1), pred = ABIOTIC_Predict_Saprobe$y, 
             upper = ABIOTIC_Predict_Saprobe$phi, lower=ABIOTIC_Predict_Saprobe$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)

#Endo
Endo_ABIOTIC1 <- lmer(Rel_Endomycorrhizae_Abundance ~ ABIOTIC_COMP1 + (1|Transect), data = Guilds_Environment)
#Plot Residuals 
plot(simulateResiduals(Endo_ABIOTIC1))
#Call Model Summary 
summary(Endo_ABIOTIC1)
#Assess the proportion of variance explained by Explatory Variable 
r.squaredGLMM(Endo_ABIOTIC1)

#Create new dataframe that stores ABIOTIC in 100 even units from smallest to lowest values 
Endo_ABIOTIC1_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$ABIOTIC_COMP1))
#Create a new predictive model from this data and our previous model
ABIOTIC_Predict_Endo<-lmer.predict(mod = Endo_ABIOTIC1, newdat=Endo_ABIOTIC1_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against ABIOTIC

#Ecto
Ecto_ABIOTIC1 <- lmer(Rel_Ectomycorrhizae_Abundance ~ ABIOTIC_COMP1 + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Ecto_ABIOTIC1))
#Call Model Summary 
summary(Ecto_ABIOTIC1)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Ecto_ABIOTIC1)

#Create new dataframe that stores ABIOTIC in 100 even units from smallest to lowest values 
Ecto_ABIOTIC1_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$ABIOTIC_COMP1))
#Create a new predictive model from this data and our previous model
ABIOTIC_Predict_Ecto<-lmer.predict(mod = Ecto_ABIOTIC1, newdat=Ecto_ABIOTIC1_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against ABIOTIC

#Path
Path_ABIOTIC1 <- lmer(Rel_Plant_Pathogen_Abundance ~ ABIOTIC_COMP1 + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Path_ABIOTIC1))
#Call Model Summary 
summary(Path_ABIOTIC1)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Path_ABIOTIC1)

#Create new dataframe that stores ABIOTIC in 100 even units from smallest to lowest values 
Path_ABIOTIC1_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$ABIOTIC_COMP1))
#Create a new predictive model from this data and our previous model
ABIOTIC_Predict_Path<-lmer.predict(mod = Path_ABIOTIC1, newdat=Path_ABIOTIC1_Predict , se.mult=1.96, binom=F, poisson=F)

#Repeat for ABIOTIC_2
#Model the relative abundance of each guild against ABIOTIC_COMP2  the lmer function from package lme4
#Saprobes 
Saprobe_ABIOTIC2 <- lmer(Rel_Saprobe_Abundance ~ ABIOTIC_COMP2 + (1|Transect), data = Guilds_Environment)
#check residuals 
plot(simulateResiduals(Saprobe_ABIOTIC2))
#Call Model Summary 
summary(Saprobe_ABIOTIC2)
#Assess the proportion of variance explained by Explanatory Variable 
r.squaredGLMM(Saprobe_ABIOTIC2)


#Create new dataframe that stores ABIOTIC_COMP2 in 100 even units from smallest to lowest values 
Saprobe_ABIOTIC2_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$ABIOTIC_COMP2))
#Create a new predictive model from this data and our previous model
ABIOTIC_Predict_Saprobe<-lmer.predict(mod = Saprobe_ABIOTIC2, newdat=Saprobe_ABIOTIC2_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against ABIOTIC
with(Guilds_Environment, plot(Rel_Saprobe_Abundance ~ ABIOTIC_COMP2, col=my_colours[factor(Transect)], pch = 16, ylab = ("Relative Saprobe Abundance"), xlab = "ABIOTIC COMP 1"))

#Endo
Endo_ABIOTIC2 <- lmer(Rel_Endomycorrhizae_Abundance ~ ABIOTIC_COMP2 + (1|Transect), data = Guilds_Environment)
#Plot Residuals 
plot(simulateResiduals(Endo_ABIOTIC2))
#Call Model Summary 
summary(Endo_ABIOTIC2)
#Assess the proportion of variance explained by Explatory Variable 
r.squaredGLMM(Endo_ABIOTIC2)

#Create new dataframe that stores ABIOTIC in 100 even units from smallest to lowest values 
Endo_ABIOTIC2_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$ABIOTIC_COMP2))
#Create a new predictive model from this data and our previous model
ABIOTIC_Predict_Endo<-lmer.predict(mod = Endo_ABIOTIC2, newdat=Endo_ABIOTIC2_Predict , se.mult=1.96, binom=F, poisson=F)
#Plot SR against ABIOTIC

#Ecto
Ecto_ABIOTIC2 <- lmer(Rel_Ectomycorrhizae_Abundance ~ ABIOTIC_COMP2 + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Ecto_ABIOTIC2))
#Call Model Summary 
summary(Ecto_ABIOTIC2)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Ecto_ABIOTIC2)

#Create new dataframe that stores ABIOTIC in 100 even units from smallest to lowest values 
Ecto_ABIOTIC2_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$ABIOTIC_COMP2))
#Create a new predictive model from this data and our previous model
ABIOTIC_Predict_Ecto<-lmer.predict(mod = Ecto_ABIOTIC2, newdat=Ecto_ABIOTIC2_Predict , se.mult=1.96, binom=F, poisson=F)


#Path
Path_ABIOTIC2 <- lmer(Rel_Plant_Pathogen_Abundance ~ ABIOTIC_COMP2 + (1|Transect), data = Guilds_Environment)
#Check Residuals
plot(simulateResiduals(Path_ABIOTIC2))
#Call Model Summary 
summary(Path_ABIOTIC2)
#Assess the proportion of variance explained by Exploratory Variable 
r.squaredGLMM(Path_ABIOTIC2)

#Create new dataframe that stores ABIOTIC in 100 even units from smallest to lowest values 
Path_ABIOTIC2_Predict<-data.frame(int = 1, x = seq_func(Guilds_Environment$ABIOTIC_COMP2))
#Create a new predictive model from this data and our previous model
ABIOTIC_Predict_Path<-lmer.predict(mod = Path_ABIOTIC2, newdat=Path_ABIOTIC2_Predict , se.mult=1.96, binom=F, poisson=F)

#Plot Figues and save to pdf 
#Saprobes
pdf(file = "Outputs/Figures/Saprobe_Environment.pdf")
par(mar = c(5.1,4.5,4.5,8.1))
with(Guilds_Environment, plot(Rel_Saprobe_Abundance ~ Transformed_Distance, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Saprobe Abundance", xlab = "Transformed Distance From Forest Edge (m)"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$Transformed_Distance), pred = Distance_Predict_Saprobe$y, 
             upper = Distance_Predict_Saprobe$phi, lower=Distance_Predict_Saprobe$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n", cex = 0.8, inset = c(-0.3535,0), xpd = TRUE)

with(Guilds_Environment, plot(Rel_Saprobe_Abundance ~ AGV_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Saprobe Abundance", xlab = "AGV COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$AGV_COMP1), pred = AGV_Predict_Saprobe$y, 
             upper = AGV_Predict_Saprobe$phi, lower=AGV_Predict_Saprobe$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
with(Guilds_Environment, plot(Rel_Saprobe_Abundance ~ SOIL_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Saprobe Abundance", xlab = "SOIL COMP 1"))
#  
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$SOIL_COMP1), pred = SOIL_Predict_Saprobe$y, 
             upper = SOIL_Predict_Saprobe$phi, lower=SOIL_Predict_Saprobe$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
with(Guilds_Environment, plot(Rel_Saprobe_Abundance ~ LITTER_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Saprobe Abundance", xlab = "LITTER COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$LITTER_COMP1), pred = LITTER_Predict_Saprobe$y, 
             upper = LITTER_Predict_Saprobe$phi, lower=LITTER_Predict_Saprobe$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
with(Guilds_Environment, plot(Rel_Saprobe_Abundance ~ ABIOTIC_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Saprobe Abundance", xlab = "ABIOTIC COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$ABIOTIC_COMP1), pred = ABIOTIC_Predict_Saprobe$y, 
             upper = ABIOTIC_Predict_Saprobe$phi, lower=ABIOTIC_Predict_Saprobe$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
with(Guilds_Environment, plot(Rel_Saprobe_Abundance ~ ABIOTIC_COMP2, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Saprobe Abundance", xlab = "ABIOTIC COMP 2"))
#Plot our mixed effects model with confidence estimates  
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$ABIOTIC_COMP2), pred = ABIOTIC_Predict_Saprobe$y, 
             upper = ABIOTIC_Predict_Saprobe$phi, lower=ABIOTIC_Predict_Saprobe$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
dev.off()

#Endo 
pdf(file = "Outputs/Figures/Endo_Environment.pdf")
par(mar = c(5.1,4.5,4.5,8.1))
#Distance
with(Guilds_Environment, plot(Rel_Endomycorrhizae_Abundance ~ Transformed_Distance, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Endomycorrhizae Abundance", xlab = "Transformed Distance From Forest Edge (m)"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$Transformed_Distance), pred = Distance_Predict_Endo$y, 
             upper = Distance_Predict_Endo$phi, lower=Distance_Predict_Endo$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#AGV
with(Guilds_Environment, plot(Rel_Endomycorrhizae_Abundance ~ AGV_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Endomycorrhizae Abundance", xlab = "AGV COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$AGV_COMP1), pred = AGV_Predict_Endo$y, 
             upper = AGV_Predict_Endo$phi, lower=AGV_Predict_Endo$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#SOIL
with(Guilds_Environment, plot(Rel_Endomycorrhizae_Abundance ~ SOIL_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Endomycorrhizae Abundance", xlab = "SOIL COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$SOIL_COMP1), pred = SOIL_Predict_Endo$y, 
             upper = SOIL_Predict_Endo$phi, lower=SOIL_Predict_Endo$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#LITTER
with(Guilds_Environment, plot(Rel_Endomycorrhizae_Abundance ~ LITTER_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Endomycorrhizae Abundance", xlab = "LITTER COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$LITTER_COMP1), pred = LITTER_Predict_Endo$y, 
             upper = LITTER_Predict_Endo$phi, lower=LITTER_Predict_Endo$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#ABIOTIC 1
with(Guilds_Environment, plot(Rel_Endomycorrhizae_Abundance ~ ABIOTIC_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Endomycorrhizae Abundance", xlab = "ABIOTIC COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$ABIOTIC_COMP1), pred = ABIOTIC_Predict_Endo$y, 
             upper = ABIOTIC_Predict_Endo$phi, lower=ABIOTIC_Predict_Endo$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#ABIOTIC 2
with(Guilds_Environment, plot(Rel_Endomycorrhizae_Abundance ~ ABIOTIC_COMP2, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Endomycorrhizae Abundance", xlab = "ABIOTIC COMP 2"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$ABIOTIC_COMP2), pred = ABIOTIC_Predict_Endo$y, 
             upper = ABIOTIC_Predict_Endo$phi, lower=ABIOTIC_Predict_Endo$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
dev.off()

#Ecto 
#Distance
pdf(file = "Outputs/Figures/Ecto_Environment.pdf")
par(mar = c(5.1,4.5,4.5,8.1))
with(Guilds_Environment, plot(Rel_Ectomycorrhizae_Abundance ~ Transformed_Distance, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Ectomycorrhizae Abundance", xlab = "Transformed Distance From Forest Edge (m)"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$Transformed_Distance), pred = Distance_Predict_Ecto$y, 
             upper = Distance_Predict_Ecto$phi, lower=Distance_Predict_Ecto$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#AGV
with(Guilds_Environment, plot(Rel_Ectomycorrhizae_Abundance ~ AGV_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Ectomycorrhizae Abundance", xlab = "AGV COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$AGV_COMP1), pred = AGV_Predict_Ecto$y, 
             upper = AGV_Predict_Ecto$phi, lower=AGV_Predict_Ecto$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#SOIL

with(Guilds_Environment, plot(Rel_Ectomycorrhizae_Abundance ~ SOIL_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Ectomycorrhizae Abundance", xlab = "SOIL COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$SOIL_COMP1), pred = SOIL_Predict_Ecto$y, 
             upper = SOIL_Predict_Ecto$phi, lower=SOIL_Predict_Ecto$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#LITTER
with(Guilds_Environment, plot(Rel_Ectomycorrhizae_Abundance ~ LITTER_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Ectomycorrhizae Abundance", xlab = "LITTER COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$LITTER_COMP1), pred = LITTER_Predict_Ecto$y, 
             upper = LITTER_Predict_Ecto$phi, lower=LITTER_Predict_Ecto$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#ABIOTIC1
with(Guilds_Environment, plot(Rel_Ectomycorrhizae_Abundance ~ ABIOTIC_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Ectomycorrhizae Abundance", xlab = "ABIOTIC COMP 1"))
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$ABIOTIC_COMP1), pred = ABIOTIC_Predict_Ecto$y, 
             upper = ABIOTIC_Predict_Ecto$phi, lower=ABIOTIC_Predict_Ecto$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#ABIOTIC2
with(Guilds_Environment, plot(Rel_Ectomycorrhizae_Abundance ~ ABIOTIC_COMP2, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Ectomycorrhizae Abundance", xlab = "ABIOTIC COMP 2"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$ABIOTIC_COMP2), pred = ABIOTIC_Predict_Ecto$y, 
             upper = ABIOTIC_Predict_Ecto$phi, lower=ABIOTIC_Predict_Ecto$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
dev.off()

#Plant Pathogens 
pdf(file = "Outputs/Figures/Path_Environment.pdf")
par(mar = c(5.1,4.5,4.5,8.1))
#Distance 
with(Guilds_Environment, plot(Rel_Plant_Pathogen_Abundance ~ Transformed_Distance, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Pathmycorrhizae Abundance", xlab = "Transformed Distance From Edge (m)"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$Transformed_Distance), pred = Distance_Predict_Path$y, 
             upper = Distance_Predict_Path$phi, lower=Distance_Predict_Path$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#AGV
with(Guilds_Environment, plot(Rel_Plant_Pathogen_Abundance ~ AGV_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Pathmycorrhizae Abundance", xlab = "AGV COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$AGV_COMP1), pred = AGV_Predict_Path$y, 
             upper = AGV_Predict_Path$phi, lower=AGV_Predict_Path$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#SOIL
with(Guilds_Environment, plot(Rel_Plant_Pathogen_Abundance ~ SOIL_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Pathmycorrhizae Abundance", xlab = "SOIL COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$SOIL_COMP1), pred = SOIL_Predict_Path$y, 
             upper = SOIL_Predict_Path$phi, lower=SOIL_Predict_Path$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#LITTER
with(Guilds_Environment, plot(Rel_Plant_Pathogen_Abundance ~ LITTER_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Pathmycorrhizae Abundance", xlab = "LITTER COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$LITTER_COMP1), pred = LITTER_Predict_Path$y, 
             upper = LITTER_Predict_Path$phi, lower=LITTER_Predict_Path$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
#ABIOTIC1
with(Guilds_Environment, plot(Rel_Plant_Pathogen_Abundance ~ ABIOTIC_COMP1, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Pathmycorrhizae Abundance", xlab = "ABIOTIC COMP 1"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$ABIOTIC_COMP1), pred = ABIOTIC_Predict_Path$y, 
             upper = ABIOTIC_Predict_Path$phi, lower=ABIOTIC_Predict_Path$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)

#ABIOTIC2
with(Guilds_Environment, plot(Rel_Plant_Pathogen_Abundance ~ ABIOTIC_COMP2, col=my_colours[factor(Transect)], pch = 16, ylab = "Relative Pathmycorrhizae Abundance", xlab = "ABIOTIC COMP 2"))
#Plot our mixed effects model with confidence estimates 
plot.CI.func(x.for.plot = seq_func(Guilds_Environment$ABIOTIC_COMP2), pred = ABIOTIC_Predict_Path$y, 
             upper = ABIOTIC_Predict_Path$phi, lower=ABIOTIC_Predict_Path$plo, 
             env.colour = "grey", env.trans=50, line.colour="black", line.weight=2, line.type=1)
legend("right", legend = c("Forest", "Forest Edge Interior", "Forest Edge Exterior", "Pioneer Near", "Pioneer Far", "Grass Near", "Grass Far"), 
       col = c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2", "#CC6677", "#AA4499"), pch = 16, bty = "n",  cex = 0.8, inset = c(-0.357,0), xpd = TRUE)
dev.off()
