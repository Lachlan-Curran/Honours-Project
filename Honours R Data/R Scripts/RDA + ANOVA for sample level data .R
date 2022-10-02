rm(list=ls())
library(tidyverse)
library(dplyr)
library(vegan)
library(patchwork)


#Import Community Data
Community <- read.csv("Cleaned Up Data/Fungi_by_Sample.csv")
#Assign row names and remove first column 
Community <- as.data.frame(Community[,c(2:5138)], row.names = Community$X)
#Reorder rows and remove UMNR 
#Remove UMNR rows 
Community<- as.data.frame(Community[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,43,44,45,46,47),])

#Import sample level data - soil and litter nutrients + spatial Distance 
#Soil
Soil_Nutrients <- read.csv("Raw_Data/Soil_Nutrients.csv") #This contains only MC data, we don't need to remove UMNR
Soil_Nutrients <- as.data.frame(Soil_Nutrients[c(1:41),c(3:6)])
#fix row error
Soil_Nutrients[6,1] <- "Forest_3_B"
#Assign row names 
Soil_Nutrients <- as.data.frame(Soil_Nutrients[,c(2:4)], row.names = Soil_Nutrients$Sample)




#Litter
Leaf_Litter_Nutrients <- read.csv("Raw_Data/Leaf_Litter_N_P .csv")
Leaf_Litter_Nutrients <- as.data.frame(Leaf_Litter_Nutrients[,c(3,4,7)], row.names = Leaf_Litter_Nutrients$Sample)



Distance <-read.csv("Raw_Data/Sample_Coordinates_MC.csv")
#Only take columns with lat and long 
Distance <- as.data.frame(Distance[,c(2,3)])
#There are two typoa in this dataframe, we will correct them here 
Distance[c(38,39),2] <- 152.8767
#Make the rownames for this data frame the same as the other two
rownames <- rownames(Soil_Nutrients)
Distance <- as.data.frame(Distance[,], row.names =  rownames)

#Import Guild Data  and remove UMNR rows
Guild <- read.csv("Cleaned Up Data/Guilds_Condensed.csv")
#Remove UMNR rows
Guild <- as.data.frame(Guild[-c(42:47),])
#assign rownames
Guild <- as.data.frame(Guild[,c(2:6)], row.names = Guild$X)



#Break up datasets according to their transects 
#Community
Forest_Community <- as.data.frame(Community[c(1:6),])
Forest_Interior_Edge_Community <- as.data.frame(Community[c(7:12),])
Forest_Exterior_Edge_Community <- as.data.frame(Community[c(13:18),])
Pioneer_Near_Community <- as.data.frame(Community[c(19:24),])
Pioneer_Far_Community <- as.data.frame(Community[c(25:30),])
Grass_Near_Community <- as.data.frame(Community[c(31:36),])
Grass_Far_Community <- as.data.frame(Community[c(37:41),])

#Guild 
Forest_Guilds <- as.data.frame(Guild[c(1:6),])
Forest_Interior_Edge_Guilds <- as.data.frame(Guild[c(7:12),])
Forest_Exterior_Edge_Guilds <- as.data.frame(Guild[c(13:18),])
Pioneer_Near_Guilds <- as.data.frame(Guild[c(19:24),])
Pioneer_Far_Guilds <- as.data.frame(Guild[c(25:30),])
Grass_Near_Guilds <- as.data.frame(Guild[c(31:36),])
Grass_Far_Guilds <- as.data.frame(Guild[c(37:41),])

#Distance
Forest_Distance <- as.data.frame(Distance[c(1:6),])
Forest_Interior_Edge_Distance <- as.data.frame(Distance[c(7:12),])
Forest_Exterior_Edge_Distance <- as.data.frame(Distance[c(13:18),])
Pioneer_Near_Distance <- as.data.frame(Distance[c(19:24),])
Pioneer_Far_Distance <- as.data.frame(Distance[c(25:30),])
Grass_Near_Distance <- as.data.frame(Distance[c(31:36),])
Grass_Far_Distance <- as.data.frame(Distance[c(37:41),])

#Soil 
Forest_Soil <- as.data.frame(Soil_Nutrients[c(1:6),])
Forest_Interior_Edge_Soil  <- as.data.frame(Soil_Nutrients[c(7:12),])
Forest_Exterior_Edge_Soil  <- as.data.frame(Soil_Nutrients[c(13:18),])
Pioneer_Near_Soil  <- as.data.frame(Soil_Nutrients[c(19:24),])
Pioneer_Far_Soil  <- as.data.frame(Soil_Nutrients[c(25:30),])
Grass_Near_Soil  <- as.data.frame(Soil_Nutrients[c(31:36),])
Grass_Far_Soil  <- as.data.frame(Soil_Nutrients[c(37:41),])

#Leaf
Forest_Litter <- as.data.frame(Leaf_Litter_Nutrients [c(1:6),])
Forest_Interior_Edge_Litter  <- as.data.frame(Leaf_Litter_Nutrients [c(7:12),])
Forest_Exterior_Edge_Litter  <- as.data.frame(Leaf_Litter_Nutrients [c(13:18),])
Pioneer_Near_Litter  <- as.data.frame(Leaf_Litter_Nutrients [c(19:24),])
Pioneer_Far_Litter  <- as.data.frame(Leaf_Litter_Nutrients [c(25:30),])
Grass_Near_Litter  <- as.data.frame(Leaf_Litter_Nutrients [c(31:36),])
Grass_Far_Litter <- as.data.frame(Leaf_Litter_Nutrients [c(37:41),])



#Lets do a quick varpart analysis to see if space or soil/leaf nutrients contributes more variance to the community/guilds - just eyball the plots
#import soil nutrient data 


#Varpart
Forest_nutrients_vs_Distance <- varpart(Forest_Community, Forest_Soil, Forest_Litter, Forest_Distance, transfo = "log")
FEI_nutrients_vs_Distance <- varpart(Forest_Interior_Edge_Community, Forest_Interior_Edge_Soil, Forest_Interior_Edge_Litter, Forest_Interior_Edge_Distance, transfo = "log")
FEE_nutrients_vs_Distance <- varpart(Forest_Exterior_Edge_Community, Forest_Exterior_Edge_Soil, Forest_Exterior_Edge_Litter, Forest_Exterior_Edge_Distance, transfo = "log")
PN_nutrients_vs_Distance <- varpart(Pioneer_Near_Community, Pioneer_Near_Soil, Pioneer_Near_Litter, Pioneer_Near_Distance, transfo = "log")
PF_nutrients_vs_Distance <- varpart(Pioneer_Far_Community, Pioneer_Far_Soil, Pioneer_Far_Litter, Pioneer_Far_Distance, transfo = "log")
GN_nutrients_vs_Distance <- varpart(Grass_Near_Community, Grass_Near_Soil, Grass_Near_Litter, Grass_Near_Distance, transfo = "log")
GF_nutrients_vs_Distance <- varpart(Grass_Far_Community, Grass_Far_Soil, Grass_Far_Litter, Grass_Far_Distance, transfo = "log")


#Plot the varpart output
pdf(file = "Outputs/Figures/Fine_Scale_Varpart_Community.pdf")
par(mar=c(1, 1, 2, 10), xpd=TRUE)
plot(Forest_nutrients_vs_Distance, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart Forest Community, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(FEI_nutrients_vs_Distance, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart FEI Community, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(FEE_nutrients_vs_Distance, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart FEE Community, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(PN_nutrients_vs_Distance, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart PN Community, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(PF_nutrients_vs_Distance, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart PF Community, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(GN_nutrients_vs_Distance, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart GN Community, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(GF_nutrients_vs_Distance, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart GF Community, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
dev.off()


#Perform RDA on each variable per transect + ANOVA to determine significance of result 
#Forest
RDA_Forest_Soil <- rda(decostand(Forest_Community, method = 'log'), Forest_Soil)
anova(RDA_Forest_Soil)
RDA_Forest_Litter <- rda(decostand(Forest_Community, method = 'log'), Forest_Litter)
anova(RDA_Forest_Litter)
RDA_Forest_Distance <- rda(decostand(Forest_Community, method = 'log'), Forest_Distance)
anova(RDA_Forest_Distance)
#Forest Edge Interior (FEI)
RDA_FEI_Soil <- rda(decostand(Forest_Interior_Edge_Community, method = 'log'), Forest_Interior_Edge_Soil)
anova(RDA_FEI_Soil)
RDA_FEI_Litter <- rda(decostand(Forest_Interior_Edge_Community, method = 'log'), Forest_Interior_Edge_Litter)
anova(RDA_FEI_Litter)
RDA_FEI_Distance <- rda(decostand(Forest_Interior_Edge_Community, method = 'log'), Forest_Interior_Edge_Distance)
anova(RDA_FEI_Distance)
#Forest_Edge_Exterior (FEE)
RDA_FEE_Soil <- rda(decostand(Forest_Exterior_Edge_Community, method = 'log'), Forest_Exterior_Edge_Soil)
anova(RDA_FEE_Soil)
RDA_FEE_Litter <- rda(decostand(Forest_Exterior_Edge_Community, method = 'log'), Forest_Exterior_Edge_Litter)
anova(RDA_FEE_Litter)
RDA_FEE_Distance <- rda(decostand(Forest_Exterior_Edge_Community, method = 'log'), Forest_Exterior_Edge_Distance)
anova(RDA_FEE_Distance)
#Pioneer Near (PN)
RDA_PN_Soil <- rda(decostand(Pioneer_Near_Community, method = 'log'), Pioneer_Near_Soil)
anova(RDA_PN_Soil)
RDA_PN_Litter <- rda(decostand(Pioneer_Near_Community, method = 'log'), Pioneer_Near_Litter)
anova(RDA_PN_Litter)
RDA_PN_Distance <- rda(decostand(Pioneer_Near_Community, method = 'log'),Pioneer_Near_Distance)
anova(RDA_PN_Distance)
#Pioneer Far (PF)
RDA_PF_Soil <- rda(decostand(Pioneer_Far_Community, method = 'log'), Pioneer_Far_Soil)
anova(RDA_PF_Soil)
RDA_PF_Litter <- rda(decostand(Pioneer_Far_Community, method = 'log'), Pioneer_Far_Litter)
anova(RDA_PF_Litter)
RDA_PF_Distance <- rda(decostand(Pioneer_Far_Community, method = 'log'),Pioneer_Far_Distance)
anova(RDA_PF_Distance)
#Grass Near (GN)
RDA_GN_Soil <- rda(decostand(Grass_Near_Community, method = 'log'), Grass_Near_Soil)
anova(RDA_GN_Soil)
RDA_GN_Litter <- rda(decostand(Grass_Near_Community, method = 'log'), Grass_Near_Litter)
anova(RDA_GN_Litter)
RDA_GN_Distance <- rda(decostand(Grass_Near_Community, method = 'log'),Grass_Near_Distance)
anova(RDA_GN_Distance)
#Grass Far 
RDA_GF_Soil <- rda(decostand(Grass_Far_Community, method = 'log'), Grass_Far_Soil)
anova(RDA_GN_Soil)
RDA_GF_Litter <- rda(decostand(Grass_Far_Community, method = 'log'), Grass_Far_Litter)
anova(RDA_GN_Litter)
RDA_GF_Distance <- rda(decostand(Grass_Far_Community, method = 'log'),Grass_Far_Distance)
anova(RDA_GF_Distance)

#Repeat for Guilds 

#Varpart
Forest_nutrients_vs_Distance_Guild <- varpart(Forest_Guilds, Forest_Soil, Forest_Litter, Forest_Distance, transfo = "log")
FEI_nutrients_vs_Distance_Guild <- varpart(Forest_Interior_Edge_Guilds, Forest_Interior_Edge_Soil, Forest_Interior_Edge_Litter, Forest_Interior_Edge_Distance, transfo = "log")
FEE_nutrients_vs_Distance_Guild <- varpart(Forest_Exterior_Edge_Guilds, Forest_Exterior_Edge_Soil, Forest_Exterior_Edge_Litter, Forest_Exterior_Edge_Distance, transfo = "log")
PN_nutrients_vs_Distance_Guild <- varpart(Pioneer_Near_Guilds, Pioneer_Near_Soil, Pioneer_Near_Litter, Pioneer_Near_Distance, transfo = "log")
PF_nutrients_vs_Distance_Guild <- varpart(Pioneer_Far_Guilds, Pioneer_Far_Soil, Pioneer_Far_Litter, Pioneer_Far_Distance, transfo = "log")
GN_nutrients_vs_Distance_Guild <- varpart(Grass_Near_Guilds, Grass_Near_Soil, Grass_Near_Litter, Grass_Near_Distance, transfo = "log")
GF_nutrients_vs_Distance_Guild <- varpart(Grass_Far_Guilds, Grass_Far_Soil, Grass_Far_Litter, Grass_Far_Distance, transfo = "log")

#Plot the varpart output
pdf(file = "Outputs/Figures/Fine_Scale_Varpart_Guilds.pdf")
par(mar=c(1, 1, 2, 10), xpd=TRUE)
plot(Forest_nutrients_vs_Distance_Guild, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart Forest Guilds, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(FEI_nutrients_vs_Distance_Guild, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart FEI Guilds, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(FEE_nutrients_vs_Distance_Guild, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart FEE Guilds, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(PN_nutrients_vs_Distance_Guild, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart PN Guilds, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(PF_nutrients_vs_Distance_Guild, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart PF Guilds, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(GN_nutrients_vs_Distance_Guild, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart GN Guilds, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
plot(GF_nutrients_vs_Distance_Guild, bg = c("Red", "Blue", "Yellow"))
legend('left', inset=c(0.95, 0), bty = "n", cex = 0.8, legend = c("Fine Scale Varpart GF Guilds, Litter and Soil Nutrients + Space", "X1 = Soil", "X2 = Litter", "X3 = Distance"))
dev.off()


#Perform RDA on each variable per transect + ANOVA to determine significance of result 
#Forest
RDA_Forest_Soil_Guilds <- rda(decostand(Forest_Guilds, method = 'log'), Forest_Soil)
anova(RDA_Forest_Soil_Guilds)
RDA_Forest_Litter_Guilds <- rda(decostand(Forest_Guilds, method = 'log'), Forest_Litter)
anova(RDA_Forest_Litter_Guilds)
RDA_Forest_Distance_Guilds <- rda(decostand(Forest_Guilds, method = 'log'), Forest_Distance)
anova(RDA_Forest_Distance_Guilds)
#Forest Edge Interior (FEI)
RDA_FEI_Soil_Guilds <- rda(decostand(Forest_Interior_Edge_Guilds, method = 'log'), Forest_Interior_Edge_Soil)
anova(RDA_FEI_Soil_Guilds)
RDA_FEI_Litter_Guilds <- rda(decostand(Forest_Interior_Edge_Guilds, method = 'log'), Forest_Interior_Edge_Litter)
anova(RDA_FEI_Litter_Guilds)
RDA_FEI_Distance_Guilds <- rda(decostand(Forest_Interior_Edge_Guilds, method = 'log'), Forest_Interior_Edge_Distance)
anova(RDA_FEI_Distance_Guilds)
#Forest_Edge_Exterior (FEE)
RDA_FEE_Soil_Guilds <- rda(decostand(Forest_Exterior_Edge_Guilds, method = 'log'), Forest_Exterior_Edge_Soil)
anova(RDA_FEE_Soil_Guilds)
RDA_FEE_Litter_Guilds <- rda(decostand(Forest_Exterior_Edge_Guilds, method = 'log'), Forest_Exterior_Edge_Litter)
anova(RDA_FEE_Litter_Guilds)
RDA_FEE_Distance_Guilds <- rda(decostand(Forest_Exterior_Edge_Guilds, method = 'log'), Forest_Exterior_Edge_Distance)
anova(RDA_FEE_Distance_Guilds)
#Pioneer Near (PN)
RDA_PN_Soil_Guilds <- rda(decostand(Pioneer_Near_Guilds, method = 'log'), Pioneer_Near_Soil)
anova(RDA_PN_Soil_Guilds)
RDA_PN_Litter_Guilds <- rda(decostand(Pioneer_Near_Guilds, method = 'log'), Pioneer_Near_Litter)
anova(RDA_PN_Litter_Guilds)
RDA_PN_Distance_Guilds <- rda(decostand(Pioneer_Near_Guilds, method = 'log'),Pioneer_Near_Distance)
anova(RDA_PN_Distance_Guilds)
#Pioneer Far (PF)
RDA_PF_Soil_Guilds <- rda(decostand(Pioneer_Far_Guilds, method = 'log'), Pioneer_Far_Soil)
anova(RDA_PF_Soil_Guilds)
RDA_PF_Litter_Guilds <- rda(decostand(Pioneer_Far_Guilds, method = 'log'), Pioneer_Far_Litter)
anova(RDA_PF_Litter_Guilds)
RDA_PF_Distance_Guilds <- rda(decostand(Pioneer_Far_Guilds, method = 'log'),Pioneer_Far_Distance)
anova(RDA_PF_Distance_Guilds)
#Grass Near (GN)
RDA_GN_Soil_Guilds <- rda(decostand(Grass_Near_Guilds, method = 'log'), Grass_Near_Soil)
anova(RDA_GN_Soil_Guilds)
RDA_GN_Litter_Guilds <- rda(decostand(Grass_Near_Guilds, method = 'log'), Grass_Near_Litter)
anova(RDA_GN_Litter_Guilds)
RDA_GN_Distance_Guilds <- rda(decostand(Grass_Near_Guilds, method = 'log'),Grass_Near_Distance)
anova(RDA_GN_Distance_Guilds)
#Grass Far 
RDA_GF_Soil_Guilds <- rda(decostand(Grass_Far_Guilds, method = 'log'), Grass_Far_Soil)
anova(RDA_GN_Soil_Guilds)
RDA_GF_Litter_Guilds <- rda(decostand(Grass_Far_Guilds, method = 'log'), Grass_Far_Litter)
anova(RDA_GN_Litter_Guilds)
RDA_GF_Distance_Guilds <- rda(decostand(Grass_Far_Guilds, method = 'log'),Grass_Far_Distance)
anova(RDA_GF_Distance_Guilds)

#Plot community dissimilarity against spatial coordinates per transect 
#Convert each dataframe to distance object 
Forest_Com <- vegdist(Forest_Community)
Forest_Dist <- dist(Forest_Distance)
Forest_Edge_Interior_Com <- vegdist(Forest_Interior_Edge_Community)
Forest_Edge_Interior_Dist <- dist(Forest_Interior_Edge_Distance)
Forest_Edge_Exterior_Com <- vegdist(Forest_Exterior_Edge_Community)
Forest_Edge_Exterior_Dist <- dist(Forest_Exterior_Edge_Distance)
Pioneer_Near_Com <- vegdist(Pioneer_Near_Community)
Pioneer_Near_Dist <- dist(Pioneer_Near_Distance)
Pioneer_Far_Com <- vegdist(Pioneer_Far_Community)
Pioneer_Far_Dist <- dist(Pioneer_Far_Distance)
Grass_Near_Com <- vegdist(Grass_Near_Community)
Grass_Near_Dist <- dist(Grass_Near_Distance)
Grass_Far_Com <- vegdist(Grass_Far_Community)
Grass_Far_Dist <-  dist(Grass_Far_Distance)


#Define Plots
Forest <- ggplot() + geom_point(aes(x = Forest_Dist, y = Forest_Com)) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +ggtitle("Forest")
Forest_Edge_Interior <- ggplot() + geom_point(aes(x = Forest_Edge_Interior_Dist, y = Forest_Edge_Interior_Com))+ theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + ggtitle("Forest Edge Interior")
Forest_Edge_Exterior <- ggplot() + geom_point(aes(x = Forest_Edge_Exterior_Dist, y = Forest_Edge_Exterior_Com)) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + ggtitle("Forest Edge Exterior")
Pioneer_Near <- ggplot() + geom_point(aes(x = Pioneer_Near_Dist, y = Pioneer_Near_Com)) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + ggtitle("Pioneer Near")
Pioneer_Far <- ggplot() + geom_point(aes(x = Pioneer_Far_Dist, y = Pioneer_Far_Com)) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + ggtitle("Pioneer Far")
Grass_Near <- ggplot() + geom_point(aes(x = Grass_Near_Dist, y = Grass_Near_Com )) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + ggtitle("Grass Near")
Grass_Far <- ggplot() + geom_point(aes(x = Grass_Far_Dist, y = Grass_Far_Com )) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + ggtitle("Grass Far")

par(mfrow = c(2,4))
Forest + plot_spacer() + plot_spacer() +  Forest_Edge_Interior + Forest_Edge_Exterior + plot_spacer() +  Pioneer_Near + Pioneer_Far + plot_spacer() + Grass_Near + Grass_Far + plot_layout(ncol = 3) 


#Plot all figures into one panel
pdf(file = "Outputs/Figures/Sample_Distance_vs_Com.pdf")
plot(Forest_Dist ~ Forest_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
plot(Forest_Edge_Interior_Dist ~ Forest_Edge_Interior_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
plot(Forest_Edge_Exterior_Dist ~ Forest_Edge_Exterior_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
plot(Pioneer_Near_Dist ~ Pioneer_Near_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
plot(Pioneer_Far_Dist ~ Pioneer_Far_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
plot(Grass_Near_Dist ~ Grass_Near_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
plot(Grass_Far_Dist ~ Grass_Far_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
dev.off()

margin(1,1,1,5)
par(mfrow=c(2,4))
plot(Forest_Dist ~ Forest_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity") + plot_spacer()
plot_spacer()
plot(Forest_Edge_Interior_Dist ~ Forest_Edge_Interior_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
plot(Forest_Edge_Exterior_Dist ~ Forest_Edge_Exterior_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
plot(Pioneer_Near_Dist ~ Pioneer_Near_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
plot(Pioneer_Far_Dist ~ Pioneer_Far_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
plot(Grass_Near_Dist ~ Grass_Near_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
plot(Grass_Far_Dist ~ Grass_Far_Com, ylab = "Community Dissimilarity", xlab = "Spatial Dissimilarity")
