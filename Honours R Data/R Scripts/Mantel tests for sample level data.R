rm(list=ls())
library(tidyverse)
library(dplyr)
library(ade4) 
library(vegan)
#Import Community Data
Community <- read.csv("Cleaned Up Data/Fungi_by_Sample.csv")
#Assign row names and remove first column 
Community <- as.data.frame(Community[,c(2:5138)], row.names = Community$X)
#Reorder rows and remove UMNR 
#Remove UMNR rows 
Community<- as.data.frame(Community[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,43,44,45,46,47),])

#Import sample level data - soil and litter nutrients + spatial coordiantes 
#Soil
Soil_Nutrients <- read.csv("Raw_Data/Soil_Nutrients.csv") #This contains only MC data, we don't need to remove UMNR
Soil_Nutrients <- as.data.frame(Soil_Nutrients[c(1:41),c(3:6)])
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

#Convert each dataframe to object of class distance 
#Community - use vegdist, bray method
Community_Whole_Dist <- vegdist(Community)
Forest_Community_Dist <- vegdist(Forest_Community)
Forest_Interior_Edge_Community_Dist <- vegdist(Forest_Interior_Edge_Community)
Forest_Exterior_Edge_Community_Dist <- vegdist(Forest_Exterior_Edge_Community)
Pioneer_Near_Community_Dist <- vegdist(Pioneer_Near_Community)
Pioneer_Far_Community_Dist <- vegdist(Pioneer_Far_Community)
Grass_Near_Community_Dist <- vegdist(Grass_Near_Community)
Grass_Far_Community_Dist <- vegdist(Grass_Far_Community)

#Guild - us evegdist, bray method 
Guilds_Whole_Dist <- vegdist(Guild)
Forest_Guilds_Dist <- vegdist(Forest_Guilds)
Forest_Interior_Edge_Guilds_Dist <- vegdist(Forest_Interior_Edge_Guilds)
Forest_Exterior_Edge_Guilds_Dist <- vegdist(Forest_Exterior_Edge_Guilds)
Pioneer_Near_Guilds_Dist <- vegdist((Pioneer_Near_Guilds))
Pioneer_Far_Guilds_Dist <- vegdist(Pioneer_Far_Guilds)
Grass_Near_Guilds_Dist <- vegdist(Grass_Near_Guilds)
Grass_Far_Guilds_Dist<- vegdist(Grass_Far_Guilds)

#Distance- use function dist from ade4 package, euclidean distances 
Forest_Distance_Dist <- dist(Forest_Distance)
Forest_Interior_Edge_Distance_Dist <- dist(Forest_Interior_Edge_Distance)
Forest_Exterior_Edge_Distance_Dist <- dist(Forest_Exterior_Edge_Distance)
Pioneer_Near_Distance_Dist <- dist(Pioneer_Near_Distance)
Pioneer_Far_Distance_Dist <- dist(Pioneer_Far_Distance)
Grass_Near_Distance_Dist <- dist(Grass_Near_Distance)
Grass_Far_Distance_Dist <- dist(Grass_Far_Distance)

#Soil 
Forest_Soil_Dist <- dist(Forest_Soil)
Forest_Interior_Edge_Soil_Dist <- dist(Forest_Interior_Edge_Soil)
Forest_Exterior_Edge_Soil_Dist <- dist(Forest_Exterior_Edge_Soil)
Pioneer_Near_Soil_Dist <- dist(Pioneer_Near_Soil)
Pioneer_Far_Soil_Dist <- dist(Pioneer_Far_Soil)
Grass_Near_Soil_Dist <- dist(Grass_Near_Soil)
Grass_Far_Soil_Dist <- dist(Grass_Far_Soil)

#Litter
Forest_Litter_Dist <- dist(Forest_Litter)
Forest_Interior_Edge_Litter_Dist <- dist(Forest_Interior_Edge_Litter)
Forest_Exterior_Edge_Litter_Dist <- dist(Forest_Exterior_Edge_Litter)
Pioneer_Near_Litter_Dist <- dist(Pioneer_Near_Litter)
Pioneer_Far_Litter_Dist <- dist(Pioneer_Far_Litter)
Grass_Near_Litter_Dist <- dist(Grass_Near_Litter)
Grass_Far_Litter_Dist <- dist(Grass_Far_Litter)


#Test the correlation of soil, leaf and space with the community at each transect 
#Forest 
Mantel_Forest_Soil <- mantel.rtest(Forest_Community_Dist, Forest_Soil_Dist, nrepet = 10000)
Mantel_Forest_Leaf <- mantel.rtest(Forest_Community_Dist, Forest_Litter_Dist, nrepet = 10000)
Mantel_Forest_Distance <- mantel.rtest(Forest_Community_Dist, Forest_Distance_Dist, nrepet = 10000)
#FEI
Mantel_Forest_Interior_Soil <- mantel.rtest(Forest_Interior_Edge_Community_Dist, Forest_Interior_Edge_Soil_Dist, nrepet = 10000)
Mantel_Forest_Interior_Leaf <- mantel.rtest(Forest_Interior_Edge_Community_Dist, Forest_Interior_Edge_Litter_Dist, nrepet = 10000)
Mantel_Forest_Interior_Distance <- mantel.rtest(Forest_Interior_Edge_Community_Dist, Forest_Interior_Edge_Distance_Dist, nrepet = 10000)
#FEE
Mantel_Forest_Exterior_Soil <- mantel.rtest(Forest_Exterior_Edge_Community_Dist, Forest_Exterior_Edge_Soil_Dist, nrepet = 10000)
Mantel_Forest_Exterior_Leaf <- mantel.rtest(Forest_Exterior_Edge_Community_Dist, Forest_Exterior_Edge_Litter_Dist, nrepet = 10000)
Mantel_Forest_Exterior_Distance <- mantel.rtest(Forest_Exterior_Edge_Community_Dist, Forest_Exterior_Edge_Distance_Dist, nrepet = 10000)
#PN
Mantel_Pioneer_Near_Soil <- mantel.rtest(Pioneer_Near_Community_Dist, Pioneer_Near_Soil_Dist, nrepet = 10000)
Mantel_Pioneer_Near_Leaf <- mantel.rtest(Pioneer_Near_Community_Dist, Pioneer_Near_Litter_Dist, nrepet = 10000)
Mantel_Pioneer_Near_Distance <- mantel.rtest(Pioneer_Near_Community_Dist, Pioneer_Near_Distance_Dist, nrepet = 10000)
#PF
Mantel_Pioneer_Far_Soil <- mantel.rtest(Pioneer_Far_Community_Dist, Pioneer_Far_Soil_Dist, nrepet = 10000)
Mantel_Pioneer_Far_Leaf <- mantel.rtest(Pioneer_Far_Community_Dist, Pioneer_Far_Litter_Dist, nrepet = 10000)
Mantel_Pioneer_Far_Distance <- mantel.rtest(Pioneer_Far_Community_Dist, Pioneer_Far_Distance_Dist, nrepet = 10000)
#GN
Mantel_Grass_Near_Soil <- mantel.rtest(Grass_Near_Community_Dist, Grass_Near_Soil_Dist, nrepet = 10000)
Mantel_Grass_Near_Leaf <- mantel.rtest(Grass_Near_Community_Dist, Grass_Near_Litter_Dist, nrepet = 10000)
Mantel_Grass_Near_Distance <- mantel.rtest(Grass_Near_Community_Dist, Grass_Near_Distance_Dist, nrepet = 10000)
#GF
Mantel_Grass_Far_Soil <- mantel.rtest(Grass_Far_Community_Dist, Grass_Far_Soil_Dist, nrepet = 10000)
Mantel_Grass_Far_Leaf <- mantel.rtest(Grass_Far_Community_Dist, Grass_Far_Litter_Dist, nrepet = 10000)
Mantel_Grass_Far_Distance <- mantel.rtest(Grass_Far_Community_Dist, Grass_Far_Distance_Dist, nrepet = 10000)

#Repeat for guilds 
#Test the correlation of soil, leaf and space with the community at each transect 
#Forest 
Mantel_Forest_Soil_Guilds <- mantel.rtest(Forest_Guilds_Dist, Forest_Soil_Dist, nrepet = 10000)
Mantel_Forest_Leaf_Guilds <- mantel.rtest(Forest_Guilds_Dist, Forest_Litter_Dist, nrepet = 10000)
Mantel_Forest_Distance_Guilds <- mantel.rtest(Forest_Guilds_Dist, Forest_Distance_Dist, nrepet = 10000)
#FEI
Mantel_Forest_Interior_Soil_Guilds <- mantel.rtest(Forest_Interior_Edge_Guilds_Dist, Forest_Interior_Edge_Soil_Dist, nrepet = 10000)
Mantel_Forest_Interior_Leaf_Guilds <- mantel.rtest(Forest_Interior_Edge_Guilds_Dist, Forest_Interior_Edge_Litter_Dist, nrepet = 10000)
Mantel_Forest_Interior_Distance_Guilds <- mantel.rtest(Forest_Interior_Edge_Guilds_Dist, Forest_Interior_Edge_Distance_Dist, nrepet = 10000)
#FEE
Mantel_Forest_Exterior_Soil_Guilds <- mantel.rtest(Forest_Exterior_Edge_Guilds_Dist, Forest_Exterior_Edge_Soil_Dist, nrepet = 10000)
Mantel_Forest_Exterior_Leaf_Guilds <- mantel.rtest(Forest_Exterior_Edge_Guilds_Dist, Forest_Exterior_Edge_Litter_Dist, nrepet = 10000)
Mantel_Forest_Exterior_Distance_Guilds <- mantel.rtest(Forest_Exterior_Edge_Guilds_Dist, Forest_Exterior_Edge_Distance_Dist, nrepet = 10000)
#PN
Mantel_Pioneer_Near_Soil_Guilds <- mantel.rtest(Pioneer_Near_Guilds_Dist, Pioneer_Near_Soil_Dist, nrepet = 10000)
Mantel_Pioneer_Near_Leaf_Guilds <- mantel.rtest(Pioneer_Near_Guilds_Dist, Pioneer_Near_Litter_Dist, nrepet = 10000)
Mantel_Pioneer_Near_Distance_Guilds <- mantel.rtest(Pioneer_Near_Guilds_Dist, Pioneer_Near_Distance_Dist, nrepet = 10000)
#PF
Mantel_Pioneer_Far_Soil_Guilds <- mantel.rtest(Pioneer_Far_Guilds_Dist, Pioneer_Far_Soil_Dist, nrepet = 10000)
Mantel_Pioneer_Far_Leaf_Guilds <- mantel.rtest(Pioneer_Far_Guilds_Dist, Pioneer_Far_Litter_Dist, nrepet = 10000)
Mantel_Pioneer_Far_Distance_Guilds <- mantel.rtest(Pioneer_Far_Guilds_Dist, Pioneer_Far_Distance_Dist, nrepet = 10000)
#GN
Mantel_Grass_Near_Soil_Guilds <- mantel.rtest(Grass_Near_Guilds_Dist, Grass_Near_Soil_Dist, nrepet = 10000)
Mantel_Grass_Near_Leaf_Guilds <- mantel.rtest(Grass_Near_Guilds_Dist, Grass_Near_Litter_Dist, nrepet = 10000)
Mantel_Grass_Near_Distance_Guilds <- mantel.rtest(Grass_Near_Guilds_Dist, Grass_Near_Distance_Dist, nrepet = 10000)
#GF
Mantel_Grass_Far_Soil_Guilds <- mantel.rtest(Grass_Far_Guilds_Dist, Grass_Far_Soil_Dist, nrepet = 10000)
Mantel_Grass_Far_Leaf_Guilds <- mantel.rtest(Grass_Far_Guilds_Dist, Grass_Far_Litter_Dist, nrepet = 10000)
Mantel_Grass_Far_Distances_Guilds <- mantel.rtest(Grass_Far_Guilds_Dist, Grass_Far_Distance_Dist, nrepet = 10000)











