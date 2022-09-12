rm(list=ls())
library(ade4)
library(vegan)
#Import all datasets and clean them up 
#AGV
AGV <- read.csv("Cleaned Up Data/AGV_Data.csv")
AGV <- as.data.frame(AGV[,c(2:4)], row.names = AGV$X)

#Abiotic
Abiotic <- read.csv("Cleaned Up Data/Abiotc_Variables_MC.csv")
Abiotic <- as.data.frame(Abiotic[,c(3:4)], row.names = Abiotic$X)

#Soil Nutrients 
Soil_Nutrients <- read.csv("Cleaned Up Data/Soil_Nutrients_MC.csv")
#Assign row names, we can use the same as Abiotic 
rownames(Soil_Nutrients) <- rownames(Abiotic)

#Leaf Litter 
Litter <- read.csv("Cleaned Up Data/Litter_MC.csv")
Litter <- as.data.frame(Litter[,c(2,3,4)], row.names = Litter$X)

#Spatial Data 
Distance <-read.csv("Raw_Data/Plot_Coordinates_MC.csv")
#Drop Transect column and plot, assign rownames as plot 
Distance <- as.data.frame(Distance[,c(3,4)], row.names = Distance$Plot)
#There is a typo in this dataframe, we will correct it here 
Distance[20,2] <- 152.8767

#Fungi by plot 
Community_By_PLot <- read.csv("Cleaned Up Data/Community_Fungi_By_Plot.csv")
#Assign row names and delete first two rows
Community_By_PLot <- as.data.frame(Community_By_PLot[,c(2:5138)], row.names = Community_By_PLot$Category)
#Remove UMNR data 
Community_By_PLot <- as.data.frame(Community_By_PLot[c(1:21),])

#Convert each object to a distance object in order to perform Mantel Test - use function dist for environmental dat and veg dist for community data 
AGV_Dist <- dist(AGV)
Abiotc_Dist <- dist(Abiotic)
Soil_Nutrients_Dist <- dist(Soil_Nutrients)
Litter_Dist <- dist(Litter)
Distance_Dist <- dist(Distance)
Community_Dist <- vegdist(Community_By_PLot)

#Test the correlation of each environmental matrix with the community matrix 
#AGV 
Community_vs_AGV <-  mantel.rtest(Community_Dist, AGV_Dist, nrepet = 10000)
#Abiotic
Communityt_vs_Abiotic <- mantel.rtest(Community_Dist, Abiotc_Dist, nrepet = 10000)
#Soil Nutrients 
Community_vs_Soil_Nutrients <- mantel.rtest(Community_Dist, Soil_Nutrients_Dist, nrepet = 10000)
#Litter 
Community_vs_Litter <- mantel.rtest(Community_Dist, Litter_Dist, nrepet = 10000)
#Distance 
Community_vs_Distance <- mantel.rtest(Community_Dist, Distance_Dist, nrepet = 10000)


#Repeat for Guilds 
#Import data and clean up
Guilds <- read.csv("Cleaned Up Data/Guilds_Condensed.csv")
Guilds <- as.data.frame(Guilds[,c(2:6)], row.names = Guilds$X)
#Add plot information 
Guilds$Plot <- c("Forest_1", "Forest_1", "Forest_2", "Forest_2", "Forest_3", "Forest_3", "Forest_Edge_Interior_1", "Forest_Edge_Interior_1", "Forest_Edge_Interior_2", "Forest_Edge_Interior_2", "Forest_Edge_Interior_3", "Forest_Edge_Interior_3", "Forest_Edge_Exterior_1", "Forest_Edge_Exterior_1", "Forest_Edge_Exterior_2", "Forest_Edge_Exterior_2", "Forest_Edge_Exterior_3", "Forest_Edge_Exterior_3", "Pioneer_Near_1", "Pioneer_Near_1", "Pioneer_Near_2", "Pioneer_Near_2", "Pioneer_Near_3", "Pioneer_Near_3", "Pioneer_Far_1", "Pioneer_Far_1", "Pioneer_Far_2", "Pioneer_Far_2", "Pioneer_Far_3", "Pioneer_Far_3", "Grass_Near_1", "Grass_Near_1", "Grass_Near_2", "Grass_Near_2", "Grass_Near_3", "Grass_Near_3", "Grass_Far_1", "Grass_Far_2", "Grass_Far_2", "Grass_Far_3", "Grass_Far_3", "UMNR_1", "UMNR_1", "UMNR_2", "UMNR_2", "UMNR_3", "UMNR_3")
#Store rownames as plot
Guilds <- as.data.frame(Guilds[,c(1:5)], row.names = Guilds$Plot)
#Store plots as a list 
Plot <- list(rownames(Guilds))
#Aggregate to Plot Level - find sum of each guild per plot
Guilds_Plots <- aggregate(Guilds, by = Plot, FUN = sum)
#Assign rownames, remove UMNR data 
Guilds_Plots <- as.data.frame(Guilds_Plots[c(1:21),])
Guilds_Plots <- as.data.frame(Guilds_Plots[,c(2:6)], row.names = Guilds_Plots$Group.1)
#Reorder so row names match other dataframes 
Guilds_Plots <- as.data.frame(Guilds_Plots[c(1,2,3,7,8,9,4,5,6,19,20,21,16,17,18,13,14,15,10,11,12),])
#Save File 
write.csv(Guilds_Plots, file = "Cleaned Up Data/Guilds_by_Plots_MC.csv")

#Convert guilds to a distance object 
Guilds_Dist <- vegdist(Guilds_Plots)

#Test the correlation of each environmental matrix with the Guilds matrix 
#AG 
Guilds_vs_AGV <-  mantel.rtest(Guilds_Dist, AGV_Dist, nrepet = 10000)
#Abiotic
Guilds_vs_Abiotic <- mantel.rtest(Guilds_Dist, Abiotc_Dist, nrepet = 10000)
#Soil Nutrients 
Guilds_vs_Soil_Nutrients <- mantel.rtest(Guilds_Dist, Soil_Nutrients_Dist, nrepet = 10000)
#Litter 
Guilds_vs_Litter <- mantel.rtest(Guilds_Dist, Litter_Dist, nrepet = 10000)
#Distance 
Guilds_vs_Distance <- mantel.rtest(Guilds_Dist, Distance_Dist, nrepet = 10000)
  
  

