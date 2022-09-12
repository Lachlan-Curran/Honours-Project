rm(list=ls())
library(vegan)
library(ade4)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
Distance_From_Edge <- read.csv("Raw_Data/Distance_edge.csv")
Distace_From_Edge <- as.data.frame(Distance_From_Edge[,c(2)], row.names = Distance_From_Edge$Transect)
Transect <- list(Distance_From_Edge$Transect)
Distance_From_Edge$Transect <- NULL
Distance_From_Edge <- aggregate(Distance_From_Edge, by = Transect, FUN = mean)
#Fungi by plot - reorder rows to be the same as other data frames and then subset MC data  
Community_By_Plot <- read.csv("Cleaned Up Data/Community_Fungi_By_Plot.csv")
Community_By_Plot <- as.data.frame(Community_By_Plot[,c(3:5138)], row.names = Community_By_Plot$Category)
Community_By_Plot <- as.data.frame(Community_By_Plot[c(1,2,3,7,8,9,4,5,6,19,20,21,16,17,18,13,14,15,10,11,12,22,23,24),])
MC_Community <- as.data.frame(Community_By_Plot[c(1:21),])
MC_Community$Transect <- c(rep("Forest", 3), rep("Forest_Interior_Edge", 3), rep("Forest_Exterior_Edge", 3), rep("Pioneer_Near", 3), rep("Pioneer_Far", 3), rep("Grass_Near", 3), rep("Grass_Far",3))
Transect <- list(MC_Community$Transect)
MC_Community <- as.data.frame(MC_Community[,c(1:5136)], row.names = MC_Community$Transect)
MC_Community_by_Transect <- aggregate(MC_Community, by = Transect, FUN = sum)
MC_Community_by_Transect <- as.data.frame(MC_Community_by_Transect[,c(2:5137)])

#Guild Data 
Guilds <- read.csv("Cleaned Up Data/Guild_Data_by_Plot.csv")
Guilds <- as.data.frame(Guilds[,c(2:7)], row.names = Guilds$Group.1)
Guild_MC <- as.data.frame(Guilds[c(1:21),])
Guild_MC <- as.data.frame(Guild_MC[,c(2:6)])
Guild_MC$Transect <- c(rep("Forest", 3), rep("Forest_Interior_Edge", 3), rep("Forest_Exterior_Edge", 3), rep("Pioneer_Near", 3), rep("Pioneer_Far", 3), rep("Grass_Near", 3), rep("Grass_Far",3))
Guild_MC <- as.data.frame(Guild_MC[,1:5], row.names = Guild_MC$Transect)
Guild_MC_Transect <- aggregate(Guild_MC, by = Transect, FUN = sum)
Guild_MC_Transect <- as.data.frame(Guild_MC_Transect[,c(2:6)], row.names = Guild_MC_Transect$Group.1)

#Create distance objects of the dataframes
MC_Community_Dist <- vegdist(MC_Community_by_Transect)
GUILD_Dist <-vegdist(Guild_MC_Transect)
Edge_Dist <- dist(Distance_From_Edge)

#Mantel
edge_community <- mantel.rtest(MC_Community_Dist, Edge_Dist)
edge_guild <- mantel.rtest(GUILD_Dist, edge)
