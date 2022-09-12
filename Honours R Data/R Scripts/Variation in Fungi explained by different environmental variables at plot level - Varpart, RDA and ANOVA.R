rm(list=ls())
library(tidyverse)
library(dplyr)
library(vegan)

#Import Woody Richness 
Woody_Richness <- read.csv("Raw_Data/Mature_Woody_Species_Count.csv")
#Create a column of unique counts per row called Species_Richness - this represent woody species richness only 
Species_Richness <- apply(Woody_Richness, 1, function(x)length(unique(x)))
#Attach column to woody_count
Species_Richness <- cbind(Woody_Richness, Species_Richness)
#Clean up data frame, select only columns containing species richness and plot and remove NA's
Species_Richness <- as.data.frame(Species_Richness[,c(2,47)])
Species_Richness <- as.data.frame(Species_Richness[c(1:24),])
Species_Richness <- as.data.frame(Species_Richness, row.names = Species_Richness$Plot)

#Import Grass and Herb Data 
Grass <- read.csv("Raw_Data/Grass_Other_Herb_Cover.csv")
#Remove other herb cover - grass cover will be enough to represent non-woody cover 
Grass <- as.data.frame(Grass[,c(2,3)], row.names = Grass$Plot)

#Import Woody DBH data 
Woody_Stem <- read.csv("Raw_Data/Median_DBH_by_Plot.csv")
#remove transect column and assing plot as row names 
Woody_Stem <- as.data.frame(Woody_Stem[,c(2,3)], row.names = Woody_Stem$Plot)

#Create a data frame containing all variables relating to the above ground vegetation (AGV)
AGV_Data <- left_join(Species_Richness, Woody_Stem, by = 'Plot')
AGV_Data <- left_join(AGV_Data, Grass, by = 'Plot')
AGV_Data <- as.data.frame(AGV_Data[,c(2:4)], row.names = AGV_Data$Plot)
#Remove UMNR data, we are only interested in comparing sites at Mary Cairncross
AGV_Data <- as.data.frame(AGV_Data[c(1:21),])
write.csv(AGV_Data, file = "Cleaned Up Data/AGV_Data.csv")

#Import community data for fungi aggregated to plot
Fungi_by_Plot <- read.csv("Cleaned Up Data/Community_Fungi_By_Plot.csv")
#Remove first column and make sort row order so they are the same as the AGV dataframee 
Fungi_by_Plot <- as.data.frame(Fungi_by_Plot[,c(2:5138)], row.names = Fungi_by_Plot$X)
#Remove UMNR Data
Fungi_by_Plot_MC <- as.data.frame(Fungi_by_Plot[c(1:21), ])

#Assess the variance contributed by each variable in AGV_Data to the variance in Fungi_by_Plot_MC using the function varpart
Varpart_AGV <- varpart(Fungi_by_Plot_MC, AGV_Data$Species_Richness, AGV_Data$Median_DBH.cm., AGV_Data$Grass_Cover_., transfo = "log")
#Call object
Varpart_AGV
#plot varpart 
plot(Varpart_AGV, bg = c("Red", "Blue", "Green"))
#Variable names are in the order we put them in, we will of course add a legend if we want to plot
#This was just a quick visualization, we will dig into these variables later using mixed effects models 

#Import soil nutrient data and aggregate to plot level (find mean of each plot)
Soil_Nutrients <- read.csv("Raw_Data/Soil_Nutrients.csv") #This contains only MC data, we don't need to remove UMNR
Soil_Nutrients <- as.data.frame(Soil_Nutrients[c(1:41),c(2,4,5,6)])
Plot <- list(Soil_Nutrients$Plot)
Soil_Nutrients <- as.data.frame(Soil_Nutrients[,c(2:4)], row.names = Soil_Nutrients$Plot)
Soil_Nutrients <- aggregate(Soil_Nutrients, by = Plot, FUN = mean)
names(Soil_Nutrients) <- c("Plot", "TOC", "TN", "TP")
#make plots the row names 
Soil_Nutrients <- as.data.frame(Soil_Nutrients[,c(2:4)], row.names = Soil_Nutrients$Plot)
#reorder rows
Soil_Nutrients<- as.data.frame(Soil_Nutrients[c(1,2,3,7,8,9,4,5,6,19,20,21,16,17,18,13,14,15,10,11,12),])

#Assess the variance contributed by each variable in Soil Nutrients to the variance in Fungi_by_Plot_MC using the function varpart
Varpart_Soil <- varpart(Fungi_by_Plot_MC, Soil_Nutrients$TOC, Soil_Nutrients$TN, Soil_Nutrients$TP, transfo = "log")
#Call object
Varpart_Soil
#plot varpart 
plot(Varpart_Soil, bg = c("Red", "Blue", "Green"))
#Variable names are in the order we put them in, we will of course add a legend if we want to plot
#This was just a quick visualization, we will dig into these variables later using mixed effects models

#Import data on soil water content, compaction and photosynthetic available radiation (PAR), join into one data frame called Abiotic
#Import PAR
PAR <- read.csv("Raw_Data/PAR.csv")
#Convert data to proportion of ambient reading (1639.5, E-W orientation, flat in exposed pasture)
PAR$Relative_PAR <- PAR$PAR/1649.5
#Only keep Relative_Par and Plot 
PAR <- as.data.frame(PAR[,c(2,4)], row.names = PAR$Plot )

#Import soil compaction
Soil_Compaction <- read.csv("Raw_Data/Penetrometer_Data.csv")
Soil_Compaction <- as.data.frame(Soil_Compaction[,c(2,7)], row.names = Soil_Compaction$Plot)
#Rename columns 
names(Soil_Compaction) <- c("Plot", "Mean Compaction")

#Import Water content data 
Water <- read.csv("Raw_Data/Soil_Weight.csv")
Water <- as.data.frame(Water[,c(2,7)], row.names = Water$Plot)
 

#Combine into one dataframe 
Abiotic<-left_join(Soil_Compaction, Water, by = 'Plot')
Abiotic <- left_join(Abiotic, PAR, by = "Plot")
#Remove UMNR 
Abiotic <- as.data.frame(Abiotic[c(1:21),])
#Make plot names row names 
Abiotic <- as.data.frame(Abiotic[,c(2:4)], row.names = Abiotic$Plot)
#Save file
write.csv(Abiotic, file = "Cleaned Up Data/Abiotc_Variables_MC.csv")

#Assess the variance contributed by each variable in Abiotic to the variance in Fungi_by_Plot_MC using the function varpart
Varpart_Abiotic<- varpart(Fungi_by_Plot_MC, Abiotic$`Mean Compaction`, Abiotic$Water_Content, Abiotic$Relative_PAR, transfo = "log")
#Call object
Varpart_Abiotic
#plot varpart 
plot(Varpart_Abiotic, bg = c("Red", "Blue", "Green"))
#Variable names are in the order we put them in, we will of course add a legend if we want to plot
#This was just a quick visualization, we will dig into these variables later using mixed effects model


#Import data for leaf litter N and P and Extract Columns for Total N, P and Organic N Total 
Leaf_Litter_Nutrients <- read.csv("Raw_Data/Leaf_Litter_N_P .csv")
Plot <- list(Leaf_Litter_Nutrients$Plot)
Leaf_Litter_Nutrients <- as.data.frame(Leaf_Litter_Nutrients[,c(3,4,7)], row.names = Leaf_Litter_Nutrients$Plot)
#Aggregate data to plot level, take the mean of each plot 
Leaf_Litter_Nutrients <- aggregate(Leaf_Litter_Nutrients, by = Plot, FUN = mean)
#assing plot as rownames 
Leaf_Litter_Nutrients <- as.data.frame(Leaf_Litter_Nutrients[,c(2:4)], row.names = Leaf_Litter_Nutrients$Group.1)
#reorder rows to match other data frames
Leaf_Litter_Nutrients <- as.data.frame(Leaf_Litter_Nutrients[c(1,2,3,7,8,9,4,5,6,19,20,21,16,17,18,13,14,15,10,11,12,22,23,24),])
#Remove UMNR
Leaf_Litter_Nutrients <- as.data.frame(Leaf_Litter_Nutrients[c(1:21),])
#rename columns to remove confusing decimal points and commas 
names(Leaf_Litter_Nutrients) <- c("Total N", "Total P", "Total Organic N")

#Assess the variance contributed by each variable in Abiotic to the variance in Fungi_by_Plot_MC using the function varpart
Varpart_Litter<- varpart(Fungi_by_Plot_MC, Leaf_Litter_Nutrients$`Total N`, Leaf_Litter_Nutrients$`Total P`, Leaf_Litter_Nutrients$`Total Organic N`, transfo = "log")
#Call object
Varpart_Litter
#plot varpart 
plot(Varpart_Litter, bg = c("Red", "Blue", "Green"))
#Variable names are in the order we put them in, we will of course add a legend if we want to plot
#This was just a quick visualization, we will dig into these variables later using mixed effects model


#Import Spatial Data 
Distance <-read.csv("Raw_Data/Plot_Coordinates_MC.csv")
#Drop Transect column and plot, assign rownames as plot 
Distance <- as.data.frame(Distance[,c(3,4)], row.names = Distance$Plot)
#There is a typo in this dataframe, we will correct it here 
Distance[20,2] <- 152.8767

#Assess the variance contributed by each lat vs long in distance to the variance in Fungi_by_Plot_MC using the function varpart
Varpart_Distance<- varpart(Fungi_by_Plot_MC, Distance$Lat, Distance$Long, transfo = "log")
#Call object
Varpart_Distance
#plot varpart 
plot(Varpart_Distance, bg = c("Red", "Blue"))
#Variable names are in the order we put them in, we will of course add a legend if we want to plot
#This was just a quick visualization, we will dig into these variables later using mixed effects model



#Repeat the process for functional guild instead of OTU's 
#Import Guild Data
Guilds <- read.csv("Cleaned Up Data/Guilds_Condensed.csv")
#Add Plot information
Plot <- c("Forest_1", "Forest_1", "Forest_2", "Forest_2", "Forest_3", "Forest_3", "Forest_Edge_Interior_1", "Forest_Edge_Interior_1", "Forest_Edge_Interior_2", "Forest_Edge_Interior_2", "Forest_Edge_Interior_3", "Forest_Edge_Interior_3", "Forest_Edge_Exterior_1", "Forest_Edge_Exterior_1", "Forest_Edge_Exterior_2", "Forest_Edge_Exterior_2", "Forest_Edge_Exterior_3", "Forest_Edge_Exterior_3", "Pioneer_Near_1", "Pioneer_Near_1", "Pioneer_Near_2", "Pioneer_Near_2", "Pioneer_Near_3", "Pioneer_Near_3", "Pioneer_Far_1", "Pioneer_Far_1", "Pioneer_Far_2", "Pioneer_Far_2", "Pioneer_Far_3", "Pioneer_Far_3", "Grass_Near_1", "Grass_Near_1", "Grass_Near_2", "Grass_Near_2", "Grass_Near_3", "Grass_Near_3", "UMNR_1", "UMNR_1", "UMNR_2", "UMNR_2", "UMNR_3", "UMNR_3", "Grass_Far_1", "Grass_Far_2", "Grass_Far_2", "Grass_Far_3", "Grass_Far_3")
Guilds<- as.data.frame(Guilds[,c(2:6)], row.names = Plot)
#aggregate to plot level, first store plot as a list
Plot <- list(Plot)
#Aggregate, find the sum of each guild/plot
Guilds <- aggregate(Guilds, by = Plot, FUN = sum)
#Reorder rows after we aggregate to match other data frames 
Guilds <- as.data.frame(Guilds[c(1,2,3,7,8,9,4,5,6,19,20,21,16,17,18,13,14,15,10,11,12,22,23,24),])
#Assign plots as row names 
Guilds <- as.data.frame(Guilds[,c(2:6)], row.names = Guilds$Group.1)
#Save file
write.csv(Guilds,"Cleaned Up Data/Guild_Data_by_Plot.csv")
#Remove UMNR data 
Guilds_MC<- as.data.frame(Guilds[c(1:21),])


#Assess the variance contributed by each variable in AGV_Data to the variance in Guilds_MC using the function varpart
Guilds_AGV <- varpart(Guilds_MC, AGV_Data$Species_Richness, AGV_Data$Median_DBH.cm., AGV_Data$Grass_Cover_., transfo = "log")
#Call object
Guilds_AGV
#plot varpart 
plot(Guilds_AGV, bg = c("Red", "Blue", "Green"))
#Variable names are in the order we put them in, we will of course add a legend if we want to plot
#This was just a quick visualization, we will dig into these variables later using mixed effects models 

#Assess the variance contributed by each variable in Abiotic to the variance in Guilds_MC using the function varpart
Guilds_Abiotic <- varpart(Guilds_MC, Abiotic$`Mean Compaction`, Abiotic$Water_Content, Abiotic$Relative_PAR, transfo = "log")
#Call object
Guilds_Abiotic
#plot varpart 
plot(Guilds_Abiotic, bg = c("Red", "Blue", "Green"))
#Variable names are in the order we put them in, we will of course add a legend if we want to plot
#This was just a quick visualization, we will dig into these variables later using mixed effects models 

#Assess the variance contributed by each variable in Soil Nutrients to the variance in Guilds_MC using the function varpart
Guilds_Soil <- varpart(Guilds_MC, Soil_Nutrients$TOC, Soil_Nutrients$TN, Soil_Nutrients$TP, transfo = "log")
#Call object
Guilds_Soil
#plot varpart 
plot(Guilds_Soil, bg = c("Red", "Blue", "Green"))
#Variable names are in the order we put them in, we will of course add a legend if we want to plot
#This was just a quick visualization, we will dig into these variables later using mixed effects models 

#Assess the variance contributed by each variable in Leaf Litter  to the variance in Guilds_MC using the function varpart
Guilds_Litter <- varpart(Guilds_MC, Leaf_Litter_Nutrients$`Total N`, Leaf_Litter_Nutrients$`Total P`, Leaf_Litter_Nutrients$`Total Organic N`, transfo  = "log")
#Call object
Guilds_Litter
#plot varpart 
plot(Guilds_Litter, bg = c("Red", "Blue", "Green"))
#Variable names are in the order we put them in, we will of course add a legend if we want to plot
#This was just a quick visualization, we will dig into these variables later using mixed effects models 

#Assess the variance contributed by lat and long in Distnace  to the variance in Guilds_MC using the function varpart
Guilds_Distance <- varpart(Guilds_MC, Distance$Lat, Distance$Long, transfo  = "log")
#Call object
Guilds_Distance
#plot varpart 
plot(Guilds_Distance, bg = c("Red", "Blue"))
#Variable names are in the order we put them in, we will of course add a legend if we want to plot
#This was just a quick visualization, we will dig into these variables later using mixed effects models 

#perform an RDA analysis to assess the significance of each data frame
#Test AGV -  
AGV_Comm_Rda <- rda(decostand(Fungi_by_Plot_MC, method = 'log'), AGV_Data)
anova(AGV_Comm_Rda)
#Abiotic - 
Abiotic_Comm_Rda <- rda(decostand(Fungi_by_Plot_MC, method = 'log'), Abiotic)
anova(Abiotic_Comm_Rda)
Soil_Nutrients_RDA <- rda(decostand(Fungi_by_Plot_MC, method = 'log'), Soil_Nutrients)
anova(Soil_Nutrients_RDA)
#Leaf Litter 
Leaf_Comm_Rda <- rda(decostand(Fungi_by_Plot_MC, method = 'log'), Leaf_Litter_Nutrients)
anova(Leaf_Comm_Rda)
#Distance 
Distance_Comm_Rda <- rda(decostand(Fungi_by_Plot_MC, method = 'log'), Distance)
anova(Distance_Comm_Rda)

#Guild 
#AGV
AGV_Guild_Rda <- rda(decostand(Guilds_MC, method = 'log'), AGV_Data)
anova(AGV_Guild_Rda) 
#Abiotic - Community 
Abiotic_Guild_Rda <- rda(decostand(Guilds_MC, method = 'log'), Abiotic)
anova(Abiotic_Guild_Rda)
Soil_Nutrients_Guild_RDA <- rda(decostand(Guilds_MC, method = 'log'), Soil_Nutrients)
anova(Soil_Nutrients_Guild_RDA)
#Leaf Litter 
Leaf_Guild_Rda <- rda(decostand(Guilds_MC, method = 'log'), Leaf_Litter_Nutrients)
anova(Leaf_Guild_Rda)
#Distance 
Distance_Guild_Rda <- rda(decostand(Guilds_MC, method = 'log'), Distance)
anova(Distance_Guild_Rda)




