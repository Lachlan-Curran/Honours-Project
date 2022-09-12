rm(list=ls())
library(tidyverse)
library(dplyr)
library(vegan)
library(ggplot2)
library(reshape2)
library(GGally)
library(vegan)  
#Guild Data 
General_Guild_Abundance<- read.csv("Cleaned Up Data/General_Guild_Abundance_by_Sample.csv")
Rel_Saprobe_Abundance <- (General_Guild_Abundance$Saprobe_Abundance/General_Guild_Abundance$Total_Abundance)*100
Rel_Endomycorrhizhae_Abundance <- (General_Guild_Abundance$Endomycorrhizhae/General_Guild_Abundance$Total_Abundance) * 100
Rel_Ectomycorrhizhae_Abundance <- (General_Guild_Abundance$Ectomycorrhizhae_Abundance/General_Guild_Abundance$Total_Abundance) * 100
Rel_Plant_Pathogen_Abundance <- (General_Guild_Abundance$Plant_Pathogen_Abundance/General_Guild_Abundance$Total_Abundance)*100
Rel_Other_Abundance <- (General_Guild_Abundance$Other_Abundance/General_Guild_Abundance$Total_Abundance)*100
Relative_Abundance_Guilds <- tibble(Rel_Saprobe_Abundance, Rel_Ectomycorrhizhae_Abundance, Rel_Endomycorrhizhae_Abundance, Rel_Plant_Pathogen_Abundance, Rel_Other_Abundance)
Relative_Abundance_Guilds_by_Sample  <- as.data.frame(Relative_Abundance_Guilds, row.names = Other_Abundance$Sample)
Relative_Abundance_Guilds_by_Sample$Sample <- General_Guild_Abundance$X
colnames(Relative_Abundance_Guilds_by_Sample) <- c("Saprobes", "Ectomycorrhizae", "Endomycorrhizae", "Plant Pathogens", "Other Guilds", "Sample")
Transect <- c(rep("Forest", 6), rep("Forest_Edge_Interior", 6), rep("Forest_Edge_Exterior", 6), rep("Pioneer_Near", 6), rep("Pioneer_Far", 6), rep("Grass_Near", 6), rep("Grass_Far", 5))
Relative_Abundance_Guilds_by_Sample <- as.data.frame(Relative_Abundance_Guilds_by_Sample, row.names = Relative_Abundance_Guilds_by_Sample$Sample)
Relative_Abundance_Guilds_by_Sample$Sample <- NULL


guild <- metaMDS(Relative_Abundance_Guilds_by_Sample, k = 2)
guild_nmds_points <- as.data.frame(guild$points)
ggplot(data = guild_nmds_points) + geom_point(aes(x =MDS1, y =MDS2)) # plot the data 
treat <- c(rep("Forest", 6), rep("Forest_Interior", 6), rep("Forest_Exterior", 6), rep("Pioneer_Near", 6), rep("Pioneer_Far", 6), rep("Grass_Near", 6), rep("UMNR",6), rep("Grass_Far", 5))
guild_nmds_points$Transect <- treat
Sample <- c("A1", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2", "A2", "B1", "B2", "C1", "C2")
guild_nmds_points$Sample <- Sample
guild_nmds_points_hull <- guild_nmds_points %>% group_by(Transect) %>% slice(chull(MDS1,MDS2))
a <- ggplot() + geom_point(data = guild_nmds_points, aes(x = MDS1, y = MDS2,)) + geom_text(data = guild_nmds_points, aes(x = MDS1, y = MDS2), label = guild_nmds_points$Sample, col = "black", size = 2, vjust = -2) + geom_polygon(data = guild_nmds_points_hull, aes(x = MDS1, y = MDS2, fill = Transect), alpha = 0.5)
pdf(file = "Outputs/Figures/Guild_NMDS.pdf")
plot(a)
dev.off()

Relative_Abundance_Guilds_by_Sample <- as.data.frame(Relative_Abundance_Guilds_by_Sample[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,43,44,45,46,47),]) #extract only MC data

#create a vector of distances of each sample from forest edge 
Distance_From_Edge <- c(rep(-42,6), rep(-3,6), rep(3,6), rep(19,6), rep(83,6), rep(29,6), rep(187,5))
Relative_Abundance_Guilds_by_Sample$Distance_From_Edge <- Distance_From_Edge
#pairwise plots 
pairwise_rel_guild_distance <- pairs(Relative_Abundance_Guilds_by_Sample)
#reshape data to long format
Rel_General_Abundance_reshaped <- melt(Relative_Abundance_Guilds_by_Sample ,  id.vars = 'Distance_From_Edge', variable.name = 'Guilds', value.name = "Relative_Abundance")
Relative_Guilds_vs_Distance <- ggplot(Rel_General_Abundance_reshaped, aes(Distance_From_Edge, Relative_Abundance, color = Guilds)) + geom_point() + geom_smooth(method =glm)
plot(Rel_General_Abundance_reshaped$Guilds ~ Rel_General_Abundance_reshaped$Relative_Abundance)

pdf(file = "Outputs/Figures/Realtive_Guilds_vs_Distance.pdf")
par()
plot(Relative_Guilds_vs_Distance)
dev.off()

#Look at guilds against each other 
Relative_Abundance_Guilds_by_Sample$Distance_From_Edge <- NULL
pairs(Relative_Abundance_Guilds_by_Sample, panel = panel.lm,
    cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
    font.labels = 2, lower.panel = panel.cor)
ggpairs(Relative_Abundance_Guilds_by_Sample, lower = list(continuous = "smooth"))

pdf(file = "Outputs/Figures/Pairwise_Guilds.pdf")
par(c(1,2))
pairs(Relative_Abundance_Guilds_by_Sample)
ggpairs(Relative_Abundance_Guilds_by_Sample)
dev.off()

Guild_vs_Distance <- glm(Relative_Abundance_Guilds_by_Sample ~ Distance_From_Edge + Relative_Abundance_Guilds_by_Sample*Distance_From_Edge)
