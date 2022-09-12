rm(list=ls())
library(ggplot2)
library(tidyverse)
library(dplyr)
library(RColorBrewer)

#Import dataset containing functional information by sample 
Guild_by_Sample <- read.csv("Raw_Data/Guild_Count_Filtered_Probable.csv")
Community_Taxa <- read.csv("Cleaned Up Data/Fungi_by_Sample.csv")
Guild_by_Sample <- Guild_by_Sample[,c(10,16,17,18,19,20,21, 22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62)]
Guild_by_Sample <- as.data.frame(Guild_by_Sample, row.names = Guild_by_Sample$guild)
#aggregate data by guild 
GUILD_AGGREGATED <- aggregate(Guild_by_Sample[,2:48], by=list(Category=Guild_by_Sample$guild), FUN=sum)
GUILD_AGGREGATED <- as.data.frame(GUILD_AGGREGATED, row.names = GUILD_AGGREGATED$Category)
GUILD_AGGREGATED$Category = NULL

GUILD_AGGREGATED <- t(GUILD_AGGREGATED)
GUILD_AGGREGATED <- as.data.frame(GUILD_AGGREGATED)
GUILD_AGGREGATED <- rownames_to_column(GUILD_AGGREGATED)
GUILD_AGGREGATED$rowname <- Community_Taxa$X
GUILD_AGGREGATED <- as.data.frame(GUILD_AGGREGATED, row.names = GUILD_AGGREGATED$rowname)
GUILD_AGGREGATED$rowname <- NULL


Saprobe <- GUILD_AGGREGATED[,c(1,2,4,5,6,7,8,10,11,12,13,14,15,16,17,18,21,22,23,25,26,28,29,30,31,34,35,36,39,41,42,43,47,48,49,50,51,52,53,54,55,56,57)]
Endomycorrhizhae <- GUILD_AGGREGATED[,c(9)]
Ectomycorrhizhae <- GUILD_AGGREGATED[,c(10,19,20,21,22,23)]
Plant_Pathogen <- GUILD_AGGREGATED[,c(4,6,7,11,21,27,28,29,30,35,46,47,48,49,50)]
Other <- GUILD_AGGREGATED[,c(1,2,3,4,5,6,7,8,10,11,12,20,21,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,48)]
Saprobe_Abundance <- rowSums(Saprobe)
Saprobe_Abundance <- as.data.frame(Saprobe_Abundance)
Saprobe_Abundance$Sample <- Community_Taxa$X
Endomycorrhizhae_Abundance <- Endomycorrhizhae
Endomycorrhizhae_Abundance <- as.data.frame(Endomycorrhizhae)
Endomycorrhizhae_Abundance$Sample <- Saprobe_Abundance$Sample
Ectomycorrhizhae_Abundance <- rowSums(Ectomycorrhizhae)
Ectomycorrhizhae_Abundance <- as.data.frame(Ectomycorrhizhae_Abundance)
Ectomycorrhizhae_Abundance$Sample <- Saprobe_Abundance$Sample
Plant_Pathogen_Abundance <- rowSums(Plant_Pathogen)
Plant_Pathogen_Abundance <- as.data.frame(Plant_Pathogen_Abundance)
Plant_Pathogen_Abundance$Sample <- Saprobe_Abundance$Sample
Other_Abundance <- rowSums(Other)
Other_Abundance <- as.data.frame(Other_Abundance)
Other_Abundance$Sample <- Saprobe_Abundance$Sample

General_Guild_Abundance <- left_join(Saprobe_Abundance, Endomycorrhizhae_Abundance, by = "Sample")
General_Guild_Abundance <- left_join(General_Guild_Abundance, Ectomycorrhizhae_Abundance, by = "Sample")
General_Guild_Abundance <- left_join(General_Guild_Abundance, Plant_Pathogen_Abundance, by = "Sample")
General_Guild_Abundance <- left_join(General_Guild_Abundance, Other_Abundance, by = "Sample")
General_Guild_Abundance <- as.data.frame(General_Guild_Abundance, row.names = General_Guild_Abundance$Sample)
General_Guild_Abundance$Sample <- NULL
Total_Abundance <-rowSums(General_Guild_Abundance)
General_Guild_Abundance$Total_Abundance <- Total_Abundance

write.csv(General_Guild_Abundance, file = "Cleaned Up Data/General_Guild_Abundance_by_Sample.csv")

Rel_Saprobe_Abundance <- (General_Guild_Abundance$Saprobe_Abundance/General_Guild_Abundance$Total_Abundance)*100
Rel_Endomycorrhizhae_Abundance <- (General_Guild_Abundance$Endomycorrhizhae/General_Guild_Abundance$Total_Abundance) * 100
Rel_Ectomycorrhizhae_Abundance <- (General_Guild_Abundance$Ectomycorrhizhae_Abundance/General_Guild_Abundance$Total_Abundance) * 100
Rel_Plant_Pathogen_Abundance <- (General_Guild_Abundance$Plant_Pathogen_Abundance/General_Guild_Abundance$Total_Abundance)*100
Rel_Other_Abundance <- (General_Guild_Abundance$Other_Abundance/General_Guild_Abundance$Total_Abundance)*100
Relative_Abundance_Guilds <- tibble(Rel_Saprobe_Abundance, Rel_Ectomycorrhizhae_Abundance, Rel_Endomycorrhizhae_Abundance, Rel_Plant_Pathogen_Abundance, Rel_Other_Abundance)
Relative_Abundance_Guilds_by_Sample  <- as.data.frame(Relative_Abundance_Guilds, row.names = Other_Abundance$Sample)
Relative_Abundance_Guilds_by_Sample$Sample <- Other_Abundance$Sample
colnames(Relative_Abundance_Guilds_by_Sample) <- c("Saprobes", "Ectomycorrhizae", "Endomycorrhizae", "Plant Pathogens", "Other Guilds", "Sample")
Transect <- c(rep("Forest", 6), rep("Forest_Edge_Interior", 6), rep("Forest_Edge_Exterior", 6), rep("Pioneer_Near", 6), rep("Pioneer_Far", 6), rep("Grass_Near", 6), rep("UMNR",6), rep("Grass_Far", 5))
Relative_Abundance_Guilds_by_Sample <- as.data.frame(Relative_Abundance_Guilds_by_Sample, row.names = Relative_Abundance_Guilds_by_Sample$Sample)


Relative_Abundance_Guilds_by_Transect <- as.data.frame(Relative_Abundance_Guilds)
Relative_Abundance_Guilds_by_Transect$Transect <- Transect
Relative_Abundance_Guilds_by_Transect <- as.data.frame(Relative_Abundance_Guilds_by_Transect, row.names = Relative_Abundance_Guilds_by_Transect$Transect)
Relative_Abundance_Guilds_by_Transect$Sample <- Other_Abundance$Sample
Relative_Abundance_Guilds_by_Transect$Plot <- Community_Taxa$X

Relative_Transect_Abundance_Mean <- aggregate(Relative_Abundance_Guilds_by_Transect, by = list(Relative_Abundance_Guilds_by_Transect$Transect), FUN = mean)
Relative_Transect_Abundance_Mean <- as.data.frame(Relative_Transect_Abundance_Mean[c(1,2,3,7,6,5,4,8),], row.names = Relative_Transect_Abundance_Mean$Group.1)
Relative_Transect_Abundance_Mean <- as.data.frame(Relative_Transect_Abundance_Mean[,c(2:6)], row.names = c("Forest", "Forest_Edge_Exterior", "Forest_Edge_Interior", "Grass_Far", "Grass_Near", "Pioneer_Far", "Pioneer_Near", "UMNR"))
Relative_Transect_Abundance_Mean <- as.matrix(Relative_Transect_Abundance_Mean)
Relative_Transect_Abundance_Mean <- t(Relative_Transect_Abundance_Mean)
Guild <- c("Saprobe", "Ectomycorrhizae", "Endomycorrhizae", "Plant Pathogen", "Other")
Relative_Transect_Abundance_Mean <- as.data.frame(Relative_Transect_Abundance_Mean, row.names = Guild)
Relative_Transect_Abundance_Mean <- as.data.frame(Relative_Transect_Abundance_Mean)
col_order <- c("Forest", "Forest_Edge_Interior", "Forest_Edge_Exterior", "Pioneer_Near", "Pioneer_Far", "Grass_Near", "Grass_Far", "UMNR")
Relative_Transect_Abundance_Mean <- as.data.frame(Relative_Transect_Abundance_Mean[,col_order])
Relative_Transect_Abundance_Mean <- as.matrix(Relative_Transect_Abundance_Mean)

 
#Define Colours
my_colours <- c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2",  "#CC6677","#AA4499", "#999999")


pdf(file = "Outputs/Figures/Guild_Proportion_By_Transect.pdf")
par(mar=c(4, 4.1, 4.1, 8.1), xpd=TRUE)
Relative_Abundance_Transect_Mean_Barplot <- barplot(Relative_Transect_Abundance_Mean, col = my_colours, cex.names = 0.5, las = 2) 
legend("right", inset=c(-0.3, 0), bty = "n", cex = 0.9, legend=Guild, fill  = my_colours)
dev.off()

#Define Transect
Transect <- c("Forest", "Forest_Edge_Interior", "Forest_Edge_Exterior", "Pioneer_Near", "Pioneer_Far", "Grass_Near", "Grass_Far", "UMNR")
#Define Colours

#define level order so the x axis isn't arranged alphabetically 
Relative_Saprobe_Abundance_By_Transect <- ggplot(data = Relative_Abundance_Guilds_by_Transect, aes(x = Transect, y = Rel_Saprobe_Abundance, fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", ) + scale_fill_manual(values = my_colours)
Relative_Endo_Abundance_By_Transect <- ggplot(data = Relative_Abundance_Guilds_by_Transect, aes(Transect, Rel_Endomycorrhizhae_Abundance, fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)+ scale_fill_manual(values = my_colours)
Relative_Ecto_Abundance_By_Transect <- ggplot(data = Relative_Abundance_Guilds_by_Transect, aes(Transect, Rel_Ectomycorrhizhae_Abundance, fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)+ scale_fill_manual(values = my_colours)
Relative_Plant_Pathogen_Abundance_By_Transect <- ggplot(data = Relative_Abundance_Guilds_by_Transect, aes(Transect, Rel_Plant_Pathogen_Abundance, fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)+ scale_fill_manual(values = my_colours)
Relative_Other_Abundance_By_Transect <- ggplot(data = Relative_Abundance_Guilds_by_Transect, aes(Transect, Rel_Other_Abundance, fill = Transect)) + geom_boxplot() + scale_x_discrete("Transect", label = abbreviate)+ scale_fill_manual(values = my_colours)
pdf(file = "Outputs/Figures/Relative_Abundance_Guilds_By_Transect.pdf")


par(c(3,2))
plot(Relative_Saprobe_Abundance_By_Transect)
plot(Relative_Ecto_Abundance_By_Transect)
plot(Relative_Endo_Abundance_By_Transect)
plot(Relative_Plant_Pathogen_Abundance_By_Transect)
plot(Relative_Other_Abundance_By_Transect)
dev.off()

Relative_Abundance_Guilds_by_Sample$Sample <- NULL
Sample_Guild_Heatmap <- heatmap(as.matrix(log(Relative_Abundance_Guilds_by_Sample + 1)), Rowv = NA, Colv = NA, cexRow = 0.5, cexCol = 0.5, col= colorRampPalette(brewer.pal(9, "YlOrRd"))(100), xlab = "Functional Guild", ylab = "Sample", legend("Left", legend = c(1:9), col = colorRampPalette(brewer.pal(9, "YlOrRd"))(25)))
pdf(file = "Outputs/Figures/Heat_Map_Guild_by_Sample.pdf")
par()
heatmap(as.matrix(log(Relative_Abundance_Guilds_by_Sample + 1)), Rowv = NA, Colv = NA, cexRow = 0.5, cexCol = 0.5, col= colorRampPalette(brewer.pal(9, "YlOrRd"))(25), xlab = "Functional Guild", ylab = "Sample", legend("Left", legend = c(1:9), col = colorRampPalette(brewer.pal(9, "YlOrRd"))(25)))
dev.off()






