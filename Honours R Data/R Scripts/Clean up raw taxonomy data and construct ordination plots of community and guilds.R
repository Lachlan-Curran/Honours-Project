rm(list=ls())
#First three lines are to fix recurring error in mac 
library(showtext)
font_add(family = "Arial", regular = "Arial.ttf") ## here is the path to the font to add.
showtext.auto()
library(biom) #we need this package to read and manipulate BIOM objects 
library(dplyr)
library(vegan)
library(tidyverse)
library(ggplot2)
library(FUNGuildR)#this package assigns functional groups to taxa
library(caret)


biom <- read_biom("Raw_Data/ACE_Data_Maleny/qiime_feature_table/feature-table.biom") # Import your BIOM file
summary(biom) #summaries the data stored in object as list
biom <- biom_data(biom) #convert from biom object to S4 object of class dgCMatrix
biom <- as.matrix(biom) # convert to matrix
#convert to data frame
Full_Taxa <- as.data.frame(biom)
#convert row names to column
Full_Taxa$Taxa <- row.names(biom)
#Rename first column 
colnames(Full_Taxa)[48] <- "Feature ID"
#Rename the count columns to their sample names 
Full_Taxa <- Full_Taxa %>% rename("Forest A1" = SE4602_J4816, "Forest A2" = SE4603_J4816, "Forest B1" = SE4604_J4816, "Forest B2" = SE4605_J4816, "Forest C1" = SE4606_J4816,"Forest C2" = SE4607_J4816) #rename samples columns to trasect codew
Full_Taxa <- Full_Taxa %>% rename("Forest Interior A1" = SE4608_J4816, "Forest Interior A2" = SE4609_J4816, "Forest Interior B1" = SE4610_J4816, "Forest Interior B2" = SE4611_J4816, "Forest Interior C1" = SE4612_J4816,"Forest Interior C2" = SE4613_J4816)
Full_Taxa <- Full_Taxa %>% rename("Forest Exterior A1" = SE4614_J4816, "Forest Exterior A2" = SE4615_J4816, "Forest Exterior B1" = SE4616_J4816, "Forest Exterior B2" = SE4617_J4816, "Forest Exterior C1" = SE4618_J4816,"Forest Exterior C2" = SE4619_J4816)
Full_Taxa <- Full_Taxa %>% rename("Pioneer Near A1" = SE4620_J4816, "Pioneer Near A2" = SE4621_J4816, "Pioneer Near B1" = SE4622_J4816, "Pioneer Near B2" = SE4623_J4816, "Pioneer Near C1" = SE4624_J4816,"Pioneer Near C2" = SE4625_J4816)
Full_Taxa <- Full_Taxa %>% rename("Pioneer Far A1" = SE4626_J4816, "Pioneer Far A2" = SE4627_J4816, "Pioneer Far B1" = SE4628_J4816, "Pioneer Far B2" = SE4629_J4816, "Pioneer Far C1" = SE4630_J4816,"Pioneer Far C2" = SE4631_J4816)
Full_Taxa <- Full_Taxa %>% rename("Grass Near A1" = SE4632_J4816, "Grass Near A2" = SE4633_J4816, "Grass Near B1" = SE4634_J4816, "Grass Near B2" = SE4635_J4816, "Grass Near C1" = SE4636_J4816,"Grass Near C2" = SE4637_J4816)
Full_Taxa <- Full_Taxa %>% rename("UMNR A1" = SE4638_J4816, "UMNR A2" = SE4639_J4816, "UMNR B1" = SE4640_J4816, "UMNR B2" = SE4641_J4816, "UMNR C1" = SE4642_J4816,"UMNR C2" = SE4643_J4816)
Full_Taxa <- Full_Taxa %>% rename("Grass Far A2" = SE4644_J4816, "Grass Far  B1" = SE4645_J4816, "Grass Far  B2" = SE4646_J4816, "Grass Far C1 " = SE4647_J4816, "Grass Far C2" = SE4648_J4816)
#normalize data 


#Import taxa info for row names
taxa_ID <- read_tsv("Raw_Data/ACE_Data_Maleny/qiime_taxonomy/taxonomy.tsv")
# convert to dataframe
taxa_ID <- as.data.frame(taxa_ID) 
#we can drop the consensus column, this is just a confidence metric which we can look at further upstream if need be 
taxa_ID_no_consensus <- subset(taxa_ID[1:2])
#Assign taxonomy from ACE to each row via the "Feature ID" Column in both 
Taxa_ID_Count <- left_join(taxa_ID_no_consensus, Full_Taxa, by = "Feature ID")
#drop ID Column 
taxa <- as.data.frame(Taxa_ID_Count[,c(2:49)])
#Make taxa rownames, remove feature ID column 
TAXA <- as.data.frame(taxa[,c(2:48)], row.names = taxa$Taxon)

#Transpose and save
Fungi_by_Sample <- t(TAXA)
Fungi_by_Sample <- as.data.frame(Fungi_by_Sample)

#Find min species count per row 
raremax <- min(rowSums(Fungi_by_Sample))
#rarefy to that minimum value 
Fungi_by_Sample <- rrarefy(Fungi_by_Sample, raremax)
Fungi_by_Sample <- as.data.frame(Fungi_by_Sample)
write.csv(Fungi_by_Sample, file = "Cleaned Up Data/Fungi_by_Sample.csv")
#Aggregate to plot level 
Plot <- c(rep("Forest_1",2), rep("Forest_2",2),rep("Forest_3",2), rep("Forest_Edge_Interior_1",2), rep("Forest_Edge_Interior_2",2), rep("Forest_Edge_Interior_3",2), rep("Forest_Edge_Exterior_1",2), rep("Forest_Edge_Exterior_2",2), rep("Forest_Edge_Exterior_3",2), rep("Pioneer_Near_1", 2), rep("Pioneer_Near_2", 2), rep("Pioneer_Near_3", 2), rep("Pioneer_Far_1", 2),rep("Pioneer_Far_2", 2), rep("Pioneer_Far_3", 2), rep("Grass_Near_1",2),rep("Grass_Near_2",2), rep("Grass_Near_3",2), rep("UMNR_1",2),  rep("UMNR_2",2),  rep("UMNR_3",2), rep("Grass_Far_1",1), rep("Grass_Far_2",2), rep("Grass_Far_3",2))
Fungi_by_Sample$Plot <- Plot
Fungi_by_Sample <- as.data.frame(Fungi_by_Sample[,c(1:5137)], row.names = Fungi_by_Sample$Plot)
Plot <- list(Plot)
Fungi_by_Plot <- aggregate(Fungi_by_Sample, by = Plot, FUN = sum)
#Reorder rows  and add row names
Fungi_by_Plot <- as.data.frame(Fungi_by_Plot[c(1,2,3,7,8,9,4,5,6,19,20,21,16,17,18,13,14,15,10,11,12,22,23,24),])
Fungi_by_Plot <- as.data.frame(Fungi_by_Plot[,c(2:5138)], row.names = Fungi_by_Plot$Group.1)
#Save File 

write.csv(Fungi_by_Plot, file = "Cleaned Up Data/Community_Fungi_By_Plot.csv")

#nmds analysis - This will take time
TAXA_nmds <- metaMDS(TAXA)
#Check the quality of the data 
stressplot(TAXA_nmds)
#Store points in dataframe 
TAXA_nmds_species <- as.data.frame(TAXA_nmds$species) #the dataframe was in the wrong configuration, in this dataframe, species = sites
#reorder rows 
TAXA_nmds_species <- as.data.frame(TAXA_nmds_species[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,43,44,45,46,47,37,38,39,40,41,42),])
#Add a transect column to guild_nmds_species - we need this info to group the points 
Transect <- c(rep("Forest", 6), rep("Forest_Edge_Interior", 6), rep("Forest_Edge_Exterior", 6), rep("Pioneer_Near", 6), rep("Pioneer_Far", 6), rep("Grass_Near", 6), rep("Grass_Far", 5),  rep("UMNR",6))
TAXA_nmds_species$Transect <- Transect
#Add a sample column as well, need this info to group points 
Sample <- c("A1", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2", "A2", "B1", "B2", "C1", "C2","A1", "A2", "B1", "B2", "C1", "C2")
TAXA_nmds_species$Sample <- Sample
#Define colours
my_colours <- c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2",  "#CC6677","#AA4499", "#999999")
#plot the data
ggplot(data = TAXA_nmds_species) + geom_point(aes(x =MDS1, y =MDS2))
#Create Hull
TAXA_nmds_species_hull <- TAXA_nmds_species %>% group_by(Transect) %>% slice(chull(MDS1,MDS2))
#Reorder variables to stop ggplot from automatically plotting alphabetically 
TAXA_nmds_species_hull$Transect <- factor(TAXA_nmds_species_hull$Transect, levels = c("Forest", "Forest_Edge_Interior", "Forest_Edge_Exterior", "Pioneer_Near", "Pioneer_Far", "Grass_Near", "Grass_Far", "UMNR"))

#Create plot
TAXA_NMDS <- ggplot() + geom_point(data = TAXA_nmds_species, aes(x = MDS1, y = MDS2,)) + geom_text(data = TAXA_nmds_species, aes(x = MDS1, y = MDS2), label = TAXA_nmds_species$Sample, col = "black", size = 2, vjust = -2) + geom_polygon(data = TAXA_nmds_species_hull, aes(x = MDS1, y = MDS2, fill = Transect), alpha = 0.5) 
#Call Plot                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  #Call plot 
TAXA_NMDS + scale_colour_manual(values = my_colours) + scale_fill_manual(values=my_colours)
pdf(file = "Outputs/Figures/Community_NMDS.pdf")
par(1,1)
TAXA_NMDS + scale_colour_manual(values = my_colours) + scale_fill_manual(values=my_colours)
dev.off()



#Assign guild data - use package FUNguildR - all we need is a column that has the taxa info in the format it currently is with the header Taxonomy 
#rename column 
colnames(taxa)[1] <- "Taxonomy"
#assign guilds to taxa - it will filter out all taxa that do not have a resolution to at least genus
Guild_Taxa <- funguild_assign(taxa)
#We only want taxa whose tropic guild confidence ranking is at least probable, so we need to remove all rows with NAs or possible confidence ratings 
Guild_Taxa <- na.omit(Guild_Taxa)
#We now want to remove all unnecessary columns, we are only interested in the counts per sample, and the guild ID 
Guild_Taxa <- Guild_Taxa[,-c(56:59)]
Guild_Taxa <- Guild_Taxa[,-c(49:53)]
Guild_Taxa <- Guild_Taxa[ -grep("Possible", Guild_Taxa$confidenceRanking),]
#Remove confidence ranking 
Guild_Taxa <- Guild_Taxa[,-c(50)]
#Save this file 
write.csv(Guild_Taxa, file = "Cleaned Up Data/Guild_Taxa_ID.csv")
#Aggregate guild count so we only 1 have unique guild per row - calculate the sum of each 
#Assign guild as a list 
guild <- list(Guild_Taxa$guild)
#assign guild as row names 
Guild_Taxa <- as.data.frame(Guild_Taxa[,c(2:48)], row.names = Guild_Taxa$guild)
#aggregate
Guild_Taxa <- aggregate(Guild_Taxa, by = guild, FUN = sum)
#re-assign guilds as row names
Guild_Taxa <- as.data.frame(Guild_Taxa[,c(2:48)], row.names = Guild_Taxa$Group.1)
#transpose data frame 
Guild_Taxa <- t(Guild_Taxa)
#convert back to dataframe 
Guild_Taxa <- as.data.frame(Guild_Taxa)
#re-order the rows so UMNR is last 
Guild_Taxa <- as.data.frame(Guild_Taxa[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,43,44,45,46,47,37,38,39,40,41,42),])
#save this as Guild_Aggregated 
write.csv(Guild_Taxa, "Raw_Data/Guilds_Aggregated.csv")
#now we want to group each guild into 1 of 5 categories: Saprobe, Endomycorrhizae, Ectomycorrhizae, Plant Pathogen or Other 
#If a guild has multiple trophic lifestyles (e.g. plant path and saprobe) then it will be counted for both those guilds
Saprobe <- Guild_Taxa[,c(1,2,4,5,6,7,8,10,11,12,13,14,15,16,17,18,21,22,23,25,26,28,29,30,31,34,35,36,39,41,42,43,47,48,49,50,51,52,53,54,55,56,57)]
Endomycorrhizae <- Guild_Taxa[,c(9)]
Ectomycorrhizae <- Guild_Taxa[,c(10,19,20,21,22,23)]
Plant_Pathogen <- Guild_Taxa[,c(4,6,7,11,21,27,28,29,30,35,46,47,48,49,50)]
Other <- Guild_Taxa[,c(1,2,3,4,5,6,7,8,10,11,12,20,21,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,48)]
#Find the total count for each guild 
Saprobe_Abundance <- rowSums(Saprobe)
Saprobe_Abundance <- as.data.frame(Saprobe_Abundance)
Endomycorrhizae_Abundance <- Endomycorrhizae
Endomycorrhizae_Abundance <- as.data.frame(Endomycorrhizae)
Ectomycorrhizae_Abundance <- rowSums(Ectomycorrhizae)
Ectomycorrhizae_Abundance <- as.data.frame(Ectomycorrhizae_Abundance)
Plant_Pathogen_Abundance <- rowSums(Plant_Pathogen)
Plant_Pathogen_Abundance <- as.data.frame(Plant_Pathogen_Abundance)
Other_Abundance <- rowSums(Other)
Other_Abundance <- as.data.frame(Other_Abundance)
#Assign a new column "sample" to each new guild dataframe from Guild_Taxa rownames 
Sample <- rownames(Guild_Taxa)
Saprobe_Abundance$Sample <- Sample
Endomycorrhizae_Abundance$Sample <- Sample
Ectomycorrhizae_Abundance$Sample <- Sample
Plant_Pathogen_Abundance$Sample <- Sample
Other_Abundance$Sample <- Sample
#Merge these data frames into one by Sample
Guilds_Condensed <- left_join(Saprobe_Abundance, Endomycorrhizae_Abundance, by = "Sample")
Guilds_Condensed <- left_join(Guilds_Condensed, Ectomycorrhizae_Abundance, by = "Sample")
Guilds_Condensed <- left_join(Guilds_Condensed, Plant_Pathogen_Abundance, by = "Sample")
Guilds_Condensed <- left_join(Guilds_Condensed, Other_Abundance, by = "Sample")

#Finally, assign row names as sample and remove sample column 
Guilds_Condensed <- as.data.frame(Guilds_Condensed[,c(1,3,4,5,6)], row.names = Guilds_Condensed$Sample)
#save file 
write.csv(Guilds_Condensed, "Raw_Data/Guilds_Condensed.csv")

#Ordination plot of Guilds using vegan 
guild_nmds <- metaMDS(Guilds_Condensed, k = 2)
#Check quality of Data
stressplot(guild_nmds)
#Extract point information
guild_nmds_points <- as.data.frame(guild_nmds$points)
#Add a transect column to guild_nmds_species - we need this info to group the points 
Transect <- c(rep("Forest", 6), rep("Forest_Edge_Interior", 6), rep("Forest_Edge_Exterior", 6), rep("Pioneer_Near", 6), rep("Pioneer_Far", 6), rep("Grass_Near", 6), rep("Grass_Far", 5),  rep("UMNR",6))
guild_nmds_points$Transect <- Transect
#Add a sample column as well, need this info to group points 
guild_nmds_points$Sample <- Sample
#Define colours
my_colours <- c("#009E73", "#E69500", "#D55E00", "#56B4E9", "#0072B2",  "#CC6677","#AA4499", "#999999")
#plot the data
ggplot(data = guild_nmds_points) + geom_point(aes(x =MDS1, y =MDS2))
#Create Hull
guild_nmds_points_hull <- guild_nmds_points %>% group_by(Transect) %>% slice(chull(MDS1,MDS2))
#Reorder variables to stop ggplot from automatically plotting alphabetically 
guild_nmds_points_hull$Transect <- factor(guild_nmds_points_hull$Transect, levels = c("Forest", "Forest_Edge_Interior", "Forest_Edge_Exterior", "Pioneer_Near", "Pioneer_Far", "Grass_Near", "Grass_Far", "UMNR"))

#Create plot
GUILD_NMDS <- ggplot() + geom_point(data = guild_nmds_points, aes(x = MDS1, y = MDS2,)) + geom_text(data = guild_nmds_points, aes(x = MDS1, y = MDS2), label = guild_nmds_points$Sample, col = "black", size = 2, vjust = -2) + geom_polygon(data = guild_nmds_points_hull, aes(x = MDS1, y = MDS2, fill = Transect), alpha = 0.5) 
#Call Plot                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  #Call plot 
GUILD_NMDS + scale_colour_manual(values = my_colours) + scale_fill_manual(values=my_colours)

#Save plots to pdf
pdf(file = "Outputs/Figures/Ordination_Community_Guild.pdf")
par(1,2)
TAXA_NMDS
GUILD_NMDS
dev.off()

#Sumamrise phylums per transect 

#Ecto 
Ecto <- Taxa_Ecto_Transect %>%
  select(-UMNR) %>% 
  mutate(Family = replace_na(str_extract(TAXA, "[:alpha:]+aceae"), "Incertae sedis")) %>% 
  # filter out Incertae sedis?
  group_by(Family) %>% 
  mutate(OTU_Number = row_number()) %>% 
  ungroup() %>% 
  pivot_longer(cols = 1:7, names_to = "Habitat", values_to = "Count") %>% 
  group_by(Habitat) %>% 
  top_n(wt = Count, n = 10) %>% 
  mutate(OTU_Name = paste0(Family, "_", OTU_Number)) %>% 
  arrange(desc(Count)) %>% 
  group_by(Habitat) %>% 
  mutate(level_order = row_number())
#transform the count data so the extreme values are less tricky to visualize 
Ecto$Count_Transformed <- log(Ecto$Count)
View()

ggplot(data = Ecto) +
  geom_bar(aes(x = level_order, y = Count_Transformed, fill = Family), stat = "identity") +
  facet_wrap(~ factor(Habitat, levels = c(
    "Grass_Near", "Grass_Far", "Pioneer_Near", "Pioneer_Far", "Forest_Edge_Interior", "Forest_Edge_Exterior", "Forest")), 
    scales="free_x", ncol = 2) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.background = element_blank()) +
  labs(x = "", y = "log(Abundance)") + scale_fill_manual(values = my_colours)







