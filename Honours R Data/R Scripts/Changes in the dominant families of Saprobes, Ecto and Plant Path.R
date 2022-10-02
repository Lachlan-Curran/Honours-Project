rm(list=ls())
#First three lines are to fix recurring error in mac 
library(showtext)
font_add(family = "Arial", regular = "Arial.ttf") ## here is the path to the font to add.
showtext.auto()
library(ggplot2)
library(tidyverse)
library(dplyr)
library(cowplot)
library(Polychrome)
library(stringr)

#define colours for plots 
my_colours <- c("#848482", "#875692", "#F38400", "#A1CAF1", "#BE0032",  "#C2B280","#FFCCFF", "#008856", "#E68FAC", "#0067A5","#F99379", "604E97", "#F6A600", "#B3446C", "#DCD300", "#882D17", "#8DB600", "#654522", "#E25822", "#2B3D26", "#000000")
swatch(my_colours)


#Import guild files 
Guilds <- read.csv("Cleaned Up Data/Guild_Taxa_ID.csv")
#Assign taxa as rownames and keep as a column 
Guilds <- as.data.frame(Guilds[,c(2:50)], row.names = Guilds$Taxonomy)
 
#We now want to assign each taxa into a broad guild: Saprobe, Endo/Ectomycorrhizae and Plant Path, we will igonore all others
#Take a subset of the data where the letters Saprobe appear in the guilds column
Taxa_Saprobe <- as.data.frame(Guilds[grep("Sapro", Guilds$guild),])
#Repeat for Ectomycorrhizae
Taxa_Ecto <- as.data.frame(Guilds[grep("Ecto", Guilds$guild),])
#Repeat for Endomycorrhizae
Taxa_Endo <- as.data.frame(Guilds[grep("Arbuscular", Guilds$guild),]) #Funguild uses the outdated term for endomycorrhizae - "Arbuscular"
#Repeat for Plant Pathogengs 
Taxa_Path <- as.data.frame(Guilds[grep("Plant Pathogen", Guilds$guild),])
#We want to remove anything that is a mycoparasite in this table - e.g. trichoderma is a mycoparaiste and is therefore found associated with plant pathogens so can be mistakenly  assigned as one
Taxa_Path <- as.data.frame(Taxa_Path[c(-grep("Fungal Parasite", Taxa_Path$guild)),])
#Now we have four separate dataframes of community abundance, sorted by guild 
#Now we need to aggregate them to the transect level 

#Saprobes 
#Remove guild ID and Taxa ID Columns 
Taxa_Saprobe <- as.data.frame(Taxa_Saprobe[,c(2:48)])
#Transpose and convert to data frame again 
Taxa_Saprobe <- t(Taxa_Saprobe)
Taxa_Saprobe <- as.data.frame(Taxa_Saprobe)
#Reorder Rows 
Taxa_Saprobe <- as.data.frame(Taxa_Saprobe[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,43,44,45,46,47,37,38,39,40,41,42),])
#Add Transect Data 
Transect <- c(rep("Forest", 6), rep("Forest Edge Interior", 6), rep("Forest Edge Exterior", 6), rep("Pioneer Near", 6), rep("Pioneer Far", 6), rep("Grass Near", 6), rep("Grass Far", 5), rep("UMNR", 6))
Taxa_Saprobe$Transect <- Transect
#Make Transect rows and delete column
Taxa_Saprobe <- as.data.frame(Taxa_Saprobe[,c(1:1161),])
#Store transect as list
Transect <- list(Transect)
#Aggregate to transect
Taxa_Saprobe_Transect <- aggregate(Taxa_Saprobe, by = Transect, FUN = sum)
#Reorder rows and make first column row names 
Taxa_Saprobe_Transect <- as.data.frame(Taxa_Saprobe_Transect[c(1,3,2,7,6,5,4,8),],)
Taxa_Saprobe_Transect <- as.data.frame(Taxa_Saprobe_Transect[,c(2:1161)], row.names = Taxa_Saprobe_Transect$Group.1)
#transpose and reconvert to dataframe 
Taxa_Saprobe_Transect <- t(Taxa_Saprobe_Transect)
Taxa_Saprobe_Transect <- as.data.frame(Taxa_Saprobe_Transect)
#Add a taxa column 
Taxa_Saprobe_Transect$TAXA <- rownames(Taxa_Saprobe_Transect)

#Repeat for Ectomycorrhizae
#Remove guild ID and Taxa ID Columns 
Taxa_Ecto <- as.data.frame(Taxa_Ecto[,c(2:48)])
#Transpose and convert to data frame again 
Taxa_Ecto <- t(Taxa_Ecto)
Taxa_Ecto <- as.data.frame(Taxa_Ecto)
#Reorder Rows 
Taxa_Ecto<- as.data.frame(Taxa_Ecto[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,43,44,45,46,47,37,38,39,40,41,42),])
#Add Transect Data 
Transect <- c(rep("Forest", 6), rep("Forest Edge Interior", 6), rep("Forest Edge Exterior", 6), rep("Pioneer Near", 6), rep("Pioneer Far", 6), rep("Grass Near", 6), rep("Grass Far", 5), rep("UMNR", 6))
Taxa_Ecto$Transect <- Transect
#Make Transect rows and delete column
Taxa_Ecto <- as.data.frame(Taxa_Ecto[,c(1:114),])
#Store transect as list
Transect <- list(Transect)
#Aggregate to transect
Taxa_Ecto_Transect <- aggregate(Taxa_Ecto, by = Transect, FUN = sum)
#Reorder rows and make first column row names 
Taxa_Ecto_Transect <- as.data.frame(Taxa_Ecto_Transect[c(1,3,2,7,6,5,4,8),],)
Taxa_Ecto_Transect <- as.data.frame(Taxa_Ecto_Transect[,c(2:114)], row.names = Taxa_Ecto_Transect$Group.1)
#transpose and reconvert to dataframe 
Taxa_Ecto_Transect <- t(Taxa_Ecto_Transect)
Taxa_Ecto_Transect <- as.data.frame(Taxa_Ecto_Transect)
#Add a taxa column 
Taxa_Ecto_Transect$TAXA <- rownames(Taxa_Ecto_Transect)

#Repeat For Endomycorrhizae 
#Remove guild and ID columns 
Taxa_Endo <- as.data.frame(Taxa_Endo[,c(2:48)])
#Transpose and convert to data frame again 
Taxa_Endo <- t(Taxa_Endo)
Taxa_Endo <- as.data.frame(Taxa_Endo)
#Reorder Rows 
Taxa_Endo<- as.data.frame(Taxa_Endo[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,43,44,45,46,47,37,38,39,40,41,42),])
#Add Transect Data 
Transect <- c(rep("Forest", 6), rep("Forest Edge Interior", 6), rep("Forest Edge Exterior", 6), rep("Pioneer Near", 6), rep("Pioneer Far", 6), rep("Grass Near", 6), rep("Grass Far", 5), rep("UMNR", 6))
Taxa_Endo$Transect <- Transect
#Make Transect rows and delete column
Taxa_Endo <- as.data.frame(Taxa_Endo[,c(1:191),])
#Store transect as list
Transect <- list(Transect)
#Aggregate to transect
Taxa_Endo_Transect <- aggregate(Taxa_Endo, by = Transect, FUN = sum)
#Reorder rows and make first column row names 
Taxa_Endo_Transect <- as.data.frame(Taxa_Endo_Transect[c(1,3,2,7,6,5,4,8),],)
Taxa_Endo_Transect <- as.data.frame(Taxa_Endo_Transect[,c(2:191)], row.names = Taxa_Endo_Transect$Group.1)
#transpose and reconvert to dataframe 
Taxa_Endo_Transect <- t(Taxa_Endo_Transect)
Taxa_Endo_Transect <- as.data.frame(Taxa_Endo_Transect)
#Add a taxa column 
Taxa_Endo_Transect$TAXA <- rownames(Taxa_Endo_Transect)
#Endomycorrhizae does not have enough resolution at the family level to be useful for this exploration, we will haev to ignore it 

#Repeat for Plant Pathogens 
#Remove guild and ID columns 
Taxa_Path <- as.data.frame(Taxa_Path[,c(2:48)])
#Transpose and convert to data frame again 
Taxa_Path <- t(Taxa_Path)
Taxa_Path <- as.data.frame(Taxa_Path)
#Reorder Rows 
Taxa_Path<- as.data.frame(Taxa_Path[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,43,44,45,46,47,37,38,39,40,41,42),])
#Add Transect Data 
Transect <- c(rep("Forest", 6), rep("Forest Edge Interior", 6), rep("Forest Edge Exterior", 6), rep("Pioneer Near", 6), rep("Pioneer Far", 6), rep("Grass Near", 6), rep("Grass Far", 5), rep("UMNR", 6))
Taxa_Path$Transect <- Transect
#Make Transect rows and delete column
Taxa_Path <- as.data.frame(Taxa_Path[,c(1:186),])
#Store transect as list
Transect <- list(Transect)
#Remove trnasect column 
Taxa_Path$Transect <- NULL
#Aggregate to transect
Taxa_Path_Transect <- aggregate(Taxa_Path, by = Transect, FUN = sum)
#Reorder rows and make first column row names 
Taxa_Path_Transect <- as.data.frame(Taxa_Path_Transect[c(1,3,2,7,6,5,4,8),],)
Taxa_Path_Transect <- as.data.frame(Taxa_Path_Transect[,c(2:186)], row.names = Taxa_Path_Transect$Group.1)
#transpose and reconvert to dataframe 
Taxa_Path_Transect <- t(Taxa_Path_Transect)
Taxa_Path_Transect <- as.data.frame(Taxa_Path_Transect)
#Add a taxa column 
Taxa_Path_Transect$TAXA <- rownames(Taxa_Path_Transect)

#Create a dataframe that contains columns of habitat, count, family and otu data 
#Saprobe 
Saprobe <- Taxa_Saprobe_Transect %>%
  select(-UMNR) %>% 
  mutate(Family = replace_na(str_extract(TAXA, "[:alpha:]+aceae"), "Incertae sedis")) %>% 
  # filter out Incertae sedis?
  group_by(Family) %>% 
  mutate(OTU_Number = row_number()) %>% 
  ungroup() %>% 
  pivot_longer(cols = 1:7, names_to = "Habitat", values_to = "Count") %>% 
  group_by(Habitat) %>% 
  top_n(wt = Count, n = 5) %>% 
  mutate(OTU_Name = paste0(Family, "_", OTU_Number)) %>% 
  arrange(desc(Count)) %>% 
  group_by(Habitat) %>% 
  mutate(level_order = row_number())
View()

#Read in substrate types for saprobes 
substrate <- read.csv("Raw_Data/Dominant Saprobes Substrate Types.csv")
#Extract columns 2 and 3 
substrate <- as.data.frame(substrate[,c(2,3)])
#merge saprobe and substrate
Saprobe <- left_join(Saprobe, substrate, by = "Family")
#incertae sedis is being tricky, manually assogn substrate for now

Saprobe$Substrate <- Saprobe$Substrate %>% replace_na('Soil')

#transform the count data so the extreme values are less tricky to visualize 
Saprobe$Count_Transformed <- log(Saprobe$Count)



 
 ggplot(data = Saprobe) +
  geom_bar(aes(x = level_order, y = Count_Transformed, fill = Family), stat = "identity") +
  facet_wrap(~ factor(Habitat, levels = c(
    "Grass Near", "Grass Far", "Pioneer Near", "Pioneer Far", "Forest Edge Interior", "Forest Edge Exterior", "Forest")), 
    scales="free_x", ncol = 2, strip.position = "bottom") +
  theme_classic() +
theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),strip.background = element_blank(), strip.placement = "outside") +
labs(x = "", y = "log(Abundance)") + scale_fill_manual(values = c("Aspergillaceae" = "#0099FF", "Chaetosphaeriaceae" = "#0033FF", "Ganodermataceae" = "#0066FF", "Lasiosphaeriaceae" = "#660099", "Herpotrichiellaceae" ="#9933CC", "Myxotrichaceae" = "#33CC33", "Kickxellaceae" = "#00CC66", "Incertae sedis" = "#336600",  
                                                                "Hygrophoraceae" = "#339966", "Clavariaceae" = "#99CC00", "Pseudeurotiaceae" = "#FFFF00", "Trichocomaceae" = "#FF9900", "Phallaceae" = "#FF6600"  )) + coord_cartesian(ylim = c(4,7))


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
  top_n(wt = Count, n = 5) %>% 
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
    "Grass Near", "Grass Far", "Pioneer Near", "Pioneer Far", "Forest Edge Interior", "Forest Edge Exterior", "Forest")), 
    scales="free_x", ncol = 2, strip.position = "bottom") +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.background = element_blank(), strip.placement = "outside") +
  labs(x = "", y = "log(Abundance)") + scale_fill_manual(values =  c("Myxotrichaceae" = "#0099FF", "Tricholomataceae" = "#660099", "Thelephoraceae" = "#00CC66", "Hygrophoraceae" = "#FF9900", "Helotiaceae" = "#0033FF",
"Pluteaceae" = "#FFCCFF")) + coord_cartesian(ylim = c(2,7))







#Pathogen 
Pathogen <- Taxa_Path_Transect %>%
  select(-UMNR) %>% 
  mutate(Family = replace_na(str_extract(TAXA, "[:alpha:]+aceae"), "Incertae sedis")) %>% 
  # filter out Incertae sedis?
  group_by(Family) %>% 
  mutate(OTU_Number = row_number()) %>% 
  ungroup() %>% 
  pivot_longer(cols = 1:7, names_to = "Habitat", values_to = "Count") %>% 
  group_by(Habitat) %>% 
  top_n(wt = Count, n = 5) %>% 
  mutate(OTU_Name = paste0(Family, "_", OTU_Number)) %>% 
  arrange(desc(Count)) %>% 
  group_by(Habitat) %>% 
  mutate(level_order = row_number())
View()
Pathogen$Count_Transformed <- log(Pathogen$Count)

#read in path mode data 
pathology <- read.csv("Raw_Data/Plant path mode Maleny.csv")
#merge pathogen and pathology tables 
Pathogen <- left_join(Pathogen, pathology, by = "Family")
#Assign incertae sedis mode 
Pathogen$Pathogen.Mode <- Pathogen$Pathogen.Mode %>% replace_na("Hemibiotroph")

 
ggplot(data = Pathogen) +
  geom_bar(aes(x = level_order, y = Count_Transformed, fill = Family), stat = "identity") +
  facet_wrap(~ factor(Habitat, levels = c(
    "Grass Near", "Grass Far", "Pioneer Near", "Pioneer Far", "Forest Edge Interior", "Forest Edge Exterior", "Forest")), 
    scales="free_x", ncol = 2, strip.position = "bottom") +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.background = element_blank(), strip.placement = "outside") +
  labs(x = "", y = "(log(Abundance)") + scale_fill_manual(values = c("Magnaporthaceae" = "#33CC33", "Entorrhizaceae" =  "#00CC66", "Ustilaginaceae" =  "#336600", "Venturiaceae" = "#339966",
                                                                    "Incertae sedis" = "#0099FF", "Ganodermataceae" = "#0033FF", "Ophiostomataceae" = "#660099", "Marasmiaceae" = "#9933CC", "Xylariaceae" = "#CC99FF", "Didymosphaeriaceae" = "#FF9900" )) + coord_cartesian(ylim= c(3,6))

#Create a species abundance curve for all taxa, colour code by guild 
#find sum of eahc sample in guild per transect 
Guilds$Forest <- rowSums(Guilds[,c(2:7)])
Guilds$Forest_Edge_Interior <- rowSums(Guilds[,c(8:13)])
Guilds$Forest_Edge_Exterior <- rowSums(Guilds[,c(14:19)])
Guilds$Pioneer_Near <- rowSums(Guilds[,c(20:25)])
Guilds$Pioneer_Far <- rowSums(Guilds[,c(26:31)])
Guilds$Grass_Near <- rowSums(Guilds[,c(32:37)])
Guilds$Grass_Far <- rowSums(Guilds[,c(44:48)])

Guilds <- as.data.frame(Transect_Guilds[grep("Sapro |Arbuscular", Transect_Guilds$guild),])







