#Summarise the proportion of each phylum per transect 
rm(list=ls())
library(tidyverse)
library(stringr)
library(dplyr)
#Import fungi by transect 
Fungi <- read.csv("Cleaned Up Data/Fungi_by_Transect.csv")
Fungi <- Fungi %>%
  mutate(Phylum = replace_na(str_extract(X, "[:alpha:]+ota")))  %>% 
  pivot_longer(cols = 2:9, names_to = "Transect", values_to = "Count") %>% 
  # filter out Incertae sedis?
  group_by(Phylum) %>%
  ungroup()
  
  Fungi1 <- tibble(Fungi) %>% group_by(Transect)

  
#Ash playing
Fungi <- read.csv("Cleaned Up Data/Fungi_by_Transect.csv")
Fungi <- Fungi %>%
  mutate(Phylum = replace_na(str_extract(X, "[:alpha:]+ota"), "Incertae sedis"))  %>% 
  # filter out Incertae sedis?
  group_by(Phylum) %>%
  ungroup() %>% 
  pivot_longer(cols = 2:9, names_to = "Transect", values_to = "Count") %>%


Fungi1 <- Fungi %>% filter(Transect=='Forest'& Transect=="Forest_Edge_Interior") %>% 

Fungi2 <- Fungi %>% group_by(Transect)







ggplot(data = Ecto) +
  geom_bar(aes(x = level_order, y = Count_Transformed, fill = Family), stat = "identity") +
  facet_wrap(~ factor(Habitat, levels = c(
    "Grass_Near", "Grass_Far", "Pioneer_Near", "Pioneer_Far", "Forest_Edge_Interior", "Forest_Edge_Exterior", "Forest")), 
    scales="free_x", ncol = 2) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.background = element_blank()) +
  labs(x = "", y = "log(Abundance)") + scale_fill_manual(values = my_colours)
