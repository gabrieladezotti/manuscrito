#Manuscrito Tombador - RAC_change
library(tidyverse)
library(codyn)

setwd("C:/R/manuscrito")

#WET
RAC_wet <- read.table("RAC_wet.txt", h = T,
                       stringsAsFactors = T, sep="\t")
season <- vector("list", length(0))
for (i in 1:29) {
  season[[i]] <- print(paste("wet"))
}
season <- unlist(season)
RAC_wet$season <- season

#DRY
RAC_dry <- read.table("RAC_dry.txt", h = T,
                      stringsAsFactors = T, sep="\t")
season <- vector("list", length(0))
for (i in 1:22) {
  season[[i]] <- print(paste("dry"))
}
season <- unlist(season)
RAC_dry$season <- season

RAC_tombador <- rbind(RAC_wet, RAC_dry)

#gráficos
#richness 
 RAC_tombador %>%
   mutate(treatment = fct_relevel(treatment,
                        "FE", "AF", "BF")) %>%
   ggplot(aes(x = treatment, y = richness_change, fill = season)) +
   geom_boxplot(width = 0.4, alpha = 0.6)

#evenness 
 RAC_tombador %>%
   mutate(treatment = fct_relevel(treatment,
                                  "FE", "AF", "BF")) %>%
   ggplot(aes(x = treatment, y = evenness_change, fill = season)) +
   geom_boxplot(width = 0.4, alpha = 0.6)

#rank
 RAC_tombador %>%
   mutate(treatment = fct_relevel(treatment,
                                  "FE", "AF", "BF")) %>%
   ggplot(aes(x = treatment, y = rank_change, fill = season)) +
   geom_boxplot(width = 0.4, alpha = 0.6)
 
##análises
 modelo.richness <- lm(richness_change ~ treatment*season, 
                       data = RAC_tombador)
 summary(modelo.richness) 

 modelo.evenness <- lm(evenness_change ~ treatment*season, 
                       data = RAC_tombador)
 summary(modelo.evenness)
 
 modelo.rank <- lm(rank_change ~ treatment*season, 
                       data = RAC_tombador)
 summary(modelo.rank) 
 