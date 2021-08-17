#Manuscrito Tombador - RAC_change
library(tidyverse)
library(codyn)

setwd("C:/R/manuscrito")
tombador <- read.table("tombador_modificado_agosto.txt", h = T,
                       stringsAsFactors = T, sep="\t")
str(tombador)
tombador$year <- as.factor(tombador$year)
tombador$month <- as.factor(tombador$month)
tombador$plot <- as.factor(tombador$plot)
tombador$subplot <- as.factor(tombador$subplot)

##subplots
plot4_dry <- subset(tombador, plot %in% 4 & season %in% "dry")
rac_4dry <- RAC_change(df = plot4_dry,
                         time.var = "year",
                         species.var = "species",
                         abundance.var = "cover",
                         replicate.var = "subplot")
plot4_wet <- subset(tombador, plot %in% 4 & season %in% "wet")
rac_4wet <- RAC_change(df = plot4_wet,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")

plot11_dry <- subset(tombador, plot %in% 11 & season %in% "dry")
rac_11dry <- RAC_change(df = plot11_dry,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")
plot11_wet <- subset(tombador, plot %in% 11 & season %in% "wet")
rac_11wet <- RAC_change(df = plot11_wet,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")

plot16_dry <- subset(tombador, plot %in% 16 & season %in% "dry")
rac_16dry <- RAC_change(df = plot16_dry,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")
plot16_wet <- subset(tombador, plot %in% 16 & season %in% "wet")
rac_16wet <- RAC_change(df = plot16_wet,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")

plot20_dry <- subset(tombador, plot %in% 20 & season %in% "dry")
rac_20dry <- RAC_change(df = plot20_dry,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")
plot20_wet <- subset(tombador, plot %in% 20 & season %in% "wet")
rac_20wet <- RAC_change(df = plot20_wet,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")

plot2_dry <- subset(tombador, plot %in% 2 & season %in% "dry")
rac_2dry <- RAC_change(df = plot2_dry,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")
plot2_wet <- subset(tombador, plot %in% 2 & season %in% "wet")
rac_2wet <- RAC_change(df = plot2_wet,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")

plot10_dry <- subset(tombador, plot %in% 10 & season %in% "dry")
rac_10dry <- RAC_change(df = plot10_dry,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")
plot10_wet <- subset(tombador, plot %in% 10 & season %in% "wet")
rac_10wet <- RAC_change(df = plot10_wet,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")

plot17_dry <- subset(tombador, plot %in% 17 & season %in% "dry")
rac_17dry <- RAC_change(df = plot17_dry,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")
plot17_wet <- subset(tombador, plot %in% 17 & season %in% "wet")
rac_17wet <- RAC_change(df = plot17_wet,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")

plot23_dry <- subset(tombador, plot %in% 23 & season %in% "dry")
rac_23dry <- RAC_change(df = plot23_dry,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")
plot23_wet <- subset(tombador, plot %in% 23 & season %in% "wet")
rac_23wet <- RAC_change(df = plot23_wet,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")

plot6_dry <- subset(tombador, plot %in% 6 & season %in% "dry")
rac_6dry <- RAC_change(df = plot6_dry,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")
plot6_wet <- subset(tombador, plot %in% 6 & season %in% "wet")
rac_6wet <- RAC_change(df = plot6_wet,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")

plot12_dry <- subset(tombador, plot %in% 12 & season %in% "dry")
rac_12dry <- RAC_change(df = plot12_dry,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")
plot12_wet <- subset(tombador, plot %in% 12 & season %in% "wet")
rac_12wet <- RAC_change(df = plot12_wet,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")

plot13_dry <- subset(tombador, plot %in% 13 & season %in% "dry")
rac_13dry <- RAC_change(df = plot13_dry,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")
plot13_wet <- subset(tombador, plot %in% 13 & season %in% "wet")
rac_13wet <- RAC_change(df = plot13_wet,
                       time.var = "year",
                       species.var = "species",
                       abundance.var = "cover",
                       replicate.var = "subplot")
