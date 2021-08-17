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

tombador_plots <- tombador %>%
  group_by(year, treatment, season, plot, species) %>%
  summarise(across(where(is.numeric), median))

####AF, BF, FE
#subsets
df_af_wet <- subset(tombador_plots, treatment %in% "AF" & season %in% "wet")
df_af_dry <- subset(tombador_plots, treatment %in% "AF" & season %in% "dry")

df_bf_wet <- subset(tombador_plots, treatment %in% "BF" &
                year != 2016 & season %in% "wet")
df_bf_dry <- subset(tombador_plots, treatment %in% "BF" &
                      year != 2016& season %in% "dry")

df_fe_wet <- subset(tombador_plots, treatment %in% "FE" &
                year != 2016 & season %in% "wet")
df_fe_dry <- subset(tombador_plots, treatment %in% "FE" &
                      year != 2016 & season %in% "dry")

#RAC_change
rac_af_dry <- RAC_change(df = df_af_dry,
                     time.var = "year",
                     species.var = "species",
                     abundance.var = "cover",
                     replicate.var = "plot")
rac_af_dry$treatment <- c("AF", "AF", "AF", "AF", "AF", "AF", "AF", "AF")

rac_af_wet <- RAC_change(df = df_af_wet,
                         time.var = "year",
                         species.var = "species",
                         abundance.var = "cover",
                         replicate.var = "plot") 
rac_af_wet$treatment <- c("AF", "AF", "AF", "AF", "AF", "AF", "AF", "AF")

rac_bf_dry <- RAC_change(df = df_bf_dry,
                        time.var = "year",
                        species.var = "species",
                        abundance.var = "cover",
                        replicate.var = "plot")  
rac_bf_dry$treatment <- c("BF", "BF", "BF", "BF", "BF", "BF", "BF", "BF")

rac_bf_wet <- RAC_change(df = df_bf_wet,
                         time.var = "year",
                         species.var = "species",
                         abundance.var = "cover",
                         replicate.var = "plot")  
rac_bf_wet$treatment <- c("BF", "BF", "BF", "BF", "BF", "BF", "BF", "BF",
                          "BF", "BF", "BF", "BF")

rac_fe_dry <- RAC_change(df = df_fe_dry,
                         time.var = "year",
                         species.var = "species",
                         abundance.var = "cover",
                         replicate.var = "plot")  
rac_fe_dry$treatment <- c("FE", "FE", "FE", "FE", "FE", "FE")

rac_fe_wet <- RAC_change(df = df_fe_wet,
                         time.var = "year",
                         species.var = "species",
                         abundance.var = "cover",
                         replicate.var = "plot")  
rac_fe_wet$treatment <- c("FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE")

##planilhas
df_wet <- rbind(rac_fe_wet, rac_bf_wet, rac_af_wet)
df_dry <- rbind(rac_fe_dry, rac_bf_dry, rac_af_dry)

#passagem de dados organizados para nova planilha
write.csv2(df_wet, file = "RAC_wet.csv", row.names = T)
write.csv2(df_dry, file = "RAC_dry.csv", row.names = T)
