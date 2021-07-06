#MANUSCRITO - 2021 - base_data
#organização de dados
library(tidyverse)

setwd("C:/R/manuscrito")
tombador_base <- read.table("tombador_1304.txt", h = T,
                             stringsAsFactors = T, sep="\t")
str(tombador_base)
tombador_base$subplot <- as.factor(tombador_base$subplot)

tombador_mod <- tombador_base %>%
  dplyr::filter(treatment == "control" |
                treatment == "annual" |
                treatment == "anual" |
                treatment == "modal") %>%
  dplyr::mutate(season = recode(season, Wet = "wet"),
                treatment = recode(treatment, anual = "annual")) %>%
  tidyr::separate(date, c("year", "month"), sep = "_") %>%
  dplyr::filter(species != "solo nu" &
                species != "biomassa morta" &
                species != "Solo nu" &
                species != "Biomassa morta" &
                species != "solo nu (formigueiro invadindo)") %>%
  dplyr::filter(cover != 0 & 
                cover != "" &
                cover != "herb. fina oposta cruzada")

teste <- tombador_mod %>%
  dplyr::mutate(species = recode(species,
  " Anthaenantia lanata" = "Anthaenantia lanata",
  "aldama grandiflora" = "Aldama grandiflora",
  "aristida sp" = "Aristida sp",
  "Artrogopon villosus" = "Artropogon villosus",
  "asteraceae caule amarelo anguloso" = "Asteraceae caule amarelo anguloso",
  "Calliandra dysantha " = "Calliandra dysantha",
  "euphorbia sp" = "Euphorbia sp",
  "folha oposta cruzada pouco tricoma" = "Folha oposta cruzada pouco tricoma",
  "herbacea alterna " = "Herbacea alterna",
  "herbacea alterna" = "Herbacea alterna",
  "limãozinho" = "Limãozinho",
  "lingua brilhante" = "Lingua brilhante",
  "merremia ericoides" = "Merremia ericoides",
  "mesosetum loliiforme" = "Mesosetum loliiforme",
  "Mimosa flavocaesia " = "Mimosa flavocaesia",
  "mimosa gracilis" = "Mimosa gracilis",
  "mimosa kalunga" = "Mimosa kalungae",
  "Myrcia " = "Myrcia",
  "Myrcia sp" = "Myrcia sp.",
  "Myrcia x " = "Myrcia x",
  "peciolo gordo" = "Peciolo gordo",
  "peltea macedoi" = "Peltaea macedoi",
  "Peltea macedoi" = "Peltaea macedoi",
  "Polygala abreuii" = "Polygala abreui",
  "rasteira arame vermelho" = "Rasteira arame vermelho",
  "rhynchospora consanguinea" = "Rhynchospora consanguinea",
  "serjania trichomisca" = "Serjania trichomisca",
  "vellozia squamata" = "Vellozia squamata",
  "veludo grande" = "Veludo grande",
  "Verbenacea pegajosa" = "Verbenaceae pegajosa"))

teste1 <- teste %>%
dplyr::filter(species != "Agrião roseta" &
              species != "Alterna cruzada" &
              species != "Alterna oposta" &
              species != "Arbustão" &
              species != "arbustao com latex" &
              species != "Arbustão peludo" & 
              species != "Arbusto 04" &
              species != "Arbusto 05" &
              species != "Arbusto 3 folhas" &
              species != "arbusto folha alterna lisa" &
              species != "Arbusto oposto alterno com estípulas" &
              species != "Asteraceae" &
              species != "Asteraceae alterna folha lisa" &
              species != "Borda serreada e caule peludo" & 
              species != "Capim cidreira" &
              species != "Cinzinha" &
              species != "Coriaceae discolor simples alterna" &
              species != "espiralada caderno" &
              species != "Falsa bernardia" &
              species != "Falsa ochrosperma" &
              species != "Falso stenodon" &
              species != "folha oposta peciologo gordo avermelhado" &
              species != "Folha oposta/ latex branco" &
              species != "Goiabinha" & 
              species != "herbacea dentada" &
              species != "Medusantha grande" &
              species != "oposta nervura vermelha" &
              species != "Pinheirinho asteraceae" &
              species != "Polygala sp" &
              species != "Ruellia/myrcia" &
              species != "Serreada com tricomas nervura central" &
              species != "Serreada discolor" & 
              species != "Toni roxo sem cheiro")