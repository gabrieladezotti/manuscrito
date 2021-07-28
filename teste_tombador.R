library(tidyverse)

setwd("C:/R/manuscrito")
tombador_base <- read.table("tombador_1304.txt", h = T,
                            stringsAsFactors = T, sep="\t")
plot6 <- read.table("plot6.txt", h = T,
                    stringsAsFactors = T, sep="\t")

teste1 <- tombador_base %>%
  dplyr::filter(plot != "C" & plot != 6)

teste <- rbind(teste1, plot6)

str(teste)
teste$subplot <- as.factor(teste$subplot)


teste_mod <- teste %>%
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
                  cover != "herb. fina oposta cruzada") %>%
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
              "Verbenacea pegajosa" = "Verbenaceae pegajosa",
              "Aldama" = "Aldama grandiflora",
              "Lippia orridula" = "Lippia horridula",
              "Harpalyce sp" = "Harpalyce tombadorensis",
              "H. obovans" = "Himatanthus obovata",
              "Euphorbia pottentiloides" = "Euphorbia potentilloides",
              "Croton grasilenses" = "Croton gracilescens",
              "Cipura xanthomelas" = "Cipura xantomelas",
              "Banisteriopsis panosa" = "Banisteriopsis pannosa",
              "Arthropogon villosus" = "Artropogon villosus",
              "Mesosetum lolliforme" = "Mesosetum loliiforme",
              "Euphorbia potentilloides " = "Euphorbia potentilloides")) %>%
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
                  species != "Toni roxo sem cheiro" &
                  species != "Folha comprida" &
                  species != "Aster verde" &
                  species != "Almeirão duro")

unique(teste_mod$species[order(teste_mod$species, decreasing = T)])
teste_mod$species <- droplevels(teste_mod$species)


##rever toda a mudança das especies pq nao estao alterando e muitas continuam,
##de resto tá tudo certo, depois é só continuar com as mudanças de manipulaçao