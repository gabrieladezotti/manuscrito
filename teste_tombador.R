#Manuscrito Tombador - manipulação de dados
library(tidyverse)

setwd("C:/R/manuscrito")
tombador_base <- read.table("tombador_1304.txt", h = T,
                            stringsAsFactors = T, sep="\t")
plot6 <- read.table("plot6.txt", h = T,
                    stringsAsFactors = T, sep="\t")

teste <- tombador_base %>%
  dplyr::filter(plot != "C" & 
                plot != 6 &
                plot != 3 &
                plot != 24)

teste <- rbind(teste, plot6)

str(teste)
summary(teste)
teste$subplot <- as.factor(teste$subplot)
teste$cover <- as.numeric(teste$cover)

teste_mod <- teste %>%
  dplyr::filter(treatment == "control" |
                  treatment == "annual" |
                  treatment == "anual" |
                  treatment == "modal" |
                  treatment == "mid") %>%
  dplyr::mutate(season = recode(season, Wet = "wet"),
         treatment = recode(treatment, anual = "annual", mid = "modal")) %>%
  tidyr::separate(date, c("year", "month"), sep = "_") %>%
  dplyr::filter(species != "solo nu" &
                  species != "biomassa morta" &
                  species != "Solo nu" &
                  species != "Biomassa morta" &
                  species != "solo nu (formigueiro invadindo)" &
                  species != "Solo nu " &
                  species != "Biomassa morta ") %>%
  dplyr::filter(cover != 0 & 
                  cover != "" &
                  cover != "herb. fina oposta cruzada" &
                  plot != 14)

teste_mod$plot <- droplevels(teste_mod$plot)
teste_mod$treatment <- droplevels(teste_mod$treatment)
levels(teste_mod$treatment)

teste_mod <- teste_mod %>%
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
     "Euphorbia potentilloides " = "Euphorbia potentilloides"))
 
  teste_mod <- teste_mod %>%  
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
                species != "Rullia/myrcia" &  
                species != "Serreada com tricomas nervura central" &
                species != "Serreada discolor" & 
                species != "Toni roxo sem cheiro" &
                species != "Folha comprida" &
                species != "Aster verde" &
                species != "Almeirão duro") %>%
    dplyr::mutate(species = recode(species,
           "herbacea alterna" = "Herbacea alterna",
           "Peltea macedoi" = "Peltaea macedoi"))
  
  teste_mod <- teste_mod %>%
    dplyr::mutate(species = recode(species,
    "Schyzachirium sp" = "Schizachyrium sp",
    "Paspalum pectinatum" = "Schizachyrium sp",
    "Paspalum" = "Schizachyrium sp",
    "herbaceae alta fofa" = "Herbacea alterna",
    "herbácea alterna fofa" = "Herbacea alterna",
    "folha verde larga " = "FVL",
    "folha verde larga" = "FVL",
    "Folha oposta cruzada pouco tricoma" = "Folha oposta cruzada tricoma",
    "Folha alterna espiralada tricomafolha/caule" = "Folha alterna espiralada tricoma",
    "Falsa ouratea (folha alterna base discolor)" = "Falsa ouratea",
    "Asteraceae caule amarelo anguloso" = "asteracea angulosa",
    "almeirão peludo" = "Gomphrena sp",
    "Bauhinia dumosa " = "Bauhinia dumosa",
    "Cyperaceae" = "Cyperaceae sp",
    "Elionurus muticus " = "Elionurus muticus",
    "Mesosetum ferrugineum " = "Mesosetum ferrugineum")) %>%
    dplyr::filter(species != "Poaceae sp1" &
                  species != "hortelã discolor" &
                  species != "Espadinha croton" &
                  species != "Myrcia peciolo gordo" &
                  species != "Falsa ouratea" &
                  species != "Asteraceae branca" &
                  species != "folha comprida" &
                  species != "Oposta cruzada peluda" &
                  species != "Mimosa" &
                  species != "Herbacea serreada dura" &
                  species != "Cruzada, super peluda/pontuda" &
                  species != "Arbusto folha larga composta coriaceae" &
                  species != "Herbacea nervura central marcada" &
                  species != "Herbacea coraçao" &
                  species != "Grama braquiária" &
                  species != "Arbusto discolor" &
                  species != "arbusto discolor alterna" &
                  species != "Achatada alterna" &
                  species != "Aldama bracteata" &
                  species != "Xyris sp" &
                  species != "Alterna pontuda" &
                  species != "Arbustinho" &
                  species != "arbusto grande (2 folhas)" &
                  species != "Asteracea alterna" &
                  species != "Babosinha lisa" &
                  species != "Trançada achatada" &
                  species != "Sabicea brasiliensis " &
                  species != "Rubiaceae sp" &
                  species != "Rhynchospora" &
                  species != "Rasteirinha" &
                  species != "Polygala poaya" &
                  species != "Polygala" &
                  species != "Pinheiro tuxinho" &
                  species != "Pinheiro" &
                  species != "Paspalum sp" &
                  species != "Panicum peludo grande" &
                  species != "Panicum base peluda" &
                  species != "panicum aspero" &
                  species != "Ouratea sp" &
                  species != "Oposta pontuda" &
                  species != "Myrtacinha" &
                  species != "Mix ruellia e croton" &
                  species != "Mini diplusodon" &
                  species != "Mimosa " &
                  species != "Lixa macia" &
                  species != "junta achatada" &
                  species != "Iridaceae" &
                  species != "Herbaceae 01" &
                  species != "Herbacea sapo" &
                  species != "Herbacea cruzada" &
                  species != "Graminea achatada" &
                  species != "gorda fofa" &
                  species != "Galianthe sp" &
                  species != "Stachytarpheta villosa" &
                  species != "Polygala longicaulis" &
                  species != "Poaceae sp2" &
                  species != "Palicourea rigida" &
                  species != "Oposta alterna" &
                  species != "Oposta alterna " &
                  species != "Myrcia guianensis" &
                  species != "lingua discolor" &
                  species != "Lessingianthus durus" &
                  species != "Biomassa morta " &
                  species != "Clitória sp" &
                  species != "Croton macrodentado" &
                  species != "Cuphea sp" &
                  species != "Euphorbiaceae (arbusto com latex)" &
                  species != "Evolvulus hipocraterioflorus" &
                  species != "Falso diplusodon" &
                  species != "Falso diplusodon" &
                  species != "Folha comprida arredondada" &
                  species != "folha aletrna brilhosa" &
                  species != "Biomassa morta " &
                  species != "Graminea coletada" &
                  species != "Insimium *" &
                  species != "")
  
  
unique(teste_mod$species[order(teste_mod$species, decreasing = T)])
teste_mod$species <- droplevels(teste_mod$species)
str(teste_mod)

teste_mod <- teste_mod %>%
  dplyr::mutate(treatment = recode(treatment, annual = "AF", modal = "BF",
                                   control = "FE"))

######
write.csv2(x = teste_mod, file = "tombador_modificado_julho.csv",
          row.names = T, sep = ";")
