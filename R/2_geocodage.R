library(tidyverse)
library(tidygeocoder)

etranger <- readRDS("donnees/etranger2.RDS")

# Liste des pays pour lesquels on veut g?ocoder les communes
liste_pays <- c("ISLANDE","ESPAGNE","SUISSE","TURQUIE","HONGRIE","GRECE","PORTUGAL",
                "POLOGNE","ALLEMAGNE","ITALIE","BELGIQUE","AUTRICHE",
                "ROUMANIE","RUSSIE","LUXEMBOURG","BULGARIE",
                "SERBIE","ARMENIE","SLOVAQUIE","DANEMARK","IRLANDE","SUEDE","LETTONIE","NORVEGE","FINLANDE","MONTENEGRO","ALBANIE",
                "UKRAINE","CROATIE","LITUANIE","ANDORRE","MACEDOINE","CHYPRE","ESTONIE","MOLDAVIE",
                "MALTE","SLOVENIE","KOSOVO",
                "SAINTMARIN","BOSNIEHERZEGOVINE","ROYAUMEUNI","PAYSBAS","REPUBLIQUETCHEQUE",
                "BIELORUSSIE","ALGERIE","TUNISIE","MAROC")

geocodage <- function(PAYS){
  if(PAYS=="PAYSBAS"){
    PAYS2 <- "PAYS-BAS"
    PAYS3 <- "PAYS BAS"
  } else if(PAYS=="SAINTMARIN"){
    PAYS2 <- "SAINT-MARIN"
    PAYS3 <- "SAINT MARIN"
  } else if(PAYS=="BOSNIEHERZEGOVINE"){
    PAYS2 <- "BOSNIE-HERZEGOVINE"
    PAYS3 <- "BOSNIE HERZEGOVINE"
  } else if(PAYS=="ROYAUMEUNI"){
    PAYS2 <- "ROYAUME-UNI"
    PAYS3 <- "ROYAUME UNI"
  } else if(PAYS=="REPUBLIQUETCHEQUE"){
    PAYS2 <- "REPUBLIQUE TCHEQUE"
    PAYS3 <- "REPUBLIQUE TCHEQUE"
  } else {
    PAYS2 <- PAYS
    PAYS3 <- PAYS
  }
  
  osm <- etranger %>%
    filter(PAYSOK==PAYS3 | PAYSOK==PAYS | PAYSOK==PAYS2) %>%
    mutate(PAYS2=PAYS2) %>% 
    geocode(city = COMMOK,
            country = PAYS2, method = "osm",
            full_results=TRUE)
  saveRDS(osm,paste0("donnees/osm_",PAYS,"2.RDS"))
  rm(osm)
}

walk(liste_pays,geocodage)

# geocodage("PORTUGAL")
# geocodage("GRECE")
# geocodage("TURQUIE")

