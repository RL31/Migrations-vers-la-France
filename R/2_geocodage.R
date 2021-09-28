library(tidyverse)
library(tidygeocoder)

etranger <- readRDS("donnees/etranger.RDS")

etranger2 <- etranger %>% 
  bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="CROATIE")) %>% 
  bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="SLOVENIE")) %>% 
  bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="MACEDOINE")) %>% 
  bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="BOSNIEHERZEGOVINE")) %>% 
  bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="SERBIE")) %>% 
  bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="MONTENEGRO")) %>% 
  bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="KOSOVO")) %>% 
  bind_rows(etranger %>% filter(str_detect(PAYSOK1,"TCHECO|SERBIEMON|SERBIEET")) %>% 
              mutate(PAYSOK=case_when(
                str_detect(PAYSOK1,"TCHECO") ~ "SLOVAQUIE",
                str_detect(PAYSOK1,"SERBIEMON|SERBIEET") ~ "MONTENEGRO",
                TRUE ~ "PAYSOK"
              ))) %>% 
  count(PAYSOK, COMMOK) %>% 
  arrange(desc(n))




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
  
  osm <- etranger2 %>%
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

