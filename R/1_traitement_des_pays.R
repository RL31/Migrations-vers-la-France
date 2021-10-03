library(sf)
library(tidyverse)
library(stringi)

# Creation de la base des lieux de naissance, lieux de décès
origine <- function(AN){
  annee <- read.csv2(paste0("donnees/site_insee/deces_",AN,".csv")) %>% 
    select(datenaiss,commnaiss,paysnaiss,datedeces,lieudeces) %>% 
    mutate(lieudeces=as.character(lieudeces))
  return(annee)
}
liste <- c(as.character(rep(1970:2020)))
base_orig <- map_df(liste,origine)

# On ne retient que les naissances a l'etranger ; 
# Travail sur les toponymes pour les standardiser (Ascii, ponctuations etc.)
etranger <- base_orig %>% 
  mutate(PAYSOK1=str_replace_all(paysnaiss,"[:punct:]|[:space:]|[:digit:]"," "),#|-
         COMMOK=str_replace_all(commnaiss,"[:punct:]|[:digit:]"," "),#[:space:]||-
         PAYSOK1=str_to_upper(stri_trans_general(PAYSOK1,"latin-ascii")),
         COMMOK=str_to_upper(stri_trans_general(COMMOK,"latin-ascii"))
  ) %>%  
  filter(PAYSOK1 != ""  & !is.na(PAYSOK1) 
         & !is.na(COMMOK) & COMMOK !="") %>% 
  mutate(PAYSOK=case_when(str_detect(PAYSOK1,"ALLEM|RFA|SARRE") | PAYSOK1=="RDA" ~ "ALLEMAGNE",
                          str_detect(PAYSOK1,"PAG|BALEA|CANAR|MAJORQ") ~ "ESPAGNE",
                          str_detect(PAYSOK1,"BRET|GB|ANGLE|ANGLON|COSS|GUERNE|JERS|GALL|ROYAUMEUNI") ~ "ROYAUMEUNI",
                          str_detect(PAYSOK1,"LOGNE") ~ "POLOGNE",
                          str_detect(PAYSOK1,"PORTUGAL|MADERE|ACOR") ~ "PORTUGAL",
                          str_detect(PAYSOK1,"BOSNIE") ~ "BOSNIEHERZEGOVINE",
                          str_detect(PAYSOK1,"BELARUS") ~ "BIELORUSSIE",
                          str_detect(PAYSOK1,"TCH|BOHEME") ~ "REPUBLIQUETCHEQUE", #dupliquer tchecoslovaquie pour chercher en slovaquie IDEM YOUGO
                          str_detect(PAYSOK1,"SUISSE|HELV") ~ "SUISSE",
                          str_detect(PAYSOK1,"SERBIE") ~ "SERBIE",#dupliquer pour montenegro
                          str_detect(PAYSOK1,"SICI|SARD|ITAL") ~ "ITALIE",
                          str_detect(PAYSOK1,"UKR") ~ "UKRAINE",
                          str_detect(PAYSOK1,"CHYP") ~ "CHYPRE",
                          str_detect(PAYSOK1,"CRET") ~ "GRECE",
                          str_detect(PAYSOK1,"ALG|ORAN|STANTINE") ~ "ALGERIE",
                          str_detect(PAYSOK1,"EIRE|IRLAND") ~ "IRLANDE",
                          str_detect(PAYSOK1,"SLAVEDEMA|MACED") ~ "MACEDOINE",
                          str_detect(PAYSOK1,"DUCH") ~ "LUXEMBOURG",
                          str_detect(PAYSOK1,"RUSS|URSS|SIBERIE") ~ "RUSSIE",
                          str_detect(PAYSOK1,"HOLLANDE") ~ "PAYSBAS",
                          PAYSOK1=="LT" ~ "LITUANIE",
                          str_detect(PAYSOK1,"MONAC") ~ "MONACO",
                          str_detect(PAYSOK1,"MARIN") ~ "SAINTMARIN",
                          str_detect(PAYSOK1,"TANGER") ~ "MAROC",
                          str_detect(PAYSOK1,"TURQUIE") ~ "TURQUIE",
                          TRUE ~ PAYSOK1)) 

# Fichier sauvegardé car sert plusieurs fois par la suite
saveRDS(etranger,"donnees/etranger.RDS")
