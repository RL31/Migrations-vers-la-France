library(tidyverse)
#library(stringi)
# library(extrafont)
# #loadfonts(dev="win")
# 
# # Creation de la base des lieux de naissance
# origine <- function(AN){
#   annee <- read.csv2(paste0("donnees/deces-",AN,".csv")) %>% 
#     select(datenaiss,commnaiss,paysnaiss) 
#   return(annee)
# }
# liste <- c(as.character(rep(1970:2020)))
# base_orig <- map_df(liste,~origine(.))
# 
# # Naissances ? l'?tranger
# etranger <- base_orig %>% 
#   mutate(PAYSOK1=str_replace_all(paysnaiss,"[:punct:]|[:space:]|[:digit:]"," "),#|-
#          COMMOK=str_replace_all(commnaiss,"[:punct:]|[:digit:]"," "),#[:space:]||-
#          PAYSOK1=str_to_upper(stri_trans_general(PAYSOK1,"latin-ascii")),
#          COMMOK=str_to_upper(stri_trans_general(COMMOK,"latin-ascii"))
#   ) %>%  
#   filter(PAYSOK1 != ""  & !is.na(PAYSOK1) 
#          & !is.na(COMMOK) & COMMOK !="") %>% 
#   mutate(PAYSOK=case_when(str_detect(PAYSOK1,"ALLEM|RFA|SARRE") | PAYSOK1=="RDA" ~ "ALLEMAGNE",
#                           str_detect(PAYSOK1,"PAG|BALEA|CANAR|MAJORQ") ~ "ESPAGNE",
#                           str_detect(PAYSOK1,"BRET|GB|ANGLE|ANGLON|COSS|GUERNE|JERS|GALL|ROYAUMEUNI") ~ "ROYAUMEUNI",
#                           str_detect(PAYSOK1,"LOGNE") ~ "POLOGNE",
#                           str_detect(PAYSOK1,"PORTUGAL|MADERE|ACOR") ~ "PORTUGAL",
#                           str_detect(PAYSOK1,"BOSNIE") ~ "BOSNIEHERZEGOVINE",
#                           str_detect(PAYSOK1,"BELARUS") ~ "BIELORUSSIE",
#                           str_detect(PAYSOK1,"TCH|BOHEME") ~ "REPUBLIQUETCHEQUE", #dupliquer tchecoslovaquie pour chercher en slovaquie IDEM YOUGO
#                           str_detect(PAYSOK1,"SUISSE|HELV") ~ "SUISSE",
#                           str_detect(PAYSOK1,"SERBIE") ~ "SERBIE",#dupliquer pour montenegro
#                           str_detect(PAYSOK1,"SICI|SARD|ITAL") ~ "ITALIE",
#                           str_detect(PAYSOK1,"UKR") ~ "UKRAINE",
#                           str_detect(PAYSOK1,"CHYP") ~ "CHYPRE",
#                           str_detect(PAYSOK1,"CRET") ~ "GRECE",
#                           str_detect(PAYSOK1,"ALG|ORAN|STANTINE") ~ "ALGERIE",
#                           str_detect(PAYSOK1,"EIRE|IRLAND") ~ "IRLANDE",
#                           str_detect(PAYSOK1,"SLAVEDEMA|MACED") ~ "MACEDOINE",
#                           str_detect(PAYSOK1,"DUCH") ~ "LUXEMBOURG",
#                           str_detect(PAYSOK1,"RUSS|URSS|SIBERIE") ~ "RUSSIE",
#                           str_detect(PAYSOK1,"HOLLANDE") ~ "PAYSBAS",
#                           PAYSOK1=="LT" ~ "LITUANIE",
#                           str_detect(PAYSOK1,"MONAC") ~ "MONACO",
#                           str_detect(PAYSOK1,"MARIN") ~ "SAINTMARIN",
#                           str_detect(PAYSOK1,"TANGER") ~ "MAROC",
#                           str_detect(PAYSOK1,"TURQUIE") ~ "TURQUIE",
#                           TRUE ~ PAYSOK1)) 
# etranger <- etranger %>% 
#   bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="CROATIE")) %>% 
#   bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="SLOVENIE")) %>% 
#   bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="MACEDOINE")) %>% 
#   bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="BOSNIEHERZEGOVINE")) %>% 
#   bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="SERBIE")) %>% 
#   bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="MONTENEGRO")) %>% 
#   bind_rows(etranger %>% filter(PAYSOK1=="YOUGOSLAVIE") %>% mutate(PAYSOK="KOSOVO")) %>% 
#   bind_rows(etranger %>% filter(str_detect(PAYSOK1,"TCHECO|SERBIEMON|SERBIEET")) %>% 
#               mutate(PAYSOK=case_when(
#                 str_detect(PAYSOK1,"TCHECO") ~ "SLOVAQUIE",
#                 str_detect(PAYSOK1,"SERBIEMON|SERBIEET") ~ "MONTENEGRO",
#                 TRUE ~ "PAYSOK"
#               ))) %>% 
#   count(PAYSOK, COMMOK,ANAIS=substr(datenaiss,1,4)) %>% 
#   arrange(desc(n))
# 

base_complete <- readRDS("sorties/base_complete.RDS")

etranger <- readRDS("donnees/etranger.RDS")

historique <- etranger %>% 
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
  count(PAYSOK, ANAIS=substr(datenaiss,1,4)) %>% 
  left_join(base_complete %>% count(PAYSOK),by=c("PAYSOK"))



graph_chronologie <- function(PAYS){

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
  
  
  historique %>% 
    filter(PAYSOK==PAYS ) %>% 
    mutate(an20=as.integer(ANAIS)+25,
           geocodage=as.factor(if_else(is.na(display_name),"0","1"))) %>%
    count(an20,geocodage,wt=n.x) %>% 
    filter(an20>1800 & an20<2025) %>% 
    ggplot(aes(x=an20,y=n,group=geocodage))+
    geom_line(aes(color=geocodage),size=1,alpha=.7) +
    annotate("rect", xmin=1900, xmax=1930, ymin=0, ymax=Inf, alpha=0.2, fill="gray80",color=NA)+ 
    annotate("rect", xmin=1960, xmax=2000, ymin=0, ymax=Inf, alpha=0.2, fill="gray80",color=NA)+ 
    # annotate("text", x=1920,y=mean(n),
    #          label="Les données non géocodées portent sur un territoire\nplus étendu que la pays actuel.",
    #          color=
    #            if(PAYS %in% c("ALBANIE","SERBIE","SLOVAQUIE","CROATIE","MACEDOINE","SLOVENIE",
    #                           "BOSNIEHERZEGOVINE","REPUBLIQUETCHEQUE","MONTENEGRO","KOSOVO")) {
    #              "gray10"
    #            } else {
    #              "transparent"
    #            }
    #          
    # )+
    # geom_rect(aes(xmin = 1900, xmax = 1925,
    #               ymin = 0, ymax = max(n)),
    #           colour=NA,fill="gray80",alpha=.5)+
    scale_color_manual(name="Géocodage",
                       labels=c("0"="Echec","1"="Réussite"),
                       values=c("0"="coral2","1"="darkolivegreen3"))+
    scale_x_continuous(breaks = c(1900,1920,1930,1940,1950,1960,1980,2000),
                       limits = c(1900,2000))+
    labs(title = paste0("Venus ", 
                        if (PAYS %in% c("ISLANDE","ESPAGNE","ALLEMAGNE","ITALIE","AUTRICHE","ARMENIE","IRLANDE",
                                        "ALBANIE","UKRAINE","ANDORRE","ESTONIE","ALGERIE")){
                          "d\'"
                        }else if (PAYS %in% c("PORTUGAL","LUXEMBOURG","DANEMARK","MONTENEGRO","KOSOVO", "ROYAUMEUNI",
                                              "MAROC")){
                          "du "
                        }else if (PAYS %in% c( "PAYSBAS")){
                          "des "
                        }else {
                          "de "
                        }
                        ,str_to_title(PAYS2),", ils avaient 25 ans en ..."),
         subtitle=paste0("Nombre de personnes nées ",
                         if (PAYS %in% c("KOSOVO","DANEMARK","MONTENEGRO","LUXEMBOURG","PORTUGAL")) {
                           "au"
                         } else if (PAYS %in% c("PAYSBAS")) {
                           "aux"
                         } else {
                           "en"
                         }
                         ," ",
                         str_to_title(PAYS2),
                         " et décédées en France de 1970 à 2020,\nselon l'année de leur 25 ans et la qualité du géocodage."),
         x="Année des 25 ans",
         y="Effectif",
         caption="Source: Insee, état civil, décès 1970-2020\nGéocodage via Nominatim (contributions OpenStreetMap)\nTraitements et erreurs: @Re_Mi_La")+
    theme_minimal()+
    theme(legend.position = "bottom",
           text = element_text(size=6,family = "Calibri"),
           plot.title = element_text(face="bold",size=10),
           plot.subtitle = element_text(size=6),
          #plot.caption = element_text(face = "italic",size=4),
           legend.title = element_text(size=6),
          legend.text = element_text(size=6),
          plot.background = element_rect(fill="white",color="white")
    )
  ggsave(paste0("sorties/chronologie_",PAYS,".jpeg"),width=10,height=8,dpi=300,units = "cm")
  
}

graph_chronologie("ITALIE")

liste_pays <- c("ISLANDE","ESPAGNE","ALLEMAGNE","ITALIE","AUTRICHE","ARMENIE","IRLANDE",
                "ALBANIE","UKRAINE","ANDORRE","ESTONIE","ALGERIE",
                
                "SUISSE","TURQUIE","HONGRIE","GRECE","POLOGNE","BELGIQUE","ROUMANIE","RUSSIE",
                "BULGARIE","SERBIE","SLOVAQUIE","SUEDE","LETTONIE","NORVEGE", "FINLANDE",
                "CROATIE","LITUANIE","MACEDOINE","CHYPRE","MOLDAVIE","MALTE","SLOVENIE",
                "SAINTMARIN","BOSNIEHERZEGOVINE","REPUBLIQUETCHEQUE","BIELORUSSIE","TUNISIE",
                
                "PORTUGAL","LUXEMBOURG","DANEMARK","MONTENEGRO","KOSOVO", "ROYAUMEUNI",
                "MAROC",
                
                "PAYSBAS")

walk(liste_pays,graph_chronologie)       



saveRDS(historique,"donnees/historique.RDS")

chronologies <- historique %>% 
  filter(PAYSOK %in% c("POLOGNE","ESPAGNE","MAROC","AUTRICHE")) %>% 
  mutate(an20=as.integer(ANAIS)+25,
         geocodage=as.factor(if_else(is.na(display_name),"0","1"))) %>%
  count(PAYSOK,an20,geocodage,wt=n.x) %>% 
  filter(an20>1800 & an20<2025) %>% 
  ggplot(aes(x=an20,y=n,group=geocodage))+
  geom_line(aes(color=geocodage),size=1,alpha=.7) +
  facet_wrap(~PAYSOK,scales = "free_y")+
  annotate("rect", xmin=1900, xmax=1930, ymin=0, ymax=Inf, alpha=0.2, fill="gray80",color=NA)+ 
  annotate("rect", xmin=1960, xmax=2000, ymin=0, ymax=Inf, alpha=0.2, fill="gray80",color=NA)+ 
  scale_color_manual(name="Géocodage",
                     labels=c("0"="Echec","1"="Réussite"),
                     values=c("0"="coral2","1"="darkolivegreen3"))+
  scale_x_continuous(breaks = c(1900,1920,1930,1940,1950,1960,1980,2000),
                     limits = c(1900,2000))+
  labs(title = "Venus d'Autriche, d'Espagne, du Maroc ou de Pologne, ils avaient 25 ans en ...",
       subtitle="Nombre de personnes nées dans l'un de ces pays et décédées en France de 1970 à 2020,\nselon l'année de leur 25 ans et la qualité du géocodage.",
       x="Année des 25 ans",
       y="Effectif",
       caption="Source: Insee, état civil, décès 1970-2020\nGéocodage via Nominatim (contributions OpenStreetMap)\nTraitements et erreurs: @Re_Mi_La")+
  theme_minimal()+
  theme(legend.position = "bottom",
        text = element_text(size=10,family = "Calibri"),
        plot.title = element_text(face="bold",size=14),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(face = "italic",size=6),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8)
  )

library(plotly)
ggplotly(chronologies)
