library(tidyverse)
library(extrafont)
loadfonts(dev="win")

# On essaie ici de circonscrire la période de "fiabilité" des données sur le
# sujet des migrations

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
  count(PAYSOK, COMMOK, ANAIS=substr(datenaiss,1,4)) %>% 
  left_join(base_complete %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              select(PAYSOK,COMMOK) %>% 
              mutate(geocodage=1),by=c("PAYSOK","COMMOK")) %>% 
  mutate(geocodage=if_else(is.na(geocodage),0,geocodage))


# Fonction qui produit un graphique par pays :
# de 1900 à 2000, combien de "migrants" décédés avaient 25 ans pour chaque année donnée
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
    mutate(an20=as.integer(ANAIS)+25) %>%
    count(an20,geocodage,wt=n) %>% 
    filter(an20>1800 & an20<2025) %>% 
    ggplot(aes(x=an20,y=n,group=geocodage))+
    geom_line(aes(color=as.factor(geocodage)),size=1,alpha=.7) +
    annotate("rect", xmin=1900, xmax=1930, ymin=0, ymax=Inf, alpha=0.2, fill="gray80",color=NA)+ 
    annotate("rect", xmin=1960, xmax=2000, ymin=0, ymax=Inf, alpha=0.2, fill="gray80",color=NA)+ 
    annotate("text", x=1920,y=mean(n),
             label="Les données non géocodées portent sur un territoire\nplus étendu que la pays actuel.",
             color=
               if(PAYS %in% c("ALBANIE","SERBIE","SLOVAQUIE","CROATIE","MACEDOINE","SLOVENIE",
                              "BOSNIEHERZEGOVINE","REPUBLIQUETCHEQUE","MONTENEGRO","KOSOVO")) {
                 "gray10"
               } else {
                 "transparent"
               }

    )+
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
          # plot.caption = element_text(face = "italic",size=4),
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


# Assemblage pour 4 pays intéressants
chronologies <- historique %>% 
  filter(PAYSOK %in% c("POLOGNE","ESPAGNE","MAROC","AUTRICHE")) %>% 
  mutate(an20=as.integer(ANAIS)+25) %>%
  count(PAYSOK,an20,geocodage,wt=n) %>% 
  filter(an20>1800 & an20<2025) %>% 
  ggplot(aes(x=an20,y=n,group=geocodage))+
  geom_line(aes(color=as.factor(geocodage)),size=1,alpha=.7) +
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
        legend.text = element_text(size=8),
        plot.background = element_rect(fill="white",color="white")
  )


ggsave(paste0("sorties/chronologie_4.jpeg"),width=20,height=16,dpi=300,units = "cm")


# library(plotly)
# ggplotly(chronologies)
