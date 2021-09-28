library(tidyverse)
library(sf)
library(extrafont)
#loadfonts(device = "win")

# Création d'une base regroupant tous les pays
# 
liste_pays <- c("ISLANDE","ESPAGNE","SUISSE","TURQUIE","HONGRIE","GRECE","PORTUGAL",
                "POLOGNE","ALLEMAGNE","ITALIE","BELGIQUE","AUTRICHE",
                "ROUMANIE","RUSSIE","LUXEMBOURG","BULGARIE",
                "SERBIE","ARMENIE","SLOVAQUIE","DANEMARK","IRLANDE","SUEDE","LETTONIE","NORVEGE",
                "FINLANDE","MONTENEGRO","ALBANIE",
                "UKRAINE","CROATIE","LITUANIE","ANDORRE","MACEDOINE","CHYPRE","ESTONIE","MOLDAVIE",
                "MALTE","SLOVENIE","KOSOVO", "SAINTMARIN","BOSNIEHERZEGOVINE","ROYAUMEUNI","PAYSBAS",
                "REPUBLIQUETCHEQUE","BIELORUSSIE","ALGERIE","TUNISIE","MAROC"
)

europe <- function(PAYS){
  print(PAYS)
  base_pays <- readRDS(paste0("donnees/osm_",PAYS,"2.RDS")) %>% 
    filter(!is.na(display_name)) %>% 
    count(long,lat,display_name,PAYSOK,COMMOK,wt=n) %>% 
    st_as_sf(coords=c("long","lat"),crs=4326) %>% 
    st_transform(3035) %>% 
    mutate(y=unlist(map(.$geometry,2)),
           x=unlist(map(.$geometry,1)),
           PAYSOK=PAYS) #%>% 
  #select(x,y,n,PAYSOK,display_name)
}
base_complete <- map_dfr(liste_pays,europe)

#saveRDS(base_complete,"sorties/base_complete.RDS")

# Fond Eurostat
fond <- st_read("donnees/CNTR_RG_10M_2020_3035.shp") # remplacer par giscoR ?

# Pour faire le lien entre le nom du pays Insee et le nom Eurostat
lien_id <- read.csv("donnees/libelle_pays.csv")

# Fonction qui traite chaque pays un à un
carte_pays2 <- function(PAYS){
  
  # définition des limites de la zone à cartographier
  ID <- lien_id %>% mutate(PAYSOK=trimws(PAYSOK,"right") ) %>% filter(PAYSOK==PAYS) %>% select(CNTR_ID) %>% pull() %>% as.character()
  print(ID)
  limites <- fond %>% filter(CNTR_ID==ID) %>% st_bbox()
  
  if (ID =="ES") {
    limites$xmin <- 2600000
    limites$ymin <- 1500000
  } else if (ID=="PT"){
    limites$xmin <- 2600000
    limites$ymin <- 1700000
    limites$ymax <- 2300000
  }else if (PAYS=="KOSOVO"){
    limites$xmin <- 5008887
    limites$xmax <- 5375158
    limites$ymin <- 2147880
    limites$ymax <- 2614333
  }
  
  # Virer ?a et utiliser base_comlete
  # base_pays <- readRDS(paste0("donnees/osm_",PAYS,"2.RDS")) %>% 
  #   filter(!is.na(display_name)) %>% 
  #   count(long,lat,display_name,wt=n) %>% 
  #   st_as_sf(coords=c("long","lat"),crs=4326) %>% 
  #   st_transform(3035) %>% 
  #   mutate(y=unlist(map(.$geometry,2)),
  #          x=unlist(map(.$geometry,1))) %>% 
  #   select(x,y,n,display_name)
  # 
  base_pays <- base_complete %>% 
    filter(PAYSOK==PAYS)
  
  # Calcul du taux de réussite du géocodage
  taux_pays <- readRDS(paste0("donnees/osm_",PAYS,"2.RDS")) %>%
    summarise(n_initial=sum(n)) %>% 
    bind_cols(base_pays %>% summarise(n_traite=sum(n))) %>% 
    mutate(resolu=round(n_traite/n_initial*100),0) %>% 
    select(resolu) %>% 
    as.numeric()
  
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
  
  ggplot()+
    geom_sf(data=fond,aes(fill=(CNTR_ID==ID)),colour="gray80")+#fill="gray95")+#
    geom_point(data=base_complete  %>% st_drop_geometry()%>% as.data.frame() %>% arrange(desc(n)),
               aes(x=x,y=y,size=n,color=(PAYSOK==PAYS3 | PAYSOK==PAYS | PAYSOK==PAYS2)),alpha=.5)+
    geom_sf_text(data=base_pays %>% 
                   slice_max(order_by=n,n=10) %>% 
                   separate(display_name,into="label",sep=",",remove=TRUE,extra="drop"),
                 aes(label=label),#,"^[:graph:]+(?=,)")),
                 size=4,
                 check_overlap = TRUE,
                 vjust=-1)+
    scale_fill_manual(name="",values=c("FALSE"="gray95","TRUE"="gray99"))+
    scale_color_manual(name="",values=c("TRUE"="tomato1","FALSE"="gray60"))+
    scale_size_continuous(name="Nombre de naissances",
                          range = c(10*base_pays  %>% st_drop_geometry()%>% as.data.frame() %>%
                                      summarise(min=min(n)) %>% select(min) %>% pull()/
                                      base_pays  %>% st_drop_geometry()%>% as.data.frame() %>%
                                      summarise(max=max(n)) %>% select(max) %>% pull() , 20),
                          breaks = c(10000,5000,1000,100,10)) +
    coord_sf(xlim =c(limites$xmin,limites$xmax),
             ylim =c(limites$ymin,limites$ymax),
             expand = TRUE) +
    labs(title = paste0("Être né ",
                        if (PAYS %in% c("KOSOVO","DANEMARK","MONTENEGRO","LUXEMBOURG","PORTUGAL","MAROC")) {
                          "au"
                        } else if (PAYS %in% c("PAYSBAS")) {
                          "aux"
                        } else {
                          "en"
                        }
                        ," ",str_to_title(PAYS2)),
         subtitle = paste0("Lieux de naissance des personnes nées ",
                           if (PAYS %in% c("KOSOVO","DANEMARK","MONTENEGRO","LUXEMBOURG","PORTUGAL")) {
                             "au"
                           } else if (PAYS %in% c("PAYSBAS")) {
                             "aux"
                           } else {
                             "en"
                           }
                           ," ",
                           str_to_title(PAYS2),
                           " et décédées en France de 1970 à 2020.\n",
                           taux_pays,
                           " % des naissances ont été géocodées."),
         caption="Source: Insee, état civil, décès 1970-2020\nGéocodage via Nominatim (contributions OpenStreetMap)\nTraitements et erreurs: @Re_Mi_La")+
    theme_void()+
    theme(text = element_text(family = "Calibri"),
          plot.title = element_text(face="bold",size=17),
          plot.subtitle = element_text(size=9),
          panel.background = element_rect(fill="lightcyan",colour = NA),
          plot.background = element_rect(fill="white",color="white"),
          plot.caption = element_text(face = "italic",size=7),
          legend.position = "bottom",
          legend.title = element_text(size=10),
          legend.text = element_text(size=10))+
    guides(fill="none",color= "none",
           size=guide_legend(override.aes =list(color="tomato1")))
  
  ggsave(file = paste0("sorties/",PAYS,"2.jpeg"),
         width = 15,
         height = 19,
         units = "cm",
         dpi = 300)
}

walk(liste_pays,carte_pays2)

carte_pays2("ESPAGNE")
carte_pays2("POLOGNE")
carte_pays2("ALLEMAGNE")
carte_pays2("ITALIE")
carte_pays2("ALGERIE")






# Pour la carte europe entiere

limites_europe <- fond %>% filter(NAME_ENGL %in% c("Iceland","Norway","Greece")) %>% st_bbox()


taux_pays <- base_complete %>% 
  as.data.frame() %>% 
  summarise(n_traite=sum(n)) %>% 
  bind_cols(readRDS("donnees/etranger.RDS") %>% 
              filter(PAYSOK  %in% liste_pays | PAYSOK %in% c("PAYS-BAS","PAYS BAS",
                                                             "SAINT-MARIN","SAINT MARIN",
                                                             "BOSNIE-HERZEGOVINE","BOSNIE HERZEGOVINE",
                                                             "ROYAUME-UNI","ROYAUME UNI",
                                                             "REPUBLIQUE TCHEQUE","REPUBLIQUE TCHEQUE")) %>% 
              count(PAYSOK, COMMOK) %>% 
              summarise(n_initial=sum(n))) %>% 
  mutate(resolu=round(n_traite/n_initial*100),0) %>% 
  select(resolu) %>% 
  as.numeric()


lien_id <- read.csv("donnees/libelle_pays.csv") %>% 
  mutate(PAYSOK=  trimws(PAYSOK,"right")) %>% 
  filter(PAYSOK %in% liste_pays) %>% 
  select(CNTR_ID) %>% 
  pull()

ggplot()+
  geom_sf(data=fond,aes(fill=(CNTR_ID %in% lien_id)),colour="gray80")+#fill="gray95")+#
  geom_point(data=base_complete  %>% st_drop_geometry()%>% as.data.frame() %>% arrange(desc(n)),
             aes(x=x,y=y,size=n),color="tomato1",alpha=.5)+
  scale_fill_manual(name="",values=c("FALSE"="gray95","TRUE"="gray99"))+
  scale_size_continuous(name="Nombre de naissances",
                        range = c(0.01*base_complete  %>% st_drop_geometry()%>% as.data.frame() %>%
                                    summarise(min=min(n)) %>% select(min) %>% as.matrix()/
                                    base_complete  %>% st_drop_geometry()%>% as.data.frame() %>%
                                    summarise(max=max(n)) %>% select(max) %>% as.matrix() , 5),
                        breaks = c(10000,5000,1000,100)) +
  coord_sf(xlim =c(limites_europe$xmin,limites_europe$xmax),
           ylim =c(limites_europe$ymin,limites_europe$ymax),
           expand = TRUE) +
  labs(title = paste0("Être né en Europe"),
       subtitle = paste0("Lieux de naissance des personnes nées en Europe et décédées en France de 1970 à 2019.\n",
                         taux_pays,
                         " % des naissances ont pu être géocodées."),
       caption="Source: Insee, état civil\nTraitements et erreurs: @Re_Mi_La")+
  theme_void()+
  theme(text = element_text(family = "Calibri"),
        plot.title = element_text(face="bold",size=17),
        plot.subtitle = element_text(size=9),
        panel.background = element_rect(fill="lightcyan",colour = NA),
        plot.caption = element_text(face = "italic",size=7),
        legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        plot.background = element_rect(fill="white",color="white"))+
  guides(fill="none")


ggsave(file = "sorties/europe.jpg",
       width = 15,
       height = 19,
       units = "cm",
       dpi = 300)
