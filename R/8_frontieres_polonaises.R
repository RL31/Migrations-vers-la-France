library(tidyverse)
library(tidygeocoder)
library(sf)

# Reprise du fichier des naissances à l'étranger pour le préparer avant le géocodage
# notamment : duplication de certains lignes pour les pays divisés en plusieurs depuis
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
                TRUE ~ "PAYSOK"))) %>% 
  mutate(ANAIS=substr(datenaiss,1,4)) %>% 
  count(PAYSOK, COMMOK,ANAIS) %>% 
  arrange(desc(n))

avec_annees <- etranger2 %>% 
  filter(PAYSOK %in% c("ALLEMAGNE","POLOGNE"))



# on ne garde que Allemagne et Pologne
allemagne_osm <- readRDS(paste0("donnees/osm_","ALLEMAGNE","2.RDS")) %>% 
  filter(is.na(display_name))

pologne_osm <- readRDS(paste0("donnees/osm_","POLOGNE","2.RDS")) %>% 
  filter(is.na(display_name))

base_geocodage_2 <- allemagne_osm %>% select(PAYSOK,COMMOK,n) %>% mutate(PAYSOK="POLOGNE") %>% 
  bind_rows(pologne_osm %>% select(PAYSOK,COMMOK,n) %>% mutate(PAYSOK="LITUANIE")) %>% 
  bind_rows(pologne_osm %>% select(PAYSOK,COMMOK,n) %>% mutate(PAYSOK="BIELORUSSIE")) %>% 
  bind_rows(pologne_osm %>% select(PAYSOK,COMMOK,n) %>% mutate(PAYSOK="UKRAINE"))


# Liste des pays pour lesquels on veut géocoder les communes de naissance
liste_pays <- c("POLOGNE","UKRAINE","LITUANIE","BIELORUSSIE")

geocodage <- function(PAYS){
  
  osm <- base_geocodage_2 %>%
    filter( PAYSOK==PAYS) %>%
    geocode(city = COMMOK,
            country = PAYSOK, method = "osm",
            full_results=TRUE)
  saveRDS(osm,paste0("donnees/osm_",PAYS,"_pologne.RDS"))
  rm(osm)
}

walk(liste_pays,geocodage)


# regroupement des donnees
donnees_carte_pologne <- readRDS("donnees/osm_POLOGNE_pologne.RDS") %>% mutate(PAYSOK="ALLEMAGNE") %>% 
  bind_rows(readRDS("donnees/osm_LITUANIE_pologne.RDS")) %>% 
  bind_rows(readRDS("donnees/osm_BIELORUSSIE_pologne.RDS")) %>%
  bind_rows(readRDS("donnees/osm_UKRAINE_pologne.RDS")) %>% 
  bind_rows(readRDS("donnees/osm_POLOGNE2.RDS")) %>% 
  filter(!is.na(display_name)) %>% 
  mutate(PAYS_initial = case_when(PAYSOK %in% c("LITUANIE","BIELORUSSIE","UKRAINE") ~ "POLOGNE",
                                  TRUE ~ PAYSOK)) %>% 
  select(-n) %>% 
  inner_join(avec_annees %>% mutate(periodes = case_when(ANAIS <= 1939 ~ "1",
                                                         ANAIS > 1939 & ANAIS <= 1945 ~ "2",
                                                         ANAIS > 1945 ~ "3")),
             by=c("PAYS_initial"="PAYSOK", "COMMOK"="COMMOK")) %>% 
  count(long,lat,display_name,PAYSOK,periodes,type,wt=n) %>%  #COMMOK,class,
  st_as_sf(coords=c("long","lat"),crs=4326) %>% 
  st_transform(2180) %>% 
  mutate(y=unlist(map(.$geometry,2)),
         x=unlist(map(.$geometry,1))) 

# carte qui représente les naissances sur le territoire polonais, actuel et ancien

# Fond Eurostat
fond <- st_read("donnees/CNTR_RG_10M_2020_3035.shp") %>% 
  st_transform(2180)# remplacer par giscoR ?

# Pour faire le lien entre le nom du pays Insee et le nom Eurostat
lien_id <- read.csv("donnees/libelle_pays.csv")

limites <- fond %>% filter(CNTR_ID %in% c("PL","CZ","LT")) %>% st_bbox() #BY

# liste_pays <- c("ISLANDE","ESPAGNE","SUISSE","TURQUIE","HONGRIE","GRECE","PORTUGAL",
#                 "POLOGNE","ALLEMAGNE","ITALIE","BELGIQUE","AUTRICHE",
#                 "ROUMANIE","RUSSIE","LUXEMBOURG","BULGARIE",
#                 "SERBIE","ARMENIE","SLOVAQUIE","DANEMARK","IRLANDE","SUEDE","LETTONIE","NORVEGE",
#                 "FINLANDE","MONTENEGRO","ALBANIE",
#                 "UKRAINE","CROATIE","LITUANIE","ANDORRE","MACEDOINE","CHYPRE","ESTONIE","MOLDAVIE",
#                 "MALTE","SLOVENIE","KOSOVO", "SAINTMARIN","BOSNIEHERZEGOVINE","ROYAUMEUNI","PAYSBAS",
#                 "REPUBLIQUETCHEQUE","BIELORUSSIE","ALGERIE","TUNISIE","MAROC"
# )
# 
# europe <- function(PAYS){
#   print(PAYS)
#   base_pays <- readRDS(paste0("donnees/osm_",PAYS,"2.RDS")) %>% 
#     filter(!is.na(display_name)) %>% 
#     count(long,lat,display_name,PAYSOK,COMMOK,wt=n) %>% 
#     st_as_sf(coords=c("long","lat"),crs=4326) %>% 
#     st_transform(2180) %>% 
#     mutate(y=unlist(map(.$geometry,2)),
#            x=unlist(map(.$geometry,1)),
#            PAYSOK=PAYS) #%>% 
#   #select(x,y,n,PAYSOK,display_name)
# }
# base_complete <- map_dfr(liste_pays,europe)

# donnees_carte_pologne  %>% 
#   st_drop_geometry()%>% 
#   as.data.frame() %>%
#   count(type)


ggplot()+
  geom_sf(data=fond,colour="gray80",fill="gray99")+#,fill="gray80"
  # geom_point(data=base_complete  %>% st_drop_geometry()%>% as.data.frame() %>% arrange(desc(n)) %>% filter(PAYSOK!="POLOGNE"),
  #            aes(x=x,y=y,size=n),color="gray60",alpha=.5)+
  geom_point(data=donnees_carte_pologne  %>% 
               st_drop_geometry()%>%
               as.data.frame() %>%
               filter(periodes=="Deuxième guerre mondiale") %>%
               arrange(desc(n)) %>%
               filter(PAYSOK=="POLOGNE") ,
             aes(x=x,y=y,size=n,color=PAYSOK),alpha=.8)+
  geom_point(data=donnees_carte_pologne  %>% 
               st_drop_geometry()%>% 
               as.data.frame() %>% 
               filter(periodes=="Deuxième guerre mondiale") %>%
               arrange(desc(n)) %>%
               filter(PAYSOK!="POLOGNE" & type == "administrative") ,
             aes(x=x,y=y,size=n,color=PAYSOK),alpha=.8)+
  geom_sf_text(data=donnees_carte_pologne %>% 
                 filter(periodes=="Deuxième guerre mondiale") %>%
                 filter(PAYSOK!="POLOGNE") %>% 
                 slice_max(order_by=n,n=10) %>% 
                 separate(display_name,into="label",sep=",",remove=TRUE,extra="drop"),
               aes(label=label),#,"^[:graph:]+(?=,)")),
               size=4,
               check_overlap = TRUE,
               vjust=-1)+
  scale_fill_manual(name="",values=c("FALSE"="gray95","TRUE"="gray99"))+
  scale_color_manual(name="Naissance",values=c("POLOGNE"="burlywood2",
                                               "LITUANIE"="#800026",
                                               "BIELORUSSIE"="#e31a1c",
                                               "UKRAINE"="#fd8d3c",
                                               "ALLEMAGNE"="#238443"),
                     labels=c("POLOGNE"="en Pologne actuelle",
                              "LITUANIE"="................... historique, actuelle Lituanie",
                              "UKRAINE"="................... historique, actuelle Ukraine",
                              "BIELORUSSIE"="................... historique, actuelle Biélorussie",
                              "ALLEMAGNE"="en Allemagne, actuelle Pologne"))+
  scale_size_continuous(name="Effectifs",
                        range = c(10*donnees_carte_pologne  %>% st_drop_geometry()%>% as.data.frame() %>%   filter(periodes=="Deuxième guerre mondiale") %>%
                                    summarise(min=min(n)) %>% select(min) %>% pull()/
                                    donnees_carte_pologne  %>% st_drop_geometry()%>% as.data.frame() %>%   filter(periodes=="Deuxième guerre mondiale") %>%
                                    summarise(max=max(n)) %>% select(max) %>% pull() , 20),
                        breaks = c(10000,5000,1000,100,10)) +
  coord_sf(xlim =c(150000,limites$xmax),
           ylim =c(limites$ymin,limites$ymax),
           expand = TRUE) +
  labs(title = "Le géocodage et ses limites : quand les frontières se déplacent",
       subtitle = "Commune de naissance des personnes nées sur le territoire polonais actuel ou historique et décédées en France de 1970 à 2020",
       caption="Source: Insee, état civil, décès 1970-2020\nGéocodage via Nominatim (contributions OpenStreetMap)\nTraitements et erreurs: @Re_Mi_La")+
  theme_void()+
  theme(text = element_text(family = "Calibri"),
        plot.title = element_text(face="bold",size=17),
        plot.caption.position = "plot",
        plot.subtitle = element_text(size=12),
        panel.background = element_rect(fill="lightcyan",colour = NA),
        plot.background = element_rect(fill="white",color="white"),
        plot.caption = element_text(face = "italic",size=10),
        legend.position = "right",
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))+
  guides(color= guide_legend(title.position = "top",
                             ncol=1,bycol=TRUE,
                             override.aes =list(size=6)),
         size=guide_legend(title.position = "top",
                           ncol=1,bycol=TRUE,
                           override.aes =list(color="gray70")))

ggsave(file = "sorties/frontieres_polonaises.jpeg",
       width = 25,
       height = 17,
       units = "cm",
       dpi = 300)





ggplot()+
  geom_sf(data=fond,colour="gray80",fill="gray99")+#,fill="gray80"
  # geom_point(data=base_complete  %>% st_drop_geometry()%>% as.data.frame() %>% arrange(desc(n)) %>% filter(PAYSOK!="POLOGNE"),
  #            aes(x=x,y=y,size=n),color="gray60",alpha=.5)+
  # geom_point(data=donnees_carte_pologne  %>% 
  #              st_drop_geometry()%>%
  #              as.data.frame() %>%
  #              arrange(desc(n)) %>%
  #              filter(PAYSOK=="POLOGNE") ,
  #            aes(x=x,y=y,size=n,color=periodes),alpha=.8)+
  geom_point(data=donnees_carte_pologne  %>% 
               st_drop_geometry()%>% 
               as.data.frame() %>% 
               arrange(desc(n)) %>%
               filter(PAYSOK!="ALLEMAGNE" & type == "administrative") ,
             aes(x=x,y=y,size=n),color="#fd8d3c",alpha=.8)+
  geom_sf_text(data=donnees_carte_pologne %>% 
                 # filter(PAYSOK!="POLOGNE") %>% 
                 slice_max(order_by=n,n=10) %>% 
                 separate(display_name,into="label",sep=",",remove=TRUE,extra="drop"),
               aes(label=label),#,"^[:graph:]+(?=,)")),
               size=4,
               check_overlap = TRUE,
               vjust=-1)+
  facet_wrap(~periodes,labeller = labeller(periodes=c("1"="Avant 1939","2"="Deuxième guerre mondiale","3"="Après 1945")) )+
  scale_fill_manual(name="",values=c("FALSE"="gray95","TRUE"="gray99"))+
  # scale_color_manual(name="Périodes",
  #                    labels=c("1"="Avant 1939",
  #                             "2"="Deuxième guerre mondiale",
  #                             "3"="Après 1945"),
  #                    values=c("1"="#800026",
  #                             "2"="#e31a1c",
  #                             "3"="#fd8d3c"))+
  scale_size_continuous(name="Effectifs",
                        range = c(10*donnees_carte_pologne  %>% st_drop_geometry()%>% as.data.frame() %>%
                                    summarise(min=min(n)) %>% select(min) %>% pull()/
                                    donnees_carte_pologne  %>% st_drop_geometry()%>% as.data.frame() %>% 
                                    summarise(max=max(n)) %>% select(max) %>% pull() , 20),
                        breaks = c(10000,5000,1000,100,10)) +
  coord_sf(xlim =c(150000,limites$xmax),
           ylim =c(limites$ymin,limites$ymax),
           expand = TRUE) +
  labs(title = "Le géocodage et ses limites : quand les frontières se déplacent",
       subtitle = "Commune de naissance des personnes nées en Pologne et décédées en France de 1970 à 2020\n   \n ",
       caption="Source: Insee, état civil, décès 1970-2020\nGéocodage via Nominatim (contributions OpenStreetMap)\nTraitements et erreurs: @Re_Mi_La")+
  theme_void()+
  theme(text = element_text(family = "Calibri"),
        plot.title = element_text(face="bold",size=17),
        plot.caption.position = "plot",
        plot.subtitle = element_text(size=12),
        panel.background = element_rect(fill="lightcyan",colour = NA),
        plot.background = element_rect(fill="white",color="white"),
        plot.caption = element_text(face = "italic",size=10),
        legend.position = "bottom",
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))+
  guides(color= guide_legend(title.position = "top",
                             ncol=1,bycol=TRUE,
                             override.aes =list(size=6)),
         size=guide_legend(title.position = "left",
                           ncol=3,bycol=TRUE))

library(gganimate)  
