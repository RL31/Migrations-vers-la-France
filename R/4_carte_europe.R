library(tidyverse)
library(sf)
#library(stringi)
#install.packages("plotly", dependencies = T)
library(plotly)
library(extrafont)
loadfonts(device = "win")

# carte par pays, centr?e Europe
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

saveRDS(base_complete,"sorties/base_complete.RDS")

fond <- st_read("donnees/CNTR_RG_10M_2020_3035.shp")
limites_europe <- fond %>% filter(NAME_ENGL %in% c("Iceland","Norway","Greece")) %>% st_bbox()

taux_pays <- base_complete %>% 
  as.data.frame() %>% 
  summarise(n_traite=sum(n)) %>% 
  bind_cols(etranger %>% filter(PAYSOK  %in% liste_pays |
                                  PAYSOK %in% c("PAYS-BAS","PAYS BAS",
                                                "SAINT-MARIN","SAINT MARIN",
                                                "BOSNIE-HERZEGOVINE","BOSNIE HERZEGOVINE",
                                                "ROYAUME-UNI","ROYAUME UNI",
                                                "REPUBLIQUE TCHEQUE","REPUBLIQUE TCHEQUE")) %>% 
              summarise(n_initial=sum(n))) %>% 
  mutate(resolu=round(n_traite/n_initial*100),0) %>% 
  select(resolu) %>% 
  as.numeric()

lien_id <- read.csv("donnees/libelle_pays.csv") %>% 
  mutate(PAYSOK=  trimws(PAYSOK,"right")) %>% 
  filter(PAYSOK %in% liste_pays) %>% 
  select(CNTR_ID) %>% 
  pull()

carte_europe<-ggplot()+
  geom_sf(data=fond,aes(fill=(CNTR_ID %in% lien_id)),colour="gray80")+#fill="gray95")+#
  geom_point(data=base_complete  %>% st_drop_geometry()%>% as.data.frame() %>% arrange(desc(n)),
             aes(x=x,y=y,size=n),color="tomato1",alpha=.5)+
  # geom_sf_text(data=base_pays %>% 
  #                slice_head(order_by=n,n=10) %>% 
  #                separate(display_name,into="label",sep=",",remove=TRUE,extra="drop"),
  #              aes(label=label),#,"^[:graph:]+(?=,)")),
  #              size=4,
  #              check_overlap = TRUE,
  #              vjust=-1)+
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
  labs(title = paste0("?tre n? en Europe"),
       subtitle = paste0("Lieux de naissance des personnes n?es en Europe et d?c?d?es en France de 1970 ? 2019.\n",
                         "00",#taux_pays,
                         " % des naissances ont pu ?tre g?ocod?es."),
       caption="Source: Insee, ?tat civil\nTraitements et erreurs: @Re_Mi_La")+
  theme_void()+
  theme(text = element_text(family = "Calibri"),
        plot.title = element_text(face="bold",size=17),
        plot.subtitle = element_text(size=9),
        panel.background = element_rect(fill="lightcyan",colour = NA),
        plot.caption = element_text(face = "italic",size=7),
        legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))+
  guides(fill="none")

ggsave(file = "sorties/europe2.png",
         width = 15,
         height = 19,
         units = "cm",
         dpi = 300)
saveRDS(carte_europe,"/Rmarkdown/carte_europe.RDS")











base_complete<-readRDS("sorties/base_complete.RDS")

liste_pays <- c("ISLANDE","ESPAGNE","SUISSE","TURQUIE","HONGRIE","GRECE","PORTUGAL",
                "POLOGNE","ALLEMAGNE","ITALIE","BELGIQUE","AUTRICHE",
                "ROUMANIE","RUSSIE","LUXEMBOURG","BULGARIE",
                "SERBIE","ARMENIE","SLOVAQUIE","DANEMARK","IRLANDE","SUEDE","LETTONIE","NORVEGE",
                "FINLANDE","MONTENEGRO","ALBANIE",
                "UKRAINE","CROATIE","LITUANIE","ANDORRE","MACEDOINE","CHYPRE","ESTONIE","MOLDAVIE",
                "MALTE","SLOVENIE","KOSOVO", "SAINTMARIN","BOSNIEHERZEGOVINE","ROYAUMEUNI","PAYSBAS",
                "REPUBLIQUETCHEQUE","BIELORUSSIE","ALGERIE","TUNISIE","MAROC"
)
fond <- st_read("donnees/CNTR_RG_10M_2020_3035.shp",
                quiet=TRUE)
limites_europe <- fond %>% filter(NAME_ENGL %in% c("Iceland","Norway","Greece")) %>% st_bbox()

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
                         "00",#taux_pays,
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
        legend.text = element_text(size=10))+
  guides(fill="none")
