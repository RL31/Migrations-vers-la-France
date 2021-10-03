library(tidyverse)
library(sf)

# Cette fois, analyse de flux importants mais très localisés

etranger <- readRDS("donnees/etranger.RDS")

etranger_flux <- etranger %>%
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
  count(PAYSOK, COMMOK,lieudeces) %>%
  arrange(desc(n))

# Fonds de carte Eurostat et IGN Admin Express COG
fond <- st_read("donnees/CNTR_RG_10M_2020_3035.shp")

coord_FR <- st_read("donnees/COMMUNE.shp") %>% 
  st_transform(3035) %>% 
  st_centroid() %>% 
  mutate(x_fr=st_coordinates(.)[,1],
         y_fr=st_coordinates(.)[,2]) %>% 
  select(INSEE_COM,x_fr,y_fr,NOM)


corato <- etranger_flux %>% 
  filter(COMMOK=="CORATO" ) %>% 
  left_join(readRDS("sorties/base_complete.RDS") %>% select(-n),by=c("COMMOK","PAYSOK")) %>% 
  select(COMMOK,lieudeces,n,x,y)

saveRDS(corato,"./donnees/corato.RDS")
corato <- readRDS("./donnees/corato.RDS")

cc <- corato %>% 
  left_join(coord_FR, by=c("lieudeces"="INSEE_COM")) %>% 
  select(-geometry)

limites_flux <- fond %>% filter(NAME_ENGL %in% c("Italy","Belgium")) %>% st_bbox()


cc %>% 
  count(substr(lieudeces,1,2)=="38",wt=n) %>% 
  mutate(pct=n/sum(n)*100)

ggplot()+
  geom_sf(data=fond,
          fill="gray97",
          colour="gray85")+
  geom_curve(data=cc ,
             aes(x=x, y=y, xend=x_fr, yend=y_fr,size=n,alpha=n),
             curvature=0.35, colour="tomato1"#,
             #arrow=NA
             #arrow(length=unit(0.1, "cm"),type="closed")
  )+
  geom_label(data=cc %>% head(1),aes(x=x,y=y,label=str_to_title(COMMOK)),color="gray50",alpha=.8)+
  ggrepel::geom_label_repel(data = cc %>% slice_max(order_by = n,n=5),
                            aes(x=I(1*x_fr),y=y_fr,label=NOM_COM),alpha=.8,
                            direction = "both",color="gray50")+
  scale_alpha_continuous(range = c(0.03, 0.3))+
  coord_sf(xlim =c(3400000 ,5048517),
           ylim =c(1800000,3167694),
           expand = TRUE) +
  labs(title = paste0("De Corato au Dauphiné"),
       subtitle = paste0("72 % des 2846 migrants nés à Corato sont décédés en Isère"),
       caption="Source: Insee, état civil\nTraitements et erreurs: @Re_Mi_La")+
  theme_void()+
  theme(plot.title = element_text(face="bold",size=17),
        plot.subtitle = element_text(size=9),
        panel.background = element_rect(fill="lightcyan2",colour = NA),
        plot.caption = element_text(face = "italic",size=9),
        legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        plot.background = element_rect(fill="white",color=NA))+
  guides(size="none",alpha="none")

ggsave("sorties/corato.jpg",dpi=300,units=c("mm"))




sommatino <- etranger %>% 
  filter(COMMOK=="SOMMATINO" ) %>% 
  left_join(readRDS("sorties/base_complete.RDS") %>% select(-n),by=c("COMMOK","PAYSOK")) %>% 
  select(COMMOK,lieudeces,n,x,y)

saveRDS(sommatino,"donnees/sommatino.RDS")
fond <- st_read("donnees/CNTR_RG_10M_2020_3035.shp")

coord_FR <- st_read("donnees/COMMUNE.shp") %>% 
  st_transform(3035) %>% 
  st_centroid() %>% 
  mutate(x_fr=st_coordinates(.)[,1],
         y_fr=st_coordinates(.)[,2]) %>% 
  select(INSEE_COM,x_fr,y_fr,NOM_COM)

ss <- sommatino %>% 
  left_join(coord_FR, by=c("lieudeces"="INSEE_COM")) %>% 
  select(-geometry)

limites_flux <- fond %>% filter(NAME_ENGL %in% c("Italy","Switzerland")) %>% st_bbox()


ss %>% 
  count(substr(lieudeces,1,2)=="38",wt=n) %>% 
  mutate(pct=n/sum(n)*100)

ggplot()+
  geom_sf(data=fond,
          fill="gray97",
          colour="gray85")+
  geom_curve(data=ss ,
             aes(x=x, y=y, xend=x_fr, yend=y_fr,size=n,alpha=n),
             curvature=0.35, colour="tomato1"#,
             #arrow=NA
             #arrow(length=unit(0.1, "cm"),type="closed")
  )+
  geom_label(data=ss %>% head(1),aes(x=x,y=y,label=str_to_title(COMMOK)),color="gray50",alpha=.8)+
  ggrepel::geom_label_repel(data = ss %>% slice_max(order_by = n,n=5),
                            aes(x=I(1*x_fr),y=y_fr,label=NOM_COM),alpha=.8,
                            direction = "both",color="gray50")+
  scale_alpha_continuous(range = c(0.03, 0.3))+
  coord_sf(xlim =c(3700000 ,limites_flux$xmax),
           ylim =c(limites_flux$ymin,limites_flux$ymax),
           expand = TRUE) +
  labs(title = paste0("De la Sicile au Dauphiné"),
       subtitle = paste0("73 % des 1325 migrants nés à Sommatino sont décédés en Isère"),
       caption="Source: Insee, état civil\nTraitements et erreurs: @Re_Mi_La")+
  theme_void()+
  theme(plot.title = element_text(face="bold",size=17),
        plot.subtitle = element_text(size=9),
        panel.background = element_rect(fill="lightcyan2",colour = NA),
        plot.caption = element_text(face = "italic",size=9),
        legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        plot.background = element_rect(fill="white",color=NA))+
  guides(size="none",alpha="none")

ggsave("sorties/sommatino.jpg",dpi=300,units=c("mm"))




herne <- etranger %>% 
  filter(COMMOK=="HERNE" ) %>% 
  left_join(readRDS("sorties/base_complete.RDS") %>% select(-n),by=c("COMMOK","PAYSOK")) %>% 
  select(COMMOK,lieudeces,n,x,y)

saveRDS(herne,"./donnees/herne.RDS")
#corato <- readRDS("./donnees/corato.RDS")
fond <- st_read("donnees/CNTR_RG_10M_2020_3035.shp")

coord_FR <- st_read("./donnees/COMMUNE.shp") %>% 
  st_transform(3035) %>% 
  st_centroid() %>% 
  mutate(x_fr=st_coordinates(.)[,1],
         y_fr=st_coordinates(.)[,2]) %>% 
  select(INSEE_COM,x_fr,y_fr,NOM_COM)

hh <- herne %>% 
  left_join(coord_FR, by=c("lieudeces"="INSEE_COM")) %>% 
  select(-geometry)

limites_flux <- fond %>% filter(NAME_ENGL %in% c("Germany","Switzerland")) %>% st_bbox()


hh %>% 
  count(substr(lieudeces,1,2) %in% c("59","62"),wt=n) %>% 
  mutate(pct=n/sum(n)*100)

ggplot()+
  geom_sf(data=fond,
          fill="gray97",
          colour="gray85")+
  geom_curve(data=hh ,
             aes(x=x, y=y, xend=x_fr, yend=y_fr,size=n,alpha=n),
             curvature=0.35, colour="tomato1"#,
             #arrow=NA
             #arrow(length=unit(0.1, "cm"),type="closed")
  )+
  geom_label(data=hh %>% head(1),aes(x=x,y=y,label=str_to_title(COMMOK)),color="gray50",alpha=.8)+
  ggrepel::geom_label_repel(data = hh %>% slice_max(order_by = n,n=5),
                            aes(x=I(1*x_fr),y=y_fr,label=NOM_COM),alpha=.8,
                            direction = "both",color="gray50")+
  scale_alpha_continuous(range = c(0.03, 0.3))+
  coord_sf(xlim =c(3500000 ,4300000),
           ylim =c(2700000,3300000),
           expand = TRUE) +
  labs(title = paste0("Des mines de Westphalie au bassin houiller\ndu Nord-Pas-de-Calais"),
       subtitle = paste0("73 % des 852 migrants nés à Herne sont décédés dans le Nord ou le Pas-de-Calais"),
       caption="Source: Insee, état civil\nTraitements et erreurs: @Re_Mi_La")+
  theme_void()+
  theme(plot.title = element_text(face="bold",size=17),
        plot.subtitle = element_text(size=9),
        panel.background = element_rect(fill="lightcyan2",colour = NA),
        plot.caption = element_text(face = "italic",size=9),
        legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        plot.background = element_rect(fill="white",color=NA))+
  guides(size="none",alpha="none")

ggsave("./sorties/herne.jpg",dpi=300,units=c("mm"))





citta <- etranger %>% 
  filter(COMMOK=="CITTA DI CASTELLO" ) %>% 
  left_join(readRDS("sorties/base_complete.RDS") %>% select(-n),by=c("COMMOK","PAYSOK")) %>% 
  select(COMMOK,lieudeces,n,x,y) %>% 
  mutate(lieudeces2= str_pad(lieudeces,5,"left","0")) %>% 
  count(COMMOK,lieudeces2,x,y,wt=n)

saveRDS(sommatino,"donnees/sommatino.RDS")
fond <- st_read("donnees/CNTR_RG_10M_2020_3035.shp")

coord_FR <- st_read("donnees/COMMUNE.shp") %>% 
  st_transform(3035) %>% 
  st_centroid() %>% 
  mutate(x_fr=st_coordinates(.)[,1],
         y_fr=st_coordinates(.)[,2]) %>% 
  select(INSEE_COM,x_fr,y_fr,NOM_COM)

cc <- citta %>% 
  left_join(coord_FR, by=c("lieudeces2"="INSEE_COM")) %>% 
  select(-geometry)

limites_flux <- fond %>% filter(NAME_ENGL %in% c("Italy","Switzerland")) %>% st_bbox()

cc %>% 
  count(substr(lieudeces2,1,2)=="06",wt=n) %>% 
  mutate(pct=n/sum(n)*100)

ggplot()+
  geom_sf(data=fond,
          fill="gray97",
          colour="gray85")+
  geom_curve(data=cc ,
             aes(x=x, y=y, xend=x_fr, yend=y_fr,size=n,alpha=n),
             curvature=0.35, colour="tomato1"#,
             #arrow=NA
             #arrow(length=unit(0.1, "cm"),type="closed")
  )+
  geom_label(data=cc %>% head(1),aes(x=x,y=y,label=str_to_title(COMMOK)),color="gray50",alpha=.8)+
  ggrepel::geom_label_repel(data = cc %>% slice_max(order_by = n,n=5),
                            aes(x=I(1*x_fr),y=y_fr,label=NOM_COM),alpha=.8,
                            direction = "both",color="gray50")+
  scale_alpha_continuous(range = c(0.03, 0.3))+
  coord_sf(xlim =c(3700000 ,4800000),
           ylim =c(2000000,2900000),
           expand = TRUE) +
  labs(title = paste0("De l'Ombrie aux Alpes-Maritimes"),
       subtitle = paste0("84 % des 899 migrants nés à Citta di Castello sont décédés dans les Alpes-Maritimes"),
       caption="Source: Insee, état civil\nTraitements et erreurs: @Re_Mi_La")+
  theme_void()+
  theme(plot.title = element_text(face="bold",size=17),
        plot.subtitle = element_text(size=9),
        panel.background = element_rect(fill="lightcyan2",colour = NA),
        plot.caption = element_text(face = "italic",size=9),
        legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        plot.background = element_rect(fill="white",color="white"))+
  guides(size="none",alpha="none")

ggsave("sorties/sommatino.jpg",dpi=300,units=c("mm"))



# Faire une carte de l'Europe avec les pays limitrophes d'une couleur et le lieu de décès des personnes
# de ces pays de la même couleur

deces_france <- coord_FR %>% 
  inner_join(etranger_flux %>% 
               filter(PAYSOK %in% c("ITALIE","ALLEMAGNE","BELGIQUE","ESPAGNE")),
             by=c("INSEE_COM"="lieudeces"))


deces_france %>% 
  ggplot()+
  geom_sf(aes(size=n),color="firebrick2",alpha=.5)+
  scale_size_continuous( range = c(0.000000001,10) )+
  facet_wrap(~PAYSOK)+
  theme_void()+
  theme(legend.position="bottom",
        text = element_text(color = "firebrick2"))

ggsave("sorties/carte_lieux_deces.jpg",dpi=300,units=c("mm")) 
# pb sur wanne  en Allemagne
# Mayence Mainz était FR jusqu'en 1930