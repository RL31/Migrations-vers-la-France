library(tidyverse)
library(stringi)
library(sf)
library(tidyverse)

# Cr?ation de la base des lieux de naissance
origine <- function(AN){
  annee <- read.csv2(paste0("donnees/deces-",AN,".csv"))  %>% 
    select(datenaiss,commnaiss,paysnaiss,datedeces,lieudeces) %>% 
    mutate(lieudeces=as.character(lieudeces))
  return(annee)
}
liste <- c(as.character(rep(1970:2020)))
base_orig <- map_df(liste,~origine(.))

base_age <-  base_orig %>% 
    mutate(age=as.integer(substr(datedeces,1,4))-as.integer(substr(datenaiss,1,4)),
         annee=substr(datedeces,1,4),
         annee25=as.integer(substr(datenaiss,1,4))+25) %>%
  #group_by(annee) %>%
  group_by(annee25) %>%
  summarise(agemoy=mean(age,na.rm = T)) %>% 
  #filter(annee>1971) %>%
  ungroup() %>% 
  filter(annee25>1900 & annee25<2022)

saveRDS(base_age,"donnees/base_age.RDS")

  ggplot()+
  geom_line(aes(x=annee25,y=agemoy,group=1),color="firebrick1",size=1)+
 # geom_line(aes(x=annee25,y=agemoy,group=1),color="firebrick1",size=1)+
  labs(x="Année des 25 ans",
       y="Âge moyen au décès")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))


# Naissances ? l'?tranger
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
etranger <- etranger %>% 
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



etranger %>% 
  filter(!PAYSOK %in% c("MARTINIQUE","GUADELOUPE","GUYANE","REUNION","LA REUNION")) %>% 
  mutate(dpt=substr(lieudeces,1,2)) %>% 
  count(PAYSOK,COMMOK,dpt,wt=n) %>% 
  filter(dpt!="97") %>% 
  group_by(PAYSOK,COMMOK) %>% 
  mutate(pct=n/sum(n)*100,
         tot=sum(n)) %>% 
  ungroup() %>% 
  filter(tot>250 & pct>40) %>% view()



corato <- etranger %>% 
  filter(COMMOK=="CORATO" ) %>% 
  left_join(readRDS("sorties/base_complete.RDS") %>% select(-n),by=c("COMMOK","PAYSOK")) %>% 
  select(COMMOK,lieudeces,n,x,y)

saveRDS(corato,"./donnees/corato.RDS")
corato <- readRDS("./donnees/corato.RDS")
fond <- st_read("donnees/CNTR_RG_10M_2020_3035.shp")

coord_FR <- st_read("donnees/COMMUNE.shp") %>% 
  st_transform(3035) %>% 
  st_centroid() %>% 
  mutate(x_fr=st_coordinates(.)[,1],
         y_fr=st_coordinates(.)[,2]) %>% 
  select(INSEE_COM,x_fr,y_fr,NOM_COM)

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
        plot.background = element_rect(fill="white",color=NA))+
  guides(size="none",alpha="none")

ggsave("sorties/sommatino.jpg",dpi=300,units=c("mm"))



# Faire une carte de l'Europe avec les pays limitrophes d'une couleur et le lieu de décès des personnes
# de ces pays de la même couleur
# 
# pb sur wanne  en Allemagne
# Mayence Mainz était FR jusqu'en 1930