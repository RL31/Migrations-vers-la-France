library(tidyverse)
library(sf)

base_complete <- readRDS("sorties/base_complete.RDS")

fond <- st_read("donnees/CNTR_RG_10M_2020_3035.shp")

# Définition de seuils de proximité
FR <- fond %>% 
  filter(CNTR_ID=="FR") %>% 
  st_cast("POLYGON") %>% 
  mutate(ID=row_number()) %>%
  filter(ID %in% c(1,3) )
FR50 <- FR %>% 
  st_buffer(dist = 50000)
FR100 <- FR %>% 
  st_buffer(dist = 100000)
FR25 <- FR %>% 
  st_buffer(dist = 25000)
FR200 <- FR %>% 
  st_buffer(dist = 200000)


# ggplot()+
#   geom_sf(data=FR)+
#   geom_sf(data=FR50,color="red",fill="transparent")

proximite25 <- st_intersection(FR25,base_complete) %>% 
  count(PAYSOK,wt=n) %>% 
  as.data.frame() %>% 
  left_join(base_complete %>%   as.data.frame() %>% count(PAYSOK,wt=n),by="PAYSOK") %>% 
  mutate(pct25=n.x/n.y*100) %>% 
  select(PAYSOK,pct25)

st_intersection(fond %>% filter(CNTR_ID=="UK"),FR200) %>% head(1) %>% 
  st_area()/ fond %>% filter(CNTR_ID=="UK") %>% st_area()*100

ggplot()+
  geom_sf(data=fond %>% filter(CNTR_ID=="UK"))
ggplot()+
  geom_sf(data=st_intersection(fond %>% filter(CNTR_ID=="UK"),FR200) )

proximite50 <- st_intersection(FR50,base_complete) %>% 
  count(PAYSOK,wt=n) %>% 
  as.data.frame() %>% 
  left_join(base_complete %>%   as.data.frame() %>% count(PAYSOK,wt=n),by="PAYSOK") %>% 
  mutate(pct50=n.x/n.y*100) %>% 
  select(PAYSOK,pct50)

proximite100 <- st_intersection(FR100,base_complete) %>% 
  count(PAYSOK,wt=n) %>% 
  as.data.frame() %>% 
  left_join(base_complete %>%   as.data.frame() %>% count(PAYSOK,wt=n),by="PAYSOK") %>% 
  mutate(pct100=n.x/n.y*100) %>% 
  select(PAYSOK,pct100)

proximite200 <- st_intersection(FR200,base_complete) %>% 
  count(PAYSOK,wt=n) %>% 
  as.data.frame() %>% 
  left_join(base_complete %>%   as.data.frame() %>% count(PAYSOK,wt=n),by="PAYSOK") %>% 
  mutate(pct200=n.x/n.y*100) %>% 
  select(PAYSOK,pct200)



proximite <- proximite50 %>% 
  left_join(proximite100,by="PAYSOK") %>% 
  left_join(proximite200,by="PAYSOK") %>% 
left_join(proximite25,by="PAYSOK")

saveRDS(proximite,"donnees/proximite.RDS")

library(waffle)

waffle <- proximite %>% 
  mutate(plus=100-pct200,
         pct200=pct200-pct100,
         pct100=pct100-pct50,
         pct50=pct50-pct25) %>% 
  mutate(pct25=if_else(is.na(pct25),0,pct25),
         pct50=if_else(is.na(pct50),0,pct50)) %>% 
  pivot_longer(cols=c("pct25","pct50","pct100","pct200","plus"),
               names_to="seuil",
               values_to="part") %>% 
  mutate(seuil=fct_relevel(seuil,c("pct25","pct50","pct100","pct200","plus")),
         PAYSOK=str_to_title(PAYSOK)) %>% 
ggplot( aes(fill=seuil, values=part)) +
  geom_waffle(color = "white", size=1.125, n_rows = 10, 
              make_proportional = TRUE) +
  scale_fill_manual(name="Commune de naissance à moins de ... km de la frontière française",
                    labels=c("pct25"="25",
                             "pct50"="50",
                             "pct100"="100",
                             "pct200"="200",
                             "plus"="Plus"),
                    values=c("pct25"="coral4",
                             "pct50"="coral3",
                             "pct100"="coral2",
                             "pct200"="coral1",
                             "plus"="gray90"))+
  facet_wrap(~PAYSOK, nrow = 2) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(legend.position = "bottom",
        text = element_text(size=13),
        strip.background =element_rect(fill="white"))+
  coord_equal()+
  guides(fill = guide_legend(title.position = 'top'))

