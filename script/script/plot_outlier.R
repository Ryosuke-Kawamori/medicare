ma <- states %>% 
  filter(region=="massachusetts") 

gg_ma <- ma %>% 
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group), color="black", fill="white")+
  theme(text = element_text(size=20))
#  coord_fixed(xlim = c(-71, -72),  ylim = c(42, 43), ratio = 1.3)


gg_out <- boston %>%
  mutate(class = ifelse(npi %in% outlier, "outlier", "normal")) %>% 
  group_by(npi, longitude, latitude, class) %>% 
  summarize() %>% 
  geom_point(data=., aes(x=longitude, y=latitude, color=class))

gg_bostonmesh <- boston %>% 
  filter(mesh=="306371") %>% 
  group_by(mesh, NE_lat, NE_lon, SW_lon, SW_lat) %>% 
  summarize() %>% 
  geom_rect(data=., aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat), fill=NA, color="black")

gg_ma + gg_bostonmesh + gg_out
