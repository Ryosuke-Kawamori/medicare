healthcare %>% 
  filter(HCPCSCode == "J2778") %>% 
  head(30000) %>% 
#  filter(AverageSubmittedChargeAmount < 500) %>% 
  filter(-125 < longitude, longitude < -50, 20 < latitude, latitude < 50) %>% 
  na.omit() %>% 
  mutate(Amount = AverageSubmittedChargeAmount*NumberofServices) %>% 
  group_by(longitude, latitude) %>% 
  summarize(mean=sum(Amount)/sum(NumberofServices)) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(radius=~20*mean, label=~as.character(mean), color=~my_color[ntile(mean,10)])
