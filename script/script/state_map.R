cCyan <- "#00a0e9"
cMagenta <- "#e4007f"
cGreen <- "#009944"
cOrange <- "#f39800"
cLightBlue <- "#0068b7"

states <- map_data("state")
county <- map_data("county")

gg_base <- ggplot()+
  geom_polygon(data=states, aes(x=long, y=lat, group=group), color="black", fill=NA)+
  geom_polygon(data=county, aes(x=long, y=lat, group=group), fill="gray", color="white", size=0.1)+
  coord_fixed(xlim = c(-123, -69),  ylim = c(23, 48), ratio = 1.3)
  #coord_fixed(xlim = c(-90, -75),  ylim = c(30, 40), ratio = 1.3)

#zipcodeのvisualize
gg_zip <- zipcode %>% 
  group_by(mesh, SW_lon, NE_lon, SW_lat, NE_lat) %>% 
  summarize(n=n()) %>% 
  geom_rect(data = ., aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat), alpha=0, fill="white", linetype=1, color="black") 
  #geom_point(data=., aes(x=longitude, y=latitude), size=0.1)

gg_prov <- healthcare %>% 
  group_by(longitude, latitude) %>% 
  summarize() %>% 
  geom_point(data=., aes(x=longitude, y=latitude), size=0.4, alpha=0.5, color=cLightBlue)

gg_base + gg_prov + theme(text = element_blank())

  
gg_health <- healthcare %>% 
  filter(CountryCodeoftheProvider=="US", EntityTypeoftheProvider=="I") %>% 
  group_by(NationalProviderIdentifier, ProviderTypeoftheProvider, longitude, latitude) %>% 
  summarise(Amount=sum(NumberofServices*AverageMedicareAllowedAmount)) %>% 
  na.omit() %>% 
  geom_point(data=.,aes(x=longitude, y=latitude, color=Amount), size=1, alpha=1)
  #dstat_density2d(aes(long,lat,fill=Amount))
#scale_fill_gradient(low=cLightBlue, high=cOrange)

gg_base + gg_health + 
  scale_fill_gradient(low=cLightBlue, high=cOrange)+
  theme(legend.position = "right")


gg_health <- healthcare %>% 
#  filter(HCPCSCode == 99213 || HCPCSCode == 99214) %>% 
#  filter(AverageSubmittedChargeAmount < 500) %>% 
  mutate(Amount = AverageSubmittedChargeAmount*NumberofServices) %>% 
  group_by(region) %>% 
  summarize(mean=sum(Amount)/sum(NumberofServices), sum = sum(Amount)) %>% 
  inner_join(., states, by = "region") %>%
  #geom_point(data=.,aes(x=longitude, y=latitude, color=AverageSubmittedChargeAmount), size=1, alpha=1)
  geom_polygon(data = ., aes(x=long, y=lat, group=group, fill=sum), color="white")


#メッシュごとに色はつけるが、数字は書き込まない場合
gg_mesh <- healthcare %>% 
  filter(hcpcs_code == "99213") %>% 
  mutate(amount = line_srvc_cnt*average_submitted_chrg_amt) %>% 
  na.omit() %>% 
  group_by(mesh, NE_lat, NE_lon, SW_lat, SW_lon) %>% 
  summarize(mean=mean(amount), n=n()) %>% 
#geom_point(data=.,aes(x=longitude, y=latitude, color=AverageSubmittedChargeAmount), size=1, alpha=1)
  geom_rect(data = ., aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat), fill=NA, color="black", alpha=1, size=1.4) 

gg_base + gg_mesh + gg_prov + theme(text=element_blank())
  scale_fill_gradient(low=cLightBlue, high=cOrange)+
  #geom_text(data=zipcode %>% 
  #            group_by(mesh) %>% 
  #            summarize(lon=mean((NE_lon+SW_lon)/2), lat=mean((NE_lat+SW_lat)/2)),
  #          aes(x=lon, y=lat, label=mesh), size = 1.5)+
  theme(legend.position = "right", text = element_text(size=20))



###メッシュに数字を書き込みたい場合
mesh_data <- healthcare %>% 
  filter(hcpcs_code == "99213", provider_type == "Internal Medicine", nppes_entity_code == "I") %>% 
  mutate(amount = line_srvc_cnt*average_submitted_chrg_amt) %>% 
  na.omit() %>% 
  group_by(mesh, NE_lat, NE_lon, SW_lat, SW_lon) %>% 
  summarize(amount = mean(amount), speciality = mean(spc)) 
  #geom_point(data=.,aes(x=longitude, y=latitude, color=AverageSubmittedChargeAmount), size=1, alpha=1)
  
gg_rect <-  geom_rect(data = mesh_data, aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat, fill=speciality), alpha=0.9) 
gg_text <-  geom_text(data = mesh_data, aes(x = (SW_lon+NE_lon)/2, y=(SW_lat+NE_lat)/2+0.06, label=round(speciality)), size = 2)
gg_meshcode <- geom_text(data = mesh_data, aes(x = (SW_lon+NE_lon)/2, y=(SW_lat+NE_lat)/2-0.07, label=mesh), size=1.6)
  #geom_text(data=zipcode %>% 
  #            group_by(mesh) %>% 
  #            summarize(longitude=mean(longitude), latitude=mean(latitude)),
  #          aes(x=lon, y=lat, label=mesh))

gg_state <- geom_polygon(data=states, aes(x=long, y=lat, group=group), color="black", fill=NA)
gg_county <-  geom_polygon(data=county, aes(x=long, y=lat, group=group), fill="gray", color="white", size=0.1)

gg_base + gg_rect  + 
  scale_fill_gradient(low=cLightBlue, high=cOrange)+ #trans="log10")+
  #geom_text(data=zipcode %>% 
  #            group_by(mesh) %>% 
  #            summarize(lon=mean((NE_lon+SW_lon)/2), lat=mean((NE_lat+SW_lat)/2)),
  #          aes(x=lon, y=lat, label=mesh), size = 1.5)+
  theme(legend.position = "right", text=element_text(size=25))


ggsave(plot=g, filename = "img/SumAmountofservices_99213.pdf", height = 30, width=30*1.3)


###メッシュごとのProviderの数
mesh_data <- healthcare %>% 
  group_by(mesh, NE_lon, NE_lat, SW_lon, SW_lat, NationalProviderIdentifier) %>% 
  summarise(n=n()) %>% 
  summarise(NumberofProviders=n()) %>% 
  na.omit()

gg_rect <-  geom_rect(data = mesh_data, aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat, fill=NumberofProviders), alpha=0.8) 
gg_text <-  geom_text(data = mesh_data, aes(x = (SW_lon+NE_lon)/2, y=(SW_lat+NE_lat)/2+0.06, label=round(NumberofProviders)), size = 1)

gg_base + gg_rect + gg_text + scale_fill_gradient(low=cLightBlue, high=cOrange, trans="log10") +
０ theme(text = element_text(size=20), legend.position =  "bottom")


##メッシュごと、ProviderごとにNumberofServiceの数を計算
mesh_data <- healthcare %>% 
  filter(EntityTypeoftheProvider == "I", ProviderTypeoftheProvider == "Internal Medicine") %>% 
  group_by(NationalProviderIdentifier, ProviderTypeoftheProvider, mesh, SW_lat, SW_lon, NE_lat, NE_lon) %>% 
  summarize(NumofS=sum(NumberofServices)) %>% 
  group_by(mesh, SW_lat, SW_lon, NE_lat, NE_lon) %>% 
  summarize(mean = mean(NumofS)) 

gg_rect <-  geom_rect(data = mesh_data, aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat, fill=mean), alpha=0.9) 
gg_text <-  geom_text(data = mesh_data, aes(x = (SW_lon+NE_lon)/2, y=(SW_lat+NE_lat)/2+0.06, label=round(mean)), size = 2)
gg_meshcode <- geom_text(data = mesh_data, aes(x = (SW_lon+NE_lon)/2, y=(SW_lat+NE_lat)/2-0.07, label=mesh), size=1.6)

gg_base + gg_rect + gg_text + scale_fill_gradient(low=cLightBlue, high=cOrange) + gg_meshcode+
  theme(text = element_text(size=20), legend.position =  "right")


##メッシュごと、幾つの治療を行なっているか
mesh_data <- healthcare %>% 
  group_by(NationalProviderIdentifier, mesh, SW_lat, SW_lon, NE_lat, NE_lon) %>% 
  summarise(n=n()) %>% 
  group_by(mesh, SW_lat, SW_lon, NE_lat, NE_lon) %>% 
  summarize(KindofServices = mean(n)) 

gg_rect <-  geom_rect(data = mesh_data, aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat, fill=KindofServices), alpha=0.9) 
gg_text <-  geom_text(data = mesh_data, aes(x = (SW_lon+NE_lon)/2, y=(SW_lat+NE_lat)/2+0.06, label=round(KindofServices)), size = 2)
gg_meshcode <- geom_text(data = mesh_data, aes(x = (SW_lon+NE_lon)/2, y=(SW_lat+NE_lat)/2-0.07, label=mesh), size=1.6)a

gg_base + gg_rect + gg_text + scale_fill_gradient(low=cLightBlue, high=cOrange) + gg_meshcode+
  theme(text = element_text(size=20), legend.position =  "right")
