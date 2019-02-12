mesh_mean <- healthcare %>% 
  #filter(provider_type == "Internal Medicine", nppes_entity_code == "I") %>% 
  mutate(spending = average_submitted_chrg_amt*line_srvc_cnt) %>% 
  group_by(mesh, npi) %>% 
  summarize(spending=sum(spending)) %>% 
  summarize(mean=mean(spending, na.rm=TRUE), n=n()) 

mesh_mean %>% 
  ggplot(aes(mean))+
  geom_histogram()+
  labs(x="Total submitted amount per provider, by each grid", y="Count")+
  theme(text = element_text(size=20))

g_mesh_mean <- mesh_mean %>% 
  inner_join(zipcode) %>% 
  #rename(`Total submitted amount per beneficiary, by each grid` = mean) %>% 
  geom_rect(data = ., aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat, fill=mean), alpha=1, size=1.4)

g_base+g_mesh_mean+scale_fill_gradient(low=cLightBlue, high=cOrange)+
  labs(x="Longitude", y="Latitude", trans = "log", fill="Total submitted amount\n per provider, \n by each grid")+
  theme(text = element_text(size=20), legend.title = element_text(size=15), 
        legend.text = element_text(size=15))
        #legend.position = c(1,0), legend.justification = c(1,0))

#speciality homogeneity
healthcare %>% 
  filter(provider_type == "Internal Medicine", nppes_entity_code == "I") %>% 
  mutate(spending = average_submitted_chrg_amt*line_srvc_cnt) %>% 
  group_by(spc, npi) %>% 
  summarize(spending=sum(spending)) %>% 
  ungroup() %>% 
  ggplot(aes(mean, fill=spc))

county_mean <- healthcare %>% 
  filter(provider_type == "Internal Medicine", nppes_entity_code == "I") %>% 
  mutate(spending = average_submitted_chrg_amt*line_srvc_cnt) %>% 
  group_by(county02, npi) %>% 
  summarize(spending=sum(spending)) %>% 
  summarize(mean=mean(spending, na.rm=TRUE), n=n()) 
  #filter(n>10)

g_county_mean <- county_mean %>% 
  inner_join(zipcode) %>% 
  rename(`Total submitted amount` = mean) 
  #geom_rect(data = ., aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat, fill=`Total submitted amount`), alpha=1, size=1.4) 

g_base+g_mesh_mean+scale_fill_gradient(low=cLightBlue, high=cOrange)+labs(x="Longitude", y="Latitude", trans="log")
