#healthcare <- read_csv("data/Medicare_Provider_Utilization_and_Payment_Data__Physician_and_Other_Supplier_PUF_CY2014.csv")
#healthcare <- read_delim("data_from_organizer/BDMM2017/datasets/Medicare_Provider_Util_Payment_PUF_CY2015.txt", delim="\t")

full_statename <- data.frame(state = state.abb, region=state.name)
data(zipcode)

zipcode <- zipcode %>% 
  na.omit() %>% 
  mutate(mesh = cal_meshcode1_vec(.$latitude, .$longitude)) %>% 
  mutate(NE_lat=meshcode_to_latlong_NE_vec(mesh)["lat",] %>% unlist(), NE_lon=meshcode_to_latlong_NE_vec(mesh)["long",] %>% unlist(), 
         SW_lat=meshcode_to_latlong_SE_vec(mesh)["lat",] %>% unlist(), SW_lon=meshcode_to_latlong_SW_vec(mesh)["long",] %>% unlist())

colnames(healthcare) <- str_replace_all(colnames(healthcare), " ", "")

healthcare <- healthcare %>% 
  mutate(zip = str_sub(nppes_provider_zip, 1, 5)) 
  
healthcare <- healthcare %>% 
  mutate(city = str_to_lower(nppes_provider_city))

healthcare <- healthcare %>% 
  left_join(., full_statename, by="state") %>% 
  mutate(region=str_to_lower(region))

mesh_to_lonlat <- sapply(unique(zipcode$mesh), meshcode_to_latlong) %>% 
  t() %>% 
  data.frame() %>% 
  mutate(mesh=rownames(.))


pro_to_lonlat <- healthcare %>% 
  select(npi, longitude, latitude) %>% 
  group_by(npi, longitude, latitude) %>% 
  summarize() %>% 
  na.omit()


  