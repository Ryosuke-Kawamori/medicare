zip_to_lonlat_county <- read_csv("data/zip_to_lonlat_county.csv", skip=1) 
colnames(zip_to_lonlat_county) <- c("zip", "county02", "county", "cntname", "zipname", "lon", "lat", "land_area_sqml", "zcta5_to_cnt_2_alloc")

zip_to_lonlat_county <- zip_to_lonlat_county %>% 
  mutate(mesh = cal_meshcode1_vec(lat, lon),
         NE_lat = meshcode_to_latlong_NE_vec(mesh) %>% .[1,] %>% unlist,
         NE_lon = meshcode_to_latlong_NE_vec(mesh) %>% .[2,] %>% unlist,
         SW_lat = meshcode_to_latlong_SW_vec(mesh) %>% .[1,] %>% unlist,
         SW_lon = meshcode_to_latlong_SW_vec(mesh) %>% .[2,] %>% unlist) %>% 
  mutate(subregion=stringr::str_replace(cntname, " +[A-Z][A-Z]", "") %>% stringr::str_to_lower()) 

write_csv(zip_to_lonlat_county, "data/zip_ver3.csv")
