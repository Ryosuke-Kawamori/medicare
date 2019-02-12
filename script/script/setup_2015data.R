# read data ---------------------------------------------------------------
healthcare <- read_delim("data_from_organizer/BDMM2017/datasets/Medicare_Provider_Util_Payment_PUF_CY2015.txt", delim="\t")

#Individualのみ
healthcare <- healthcare %>% 
  filter(nppes_entity_code == "I")

# read npi data -----------------------------------------------------------
npi <- read_csv("data_from_organizer/BDMM2017/datasets/npidata_20050523-20171112.csv", 
                col_types = cols_only(NPI = "c",
                                      `Healthcare Provider Taxonomy Code_1` = "c",
                                      `Healthcare Provider Taxonomy Code_2` = "c",
                                      `Healthcare Provider Taxonomy Code_3` = "c",
                                      `Healthcare Provider Taxonomy Code_4` = "c",
                                      `Healthcare Provider Taxonomy Code_5` = "c",
                                      `Healthcare Provider Taxonomy Code_6` = "c",
                                      `Healthcare Provider Taxonomy Code_7` = "c",
                                      `Healthcare Provider Taxonomy Code_8` = "c",
                                      `Healthcare Provider Taxonomy Code_9` = "c",
                                      `Healthcare Provider Taxonomy Code_10` = "c",
                                      `Healthcare Provider Taxonomy Code_11` = "c",
                                      `Healthcare Provider Taxonomy Code_12` = "c",
                                      `Healthcare Provider Taxonomy Code_13` = "c",
                                      `Healthcare Provider Taxonomy Code_14` = "c"))

npi <- npi %>% 
  unite(Taxonomy, `Healthcare Provider Taxonomy Code_1`:`Healthcare Provider Taxonomy Code_14`, sep=",") %>% 
  mutate(Taxonomy = str_replace_all(Taxonomy, ",NA", "")) %>% 
  mutate(spc = str_count(Taxonomy, ",") + 1) 


# stateとzipcodeとcountyのdataframeの準備 ----------------------------------------------

#statenameの準備
full_statename <- data.frame(state = state.abb, region=state.name)

#zipcode・mesh・lon・latの準備
#zipcode <- read_csv("data/zip_to_mesh.csv")

#zip to countyの準備
zipcode <- read_csv("data/zip_ver3.csv")
  

#zipcode・mesh・lon・latの追加
healthcare <- healthcare %>% 
  mutate(zip = str_sub(nppes_provider_zip, 1, 5)) %>% 
  left_join(zipcode %>% select(-region), by="zip") %>% 
  mutate(city = str_to_lower(nppes_provider_city)) %>% 
  left_join(full_statename, by=c("nppes_provider_state"="state")) %>% 
  mutate(region=str_to_lower(region)) %>% 
  left_join(npi, by=c("npi"="NPI")) 
  #speciality1のみ
  #filter(spc==1)

rm(npi)

mesh_to_lonlat <- sapply(unique(zipcode$mesh), meshcode_to_latlong) %>% 
  t() %>% 
  data.frame() %>% 
  mutate(mesh=rownames(.))

npi_to_lonlat <- healthcare %>% 
  select(npi, longitude, latitude) %>% 
  group_by(npi, longitude, latitude) %>% 
  summarize() %>% 
  na.omit()

