# combine three data frames  ----------------------------------------------

#healthcare <- read_delim("data_from_organizer/BDMM2017/datasets/Medicare_Provider_Util_Payment_PUF_CY2015.txt", delim="\t", n_max=10)

healthcare <- read_csv("data/Medicare_Provider_Utilization_and_Payment_Data__Physician_and_Other_Supplier_PUF_CY2015.csv")
colnames(healthcare) <- c("npi","nppes_provider_last_org_name","nppes_provider_first_name",
                          "nppes_provider_mi","nppes_credentials","nppes_provider_gender",
                          "nppes_entity_code","nppes_provider_street1","nppes_provider_street2",
                          "nppes_provider_city","nppes_provider_zip","nppes_provider_state",
                          "nppes_provider_country","provider_type","medicare_participation_indicator",
                          "place_of_service","hcpcs_code","hcpcs_description","hcpcs_drug_indicator",
                          "line_srvc_cnt","bene_unique_cnt","bene_day_srvc_cnt","average_Medicare_allowed_amt",
                          "average_submitted_chrg_amt","average_Medicare_payment_amt","average_Medicare_standard_amt")
healthcare <- healthcare %>% mutate(npi = as.character(npi))

healthcare <- healthcare %>% 
  filter(nppes_entity_code == "I")

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

leie <- read_csv("data/fraudlabel/UPDATED.csv") %>% 
  mutate(EXCLDATE = lubridate::ymd(EXCLDATE)) %>% 
  select(NPI, EXCLDATE, REINDATE, EXCLTYPE) %>% 
  filter(lubridate::year(EXCLDATE) > 2015) %>% 
  mutate(class = 1) %>% 
  filter(EXCLTYPE %in% c("1128a1", "1128a2", "1128a3", "1128b4"))

taxonomy <- read_csv("data_from_organizer/BDMM2017/datasets/nucc_taxonomy_150.csv")

healthcare <- healthcare %>% 
  left_join(., npi, by = c("npi" = "NPI")) %>% 
  left_join(., taxonomy, by = c("Taxonomy" = "Code")) 

rm(npi)



# add geographical information --------------------------------------------

full_statename <- data.frame(state = state.abb, region=state.name)
data(zipcode)

zipcode <- zipcode %>% 
  na.omit() %>% 
  mutate(mesh = cal_meshcode1_vec(.$latitude, .$longitude)) %>% 
  mutate(NE_lat=meshcode_to_latlong_NE_vec(mesh)["lat",] %>% unlist(), NE_lon=meshcode_to_latlong_NE_vec(mesh)["long",] %>% unlist(), 
         SW_lat=meshcode_to_latlong_SE_vec(mesh)["lat",] %>% unlist(), SW_lon=meshcode_to_latlong_SW_vec(mesh)["long",] %>% unlist())

colnames(healthcare) <- str_replace_all(colnames(healthcare), " ", "")

healthcare <- healthcare %>% 
  mutate(zip = str_sub(nppes_provider_zip, 1, 5)) %>% 
  left_join(., zipcode, by="zip") %>% 
  mutate(city = str_to_lower(nppes_provider_city)) %>% 
  left_join(., full_statename, by="state") %>% 
  mutate(region=str_to_lower(region)) %>% 
  left_join(leie, by=c("npi"="NPI")) %>% 
  tidyr::replace_na(list(class=0))

# mesh_to_lonlat <- sapply(unique(zipcode$mesh), meshcode_to_latlong) %>% 
#   t() %>% 
#   data.frame() %>% 
#   mutate(mesh=rownames(.))
# 
# pro_to_lonlat <- healthcare %>% 
#   select(npi, longitude, latitude) %>% 
#   group_by(npi, longitude, latitude) %>% 
#   summarize() %>% 
#   na.omit()
