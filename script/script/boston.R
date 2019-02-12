healthcare <- read_delim("data_from_organizer/BDMM2017/datasets/Medicare_Provider_Util_Payment_PUF_CY2015.txt", delim="\t")

boston <- healthcare %>% 
  filter(nppes_entity_code == "I", mesh=="306371") 

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

taxonomy <- read_csv("data_from_organizer/BDMM2017/datasets/nucc_taxonomy_150.csv")

boston <- boston %>% 
  left_join(., npi %>% filter(spc==1), by = c("npi" = "NPI")) %>% 
  left_join(., taxonomy, by = c("Taxonomy" = "Code")) 

write_csv(boston, "data/boston.csv")
