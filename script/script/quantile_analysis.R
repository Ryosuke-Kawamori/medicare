###meshごとにやったやつ
qtile_out <- healthcare %>%
  filter(EntityTypeoftheProvider == "I") %>% 
  select(NationalProviderIdentifier, HCPCSCode, ProviderTypeoftheProvider, NumberofServices, NumberofMedicareBeneficiaries, mesh) %>% 
  group_by(NationalProviderIdentifier, ProviderTypeoftheProvider, HCPCSCode, mesh) %>% 
  summarise_at(vars(matches("Number.*|Average.*")), sum) %>% 
  ungroup() %>% 
  group_by(HCPCSCode, mesh) %>% 
  mutate(n=n()) %>% 
  mutate(
    nNumberofServices = ntile(NumberofServices, 100),
    nNumberofBeneficiaries = ntile(NumberofMedicareBeneficiaries, 100)
    ) %>% 
  #mutate_at(vars(matches("Average.*")), funs(ntile(.,100))) %>% 
  #filter_at(vars(matches("n.*")), all_vars(. > 95)) #
  filter(nNumberofServices > 95)
  
###メッシュ無視のやつ
qn_allout <- healthcare %>%
  filter(EntityTypeoftheProvider == "I") %>% 
  select(NationalProviderIdentifier, HCPCSCode, ProviderTypeoftheProvider, NumberofServices, NumberofMedicareBeneficiaries) %>% 
  group_by(NationalProviderIdentifier, ProviderTypeoftheProvider, HCPCSCode) %>% 
  summarise_at(vars(matches("Number.*|Average.*")), sum) %>% 
  ungroup() %>% 
  group_by(HCPCSCode) %>% 
  mutate(n=n()) %>% 
  mutate(
    nNumberofServices = ntile(NumberofServices, 100),
    nNumberofBeneficiaries = ntile(NumberofMedicareBeneficiaries, 100)
  ) %>% 
  #mutate_at(vars(matches("Average.*")), funs(ntile(.,100))) %>% 
  #filter_at(vars(matches("n.*")), all_vars(. > 95)) 
  filter(nNumberofServices > 95)

##メッシュありのdata
healthcare %>% 
  filter(NationalProviderIdentifier %in% qtile_out$NationalProviderIdentifier, !is.na(state)) %>% 
  group_by(state, NationalProviderIdentifier) %>% 
  summarize() %>% 
  summarize(n=n()) %>% 
  mutate(class = "Grid Square") -> df_w_mesh
##メッシュなしのdata
healthcare %>% 
  filter(NationalProviderIdentifier %in% qn_allout$NationalProviderIdentifier, !is.na(state)) %>% 
  group_by(state, NationalProviderIdentifier) %>% 
  summarize() %>%
  summarize(n=n()) %>% 
  mutate(class = "Direct Calculation") -> df_wo_mesh
##メッシュの有無
rbind(df_w_mesh, df_wo_mesh) %>% 
  ggplot(aes(x=state, y=n))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust=1, size=15), text = element_text(size=20))+
  facet_grid(class~.)


#Serviceごとではなく,ProviderTypeごとにNumberofServiceをまとめることにした.
outlier <- healthcare %>%
  filter(EntityTypeoftheProvider == "I") %>% 
  group_by(NationalProviderIdentifier, ProviderTypeoftheProvider, mesh, NE_lon, NE_lat, SW_lon, SW_lat) %>% 
  summarize(SumNumberofServices = sum(NumberofServices), ratio = sum(NumberofServices)/sum(NumberofMedicareBeneficiaries)) %>% 
  group_by(mesh, NE_lon, NE_lat, SW_lon, SW_lat) %>% 
  mutate(
    nNumberofServices = ntile(SumNumberofServices, 100),
    nNumberofBeneficiaries = ntile(ratio, 100)
  ) 
  #filter_at(vars(matches("n.*")), any_vars(. > 95))

#outlierがどこにいるかプロット
gg_numout <- outlier %>% 
  inner_join(., pro_to_lonlat, by="NationalProviderIdentifier") %>% 
  geom_point(data=., aes(x=longitude, y=latitude), size=1)

gg_base + gg_numout

#outlierをhealthcare tableから抜き出してくる
healthcare %>% 
  filter(EntityTypeoftheProvider == "I") %>% 
  group_by(NationalProviderIdentifier) %>% 
  summarize(NumofService = sum(NumberofServices), NumofBeneficiaries = sum(NumberofBeneficiaries)) %>% 
  right_join(., outlier, by="NationalProviderIdentifier") 
  
gg_base + gg_numout 
  
outlier %>% 
  filter(nNumberofBeneficiaries > 95)

