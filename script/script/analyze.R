healthcare %<>% 
  group_by(HCPCSCode) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(rank = dense_rank(desc(n))) 

important_prop <- healthcare %>% 
  filter(rank <= 10) %>% 
  select(HCPCS_Code) %>% 
  unique()


health_care_variable <- healthcare %>%
  filter(rank <= 2) %>% 
  select(National_Provider_Identifier, HCPCS_Code, Average_Medicare_Allowed_Amount, Average_Submitted_Charge_Amount) %>% 
  gather(key="key", value="value", -National_Provider_Identifier, -HCPCS_Code) %>% 
  unite(temp, HCPCS_Code, key) %>%  group_by(temp) %>% 
  mutate(id=1:n()) %>% 
  ungroup() %>% 
  spread(temp, value) %>% 
  na.omit()


healthcare %>% 
  filter(rank <= 10) %>% 
  ggplot(aes(Average_Medicare_Allowed_Amount)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~HCPCS_Description)