outlier_lof <- lof_hcpcs_result %>% 
  filter(row_number()/nrow(.) < 0.20) %>% 
  select(npi) %>% 
  mutate(predict=1)

outlier_svm <- predicted_df_mesh_stat[[3]] %>% 
  filter(predict==1) %>% 
  select(npi, predict)

outlier <- outlier_svm

outlier_df <- outlier %>% 
  right_join(healthcare %>% filter(npi %in% npi_id)) %>% 
  replace_na(list(predict=0))

outlier_df %>% 
  count(npi, provider_type, predict, class) %>% 
  count(provider_type, predict, class) %>% 
  filter(predict == 1, class==1)

outlier_df %>% 
  #Individual・内科・meshcodeの選択
  filter(provider_type == "Internal Medicine") %>% 
  #filter(nppes_entity_code=="I", provider_type == "Internal Medicine", mesh %in% c("306371")) %>% 
  #金額ベース
  select(npi, hcpcs_code, line_srvc_cnt,
         average_Medicare_allowed_amt, average_submitted_chrg_amt, average_Medicare_payment_amt, average_Medicare_standard_amt, class, predict) %>% 
  transmute(npi = npi,
            hcpcs_code = hcpcs_code,
            submitted_chrg_amt = average_submitted_chrg_amt*line_srvc_cnt,
            class = class,
            predict = predict) %>% 
  group_by(npi, hcpcs_code, class, predict) %>% 
  summarize(submitted_chrg_amt = sum(submitted_chrg_amt)) %>% 
  ungroup() %>% 
  filter(hcpcs_code %in% c("99213", "99214", "99215"))  %>% 
  complete(hcpcs_code, nesting(npi, class, predict), fill=list(submitted_chrg_amt=0)) %>% 
  #mutate(predict = as.factor(paste(predict, class))) %>% 
  mutate(predict = as.factor(predict)) %>% 
  ggplot(aes(x=log(1+submitted_chrg_amt), fill=predict))+
  geom_histogram()+
  facet_grid(predict~hcpcs_code, scales="free_y")+
  theme(text=element_text(size=18))


outlier_df %>% 
  #Individual・内科・meshcodeの選択
  filter(provider_type == "Internal Medicine") %>% 
  #金額ベース
  select(npi, hcpcs_code, line_srvc_cnt,
         average_Medicare_allowed_amt, average_submitted_chrg_amt, average_Medicare_payment_amt, average_Medicare_standard_amt, predict) %>% 
  transmute(npi = npi,
            hcpcs_code = hcpcs_code,
            submitted_chrg_amt = average_submitted_chrg_amt*line_srvc_cnt,
            predict = predict) %>% 
  group_by(npi, hcpcs_code, predict) %>% 
  summarize(submitted_chrg_amt = sum(submitted_chrg_amt)) %>% 
  ungroup() %>% 
  group_by(hcpcs_code) %>% 
  mutate(hcpcs_count = n()) %>% 
  ungroup() %>% 
  filter(between(dense_rank(desc(hcpcs_count)),50,70)) %>%
  select(-hcpcs_count) %>%
  complete(hcpcs_code, nesting(npi, predict), fill=list(submitted_chrg_amt=0)) %>% 
  mutate(predict = as.factor(predict)) %>% 
  ggplot(aes(x=log(1+submitted_chrg_amt), y=hcpcs_code, fill=predict))+
  geom_density_ridges(alpha=0.3)+
  theme(text=element_text(size=18))
