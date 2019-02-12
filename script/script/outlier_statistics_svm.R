outlier <- predicted_df_mesh_stat[[3]] %>% 
  filter(predict==1) %>% 
  select(npi, predict)


tibble(nu = seq(0.001, 0.98, length=20), conf_df = predicted_df_not_mesh_stat) %>% 
  dplyr::mutate(conf_df = purrr::map(conf_df, function(x){sum(as.integer(as.character(x$predict))/nrow(x))})) %>% 
  unnest %>% 
  ggplot(aes(nu, conf_df))+
  geom_point()+
  geom_line()+
  geom_text(aes(label=nu))+
  theme(text = element_text(size=20))+
  labs(x="hyparparamete : nu", y="ratio of outlier")
