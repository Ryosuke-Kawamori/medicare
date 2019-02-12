# 地域・科のフィルタと特徴量の準備 --------------------------------------------------------
boston_healthcare <- healthcare %>% 
  #Individual・内科・meshcodeの選択count
  filter(nppes_entity_code=="I", provider_type == "Internal Medicine", mesh %in% c("306371", "306372")) %>% 
  #filter(provider_type=="Internal Medicine") %>% 
  #金額ベース
  dplyr::select(npi, hcpcs_code, line_srvc_cnt, average_Medicare_allowed_amt, average_submitted_chrg_amt) %>% # average_Medicare_payment_amt, average_Medicare_standard_amt) %>% 
  transmute(npi = npi,
            hcpcs_code = hcpcs_code,
            submitted_chrg_amt = average_submitted_chrg_amt*line_srvc_cnt) %>% 
  group_by(npi, hcpcs_code) %>% 
  summarize(submitted_chrg_amt = sum(submitted_chrg_amt)) %>% 
  ungroup() 

internal_healthcare <- boston_healthcare %>% 
  gather(key=which_amount, value=amount, -npi, -hcpcs_code) %>% 
  unite(hcpcs_which_amount, hcpcs_code, which_amount) %>% 
  spread(key=hcpcs_which_amount, value=amount, fill=0) %>% 
  mutate_if(is.double, funs(scale_this(log(.+1)))) 
  
internal_feature <- internal_healthcare %>% 
  select(-npi) %>% 
  data.matrix
rownames(internal_feature) <- internal_healthcare$npi
internal_feature <- internal_feature[,apply(internal_feature,2,function(x){all(!is.nan(x))})]

#internal_feature_r <- internal_feature[sample(1:nrow(internal_feature), 20000), ]

# outlier detection -------------------------------------------------------

#internal_feature <- as(internal_feature, "matrix.csr")
ratio_o <- data.frame(nu=seq(0.001, 0.4, length=20)) %>% 
  mutate(ratio = purrr::map(nu, function(x){
    internal_svm <- e1071::svm(internal_feature, y=NULL, nu=x, type="one-classification")
    outlier_df <- tibble(npi=internal_healthcare$npi, predict = factor(if_else(predict(internal_svm)==TRUE, 0, 1))) 
    return(outlier_df %>% count(predict))
  }))
result <- ratio_o %>% 
  mutate(result = purrr::map(ratio, . %>% mutate(sum = sum(n)))) %>% 
  mutate(result = purrr::map(result, . %>% mutate(n = n/sum)))

result %>% 
  select(nu, result) %>% 
  unnest() %>% 
  filter(predict == 1) %>% 
  ggplot(aes(x=nu, y=n)) +
  geom_point()+
  geom_text(aes(label=nu), vjust=2)+
  geom_line()+
  theme(text = element_text(size=20))+
  labs(y="Ratio of outlier")

    

internal_svm <- e1071::svm(internal_feature, y=NULL, nu=0.1, type="one-classification")
outlier_df <- tibble(npi=internal_healthcare$npi, predict = factor(if_else(predict(internal_svm)==TRUE, 0, 1))) 

outlier_df <- healthcare %>% 
  filter(nppes_entity_code=="I", provider_type == "Internal Medicine", mesh %in% c("306371")) %>% 
  left_join(outlier_df) %>% 
  select(npi, longitude, latitude, outlier) %>% 
  count(npi, longitude, latitude, outlier) %>% 
  select(-n) %>% 
  mutate(predict = ifelse(outlier==0, "normal", "outlier"))
  
# plot outlier ------------------------------------------------------------

states <- map_data("state")
county <- map_data("county")

ma <- states %>% 
  filter(region=="massachusetts")

gg_ma <- ma %>% 
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group), color="black", fill="white")+
  theme(text = element_text(size=20))

gg_out <- outlier_df %>% 
  group_by(lon, lat, Class) %>% 
  summarize(`Number of provider`=n()) %>% 
  geom_point(data=., aes(lon, lat, size=`Number of provider`, color=Class), alpha=0.4)

gg_bostonmesh <- healthcare %>% 
  filter(mesh=="306371") %>% 
  group_by(mesh, NE_lat, NE_lon, SW_lon, SW_lat) %>% 
  summarize() %>% 
  geom_rect(data=., aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat), fill=NA, color="black")

gg_ma + gg_bostonmesh + gg_out +
  scale_color_manual(name="Class", values=c("outlier"="red3", "normal"="steelblue"))+
  labs(x="Longitude", y="Latitude")


outlier_df %>% 
  #Individual・内科・meshcodeの選択
  filter(nppes_entity_code=="I", provider_type == "Internal Medicine", mesh %in% c("306371")) %>% 
  #金額ベース
  select(npi, hcpcs_code, line_srvc_cnt,
         average_Medicare_allowed_amt, average_submitted_chrg_amt, average_Medicare_payment_amt, average_Medicare_standard_amt, outlier) %>% 
  transmute(npi = npi,
            hcpcs_code = hcpcs_code,
            #allowed_amt = average_Medicare_allowed_amt*line_srvc_cnt,
            submitted_chrg_amt = average_submitted_chrg_amt*line_srvc_cnt,
            outlier = outlier) %>% 
  #medicare_payment_amt = average_Medicare_payment_amt*line_srvc_cnt,
  #medicare_standard_amt = average_Medicare_standard_amt*line_srvc_cnt,
  group_by(npi, hcpcs_code, outlier) %>% 
  summarize(submitted_chrg_amt = sum(submitted_chrg_amt)) %>% 
  ungroup() %>% 
  group_by(hcpcs_code) %>% 
  mutate(hcpcs_count = n()) %>% 
  ungroup() %>% 
  filter(between(dense_rank(desc(hcpcs_count)),1,10)) %>%
  select(-hcpcs_count) %>% 
  complete(hcpcs_code, nesting(npi, outlier), fill=list(submitted_chrg_amt=0)) %>% 
  ggplot(aes(x=log(1+submitted_chrg_amt), y=hcpcs_code, fill=outlier))+
  geom_density_ridges(alpha=0.3)+
  theme(text=element_text(size=18))
  #theme_joy()

# boston_healthcare %>% 
#   group_by(hcpcs_code) %>% 
#   mutate(n=n()) %>% 
#   ungroup() %>% 
#   mutate(rank=dense_rank(desc(n))) %>% 
#   filter(between(rank, 1, 5)) %>% 
#   select(-n,-rank) %>% 
#   left_join(., outlier_df, by="npi") %>% 
#   gather(key=which_amount, value=amount, -npi, -hcpcs_code, -outlier) %>% 
#   unite(hcpcs_which_amount, hcpcs_code, which_amount) %>% 
#   spread(key=hcpcs_which_amount, value=amount, fill=0) %>% 
#   #mutate_if(is.double, funs(scale_this(log(.+1)))) %>% 
#   select(-npi) %>% 
#   ggpairs(aes_string(color="outlier"))