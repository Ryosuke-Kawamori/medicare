nu_range <- seq(0.001, 0.98, length=20)
min_n <- 20
min_pts <- 20
set.seed(100)
#enforce_state <- c("new york", "california", "florida")
enforce_state <- c("new york", "california")
#enforce_state <- c("new york")
#healthcare <- healthcare %>% sample_n(400000)

npi_id <-  healthcare %>% 
  dplyr::filter(region %in% enforce_state) %>% 
  count(npi, mesh, provider_type) %>% 
  select(-n) %>% 
  group_by(mesh, provider_type) %>% 
  mutate(n1=n()) %>% 
  ungroup() %>% 
  group_by(provider_type) %>% 
  mutate(n2=n()) %>% 
  ungroup() %>% 
  dplyr::filter(n1 > min_n, n2 > min_n) %>% 
  count(npi) %>% 
  .$npi


original_paper <- healthcare %>% 
  dplyr::filter(region %in% enforce_state) %>% 
  dplyr::filter(npi %in% npi_id) %>% 
  featurize_provider_stat()
original_paper <- original_paper[apply(original_paper, 1, function(x){!any(is.na(x))}),]


original_result <- lof(original_paper, k=min_pts)
original_lof_df <- tibble(npi = rownames(original_paper), lof = original_result) %>% 
  arrange(desc(lof)) %>% 
  left_join(leie, by = c("npi" =  "NPI")) %>% 
  replace_na(list(class=0)) %>% 
  mutate(not_class = if_else(class == 0, 1, 0)) %>% 
  mutate(tp = cumsum(class/sum(class))) %>% 
  mutate(fp = cumsum(not_class/sum(not_class)))


splited_df_provider <- healthcare %>%
  dplyr::filter(region %in% enforce_state) %>% 
  dplyr::filter(npi %in% npi_id) %>% 
  nest(-provider_type) %>%
  dplyr::mutate(hcpcs = purrr::map(data, featurize_provider_hcpcs)) %>%
#  dplyr::mutate(all = purrr::map(data, featurize_provider_all_without_providertype)) %>% 
  dplyr::mutate(stat = purrr::map(data, featurize_provider_stat_without_providertype)) %>% 
  dplyr::mutate(npi = purrr::map(hcpcs, ~as_tibble(rownames(.)))) %>% 
  dplyr::mutate(n = purrr::map_dbl(hcpcs, nrow)) %>% 
  arrange(desc(n))


splited_df_providermesh <- healthcare %>%
  dplyr::filter(region %in% enforce_state) %>% 
  dplyr::filter(npi %in% npi_id) %>% 
  nest(-mesh, -provider_type) %>%
  dplyr::mutate(hcpcs = purrr::map(data, featurize_provider_hcpcs)) %>%
#  dplyr::mutate(all = purrr::map(data, featurize_provider_all_without_providertype)) %>% 
  dplyr::mutate(stat = purrr::map(data, featurize_provider_stat_without_providertype)) %>% 
  dplyr::mutate(npi = purrr::map(hcpcs, ~as_tibble(rownames(.)))) %>% 
  dplyr::mutate(n = purrr::map_dbl(hcpcs, nrow)) %>% 
  arrange(desc(n)) 

splited_df_mesh <- healthcare %>%
  dplyr::filter(region %in% enforce_state) %>% 
  dplyr::filter(npi %in% npi_id) %>% 
  nest(-mesh) %>% 
  dplyr::mutate(hcpcs = purrr::map(data, featurize_provider_hcpcs)) %>%
#  dplyr::mutate(all = purrr::map(data, featurize_provider_all_without_providertype)) %>% 
  dplyr::mutate(stat = purrr::map(data, featurize_provider_all_without_providertype)) %>% 
  dplyr::mutate(npi = purrr::map(hcpcs, ~as_tibble(rownames(.)))) %>% 
  dplyr::mutate(n = purrr::map_dbl(hcpcs, nrow)) %>% 
  arrange(desc(n)) 


# provider stat feature ---------------------------------------------------

lof_provider_stat_result <- lof_df(df=splited_df_provider,
                                   group_name = quos(provider_type),
                                   feature_name = quo(hcpcs),
                                   min_pts = min_pts,
                                   leie=leie)

# provider hcpcs feature ------------------------------------------------------

lof_provider_hcpcs_result <- lof_df(df=splited_df_provider,
                                    group_name = quos(provider_type),
                                    feature_name = quo(hcpcs),
                                    min_pts = min_pts,
                                    leie=leie)

# provider-mesh hcpcs feature --------------------------------------------------

lof_providermesh_hcpcs_result <- lof_df(df=splited_df_providermesh,
                                        group_name = quos(provider_type, mesh),
                                        feature_name = quo(hcpcs),
                                        min_pts = min_pts,
                                        leie=leie)

# provider-mesh stat feature -------------------------------------------------------

lof_providermesh_stat_result <- lof_df(df=splited_df_providermesh,
                                       group_name = quos(provider_type, mesh),
                                       feature_name = quo(stat),
                                       min_pts=min_pts,
                                       leie=leie)

# mesh hcpcs feature ------------------------------------------------------

lof_mesh_hcpcs_result <- lof_df(df=splited_df_mesh,
                               group_name = quos(mesh),
                               feature_name = quo(hcpcs),
                               min_pts = min_pts,
                               leie=leie)

# Mesh stat feature -------------------------------------------------------

lof_mesh_stat_result <- lof_df(df=splited_df_mesh,
                               group_name = quos(mesh),
                               feature_name = quo(stat),
                               min_pts = min_pts,
                               leie=leie)

# result of lof -----------------------------------------------------------

result_lof_df <- tibble(type = c("none stat", "provider stat", "provider hcpcs", "provider-mesh stat", "provider-mesh hcpcs", "mesh hcpcs", "mesh stat"), 
       results = list(original_lof_df, lof_provider_stat_result, lof_provider_stat_result, lof_providermesh_hcpcs_result, lof_providermesh_stat_result, lof_mesh_hcpcs_result, lof_mesh_stat_result))

result_lof_df %>% 
  dplyr::mutate(s = purrr::map(results, . %>% mutate(s=auc(fp,tp)) %>% dplyr::summarize(sum=sum(s)))) %>% 
  dplyr::select(type, s) %>% 
  unnest

result_lof_df %>% 
  unnest %>% 
  ggplot(aes(x=fp, y=tp, color=type))+
  #geom_point()+
  geom_line()+
  labs(x="fpr", y="tpr")+
  theme(text = element_text(size=20))


# one-svm model hcpcs feature-----------------------------------------------------------


predicted_df_not_mesh <- lapply(seq(0.001, 0.98, length=20), function(hypar){
  print(hypar)
  predict_df <- splited_df_not_mesh %>% 
    dplyr::mutate(predict = purrr::map(data_mat, function(x){one_svm(x, nu=hypar)})) %>% 
    dplyr::mutate(label = purrr::map2(npi, predict, cbind)) %>% 
    dplyr::select(label) %>%
    map_df(bind_rows)
  
  colnames(predict_df) <- c("npi", "predict") 
  conf <- left_join(predict_df, leie, by=c("npi"="NPI")) %>% 
    replace_na(list(class=0)) 
#    count(predict, class) 
#  print(conf)
  return(conf)
})


svm_not_mesh_hcpcs_result <- tibble(nu = seq(0.001, 0.98, length=20), conf_df = predicted_df_not_mesh) %>% 
  dplyr::mutate(conf_df = purrr::map(conf_df, function(x){x %>% count(predict, class)})) %>% 
  mutate(fpr = purrr::map(conf_df, function(x){fpr(conf_mat(x))})) %>% 
  mutate(tpr = purrr::map(conf_df, function(x){tpr(conf_mat(x))})) %>% 
  select(nu, fpr, tpr) %>% 
  unnest() 

ggplot(data = svm_not_mesh_result ,aes(x=fpr, y=tpr))+
  geom_point()+
  geom_line()+
  geom_text(aes(label=round(nu,2), vjust=2))+
  theme(text=element_text(size=30))



# one-svm model hcpcs feature mesh --------------------------------------------


predicted_df_mesh <- lapply(seq(0.001, 0.98, length=20), function(hypar){ 
    print(hypar)
  predict_df <- splited_df_mesh %>% 
    dplyr::mutate(predict = purrr::map(data_mat, function(x){one_svm(x, nu=hypar)})) %>% 
    dplyr::mutate(label = purrr::map2(npi, predict, cbind)) %>% 
    dplyr::select(label) %>%
    map_df(bind_rows)
  
  colnames(predict_df) <- c("npi", "predict") 
  conf <- left_join(predict_df, leie, by=c("npi"="NPI")) %>% 
    replace_na(list(class=0)) 
#    count(predict, class) 
#  print(conf)
  return(conf)
})


svm_mesh_hcpcs_result <- tibble(nu = seq(0.001, 0.98, length=20), conf_df = predicted_df_mesh) %>% 
  dplyr::mutate(conf_df = purrr::map(conf_df, function(x){x %>% count(predict, class)})) %>% 
  mutate(fpr = purrr::map(conf_df, function(x){fpr(conf_mat(x))})) %>% 
  mutate(tpr = purrr::map(conf_df, function(x){tpr(conf_mat(x))})) %>% 
  select(nu, fpr, tpr) %>% 
  unnest()


ggplot(data = svm_model_mesh_result, aes(x=fpr, y=tpr))+
  geom_point()+
  geom_line()+
  geom_text(aes(label=round(nu,2), vjust=2))+
  theme(text=element_text(size=30))



# one-svm model stat feature not mesh-----------------------------------------------------------


predicted_df_not_mesh_stat <- lapply(seq(0.001, 0.98, length=20), function(hypar){
  print(hypar)
  predict_df <- splited_df_not_mesh %>% 
    dplyr::mutate(predict = purrr::map(data_mat8, function(x){one_svm(x, nu=hypar)})) %>% 
    dplyr::mutate(label = purrr::map2(npi, predict, cbind)) %>% 
    dplyr::select(label) %>%
    map_df(bind_rows)
  
  colnames(predict_df) <- c("npi", "predict") 
  conf <- left_join(predict_df, leie, by=c("npi"="NPI")) %>% 
    replace_na(list(class=0))
#    count(predict, class) 
#  print(conf)
  return(conf)
})


svm_not_mesh_result <- tibble(nu = seq(0.001, 0.98, length=20), conf_df = predicted_df_not_mesh_stat) %>% 
  dplyr::mutate(conf_df = purrr::map(conf_df, function(x){x %>% count(predict, class)})) %>% 
  mutate(fpr = purrr::map(conf_df, function(x){fpr(conf_mat(x))})) %>% 
  mutate(tpr = purrr::map(conf_df, function(x){tpr(conf_mat(x))})) %>% 
  select(nu, fpr, tpr) %>% 
  unnest() 


tibble(nu = seq(0.001, 0.98, length=20), conf_df = predicted_df_not_mesh_stat) %>% 
  dplyr::mutate(conf_df = purrr::map(conf_df, function(x){sum(as.integer(as.character(x$predict))/nrow(x))})) %>% 
  unnest %>% 
  ggplot(aes(nu, conf_df))+
  geom_point()+
  geom_line()+
  theme(text = element_text(size=20))+
  labs(x="hyparparamete : nu", y="ratio of outlier")


# one-svm model stat feature mesh ---------------------------------------------


predicted_df_mesh_stat <- lapply(seq(0.001, 0.98, length=20), function(hypar){ 
  print(hypar)
  predict_df <- splited_df_mesh %>% 
    dplyr::mutate(predict = purrr::map(data_mat8, function(x){one_svm(x, nu=hypar)})) %>% 
    dplyr::mutate(label = purrr::map2(npi, predict, cbind)) %>% 
    dplyr::select(label) %>%
    map_df(bind_rows)
  
  colnames(predict_df) <- c("npi", "predict") 
  conf <- left_join(predict_df, leie, by=c("npi"="NPI")) %>% 
    replace_na(list(class=0)) 
#    count(predict, class) 
#  print(conf)
  return(conf)
})


svm_mesh_result <- tibble(nu = seq(0.001, 0.98, length=20), conf_df = predicted_df_mesh_stat) %>% 
  dplyr::mutate(conf_df = purrr::map(conf_df, function(x){x %>% count(predict, class)})) %>% 
  mutate(fpr = purrr::map(conf_df, function(x){fpr(conf_mat(x))})) %>% 
  mutate(tpr = purrr::map(conf_df, function(x){tpr(conf_mat(x))})) %>% 
  select(nu, fpr, tpr) %>% 
  unnest() 


tibble(nu = seq(0.001, 0.98, length=20), conf_df = predicted_df_mesh_stat) %>% 
  dplyr::mutate(conf_df = purrr::map(conf_df, function(x){sum(as.integer(as.character(x$predict))/nrow(x))})) %>% 
  unnest %>% 
  ggplot(aes(nu, conf_df))+
  geom_point()+
  geom_line()


# result of svm -----------------------------------------------------------


result_svm_df <- tibble(type = c("provider stat", "provider hcpcs", "provider-mesh stat", "provider-mesh hcpcs"),
       results = list(svm_not_mesh_result, svm_not_mesh_hcpcs_result, svm_mesh_result, svm_mesh_hcpcs_result)) %>% 
  mutate(results = purrr::map(results, . %>% arrange(fpr)))


result_svm_df %>% 
  dplyr::mutate(s = purrr::map(results, . %>% arrange(fpr) %>% mutate(s=auc(fpr,tpr)) %>% summarize(sum=sum(s)))) %>% 
  select(type, s) %>% 
  unnest

result_svm_df %>% 
  unnest %>% 
  ggplot(aes(x=fpr, y=tpr, color=type))+
  geom_point()+
  geom_line()+
  theme(text = element_text(size=20))


tibble(type=rep("mesh", 20), nu = seq(0.001, 0.98, length=20), conf_df = predicted_df_mesh) %>% 
  rbind(tibble(type=rep("not mesh", 20), nu = seq(0.001, 0.2, length=20), conf_df = predicted_df_not_mesh)) %>% 
  mutate(fpr = purrr::map(conf_df, function(x){fpr(conf_mat(x))})) %>% 
  mutate(tpr = purrr::map(conf_df, function(x){tpr(conf_mat(x))})) %>% 
  select(type, nu, fpr, tpr) %>% 
  unnest() %>% 
  ggplot(aes(x=fpr, y=tpr, color=type))+
  geom_point()+
  geom_line()+
  geom_abline(intercept=0,slope=1)+
  geom_text(aes(label=round(nu,2), vjust=2))+
  theme(text=element_text(size=30))
  
  
feature_mat <- featurize_provider(filterd_healthcare)



# compare split method ----------------------------------------------------


bind_rows(list(original_lof_df %>% mutate(type="non-split"), 
               lof_result %>% mutate(type="provider-split"),
               lof_mesh_result %>%  mutate(type="provider-mesh-split"))) %>% 
ggplot(aes(fp, tp, color=type))+
  geom_point(size=1)+
  theme(text = element_text(size=20))


# compare feature method ----------------------------------------------------


bind_rows(list(lof_hcpcs_result %>% mutate(type="hcpcs-feature-provider-split"),
               lof_mesh_hcpcs_result %>% mutate(type="hcpcs-feature-provider-mesh-split"),
               lof_mesh_result %>%  mutate(type="stat-feature-provider-mesh-split"),
               lof_result %>% mutate(type="stat-feature-provider-split"))) %>% 
  ggplot(aes(fp, tp, color=type))+
  geom_point()+
  theme(text = element_text(size=20))

bind_rows(list(lof_hcpcs_result %>% mutate(type="hcpcs-feature-provider-split"),
               lof_result %>% mutate(type="stat-feature-provider-split"))) %>% 
  ggplot(aes(fp, tp, color=type))+
  geom_point()+
  theme(text = element_text(size=20))

bind_rows(list(lof_mesh_hcpcs_result %>% mutate(type="hcpcs-feature-provider-mesh-split"),
               lof_mesh_result %>%  mutate(type="stat-feature-provider-mesh-split"))) %>% 
  ggplot(aes(fp, tp, color=type))+
  geom_point()+
  theme(text = element_text(size=20))




# tamesi <- healthcare %>% 
#   filter(region %in% enforce_state) %>% 
#   left_join(predict_df) %>% 
#   mutate(amount=line_srvc_cnt*average_submitted_chrg_amt) %>% 
#   group_by(npi, predict, class) %>% 
#   summarize(mean=mean(amount), sd=sd(amount), median=median(amount), sum=sum(amount)) %>% 
#   ungroup()
# 
# tamesi %>% 
#   ggplot(aes(log(1+value)))+
#   geom_histogram()+
#   facet_grid(key~.)+
#   geom_dotplot(data=tamesi %>% filter(predict==1, class==1), aes(value))
# 
# healthcare %>% 
#   filter(region %in% enforce_state) %>% 
#   select(npi, class, bene_unique_cnt, line_srvc_cnt) %>% 
#   gather(key=variable, value=value, -npi, -class) %>% 
#   group_by(npi, class, variable) %>% 
#   summarize(mean=mean(value), sd=sd(value), median=median(value), sum=sum(value)) %>% 
#   replace_na(list(sd=0)) %>% 
#   gather(key=stat, value=value, -npi, -variable, -class) %>% 
#   mutate(variable=paste(variable, stat, sep="_"))
# 
# 
# healthcare %>% 
#   filter(region %in% enforce_state) %>% 
#   select(npi, bene_unique_cnt, line_srvc_cnt) %>% 
#   gather(key=variable, value=value, -npi) %>% 
#   group_by(npi, variable) %>% 
#   summarize(mean=mean(value), sd=sd(value), median=median(value), sum=sum(value)) %>% 
#   ungroup() %>% 
#   replace_na(list(sd=0)) %>% 
#   gather(key=stat, value=value, -npi, -variable) %>% 
#   mutate(variable=paste(variable, stat, sep="_")) %>% 
#   select(-stat) %>% 
#   spread(key=variable, value=value) %>% 
#   left_join(leie, by=c("npi"="NPI")) %>% 
#   replace_na(list(class=0)) %>% 
#   filter(class == 1) %>% 
#   mutate(class = as.factor(class)) %>% 
#   ggplot(aes(x=bene_unique_cnt_mean, y=line_srvc_cnt_mean, fill=class, color=class))+
#   geom_density2d()
