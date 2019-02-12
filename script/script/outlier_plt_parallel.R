lof_provider_stat_result <- read_rds("data/lof_provider_stat_result.rds")
lof_provider_hcpcs_result <- read_rds("data/lof_provider_hcpcs_result.rds")
lof_providermesh_stat_result <- read_rds("data/lof_providermesh_stat_result.rds")
lof_providermesh_hcpcs_result <- read_rds("data/lof_providermesh_hcpcs_result.rds")
lof_mesh_stat_result <- read_rds("data/lof_mesh_stat_result.rds")
lof_mesh_hcpcs_result <- read_rds("data/lof_mesh_hcpcs_result.rds")
original_lof_df <- read_rds("data/original_lof_df.rds")

svm_provider_stat_result <- read_rds("data/svm_provider_stat_result.rds")
svm_provider_hcpcs_result <- read_rds("data/svm_provider_hcpcs_result.rds")
svm_providermesh_stat_result <- read_rds("data/svm_providermesh_stat_result.rds")
svm_providermesh_hcpcs_result <- read_rds("data/svm_providermesh_hcpcs_result.rds")
svm_mesh_stat_result <- read_rds("data/svm_mesh_stat_result.rds")
#svm_mesh_hcpcs_result <- read_rds("data/svm_mesh_hcpcs_result.rds")

result_lof_df <- tibble(Type = c("NONE STAT", "PROVIDER-STAT", "PROVIDER-HCPCS", "PROVIDER-GRID STAT", "PROVIDER-GRID HCPCS", "GRID STAT", "GRID HCPCS"), 
                        results = list(original_lof_df, lof_provider_stat_result, lof_provider_hcpcs_result, lof_providermesh_stat_result, lof_providermesh_hcpcs_result, lof_mesh_stat_result, lof_mesh_hcpcs_result))

result_svm_df <- tibble(Type = c("PROVIDER STAT", "PROVIDER HCPCS", "PROVIDER-GRID STAT", "PROVIDER-GRID HCPCS", "GRID STAT"), 
                        results = list(svm_provider_stat_result, svm_provider_hcpcs_result, svm_providermesh_stat_result, svm_providermesh_hcpcs_result, svm_mesh_stat_result))

result_lof_df %>% 
  dplyr::mutate(s = purrr::map(results, . %>% mutate(s=auc(fp,tp)) %>% dplyr::summarize(sum=sum(s)))) %>% 
  dplyr::select(Type, s) %>% 
  unnest 

result_svm_df %>% 
  dplyr::mutate(s = purrr::map(results, . %>% mutate(s=auc(fpr,tpr)) %>% dplyr::summarize(sum=sum(s)))) %>% 
  dplyr::select(Type, s) %>% 
  unnest 

result_lof_df %>% 
  unnest %>% 
  ggplot(aes(x=fp, y=tp, color=Type))+
  #geom_point()+
  geom_line()+
  geom_abline(intercept = 0)+
  labs(x="FPR", y="TPR")+
  theme(text = element_text(size=25))


result_svm_df %>% 
  unnest %>% 
  ggplot(aes(x=fpr, y=tpr, color=Type))+
  geom_point()+
  geom_abline(intercept = 0)+
  geom_line()+
  labs(x="FPR", y="TPR")+
  theme(text = element_text(size=25))
