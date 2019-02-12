boston_healthcare %>% 
  left_join(outlier_df) %>% 
  na.omit() %>% 
  group_by(hcpcs_code) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(rank=dense_rank(desc(n))) %>% 
  filter(rank<=30) %>% 
  ggplot(aes(y=log(submitted_chrg_amt),x=hcpcs_code,color=outlier))+
  geom_boxplot()+
  coord_flip()

#boston_healthcare %>% 
#  left_join()

healthcare %>% 
  filter(hcpcs_code=="99214") %>% 
  select(hcpcs_description)

boston_outmean <- boston_healthcare %>% 
  left_join(outlier_df) %>% 
  na.omit() %>% 
  group_by(hcpcs_code) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(rank=dense_rank(desc(n))) %>% 
  filter(rank<=15) %>% 
  complete(nesting(npi, outlier), hcpcs_code, fill=list(submitted_chrg_amt=0)) %>% 
  filter(outlier==0) %>% 
  group_by(hcpcs_code) %>% 
  summarize(mean=mean(submitted_chrg_amt))

boston_healthcare %>% 
  left_join(outlier_df) %>% 
  na.omit() %>% 
  group_by(hcpcs_code) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(rank=dense_rank(desc(n))) %>% 
  filter(rank<=10) %>% 
  complete(nesting(npi, outlier), hcpcs_code, fill=list(submitted_chrg_amt=0)) %>% 
  #filter(hcpcs_code=="99213") %>% 
  ggplot(aes(x=log(1+submitted_chrg_amt), y=hcpcs_code, fill=outlier))+
  geom_density_ridges(alpha=0.3)+
  scale_fill_hue(labels = c("0"="outlier", "1"="normal"))+
  theme(text=element_text(size=18))
  #geom_histogram(alpha=0.5, positoin="identity")
    
boston_healthcare %>% 
  left_join(outlier_df) %>% 
  na.omit() %>% 
  group_by(hcpcs_code) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(rank=dense_rank(desc(n))) %>% 
  filter(rank<=15) %>% 
  complete(nesting(npi, outlier, Class), hcpcs_code, fill=list(submitted_chrg_amt=0)) %>% 
  ggplot(aes(x=log(1+submitted_chrg_amt), y=hcpcs_code, fill=outlier))+
  geom_joy2(alpha=0.5, stat="binline")+
#  geom_joy2(alpha=0.3)+
  facet_grid(.~Class)+
  scale_fill_hue(labels = c("0"="outlier", "1"="normal"))+
  theme(axis.title = element_text(size=15), legend.text = element_text(size=15), legend.title = element_text(size=15))+
  labs(x="log(1+total submitted amount of money)")

all_point <- boston_healthcare %>% 
  left_join(outlier_df) %>% 
  na.omit() %>% 
  group_by(hcpcs_code) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(rank=dense_rank(desc(n))) %>% 
  filter(rank<10) %>% 
  complete(nesting(npi, outlier), hcpcs_code, fill=list(submitted_chrg_amt=0))

normal_point <- all_point %>% 
  filter(outlier==1)

one_outlier <- all_point %>% 
  filter(npi == "1902804107")

normal_point %>% 
  ggplot(aes(x=log(1+submitted_chrg_amt), fill=outlier))+
  geom_histogram()+
  geom_density(alpha=0.2)+
  facet_wrap(~hcpcs_code, nrow=2)+
  geom_dotplot(data=one_outlier, dotsize=2, color=NA)+
  labs(x="log(1+total submitted amount of money)")+
  scale_fill_hue(labels = c("0"="outlier", "1"="normal"))+
  theme(axis.title = element_text(size=15), legend.text = element_text(size=15), legend.title = element_text(size=15))
