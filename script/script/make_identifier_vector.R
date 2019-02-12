health_Provider_type_list <- healthcare %>%
  filter(EntityTypeoftheProvider == "I", ProviderTypeoftheProvider %in% c("Internal Medicine", "Family Practice")) %>% 
  select(NationalProviderIdentifier, HCPCSCode, ProviderTypeoftheProvider, NumberofServices, NumberofMedicareBeneficiaries, mesh) %>% 
  group_by(NationalProviderIdentifier, ProviderTypeoftheProvider, HCPCSCode, mesh) %>% 
  summarise_at(vars(matches("Number.*|Medicare.*")), sum) %>% 
  select(NationalProviderIdentifier, HCPCSCode, ProviderTypeoftheProvider, 
         NumberofServices, NumberofMedicareBeneficiaries, mesh) %>% 
  ungroup() %>% 
  split(x = .,.$ProviderTypeoftheProvider) %>% 
  lapply(., function(x){gather(data=x, key="key", value="value", -NationalProviderIdentifier, -HCPCSCode, -mesh)}) %>% 
  lapply(., function(x){unite(data=x, col=tmp, HCPCSCode, key, sep="")}) %>% 
  lapply(., function(x){spread(data=x, tmp, value, fill=0)}) 


h_ksvm_df <- healthcare %>%
  filter(EntityTypeoftheProvider == "I", mesh=="306371", HCPCSCode=="99213") %>% 
  select(NationalProviderIdentifier, EntityTypeoftheProvider, HCPCSCode, ProviderTypeoftheProvider, NumberofServices, NumberofMedicareBeneficiaries, 
         AverageMedicareAllowedAmount, AverageSubmittedChargeAmount, AverageMedicarePaymentAmount, AverageMedicareStandardizedAmount) %>% 
  mutate(
    MedicareAllowedAmount = AverageMedicareAllowedAmount*NumberofServices,
    MedicareSubmittedAmount = AverageSubmittedChargeAmount*NumberofServices,
    MedicarePaymentAmount = AverageMedicarePaymentAmount*NumberofServices,
    MedicareStandardizedAmount = AverageMedicareStandardizedAmount*NumberofServices
    ) %>% 
  group_by(NationalProviderIdentifier, EntityTypeoftheProvider, ProviderTypeoftheProvider, HCPCSCode) %>% 
  summarise_at(vars(matches("Number.*|Medicare.*")), sum) %>% 
  ungroup() %>% 
  group_by(HCPCSCode) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(HCPCSRank = dense_rank(desc(n))) %>% 
  ungroup() %>% 
  filter(HCPCSRank < 20) %>% 
  select(NationalProviderIdentifier, EntityTypeoftheProvider, HCPCSCode, ProviderTypeoftheProvider, 
         NumberofServices, NumberofMedicareBeneficiaries, MedicareSubmittedAmount, MedicareStandardizedAmount) %>% 
  gather(key="key", value="value", -NationalProviderIdentifier, -EntityTypeoftheProvider, -HCPCSCode, -ProviderTypeoftheProvider) %>% 
  unite(col=tmp, HCPCSCode, key, sep="") %>% 
  spread(tmp, value, fill=0) %>% 
  mutate_at(c(-1,-2,-3), scale_this) %>% 
  mutate(class=1)


#最終的な考えとして、ProviderTypeによってもHCPCSCodeのNumberofServiceは変化してしまう.
#したがって、各Geolocation・ProviderTypeごとにNumberofServiceをくっ付けたベクトルを作るのが良い?→そんなこともなさそう
#結局、SVMのところでProviderごとにクラスタ化してくれている.
ksvm_df <- healthcare %>%
  filter(EntityTypeoftheProvider == "I", mesh %in% c("306371"), ProviderTypeoftheProvider == "Internal Medicine") %>% 
  select(NationalProviderIdentifier, HCPCSCode, NumberofServices, NumberofMedicareBeneficiaries, mesh) %>% 
  group_by(NationalProviderIdentifier, HCPCSCode, mesh) %>% 
  summarise_at(vars(matches("Number.*|Medicare.*")), sum) %>% 
  ungroup() %>% 
  group_by(HCPCSCode) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(HCPCSRank = dense_rank(desc(n))) %>% 
  ungroup() %>% 
  filter(HCPCSRank < 50) %>% 
  select(NationalProviderIdentifier, HCPCSCode, NumberofServices, mesh) %>% # NumberofMedicareBeneficiaries, mesh) %>% 
  gather(key="key", value="value", -NationalProviderIdentifier, -HCPCSCode, -mesh) %>% 
  unite(col=tmp, HCPCSCode, key, sep="") %>% 
  spread(tmp, value, fill=0) %>% 
  mutate_at(c(-1,-2,-3), scale_this) 
  #ggpairs(data=., columns = 3:10, mapping = aes(color=mesh))

##クラス付きベクトル(1が正常2が異常)
ksvm_df_c <- ksvm_df %>% 
  mutate(color=detect_outlier(c(-1,-2,-3),. ))

ksvm_df_c %>% 
  ggpairs(data=., columns=c(44,46,47,48), mapping=aes(color=color),
          lower = list(continuous = wrap("points", alpha = 0.8, size=1.4)))+
  theme(text = element_text(size = 15), axis.text=element_text(size=0.01))

outlier <- ksvm_df_c %>% 
  filter(color==2) %>% 
  {.$NationalProviderIdentifier}

##異常か否かによってhistogramの色を変えてプロット
healthcare %>% 
  filter(EntityTypeoftheProvider == "I", mesh %in% c("306570")) %>% 
  mutate(color = ifelse(NationalProviderIdentifier %in% outlier, 1,2)) %>%
  group_by(HCPCSCode) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(rank = dense_rank(desc(n)), color=as.factor(color)) %>% 
  filter(rank < 5) %>% 
  ggplot(aes(NumberofServices)) +
  geom_histogram(aes(fill = color), binwidth=10)+
  facet_wrap(~HCPCSCode, scale="free")+
  theme(text = element_text(size=20))

  
healthcare %>% 
  filter(NationalProviderIdentifier %in% outlier) %>% 
  group_by(NationalProviderIdentifier, ProviderTypeoftheProvider) %>% 
  summarize() %>% 
  ggplot(aes(ProviderTypeoftheProvider))+
  geom_bar()+
  theme(axis.text.x = element_text(angle=90, hjust=1))

  