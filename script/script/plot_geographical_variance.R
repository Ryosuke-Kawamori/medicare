
# 地図の準備 -------------------------------------------------------------------

cCyan <- "#00a0e9"
cMagenta <- "#e4007f"
cGreen <- "#009944"
cOrange <- "#f39800"
cLightBlue <- "#0068b7"

states <- map_data("state")
county <- map_data("county")

g_base <- ggplot()+
  geom_polygon(data=states, aes(x=long, y=lat, group=group), color="black", fill=NA)+
  geom_polygon(data=county, aes(x=long, y=lat, group=group), fill="gray", color="white", size=0.1)+
  coord_fixed(xlim = c(-123, -69),  ylim = c(23, 48), ratio = 1.3)


# Total Submitted amountのGeographical Varianceをヒストグラムプロット+地図プロット ---------------------------------

mesh_mean <- healthcare %>% 
  filter(hcpcs_code == "99213", provider_type == "Internal Medicine") %>% 
  group_by(mesh) %>% 
  summarize(mean=mean(average_submitted_chrg_amt*line_srvc_cnt, na.rm=TRUE), n=n()) %>% 
  filter(n>10) 

mesh_mean %>% 
  ggplot(aes(mean))+
  geom_histogram()+
  labs(x="Mean of total submitted amount in each mesh", y="Count")

state_mean <- healthcare %>% 
  filter(hcpcs_code == "99213", provider_type == "Internal Medicine") %>% 
  group_by(region) %>% 
  summarize(mean=mean(average_submitted_chrg_amt*line_srvc_cnt, na.rm=TRUE), n=n()) %>% 
  filter(n>10) 

state_mean %>% 
  left_join(state_beneficiaries, by=c("region" = "Area_of_Residence")) %>% 
  ggplot(aes(Total_Medicare_Enrollees, mean))+
  geom_point()+
  geom_label_repel(aes(label=region))+
  #geom_text(aes(label=region), hjust=0, vjust=0)+
  labs(x="Total_Medicare_Enrollees", y="Mean of Total submitted amount in each region")

g_mesh_mean <- mesh_mean %>% 
  inner_join(zipcode) %>% 
  rename(`Total submitted amount` = mean) %>% 
  geom_rect(data = ., aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat, fill=`Total submitted amount`), alpha=1, size=1.4) 

g_base+g_mesh_mean+scale_fill_gradient(low=cLightBlue, high=cOrange)+labs(x="Longitude", y="Latitude", trans="log")


# 平均SpecitiesのGeographical Varianceをヒストグラムプロット+地図プロット---------------------------

#meshごとのnpi speciality数平均を調べる
mesh_mean <- healthcare %>%
  count(npi, spc, mesh) %>% 
  group_by(mesh) %>% 
  select(-n) %>% 
  summarize(mean=mean(spc), n=n()) %>% 
  filter(n>10)

mesh_mean %>% 
  ggplot(aes(mean))+
  geom_histogram()+
  labs(x="Average Number of Specialities", y="Count")

g_mesh_mean <- mesh_mean %>% 
  left_join(zipcode) %>% 
  rename(`Average number of specialities` = mean) %>% 
  geom_rect(data = ., aes(xmin=SW_lon, xmax=NE_lon, ymin=SW_lat, ymax=NE_lat, fill=`Average number of specialities`), alpha=1, size=1.4) 

#stateごとのnpi speciality数平均を調べる
state_mean <- healthcare %>% 
  count(npi, region, spc) %>% 
  select(-n) %>% 
  group_by(region) %>% 
  summarize(mean=mean(spc), n=n()) 

#countyごとのnpi speciality数平均を調べる
county_mean <- healthcare %>% 
   count(npi, cntname, spc) %>% 
   group_by(cntname) %>% 
   select(-n) %>% 
   summarize(mean=mean(spc), n=n())

#stateごとのmedicare enrollees数を持ってくる
state_beneficiaries <- read_excel("data/CPS_MDCR_ENROLL_AB_8.xlsx") 
state_beneficiaries <- state_beneficiaries %>% 
  na.omit() 
colnames(state_beneficiaries) <- str_replace_all(state_beneficiaries[1,], " ", "_")
state_beneficiaries <- state_beneficiaries[-1,] %>% 
  mutate_at(-1, as.double) %>% 
  mutate(Area_of_Residence = str_to_lower(Area_of_Residence)) %>% 
  .[-1,]


# countyごとの統計 -------------------------------------------------------------------

#countyごとのmedicare enrollees数を持ってくる
county_beneficiaries <- read_excel("data/State_County_All_Table.xlsx", sheet="State_county 2015", skip=1)
colnames(county_beneficiaries) <- str_to_lower(colnames(county_beneficiaries) %>% 
                                                 str_replace_all(" |-", "_"))

#countyごとのvarianceの計算・可視化
county_beneficiaries <- county_beneficiaries %>% 
  mutate(cnty2name = paste(county, state, sep=" "))

g_county <- county_beneficiaries %>% 
  mutate(county=stringr::str_to_lower(county)) %>% 
  right_join(., county, by=c("county"="subregion")) %>% 
  geom_polygon(data=., aes(group=group, x=long, y=lat, fill=beneficiaries_with_part_a_and_part_b))

#countyごとのprovider請求額平均
county_mean_spending <- healthcare %>% 
  mutate(spending = average_submitted_chrg_amt*line_srvc_cnt) %>% 
  group_by(region, npi) %>% 
  summarize(spending=sum(spending, na.rm=TRUE)) %>% 
  summarize(spending=mean(spending, na.rm=TRUE)) 

g_county_spending <- county_mean_spending %>% 
  right_join(., county, by=c("region"="county_mean_spending"))
  
#medicare enrollee-Average number of specialityのプロット
state_mean %>% 
  left_join(state_beneficiaries, by=c("region" = "Area_of_Residence")) %>% 
  ggplot(aes(Total_Medicare_Enrollees, mean))+
  geom_point()+
  geom_label_repel(aes(label=region))+
  #geom_text(aes(label=region), hjust=0, vjust=0)+
  labs(x="Total_Medicare_Enrollees", y="Average Number of Specialities")

#meshごとのnumber of speciality plot
g_base + g_mesh_mean + scale_fill_gradient(low=cLightBlue, high=cOrange)+labs(x="Longitude", y="Latitude")
  

#medicare enrollee-Average number of specialityのプロット
county_mean %>% 
  left_join(county_beneficiaries, by=c("cntname"="cnty2name")) %>% 
  ggplot(aes(beneficiaries_with_part_a_and_part_b, mean))+
  geom_point()
