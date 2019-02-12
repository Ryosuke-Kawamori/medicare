library(Hmisc)
library(stringr)
library()

taxonomy <- read_csv("data/CROSSWALK_MEDICARE_PROVIDER_SUPPLIER_to_HEALTHCARE_PROVIDER_TAXONOMY.csv")

provider_speciality <- taxonomy %>% 
  fill(`MEDICARE PROVIDER/SUPPLIER TYPE DESCRIPTION`) %>% 
  count(`MEDICARE PROVIDER/SUPPLIER TYPE DESCRIPTION`) 

group <- provider_speciality$`MEDICARE PROVIDER/SUPPLIER TYPE DESCRIPTION`
group_n <- provider_speciality$n

#taxonomy <- as.data.frame(taxonomy)

taxonomy <- taxonomy %>% 
  select(-`MEDICARE SPECIALTY CODE`)

latex(taxonomy, file="data/taxonomy.tex", 
      caption="Provider's specialities and their taxonomy codes.", 
      label="tb:taxonomy",
      #    n.rgroup = group_n,
      #    rgroup = group,
      #    dcolumn=TRUE,
      longtable = TRUE,
      rowname = NULL,
      size = "tiny",
      line.pages= 1000
      #landscape=TRUE
    )