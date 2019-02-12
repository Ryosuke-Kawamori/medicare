#submitted amount of money
featurize_provider_hcpcs <- function(df){
  feature_df <- df %>% 
    select(npi, hcpcs_code, line_srvc_cnt, average_submitted_chrg_amt) %>% 
    transmute(npi = npi,
              hcpcs_code = hcpcs_code,
              submitted_chrg_amt = line_srvc_cnt*average_submitted_chrg_amt) %>% 
              #submitted_chrg_amt = average_submitted_chrg_amt*line_srvc_cnt) %>% 
    group_by(npi, hcpcs_code) %>% 
    dplyr::summarize(submitted_chrg_amt = sum(submitted_chrg_amt)) %>% 
    ungroup() %>% 
    
    # group_by(hcpcs_code) %>% 
    # mutate(n=n()) %>% 
    # ungroup() %>% 
    # mutate(rank=dense_rank(desc(n))) %>% 
    # filter(rank<50) %>% select(-n,-rank) %>% 
    
    gather(key=which_amount, value=amount, -npi, -hcpcs_code) %>% 
    unite(hcpcs_which_amount, hcpcs_code, which_amount) %>% 
    spread(key=hcpcs_which_amount, value=amount, fill=0) 
  
  feature_name <- feature_df$npi
  
  internal_feature_mat <- feature_df %>% 
    mutate_if(is.double, funs(scale_this(log(.+1)))) %>% 
    select(-npi) %>% 
    data.matrix
  rownames(internal_feature_mat) <- feature_name
  return(internal_feature_mat[, apply(internal_feature_mat, 2, 
                                      function(x){!any(is.nan(x)) & any(!is.infinite(x))}), drop=FALSE])
}

#submitted amount of money and ratio of unique beneficiaries
featurize_provider2 <- function(df){
  feature_df <- df %>% 
    select(npi, hcpcs_code, line_srvc_cnt, average_submitted_chrg_amt, bene_unique_cnt) %>% 
    group_by(npi, hcpcs_code) %>% 
    dplyr::summarize(line_srvc_cnt = mean(line_srvc_cnt), average_submitted_chrg_amt = mean(average_submitted_chrg_amt), 
              bene_unique_cnt = mean(bene_unique_cnt)) %>% 
    ungroup() %>% 
    transmute(npi = npi,
              hcpcs_code = hcpcs_code,
              submitted_chrg_amt = line_srvc_cnt*average_submitted_chrg_amt,
              ratio_of_unique = bene_unique_cnt/line_srvc_cnt) %>% 
    gather(key=which_amount, value=amount, -npi, -hcpcs_code) %>% 
    unite(hcpcs_which_amount, hcpcs_code, which_amount) %>% 
    spread(key=hcpcs_which_amount, value=amount, fill=0) 
  
  feature_name <- feature_df$npi
  
  internal_feature_mat <- feature_df %>% 
    mutate_if(is.double, funs(scale_this(log(.+1)))) %>% 
    select(-npi) %>% 
    data.matrix
  rownames(internal_feature_mat) <- feature_name
  return(internal_feature_mat[, apply(internal_feature_mat, 2,
                                      function(x){!any(is.nan(x)) & any(!is.infinite(x))}), drop=FALSE])
}

#almosst all features
featurize_provider_all_without_providertype <- function(df){
  feature_df <- df %>% 
    select(npi, hcpcs_code, line_srvc_cnt, bene_unique_cnt, bene_day_srvc_cnt, average_submitted_chrg_amt, average_Medicare_allowed_amt) %>% 
    group_by(npi, hcpcs_code) %>% 
    dplyr::summarize(line_srvc_cnt = mean(line_srvc_cnt), bene_unique_cnt = mean(bene_unique_cnt),
              average_submitted_chrg_amt = mean(average_submitted_chrg_amt), 
              bene_day_srvc_cnt = mean(bene_day_srvc_cnt), average_Medicare_allowed_amt = mean(average_Medicare_allowed_amt)) %>% 
    ungroup() %>% 
    gather(key=which_amount, value=amount, -npi, -hcpcs_code) %>% 
    unite(hcpcs_which_amount, hcpcs_code, which_amount) %>% 
    spread(key=hcpcs_which_amount, value=amount, fill=0) 
  
  feature_name <- feature_df$npi
  
  internal_feature_mat <- feature_df %>% 
    mutate_if(is.double, funs(scale_this(log(.+1)))) %>% 
    select(-npi) %>% 
    data.matrix
  rownames(internal_feature_mat) <- feature_name
  return(internal_feature_mat[, apply(internal_feature_mat, 2,
                                      function(x){!any(is.nan(x)) & any(!is.infinite(x))}), drop=FALSE])
}


#all feature
featurize_provider_all <- function(df){
  feature_df <- df %>% 
    select(npi, hcpcs_code, average_submitted_chrg_amt, average_Medicare_payment_amt, 
           line_srvc_cnt, bene_unique_cnt, bene_day_srvc_cnt) %>% 
    group_by(npi, hcpcs_code) %>% 
    dplyr::summarize(average_submitted_chrg_amt = mean(average_submitted_chrg_amt), 
              average_Medicare_payment_amt = mean(average_Medicare_payment_amt), 
              line_srvc_cnt = mean(line_srvc_cnt), 
              bene_unique_cnt = mean(bene_unique_cnt),
              bene_day_srvc_cnt = mean(bene_day_srvc_cnt)) %>% 
    ungroup() %>% 
    gather(key=key, value=value, -npi, -hcpcs_code) %>% 
    group_by(npi, key) %>% 
    dplyr::summarize(sd = sd(value), mean = mean(value), min = min(value), max = max(value), median = median(value), sum = sum(value)) %>% 
    ungroup() %>% 
    replace_na(list(sd=0)) %>% 
    gather(key=stat, value=value, -npi, -key) %>% 
    mutate(key=paste(key, stat, sep="_")) %>% 
    select(-stat) %>% 
    spread(key, value)
  
  gender_df <- df %>% 
    distinct(npi, nppes_provider_gender) %>% 
    mutate(nppes_provider_gender = if_else(nppes_provider_gender=="M", 1, 0))
  
  provider_type <- df %>%
    distinct(npi, provider_type) %>% 
    mutate(provider_type = as.integer(as.factor(provider_type))) %>% 
    .$provider_type %>% 
    mltools::one_hot() %>% 
    as_tibble() 
  
  provider_type <- cbind(df %>% distinct(npi), provider_type) %>% as_tibble()
  
  feature_df <- left_join(feature_df, gender_df) %>% 
    left_join(provider_type)
  
  feature_name <- feature_df$npi
  
  feature_mat <- feature_df %>% 
    mutate_if(is.double, funs(scale_this(log(.+1)))) %>% 
    select(-npi) %>% 
    data.matrix
  
  rownames(feature_mat) <- feature_name
  return(feature_mat[, apply(feature_mat, 2, 
                                      function(x){!any(is.nan(x)) & any(!is.infinite(x))}), drop=FALSE])
}


#stat feature
featurize_provider_stat <- function(df){
  feature_df <- df %>% 
    select(npi, hcpcs_code, average_submitted_chrg_amt, average_Medicare_payment_amt, 
           line_srvc_cnt, bene_unique_cnt, bene_day_srvc_cnt) %>% 
    group_by(npi, hcpcs_code) %>% 
    dplyr::summarize(average_submitted_chrg_amt = mean(average_submitted_chrg_amt), 
              average_Medicare_payment_amt = mean(average_Medicare_payment_amt), 
              line_srvc_cnt = mean(line_srvc_cnt), 
              bene_unique_cnt = mean(bene_unique_cnt),
              bene_day_srvc_cnt = mean(bene_day_srvc_cnt)) %>% 
    ungroup() %>% 
    gather(key=key, value=value, -npi, -hcpcs_code) %>% 
    group_by(npi, key) %>% 
    dplyr::summarize(sd = sd(value), mean = mean(value), min = min(value), max = max(value), median = median(value), sum = sum(value)) %>% 
    ungroup() %>% 
    replace_na(list(sd=0)) %>% 
    gather(key=stat, value=value, -npi, -key) %>% 
    mutate(key=paste(key, stat, sep="_")) %>% 
    select(-stat) %>% 
    spread(key, value)
  
  gender_df <- df %>% 
    distinct(npi, nppes_provider_gender) %>% 
    mutate(nppes_provider_gender = if_else(nppes_provider_gender=="M", 1, 0))
  
  provider_type <- df %>%
    distinct(npi, provider_type) %>% 
    mutate(provider_type = as.integer(as.factor(provider_type))) %>% 
    .$provider_type %>% 
    mltools::one_hot() %>% 
    as_tibble() 
  
  provider_type <- cbind(df %>% distinct(npi), provider_type) %>% as_tibble()
  
  feature_df <- left_join(feature_df, gender_df) %>% 
    left_join(provider_type)
  
  feature_name <- feature_df$npi
  
  feature_mat <- feature_df %>% 
    mutate_if(is.double, funs(scale_this(log(.+1)))) %>% 
    select(-npi) %>% 
    data.matrix
  
  rownames(feature_mat) <- feature_name
  return(feature_mat[, apply(feature_mat, 2, 
                             function(x){!any(is.nan(x)) & any(!is.infinite(x))}), drop=FALSE])
}

#stat feature
featurize_provider_stat_without_providertype <- function(df){
  feature_df <- df %>% 
    select(npi, hcpcs_code, average_submitted_chrg_amt, average_Medicare_payment_amt, 
           line_srvc_cnt, bene_unique_cnt, bene_day_srvc_cnt) %>% 
    group_by(npi, hcpcs_code) %>% 
    dplyr::summarize(average_submitted_chrg_amt = mean(average_submitted_chrg_amt), 
              average_Medicare_payment_amt = mean(average_Medicare_payment_amt), 
              line_srvc_cnt = mean(line_srvc_cnt), 
              bene_unique_cnt = mean(bene_unique_cnt),
              bene_day_srvc_cnt = mean(bene_day_srvc_cnt)) %>% 
    ungroup() %>% 
    gather(key=key, value=value, -npi, -hcpcs_code) %>% 
    group_by(npi, key) %>% 
    dplyr::summarize(sd = sd(value), mean = mean(value), min = min(value), max = max(value), median = median(value), sum = sum(value)) %>% 
    ungroup() %>% 
    replace_na(list(sd=0)) %>% 
    gather(key=stat, value=value, -npi, -key) %>% 
    mutate(key=paste(key, stat, sep="_")) %>% 
    select(-stat) %>% 
    spread(key, value)
  
  gender_df <- df %>% 
    distinct(npi, nppes_provider_gender) %>% 
    mutate(nppes_provider_gender = if_else(nppes_provider_gender=="M", 1, 0))
  
  feature_df <- left_join(feature_df, gender_df) 
  
  feature_name <- feature_df$npi
  
  feature_mat <- feature_df %>% 
    mutate_if(is.double, funs(scale_this(log(.+1)))) %>% 
    select(-npi) %>% 
    data.matrix
  
  rownames(feature_mat) <- feature_name
  return(feature_mat[, apply(feature_mat, 2, 
                             function(x){!any(is.nan(x)) & any(!is.infinite(x))}), drop=FALSE])
}
