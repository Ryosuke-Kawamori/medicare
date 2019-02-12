hoteling <- function(X, p=0.99){
  mx <- colMeans(X)
  Xc <- as.matrix(X) - matrix(1, nrow(X), 1) %*% mx
  Sx <- t(Xc) %*% Xc / nrow(X)
  am <- rowSums((Xc %*% solve(Sx)) * Xc)
  thr <- qchisq(p, 3)
  return((am > thr)*1)
}

mahalanobis_distance <- function(X){
  m_X <- colMeans(X)
  v_X <- var(X)
  return(mahalanobis(X, m_X, v_X))
}

reduce_dim <- function(X, r=0.95){
  
  pr <- prcomp(X, scale=TRUE)
  U <- pr$rotation
  if(nrow(U)==1){
    col_r <- 1
  }else{
    col_r <- 1:(head(which(summary(pr)$importance["Cumulative Proportion", ] > r), 1))
  }
  U_r <- U[ ,col_r]
  
  return(X %*% U_r)
}

one_svm <- function(mat, nu=0.1, max_size=10000){
  #   row_index <- sample(1:nrow(mat), size=max_size)
  # }else{
  #   row_index <- 1:nrow(mat)
  # }
  svm_model <- svm(mat, y=NULL, nu=nu, type="one-classification")
  outlier_df <- tibble(predict = factor(if_else(predict(svm_model)==TRUE, 0, 1)))
  return(outlier_df)
}

iof <- function(mat, trees=100, subtrees=10){
  mod = iForest(X=mat, trees, subtrees)
  return(predict(mod, mat))
}


lof_df <- function(df, group_name, feature_name, min_pts=20, leie){
  predicted_lof <- df %>% 
    dplyr::mutate(predict = purrr::map(!!feature_name, function(x){lof(dist(x), k=min_pts)})) %>% 
    dplyr::mutate(label = purrr::map2(npi, predict, cbind)) %>% 
    dplyr::select(!!!group_name, label) 
  
  lof_result <- predicted_lof %>% 
    unnest 
  colnames(lof_result) <- c(sapply(group_name, quo_name), "npi", "predict")
  
  lof_result <- lof_result %>% 
    group_by(!!!group_name) %>% 
    mutate(rank = percent_rank(predict)) %>% 
    ungroup %>% 
    left_join(leie, by = c("npi"="NPI")) %>% 
    tidyr::replace_na(list(class=0)) %>% 
    arrange(desc(rank)) %>% 
    mutate(not_class = if_else(class == 0, 1, 0)) %>% 
    mutate(tp = cumsum(class)/sum(class)) %>% 
    mutate(fp = cumsum(not_class)/sum(not_class))
  return(lof_result)
}


lof_df_parallel <- function(df, group_name, feature_name, min_pts=20, leie){
  predicted_lof <- df %>% 
    dplyr::mutate(predict = purrr::map(!!feature_name, function(x){lof(dist(x), k=min_pts)})) %>% 
    dplyr::mutate(label = purrr::map2(npi, predict, cbind)) %>% 
    dplyr::select(!!!group_name, label) 
  
  lof_result <- predicted_lof %>% 
    collect() %>% 
    unnest()
  colnames(lof_result) <- c(sapply(group_name, quo_name), "npi", "predict")
  
  lof_result <- lof_result %>% 
    group_by(!!!group_name) %>% 
    mutate(rank = percent_rank(predict)) %>% 
    ungroup %>% 
    left_join(leie, by = c("npi"="NPI")) %>% 
    tidyr::replace_na(list(class=0)) %>% 
    arrange(desc(rank)) %>% 
    mutate(not_class = if_else(class == 0, 1, 0)) %>% 
    mutate(tp = cumsum(class)/sum(class)) %>% 
    mutate(fp = cumsum(not_class)/sum(not_class))
  return(lof_result)
}



svm_df_parallel <- function(df, group_name, feature_name, nu=seq(0.001, 0.98, length=20), leie, cluster){
  
  predicted_df <- lapply(nu, function(hypar){ 
    cluster_copy(cluster, hypar)
    print(hypar)
    predict_df <- df %>% 
      dplyr::mutate(predict = purrr::map(!!feature_name, function(x){one_svm(x, nu=hypar)})) %>% 
      dplyr::mutate(label = purrr::map2(npi, predict, cbind)) %>% 
      collect() %>% 
      ungroup() %>% 
      dplyr::select(label) %>% 
      map_df(bind_rows)
    
    colnames(predict_df) <- c("npi", "predict") 
    conf <- left_join(predict_df, leie, by=c("npi"="NPI")) %>% 
      tidyr::replace_na(list(class=0)) 
    return(conf)
  })
  
  svm_result <- tibble(nu, conf_df = predicted_df) %>% 
    dplyr::mutate(conf_df = purrr::map(conf_df, function(x){x %>% count(predict, class)})) %>% 
    mutate(fpr = purrr::map(conf_df, function(x){fpr(conf_mat(x))})) %>% 
    mutate(tpr = purrr::map(conf_df, function(x){tpr(conf_mat(x))})) %>% 
    select(nu, fpr, tpr) %>% 
    unnest()
  
  return(svm_result)
}