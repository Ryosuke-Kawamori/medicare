detect_outlier <- function(remove, df){
  new_df <- df %>% mutate(class=1)
  ksvm_model <- ksvm(class~.,
                     data=as.matrix(new_df[,remove]),
                     type="one-svc",
                     kernel="rbfdot",
                     kpar=list(sigma=0.1),
                     nu=0.01,
                     cross=0.01
                     )
  return(as.factor(ifelse(predict(ksvm_model)==TRUE,1,2)))
}

detect_outlier <- function(remove, df){
  
  remove <- enquo(remove)
  
  new_df <- df %>%
    mutate(class=1) %>% 
    select(-!!remove)
  
  svm_model <- svm(x = as.matrix(new_df),
                   y = NULL,
                   type="one-classification",
                   kernel="radial",
                   nu=0.01,
                   cross=0.01
  )
  return(as.factor(ifelse(predict(svm_model)==TRUE,1,2)))
}
