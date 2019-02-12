conf_mat <- function(df){
  df <- df %>% 
    complete(class, predict, fill=list(n=0))
  row <- df$class %>% as.character %>% as.numeric
  col <- df$predict %>% as.character %>% as.numeric

  conf_mat <- matrix(0, nrow=2, ncol=2)
  rownames(conf_mat) <- c("false", "true")
  colnames(conf_mat) <- c("false", "true")
  for(i in row){
    for(j in col){
      conf_mat[i+1,j+1] <- df %>% filter(class==i, predict==j) %>% .$n
    }
  }
  return(conf_mat)
}

fpr <- function(conf_df){
  return(conf_df["false","true"]/(conf_df["false","false"]+conf_df["false","true"]))
}

tpr <- function(conf_df){
  return(conf_df["true","true"]/(conf_df["true","true"]+conf_df["true","false"]))
}

tnr <- function(conf_df){
  return(conf_df["false","false"]/(conf_df["false","false"]+conf_df["true","false"]))
}

auc <- function(fpr, tpr){
  return((fpr-lag(fpr,default=0))*(tpr+lag(tpr,default=0))*0.5)
}
