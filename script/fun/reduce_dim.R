reduce_dim <- function(X, r=0.95){
  
  pr <- prcomp(X, scale=TRUE)
  U <- pr$rotation
  col_r <- 1:(tail(which(summary(pr)$importance["Cumulative Proportion", ] < 0.95), 1) + 1)
  U_r <- U[ ,col_r]
  
  return(X %*% U_r)
}