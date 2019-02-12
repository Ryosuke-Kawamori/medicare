presicion <- function(X){
  return(X[0,0]/(X[0,0]+X[0,1]))
}
recall <- function(X){
  return(X[[0,0]/X[0,0]+X[1,0]])
}