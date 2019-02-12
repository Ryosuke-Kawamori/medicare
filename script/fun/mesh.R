source("http://www.fttsus.jp/worldmesh/R/worldmesh.R")
cal_meshcode1_vec <- function(lat,lon){mapply(cal_meshcode1,lat,lon)}
cal_meshcode2_vec <- function(lat,lon){mapply(cal_meshcode2,lat,lon)}
meshcode_to_latlong_NE_vec <- function(x){sapply(x, meshcode_to_latlong_NE)}
meshcode_to_latlong_NW_vec <- function(x){sapply(x, meshcode_to_latlong_NW)}
meshcode_to_latlong_SE_vec <- function(x){sapply(x, meshcode_to_latlong_SE)}
meshcode_to_latlong_SW_vec <- function(x){sapply(x, meshcode_to_latlong_SW)}


scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}
 