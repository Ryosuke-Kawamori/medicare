library(tidyverse)
library(stringr)
library(ggmap)
library(maps)
library(mapdata)
library(zipcode)
library(GGally)
library(kernlab)
#library(tidytext)
library(e1071)
#library(SparseM)
library(readxl)
#library(ggjoy)
library(lubridate)
library(dbscan)
library(DMwR)
library(keras)

library(mltools)
library(multidplyr)

source("script/fun/mesh.R")
#source("script/script/join_geod.R")
#source("script/script/OneSVM.R")

scale_this <- function(x){
  (x-mean(x, na.rm=TRUE)/sd(x, na.rm=TRUE))
}
