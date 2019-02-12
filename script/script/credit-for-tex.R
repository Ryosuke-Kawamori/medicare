library(tidyverse)
library(e1071)
library(magrittr)
library(reshape2)
library(DMwR)


# define functions --------------------------------------------------------

#scale function
scale_this <- function(x){
return((x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
}

#calc area under curve
auc <- function(fpr, tpr){
  return((fpr-lag(fpr,default=0))*(tpr+lag(tpr,default=0))*0.5)
}

#one support vector machine
one_svm <- function(mat, nu=0.1){
  svm_model <- svm(mat, y=NULL, nu=nu, type="one-classification")
  outlier_df <- tibble(predict = factor(if_else(predict(svm_model)==TRUE, 0, 1)))
  return(outlier_df)
}

#calc confusion matrix from datafrme which have class column and predict column
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

#false positive rate from confusion matrix
fpr <- function(conf_df){
  return(conf_df["false","true"]/(conf_df["false","false"]+conf_df["false","true"]))
}

#true positive rate from confusion matrix
tpr <- function(conf_df){
  return(conf_df["true","true"]/(conf_df["true","true"]+conf_df["true","false"]))
}

set.seed(100)
credit_row <- read_csv("data/credit/creditcard.csv")

N <- nrow(credit_row)
MIN_PTS <- 20
nu_range <- seq(0.001, 0.98, length=20)
#N <- 100000

#credit_fraud <- credit_row %>% filter(Class==1)
#credit_normal <- credit_row %>% filter(Class==0) %>% sample_n(N-nrow(credit_fraud))
#credit <- bind_rows(list(credit_fraud, credit_normal))
credit <- credit_row

x <- credit %>% 
  select(-Time, -Class) %>% 
  mutate_if(is.double, scale_this) %>% 
  as.matrix()

y <- credit %>% 
  select(Class) %>% 
  as.matrix() %>% 
  .[,1]


# One SVM evaluation ------------------------------------------------------

predict_onesvm_df <- tibble(nu = nu_range, x=rep(list(x), length(nu_range)), class=rep(list(tibble(class=y)), length(nu_range))) %>% 
  dplyr::mutate(predict = map2(x, nu, function(data_x, hyper_nu){
    print(hyper_nu) 
    one_svm(mat=data_x, nu=hyper_nu)}))

predict_onesvm_roc <- predict_onesvm_df %>% 
  mutate(df = purrr::map2(class, predict, function(x,y){bind_cols(list(x,y))})) %>% 
  mutate(conf_df = purrr::map(df, . %>% count(class,predict))) %>% 
  mutate(fpr = purrr::map(conf_df, function(x){fpr(conf_mat(x))})) %>% 
  mutate(tpr = purrr::map(conf_df, function(x){tpr(conf_mat(x))})) %>% 
  select(nu, fpr, tpr) %>% 
  unnest() %>% 
  bind_rows(list(., tibble(fpr=c(0,1), tpr=c(0,1)))) %>% 
  arrange(tpr) %>% 
  mutate(s = auc(fpr=fpr, tpr=tpr))
onesvm_auc <- sum(predict_onesvm_roc$s)


# LOF detection -----------------------------------------------------------

predict_lof_df <- lof(x, k=MIN_PTS)

predict_lof_roc <- bind_cols(list(credit, tibble(lof=predict_lof_df))) %>% 
  dplyr::arrange(desc(lof)) %>% 
  mutate(class=Class) %>% 
  dplyr::mutate(not_class = if_else(class == 0, 1, 0)) %>% 
  dplyr::mutate(tpr = cumsum(class)/sum(class)) %>% 
  dplyr::mutate(fpr = cumsum(not_class)/sum(not_class)) %>% 
  mutate(s = auc(fpr=fpr, tpr=tpr))

lof_auc <- sum(predict_lof_roc$s)

predict_onesvm_roc <- read_rds("data/predict_onesvm_roc.rds")
predict_lof_roc <- read_rds("data/predict_lof_roc.rds")


tibble(Model=c("One-SVM","LOF"), list(predict_onesvm_roc, predict_lof_roc)) %>% 
  unnest() %>% 
  ggplot(aes(fpr, tpr, color=Model))+
  geom_line()+
  theme(text=element_text(size=20))+
  labs(x="FPR", y="TPR")
  
#write_rds(x = predict_lof_roc, "data/predict_lof_roc.rds")
#write_rds(x = predict_onesvm_roc, "data/predict_onesvm_roc.rds")
print(lof_auc)
print(onesvm_auc)
