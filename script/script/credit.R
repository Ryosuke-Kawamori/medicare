library(tidyverse)
library(e1071)
library(magrittr)
library(reshape2)
library(dbscan)

credit_row <- read_csv("../../medicare/data/credit/creditcard.csv")

N <- nrow(credit_row)
MIN_PTS <- 20
nu_range <- seq(0.001, 0.98, length=20)
#N <- 2000
r <- 0.75

credit <- credit_row %>% 
  sample_n(N)
train_ind <- sample(seq_len(N), size = N*r)

x <- credit %>% 
  select(-Time, -Class) %>% 
  mutate_if(is.double, scale_this) %>% 
  as.matrix()
x_train <- x[train_ind, ]
x_test <- x[-train_ind,]

y <- credit %>% 
  select(Class) %>% 
  as.matrix() %>% 
  .[,1]
y_train <- y[train_ind]
y_test <- y[-train_ind]




# labeled SVM evaluation --------------------------------------------------


# weight <- as.integer(round(table(y_train)[1]/table(y_train)[2])[1])
# 
# credit_svm_sup <- svm(x_train, y=y_train, type="C-classification", kernel="radial", class.weights = c('0'=1, '1'=500))
# #credit_svm_sup <- svm(x_train, y=y_train, type="C-classification", kernel="radial")
# result_sup <- bind_cols(tibble(Class = y_test), predict(credit_svm_sup, newdata=x_test) %>% tibble(pred=.))
# conf_mat_sup <- result_sup %>% 
#   count(Class, pred) %>% 
#   complete(Class, pred, fill = list(n=0)) %>% 
#   dcast(Class~pred) %>% 
#   .[,2:3] %>% 
#   as.matrix() 
#   apply(1,function(x){x/sum(x)}) %>% 
#     t()
# 
# sup_df <- tibble(type = "Supervised SVM", 
#   TP = conf_mat_sup[2,2], FP = conf_mat_sup[1,2], 
#   TN = conf_mat_sup[1,1], FN = conf_mat_sup[2,1])



# One SVM evaluation ------------------------------------------------------

x_test <- x

predict_onesvm_df <- tibble(nu = nu_range, x=rep(list(x), length(nu_range)), class=rep(list(tibble(class=y)), length(nu_range))) %>% 
  dplyr::mutate(predict = map2(x, nu, function(data_x, hyper_nu){
    print(hyper_nu) 
    one_svm(mat=data_x, nu=hyper_nu)}))

predict_onesvm_df <- predict_onesvm_df %>% 
  mutate(df = purrr::map2(class, predict, function(x,y){bind_cols(list(x,y))})) %>% 
  mutate(conf_df = purrr::map(df, . %>% count(class,predict))) %>% 
  mutate(fpr = purrr::map(conf_df, function(x){fpr(conf_mat(x))})) %>% 
  mutate(tpr = purrr::map(conf_df, function(x){tpr(conf_mat(x))})) %>% 
  select(nu, fpr, tpr) %>% 
  unnest()

# roc_unsup_df <- roc %>% 
#   t() %>% 
#   as.tibble() %>% 
#   mutate(type="Unsupervised SVM") %>% 
#   set_colnames(c("nu", "TP", "FP", "TN", "FN")) %>% 
#   mutate(type="Supervised SVM") 

# ggplot(data=roc_unsup_df, aes(FP,TP))+
#   geom_line()+
#   geom_point()+
#   geom_text(aes(label=round(nu,4)), vjust=0)+
#   geom_point(data=sup_df, aes(x=FP, y=TP), color="red")+
#   geom_text(data=sup_df, aes(x=FP, y=TP, label=type), vjust=0, nudge_x = 0.012, size=5)+
#   theme(text = element_text(size=20), axis.text = element_text(size=15))



# LOF detection -----------------------------------------------------------


predict_lof_df <- lof(x, k=MIN_PTS)

bind_cols(list(credit, tibble(lof=lof_df))) %>% 
  dplyr::arrange(desc(lof)) %>% 
  mutate(class=Class) %>% 
  dplyr::mutate(not_class = if_else(class == 0, 1, 0)) %>% 
  dplyr::mutate(tp = cumsum(class)/sum(class)) %>% 
  dplyr::mutate(fp = cumsum(not_class)/sum(not_class)) %>% 
  mutate(s = auc(fpr=fp, tpr=tp)) %>% 
  ggplot(aes(fp,tp))+
  geom_line()+
  labs(x="fpr", y="tpr")+
  theme(text = element_text(size=20))
