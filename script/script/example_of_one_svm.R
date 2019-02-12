library(MASS)
library(dplyr)
library(ggplot2)

#theme of ggplot
g_theme <- theme_linedraw()+
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

#parameters of multinormal distribution
n <- 1000
mu_1 <- c(1,10)
sigma_1 <- matrix(c(2,1,2,1), nrow = 2)
mu_2 <- c(10,1)
sigma_2 <- matrix(c(1,0,0,1), nrow = 2)

#dataframe of generated samples
data_df <- data.frame(rbind(mvrnorm(n, mu_1, sigma_1), mvrnorm(n, mu_2, sigma_2))) %>% 
  mutate(class=1)
colnames(data_df) <- c("x1", "x2", "class")

svm_model <- svm(x=as.matrix(data_df[,1:2]),
                 labels=NULL,
                 type="one-classification",
                 kernel="radial",
#                 kpar=list(sigma=0.1),
                 nu=0.6,
                 cross=0.01
)

outlier <- as.factor(ifelse(predict(svm_model)==TRUE,2,1))
data_df$outlier <- outlier

#tuned <- tune.svm(x=as.matrix(data_df[,1:2]), y=rep(TRUE, nrow(data_df)), type='one-classification', kernel='radial', gamma=c(0.001,0.1,0.01), nu=c(0.1,0.01,0.001))
#tuned$best.parameters
# data_df %>% 
#   ggplot(aes(x1, x2, group=outlier, color=outlier))+
#   geom_point()+
#   labs(x="x", y="y")+
#   theme(text=element_text(size=20), legend.position = "none")
# 
# data_df %>% 
#   ggplot(aes(x1, x2, group=outlier))+
#   geom_point()+
#   labs(x="x", y="y")+
#   theme(text=element_text(size=20), legend.position = "none")

px <- seq(-15,15,0.05)
py <- seq(-15,15,0.05)
pgrid <- expand.grid(px,py)
pred <- predict(svm_model, newdata = pgrid)*1

bind_cols(tbl_df(pgrid), tbl_df(pred)) %>% 
  ggplot()+
  geom_contour(aes(x=Var1, y=Var2, z=value))+
  geom_point(data=data_df, aes(x=x1, y=x2, color=outlier))+
  labs(x="x1", y="x2")
