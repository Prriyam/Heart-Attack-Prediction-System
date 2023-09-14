# Model Evaluation - ROC Curve
library(pROC)

heart.df<-read.csv("heart.csv")
heart.df$output <- as.factor(heart.df$output)
str(heart.df)
heart.df

# Partitioning the Data set into Training Data set and Validation Data set
set.seed(1)  
train.index <- sample(c(1:dim(heart.df)[1]), dim(heart.df)[1]*0.8)  
train.df <- heart.df[train.index,]
valid.df <- heart.df[-train.index,]


# Building Prediction Model
logit.reg.pred.train <- predict(logit.reg, train.df, type = "response")
logit.reg.pred.train.classes <- ifelse(logit.reg.pred.train > 0.5, 1, 0)

logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
logit.reg.pred.classes <- ifelse(logit.reg.pred > 0.5, 1, 0)

# Ploting ROC Curve
r_train <- roc(train.df$output,logit.reg.pred.train)
r_valid <- roc(valid.df$output,logit.reg.pred.classes)

plot.roc(r_train)
plot.roc(r_valid)

auc(r_train)
auc(r_valid)
