# Logistic Regression
library(caret)

heart.df<-read.csv("heart.csv")
heart.df$output <- as.factor(heart.df$output)
str(heart.df)
summary(heart.df)

# Partitioning the Data set into Training Data set and Validation Data set
set.seed(1)  
train.index <- sample(c(1:dim(heart.df)[1]), dim(heart.df)[1]*0.8)  
train.df <- heart.df[train.index,]
valid.df <- heart.df[-train.index,]

# Logistic Regression
logit.reg <- glm(output ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)
coef(logit.reg)   
RegOut<-c(coef(logit.reg));  RegOut

# Confusion Matrix for Training and Validation Data set
logit.reg.pred.train <- predict(logit.reg, train.df, type = "response")
data.frame(actual = train.df$output[1:5], predicted = logit.reg.pred.train[1:5])
logit.reg.pred.train.classes <- ifelse(logit.reg.pred.train > 0.5, 1, 0)
confusionMatrix(as.factor(logit.reg.pred.train.classes), as.factor(train.df$output))

logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
data.frame(actual = valid.df$output[1:5], predicted = logit.reg.pred[1:5])
logit.reg.pred.classes <- ifelse(logit.reg.pred > 0.5, 1, 0)
confusionMatrix(as.factor(logit.reg.pred.classes), as.factor(valid.df$output))

# Model Selection
full.logit.reg <- glm(output ~ ., data = train.df, family = "binomial") 
empty.logit.reg  <- glm(output ~ 1,data = train.df, family= "binomial")
summary(empty.logit.reg)

backwards = step(full.logit.reg)
summary(backwards)

backwards.reg.pred <- predict(backwards, valid.df, type = "response")
backwards.reg.pred.classes <- ifelse(backwards.reg.pred > 0.5, 1, 0)
confusionMatrix(as.factor(backwards.reg.pred.classes), as.factor(valid.df$output))


