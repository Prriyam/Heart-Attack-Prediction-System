# Decision Tree Analysis
library(rpart)
library(rpart.plot)
library(caret)

heart.df<-read.csv("heart.csv")
heart.df$output <- as.factor(heart.df$output)
str(heart.df)
heart.df

# Partitioning the Data set into Training Data set and Validation Data set
set.seed(1)  
train.index <- sample(c(1:dim(heart.df)[1]), dim(heart.df)[1]*0.8)  
train.df <- heart.df[train.index,]
valid.df <- heart.df[-train.index,]

# Decision Tree
decision_tree <- rpart(output ~ ., data = train.df ,method = "class")
summary(decision_tree)

# Plotting the decision Tree
prp(decision_tree, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
rpart.plot(decision_tree)

# Count of Leaf Node
length(decision_tree$frame$var[decision_tree$frame$var == "<leaf>"])

# Building Decision Tree with Information Gain Method
info_gain <- rpart(output ~ ., data = train.df, parms = list(split = 'information'), method = "class")
prp(info_gain, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
length(info_gain$frame$var[info_gain$frame$var == "<leaf>"])


# Confusion Matrix for Training and Validation Data set
decision_tree_cm_train <- predict(decision_tree,train.df,type = "class")
confusionMatrix(decision_tree_cm_train, as.factor(train.df$output))


decision_tree_cm_valid <- predict(decision_tree,valid.df,type = "class")
confusionMatrix(decision_tree_cm_valid, as.factor(valid.df$output))

# Pruning
set.seed(1)
pruning <- rpart(output ~ ., data = train.df, method = "class", cp = 0.00001, minsplit = 1, xval = 5) 
printcp(pruning) 
pruning.ct <- prune(pruning, cp = 0.018)

printcp(pruning.ct)
prp(pruning.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruning.ct$frame$var == "<leaf>", 'gray', 'white')) 


# Confusion Matrix after Pruning
pruning_cm_train<- predict(pruning.ct,train.df,type="class")
confusionMatrix(pruning_cm_train, as.factor(train.df$output))

pruning_cm_valid<- predict(pruning.ct,valid.df,type="class")
confusionMatrix(pruning_cm_valid, as.factor(valid.df$output))

length(pruning.ct$frame$var[pruning.ct$frame$var == "<leaf>"])




