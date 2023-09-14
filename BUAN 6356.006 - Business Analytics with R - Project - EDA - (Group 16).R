# Exploratory Data Analysis
library(ggplot2)
library(plotly)
library(ggthemes)

heart.df<-read.csv("heart.csv")

# Data Pre-processing - Null value validation
is.na(heart.df) 
sum(!complete.cases(heart.df))

# Data visualization
head(heart.df)

# Summary Statistics
summary(heart.df)

# Finding Outlines using Box Plot
ggplot(data = heart.df, aes(y=trtbps)) +
  geom_boxplot(fill=c('Yellow'))

ggplot(data = heart.df, aes(y=thalachh)) +
  geom_boxplot(fill=c('Yellow'))

ggplot(data = heart.df, aes(y=oldpeak)) +
  geom_boxplot(fill=c('Yellow'))

ggplot(data = heart.df, aes(y=chol)) +
  geom_boxplot(fill=c('Yellow'))

# Histogram
age <- subset (heart.df, select = -c(2,3,4,5,6,7,8,9,10,11,12,13,14))
Trtbps <- subset (heart.df, select = -c(1,2,3,5,6,7,8,9,10,11,12,13,14))
Trtbps1 <- as.numeric(unlist(Trtbps))

sex <- as.factor(heart.df$sex)

ggplot(heart.df,aes(x=age)) + geom_bar(aes(fill=factor(sex)))+theme_gdocs()

ggplot(heart.df,aes(x=Trtbps1)) + geom_bar(aes(fill=factor(sex)))+theme_gdocs()


# Scatter Plot
ggplot(scatter_plot,          
       aes(x = age,
           y = chol,
           col = as.factor(sex))) +
  geom_point()

ggplot(heart.df,                       
       aes(x = age,
           y = chol,shape = as.factor(output),
           col = as.factor(sex))) +
  geom_point()
