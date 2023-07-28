# getting working directory
getwd()

# reading data
WQ <- read.csv("Water_Quality.csv")

# viewing data
View(WQ)

# data structure
str(WQ)

#removing missing value by Random Forests Approach
library(mice) # for data imputation( Missing data Imputation using Chain Equations)
WQ_imp <- mice(WQ, m=5,method="rf",maxit=5)

WQ_imp$where # to see the places where impuattion has taken place. TRUE : imputed


# getting complete data
WQ_complete <- complete(WQ_imp)


# corolation between all variables :
cor(WQ_complete[,])


#####################################################################################################
#####################################################################################################

#spliting data into train and test 
library(caret) # library to implement many different statistical models. 

# split data to train and test

index <- createDataPartition(WQ_complete$Potability,p=0.80,list=FALSE)

WQ_train <- WQ_complete[index,] 
dim(WQ_train) # to ensure proper split
str(WQ_train)
head(WQ_train)
WQ_test <- WQ_complete[-index,] 
dim(WQ_test) # to ensure proper split
str(WQ_test)
head(WQ_test)


# fitting the logistic regression when considering all the predictors
MODEL1 <- glm(Potability~., family = binomial(link = "logit"), data = WQ_train)

# MODEL1 summary 
summary(MODEL1)

# analysis variance MODEL1
anova(MODEL1, test = "Chisq")


# fitting of logistic regression when considering only the statistically significant predictors
MODEL2 <- glm(Potability~.-Solids-Chloramines-Organic_carbon, family = binomial(link = "logit"),data = WQ_train)

# MODEL2 summary 
summary(MODEL2)

# analysis variance MODEL2
anova(MODEL2, test = "Chisq")

# prediction model1
prdct1 <- predict(MODEL1,newdata = WQ_test,type = "response")
# prediction model2
prdct2 <- predict(MODEL2,newdata = WQ_test,type = "response")

# Accuracy cheacking
library(ggplot2)
library(lattice)
library(caret)


prdct1 <- ifelse(prdct1 > 0.5,1,0)
err1 <- mean(prdct1 != WQ_test$Potability)
print(paste('Accuracy',1-err1))


prdct2 <- ifelse(prdct2 > 0.5,1,0)
err2 <- mean(prdct2 != WQ_test$Potability)
print(paste('Accuracy',1-err2))

