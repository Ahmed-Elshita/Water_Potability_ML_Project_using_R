# set seed fo random number generator to keep results reproducible
set.seed(123455)

# Anomaly Detection in Water Quality

#setwd("C:\# R") # set working directory

df <- read.csv("Water_Quality.csv") # read the input data

# Potability : water is safe for drinking

head(df)

df$Potability <- factor(df$Potability) # to covert numeric variable to factor variable for  further analysis

############################################################
#                                                          #
#                      describe data                       #
#                                                          #
############################################################

library(psych) # for function describe
describe(df)

describeBy(df,group=df$Potability) # describe the data sperately for the two levels of potability

# explore data using plot panel
library(GGally) # To explore  the data visually , as scatter plots, histograms etc and also to see correlation
ggpairs(df) # plots show no autocorrelation, can be sued for modeling

ggpairs(df, aes(color=Potability)) # the plots will show potable and notpotable observations as separate colors


############################################################
#                                                          #
#                      data cleaning                       #
#                                                          #
############################################################

# Visualize missing data
library(VIM)
aggr(df) # visualize missing data form the data frame

# In sulphate , more than 20% data is missing. We can not omit the observations with the missing values.
# we will use 'rf- random forest model' to impute* fill in) the missing data 


# imputation with mice package

library(mice) # library for mice function for data imputation( Missing data Imputation using Chain Equations)
df_imp <- mice(df, m=5,method="rf",maxit=5)

df_imp$where # to see the places where impuattion has taken place. TRUE : imputed

# getting complete data
df_complete <- complete(df_imp)

# confirm imputation

aggr(df_complete)

# confirm imputation quality
nrow(df)

# join original and imputed data 

df_both <- rbind(df,df_complete)
nrow(df_both)

# add column imputed: Ori/imp to distinguish original and imputed data and 
df_both$imputed <- rep(c("ori","imp"),each=nrow(df))
tail(df_both$imputed )

#plot boxplots to compare original and imputed data for columns with missing data
par(mfrow=c(1,3)) # divide canvas into  different sections

boxplot(ph~imputed,df_both, main="pH",outcol="red")
boxplot(ph~imputed,df_both, main="Sulfate",outcol="red")
boxplot(ph~imputed,df_both, main="Turbidity",outcol="red")


# There is no difference in original and imputed data. We cab use imputed data set as original dataset for further analysis without any loss of reliability of results.

# fitting model to classify observations as potable or not.

library(caret) # library to implement many different statistical models. 

# split data to train and test

index <- createDataPartition(df_complete$Potability,p=0.75,list=FALSE)

df_train <- df_complete[index,] 
dim(df_train) # to ensure proper split
str(df_train)

df_test <- df_complete[-index,] 
dim(df_test) # to ensure proper split

############################################################
#                                                          #
#                      fit the model                       #
#                                                          #
############################################################


# prepare for cross validation

tc <- trainControl(method="repeatedcv",number=10,repeats=5)

############################################################
#                                                          #
#                   logistic regression                    #
#                                                          #
############################################################

# Here we will use hardness, sulfate , solids, conductivity

mod_reglog <- train(Potability~., df_train[,c(2,3,5,6,10) ], trControl =tc,method="regLogistic")

# test model with test data

pred_pot <- predict(mod_reglog ,df_test)

confusionMatrix(df_test$Potability,pred_pot)

plot(mod_reglog)
plot(varImp(mod_reglog)) #features can be selected based on variable importance.

varImp(mod_reglog) # to get variable importance numeric values. Variable hardness has importance 100  and other variables importance are as per the output. 

# Accuracy of classification by logistic regression after feature selection  is 0.6112.