##################################
#MA321 Group Assignment
#Multinomial Logistic Regression
#Jorge Eduardo Maya Bautista
#Github: maybje
##################################
rm(list = ls()) #clearing working space
#loading libraries
library(dplyr)
library(ggplot2)
library(kernlab)
library(GGally)
library(nnet)
library(caret)
library(MASS)
library(doParallel)
library(glmnet)
library(tidyr)
library(kableExtra)

#setting wd
setwd("your wd")
wd=getwd()
data=read.csv(paste0(wd,"/data_log_imputed.csv")) #loading data

#Selecting relevant variables
data=dplyr::select(data,-c("Id","OverallCond","Overall_Groups","MiscVal"))

#Recoding variables to decrease number of regressors in logistic model
data["overallqual"]=1
data[(data$OverallQual>4 & data$OverallQual<8),"overallqual"]=2
data[(data$OverallQual>=8),"overallqual"]=3
data$OverallQual=NULL

data[data$TotRmsAbvGrd>12,"TotRmsAbvGrd"]=12

data[data$Fireplaces>0,"Fireplaces"]=1

data[data$PoolArea>0,"PoolArea"]=1

data["SaleC"]=0
data[data$SaleCondition!="Normal","SaleC"]=1
data$SaleCondition=NULL

#defining X matrix and Y vector of independent and dependent variables, respectively
y=data[,"condition"]
x=dplyr::select(data,-c("condition","SalePrice"))

#Get data class
data_class=lapply(x, class)

#define numerical and categorical variables
numerical=data_class!="factor"
categorical=data_class=="factor"

kable(table(data$condition), format="latex", digits=2, booktabs=TRUE) #latex table

#A) 10-fold CV
###Without upsampling: 
####mean accuracy:0.817; mean recall: 0.541; mean precision: 0.633; mean F1: 0.61; mean kappa: 0.401
set.seed(21)
cv=trainControl("cv", index=createFolds(y, 10,returnTrain = T),allowParallel=TRUE,
                summaryFunction = multiClassSummary)  #setting up k-fold CV

#Cluster of cores for parallel computing
cl <- makeCluster(detectCores()-2);
registerDoParallel(cl) 
start_time = Sys.time() #execution time
#train multinomial logistic regression
logit_cv=train(x,y,method="multinom",trControl = cv,metric="Kappa") 
end_time = Sys.time()
on.exit(stopCluster(cl))
print(end_time-start_time)

y_hat_cv=predict(logit_cv,x)
confusionMatrix(as.factor(y_hat_cv),y)

#saving results
write.csv(logit_cv$results, "logit_cv.csv", row.names = F)
write.csv(cbind(y_hat_cv,y), "y_hat_cv.csv", row.names = F)


###With upsampling: Classes are highly imbalance
####mean accuracy: 0.71; mean recall: 0.646; mean precision: 0.504; mean F1: 0.533; mean kappa: 0.379
set.seed(21)
cv_up=trainControl("cv", index=createFolds(y, 10,returnTrain = T),allowParallel=TRUE,
                summaryFunction = multiClassSummary, sampling="up")  #setting up k-fold CV

#Cluster of cores for parallel computing
cl <- makeCluster(detectCores()-2);
registerDoParallel(cl) 
start_time = Sys.time() #execution time
#train multinomial logistic regression
logit_cv_up=train(x,y,method="multinom",trControl = cv_up,metric="Kappa")
end_time = Sys.time()
on.exit(stopCluster(cl))
print(end_time-start_time)

y_hat_cvup=predict(logit_cv_up,x)
confusionMatrix(as.factor(y_hat_cvup),y)

#saving results
write.csv(logit_cv_up$results, "logit_cv_up.csv", row.names = F)
write.csv(cbind(y_hat_cvup,y), "y_hat_cv_up.csv", row.names = F)

#B) Validation Set
set.seed(21)
validation=createDataPartition(y,p=0.8,list=F)  #splitting data into train and test set (80-20)

#defining sets
y_train=y[validation]
x_train=x[validation,]

y_test=y[-validation]
x_test=x[-validation,]
###Without upsampling
#### accuracy: 0.822; recall: 0.559; precision: 0.717; F1:0.628; Kappa: 0.418
start_time = Sys.time()
#training multinomial logistic regression
logit_val=train(x_train,y_train,method="multinom",metric="Kappa")
end_time = Sys.time()
print(end_time-start_time)

#predicting  category
y_hat_val=predict(logit_val,x_test)
confusionMatrix(y_hat_val,y_test) #confusion matrix

#saving results
write.csv(logit_val$results, "logit_val.csv", row.names = F)
write.csv(cbind(y_hat_val,y_test), "y_hat_val.csv", row.names = F)

#With upsampling
####accuracy: 0.693; recall: 0.565; precision: 0.476; F1:0.517; Kappa: 0.693
#Upsampling categories
train_up=upSample(x_train,y_train,list=F, yname="y_train")
y_train_up=train_up[,"y_train"] #Defining target vector
x_train_up=dplyr::select(train_up,-"y_train") #defining X matrix

start_time = Sys.time()#execution time
#train multinomial logistic regression
logit_val_up=train(x_train_up,y_train_up,method="multinom",metric="Kappa")
end_time = Sys.time()
print(end_time-start_time)

#predicting  category
y_hat_valup=predict(logit_val_up,x_test)
confusionMatrix(y_hat_valup,y_test) #Confusion Matrix

#saving results
write.csv(logit_val_up$results, "logit_val_up.csv", row.names = F)
write.csv(cbind(y_hat_valup,y_test), "y_hat_val_up.csv", row.names = F)
#####################################################################################
#PCA with 10-fold CV and upSampling
####mean accuracy: 0.673; mean recall: 0.661; mean: precision: 0.489; F1: 0.501; Kappa:0.331
x_num=x   #Defining auxiliar X matrix
#converting factors to numerical ordinal variables
for (c in names(data[categorical])){
  x_num[c]=as.numeric(x[[c]])
}

#PCA on all variables
pca=princomp(x_num, cor=T)
#PCA barplot
ggplot()+geom_col(aes(x=factor(names(pca$sdev),levels=names(pca$sdev)),y=pca$sdev^2/sum(pca$sdev^2)),
                  fill="blue")+
  theme_bw() + labs(x = "Component", y="Proportion of Variance")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))
ggsave("report/pca.pdf")

set.seed(21)
cv=trainControl("cv", index=createFolds(y, 10,returnTrain = T),allowParallel=TRUE,
                summaryFunction = multiClassSummary,sampling="up") #setting up k-fold CV

start_time = Sys.time() #execution time
#train multinomial logistic regression
logit_pca=train(pca$scores,y,method="multinom", tuneGrid =expand.grid(
  decay = c(1)),trControl = cv,metric="Kappa")
end_time = Sys.time()
print(end_time-start_time)


y_hat_pca=predict(logit_pca,pca$scores)
confusionMatrix(y_hat_pca,y)

#Exporting results table to Latex format
star=multinom(y~pca$scores, decay=c(1), Hess=T)
stargazer(star, align=T,no.space=T)

#saving results
write.csv(logit_pca$results, "logit_pca.csv", row.names = F)
write.csv(cbind(y_hat_pca,y), "y_hat_pca.csv", row.names = F)

#estimating probabilities
probs=fitted(logit_pca,pca$scores[,c(1,2,3,4)], type="prob")
#upsampling probabilities to match the number of probabilities to the number of probabilities 
mydata <- upSample(pca$scores[,c(1,2,3,4)], y, list = FALSE, yname = "condition")
mydata=dplyr::select(mydata,-"condition") #Getting rid of Y column
predictors <- colnames(mydata)  #getting column names

#merging PCA scores with log odds
mydata <- mydata %>%
  mutate(logit = log(probs[,2]/(1-probs[,2]))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#Plot of factors vs log odds
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

ggsave("report/log_odd2.pdf")

###########Validation set 80-20
#defining train and test set
y_train=y[validation]
x_train_pca=x_num[validation,]

y_test=y[-validation]
x_test_pca=x_num[-validation,]

#upsampling minority classesin train 
train_up=upSample(x_train_pca,y_train,list=F, yname="y_train")
y_train_up=train_up[,"y_train"]
x_train_pca_up=dplyr::select(train_up,-"y_train")

#PCA on train set
pca_val=princomp(x_train_pca_up, cor=T)

start_time = Sys.time()#execution time
#train multinomial logistic regression
logit_val_pca=train(pca_val$scores,y_train_up,method="multinom",metric="Kappa")
end_time = Sys.time()
print(end_time-start_time)

#PCA on test set
pca_test_val=princomp(x_test_pca, cor=T)

#predicting test set
y_hat_valpca=predict(logit_val_pca,pca_test_val$scores)
confusionMatrix(y_hat_valpca,y_test)  #confusion matrix
