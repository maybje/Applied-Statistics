##################################
#MA321 Group Assignment
#Clustering Analysis
#Jorge Eduardo Maya Bautista
#Github: maybje
##################################
rm(list = ls()) #Clearing working space
#Loading categories
library(data.table)
library(glmnet)
library(caret)
library(MASS)
library(scales)
library(dbscan)

#defining working directory
setwd("your wd")
wd=getwd()
data=read.csv(paste0(wd,"/pca_set.txt"))  #loading data

#defining label vector and X matrix
y = fread(paste0(wd,"/data_log_imputed.csv"),select=c(35))
y=as.numeric(as.factor(y$condition))
x=data[1:11]

#Plotting first 2 principal components by original categories
ggplot()+geom_point(aes(x[,1],x[,2], color=factor(y)))+theme_bw()+scale_colour_manual(values=c("#03c900","#eb4034","blue"), 
                    name="",labels=c("Average", "Good","Poor"))+theme(legend.position = c(0.9, 0.9),legend.background=element_blank(),legend.key=element_blank(),
        legend.text=element_text(size=11))+ylab("Second Component")+xlab("First Component")

ggsave("report/overall.pdf")

#K-means with original number of clusters
k=kmeans(x,  3, nstart=20,iter.max = 15)

set.seed(21)
cv_up=trainControl("cv", index=createFolds(as.factor(db$cluster), 10,returnTrain = T),allowParallel=TRUE,
      summaryFunction = multiClassSummary, sampling="up")  #setting up k-fold CV


start_time = Sys.time()#execution time
#train multinomial logistic regression
logit_db=train(x,as.factor(k$cluster),method="multinom",
          trControl = cv_up,metric="Kappa", tuneGrid =expand.grid(decay = c(1)))
end_time = Sys.time()
print(end_time-start_time)

y_hat_cvup=predict(logit_cv_up,x)
confusionMatrix(as.factor(y_hat_cvup),as.factor(db$cluster))

#Plotting first two components by resulting clusters
ggplot()+geom_point(aes(x[,1],x[,2], color=factor(k$cluster)))+theme_bw()+
  theme(legend.position = "none")+ylab("Second Component")+xlab("First Component")

ggsave("report/k3.pdf")

#Elbow Method
kmax=10 #number of clusters to be testes
#Total within-cluster sum of distances
wss <- sapply(1:kmax, 
              function(k){kmeans(x, k, nstart=20,iter.max = 15 )$tot.withinss})

#scaling distance
wss=(wss-min(wss))/(max(wss)-min(wss))
delta1=wss[1:(length(wss)-1)]-wss[2:length(wss)]  #first difference
delta2=delta1[1:(length(delta1)-1)]-delta1[2:length(delta1)]  #second difference
#strength of difference
strength=rep(0,length(delta2))  
strength=delta2[1:length(delta2)]-delta1[2:length(delta2)]
strength[strength<0]=0


#plot of total within-cluster sum of distances and strength
ggplot()+geom_col(aes(2:(length(strength)+1),strength*10, fill="#0bd6ae"))+
  geom_point(aes(1:kmax,wss))+geom_line(aes(1:kmax,wss),linetype="solid")+theme_bw()+
  scale_x_continuous(limits = c(1, kmax), breaks=1:kmax)+ylab("Total within-cluster sum of squares")+
  xlab("Number of Clusters")+scale_y_continuous()+
  theme(legend.position = c(0.85, 0.9),legend.background=element_blank(),legend.key=element_blank(),
        legend.text=element_text(size=11))+
  scale_fill_manual(values=c("#0bd6ae"), 
                    name="",labels=c("Strength"))

ggsave("report/elbow.pdf")

#getting optimal number of clusters
smax=which.max(strength)+1

#estimating kmeans with optimal number of clusters
my.km <- kmeans(x,centers=smax,nstart=10)


#plot of first 2 components clustered 
ggplot()+geom_point(aes(x[,1],x[,2]), color=my.km$cluster)+theme_bw()+
  ylab("Second Component")+xlab("First Component")

ggsave("report/kmeans.pdf")

set.seed(21)
cv_up=trainControl("cv", index=createFolds(as.factor(my.km$cluster), 10,returnTrain = T),allowParallel=TRUE,
                   summaryFunction = multiClassSummary, sampling="up")

 
start_time = Sys.time()
logit_k=train(x,as.factor(my.km$cluster),method="multinom",trControl = cv_up,metric="Kappa", tuneGrid =expand.grid(
  decay = c(.5)))
end_time = Sys.time()
print(end_time-start_time)

y_hat_k=predict(logit_k,x)
confusionMatrix(as.factor(y_hat_k),as.factor(my.km$cluster))
