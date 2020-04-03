##################################
#MA321 Group Assignment
##################################
rm(list = ls())
library(pastecs)
library(kableExtra)
library(reshape2)
library(GGally)
library(ggpubr)
library(randomForest)
library(mice)
library(dplyr)
library(ggplot2)
library(reshape2)

#setting working directory
setwd("your wd")
#importing data
data=read.csv("your wd/house_data.csv")

#Get data class
data_class=lapply(data, class)
#define numerical and categorical variables
numerical=data_class=="integer"
categorical=data_class!="integer"

names=colnames(data) #getting column names

#Taking logs of skewed variables
skewed=c("LotFrontage","LotArea","MasVnrArea","TotalBsmtSF","X1stFlrSF","LowQualFinSF",
         "GrLivArea", "GarageArea","PoolArea","MiscVal","SalePrice")

for (s in skewed){
    data[,s]=log(data[,s]+1)
}

#Visualizing missing data (ordered by Overall Condition)
data2=data[order(data$OverallCond),-1]
data2=data2 %>% is.na %>% melt
data2["Var1"]=rep(seq(1,2919,1), times = 50)
ggplot(data2, aes(Var2, Var1, fill=value)) + 
  geom_raster() + 
  scale_fill_manual(values = c("black", "gray"),name="", labels=c("Present", "Missing")) +
  theme_minimal() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  labs(x = "Variables in Dataset",
       y = "Observations \n (ordered by 'Overall Condition')")+
  scale_y_continuous(limits = c(0,3000), expand = c(0, 0))

ggsave("report/missing_matrix.pdf")

#getting columns with missing values
missing=colSums(is.na(data))/dim(data)[1]

#Deleting missin columns with missingess greater than 50%
data=data[,!missing>.5]

names=colnames(data)  #getting column names

#define numerical and categorical variables
data_class=lapply(data, class)

numerical=data_class=="integer" | data_class=="numeric" 
categorical=data_class!="integer" & data_class!="numeric" 

#Multicolinearity
#Estimating correlation matrix
corr <- round(cor(na.omit(data[,numerical][,-1])), 2)
corr=melt(corr) #reshaping matrix
numerical2=numerical  #defining subset of numerical variables
numerical2[c("OverallCond", "SalePrice")]=FALSE
collinear=round(cor(na.omit(data[,numerical2][,-1])), 2)  #correlation matrix to detect collinearity
collinear=melt(replace(collinear, lower.tri(collinear, T), NA), na.rm = TRUE) #reshape matrix

not_keep=c()  #defining empty list por collinear variables
#loop to compare correlation of each pair of numerical variables and keep the one with the highest
#correlation coefficient with Sale Price
for (r in 1:length(collinear[,3])){
  if (abs(collinear[r,3]) >0.5){
    print(paste0(as.character(collinear[r,1])," - ",as.character(collinear[r,2])))
    if (sum(c(as.character(collinear[r,1]),as.character(collinear[r,2])) %in% not_keep)) next
    if (abs(corr[corr$Var1==as.character(collinear[r,1])&corr$Var2=="SalePrice",3])>
        abs(corr[corr$Var1==as.character(collinear[r,2])&corr$Var2=="SalePrice",3])){
      not_keep=c(not_keep,as.character(collinear[r,2]))
    }
    else{
      not_keep=c(not_keep,as.character(collinear[r,1]))
    }
  }
} 

#deleting collinear variables
data=dplyr::select(data,-not_keep)

#everytime a column is deleted we need to redefine these auxiliar variables
names=colnames(data)  #getting column names

#define numerical and categorical variables
data_class=lapply(data, class)

numerical=data_class=="integer" | data_class=="numeric" 
categorical=data_class!="integer" & data_class!="numeric" 

#defining overall condition groups
data["Overall_Groups"]=1
data[(data$OverallCond>3 & data$OverallCond<7),"Overall_Groups"]=2
data[(data$OverallCond>=7),"Overall_Groups"]=3
data$condition=factor(data$Overall_Groups, labels=c("Poor","Average","Good"))


##categorical collinearity
#we test each possible pair of categorical variable to see if they are independent or not.
#In case they are not independent, we keep the one with the strongest association with 
#"condition" measured by the magnitude of the p-value
coll_categorical=c()
for (c in names[categorical]){
  for (d in names[categorical]){
    if(is.nan(chisq.test(table(data[,c], data[,d]), simulate.p.value = TRUE)$p.value)) next
    if (chisq.test(table(data[,c], data[,d]), simulate.p.value = TRUE)$p.value<0.05 &
        c!=d){
      if (sum(c(c,d) %in% coll_categorical)) next
      if (chisq.test(table(data[,c], data[,"condition"]), simulate.p.value = TRUE)$p.value==
          chisq.test(table(data[,d], data[,"condition"]), simulate.p.value = TRUE)$p.value) next
      if (chisq.test(table(data[,c], data[,"condition"]), simulate.p.value = TRUE)$p.value<
          chisq.test(table(data[,d], data[,"condition"]), simulate.p.value = TRUE)$p.value){
        print(paste0( c,",",d))
        coll_categorical=c(coll_categorical,d)
      }
      else{
        print(paste0(c,",",d))
        coll_categorical=c(coll_categorical,c)
      }
    }
  }
}
#we delete collinear categorical variables
data=dplyr::select(data,-coll_categorical)

###Exploring if abscene of data is related with overall condition
names=colnames(data)#getting column names

#define numerical and categorical variables
data_class=lapply(data, class)

numerical=data_class=="integer" | data_class=="numeric" 
categorical=data_class!="integer" & data_class!="numeric" 

#getting columns with missing values
missing=colSums(is.na(data))/dim(data)[1]

proportions=prop.table(table(data[,"Overall_Groups"]))  #original proportions between classess

#one-way goodness of fit test to see if the absence of data is related with the condition
for (c in names[missing>0]){
    print(c)
    print(chisq.test(table(data[!is.na(data[,c]),"Overall_Groups"]), p=proportions, simulate.p.value = TRUE))
}

#plotting original and complete case density functions to see the abscence of data is related 
#with Sale Price or they are essentially the same
dens_price=ggplot()+stat_density(data, mapping =aes(x=(SalePrice),color="black"),geom="line")+
  stat_density(data=na.omit(data), mapping=aes(x=(SalePrice),color="red"),geom="line")+theme_bw()+
  theme(legend.position = c(0.9, 0.9),legend.background=element_blank(),legend.key=element_blank(),
        legend.text=element_text(size=11))+
  scale_colour_manual(name = '', 
                      values =c('black'='black','red'='red'), labels = c('Original','Complete'))+
  scale_x_continuous(breaks = seq(10.5,13.5,0.5))

dens_price
ggsave("report/missing_dens.pdf")

###imputation
#defining mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

##Mean imputation for numerical variables with missing rate smaller than 10% (can be median)
##and mode imputation for categorical variables.
for (m in names[missing>0 & missing<0.1]){
  if (m %in% names[numerical]){
    data[is.na(data[,m]),m]=mean(data[,m], na.rm=T)
  }
  else{
    data[is.na(data[,m]),m]=Mode(data[,m])
  }
}

#OUTLIERS
#ggplot(data, aes(x=log(LotArea)))+stat_density(geom="line")

#quantile(data$LotArea, c(.90,0.95,.99))
#data$LotArea[data$LotArea>quantile(data$LotArea, c(.99))]=quantile(data$LotArea, c(.99))
#ggplot(data, aes(x=log(LotArea)))+stat_density(geom="line")

#front_den=ggplot(data, aes(x=LotFrontage, color="black"))+stat_density(geom="line")
#summary(data$LotFrontage)
#quantile(na.omit(data$LotFrontage), c(.90,0.95,.99))
#data$LotFrontage[data$LotFrontage>quantile(na.omit(data$LotFrontage), c(.99))]=quantile(na.omit(data$LotFrontage), c(.99))

#Random Forest imputation for LoFrontage (missing rate of 16%
#A <- is.na(data[,names!="Id" & names!="SalePrice"])
#front=mice(data[,names!="Id" & names!="SalePrice"], where = A, method="rf")
#front=rowMeans(front$imp$LotFrontage)

#data$LotFrontage[A[,1]]=front
#front_den+stat_density(data, mapping=aes(x=(LotFrontage), color="red"),geom="line")+theme_bw()+
 # theme(legend.position = c(0.9, 0.9),legend.background=element_blank(),legend.key=element_blank(),
  #      legend.text=element_text(size=11))+
  #scale_colour_manual(name = '', 
   #                   values =c('black'='black','red'='red'), labels = c('Orginal','Imputed'))

#Descriptive statistics
names=colnames(data)#getting column names

#define numerical and categorical variables
data_class=lapply(data, class)

numerical=data_class=="integer" | data_class=="numeric" 
categorical=data_class!="integer" & data_class!="numeric" 

#Descriptive statistics table
options(scipen=100)
options(digits=3)
sum_stats=t(stat.desc(data[,numerical]))
colnames(sum_stats)[1]="obs"
colnames(sum_stats)[3]="NAs"

sum_stats=sum_stats[2:dim(sum_stats)[1],c("obs","NAs", "min","max", "median", "mean", "std.dev")]

kable(sum_stats, format="latex", digits=2, booktabs=TRUE) #latex fornat

#Boxplots
melted=melt(scale(data[numerical]),id.vars="Id")  #normalize the data

ggplot(data = melted[melted$Var2!="Id",], aes(x=Var2, y=value)) + geom_boxplot()+theme_bw()+
  theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=90))

ggsave("report/boxplot.pdf")

#plotting heatmap of correlation matrix
corr <- round(cor(na.omit(data[,numerical][,-c(1,18)])), 2)
corr=melt(corr)

ggplot(data = corr, aes(x=Var1,ordered(Var2, levels =     rev(sort(unique(Var2)))), fill=value)) + theme_bw()+
  geom_tile()+ theme(axis.title.x = element_blank(),axis.text.y = element_text(vjust=0),
                     axis.title.y = element_blank(),axis.text.x=element_blank(), legend.title = element_blank()) +
  scale_fill_gradient2(low="red", mid="blue", high="red", 
                       midpoint=0,labels=c("-1.0", "-0.5","0","0.5","1.0"),breaks=c(-1,-.5,0,.5,1),limits=c(-1,1))

ggsave("report/corr_matrix.pdf")

#barplots of categorical data
#Creating a list of first block of categorical variables barplots to plot them together in an arrange
plist = sapply(names[categorical][1:9], function(col) {
  ggplot()+geom_bar(data=data[,categorical], aes_string(col))+ theme_bw()+
    theme(axis.title.x = element_text(size=8),axis.text.y = element_text(vjust=0, size=7)
          ,axis.text.x = element_text(angle = 90, hjust = 1, size=7),axis.title.y = element_text(size=8))
}, simplify=FALSE)

ggarrange(plotlist=plist) #create arrange

ggsave("report/barplots1.pdf")

#Creating a list of second block of categorical variables barplots to plot them together in an arrange
plist = sapply(names[categorical][10:17], function(col) {
  ggplot()+geom_bar(data=data[,categorical], aes_string(col))+ theme_bw()+
    theme(axis.title.x = element_text(size=8),axis.text.y = element_text(vjust=0, size=7)
          ,axis.text.x = element_text(angle = 90, hjust = 1, size=7),axis.title.y = element_text(size=8))
}, simplify=FALSE)

ggarrange(plotlist=plist) #create arrange

ggsave("report/barplots2.pdf")

write.csv(data, "data_log_imputed.csv", row.names = F)  #save cleaned dataset
