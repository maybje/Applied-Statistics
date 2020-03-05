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

setwd("D:/Documentos/Essex/Applied Statistics/assignment 2")
data=read.csv("D:/Documentos/Essex/Applied Statistics/assignment 2/house_data.csv")

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

#Summary statistics
options(scipen=100)
options(digits=3)
sum_stats=t(stat.desc(data[,numerical]))
colnames(sum_stats)[1]="obs"
sum_stats=cbind(sum_stats,as.vector(sum_stats[,2]+sum_stats[,3]))
colnames(sum_stats)[15]="NAs"

sum_stats=sum_stats[2:dim(sum_stats)[1],c("obs", "min","max", "median", "mean", "std.dev","NAs")]

kable(sum_stats, format="latex", digits=2, booktabs=TRUE)

#Boxplots
melted=melt(scale(data[numerical]),id.vars="Id")

ggplot(data = melted[melted$Var2!="Id",], aes(x=Var2, y=value)) + geom_boxplot()+theme_bw()+
  theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=90))


#Visualizing missing data (ordered by Overall Condition)
data2=data[order(data$OverallCond),-1]
data2=data2 %>% is.na %>% melt
data2["Var1"]=rep(seq(1,2919,1), times = 50)
ggplot(data2, aes(Var2, Var1, fill=value)) + 
  geom_raster() + 
  scale_fill_manual(values = c("black", "#22DADB"),name="", labels=c("Present", "Missing")) +
  theme_minimal() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  labs(x = "Variables in Dataset",
       y = "Observations \n (ordered by 'Overall Condition')")+
  scale_y_continuous(limits = c(0,3000), expand = c(0, 0))

missing=colSums(is.na(data))/dim(data)[1]

#########################################################
#Deleting missin columns with missingess greater than 50%
data=data[,!missing>.5]

names=colnames(data)  #getting column names

#define numerical and categorical variables
data_class=lapply(data, class)

numerical=data_class=="integer" | data_class=="numeric" 
categorical=data_class!="integer" & data_class!="numeric" 

#Multicolinearity
corr <- round(cor(na.omit(data[,numerical][,-1])), 2)
corr=melt(corr)
numerical2=numerical
numerical2[c("OverallCond", "SalePrice")]=FALSE
collinear=round(cor(na.omit(data[,numerical2][,-1])), 2)
collinear=melt(replace(collinear, lower.tri(collinear, T), NA), na.rm = TRUE)

not_keep=c()
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

data=dplyr::select(data,-not_keep)

#Plotting correlation matrix
names=colnames(data)  #getting column names

#define numerical and categorical variables
data_class=lapply(data, class)

numerical=data_class=="integer" | data_class=="numeric" 
categorical=data_class!="integer" & data_class!="numeric" 
corr <- round(cor(na.omit(data[,numerical][,-1])), 2)
corr=melt(corr)

ggplot(data = corr, aes(x=Var1,ordered(Var2, levels =     rev(sort(unique(Var2)))), fill=value)) + 
  geom_tile()+ theme(axis.title.x = element_blank(),axis.text.y = element_text(vjust=0),
                     axis.title.y = element_blank(),axis.text.x=element_blank(), legend.title = element_blank()) +
  scale_fill_gradient2(low="red", mid="blue", high="red", 
                       midpoint=0,labels=c("-1.0", "-0.5","0","0.5","1.0"),breaks=c(-1,-.5,0,.5,1),limits=c(-1,1))



#defining overall condition groups
data["Overall_Groups"]=1
data[(data$OverallCond>3 & data$OverallCond<7),"Overall_Groups"]=2
data[(data$OverallCond>=7),"Overall_Groups"]=3
data$condition=factor(data$Overall_Groups, labels=c("Poor","Average","Good"))


#Exploring correlation between categorical variables and overall condition
non_inf=c()
for (c in names[categorical]){
  if (chisq.test(table(data[,c], data[,"Overall_Groups"]), simulate.p.value = TRUE)$p.value<0.05){
    print(c)
    print(chisq.test(table(data[,c], data[,"Overall_Groups"]), simulate.p.value = TRUE))
  }
  else{
    non_inf=c(non_inf,c)
    #print(c)
    #print(table(data[,c], data[,"Overall_Groups"]))
  }
}

#dropping non informative categorical variables


data=dplyr::select(data,-non_inf)


###Exploring if missingness is related with overall condition
names=colnames(data)

data_class=lapply(data, class)

numerical=data_class=="integer" | data_class=="numeric" 
categorical=data_class!="integer" & data_class!="numeric" 

missing=colSums(is.na(data))/dim(data)[1]

proportions=prop.table(table(data[,"Overall_Groups"]))

for (c in names[missing>0]){
    print(c)
    print(chisq.test(table(data[!is.na(data[,c]),"Overall_Groups"]), p=proportions, simulate.p.value = TRUE))
}

dens_price=ggplot()+stat_density(data, mapping =aes(x=(SalePrice),color="black"),geom="line")+
  stat_density(data=na.omit(data), mapping=aes(x=(SalePrice),color="red"),geom="line")+theme_bw()+
  theme(legend.position = c(0.9, 0.9),legend.background=element_blank(),legend.key=element_blank(),
        legend.text=element_text(size=11))+
  scale_colour_manual(name = '', 
                      values =c('black'='black','red'='red'), labels = c('Original','Complete'))+
  scale_x_continuous(breaks = seq(10.5,13.5,0.5))
dens_price


###imputation
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

#barplots (can be modified)
ggarrange(ggplot()+geom_bar(data=data[,categorical], aes(Street)), 
          ggplot()+geom_bar(data=data[,categorical], aes(Utilities)),
          ggplot()+geom_bar(data=data[,categorical], aes(LotConfig)),
          ggplot()+geom_bar(data=data[,categorical], aes(Neighborhood)),
          ggplot()+geom_bar(data=data[,categorical], aes(Condition1)),
          ggplot()+geom_bar(data=data[,categorical], aes(SaleCondition)), ncol=3, nrow=2)


write.csv(data, "data_log_imputed.csv", row.names = F)
