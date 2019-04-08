library(dplyr)
library(tidyr)
library(ggplot2)
library(Information)
library(cowplot)
library(ggthemes)
# read demographic data

dem_data <- read.csv("Demographic data.csv")

#Understand demographic data

summary(dem_data)
str(dem_data)
dim(dem_data)

table(dem_data$Performance.Tag)

# identify duplicate rows - No duplicate rows
nrow(dem_data) - nrow(unique(dem_data))

# find  blanks proportion- negligible and can be ignored
blanks<- as.data.frame(sapply(dem_data, function(x)length(which(x == ''))/nrow(dem_data)))
View(blanks)                                   

# find NAs proportion - neglible NAs and can be ignored
NAs<- as.data.frame(sapply(dem_data,function(x)mean(is.na(x))))
View(NAs)
dem_data_NA <- dem_data[rowSums(is.na(dem_data))== 0,]


### Univariate Analysis
# Categorical Variable Plots
plot_grid(ggplot(dem_data_NA,aes(x=Gender)) +geom_bar()+theme_grey(),
          ggplot(dem_data_NA,aes(x=Education)) +geom_bar()+theme_grey(),
          ggplot(dem_data_NA,aes(x=Type.of.residence))+geom_bar()+theme_grey(),
          ggplot(dem_data_NA,aes(x=Profession))+geom_bar()+theme_grey(),
          ggplot(dem_data_NA,aes(x=Marital.Status..at.the.time.of.application.))+geom_bar()+theme_grey(),
          ggplot(dem_data_NA,aes(x=No.of.dependents))+geom_bar()+theme_grey()
          )

ggsave("Discrete_univariate_analysis.jpeg",height = 8,width = 15)

#Continuous variable Analysis- Age, Income,no.of months in company,no.of months in residence
plot_grid(ggplot (dem_data_NA,aes(x=Age)) +geom_histogram(binwidth=5,color="red",fill="blue",alpha=0.5)+ xlim(0,85),
ggplot(dem_data_NA,aes(x=Income)) +geom_histogram(color="red",fill="blue",alpha=0.4)+xlim(0,70),
ggplot(dem_data_NA,aes(x=No.of.months.in.current.residence)) +geom_histogram(color="red",fill="blue",alpha=0.4)+theme_grey(),
ggplot(dem_data_NA,aes(x=No.of.months.in.current.company)) + geom_histogram(color="red",fill="blue",alpha=0.4)
)
ggsave("Continuous_univariate_analysis_histograms.jpeg",height = 8,width = 15)

# Continuous variables- Finding outliers using boxplot
plot_grid(ggplot(dem_data,aes(y=Age)) + geom_boxplot(outlier.color = 'red')+theme_grey(),
ggplot(dem_data_NA,aes(y=Income))+geom_boxplot(outlier.color='red')+theme_grey(),
ggplot(dem_data_NA,aes(y=No.of.months.in.current.residence))+geom_boxplot(outlier.color='red')+theme_grey(),
ggplot(dem_data_NA,aes(y=No.of.months.in.current.company))+geom_boxplot(outlier.color='red')+theme_grey())

ggsave("Continuous_univariate_outlier_analysis.jpeg",height = 8,width = 15)

#Bivariate Analysis
#find IV and WOE
IV<- create_infotables(data=dem_data_NA,y ="Performance.Tag",bins=10,parallel = TRUE)
IV_df<-data.frame(IV$Summary)
IV_df<- arrange(IV_df,desc(IV_df$IV))
View(IV_df)
#Income, number of months in current residence and number of months in current company have highest IV.IV indicates some weak relation
#Plot a relation graph for all 3 varaibles and performance.tag, shows weak realtion.

plot_grid(ggplot(dem_data_NA,aes(x=factor(Performance.Tag),y=Income))+ geom_boxplot()+xlab("Performance"),
ggplot(dem_data_NA,aes(x=factor(Performance.Tag),y=No.of.months.in.current.residence))+ geom_boxplot()+xlab("Performance"),
ggplot(dem_data_NA,aes(x=factor(Performance.Tag),y=No.of.months.in.current.company))+ geom_boxplot()+xlab("Performance"))

ggsave("WeakRelatedVariables_with_TargetVariable.jpeg",height = 8,width = 15)

# Not much relation of performance with Gender,Marital Status,Education,Profession,Type Of residence
plot_grid(ggplot(dem_data_NA,aes(x=factor(Performance.Tag),fill=Gender)) +geom_bar(position="fill") + xlab("Default"),
ggplot(dem_data_NA,aes(x=factor(Performance.Tag),fill=Education)) +geom_bar(position="fill") + xlab("Default"),
ggplot(dem_data_NA,aes(x=factor(Performance.Tag),fill=Profession)) +geom_bar(position="fill") + xlab("Default"),
ggplot(dem_data_NA,aes(x=factor(Performance.Tag),fill=Type.of.residence)) +geom_bar(position="fill") + xlab("Default"),
ggplot(dem_data_NA,aes(x=factor(Performance.Tag),fill=Marital.Status..at.the.time.of.application.)) +geom_bar(position="fill") + xlab("Default")
)

ggsave("Low_IV_Variables.jpeg",height=8,width=15)

#Merge Demographic and credit Bureau Data

credit_data <-read.csv("Credit Bureau data.csv")
summary(credit_data)
dim(credit_data)
length(unique(credit_data$Application.ID))-length(unique(dem_data$Application.ID))
setdiff((credit_data$Application.ID),(dem_data$Application.ID))

merged_data <- merge(dem_data,credit_data,by=1,all=TRUE)   
merged_data <- merged_data[,-ncol(merged_data)]

#Remove NAs from merged data
merged_data_NA <- merged_data[-which(is.na(merged_data$Performance.Tag.x)),]
View(merged_data_NA)
str(merged_data_NA)

# find  blanks proportion- negligible and can be ignored
blanks_merged<- as.data.frame(sapply(merged_data_NA, function(x)length(which(x == ''))/nrow(dem_data)*100))
View(blanks_merged)                                   

# find NAs proportion - neglible NAs and can be ignored-1.5% NAs in Average utilization 
NAs_merged<- as.data.frame(sapply(merged_data_NA,function(x)mean(is.na(x))*100))

View(NAs_merged)

# Replace CC NAs with average values

merged_data_NA$Avgas.CC.Utilization.in.last.12.months[(which(is.na(merged_data_NA$Avgas.CC.Utilization.in.last.12.months)))] <- mean(merged_data_NA$Avgas.CC.Utilization.in.last.12.months,na.rm=TRUE)
merged_data$Avgas.CC.Utilization.in.last.12.months[(which(is.na(merged_data$Avgas.CC.Utilization.in.last.12.months)))] <- mean(merged_data$Avgas.CC.Utilization.in.last.12.months,na.rm=TRUE)

 # convert negative income to zero values

merged_data_NA[which(merged_data_NA$Income<= 0),"Income"] <-1
merged_data[which(merged_data$Income<= 0),"Income"] <-1
# Create a new income /outstanding balance variable
merged_data_NA$bal_income_ratio <-merged_data_NA$Outstanding.Balance/(merged_data_NA$Income*12*1000)
merged_data$bal_income_ratio <-merged_data$Outstanding.Balance/(merged_data$Income*12*1000)
# IV and WOE of merged data set
IV_merge <- create_infotables(data=merged_data_NA,y="Performance.Tag.x" ,bins=10,parallel = TRUE)
IV_merge_df<- as.data.frame(IV_merge$Summary)
IV_merge_df <- arrange(IV_merge_df,desc(IV_merge_df$IV))
IV_merge_df_IV <-IV_merge_df[which(IV_merge_df$IV>= 2.5e-01),]
ggplot(IV_merge_df_IV,aes(x= IV_merge_df_IV$Variable,y=IV_merge_df_IV$IV)) + geom_bar(stat="identity")+ggtitle("IV Values>0.25")
ggsave("IV_values_gretaerthan0.25.jpeg", height= 20 , width=30)

IV_merge_df_IV <-IV_merge_df[which(IV_merge_df$IV < 2.5e-01&IV_merge_df$IV>=2e-01),]
ggplot(IV_merge_df_IV,aes(x= IV_merge_df_IV$Variable,y=IV_merge_df_IV$IV)) + geom_bar(stat="identity")+ggtitle("IV Values between 0.25&0.2")
ggsave("IV_values_between0.25_0.2.jpeg", height= 20 , width=30)


IV_merge_df_IV <-IV_merge_df[which(IV_merge_df$IV < 2e-01&IV_merge_df$IV>=1e-01),]
ggplot(IV_merge_df_IV,aes(x= IV_merge_df_IV$Variable,y=IV_merge_df_IV$IV)) + geom_bar(stat="identity")+ggtitle("IV Values between 0.2&0.1")
ggsave("IV_values_between0.2_0.1.jpeg", height= 20 , width=30)


#COnvert dependent variable to factor

merged_data_NA$Performance.Tag.x <- as.factor(merged_data_NA$Performance.Tag.x)
merged_data$Performance.Tag.x <- as.factor(merged_data$Performance.Tag.x)


# Per Iv important predictor variables with values greter than 0.2 are
#Avg Utiization,no. of trades open in last 12 months,no. of pl trades open in last 12,number of enquiries,
#Outstanding balance ,no. of times 30 DPD in 6 months,total trades,all attributes on DPD


# analyze CC utilization and treat outliers
ggplot(merged_data_NA,aes(y=merged_data$Avgas.CC.Utilization.in.last.12.months)) + geom_boxplot(outlier.color = "red") +theme_bw()

# Replace utilization greater than 100 to 100
merged_data_NA[which(merged_data_NA$Avgas.CC.Utilization.in.last.12.months>100),"Avgas.CC.Utilization.in.last.12.months"]<-100
merged_data[which(merged_data$Avgas.CC.Utilization.in.last.12.months>100),"Avgas.CC.Utilization.in.last.12.months"]<-100

#Univariate Analysis of cotinuous variables- CC Utilization,outstanding balance,number of trades open in last12,
# Number of enquiries # Number of PL Trades

plot_grid(ggplot(merged_data_NA,aes(x=merged_data_NA$Avgas.CC.Utilization.in.last.12.months)+geom_histogram()+ggtitle("CC Utilization Distribution")),
          ggplot(merged_data_NA,aes(x=merged_data_NA$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)+geom_histogram(color="red",fill="green",alpha=0.4,na.rm = TRUE)+ggtitle("Number of enquiries")),
          ggplot(merged_data_NA,aes(x=merged_data_NA$No.of.trades.opened.in.last.12.months)+geom_histogram(color="red",fill="green",alpha=0.4,na.rm = TRUE)+ggtitle("Trades open in last 12"))
          )



# Complete Bivariate ANalysis of merged data using ggplot

summary (merged_data[,c(12:28)])
# Convert avg cc utilization, number of total trades,no. of inquires and no of PL Trades open in last 12 months to buckets
table(cut(merged_data_NA$No.of.trades.opened.in.last.12.months,c(0,2,5,10,28)))
table(cut(merged_data_NA$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,c(-1,1,3,6,20)))
 table(cut(merged_data_NA$Total.No.of.Trades,c(-1,3,6,10,14,44)))

merged_data_NA$Avgas.CC.Utilization.in.last.12.months.bucket<- cut(merged_data_NA$Avgas.CC.Utilization.in.last.12.months,c(-1,25,75,90,100)) 
merged_data_NA$No.of.trades.opened.in.last.12.months.bucket<- cut(merged_data_NA$No.of.trades.opened.in.last.12.months,c(-1,2,5,10,28))
merged_data_NA$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket <- cut(merged_data_NA$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,c(-1,1,3,6,20))
merged_data_NA$Total.No.of.Trades.bucket <- cut(merged_data_NA$Total.No.of.Trades,c(-1,3,6,10,14,44))

merged_data_NA$Outstanding.Balance.bucket<- cut(merged_data_NA$Outstanding.Balance,c(-1,230000,130000,3000000,5300000))

merged_data_NA$Income.bucket <- cut(merged_data_NA$Income,c(-1,15,25,40,62))



merged_data$Avgas.CC.Utilization.in.last.12.months.bucket<- cut(merged_data$Avgas.CC.Utilization.in.last.12.months,c(-1,25,75,90,100)) 
merged_data$No.of.trades.opened.in.last.12.months.bucket<- cut(merged_data$No.of.trades.opened.in.last.12.months,c(-1,2,5,10,28))
merged_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket <- cut(merged_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,c(-1,1,3,6,20))
merged_data$Total.No.of.Trades.bucket <- cut(merged_data$Total.No.of.Trades,c(-1,3,6,10,14,44))

merged_data$Outstanding.Balance.bucket<- cut(merged_data$Outstanding.Balance,c(-1,230000,130000,3000000,5300000))

merged_data$Income.bucket <- cut(merged_data$Income,c(-1,15,25,40,62))


# create a income/outstanding balance ratio field and bucket it into 4 quartiles

merged_data_NA$bal_income_ratio.bucket <- cut(merged_data_NA$bal_income_ratio,c(-1,0.7,3,8,15,35))
merged_data$bal_income_ratio.bucket <- cut(merged_data$bal_income_ratio,c(-1,0.7,3,8,15,35))

# Plot high IV value continuous variables using boxplot
plot_grid(ggplot(merged_data_NA,aes(merged_data_NA$Performance.Tag.x,y=merged_data_NA$Avgas.CC.Utilization.in.last.12.months))+geom_boxplot()+xlab("Performance")+ylab("CC Utilization"),
ggplot(merged_data_NA,aes(merged_data_NA$Performance.Tag.x,y=merged_data_NA$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))+geom_boxplot()+xlab("Performance")+ylab("Inquiries made last 1 yr"),
ggplot(merged_data_NA,aes(merged_data_NA$Performance.Tag.x,y=merged_data_NA$No.of.PL.trades.opened.in.last.12.months))+geom_boxplot()+xlab("Performance")+ylab("PL trades in last 1 yr"),
ggplot(merged_data_NA,aes(merged_data_NA$Performance.Tag.x,y=merged_data_NA$No.of.trades.opened.in.last.12.months))+geom_boxplot()+xlab("Performance")+ylab("Trades made last 1 yr"),
ggplot(merged_data_NA,aes(merged_data_NA$Performance.Tag.x,y=merged_data_NA$Outstanding.Balance))+geom_boxplot()+xlab("Performance")+ylab("Outstanding Balance"),
ggplot(merged_data_NA,aes(merged_data_NA$Performance.Tag.x,y=merged_data_NA$Total.No.of.Trades))+geom_boxplot()+xlab("Performance")+ylab("Total Trades")
)

ggsave("HighIV_Values_boxplot_withoutNA.jpeg",height=5,width=15)
 # Plot high IV value continuious variables as buckets versus the default
# Plot Income/outstanding balance


plot_grid(ggplot(merged_data_NA,aes(x=factor(merged_data_NA$Performance.Tag.x),fill=bal_income_ratio.bucket)) + geom_bar(position= "fill" )+ xlab("Performance") +ylab("Income-Balance Ratio"),
ggplot(merged_data_NA,aes(x=factor(merged_data_NA$Performance.Tag.x),fill=Avgas.CC.Utilization.in.last.12.months.bucket)) +geom_bar(position="fill") + xlab("Performance") + ylab("Average CC Utilizations"),
ggplot(merged_data_NA,aes(x=factor(merged_data_NA$Performance.Tag.x),fill=factor(Presence.of.open.home.loan))) +geom_bar(position="fill")+xlab("Performance") +ylab("Presence of Home loan"),
ggplot(merged_data_NA,aes(x=factor(merged_data_NA$Performance.Tag.x),fill=factor(No.of.times.30.DPD.or.worse.in.last.6.months))) +geom_bar(position="fill")+xlab("Performance") +ylab("30 DPD in last 12 months")
)

class(merged_data_NA$Avgas.CC.Utilization.in.last.12.months.bucket)

ggsave("HighIV_Values_bucketed_NA.jpeg",height=5,width=15)


###Basic EDA ends here#######
##### Part 1 ENDS HERE#################################
####################################################################

########################################################################

####Part 2 - Build first basic LOGISTIC REGRESSION MODEL AND EVALUATE##########
##Logistic Regression data prepartion##

lr_data<- merged_data_NA

#Remove blanks

lr_data_blank <- lr_data

nrow(lr_data_blank)
summary(lr_data_blank)
 lr_data_blank<- lr_data_blank[rowSums(is.na(lr_data_blank))==0,]
sum(is.na(lr_data_blank))
 
 View(lr_data_blank)
#Bucket categorical variables & contnuous variables
lr_data_cat <- lr_data_blank[, c(1,3,4,7,8,9,31,32,33,34,35,36,37)]
lr_data_continuous <-  lr_data_blank[,-c(3,4,7,8,9,31,32,33,34,35,36,37)]

View(lr_data_cat)

# Standardise continuous variables
lr_data_continuous_scaled <- data.frame(sapply(lr_data_continuous[,-c(1,7)], function(x){scale(x)}))

#Create dummy varaibles for all the categorical variables
lr_data_cat_dummy <- data.frame (sapply(lr_data_cat[,-1],function(x)data.frame(model.matrix(~x-1,data=lr_data_cat))[,-1]))

View(lr_data_cat_dummy)  


# merge
nrow(lr_data_continuous_scaled)
nrow(lr_data_cat_dummy)


# Add cat_dummy and scaled continuous

lr_data_final <- cbind(lr_data_cat_dummy,lr_data_continuous_scaled)
lr_data_final[,64] <- lr_data_blank$Performance.Tag.x
colnames(lr_data_final)[64] <- 'Loan Performance'
lr_data_final$`Loan Performance`<- factor(lr_data_final$`Loan Performance`)
nrow(lr_data_final)
dim(lr_data_final)
str(lr_data_final)
##### use lr_data_final to run logistic regression

###### Baseline Logistic Regression Model Building #####
library(MASS)
library(car)
library(e1071)
library(ggplot2)
library(cowplot)
library(caTools)
library(heuristica)
library(caret)


### Splitting data into traing and test data##

set.seed(70)
indices <- sample.split(lr_data_final$`Loan Performance`,SplitRatio = 0.7)

traindata <- lr_data_final[indices,]
testdata <- lr_data_final[!(indices),]

###STart building Logistic Regression########
##Initial Model

model_1 = glm(`Loan Performance` ~ ., data = traindata, family = "binomial")
summary(model_1) # Null deviance: 15334  on 44223  degrees of freedom Residual deviance: 14506  on 44160  degrees of freedom
##AIC: 14634



#Remove all the columns that has p-value >0.5 and seems highly insignificant

traindata_1 <- traindata [indices,-c(1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	22,	30,	31,	32,	37,	41,	42,	46,	49,	50,	59,	60,	62,	63)]
testdata_1 <- traindata [!(indices),-c(1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	22,	30,	31,	32,	37,	41,	42,	46,	49,	50,	59,	60,	62,	63)]

model_2 <- glm(`Loan Performance` ~ ., data = traindata_1, family = "binomial")
summary(model_2)

#stepaic
library("MASS")
model_3 <- stepAIC(model_2,direction= "both")
summary(model_3)

### Remove Total.No.of.Trades.bucket.x.6.10.,No.of.times.60.DPD.or.worse.in.last.6.months,Total.No.of.Trades,No.of.times.30.DPD.or.worse.in.last.6.months,No.of.trades.opened.in.last.6.months,No.of.trades.opened.in.last.12.months.bucket.x.2.5.,Total.No.of.Trades.bucket.x.3.6.,No.of.trades.opened.in.last.12.months.bucket.x.10.28.

model_4<- glm(formula=`Loan Performance`~Total.No.of.Trades.bucket.x.10.14.+
                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                bal_income_ratio.bucket.x.15.35.+
                Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                Income.bucket.x.15.25.+
                Avgas.CC.Utilization.in.last.12.months.bucket.x.75.90.+
                No.of.months.in.current.residence+
                No.of.PL.trades.opened.in.last.6.months+
                bal_income_ratio.bucket.x.8.15.+
                No.of.trades.opened.in.last.12.months+
                No.of.PL.trades.opened.in.last.12.months+
                No.of.months.in.current.company+
                bal_income_ratio.bucket.x.3.8.+
                Income.bucket.x.25.40.+
                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.6.20.+
                Income.bucket.x.40.62.+
                Income+
                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                Avgas.CC.Utilization.in.last.12.months.bucket.x.90.100.+
                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                No.of.times.30.DPD.or.worse.in.last.12.months, family = "binomial",data=traindata_1)
summary(model_4) 

# Remove No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,No.of.months.in.current.residence,Avgas.CC.Utilization.in.last.12.months.bucket.x.90.100.


model_5 <- glm(formula= `Loan Performance`~Total.No.of.Trades.bucket.x.10.14.+
                 bal_income_ratio.bucket.x.15.35.+
                 Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                 Income.bucket.x.15.25.+
                 Avgas.CC.Utilization.in.last.12.months.bucket.x.75.90.+
                 No.of.PL.trades.opened.in.last.6.months+
                 bal_income_ratio.bucket.x.8.15.+
                 No.of.trades.opened.in.last.12.months+
                 No.of.PL.trades.opened.in.last.12.months+
                 No.of.months.in.current.company+
                 bal_income_ratio.bucket.x.3.8.+
                 Income.bucket.x.25.40.+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.6.20.+
                 Income.bucket.x.40.62.+
                 Income+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                 No.of.times.30.DPD.or.worse.in.last.12.months,family="binomial",data= traindata_1)
summary(model_5)
library(car)
vif(model_5)


# Remove Total.No.of.Trades.bucket.x.10.14.,No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,Income.bucket.x.15.25.,Avgas.CC.Utilization.in.last.12.months.bucket.x.75.90.
model_6 <- glm(formula= `Loan Performance`~
                 bal_income_ratio.bucket.x.15.35.+
                 Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                 Income.bucket.x.15.25.+
                 No.of.PL.trades.opened.in.last.6.months+
                 bal_income_ratio.bucket.x.8.15.+
                 No.of.trades.opened.in.last.12.months+
                 No.of.PL.trades.opened.in.last.12.months+
                 No.of.months.in.current.company+
                 bal_income_ratio.bucket.x.3.8.+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.6.20.+
                 Income.bucket.x.40.62.+
                 Income+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                 No.of.times.30.DPD.or.worse.in.last.12.months,family="binomial",data= traindata_1)
summary(model_6)
vif(model_6)

#Remove Outstanding.Balance.bucket.x.3e.06.5.3e.06.,Income.bucket.x.15.25.Income.bucket.x.40.62.
model_7 <- glm(formula= `Loan Performance`~
                 bal_income_ratio.bucket.x.15.35.+
                 No.of.PL.trades.opened.in.last.6.months+
                 bal_income_ratio.bucket.x.8.15.+
                 No.of.trades.opened.in.last.12.months+
                 No.of.PL.trades.opened.in.last.12.months+
                 No.of.months.in.current.company+
                 bal_income_ratio.bucket.x.3.8.+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.6.20.+
                 Income+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                 No.of.times.30.DPD.or.worse.in.last.12.months,family="binomial",data= traindata_1)
summary(model_7)
vif(model_7)

# Remove bal_income_ratio.bucket.x.15.35.,No.of.PL.trades.opened.in.last.6.months,bal_income_ratio.bucket.x.8.15.,Income,No.of.trades.opened.in.last.12.months+,No.of.PL.trades.opened.in.last.12.months+,bal_income_ratio.bucket.x.3.8.
model_8 <- glm(formula= `Loan Performance`~
                 No.of.months.in.current.company+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.6.20.+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                 No.of.times.30.DPD.or.worse.in.last.12.months,family="binomial",data= traindata_1)
summary(model_8)
vif(model_8)



######final model with 5 significant variables###

final_model_base <- model_8

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of defualt for test data 

test_pred_base = predict(final_model_base, type = "response", 
                    newdata = testdata_1[,-32])


# Let's see the summary 

summary(test_pred)
length(test_pred)

testdata_1$prob <- test_pred_base

# Let's use the probability cutoff of 10%.

test_pred_default_base <- factor(ifelse(test_pred_base >= 0.10, "Yes", "No"))
test_actual_default_base <- factor(ifelse(testdata_1$`Loan Performance`==1,"Yes","No"))


table(test_actual_default_base,test_pred_default_base)




#########################################################################################
# Let's Choose the cutoff value. 


# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_default_base <- factor(ifelse(test_pred_base >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_default_base, test_actual_default_base, positive = "No")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.20 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred_base)

s = seq(.01,.20,length=100) 
 s

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,0.2,length=5),seq(0,0.2,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
View(OUT)

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]

cutoff
# Let's choose a cutoff value of 0.04454 for final model

test_cutoff_default_base <- factor(ifelse(test_pred_base >=0.04454, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_default_base, test_actual_default_base, positive = "No")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc # 61.9

sens #61.9

spec #61.9

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_default_base <- ifelse(test_cutoff_default_base=="Yes",1,0)
test_actual_default_base <- ifelse(test_actual_default_base=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_default_base, test_actual_default_base)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

###Ks stats-0.2286####

 ####Part2- Baseline LOgistic Regression Model Building and evaluation ends here#####
####################################################################

########################################################################

### Part 3 - Logistic Regression on rebalanced data using SMOTE#################
### Logistic Regression Model Building on balanced data by oversampling default data and undersampling non defaults######
###Use SMOTE for rebalancing data####

set.seed(100)

indices = sample.split(lr_data_final$`Loan Performance`, SplitRatio = 0.7)

train = lr_data_final[indices,]

test = lr_data_final[!(indices),]

####balance data using smote####

library(DMwR)
lr_data_final_bal <- lr_data_final
train_bal <- SMOTE(`Loan Performance`~ .,train,perc.over = 100,perc.under = 200)
table(train_bal$`Loan Performance`)

model_1_bal <- glm(`Loan Performance`~ .,data = train_bal,family = "binomial")

summary(model_1_bal)

# Stepwise selection
library("MASS")

model_2_bal<- stepAIC(model_1_bal, direction="both")

summary(model_2_bal)

# Removing multicollinearity through VIF check
library(car)
vif(model_2_bal)

#Excluding No.of.trades.opened.in.last.12.months

model_3_bal <- glm(formula = `Loan Performance`~ Marital.Status..at.the.time.of.application..xMarried+
                     Education.xBachelor+
                     Education.xMasters+
                     Education.xProfessional+
                     Profession.xSE+
                     Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                     No.of.trades.opened.in.last.12.months.bucket.x.2.5.+
                     No.of.trades.opened.in.last.12.months.bucket.x.5.10.+
                     No.of.trades.opened.in.last.12.months.bucket.x.10.28.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.6.10.+
                     Total.No.of.Trades.bucket.x.10.14.+
                     Total.No.of.Trades.bucket.x.14.44.+
                     Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                     Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                     Income.bucket.x.15.25.+
                     Income.bucket.x.25.40.+
                     Income.bucket.x.40.62.+
                     bal_income_ratio.bucket.x.3.8.+
                     No.of.dependents+
                     Income+
                     No.of.months.in.current.residence+
                     No.of.times.60.DPD.or.worse.in.last.6.months+
                     No.of.times.30.DPD.or.worse.in.last.6.months+
                     No.of.times.90.DPD.or.worse.in.last.12.months+
                     Avgas.CC.Utilization.in.last.12.months+
                     No.of.PL.trades.opened.in.last.12.months+
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                     Presence.of.open.home.loan+
                     Total.No.of.Trades+
                     Presence.of.open.auto.loan,
                   family = "binomial", data = train_bal
)

summary(model_3_bal) 

vif(model_3_bal) 


#Excluding Total.No.of.Trades

model_4_bal <- glm(formula = `Loan Performance`~ 
                     Marital.Status..at.the.time.of.application..xMarried+
                     Education.xBachelor+
                     Education.xMasters+
                     Education.xProfessional+
                     Profession.xSE+
                     Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                     No.of.trades.opened.in.last.12.months.bucket.x.2.5.+
                     No.of.trades.opened.in.last.12.months.bucket.x.5.10.+
                     No.of.trades.opened.in.last.12.months.bucket.x.10.28.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.6.10.+
                     Total.No.of.Trades.bucket.x.10.14.+
                     Total.No.of.Trades.bucket.x.14.44.+
                     Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                     Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                     Income.bucket.x.15.25.+
                     Income.bucket.x.25.40.+
                     Income.bucket.x.40.62.+
                     bal_income_ratio.bucket.x.3.8.+
                     No.of.dependents+
                     Income+
                     No.of.months.in.current.residence+
                     No.of.times.60.DPD.or.worse.in.last.6.months+
                     No.of.times.30.DPD.or.worse.in.last.6.months+
                     No.of.times.90.DPD.or.worse.in.last.12.months+
                     Avgas.CC.Utilization.in.last.12.months+
                     No.of.PL.trades.opened.in.last.12.months+
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                     Presence.of.open.home.loan+
                     Presence.of.open.auto.loan,
                   family = "binomial", data = train_bal
)

summary(model_4_bal) 

vif(model_4_bal) 


#Excluding Income.bucket.x.40.62.

model_5_bal <- glm(formula = `Loan Performance`~ 
                     Education.xBachelor+
                     Education.xMasters+
                     Education.xProfessional+
                     Profession.xSE+
                     Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                     No.of.trades.opened.in.last.12.months.bucket.x.2.5.+
                     No.of.trades.opened.in.last.12.months.bucket.x.5.10.+
                     No.of.trades.opened.in.last.12.months.bucket.x.10.28.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.6.10.+
                     Total.No.of.Trades.bucket.x.10.14.+
                     Total.No.of.Trades.bucket.x.14.44.+
                     Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                     Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                     Income.bucket.x.15.25.+
                     Income.bucket.x.25.40.+
                     bal_income_ratio.bucket.x.3.8.+
                     No.of.dependents+
                     Income+
                     No.of.months.in.current.residence+
                     No.of.times.60.DPD.or.worse.in.last.6.months+
                     No.of.times.30.DPD.or.worse.in.last.6.months+
                     No.of.times.90.DPD.or.worse.in.last.12.months+
                     Avgas.CC.Utilization.in.last.12.months+
                     No.of.PL.trades.opened.in.last.12.months+
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                     Presence.of.open.home.loan+
                     Total.No.of.Trades+
                     Presence.of.open.auto.loan,
                   family = "binomial", data = train_bal
)

summary(model_5_bal) 

vif(model_5_bal) 


#Excluding Total.No.of.Trades.bucket.x.14.44.

model_6_bal <- glm(formula = `Loan Performance`~ 
                     Education.xBachelor+
                     Education.xMasters+
                     Education.xProfessional+
                     Profession.xSE+
                     Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                     No.of.trades.opened.in.last.12.months.bucket.x.2.5.+
                     No.of.trades.opened.in.last.12.months.bucket.x.5.10.+
                     No.of.trades.opened.in.last.12.months.bucket.x.10.28.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.6.10.+
                     Total.No.of.Trades.bucket.x.10.14.+
                     Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                     Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                     Income.bucket.x.15.25.+
                     Income.bucket.x.25.40.+
                     bal_income_ratio.bucket.x.3.8.+
                     No.of.dependents+
                     Income+
                     No.of.months.in.current.residence+
                     No.of.times.60.DPD.or.worse.in.last.6.months+
                     No.of.times.30.DPD.or.worse.in.last.6.months+
                     No.of.times.90.DPD.or.worse.in.last.12.months+
                     Avgas.CC.Utilization.in.last.12.months+
                     No.of.PL.trades.opened.in.last.12.months+
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                     Presence.of.open.home.loan+
                     Total.No.of.Trades+
                     Presence.of.open.auto.loan,
                   family = "binomial", data = train_bal
)

summary(model_6_bal) 

vif(model_6_bal) 

#Excluding No.of.trades.opened.in.last.12.months.bucket.x.10.28

model_7_bal <- glm(formula = `Loan Performance`~ 
                     Education.xBachelor+
                     Education.xMasters+
                     Education.xProfessional+
                     Profession.xSE+
                     Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                     No.of.trades.opened.in.last.12.months.bucket.x.2.5.+
                     No.of.trades.opened.in.last.12.months.bucket.x.5.10.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.6.10.+
                     Total.No.of.Trades.bucket.x.10.14.+
                     Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                     Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                     Income.bucket.x.15.25.+
                     Income.bucket.x.25.40.+
                     bal_income_ratio.bucket.x.3.8.+
                     No.of.dependents+
                     Income+
                     No.of.months.in.current.residence+
                     No.of.times.60.DPD.or.worse.in.last.6.months+
                     No.of.times.30.DPD.or.worse.in.last.6.months+
                     No.of.times.90.DPD.or.worse.in.last.12.months+
                     Avgas.CC.Utilization.in.last.12.months+
                     No.of.PL.trades.opened.in.last.12.months+
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                     Presence.of.open.home.loan+
                     Total.No.of.Trades+
                     Presence.of.open.auto.loan,
                   family = "binomial", data = train_bal
)

summary(model_7_bal) 

vif(model_7_bal) 


#Excluding Education.xMasters

model_8_bal <- glm(formula = `Loan Performance`~ 
                     Education.xBachelor+
                     Education.xProfessional+
                     Profession.xSE+
                     Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                     No.of.trades.opened.in.last.12.months.bucket.x.2.5.+
                     No.of.trades.opened.in.last.12.months.bucket.x.5.10.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.6.10.+
                     Total.No.of.Trades.bucket.x.10.14.+
                     Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                     Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                     Income.bucket.x.15.25.+
                     Income.bucket.x.25.40.+
                     bal_income_ratio.bucket.x.3.8.+
                     No.of.dependents+
                     Income+
                     No.of.months.in.current.residence+
                     No.of.times.60.DPD.or.worse.in.last.6.months+
                     No.of.times.30.DPD.or.worse.in.last.6.months+
                     No.of.times.90.DPD.or.worse.in.last.12.months+
                     Avgas.CC.Utilization.in.last.12.months+
                     No.of.PL.trades.opened.in.last.12.months+
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                     Presence.of.open.home.loan+
                     Total.No.of.Trades+
                     Presence.of.open.auto.loan,
                   family = "binomial", data = train_bal
)

summary(model_8_bal) 

vif(model_8_bal) 

#Excluding No.of.trades.opened.in.last.12.months.bucket.x.5.10.

model_9_bal <- glm(formula = `Loan Performance`~ 
                     Education.xBachelor+
                     Education.xProfessional+
                     Profession.xSE+
                     Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                     No.of.trades.opened.in.last.12.months.bucket.x.2.5.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.3.6.+
                     Total.No.of.Trades.bucket.x.6.10.+
                     Total.No.of.Trades.bucket.x.10.14.+
                     Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                     Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                     Income.bucket.x.15.25.+
                     Income.bucket.x.25.40.+
                     bal_income_ratio.bucket.x.3.8.+
                     No.of.dependents+
                     Income+
                     No.of.months.in.current.residence+
                     No.of.times.60.DPD.or.worse.in.last.6.months+
                     No.of.times.30.DPD.or.worse.in.last.6.months+
                     No.of.times.90.DPD.or.worse.in.last.12.months+
                     Avgas.CC.Utilization.in.last.12.months+
                     No.of.PL.trades.opened.in.last.12.months+
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                     Presence.of.open.home.loan+
                     Total.No.of.Trades+
                     Presence.of.open.auto.loan,
                   family = "binomial", data = train_bal
)

summary(model_9_bal) 

vif(model_9_bal)



#Excluding Education.xBachelor & Education.xProfessional

model_10_bal <- glm(formula = `Loan Performance`~ 
                      Profession.xSE+
                      Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                      No.of.trades.opened.in.last.12.months.bucket.x.2.5.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.6.10.+
                      Total.No.of.Trades.bucket.x.10.14.+
                      Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                      Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                      Income.bucket.x.15.25.+
                      Income.bucket.x.25.40.+
                      bal_income_ratio.bucket.x.3.8.+
                      No.of.dependents+
                      Income+
                      No.of.months.in.current.residence+
                      No.of.times.60.DPD.or.worse.in.last.6.months+
                      No.of.times.30.DPD.or.worse.in.last.6.months+
                      No.of.times.90.DPD.or.worse.in.last.12.months+
                      Avgas.CC.Utilization.in.last.12.months+
                      No.of.PL.trades.opened.in.last.12.months+
                      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                      Presence.of.open.home.loan+
                      Total.No.of.Trades+
                      Presence.of.open.auto.loan,
                    family = "binomial", data = train_bal
)

summary(model_10_bal) 

vif(model_10_bal)


#Excluding No.of.trades.opened.in.last.12.months.bucket.x.2.5.

model_11_bal <- glm(formula = `Loan Performance`~ 
                      Profession.xSE+
                      Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.6.10.+
                      Total.No.of.Trades.bucket.x.10.14.+
                      Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                      Outstanding.Balance.bucket.x.3e.06.5.3e.06.+
                      Income.bucket.x.15.25.+
                      Income.bucket.x.25.40.+
                      bal_income_ratio.bucket.x.3.8.+
                      No.of.dependents+
                      Income+
                      No.of.months.in.current.residence+
                      No.of.times.60.DPD.or.worse.in.last.6.months+
                      No.of.times.30.DPD.or.worse.in.last.6.months+
                      No.of.times.90.DPD.or.worse.in.last.12.months+
                      Avgas.CC.Utilization.in.last.12.months+
                      No.of.PL.trades.opened.in.last.12.months+
                      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                      Presence.of.open.home.loan+
                      Total.No.of.Trades+
                      Presence.of.open.auto.loan,
                    family = "binomial", data = train_bal
)

summary(model_11_bal) 

vif(model_11_bal)


#Excluding Outstanding.Balance.bucket.x.3e.06.5.3e.06.

model_12_bal <- glm(formula = `Loan Performance`~ 
                      Profession.xSE+
                      Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.6.10.+
                      Total.No.of.Trades.bucket.x.10.14.+
                      Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                      Income.bucket.x.15.25.+
                      Income.bucket.x.25.40.+
                      bal_income_ratio.bucket.x.3.8.+
                      No.of.dependents+
                      Income+
                      No.of.months.in.current.residence+
                      No.of.times.60.DPD.or.worse.in.last.6.months+
                      No.of.times.30.DPD.or.worse.in.last.6.months+
                      No.of.times.90.DPD.or.worse.in.last.12.months+
                      Avgas.CC.Utilization.in.last.12.months+
                      No.of.PL.trades.opened.in.last.12.months+
                      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                      Presence.of.open.home.loan+
                      Total.No.of.Trades+
                      Presence.of.open.auto.loan,
                    family = "binomial", data = train_bal
)

summary(model_12_bal) 

vif(model_12_bal)


#Excluding No.of.times.90.DPD.or.worse.in.last.12.months 

model_13_bal <- glm(formula = `Loan Performance`~ 
                      Profession.xSE+
                      Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.6.10.+
                      Total.No.of.Trades.bucket.x.10.14.+
                      Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                      Income.bucket.x.15.25.+
                      Income.bucket.x.25.40.+
                      bal_income_ratio.bucket.x.3.8.+
                      No.of.dependents+
                      Income+
                      No.of.months.in.current.residence+
                      No.of.times.60.DPD.or.worse.in.last.6.months+
                      No.of.times.30.DPD.or.worse.in.last.6.months+
                      Avgas.CC.Utilization.in.last.12.months+
                      No.of.PL.trades.opened.in.last.12.months+
                      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                      Presence.of.open.home.loan+
                      Total.No.of.Trades+
                      Presence.of.open.auto.loan,
                    family = "binomial", data = train_bal
)

summary(model_13_bal) 

vif(model_13_bal)


#Excluding  No.of.times.60.DPD.or.worse.in.last.6.months 

model_14_bal <- glm(formula = `Loan Performance`~ 
                      Profession.xSE+
                      Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.6.10.+
                      Total.No.of.Trades.bucket.x.10.14.+
                      Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                      Income.bucket.x.15.25.+
                      Income.bucket.x.25.40.+
                      bal_income_ratio.bucket.x.3.8.+
                      No.of.dependents+
                      Income+
                      No.of.months.in.current.residence+
                      No.of.times.30.DPD.or.worse.in.last.6.months+
                      Avgas.CC.Utilization.in.last.12.months+
                      No.of.PL.trades.opened.in.last.12.months+
                      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                      Presence.of.open.home.loan+
                      Total.No.of.Trades+
                      Presence.of.open.auto.loan,
                    family = "binomial", data = train_bal
)

summary(model_14_bal) 

vif(model_14_bal)


#Excluding   No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.

model_15_bal <- glm(formula = `Loan Performance`~ 
                      Profession.xSE+
                      Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.6.10.+
                      Total.No.of.Trades.bucket.x.10.14.+
                      Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                      Income.bucket.x.15.25.+
                      Income.bucket.x.25.40.+
                      bal_income_ratio.bucket.x.3.8.+
                      No.of.dependents+
                      Income+
                      No.of.months.in.current.residence+
                      No.of.times.30.DPD.or.worse.in.last.6.months+
                      Avgas.CC.Utilization.in.last.12.months+
                      No.of.PL.trades.opened.in.last.12.months+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                      Presence.of.open.home.loan+
                      Total.No.of.Trades+
                      Presence.of.open.auto.loan,
                    family = "binomial", data = train_bal
)

summary(model_15_bal) 

vif(model_15_bal)


#Excluding Total.No.of.Trades   

model_16_bal <- glm(formula = `Loan Performance`~ 
                      Profession.xSE+
                      Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.3.6.+
                      Total.No.of.Trades.bucket.x.6.10.+
                      Total.No.of.Trades.bucket.x.10.14.+
                      Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                      Income.bucket.x.15.25.+
                      Income.bucket.x.25.40.+
                      bal_income_ratio.bucket.x.3.8.+
                      No.of.dependents+
                      Income+
                      No.of.months.in.current.residence+
                      No.of.times.30.DPD.or.worse.in.last.6.months+
                      Avgas.CC.Utilization.in.last.12.months+
                      No.of.PL.trades.opened.in.last.12.months+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                      Presence.of.open.home.loan+
                      Presence.of.open.auto.loan,
                    family = "binomial", data = train_bal
)

summary(model_16_bal) 

vif(model_16_bal)



##Excluding    
#Presence.of.open.home.loan,
#No.of.dependents,
#Income.bucket.x.25.40.,Total.No.of.Trades.bucket.x.10.14.,Total.No.of.Trades.bucket.x.6.10.,
#Total.No.of.Trades.bucket.x.3.6.
#Income.bucket.x.15.25.
#No.of.dependents
#Presence.of.open.home.loan

model_17_bal <- glm(formula = `Loan Performance`~ 
                      Profession.xSE+
                      Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                      Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
                      bal_income_ratio.bucket.x.3.8.+
                      Income+
                      No.of.months.in.current.residence+
                      No.of.times.30.DPD.or.worse.in.last.6.months+
                      Avgas.CC.Utilization.in.last.12.months+
                      No.of.PL.trades.opened.in.last.12.months+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                      Presence.of.open.auto.loan,
                    family = "binomial", data = train_bal
)

summary(model_17_bal) 

vif(model_17_bal)


#excluding
#Outstanding.Balance.bucket.x.2.3e.05.3e.06.+
#No.of.months.in.current.residence+
#Income+

model_18_bal <- glm(formula = `Loan Performance`~ 
                      Profession.xSE+
                      Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                      bal_income_ratio.bucket.x.3.8.+
                      No.of.months.in.current.residence+
                      No.of.times.30.DPD.or.worse.in.last.6.months+
                      Avgas.CC.Utilization.in.last.12.months+
                      No.of.PL.trades.opened.in.last.12.months+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
                      Presence.of.open.auto.loan,
                    family = "binomial", data = train_bal
)

summary(model_18_bal) 

vif(model_18_bal)
##excluding
#Profession.xSE+
#bal_income_ratio.bucket.x.3.8.+
#No.of.months.in.current.residence+
#  Presence.of.open.auto.loan,



model_19_bal <- glm(formula = `Loan Performance`~ 
                      Avgas.CC.Utilization.in.last.12.months.bucket.x.25.75.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.1.3.+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.bucket.x.3.6.+
                      No.of.times.30.DPD.or.worse.in.last.6.months+
                      Avgas.CC.Utilization.in.last.12.months+
                      No.of.PL.trades.opened.in.last.12.months+
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
                    family = "binomial", data = train_bal
)

summary(model_19_bal) 

vif(model_19_bal)


########################################################################
# With 7 significant variables in the model

final_model_bal<- model_19_bal

#######################################################################


#######################################################################
### Model Evaluation
library(e1071)
library(caret)

test_pred_bal = predict(final_model_bal, type = "response",test[,-64])

test_actual_bal <- factor(ifelse(test$`Loan Performance`==1,"Yes","No"))

length(test_actual_bal)
length(test_pred_bal)
summary(test_pred_bal)
### lets say cut off probability is 45%
test_pred_default_bal <- factor(ifelse(test_pred_bal >= 0.15,"Yes","No"))
table(test_pred_default_bal,test_actual_bal)
####Very bad accuracy numbers######
#### Bad Model#### Rejected
######Part-3 LR model building and valuation on balanced data ends here##########################

##################################################part 4- WOE based Logistic Regression Model##############


##Logistic Regression on the table replaced by WOE values##

lr_data<- merged_data_NA

###Create WOE Table and dataframe#####


##### use woe_final_standard to run logistic regression

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(woeBinning)

binning <- woe.binning(lr_data,'Performance.Tag.x',lr_data)
new_df <- woe.binning.deploy(lr_data,binning,add.woe.or.dum.var = 'woe')
View(new_df)
ncol(new_df)
woe_df_final <- new_df[,c(12,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97)]
ncol(woe_df_final)
View(woe_df_final)
summary(woe_df_final)
str(woe_df_final)

# Standardise WOE values
woe_final_standard <- data.frame(sapply(woe_df_final[,-c(1)], function(x){scale(x)}))



# merge the target variable performance

woe_final_standard <- cbind(woe_df_final$Performance.Tag.x,woe_final_standard)
colnames(woe_final_standard)[1] <- 'Loan Performance'
View(woe_final_standard)
str(woe_final_standard)
library(caTools)
library(heuristica)

### Splitting data into traing and test data##

set.seed(90)
indices <- sample.split(woe_final_standard$`Loan Performance`,SplitRatio = 0.7)

traindata_woe <- woe_final_standard[indices,]
testdata_woe <- woe_final_standard[!(indices),]

###STart building Logistic Regression on data replaced by WOE values########
##Initial Model

model_1_woe = glm(`Loan Performance` ~ ., data = traindata_woe, family = "binomial")
summary(model_1_woe) # Null deviance: 15334  on 44223  degrees of freedom Residual deviance: 14506  on 44160  degrees of freedom
##AIC: 16388



#Remove all the columns that has p-value >0.5 and hence highly insignificant

traindata_1_woe <- traindata_woe [,-c(2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	22,	30,	31,	32,	37,	41,	42,	46,	49,	50,	59,	60,	62,	63)]
testdata_1_woe <- testdata_woe [,-c(2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	22,	30,	31,	32,	37,	41,	42,	46,	49,	50,	59,	60,	62,	63)]

model_2_woe <- glm(`Loan Performance`~ ., data = traindata_1_woe, family = "binomial")
summary(model_2_woe)

#stepaic
library("MASS")
model_3_woe <- stepAIC(model_2_woe,direction= "both")
summary(model_3_woe)

### Remove Total.No.of.Trades.bucket.x.6.10.,No.of.times.60.DPD.or.worse.in.last.6.months,Total.No.of.Trades,No.of.times.30.DPD.or.worse.in.last.6.months,No.of.trades.opened.in.last.6.months,No.of.trades.opened.in.last.12.months.bucket.x.2.5.,Total.No.of.Trades.bucket.x.3.6.,No.of.trades.opened.in.last.12.months.bucket.x.10.28.



model_4_woe<- glm(`Loan Performance`~ woe.No.of.months.in.current.company.binned+	
                    woe.Income.bucket.binned+	
                    woe.No.of.times.90.DPD.or.worse.in.last.12.months.binned+	
                    woe.Avgas.CC.Utilization.in.last.12.months.binned+	
                    woe.Avgas.CC.Utilization.in.last.12.months.bucket.binned+	
                    woe.Income.binned+	woe.Profession.binned+	
                    woe.No.of.trades.opened.in.last.12.months.binned+	
                    woe.No.of.times.30.DPD.or.worse.in.last.12.months.binned+	
                    woe.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..binned,family = "binomial",data=woe_final_standard
)


summary(model_4_woe) 

# Remove woe.Income.bucket.binned

model_5_woe<- glm(`Loan Performance`~ woe.No.of.months.in.current.company.binned+	
                    woe.No.of.times.90.DPD.or.worse.in.last.12.months.binned+	
                    woe.Avgas.CC.Utilization.in.last.12.months.binned+	
                    woe.Avgas.CC.Utilization.in.last.12.months.bucket.binned+	
                    woe.Income.binned+	woe.Profession.binned+	
                    woe.No.of.trades.opened.in.last.12.months.binned+	
                    woe.No.of.times.30.DPD.or.worse.in.last.12.months.binned+	
                    woe.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..binned,family = "binomial",data=woe_final_standard
)


summary(model_5_woe) 


# Remove woe.Income.binned

model_6_woe<- glm(`Loan Performance`~ woe.No.of.months.in.current.company.binned+	
                    woe.Avgas.CC.Utilization.in.last.12.months.binned+	
                    woe.Avgas.CC.Utilization.in.last.12.months.bucket.binned	
                  +woe.Profession.binned+	
                    woe.No.of.trades.opened.in.last.12.months.binned+	
                    woe.No.of.times.30.DPD.or.worse.in.last.12.months.binned+	
                    woe.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..binned,family = "binomial",data=woe_final_standard
)


summary(model_6_woe) 


# Remove woe.No.of.months.in.current.company.binned+woe.Profession.binned+	 woe.Avgas.CC.Utilization.in.last.12.months.bucket.binned

model_7_woe<- glm(`Loan Performance`~ 	
                    woe.Avgas.CC.Utilization.in.last.12.months.binned+	
                    woe.No.of.trades.opened.in.last.12.months.binned+	
                    woe.No.of.times.30.DPD.or.worse.in.last.12.months.binned+	
                    woe.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..binned,family = "binomial",data=woe_final_standard
)


summary(model_7_woe) 



######final model with 4 significant variables###

final_model_woe <- model_7_woe

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data 

test_pred_woe = predict(final_model_woe, type = "response", newdata = testdata_woe[,-1])


# Let's see the summary 

summary(test_pred)
length(test_pred_woe)

testdata_woe$prob <- test_pred_woe

# Let's use the probability cutoff of 10%.

test_pred_default <- factor(ifelse(test_pred_woe >= 0.04, "Yes", "No"))
test_actual_default <- factor(ifelse(testdata_woe$`Loan Performance`==1,"Yes","No"))


table(test_actual_default,test_pred_default)




#########################################################################################
# Let's Choose the cutoff value. 


# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_default <- factor(ifelse(test_pred_woe >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.15 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred_woe)

s = seq(.01,.20,length=100) 

s
OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,0.2,length=5),seq(0,0.2,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
View(OUT)
 
cutoff_woe <- s[which(abs(OUT[,1]-OUT[,2])<0.04)]

cutoff_woe
# Let's choose a cutoff value of 0.050 for final model

test_cutoff_default <- factor(ifelse(test_pred_woe >=0.0503, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc # 61.5%

sens #61.2%

spec #61.5%

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_default <- factor(ifelse(test_cutoff_default=="Yes",1,0))
test_actual_default <- factor(ifelse(test_actual_default=="Yes",1,0))

str(test_cutoff_default)
str(test_actual_default)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_default, test_actual_default)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
##### K-statistcs is 0.28  much higher than the models from part 2 & 3###
#### While accuracy metrics are same in all 3 models, k-statistics i highest on LR model run on the WOE values####

#### WOE based LR model will be the preferred model here#######

###### Part 4 ends here #############################################################################

##### Part 5 - Analyze the rejected candidates #####################################################


### prepare Rejected Applicants dataset for LR models
merged_data_NA_1 <- merged_data[which(is.na(merged_data$Performance.Tag.x)),]
rejected_app <- merged_data_NA_1
rejected_app <- rejected_app[,-12]
rejected_app <- rejected_app[(rowSums(is.na(rejected_app)))==0,]
str(rejected_app)
nrow(rejected_app)
ncol(rejected_app)
 


#Bucket categorical variables & contnuous variables
rejected_cat <- rejected_app[, c(1,3,4,7,8,9,30,31,32,34,35,36,33)]
rejected_continuous <-  rejected_app[,-c(3,4,7,8,9,30,31,32,34,35,36,33)]
  



# Standardise continuous variables
rejected_continuous <- data.frame(sapply(rejected_continuous[,-1], function(x){scale(x)}))

#Create dummy varaibles for all the categorical variables
rejected_dummy <- data.frame (sapply(rejected_cat[,-1],function(x)data.frame(model.matrix(~x-1,data=rejected_cat))[,-1]))

View(rejected_dummy)  

# Add cat_dummy and scaled continuous

rejected_final <- cbind(rejected_dummy,rejected_continuous)



###### REjected applications dataframe ready to predict performance based on part 2 models#######
### Apply part 2 logistic regression model and predict if any rejected applicant could have been approved####
test_pred_rejected <- predict(model_8,newdata= rejected_final,type = "response")
summary(test_pred_rejected)
## cut of probability for part 2 model was 0.044
rejected_final$prob <- test_pred_rejected
rejected_final$LoanPerformance <- factor(ifelse(rejected_final$prob >= 0.044, "YES","NO"))  
table(rejected_final$LoanPerformance)



### All the rejected applicant would have been rejected by the part-2 model also#####


#######Rejected applicants analysis using WOE based Logistic regression model####################

#### Prepare the data set
library(woeBinning)
binning_reject <- woe.binning(merged_data,'Performance.Tag.x',merged_data)
new_df_reject <- woe.binning.deploy(merged_data,binning,add.woe.or.dum.var = 'woe')
nrow(new_df_reject)
woe_df_final_reject <- new_df_reject[,c(12,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97)]
woe_df_final_reject <- woe_df_final_reject[which(is.na(woe_df_final_reject$Performance.Tag.x)),]

# Standardise WOE values
woe_final_standard_reject <- data.frame(sapply(woe_df_final_reject[,-c(1)], function(x){scale(x)}))

View(woe_final_standard_reject)

### Rejected applicant data converted to WOE for WOE models####
test_predict_woe <- predict(model_5_woe,type="response",newdata= woe_final_standard_reject)
length(test_predict_woe)


### Cut off probability chosen for WOE is 0.0503####
woe_final_standard_reject$LoanPerformance <- factor(ifelse(test_predict_woe>= 0.0503,"Yes","No"))
table(woe_final_standard_reject$LoanPerformance)

#### Basis the cutoff probability , WOE based model would have rejected all the applicants######