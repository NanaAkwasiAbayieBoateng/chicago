
#==================================================================
# Install packages not already installed in a list
#==================================================================

rm(list=ls())

list=c("tidyverse","stringr","forcats","ggmap","rvest","tm","SnowballC","dplyr","calibrate","doParallel",
       "stringi","ggplot2","maps","httr","rsdmx","devtools","plyr","dplyr","ggplot2","caret","elasticnet",
       "magrittr","broom","glmnet","Hmisc",'knitr',"RSQLite","RANN","lubridate","ggvis","plotly","lars",
       "ggcorrplot","GGally","ROCR","lattice","car","corrgram","ggcorrplot","sqldf","parallel","readxl","ggmosaic",
       "vcd","Amelia","d3heatmap","ResourceSelection","ROCR","plotROC","DT","aod")



list_packages <- list
new.packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

R<-suppressWarnings(suppressMessages(sapply(list, require, character.only = TRUE)))

#==================================================================
#  Set up parallel processing
# leave two cores for operating system
#==================================================================

cluster <- makeCluster(detectCores() - 2) 
registerDoParallel(cluster)




#==================================================================
#display all coumns of data with dplyr
# Print first 1000 rows of dataset
#==================================================================

options(dplyr.width = Inf)

options(dplyr.print_max = 1000)




#==================================================================
# read multiple excel files
# set working directory to  folder that contains excel files
#"C:/Users/Gucci148/Documents/DataMiningscience/UICProject"
#==================================================================




temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)

data=read_csv("C:/Users/Gucci148/Documents/DataMiningscience/UICProject/Workbook1.csv")




colnames(data)<-c("ID","Gender","Age","Hypertension","Employment","Hispanic","Education","Hispanic_ancestry",
                  "race_white","race_black","race_Asian","race_pacific","race_Amer_Indian","race_other",
                  "race_dontknow","race_refused","Asian_ancestry")



data=data[-c(1:9),]

data$Age=as.numeric(data$Age)

head(data)

tail(data,10)
dim(data)

str(data)
names(data)

write_csv(data,"C:/Users/Gucci148/Documents/DataMiningscience/UICProject/Workbook3.csv")

#==================================================================
# Look at  the  structure of the data with the glimpse function in 
#  dplyr  package
#==================================================================

str(newdata)

dplyr::glimpse(data)

summary(data)


#==================================================================
#check the number of missing rows
#==================================================================

colSums(is.na.data.frame(data))


data[!complete.cases(data),]


data[which(data$Asian_ancestry!="."),]


data$Asian_ancestry=ifelse(data$Asian_ancestry==".","<NA>",data$Asian_ancestry)


data=data[complete.cases(data),]

#==================================================================
#NO CODED RESPONSE APPLICABLE (SPECIFY)
#==================================================================

#data%>%dplyr::filter(str_detect(Hypertension, "NO CODED RESPONSE APPLICABLE (SPECIFY)"))

ndata=data%>%dplyr::select(-ID)%>%dplyr::filter(Hypertension!="NO CODED RESPONSE APPLICABLE (SPECIFY)",Employment
                                                  !="NO CODED RESPONSE APPLICABLE (LEAVE NOTE FIRST)"
                                                  ,Hispanic!="NO CODED RESPONSE APPLICABLE (SPECIFY)",
                                                  Education!="NO CODED RESPONSE APPLICABLE (SPECIFY)")



p=xtabs(~Hypertension+Age, data=ndata)
chisq.test(xtabs(~Hypertension+Gender, data=ndata),simulate.p.value=T)
fisher.test(xtabs(~Hypertension+Gender, data=ndata))
chisq.test(xtabs(~Hypertension+Employment, data=ndata),simulate.p.value=T)
chisq.test(xtabs(~Hypertension+ Hispanic, data=ndata),simulate.p.value=T)
xtabs(~Hypertension+Asian_ancestry, data=ndata)




ndata=mutate(ndata,Gender=as.factor(Gender),Age=as.numeric(Age),Hypertension=as.factor(Hypertension),Employment=as.factor(Employment),
       Hispanic=as.factor(Hispanic),Education=as.factor(Education),Hispanic_ancestry=as.factor(Hispanic_ancestry),
       race_white=as.factor(race_white),race_black=as.factor(race_black),race_Asian=as.factor(race_Asian),
       race_pacific=as.factor(race_pacific),race_Amer_Indian=as.factor(race_Amer_Indian),race_other=as.factor(race_other),
       race_dontknow=as.factor(race_dontknow),race_refused=as.factor(race_refused),Asian_ancestry=as.factor(Asian_ancestry) )
 

#newdata%>%select(-race_pacific )%>%glm(Hypertension~., family = "binomial",data=newdata)


#==================================================================
#Machine learning Approach
#==================================================================


#==================================================================
#partition data into training set and test set
#==================================================================

index1=sample(1:dim(ndata)[1],replace=F,round(0.7*dim(ndata)[1]))

trainingdata=ndata[index1,]

testdata=ndata[-index1,]

dim(testdata)

dim(trainingdata)

index <- createDataPartition(ndata$Age, p=0.70, list=FALSE)

trainSet <- ndata[ index,]

testSet <- ndata[-index,]

dim(testSet)

dim(trainSet)











ndata=ndata%>%dplyr::select(-race_pacific )

fit<-glm(Hypertension~Gender+Age+Employment+Hispanic+Education+Hispanic_ancestry+race_white+race_black+
           race_Asian+race_Amer_Indian+race_other+race_dontknow+race_refused+Asian_ancestry
         , family = "binomial",data=  trainSet)


## CIs using profiled log-likelihood

knitr::kable(confint(fit))


## odds ratios only)
knitr::kable(exp(coef(fit)))



## odds ratios and 95% CI
exp(cbind(OR = coef(fit), confint(fit)))




#install.packages("aod")

library(aod)


wald.test(b = coef(fit), Sigma = vcov(fit), Terms = 3:9)


#==================================================================
#stepwise regression
#Feature selection
#==================================================================


both=step(fit, direction = "both",trace=0 )

summary(both)

both$anova

# by default backward

st=step(fit,trace=0)


summary(st)

tidy(st)

augment(both)

#==================================================================
#Evaluating Model Fit regression
#==================================================================

st$anova

#install.packages("ResourceSelection")




hoslem.test(trainSet$Hypertension, fitted(both), g=10)


#==================================================================
#Variable Importance Model Fit regression
#==================================================================

varImp(both)

dat=sort(varImp(both)$Overall,decreasing = T)
importanceOrder=order(-varImp(both)$Overall)
names=rownames(varImp(both))[importanceOrder]
names(dat)=names

barplot(dat,las=2,ylab="",xlab="", horiz=TRUE)
box(lty = '1373', col = 'black')


d=data_frame(varImp(both)$Overall)
row.names(d)
p <- plot_ly(
  y = names,
  x = dat,
  name = "SF Zoo",
  type = "bar"
)
p




plot(varImp(both, scale = FALSE), main="Variable Importance using Logistic Regression")


#==================================================================
#prediction
#==================================================================

newdata1 <- with(ndata, data.frame(Gender=factor(1:2),Age = mean(Age), Employment = factor(1:8),Hispanic=factor(1:2),
               Education=factor(1:23),Hispanic_ancestry=factor(1:5) ,race_white=factor(1:2) ,race_black=factor(1:2) ,race_Asian=factor(1:2) ,
               race_Amer_Indian=factor(1:2),race_other=factor(1:2),race_dontknow=factor(1:2),race_refused=factor(1:2)))


newdata3 <- cbind(testSet , predict(both, newdata = testSet , type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
knitr::kable(head(newdata3))

DT::datatable(newdata3)





##  correct predictions rate for test data

#confusion matrix
predict <- predict(both,testSet, type = 'response')

pp=if_else(predict >0.5,"Yes","No")

tab=table(testSet$Hypertension, pp)



accuracy=sum(diag(tab))/sum(tab)

accuracy


#==================================================================
#confusion matrix
#==================================================================

confusionMatrix(tab)  



#accuracy {SDMTools}
#accuracy(testSet$Hypertension,predict,threshold=0.5)

confusionMatrix(testSet$Hypertension,predict)


##  correct predictions rate for train data

predict1 <- predict(both,trainSet, type = 'response')

pp=if_else(predict1 >0.5,"Yes","No")


tab=table(trainSet$Hypertension, pp)

#accuracy of prediction
acc=(tab[1]+tab[4])/sum(tab)




predict(both, newdata=testSet)


#==================================================================
# predicted covariates
#"link", "response", "terms"
#==================================================================



predict(both, newdata=testSet, type="terms")

#==================================================================
# predicted probability plot
#==================================================================
# This is how our model does on predicting hypertension,For example ,for a person Age 83 and 
#Hypertension=Yes,the model predicts with 92% probability that the person has Hypertension

p=ggplot(newdata3, aes(x = Age, y = PredictedProb,color=Hypertension))+geom_point()+theme_minimal()
ggplotly(p)


ggplot(newdata3, aes(x=Age, y=Hypertension)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)





pred=predict(both, newdata = testSet , type = "link")


pred=predict(both, newdata = trainSet , type = "link")

probs <- exp(pred)/(1+exp(pred))


#confusion matrix
predict <- predict(both, type = 'response')
table(trainSet$Hypertension, pred > 0.5)
table(trainSet$Hypertension, predict > 0.5)





predict <- predict(both,testSet, type = 'response')
hypertesion=if_else(newdata3$Hypertension=="Yes",1,0)

newdata4=newdata3%>%dplyr::mutate(HypertensionN=if_else(Hypertension=="Yes",1,0))


p=ggplot(newdata4, aes(x = HypertensionN, y = PredictedProb))+geom_point()+theme_minimal()
ggplotly(p)

p=if_else(newdata4$predict >0.5,"Yes","No")
confusionMatrix(newdata4$HypertensionN,p)






#==================================================================
#Cross Validation Errors
#==================================================================



ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(Hypertension ~ Age +  Employment + race_white+race_black + race_Amer_Indian + race_other + race_dontknow + 
                   race_refused + Asian_ancestry
                   ,  data=trainSet, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)



pred = predict(mod_fit, newdata=testSet)

confusionMatrix(data=pred, testSet$Hypertension)


#==================================================================
#ROCR Curve
#==================================================================

predict <- predict(both, type = 'response')

length(predict)

dim(trainSet)

ROCRpred <- prediction(predict, trainSet$Hypertension)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

library(plotROC)

#install.packages("plotROC")

# The Roc Geom
# Next I use the ggplot function to define the aesthetics, and 
# the geom_roc function to add an ROC curve layer. 



p <- ggplot(as.data.frame(trainingdata), aes(d = Hypertension, m = Age)) + geom_roc()+ style_roc()


plot_interactive_roc((p))


cat(
  export_interactive_roc(p, 
                         prefix = "a")
)



library(pROC)
#install.packages("pROC")
# Compute AUC for predicting Class with the variable CreditHistory.Critical
f1 = roc(Hypertension ~ Age, data=trainSet) 
plot(f1, col="red")




#==================================================================
#convert categorical variables  to numeric variables
#==================================================================


#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- preProcess(ndata, method = c("knnImpute","center","scale"))
library('RANN')
train_processed <- predict(preProcValues, ndata)
sum(is.na(train_processed))

dmy <- dummyVars(" ~ .", data = ndata,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))



dummies <- dummyVars(Hypertension~ ., data = newdata)
head(predict(dummies, newdata = newdata))


dummies <- dummyVars(survived ~ ., data = etitanic)
head(predict(dummies, newdata = etitanic))
head(model.matrix(factor(Hypertension) ~ ., data = newdata))



#==================================================================
#Exploratory Data Analysis
#==================================================================

p=ggplot(newdata, aes(x=Hypertension , y=Age, fill=Hypertension)) + geom_boxplot() +guides(fill=FALSE) +
   ggtitle("Age vs Hypertension")
ggplotly(p)


p=ggplot(newdata, aes(x = Hypertension, y = Age, fill = Hypertension)) + geom_boxplot() +
  facet_wrap(~ Gender, ncol = 2)+ggtitle("Age vs Hypertension stratified by Gender")
ggplotly(p)



p=ggplot(newdata, aes(x = Hypertension, y = Age, fill = Hypertension)) + geom_boxplot() +
  facet_wrap(~ Gender, ncol = 2)+ggtitle("")
ggplotly(p)

p=ggplot(newdata, aes(x = Hypertension, y = Age, fill = Hypertension)) + geom_boxplot() +
  facet_wrap(~ Hispanic, ncol = 2)+ggtitle("Age vs Hypertension stratified by Hispanaic Race")
ggplotly(p)


p=ggplot(newdata, aes(x = Hypertension, y = Gender, fill = Hypertension)) + geom_boxplot() +
  facet_wrap(~ Employment)
ggplotly(p)


#install.packages("Amelia")
library(Amelia)

missmap(newdata, main = "Missing values vs observed")



p=ggplot(data=newdata) + geom_mosaic(aes( x=product(Gender), fill=Hypertension))
ggplotly(p)


mosaic(xtabs(~Hypertension+Hispanic+Gender, data=newdata), shade=TRUE, legend=TRUE) 

assoc(xtabs(~Hypertension+Hispanic+Gender, data=newdata), shade=TRUE, legend=TRUE)

mosaic(xtabs(~Hypertension+race_white+Gender, data=newdata), shade=TRUE, legend=TRUE
       ,main="Visualizing Hypertension by Gender and Ethnicity") 


mosaic(xtabs(~Hypertension+race_black+Gender, data=newdata), shade=TRUE, legend=TRUE) 

doubledecker(Hypertension ~ race_white + Gender, data=newdata,
             main="Visualizing Hypertension by Gender and Ethnicity")





#==================================================================
#convert character variables  to factor variables
#==================================================================
dat=newdata
character_vars <- lapply(dat, class) == "character"
dat[, character_vars] <- lapply(dat[, character_vars], as.factor)


str(df)

df=newdata
cols.to.factor <- sapply( df, function(col) length(unique(col)) < log10(length(col)) )
df[ cols.to.factor] <- lapply(df[ cols.to.factor] , factor)


df[] <- lapply( df, factor) # the "[]" keeps the dataframe structure
col_names <- names[!(names(df)%in%"Age"),]
# do do it for some names in a vector named 'col_names'
df[col_names] <- lapply(df[col_names] , factor)

DF=newdata
## The conversion
DF[sapply(DF, is.character)] <- lapply(DF[sapply(DF, is.character)], 
                                       as.factor)

class(DF)







dim(data)
ndata=ndata[,-15]
#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- preProcess(ndata, method = c("knnImpute","center","scale"))
library('RANN')
train_processed <- predict(preProcValues, ndata)
sum(is.na(train_processed))


colSums(is.na.data.frame(ndata))





#==================================================================
#National Comparson
#Numbers from CDC
#==================================================================





tab <- as.table(matrix(c(43,45.7,27.8,28.9,33.9,31.3,34.1,32.7),4,2,byrow=T))
dimnames(tab) <- list(Race = c("African Americans","Mexican Americans","Whites","All"),
                      Rate  =c("Men(%)","Women(%)") )
tab



write_csv(ndata,"C:/Users/Gucci148/Documents/DataMiningscience/UICProject/ndata.csv")




table(ndata$Hypertension,ndata$race_white )




sum(table(ndata$Hypertension,ndata$Hispanic))

table(ndata$Hypertension,ndata$race_black  )
102/424


tab <- as.table(matrix(c(43,45.7,27.8,28.9,33.9,31.3,34.1,32.7),4,2,byrow=T))
dimnames(tab) <- list(Race = c("African Americans","Mexican Americans","Whites","All"),
                      Rate  =c("Men(%)","Women(%)") )
tab


ndata%>%dplyr::filter(Hypertension=="Yes",race_black=="Yes",Gender=="Female")%>%summarise(count=n())%>%mutate((count/424)*100)


ndata%>%dplyr::filter(Hypertension=="Yes",race_black=="Yes",Gender=="Male")%>%summarise(count=n())%>%mutate((count/424)*100)



ndata%>%dplyr::filter(Hypertension=="Yes",race_white=="No",Gender=="Female")%>%summarise(count=n())%>%mutate((count/424)*100)%>%mutate((count/424)*100)


ndata%>%dplyr::filter(Hypertension=="Yes",race_white=="No",Gender=="Male")%>%summarise(count=n())%>%mutate((count/424)*100)%>%mutate((count/424)*100)


ndata%>%dplyr::filter(Hypertension=="Yes",race_white=="Yes",Gender=="Female")%>%summarise(count=n())%>%mutate((count/424)*100)%>%mutate((count/424)*100)


ndata%>%dplyr::filter(Hypertension=="Yes",race_white=="Yes",Gender=="Male")%>%summarise(count=n())%>%mutate((count/424)*100)%>%mutate((count/424)*100)



ndata%>%dplyr::filter(Hypertension=="Yes",Hispanic=="Yes",Gender=="Male")%>%summarise(count=n())%>%mutate((count/424)*100)%>%mutate((count/424)*100)


ndata%>%dplyr::filter(Hypertension=="Yes",Hispanic=="Yes",Gender=="Female")%>%summarise(count=n())%>%mutate((count/424)*100)%>%mutate((count/424)*100)



ndata%>%dplyr::filter(Hypertension=="Yes",Gender=="Female")%>%summarise(count=n())%>%mutate((count/424)*100)

ndata%>%dplyr::filter(Hypertension=="Yes",Gender=="Male")%>%summarise(count=n())%>%mutate((count/424)*100)




tab <- as.table(matrix(c(6.34,17.68,2.12,2.36,3.3,3.54,11.8,23.6),4,2,byrow=T))
dimnames(tab) <- list(Race = c("African Americans","Mexican Americans","Whites","All"),
                      Rate  =c("Men(%)","Women(%)") )
tab






Race = c("African Americans","Mexican Americans","Whites","All")
Male=c(6.34,2.1,3.3,11.8)
Female=c(17.69,2.4,3.5,23.6)
data=data_frame(Race,Male,Female)
p <- plot_ly(data, x = ~Race, y = ~Male, type = 'bar', name = 'Male') %>%
  add_trace(y = ~Female, name = 'Female') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')

p



#==================================================================
#stacked bar plots
#==================================================================



White<- c("Yes","No")
Male <- c(6.4, 8.3)
Female<- c(17.69, 20.28)
data <- data.frame(White, Female, Male)

p <- plot_ly(data, x = ~White, y = ~Female, type = 'bar', name = 'Female') %>%
  add_trace(y = ~Male, name = 'Male') %>%
  layout(yaxis = list(title = 'Percentage'), barmode = 'stack')%>%
  layout(title = "Percentage of Whites with Hypertension",
         xaxis = list(title = "White"),
         yaxis = list(title = "Percentage"))
p






#===============================================================================
#  Impute Missing values Model 
#===============================================================================



#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- preProcess(ndata, method = c("knnImpute","center","scale"))
library('RANN')
train_processed <- predict(preProcValues, ndata)
sum(is.na(train_processed))


#===============================================================================
#Converting outcome variable to numeric
#===============================================================================


train_processed$Hypertension<-ifelse(train_processed$Hypertension=='No',0,1)

#==================================================================

#converting categorical data to numeric
#First one Hypertension left out of conversion
# Hypertension and all other variables converted to dummy
#==================================================================


head(model.matrix(Hypertension ~ ., data = ndata))

head(model.matrix(~ ., data = ndata))

#==================================================================
# Creating dummy variables
#==================================================================



dummies <- dummyVars(Hypertension~ ., data = ndata)


transformed=predict(dummies, newdata = ndata)


head(transformed)

str(transformed)




dumm<- dummyVars(~ ., data = ndata)


transform=predict(dumm, newdata = ndata)


head(transform)


str(transform)


#Converting every categorical variable to numerical using dummy variables

dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T)

transforme <- data_frame(predict(dmy, newdata = train_processed))

#Converting the dependent variable back to categorical

transforme$Hypertension<-as.factor(transforme$Hypertension)


str(train_processed)

str(ndata)

sum(is.na(ndata))

#===============================================================================
#  Splitting data
#===============================================================================

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(train_transformed$Loan_Status, p=0.75, list=FALSE)
trainSet <- train_transformed[ index,]
testSet <- train_transformed[-index,]

#===============================================================================
#  Build a  Partial Least Squares Model 
#===============================================================================

# Partial Least Squares is one way to reduce dimension of the predictors used in the model. It identifies
#linear combinations,or directions, that best represent the predictors in the data.The directions are identified in 
# unsupervised way since the outcome variable is not in identifying the principal
# component directions.The predictors are preprocessed by centering and scaling.
# PLS will seek
#directions of maximum variation while simultaneously considering correlation with the response.


head(ndata)

control <- trainControl(method="repeatedcv", number=10, repeats=5)


predictors=setdiff(names(trainSet),trainSet$Hypertension)
outcome=trainSet$Hypertension

plsfit=train(trainSet[,predictors], outcome,
             method = "pls",tuneLength = 20,
             preProc = c("center", "scale"),
             trControl = control)






# Predict using test data


pred<-predict(plsfit,testSet[,predictors])

my_data=data_frame(predicted=pred,observed=testSet$Tip)

ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('PLS')

# Print, plot variable importance
print(varImp(plsfit, scale = FALSE))

plot(varImp(plsfit, scale = FALSE), main="Variable Importance using PLS")

summary(plsfit)

plot(plsfit)


# Mean Squared Error

sqrt(mean(pred- testSet$Tip)^2)
