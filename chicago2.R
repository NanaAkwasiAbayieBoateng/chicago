
#==================================================================
# Install packages not already installed in a list
#==================================================================

rm(list=ls())

list=c("tidyverse","stringr","forcats","ggmap","rvest","tm","SnowballC","dplyr","calibrate","doParallel",
       "stringi","ggplot2","maps","httr","rsdmx","devtools","plyr","dplyr","ggplot2","caret","elasticnet",
       "magrittr","broom","glmnet","Hmisc",'knitr',"RSQLite","RANN","lubridate","ggvis","plotly","lars",
       "ggcorrplot","GGally","ROCR","lattice","car","corrgram","ggcorrplot","sqldf","parallel","readxl","ggmosaic",
       "vcd","Amelia","d3heatmap","ResourceSelection","ROCR","plotROC","DT","aod","mice","Hmisc","data.table")


list=c("tidyverse","stringr","forcats","ggmap","rvest","tm","SnowballC","dplyr","calibrate","doParallel",
       "stringi","ggplot2","maps","httr","rsdmx","devtools","plyr","dplyr","ggplot2","caret","elasticnet",
       "magrittr","broom","glmnet","Hmisc",'knitr',"RSQLite","RANN","lubridate","ggvis","plotly","lars",
       "ggcorrplot","GGally","ROCR","lattice","car","corrgram","ggcorrplot","parallel","readxl","ggmosaic",
       "vcd","Amelia","d3heatmap","ResourceSelection","ROCR","plotROC","DT","aod","mice","Hmisc","data.table")



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






chicago=read_csv("C:/Users/Gucci148/Documents/DataMiningscience/UICProject/Workbook3.csv")

chicago=fread("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/UICProject/Workbook3.csv")

colnames(chicago)<-c("ID","Gender","Age","Hypertension","Employment","Hispanic","Education","Hispanic_ancestry",
                  "race_white","race_black","race_Asian","race_pacific","race_Amer_Indian","race_other",
                  "race_dontknow","race_refused","Asian_ancestry")

head(chicago)

str(chicago)


#==================================================================
#check the number of missing rows
#==================================================================

colSums(is.na.data.frame(chicago))



#==================================================================
#NO CODED RESPONSE APPLICABLE (SPECIFY)
#==================================================================

#data%>%dplyr::filter(str_detect(Hypertension, "NO CODED RESPONSE APPLICABLE (SPECIFY)"))

chicago=chicago%>%dplyr::select(-ID)%>%dplyr::filter(Hypertension!="NO CODED RESPONSE APPLICABLE (SPECIFY)",Employment
                                                !="NO CODED RESPONSE APPLICABLE (LEAVE NOTE FIRST)"
                                                ,Hispanic!="NO CODED RESPONSE APPLICABLE (SPECIFY)",
                                                Education!="NO CODED RESPONSE APPLICABLE (SPECIFY)")


chicago=mutate(chicago,Gender=as.factor(Gender),Age=as.numeric(Age),Hypertension=as.factor(Hypertension),Employment=as.factor(Employment),
             Hispanic=as.factor(Hispanic),Education=as.factor(Education),Hispanic_ancestry=as.factor(Hispanic_ancestry),
             race_white=as.factor(race_white),race_black=as.factor(race_black),race_Asian=as.factor(race_Asian),
             race_pacific=as.factor(race_pacific),race_Amer_Indian=as.factor(race_Amer_Indian),race_other=as.factor(race_other),
             race_dontknow=as.factor(race_dontknow),race_refused=as.factor(race_refused),Asian_ancestry=as.factor(Asian_ancestry) )



chicago=chicago%>%dplyr::select(-race_pacific ,-Asian_ancestry)




#======================================================================================

#Imputing missing values using KNN.Also centering and scaling numerical columns
#the imputation can not work in rows where both your predictors are NA's.
#Error in FUN(newX[, i], ...) : 
#  cannot impute when all predictors are missing in the new data point
#======================================================================================



preProcValues <- preProcess(chicago, method = c("knnImpute","center","scale"))

train_processed <- predict(preProcValues, chicago)

sum(is.na(train_processed))

str(chicago)


#==================================================================
#mice package works 
#==================================================================

#install.packages("mice")
#install.packages("VIM")
library(mice)
library(VIM)

imputeData <- mice(chicago,m=5,maxit=50,meth='pmm',seed=500)



sum(is.na(imputeData))

str(imputeData)

colSums(is.na.data.frame(imputeData[[2]]))




md.pattern(chicago)

aggr_plot <- aggr(chicago, col=c('navyblue','red'), numbers=TRUE, 
                  sortVars=TRUE, labels=names(chicago), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

marginplot(chicago[c("Age")])
# The margin plot of the pairs can be plotted using VIM package as
#marginplot(nhanes[, c("chl", "bmi")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

#==================================================================
#delete 
#==================================================================


chicago=chicago[complete.cases(chicago),]

sum(is.na(chicago))



#==================================================================

#Converting outcome variable to numeric
#==================================================================

chicago$Hypertension<-ifelse(chicago$Hypertension=='No',0,1)



str(chicago)



#==================================================================
#convert categorical variables  to numeric variables
#==================================================================


dmy <- dummyVars(" ~ .", data = chicago,fullRank = T)

transformed <- data.frame(predict(dmy, newdata = chicago))


#Checking the structure of transformed train file
str(transformed)


#==================================================================
#Converting the dependent variable back to categorical
#==================================================================

transformed$Hypertension<-as.factor(transformed$Hypertension)



#==================================================================
#Spliting training set into two parts based on outcome: 70% and 30%
#==================================================================

index <- createDataPartition(transformed$Hypertension,p=0.70, list=FALSE)

trainSet <- transformed[ index,]

testSet <- transformed[-index,]



#==================================================================
#Feature selection using rfe in caret(recursive feature extraction)
#predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
#Alternatively
#predictors<-setdiff(names(trainSet),outcomeName)
#==================================================================
# 



# ctrl <- rfeControl(functions = lmFuncs,
#                    method = "repeatedcv",
#                    repeats = 5,
#                    verbose = FALSE)
# 
# 
# pred<-setdiff(names(trainSet),outcomeName)
# 
# feature_select1 <- rfe(trainSet[,-3], trainSet[,outcomeName],
#                        rfeControl = ctrl )

#install.packages('randomForest', dependencies=TRUE)
#install.packages('caret', dependencies = TRUE)
library(randomForest)

control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
outcomeName<-'Hypertension'

predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]

feature_select <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)





table(trainSet$Hypertension)


feature_select


names(feature_select)



predictors(feature_select)

summary(feature_select)




#===================================================================================
#Taking only the top 5 predictors
#Age, Employment.Primarily.retired..or, Education.10th.Grade, 
#Employment.Unable.to.work., race_black.Yes from feature_select
# Cs function from Hmisc converts names to character variables

#===================================================================================



library(Hmisc)

predictor=Hmisc::Cs( Age, Employment.Primarily.retired..or, Employment.Unable.to.work., race_white.Yes, Employment.Self.employed.)


#===================================================================================
# plot variable selection
#===================================================================================


trellis.par.set(caretTheme())

plot(feature_select, type = c( "o","g"))

#===================================================================================
# Model Building
# as.factor(trainSet$Hypertension) to do classifcation/logistic regression
#===================================================================================



#===================================================================================
# logistic regression
#===================================================================================

model_glm<-train(trainSet[,predictor],as.factor(trainSet$Hypertension),method='glm',family="binomial")


summary(model_glm)

str(trainSet)




# Predict using the test data
pred<-predict(model_glm,testSet)

my_data=data.frame(cbind(predicted=pred,observed=testSet$Hypertension))

ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('logistic model')

# Print, plot variable importance
print(varImp(model_glm, scale = FALSE))

plot(varImp(model_glm, scale = FALSE), main="Variable Importance using logistic/glm")


confusionMatrix(testSet$Hypertension,pred)



#==================================================================
#ROCR Curve
#==================================================================




p <- ggplot(data.frame(trainSet), aes(d = as.numeric(Hypertension), m = Age)) + geom_roc()+ style_roc()


plot_interactive_roc(p)





# Mean Squared Error

sqrt(mean(as.numeric(pred)-as.numeric(testSet$Hypertension))^2)

table(pred,testSet$Hypertension)

class(testSet$Hypertension)
class(pred)
#===================================================================================
# gradient boosted machines
#===================================================================================

model_gbm<-train(trainSet[,predictor],trainSet[,outcomeName],method='gbm')

summary(model_gbm)

# Predict using the test data
pred<-predict(model_gbm,testSet)
pred
confusionMatrix(testSet$Hypertension,pred)

#===================================================================================
# parameter tuning
#===================================================================================

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)
modelLookup(model='gbm')
#Creating grid
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))
# training the model
model_gbm<-train(trainSet[,predictor],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneGrid=grid)
# summarizing the model
print(model_gbm)


plot(model_gbm)



#using tune length
model_gbm<-train(trainSet[,predictor],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=10)
print(model_gbm)



pred<-predict(model_gbm,testSet)


ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('gbm')

# Print, plot variable importance
print(varImp(model_gbm, scale = FALSE))

plot(varImp(model_gbm, scale = FALSE), main="Variable Importance using logistic/gbm")


confusionMatrix(testSet$Hypertension,pred)


plot(model_gbm)






(plot(model_gbm, plotType = "level"))

(resampleHist(model_gbm))


plot(model_gbm)



#===================================================================================
#Extreme gradient boosted machines
# xgboost
#===================================================================================

#install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
#install.packages("xgboost")
#install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
library(xgboost)


sparse_matrix <- sparse.model.matrix(Hypertension ~ .-1, data = transformed)

xgb_train_matrix <- xgb.DMatrix(data = as.matrix(trainSet[,predictor]), label = if_else(trainSet[,outcomeName]==1,0,1))

xgb_test_matrix <- xgb.DMatrix(data = as.matrix(testSet[,predictor]), label = if_else(testSet[,outcomeName]==1,0,1))


watchlist <- list(train = xgb_train_matrix, test = xgb_test_matrix)

label <- getinfo(xgb_test_matrix, "label")



param <- list("objective" = "binary:logistic")

xgb.cv(param = param, 
       data = xgb_train_matrix, 
       nfold = 3,
       label = getinfo(xgb_train_matrix, "label"),
       nrounds = 5)

bst_1 <- xgb.train(data = xgb_train_matrix, 
                   label = getinfo(xgb_train_matrix, "label"),
                   max.depth = 2, 
                   eta = 1, 
                   nthread = 4, 
                   nround = 50, # number of trees used for model building
                   watchlist = watchlist, 
                   objective = "binary:logistic")


features = colnames(as.matrix(trainSet[,predictor]))
importance_matrix_1 <- xgb.importance(features, model = bst_1)
print(importance_matrix_1)


#install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
xgb.ggplot.importance(importance_matrix_1) +
  theme_minimal()




pred_1 <- predict(bst_1, xgb_test_matrix)

result_1 <- data.frame(
                       outcome =testSet$Hypertension, 
                       label = label, 
                       prediction_p_death = round(pred_1, digits = 2),
                       prediction = as.integer(pred_1 > 0.5),
                       prediction_eval = ifelse(as.integer(pred_1 > 0.5) != label, "wrong", "correct"))
result_1



#===================================================================================
#Extreme gradient boosted machines
# caret
#===================================================================================

train(trainSet[,predictor],trainSet[,outcomeName], method='xgbTree')


#===================================================================================
# random forest
#===================================================================================

model_rf<-train(trainSet[,predictor],trainSet[,outcomeName],method='rf')

summary(model_rf)


pred<-predict(model_rf,testSet)


ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('rf')

# Print, plot variable importance
print(varImp(model_rf, scale = FALSE))

plot(varImp(model_rf, scale = FALSE), main="Variable Importance of rf")


confusionMatrix(testSet$Hypertension,pred)



ctrl <- trainControl(method = "CV",
                     number=10,
                     classProbs = TRUE,
                     allowParallel = TRUE,
                     summaryFunction = twoClassSummary)

# oherwise error with names of variables

response=if_else(trainSet$Hypertension==1,"yes","no")
  
rfFit <- train(trainSet[,predictor],response,
               method = "rf",
               tuneGrid = expand.grid(.mtry = seq(4,20,by=2)),
               ntrees=1000,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)


pred <- predict.train(rfFit, newdata = testSet, type = "prob") 


# Print, plot variable importance
print(varImp(rfFit, scale = FALSE))

plot(varImp(rfFit, scale = FALSE), main="Variable Importance of rf")

response1=if_else(testSet$Hypertension==1,"yes","no")

confusionMatrix(testSet$Hypertension,pred)

#===================================================================================
# neural betwork
#===================================================================================

model_nnet<-train(trainSet[,predictor],trainSet[,outcomeName],method='nnet')

summary(model_nnet)



pred<-predict(model_nnet,testSet)


ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('nn')

# Print, plot variable importance
print(varImp(model_nnet, scale = FALSE))

plot(varImp(model_nnet, scale = FALSE), main="Variable Importance of nn")


confusionMatrix(testSet$Hypertension,pred)



#===================================================================================
# partial least squares
#===================================================================================


model_pls<-train(trainSet[,predictor],trainSet[,outcomeName],method='pls', tuneLength = 15)

plot(model_pls)


# Predict using the test data
pred<-predict(model_pls,testSet,type = "prob")

dim(testSet)
dim(trainSet)

my_data=data_frame(cbind(predicted=pred,observed=testSet$Hypertension))

ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('pls')

# Print, plot variable importance
print(varImp(model_glm, scale = FALSE))

plot(varImp(model_glm, scale = FALSE), main="Variable Importance using pls")


confusionMatrix(testSet$Hypertension,pred)



#===================================================================================
# support vector machines
#===================================================================================


model_svm<-train(trainSet[,predictor],trainSet[,outcomeName],method='svmLinearWeights2')

plot(model_svm)


# Predict using the test data
pred<-predict(model_svm,testSet,type = "prob")



#===================================================================================
# multiple Algorithms at the same time
#===================================================================================

mat = lapply(c("LogitBoost", 'xgbTree', 'rf', 'svmRadial'), 
             function (met) {
               train(trainSet[,predictor],trainSet[,outcomeName], method=met)
             })



mat[[1]][[4]]["Accuracy"]
mat[[2]][[4]]["Accuracy"]
mat[[3]][[4]]["Accuracy"]
mat[[4]][[4]]["Accuracy"]



c=cbind(mat[[1]][[4]]["Accuracy"],mat[[3]][[4]]["Accuracy"],
                 mat[[4]][[4]]["Accuracy"])
names(c)=c("LogitBoost", "rf", "svmRadial")

c=reshape::melt(c)

names(c)=c("Algorithm", "Accuracy")

fill <- "#4271AE"
line <- "#1F3552"

#install.packages("plotly")
#devtools::install_github('hadley/ggplot2')
library(plotly)
p <- ggplot(c,aes(x =Algorithm, y = Accuracy)) +geom_boxplot(fill = fill, colour = line)+ theme_bw()
ggplotly(p)


library(plotly)
packageVersion('plotly')
p2 <- plot_ly(c, y = ~Accuracy, color = ~Algorithm, type = "box")
p2

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,classProbs = TRUE,
  repeats = 5,allowParallel = TRUE)

modelLookup(model='gbm')

meth=Hmisc::Cs(AdaBag,logicBag,LogitBoost,C5.0,deepboost,xgbLinear,xgbTree,
               kernelpls,rbf,RRF,svmPoly)


model_mult<-train(trainSet[,predictor],trainSet[,outcomeName],method="logicBag",trControl=fitControl,
                  preProcess = c("center","scale"))


mult<-function(meth){

  model_mult<-train(trainSet[,predictor],trainSet[,outcomeName],method=meth,trControl=fitControl,
                    preProcess = c("center","scale"))  
}


mult("AdaBag")

sapply()

#===================================================================================
# multiple algorithms
#===================================================================================


met=c("LogitBoost", 'xgbTree', 'rf', 'svmRadial')

length(met)

re=list()
for (i in seq_along(met)) {
  
 re[[i]]<-train(trainSet[,predictor],trainSet[,outcomeName],method=met[i],trControl=fitControl,
                    preProcess = c("center","scale"))  
}
re[[1]]