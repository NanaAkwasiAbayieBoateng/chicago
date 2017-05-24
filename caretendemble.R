
#==================================================================
# Install packages not already installed in a list
#==================================================================

rm(list=ls())



list=c("tidyverse","stringr","forcats","ggmap","rvest","tm","SnowballC","dplyr","calibrate","doParallel",
       "stringi","ggplot2","maps","httr","rsdmx","devtools","plyr","dplyr","ggplot2","caret","elasticnet",
       "magrittr","broom","glmnet","Hmisc",'knitr',"RSQLite","RANN","lubridate","ggvis","plotly","lars",
       "ggcorrplot","GGally","ROCR","lattice","car","corrgram","ggcorrplot","parallel","readxl","ggmosaic",
       "vcd","Amelia","d3heatmap","ResourceSelection","ROCR","plotROC","DT","aod","mice","Hmisc","data.table",
       "mlbench","pROC","rpart","caretEnsemble","randomForest","nnet")



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




#===================================================================================
# multiple models
#===================================================================================
# feature.names=names(trainSet)
# 
# for (f in feature.names) {
#   if (class(trainSet[[f]])=="factor") {
#     levels <- unique(c(trainSet[[f]]))
#     trainSet[[f]] <- factor(trainSet[[f]],
#                          labels=make.names(levels))
#   }
# }

#levels(trainSet$Hypertension) <- make.names(levels(factor(trainSet$Hypertension)))

mcontrol <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",classProbs=TRUE,allowParallel = TRUE,
  index=createResample(trainSet[,outcomeName], 25),
  summaryFunction=twoClassSummary
)

# oherwise error with names of variables

#response=if_else(trainSet$Hypertension==1,"yes","no")

#trainSet[] <- lapply(trainSet, factor)
#"LogitBoost", 'xgbTree', 'rf', 'svmRadial'

meth=Hmisc::Cs(AdaBag,LogitBoost,xgbTree)

model_l <- caretList(
  trainSet[,predictor],trainSet[,outcomeName],
  trControl=my_control,metric="ROC",
  methodList=meth
)

 
str(trainSet)

str(training)
 
class(trainSet)

class(training)


#===================================================================================
# sonar
#===================================================================================

  #Adapted from the caret vignette
  library("caret")
  library("mlbench")
  library("pROC")
  data(Sonar)
  set.seed(107)
  inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
  training <- Sonar[ inTrain,]
  testing <- Sonar[-inTrain,]
  my_control <- trainControl(
    method="boot",
    number=25,
    savePredictions="final",
    classProbs=TRUE,
    index=createResample(training$Class, 25),
    summaryFunction=twoClassSummary
  )

 
  library("rpart")
  library("caretEnsemble")
  model_list <- caretList(
    Class~., data=training,
    trControl=my_control,metric="ROC",
    methodList=c("glm", "rpart")
  )
  
  
  p <- as.data.frame(predict(model_list, newdata=head(testing)))
  print(p)
  
  
  
  library("mlbench")
  library("randomForest")
  library("nnet")
  model_list_big <- caretList(
    Class~., data=training,
    trControl=my_control,
    metric="ROC",
    methodList=c("glm", "rpart"),
    tuneList=list(
      rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
      rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
      nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
    )
  )
  
  #===================================================================================
  # ensemble
  #===================================================================================
  
  xyplot(resamples(model_list))
  
  
  modelCor(resamples(model_list))
  
  
  greedy_ensemble <- caretEnsemble(
    model_list, 
    metric="ROC",
    trControl=trainControl(
      number=2,
      summaryFunction=twoClassSummary,
      classProbs=TRUE
    ))
  summary(greedy_ensemble)
  
  
  library("caTools")
  model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
  model_preds <- lapply(model_preds, function(x) x[,"M"])
  model_preds <- data.frame(model_preds)
  ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
  model_preds$ensemble <- ens_preds
  caTools::colAUC(model_preds, testing$Class)
  
  varImp(greedy_ensemble)
  
  
  
  #===================================================================================
  # stacking
  #===================================================================================
  
  glm_ensemble <- caretStack(
    model_list,
    method="glm",
    metric="ROC",
    trControl=trainControl(
      method="boot",
      number=10,
      savePredictions="final",
      classProbs=TRUE,
      summaryFunction=twoClassSummary
    )
  )
  model_preds2 <- model_preds
  model_preds2$ensemble <- predict(glm_ensemble, newdata=testing, type="prob")
  CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
  colAUC(model_preds2, testing$Class)
  
  
  CF/sum(CF)
  
  
  
  library("gbm")
  gbm_ensemble <- caretStack(
    model_list,
    method="gbm",
    verbose=FALSE,
    tuneLength=10,
    metric="ROC",
    trControl=trainControl(
      method="boot",
      number=10,
      savePredictions="final",
      classProbs=TRUE,
      summaryFunction=twoClassSummary
    )
  )
  model_preds3 <- model_preds
  model_preds3$ensemble <- predict(gbm_ensemble, newdata=testing, type="prob")
  colAUC(model_preds3, testing$Class)
  
  
  #===================================================================================
  # tutorial
  #===================================================================================
  
  ## Not run:
  set.seed(42)
  models <- caretList(
    iris[1:50,1:2],
    iris[1:50,3],
    trControl=trainControl(method="cv"),
    methodList=c("glm", "rpart"))
  ens <- caretEnsemble(models)
  autoplot(ens)