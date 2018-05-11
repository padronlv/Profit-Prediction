install.packages("dplyr")
#------------------------------load_libraries--------------------------------------
library(readr)
library(caret)
library(corrplot)
library(dplyr)

#look for package for the 4 cores.
#----------------------------------------data_import------------------------------

data_existing_products <- read.csv("existingproductattributes2017.2.csv")
data_new_products <- read.csv("newproductattributes2017.2.csv")

#---------------------------------------data_exploration--------------------------

boxplot((data=data_existing_products), main="Distribution")
ggplot(data_existing_products) + geom_point(aes(x=Volume, y=x5StarReviews))
length(which(is.na(data_existing_products)))

#data_dljdl <- data_clean[which(data_clean$ProductType = "Laptop"),]
#---------------------------------------preprocess---------------------------------
data_existing_products_preclean <- data_existing_products
data_existing_products_preclean$x5StarReviews <- NULL
data_existing_products_preclean$ProductNum <- NULL
data_existing_products_preclean$BestSellersRank <- NULL
ggplot(data_existing_products_preclean) + geom_point(aes(x=Volume, y=PositiveServiceReview))


data_clean <- data_existing_products_preclean[which(data_existing_products_preclean$Volume < 6000),]
ggplot(data_clean, aes(x=Volume, y=x4StarReviews - x1StarReviews)) + geom_point() + stat_smooth(method = "lm")+ facet_grid(ProductType~.)

data_selected_for_plots <- filter(data_clean, ProductType == "Laptop" |ProductType == "PC" | ProductType == "Netbook" |ProductType == "Smartphone")
ggplot(data_selected_for_plots, aes(x=Volume, y=x4StarReviews +x3StarReviews - x2StarReviews - x1StarReviews)) + geom_point() + stat_smooth(method = "lm")+ facet_grid(ProductType~.)
ggplot(data_selected_for_plots, aes(x=Volume, y=PositiveServiceReview - NegativeServiceReview)) + geom_point() + stat_smooth(method = "lm")+ facet_grid(ProductType~.)
ggplot(data_clean, aes(x=Volume, y=x4StarReviews +x3StarReviews - x2StarReviews - x1StarReviews)) + geom_point() + stat_smooth(method = "lm")
ggplot(data_clean, aes(x=PositiveServiceReview - NegativeServiceReview, y=Volume)) + geom_point() + stat_smooth(method = "lm")
ggplot(data_clean, aes(x=Price, y=Volume)) + geom_point() + stat_smooth(method = "lm")


#Boolean conversion
Product_typeboolean <- dummyVars(" ~ .", data = data_clean)
data_boolean <- data.frame(predict(Product_typeboolean, newdata = data_clean))
data_selected <- filter(data_boolean, ProductType.Laptop == 1 |ProductType.PC ==1 | ProductType.Netbook == 1 |ProductType.Smartphone== 1)


#Correlation Matrix
CorMat <- cor(data_boolean)
corrplot(CorMat)




#Detecting NA and eliminating NA
#data_NA <- data_boolean[!complete.cases(data_boolean),]
#data_NA
#data_boolean_clean <- data_boolean[complete.cases(data_boolean),]


#---------PreTrain-----------
#Datapartition
Data_Partition <- createDataPartition(data_selected$Volume, p = .75, list = FALSE)
training <- data_selected[Data_Partition,]
testing <- data_selected[-Data_Partition,]

#10 fold cross validation
Control_RepeatedCV <- trainControl(method = "repeatedcv", number = 2, repeats = 4)
#fitControl <- trainControl(method = "cv", number = 10)


#--------------automatic variable selection (not in use)----------------

# defining control function for profiler
my_control <-rfeControl(functions = lmFuncs, method = "repeatedcv", repeats = 5, verbose = FALSE)
my_profiler <- rfe(data_boolean[,1:15], data_boolean$Volume, rfeControl = my_control)

# run profiler # compares performance of 4 vs. more attributes # do not compare this performance metrics with performance of prediction!
plot(my_profiler) # plot comparison of 4 vs more metrics

indices_profiler <- c("x3StarReviews","Volume")

# your new subset of most important independent attributes
yourBetterTable <- data_boolean[,indices_profiler]



#---------------------------------SVM-----------------------------------------

#train SVM
SVM_L <- train(Volume~ProductType.Laptop + ProductType.PC + ProductType.Netbook + ProductType.Smartphone 
               + PositiveServiceReview + x4StarReviews  + x1StarReviews + x3StarReviews, data = training, method = "svmLinear",
               preprocess = c("center","scale"), tuneLength = 10)

#ProductType.Laptop + ProductType.PC + ProductType.Netbook + ProductType.Smartphone 
#+ PositiveServiceReview + x4StarReviews  + x1StarReviews
summary(data_clean)
SVM_L

varImp(SVM_L)
plot(varImp(SVM_L))

#predictor variables
predictors(SVM_L)

#make predictions
testPredSVM_L <- predict(SVM_L, testing)

#performace measurment
postResample(testPredSVM_L, testing$Volume)

#plot predicted verses actual
plot(testPredSVM_L,testing$Volume)

#------------------------------------DT----------------------------------------
#train DT
DT <- train(Volume~.-ProductType, data = training, method = "svmLinear",
               preprocess = c("center","scale"), trControl=Control_RepeatedCV, tuneLength = 20)
DT
varImp(DT)
#plot DT

#predictor variables
predictors(DT)

#make predictions
testPredDT <- predict(DT, testing)

#performace measurment
postResample(testPredDT, testing$Volume)

#plot predicted verses actual
plot(testPredDT,testing$Volume)

#------------------------------------XGBM----------------------------------------
#train DT
XGBM <- train(Volume~ PositiveServiceReview + x4StarReviews + x1StarReviews, data = training, method = "xgbTree",
            preprocess = c("center","scale"), trControl=Control_RepeatedCV, tuneLength = 20)
XGBM

varImp(XGBM)

#predictor variables
predictors(XGBM)

#make predictions
testPredXGBM <- predict(XGBM, testing)

#performace measurment
postResample(testPredXGBM, testing$Volume)

#plot predicted verses actual
plot(testPredXGBM,testing$Volume)


#--------------------------------Predict New Products---------------------------------------------
#make predictions
data_new_products_preclean <- data_new_products
data_new_products_preclean$x5StarReviews <- NULL
data_new_products_preclean$ProductNum <- NULL
data_new_products_preclean$BestSellersRank <- NULL




#Boolean conversion
Product_new_typeboolean <- dummyVars(" ~ .", data = data_new_products_preclean)
data_new_boolean <- data.frame(predict(Product_new_typeboolean, newdata = data_new_products_preclean))
data_new_selected <- filter(data_new_boolean, ProductType.Laptop == 1 |ProductType.PC ==1 | ProductType.Netbook == 1 |ProductType.Smartphone== 1)

data_new_selected$Volume <- predict(SVM_L, data_new_selected)
volumepre <- predict(SVM_L, data_new_selected)
ggplot()+geom_bar(data_new_selected, aes(x=P))

#