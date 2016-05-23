library(lubridate)
d$Weekday1 <- wday(d$date_filed)
d$Month1   <- month(d$date_filed)

p <- select(d,case_type,court_name,CivilOrCriminal,AgeinDays,DaysToFirstHearing,NumHearing,
            AvgDaysBetweenHearing,NewStatus,Weekday1,Month1)
###
# remove Pending cases
###
p <- na.omit(p) 
dim(p)
dim(d)
table(p$NewStatus)
table(d$NewStatus)

###
# Corelation
### 
cor(d$AgeinDays,d$DaysToFirstHearing,use="complete.obs")
cor(p$AgeinDays,p$DaysToFirstHearing,use="complete.obs")
cor(p$AgeinDays,p$NumHearing,use="complete.obs")
cor(p$AgeinDays,p$AvgDaysBetweenHearing,use="complete.obs")

chisq.test(p$case_type,p$court_name)
chisq.test(p$case_type,p$CivilOrCriminal)
chisq.test(p$CivilOrCriminal,p$court_name)


###
# convert all to numeric
###

str(p)
p$case_type <- as.numeric(as.factor(p$case_type)) - 1
table(p$case_type)

p$court_name <- as.numeric(as.factor(p$court_name)) - 1
p$CivilOrCriminal <- as.numeric(as.factor(p$CivilOrCriminal)) - 1
p$NewStatus <- as.numeric(as.factor(p$NewStatus)) - 1
str(p)

library(caret)
set.seed(100)
inTrain <- createDataPartition(p$NewStatus, p = .8)[[1]]
train <- p[ inTrain, ]
test  <- p[-inTrain, ]


###
# Prediction
###

# Baseline model - predict the mean of the training data
best.guess <- mean(train$AgeinDays) 

# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-test$AgeinDays)^2))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-test$AgeinDays))
MAE.baseline

###
# linear regression
###
lm.fit1 <- lm(AgeinDays ~ NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)
summary(lm.fit1)

lm.fit2 <- lm(AgeinDays ~ case_type + court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)
summary(lm.fit2)

lm.fit3 <- lm(AgeinDays ~ court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)
summary(lm.fit3)

lm.fit4 <- lm(AgeinDays ~ court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing + Weekday1 + Month1 ,data=train)
summary(lm.fit4)

#plot(lm.fit1)
plot(predict (lm.fit1), residuals (lm.fit1))
plot(hatvalues (lm.fit1))
which.max (hatvalues (lm.fit1))


lm1.yhat <- predict(lm.fit1,test, se.fit = FALSE)
lm2.yhat <- predict(lm.fit2,test, se.fit = FALSE)
lm3.yhat <- predict(lm.fit3,test, se.fit = FALSE)
lm4.yhat <- predict(lm.fit4,test, se.fit = FALSE)

RMSE.lm1 <- sqrt(mean((lm1.yhat-test$AgeinDays)^2))
RMSE.lm1

MAE.lm1 <- mean(abs(lm1.yhat-test$AgeinDays))
MAE.lm1

RMSE.lm2 <- sqrt(mean((lm2.yhat-test$AgeinDays)^2))
MAE.lm2 <- mean(abs(lm2.yhat-test$AgeinDays))

RMSE.lm3 <- sqrt(mean((lm3.yhat-test$AgeinDays)^2))
MAE.lm3 <- mean(abs(lm3.yhat-test$AgeinDays))

RMSE.lm4 <- sqrt(mean((lm4.yhat-test$AgeinDays)^2))
MAE.lm4 <- mean(abs(lm4.yhat-test$AgeinDays))

###
# rpart
###
library(rpart)

rt <- rpart(AgeinDays ~ court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)

test.pred.rtree <- predict(rt,test) 

RMSE.rt <- sqrt(mean((test.pred.rtree-test$AgeinDays)^2))
RMSE.rt

MAE.rt <- mean(abs(test.pred.rtree-test$AgeinDays))
MAE.rt


###
# random forest
###
library(randomForest)
set.seed(123)

rf1 <- randomForest(AgeinDays ~ court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,
                    data=train, importance = TRUE, ntree=1000)

which.min(rf1$mse)
# 63 trees are enough :)

plot(rf1)
imp <- as.data.frame(sort(importance(rf1)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

rf1.yhat <- predict(rf1,test)
RMSE.rf1 <- sqrt(mean((rf1.yhat-test$AgeinDays)^2))
RMSE.rf1

MAE.rf1 <- mean(abs(rf1.yhat-test$AgeinDays))
MAE.rf1

###
# xgboost
###
library(xgboost)

str(train)
set.seed(3)
xb.fit1 <- xgboost(data        = data.matrix(subset(train, select = -AgeinDays)),
               label       = train$AgeinDays,
               nrounds     = 200,
               objective   = "reg:linear",
               eval_metric = "rmse")

xb1.yhat <- predict(xb.fit1, data.matrix(subset(test, select = -AgeinDays)))
RMSE.xb <- sqrt(mean((xb1.yhat-test$AgeinDays)^2))
RMSE.xb

MAE.xb <- mean(abs(xb1.yhat-test$AgeinDays))
MAE.xb

###
# feature importance
###

# get the trained model
model = xgb.dump(xb.fit1, with.stats=TRUE)
# get the feature real names
names = dimnames(train)[[2]]
names
# compute feature importance matrix
importance_matrix = xgb.importance(names, model=xb.fit1)

# plot
gp = xgb.plot.importance(importance_matrix)
print(gp) 

# 2
xb.fit2 <- xgboost(data        = data.matrix(subset(train, select = -AgeinDays)),
                label       = train$AgeinDays,
                nfold=5, nrounds=200, eta=0.02,
                max_depth=5, subsample=0.6, colsample_bytree=0.85,min_child_weight=1,
                objective   = "reg:linear",
                eval_metric = "rmse")


xb2.yhat <- predict(xb.fit2, data.matrix(subset(test, select = -AgeinDays)))
RMSE.xb2 <- sqrt(mean((xb2.yhat-test$AgeinDays)^2))
RMSE.xb2

MAE.xb2 <- mean(abs(xb2.yhat-test$AgeinDays))
MAE.xb2

xb.fit3 <- xgboost(data        = data.matrix(subset(train, select = -AgeinDays)),
                   label       = train$AgeinDays,
                   nrounds     = 1200,
                   objective   = "reg:linear",
                   eval_metric = "rmse")

xb3.yhat <- predict(xb.fit3, data.matrix(subset(test, select = -AgeinDays)))
RMSE.xb3 <- sqrt(mean((xb3.yhat-test$AgeinDays)^2))
RMSE.xb3

MAE.xb3 <- mean(abs(xb3.yhat-test$AgeinDays))
MAE.xb3

###
# Create a df of the RMSE and mAE to compare
###

df <- data.frame(Method = c("Baseline","Linear Regression1","Linear Regression2","Linear Regression3","Rpart","Random forest","X Boost1","X Boost2","X Boost3"),
                       RMSE   = c(RMSE.baseline,RMSE.lm1,RMSE.lm2,RMSE.lm3,RMSE.rt,RMSE.rf1,RMSE.xb,RMSE.xb2,RMSE.xb3),
                       MAE    = c(MAE.baseline,MAE.lm1,MAE.lm2,MAE.lm3,MAE.rt,MAE.rf1,MAE.xb,MAE.xb2,MAE.xb3)) 

df
