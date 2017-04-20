
library(data.table)
library(Hmisc)
require(foreign)
require(MASS)
require(pls)
mb.train <- fread("CMAPSS_train-1_100rows.csv", header = T, sep = ',')

# drop nominal/ordinal data
mb.train$turbine_no <- NULL
mb.train$flight_number <- NULL
mb.train<- within(mb.train, rm(Sensor1,Sensor2,Sensor3,Sensor4,Sensor5,Sensor6,Sensor10,
                    Sensor13,Sensor14,Sensor15,Sensor16,Sensor18,Sensor19,Sensor21))

#build a linear regression model 
# lm(TARGET_WINS ~ ., data = mb)
mod1 <- summary(ols <- lm(RUL ~ ., data = mb.train))

# abs residual values 
rabs <- abs(r)
a <- cbind(mb, d1, r, rabs)
asorted <- a[order(-rabs), ]
asorted[1:10, ]

# remove duplicate rows
# mbnodup <- subset(mb,!duplicated(mb$Mach,mb$altitude_sea_level,mb$altitude_sea_level,mb$Sensor7,mb$Sensor8,mb$Sensor9,mb$Sensor12,
#                                  mb$Sensor17,mb$Sensor20))

  
# running robust regression on mb object
# summary(mbnodup.rr <- rlm(RUL ~ altitude_sea_level,Mach, Sensor7, Sensor8, Sensor9, Sensor12, Sensor17, Sensor20, data = mbnodup))

# compare residuals and weights for response
# hweights <- data.frame(TARGET_WINS= mb$TARGET_WINS, resid = mb.rr$resid, weight = mb.rr$w)
# hweights2 <- hweights[order(mb.rr$w), ]
# hweights2[1:15, ]

#read test data
mb_test <- fread("CMAPSS_train-1_rest.csv", header = T, sep = ',')

predictions = cbind(mb_test, Predicted_RUL= predict(ols, newdata = mb_test))

#New model for predicting RUL from the train data exported in the above step
#read the data set
mb.train.rul <- fread("Train_data_Final.csv", header = T, sep = ',')
mb.train.rul<- within(mb.train.rul, rm(Sensor1,Sensor2,Sensor3,Sensor4,Sensor5,Sensor6,Sensor10,
                               Sensor13,Sensor14,Sensor15,Sensor16,Sensor18,Sensor19,Sensor21))
mb.train.rul$turbine_no <- NULL
mb.train.rul$flight_number <- NULL
mb.train.rul$Thrust_Resolver_Angle <- NULL

#principal component analysis
mb.train.rul$Thrust_Resolver_Angle <- NULL #becasue the variance is zero, all enteries are equal to 100
my_data <- subset(mb.train.rul, select = -c(RUL)) #remove RUL
prin_comp <- prcomp(my_data,scale. = T) #run pca

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:10]

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

#add a training set with principal components
train.data <- data.frame(RUL= mb.train.rul$RUL, prin_comp$x)

#we are interested in first 3 PCAs
train.data <- train.data[,1:4]

#transform test into PCA

pca.test <- mb.train <- fread("CMAPSS_test-1.csv", header = T, sep = ',')
pca.test <- within(pca.test, rm(Sensor1,Sensor2,Sensor3,Sensor4,Sensor5,Sensor6,Sensor10,
                                       Sensor13,Sensor14,Sensor15,Sensor16,Sensor18,Sensor19,Sensor21))

pca.test$turbine_no <- NULL
pca.test$flight_number <- NULL
pca.test$Thrust_Resolver_Angle <- NULL

#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

#running decision tree to select pc's
rpart.model <- rpart(RUL ~ .,data = train.data, method = "anova")

#predict new rul on test data
rpart.prediction <- predict(rpart.model, test.data)

#export predicted rul to csv
write.csv(rpart.prediction,"test1.csv")

#running PCR for estimating MSE http://www.milanor.net/blog/performing-principal-components-regression-pcr-in-r/
require(pls)
set.seed (1000)
pcr_model <- pcr(RUL~., data = train.data, scale = TRUE, validation = "CV")
summary(pcr_model)

# Plot the cross validation MSE
validationplot(pcr_model, val.type="MSEP")

# Plot the root mean squared error
validationplot(pcr_model)

#score using pcr
pcr_pred <- predict(pcr_model, test.data, ncomp = 3)
mean((pcr_pred - train.data[,"RUL"])^2) #MSE calculation 
