# Daniel Gomes, John Gomes, Blake Simmons
# CIS490 Project 2
# Adults dataset

# libraries
library(rpart)
library(rpart.plot)

adults <- read.csv('adult.csv', header=FALSE)

#name columns
names(adults) <- c("age", "workclass", "fnlwgt", "education", 
                 "education-num", "marital-status", "occupation", "relationship",
                 "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country",
                 "class")

#set seed
set.seed(490)

#split data
adults.split <- sample(1:nrow(adults), size = nrow(adults) * 0.7)
adults.train <- adults[adults.split,]
adults.test <- adults[-adults.split,]


###cart model (no pruning) WITH cp = 0.001 in rpart function
###this will make a tree using the whole dataset - class variable

adults.cart <- rpart(formula = class ~ ., data= adults.train, method= "class",
                     control= rpart.control(minbucket = 2, xval = 10, cp = 0.001))
prp(adults.cart, roundint = FALSE)


###cart model (no pruning) WITHOUT cp in rpart function
###results in making a tree of size 4

#adults.cart <- rpart(formula = class ~ ., data= adults.train, method= "class",
#                     control= rpart.control(minbucket = 2, xval = 10))
#prp(adults.cart, roundint = FALSE)

#pruning
cp.adults.param <- adults.cart$cptable




#create tables for cross validation

###first attempt without adding cp value to rpart function
#train.acc <- double(4)
#cv.acc <- double(4)
#test.acc <- double(4)


###second attempt after adding cp = 0.001 to rpart function
train.acc <- double(11)
cv.acc <- double(11)
test.acc <- double(11)




for (i in 1:nrow(cp.adults.param)) {
  alpha <- cp.adults.param[i, 'CP']
  train.cm <- table(adults.train$class, predict(prune(adults.cart, cp=alpha), newdata = adults.train, type='class'))
  train.acc[i] <- 1-sum(diag(train.cm))/sum(train.cm)
  cv.acc[i] <- cp.adults.param[i, 'xerror'] * cp.adults.param[i, 'rel error']
  test.cm <- table(adults.test$class, predict(prune(adults.cart, cp=alpha), newdata = adults.test, type='class'))
  test.acc[i] <- 1-sum(diag(test.cm))/sum(test.cm)
}


#plot cross validation
matplot(cp.adults.param[,'nsplit'], cbind(train.acc, cv.acc, test.acc), pch=19, col=c("red", "black", "blue"), type="b", ylab="Loss", xlab="Depth")
legend("right", c('Train', 'CV', 'Test'), col=seq_len(3),cex=0.8,fill=c("red", "black", "blue"))

#create model and plot new tree
prune.adults.trees <- prune(adults.cart, cp=cp.adults.param[4 ,'CP'])
prp(prune.adults.trees)

