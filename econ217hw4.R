setwd("Documents/RSTUFF/")
#3
library(matrixStats)
library(grf)

set.seed(1)

star.data<-read.csv('star.csv')
attach(star.data)

y<-star.data$y #outcome
w<-star.data$w #treatment
X<-as.matrix(cbind(fem,wh,fl,urb,age,exp,lad,deg)) #covars

#forest
tau.forest <- causal_forest(X, y, w, num.trees = 4000)

subgroups<-expand.grid(c(0,1),c(0,1),c(0,1),c(0,1))#generating 16 subgroups
num.subgroups<-nrow(subgroups) #16
num.Xvars<-ncol(X) # 8 covariates in total
X.eval<-matrix(0,nrow=num.subgroups,ncol=num.Xvars) #init empty 16x8 matrix
for(j in 1:num.subgroups){
  X.eval[j,1:4]<-as.matrix(subgroups[j,]) 
  X.eval[j,-c(1:4)]<-colMeans(X)[-c(1:4)] #the others
}

tau.hat <- predict(tau.forest, X.eval, estimate.variance = TRUE)

#SEs
SE <- sqrt(tau.hat$variance.estimates)
#CIs
CI<-cbind(tau.hat$predictions - 1.96 * SE, 
          tau.hat$predictions + 1.96 * SE)

#Output
results.summ <-cbind(subgroups,tau.hat$predictions,SE,CI)
colnames(results.summ)<-c("fem","wh","fl","urban","estimate","SE","CI_lower","CI_upper")
print(results.summ)

#4

library(keras)
set.seed(1)
mnist <- dataset_mnist() 
# training set
x_train <- mnist$train$x
y_train <- mnist$train$y
# test set
x_test <- mnist$test$x
y_test <- mnist$test$y

# Reshape covariates so that we have 784 = 28*28 columns per image.
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

# Rescale covariates, dividing by maximum value of x_train.
x_train <- x_train / 255
x_test <- x_test / 255

# Make outcomes categorical.
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

testl1s<-c(0.00001,0.0001,.001,.005,.01,.05)
scorelist<- c() #should probably init this at the length I need it, but w/e
for(i in (testl1s)){
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units = 256, activation = 'relu', input_shape = c(784), kernel_regularizer = regularizer_l1(l = i)) %>% 
    layer_dense(units = 128, activation = 'relu', kernel_regularizer = regularizer_l1(l = i)) %>% 
    layer_dense(units = 10, activation = 'softmax')
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_sgd(),
    metrics = 'accuracy'
  )
  
  history <- model %>% fit(x_train, y_train, epochs = 30, batch_size = 128, validation_split = 0.2) 
  score <- model %>% evaluate(x_test, y_test)
  print(i)
  print(score)
  scorelist<-append(scorelist, score[2])
}
#seems that as the value of our l1 increases, accuracy gets lower and lower.
#so our first result is the fastest
plot(scorelist)


#5
set.seed(1)
library(ISLR)
library(e1071)
data("OJ")
#constructing outcomes and inputs
y<-OJ$Purchase 
X<-OJ[,2:18] 

#setup stuff
test<-1:500
train.y<-y[-test]
train.X<-X[-test,]
test.y<-y[test]
test.X<-X[test,]
train.data<-data.frame(x=train.X, y=train.y)
test.data<-data.frame(x=test.X, y=test.y)

#training
costs<-((1:200)/100)
test.accuracy<-costs
for(i in 1:length(costs)){
  svmfit<-svm(y~., 
              data=train.data, 
              kernel="radial",
              gamma=2, 
              cost = costs[i])
  #outcomes
  test.ypred<-predict(svmfit,test.data)
  test.accuracy[i]<-mean(test.ypred==test.data$y)
}
#what's best?
test.bestcost<-costs[which.max(test.accuracy)]
print(test.bestcost)
