###################
# Data Mining Hw2 #
###################

###############
# Boston data #
###############
library(MASS)
data(Boston); #this data is in MASS package

set.seed(7046001)
n<-nrow(Boston)
m<-ncol(Boston)
sampleIndex<-sample(n,n*0.9,replace=F)
Boston_train<-Boston[sampleIndex,]  #get sample
Boston_test <-Boston[-sampleIndex,]
varnames<-colnames(Boston_train) 
#explore---------------------
summary(Boston_train)
par(mfrow=c(4,4))
for(i in 1:m){
  hist(Boston_train[,i],main="",xlab=varnames[i])
}
par(mfrow=c(1,1))


Boston_train$chas <-as.factor(Boston_train$chas)

pairs(Boston_train, main = "Scatter plot of the training data",
      pch = 21)
#Model selection---------------------------------
#include all the predictors  (model_1)
model_1 = lm(medv ~ crim + zn + indus + chas +  nox + rm + age + dis + rad + tax + ptratio + black + lstat, data = Boston_train)
summary(model_1)
#stepwise
model_step1 = step(model_1, direction="both")
# model_step1 = lm(formula = medv ~ crim + zn + chas + nox + rm + dis + rad + 
#                   tax + ptratio + black + lstat, data = Boston_train)
# 
# model_0 = lm(medv ~ 1, data = Boston_train)
# summary(model_0)
# model_step0 = step(model_0, scope = list(lower = model_0 , upper = model_1),direction="both")
summary(model_step1)
plot(model_step1)

# boxcox(model_step1,lambda=seq(-1,1,by=.1))
# Boston_train$y <- (Boston_train$medv^0.1313131-1)/0.1313131
# model_step1b = lm(formula = y ~ crim + zn + chas + nox + rm + dis + rad + 
#                    +  tax + ptratio + black + lstat, data = Boston_train)
# summary(model_step1b)
# plot(model_step1b)

model2 <- lm(formula = y ~ crim + zn + chas + nox + rm + dis + rad + 
                                   +  tax + ptratio + black + lstat, data = Boston_train)



#interaction
lm(medv ~ crim + zn + crim:zn, data = Boston_train)
# The following way automatically add the main effects of crim and zn
lm(medv ~ crim * zn, data = Boston_train)


#Evaluating model fitness
#MSE
model_summary = summary(model_1)
(model_summary$sigma)^2
#R2
model_summary$r.squared
#adjusted R2
model_summary$adj.r.squared

#AIC
AIC(model_1)
#BIC
BIC(model_1)

pi = predict(object = model_1, newdata = Boston_test)
#Mean Squared Error (MSE): average of the squared differences between the predicted and actual values

mean((pi - Boston_test$medv)^2)

#Model Selection



#################
# 2. Simulation #
#################


set.seed(7046001)

library(stats)
###  1.a)
# Normally distributed numbers, a 200 of them.
nv <- c(25,100,200,500,5000)
sigs<- c(0.1,0.5,1)
output <- NULL
modelt <- NULL
i=1
for (n in nv){
  for (sig in sigs){
    #   sig<- 1
    z1 <- rnorm(n)
    x1 <- 2 + 0.5*z1      # Makes a vector x out of the vector z
    summary(x1)
    z2 <- rnorm(n)
    x2 <- 0.1*z2-1
    x3 <- x1*x2
    
    # An OLS model
    u <- rnorm(n,sd=sig)
    y <- 4 + 0.9*x1 + 3*x2 + u  #true model
    summary(y)
    
    #you may want to generate data in another file and save above data as data1.Rdata
    #so that you can use the same data set in future
    # then load the previous saved data
    
    #  load("E:\\WORK\\Course\\Spring 05\\QA727\\data1.RData")
    
    # A regression
    model <- lm(y ~ x1+x2+x3)
    summary(model)
    
    #stepwise
    model.step<-step(model)#based on AIC
    
    # manually, try other criteria, e.g. R2, MSE etc
    allmodel <- c(y ~ x1+x2+x3,y ~ x1+x2,y ~ x1+x3,y ~ x2+x3,y ~ x1,y ~ x2,y ~ x3 )
    for(temp in allmodel){
      model123 <- lm(temp)
      summodel <-summary(model123)
      AIC1  <- AIC(model123)
      R2    <- summodel$r.squared
      AdjR2 <- summodel$adj.r.squared
      MSE   <- (summodel$sigma)^2
      modelt[[i]] <-summodel
      output[[i]] <- data.frame(i,n,sig,model=Reduce(paste, deparse(temp)),AIC1,MSE,R2,AdjR2)
      i <- i+1
    }
  }
}
output1 <- do.call(rbind,output)
write.csv(output1,"F:\\school\\DataMining\\hw\\hw2\\Qn2.csv")
modelt[[60]]  #using AIC, MSE or Adj R2
modelt[[57]]  #using R2

##########################
# 3. MC Simulation study #
##########################

set.seed(7046001)

list.MSE <- NULL
list.beta0 <- NULL
list.beta1 <- NULL
list.beta2 <- NULL
x1 <- rnorm(n = 200, mean = 2, sd = 0.5)
x2 <- rnorm(n = 200, mean = -1, sd = 0.1)  
for(j in 1:100) 
  #for 100 times  
{		
  u <- rnorm(n = 200, mean = 0, sd = 1)		
  y <- 4 + 0.9 * x1 + 3 * x2 + u #simulate the response variable		
  lm.out <- lm(y ~ x1+x2) #fit a linear regression model		
  summary <- summary(lm.out) 		
  list.MSE[j] <- (summary$sigma)^2           # get the model MSE for each m		
  list.beta0[j] <- summary$coefficients[1]   # get the estimate of the intercept for each m	
  list.beta1[j] <- summary$coefficients[2]   # get the estimate of the coefficient of x1 for each m	
  list.beta2[j] <- summary$coefficients[3]   # get the estimate of the coefficient of x2 for each m	
}
#average estimated model MSE------------
MSE <- mean(list.MSE)		
# get the average MSE of the 200 model MSEs
beta0 <- mean(list.beta0)	
# get the average estiamte value of the intercept 
beta1 <- mean(list.beta1)	
# get the average estiamte value of the coefficient of x1
beta2 <- mean(list.beta2)	
# get the average estiamte value of the coefficient of x2-----------------
mean.beta <- c(beta0, beta1, beta2)	
mean.theta <- c(mean.beta, MSE)
# make the above three estimates into one vector
#estimation bias--------------
bias.beta <- c(beta0 - 4, beta1 - 0.9, beta2 - 3)	
bias.theta <- c(bias.beta,MSE-1 )
# get the bias of the coefficients estimate
#variance-----------------
var.beta0 <- var(list.beta0)	# get variance of the intercept estimates
var.beta1 <- var(list.beta1)	# get variance of x1 coefficient estimates
var.beta2 <- var(list.beta2)	# get variance of x2 coefficient estimates
var.beta  <- c(var.beta0, var.beta1, var.beta2)	# make the above three variances into one vector
var1.beta0 <- 99/100*var(list.beta0)  # get variance of the intercept estimates
var1.beta1 <- 99/100*var(list.beta1)	# get variance of x1 coefficient estimates
var1.beta2 <- 99/100*var(list.beta2)	# get variance of x2 coefficient estimates
var1.beta  <- c(var1.beta0, var1.beta1, var1.beta2)
var1.MSE   <- 99/100*var(list.MSE)
var1.theta <-c(var1.beta, var1.MSE)
#mse-----------
mse.beta0 <- bias.beta[1]^2 + var1.beta0
mse.beta1 <- bias.beta[2]^2 + var1.beta1
mse.beta2 <- bias.beta[3]^2 + var1.beta2
mse.beta  <- c(mse.beta0, mse.beta1, mse.beta2)	
mse.MSE   <- bias.theta[4]^2 + var1.MSE
mse.theta <-c(mse.beta,mse.MSE)
# get the mse of the coefficient estimates
# print the results
true.beta <- c(4,0.9,3)
true.beta
mean.beta
bias.beta
var.beta
mse.beta
MSE

c(4,0.9,3,1)
mean.theta
bias.theta
var1.theta
mse.theta
par(mfrow=c(2,2))
hist(list.beta0,freq=FALSE,main="Distribution of estimated beta0",xlab="estimated beta0")
lines(density(list.beta0),col='red')
hist(list.beta1,freq=FALSE,ylim=c(0,3.5),main="Distribution of estimated beta1",xlab="estimated beta1")
lines(density(list.beta1),col='red')
hist(list.beta2,freq=FALSE,main="Distribution of estimated beta2",xlab="estimated beta2")
lines(density(list.beta2),col='red')
hist(list.MSE,freq=FALSE,ylim=c(0,4),main="Distribution of MSE",xlab="MSE")
lines(density(list.MSE),col='red')
par(mfrow=c(1,1))
