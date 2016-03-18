################################
# Data Mining HW #3            #
################################

# install.packages("verification")
library("verification")


#1.(a)----------------------#
# A Simulation Study (Logistic Regression). 
# Assume y|x~Binary(p), where p= E(y|x), and logit(pi)= -1+5.1 x1i-0.3* x2i
# Generate data with x1i~Unif(0,1), x2i =1 for i odd and x2i =0 for i even, 
# and sample size n=500. 
# Try generalized linear model (GLM) with logistic and probit links. 
# What's your finding? Write a brief report up to one page.
set.seed(7046001)
n   <- 500
x1  <- runif(n)
x2  <- (1:n)%%2





#1.(b)----------------------#
# A Simulation Study (Probit Regression). 
# Assume y|x~Binary(p), where p= E(y|x), and qnorm(pi)= -1+5.1* x1i-0.3* x2i
# Generate data with x1i~Unif(0,1), x2i =1 for i odd and x2i =0 for i even, 
# and sample size n=500. 
# Try generalized linear model (GLM) with logistic and probit links. 
# What's your finding? Write a brief report up to one page.
set.seed(7046001)
n   <- 500
x1  <- runif(n)
x2  <- (1:n)%%2
g   <- -1+5.1*x1-0.3*x2
p   <- pnorm(g)
y   <- rbinom(n,1,p)
test<- data.frame(y,x1,x2)
#(1) Probit regression
glm_probit<- glm(y~x1+x2,family=binomial(link = "probit"),data=test)
summary(glm_probit)
logLik(glm_probit)
(AIC_probit<- AIC(glm_probit) )
(BIC_probit<- BIC(glm_probit) )
#ROC Curve, predictions
pred_probit <- predict(glm_probit, test, type = "response")
roc_probit  <- roc.plot(test$y, pred=pred_probit)
roc_probit$roc.vol #To get the area under the ROC curve

#(2) Logistic regression
glm_logit<- glm(y~x1+x2,family=binomial(link = "logit"))
summary(glm_logit)
logLik(glm_logit)
(AIC_logit<- AIC(glm_logit) )
(BIC_logit<- BIC(glm_logit) )
#ROC Curve, predictions
pred_logit <- predict(glm_logit, test, type = "response")
roc_logit  <- roc.plot(test$y, pred=pred_logit)
roc_logit$roc.vol #To get the area under the ROC curve

#compare roc's of the two models in the same graph
roc.plot(x=test$y,pred=cbind(pred_probit,pred_logit),legend=TRUE,leg.text=c("Probit","Logit"))$roc.vol
#Predicted probabilities
# pred <-data.frame(value=c(pred_probit,pred_logit,y),
#                   group=as.factor(c(rep("Probit",n),rep("Logistic",n),
#                                     rep("True value",n))),
#                   index=rep(1:n,3) )
# library(ggplot2)
# qplot(x=index, y=value, 
#       data=pred, 
#       colour=group, 
#       main="GGPLOT line plot with groups") 
# ggplot(data = pred, aes(x = index, y = value, color = group)) +     geom_point()


#Cross validation and cost function







#2--------------------------#




#3---------------------------#
set.seed(7046001)

bank = read.csv("F:/school/DataMining/Data/bankruptcy.csv")
# Random sample a training data set that contains 80% of original data points
n <- nrow(bank)
nT<- sum(bank[,2]==1)
nF<- n-nT
ntrain <- round(0.8*n)
ntest  <- n-ntrain
ntrainT <-round(0.8*nT)
ntrainF <- ntrain- ntrainT

indexT <- sample(nT,ntrainT,replace=FALSE)
indexF <- sample(nF,ntrainF,replace=FALSE)
index  <- c((1:n)[bank[,2]==1][indexT],(1:n)[bank[,2]==0][indexF])
bank0 <- bank[index,]#training data
bank1 <- bank[-index,]#testing data
#training
#data explore
colnames(bank0)
summaryb<-summary(bank0[,4:13])
R<-bank0[,4:13]
N<-rep(ntrain,10)
Rmean<-sapply(R, function(x) mean(as.numeric(x)) )
Rmedian<-sapply(R, function(x) median(as.numeric(x)) )
RstdDev<-sapply(R, function(x) sd(as.numeric(x)) )
Rmin<- sapply(R, function(x) min(as.numeric(x)) )
Rmax<- sapply(R, function(x) max(as.numeric(x)) )
Rsum<-data.frame(Variable=c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10"),
                 N=N,Mean=Rmean,Median=Rmedian,"Std Dev"=RstdDev,"Minimum"=Rmin,"Maximum"=Rmax)
#scatter plot

pairs(bank0[,c(2,4:13)], main = "Scatter plot of the training data",pch = 21)
table(bank0[,2])
#correlation matrix
cor_bank0<-cor(bank0[,c(2,4:13)])

table(bank0$FYEAR)

#outlier

boxplot(R1~DLRSN,data=bank0)
#Full model
modelF<- glm(DLRSN~R1+R2+R3+R4+R5+R6+R7+R8+R9+R10,data=bank0,family=binomial(link = "logit"))
modelF.s<-summary(modelF)
logLik(modelF)
AIC(modelF)
BIC(modelF)


# model0<- glm(DLRSN~1,data=bank0,family=binomial(link = "logit"))
# model0.s<-summary(model0)
#
AIC.step <- step(modelF)
model.AIC<- glm(DLRSN~R1 + R2 + R3 + R6 + R7 + R8 + R9 + R10,data=bank0,family=binomial(link = "logit"))
model.AIC.s<-summary(model.AIC)
logLik(model.AIC)
AIC(model.AIC)
BIC(model.AIC)
# 
# AIC.step0 <- step(model0,scope=c(lower=model0,upper=modelF),direction="both")
# model0.AIC<- glm(DLRSN~R1 + R2 + R3 + R6 + R7 + R8 + R9 + R10,data=bank0,family=binomial(link = "logit"))
# model0.AIC.s<-summary(model0.AIC)
# 
# 
# model1<- glm(DLRSN~R1 + R2 + R3,data=bank0,family=binomial(link = "logit"))
# AIC.step1 <- step(model1,scope=c(lower=model0,upper=modelF),direction="both")
# model1.AIC<- glm(DLRSN~R1 + R2 + R3 + R6 + R7 + R8 + R9 + R10,data=bank0,family=binomial(link = "logit"))
# model1.AIC.s<-summary(model1.AIC)

#
BIC.step <- step(modelF, k = log(ntrain))
model.BIC<- glm(DLRSN~R2 + R3 + R6 + R7 + R9 + R10,data=bank0,family=binomial(link = "logit"))
model.BIC.s<-summary(model.BIC)
logLik(model.BIC)
AIC(model.BIC)
BIC(model.BIC)
# model1<- glm(DLRSN~R1+R6+R7+R8+R9+R10,data=bank0,family=binomial(link = "logit"))
# summary(model1)




#ROC Curve, predictions

pred.F <- predict(modelF, bank0, type = "response")
roc_F  <- roc.plot(bank0$DLRSN, pred=pred.F)
roc_F$roc.vol #To get the area under the ROC curve

pred.AIC <- predict(model.AIC, bank0, type = "response")
roc_AIC  <- roc.plot(bank0$DLRSN, pred=pred.AIC)
roc_AIC$roc.vol #To get the area under the ROC curve

pred.BIC <- predict(model.BIC, bank0, type = "response")
roc_BIC  <- roc.plot(bank0$DLRSN, pred=pred.BIC)
roc_BIC$roc.vol #To get the area under the ROC curve

#compare roc's of the two models in the same graph
roc.plot(x=bank0$DLRSN,pred=cbind(pred.AIC,pred.BIC),legend=TRUE,leg.text=c("AIC","BIC"))$roc.vol
roc.plot(x=bank0$DLRSN,pred=cbind(pred.F,pred.AIC,pred.BIC),legend=TRUE,leg.text=c("Full model","2nd           ","3rd            "),col=c('red','green','blue'))$roc.vol

#in sample
pred.AIC.in <- pred.AIC > 0.5
pred.AIC.in <- as.numeric(pred.AIC.in)
# Next we look at the confusion matrix, dnn is used to label the column and row:
  
mrtable <-  table(bank0$DLRSN, pred.AIC.in, dnn = c("Truth", "Predicted"))



(373+115)/(3613+115+373+248)
#out of sample

pred.AICout <- predict(model.AIC, bank1, type = "response")
roc_AICout  <- roc.plot(bank1$DLRSN, pred=pred.AICout,legend=TRUE,leg.text="Final model")$roc.vol
roc_AICout#To get the area under the ROC curve

pred.AIC.out <- pred.AICout > 0.5
pred.AIC.out <- as.numeric(pred.AIC.out)
# Next we look at the confusion matrix, dnn is used to label the column and row:

mrtable <-  table(bank1$DLRSN, pred.AIC.out, dnn = c("Truth", "Predicted"))
mrtable

(93+39)/(893+39+93+62)
