Landing <- read.csv("C:/Users/Wang/Downloads/Landing.csv")
attach(Landing)
colnames(Landing)

x<-Landing[,-1]
m<-ncol(x)
varnames<-colnames(x)
summary(x)
par(mfrow=c(3,3))
for(i in 1:m){
  hist(x[,i],main="",xlab=varnames[i])
}
par(mfrow=c(1,1))

pairs(x)


data <- subset(Landing,duration >= 40 
               & height >= 6 
               & (speed_ground >= 30 & speed_ground <= 140)
               & (speed_air >= 30 & speed_air <=140 )
               & distance < 6000)





library("fpc")
fit = kmeans(data[,2:8],2)
plotcluster(data[,2:8],fit$cluster)

A <- subset(data,aircraft =="airbus")[,- 1]

# next we sample 90% of the original data and use it 
# as the training set
# the remaining 10% is used as test set
# the regression model will be built on the train set 
# and future performance
# of model will be evaluated with test set
subset = sample(nrow(A), nrow(A)*.9)
Atrain = A[subset,]
Atest = A[- subset,]

# model building
model_1 <- lm(distance~., Atrain)
summary(model_1)


par(mfrow=c(2,2))
plot(model_1)
# evaluating model fitness
summary(model_1)$sigma^2
summary(model_1)$r.squared
summary(model_1)$adj.r.squared
AIC(model_1)
BIC(model_1)
logLik(model_1)
plot(model_1)

Atrain2<-Atrain[-c(37,38,39,54),]

model_2<-lm(distance~., Atrain2)
summary(model_2)


summary(model_2)$sigma^2
summary(model_2)$r.squared
summary(model_2)$adj.r.squared
AIC(model_2)
BIC(model_2)
logLik(model_2)

# variable selection
# best subset regression
library("leaps")
subset_result = regsubsets(distance~., Atrain2,nbest = 2, 
                           nvmax =14)
plot(subset_result,scale = "bic")

# 
fullmodel <- lm(distance ~ .,Atrain)
nullmodel <- lm(distance ~ 1,Atrain)
model.1 = step(fullmodel, direction = "backward")
model.2 = step(nullmodel, scope = list(
  lower = nullmodel, upper = fullmodel), direction = "forward")
model.3 = step(nullmodel, scope = list(
  lower = nullmodel, upper = fullmodel), direction = "both")

summary(nullmodel)
summary(model.1)
summary(model.2)
summary(model.3)
# diagnostic plots

m2<-lm(distance ~ speed_air + height + pitch + speed_ground + no_pasg,Atrain)
m3<-lm(distance ~ speed_air + height + pitch + speed_ground,Atrain)
m4<-lm(distance ~ speed_air + height + pitch,Atrain)
m5<-lm(distance ~ speed_air + height,Atrain)
m6<-lm(distance ~ speed_air,Atrain)

summary(fullmodel)
AIC(fullmodel)
BIC(fullmodel)


model <-lm(distance~speed_air + height + pitch,Atrain2)
summary(model)
plot(model)

pi = predict(model, Atest)
mean((pi - Atest$distance)^2)
