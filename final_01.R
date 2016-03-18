Landing <- read.csv("C:/Users/Wang/Downloads/Landing.csv")
attach(Landing)
colnames(Landing)

x<-Landing[,-1]
m<-ncol(x)
varnames<-colnames(x)
summary(x)
par(mfrow=c(2,4))
for(i in 1:m){
  hist(x[,i],main="",xlab=varnames[i])
}
par(mfrow=c(1,1))

pairs(x)

library("TSA")
BoxCox.ar(distance)
Y<-log(distance)
#library("fpc")
#fit = kmeans(data[,2:8],2)
#plotcluster(data[,2:8],fit$cluster)

data <- Landing[duration >= 40 
                & height >= 6 
                & (speed_ground >= 30 | speed_ground <= 140)
                & (speed_air >= 30 | speed_air <= 140)
                & distance <= 6000,]
A0 <- Landing[duration >= 40 
              & height >= 6 
              & (speed_ground >= 30 | speed_ground <= 140)
              & (speed_air >= 30 | speed_air <= 140)
              & distance <= 6000
              &aircraft =="airbus",]
B0 <- Landing[duration >= 40 
              & height >= 6 
              & (speed_ground >= 30 | speed_ground <= 140)
              & (speed_air >= 30 | speed_air <= 140)
              & distance <= 6000
              &aircraft =="boeing",]

A<-A0[,-1]
B<-B0[,-1]
pairs(A)
pairs(B)

###################
#Airbus
###################
m<-ncol(A)
varnames<-colnames(A)
par(mfrow=c(2,4))
for(i in 1:m){
  hist(A[,i],main="",xlab=varnames[i])
}
par(mfrow=c(1,1))

pairs(A)
###################
#mean(A$speed_ground)
#var(A$speed_ground)
#mean(A$speed_air)
#var(A$speed_air)
###################
modela1<-lm(A$distance~duration+no_pasg+speed_ground+speed_air+height+pitch,A)
summary(modela1)
pa <- predict(modela1)
ap <- na.omit(A$pitch)
plot(ap,pa)
line(ap,pa)
#abline(114.7)
abline(modela1)

modelb1<-lm(B$distance~duration+no_pasg+speed_ground+speed_air+height+pitch,B)
summary(modelb1)
pb <- predict(modelb1)
bp <- na.omit(B$pitch)
plot(bp,pb)
line(bp,pb)
#abline(-97.7307)
abline(1771.3,212.9)

model1<-lm(data$distance~.,data)
summary(model1)

model2<-lm(data$distance~aircraft+duration+no_pasg+speed_ground+speed_air+height+pitch+pitch*aircraft,data)
summary(model2)
###################
Ya<-log(A$distance)
Yb<-log(B$distance)
Y<-log(data$distance)
###################
model3 = step(model2,direction="backward")
(AIC(model3))
summary(model3)

model4 = step(model2,direction="forward")
(AIC(model4))
summary(model4)
###################
#Multicollinearity
###################
library("car")
vif(model1)

A$speed1 = abs(A$speed_ground-A$speed_air)
model2<-lm(Ya~duration+speed1+height+pitch,A)
summary(model2)

A$speed2 = A$speed_ground/A$speed_air
model3<-lm(Ya~duration+speed2+height+pitch,A)
summary(model3)
###################
