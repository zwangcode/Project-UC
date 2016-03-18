landing<-read.csv("C:/Users/Wang/Downloads/Landing.csv",head=TRUE,sep=",")
attach(landing)
# data exploration
str(landing)
summary(landing)

x<-landing[,-1]
m<-ncol(x)
varnames<-colnames(x)

par(mfrow=c(2,4))
for(i in 1:m){
  hist(x[,i],main="",xlab=varnames[i])
}
par(mfrow=c(1,1))
###################
log transform


###################
# data cleaning
data <- landing[duration >= 40 
                & height >= 6 
                & (speed_ground >= 30 | speed_ground <= 140)
                & (speed_air >= 30 | speed_air <= 140)
                & distance <= 6000,]

data <- na.omit(data)
# new_data exploration
str(data)
attributes(data)$names
summary(data)

# data visualization
# scatter plot
pairs(data)

data_train_1<-data[-c(109:118,186:195),]
data_test_1<-data[c(109:118,186:195),]



# modeling
model1<-lm(distance~.,data_train_1)
summary(model1)

plot(model1$res)

qqnorm(model1$res);qqline(model1$res)
shapiro.test(model1$res)

library("lmtest")
gqtest(model1)

anova(model1)

library("TSA")
BoxCox.ar(distance)


model2<-lm(log(distance)~.,data_train_1)
summary(model2)

plot(model1$res)

qqnorm(model1$res);qqline(model1$res)
shapiro.test(model1$res)

library("lmtest")
gqtest(model2)

anova(model2)

model2_1 = step(model2)
##############
Reg_Diag(model1)


Reg_Diag(model1)[c(20,93,128,143,164),]

data2<-data[ - c(20,93,128,143,164),]
model2<-lm(distance~.,data2)
summary(model2)

library("car")
vif(model2)

speed <- abs(data2$speed_ground-data2$speed_air)
model3<-lm(distance~ . -speed_ground - speed_air +speed,data2)
summary(model3)

library("lmtest")
dwtest(model2)

y0 = predict(model2,data_test_1,interval="prediction",level=0.95)
y0<-as.data.frame(y0)

plot(y0$fit)
points(data_test_1$distance,col='red')
lines(y0[,2],lty="dashed")
lines(y0[,3],lty="dashed")