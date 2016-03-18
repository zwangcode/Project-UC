Landing <- read.csv("C:/Users/Wang/Downloads/Landing.csv")
attach(Landing)
###################
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
###################
modela1<-lm(A$distance~duration+no_pasg+speed_ground+speed_air+height+pitch,A)
summary(modela1)
pa <- predict(modela1)
ap <- na.omit(A$pitch)
par(mfrow=c(1,2))
plot(ap,pa)
line(ap,pa)
abline(1728.3,175.6)

modelb1<-lm(B$distance~duration+no_pasg+speed_ground+speed_air+height+pitch,B)
summary(modelb1)
pb <- predict(modelb1)
bp <- na.omit(B$pitch)
plot(bp,pb)
line(bp,pb)
abline(1771.3,212.9)
###################
model1<-lm(data$distance~.,data)
summary(model1)

model2<-lm(data$distance~aircraft+duration+no_pasg+speed_ground+speed_air+height+pitch+pitch*aircraft,data)
summary(model2)
###################
model3 = step(model2,direction="backward")
(AIC(model3))
summary(model3)

model4 = step(model2,direction="forward")
(AIC(model4))
summary(model4)