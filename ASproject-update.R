#input data
initial <- read.csv("~/Dropbox/R/App Stat Project/Initialdata.csv")

#select the independent variable
n = ncol(initial) # the number of the data's matrix
analysisdata = initial[2:n] # select the value of the independent variable

#log transform
y=log(analysisdata $ hp)
#y=analysisdata $ hp

#assignment the value x1=population,x2=GDP,x3=Area,x4=income
x1 = analysisdata $ population
x2 = analysisdata $ GDP
x3 = analysisdata $ Area
x4 = analysisdata $ income

#box plot the data
boxplot(x1)
boxplot(x2)
boxplot(x3)
boxplot(x4)

View(analysisdata)
pairs(analysisdata)
cor(analysisdata)

#transform population/Area x1/x3
x13 = x1/x3

a = lm(y ~ x2 + x13 + x4,data = analysisdata)
summary(a)

#SW test
shapiro.test(a$res)

#y hat,res plot
y.res = resid(a);y.fit = predict(a)
plot(y.res~y.fit)
y.rst = rstandard(a)
plot(y.rst~y.fit)

#heteroscedastic test
library(lmtest)
#H0: have no heteroscedastic
gqtest(a)

#QQ plot
plot(a,2)

opar = par(mfrow=c(2,2))
plot(a,which = 1:4,
    caption = c("Residuals vs Fitted", "Normal Q-Q plot",
                 "Scale-Location plot", "Cook's distance plot"),
    panel = points,
    sub.caption = deparse(a$call), main = "",
    ask = prod(par("mfool"))<length(which)&&dev.interactive(),
    id.n = 3,
    labels.id = names(residuals(a)), cex.id = 0.75
     )


if (FALSE)
{
#influence Hat dig
p = 4; m = nrow(analysisdata); h <- hatvalues(a)
cf <- 1:m; cf[h > 2*p/m]


#DFFITS
p = 4; m = nrow(analysisdata); d <- dffits(a)
cf <- 1:m; cf[d > 2*sqrt(p/m)]
}

anova(a)

Reg_Diag<-function(fm){
  n<-nrow(fm$model); df<-fm$df.residual
  p<-n-df-1; s<-rep(" ", n);  
  res<-residuals(fm); s1<-s; s1[abs(res)==max(abs(res))]<-"*"
  sta<-rstandard(fm); s2<-s; s2[abs(sta)>2]<-"*"
  stu<-rstudent(fm); s3<-s; s3[abs(sta)>2]<-"*"
  h<-hatvalues(fm); s4<-s; s4[h>2*(p+1)/n]<-"*"
  d<-dffits(fm); s5<-s; s5[abs(d)>2*sqrt((p+1)/n)]<-"*"
  c<-cooks.distance(fm); s6<-s; s6[c==max(c)]<-"*"
  co<-covratio(fm); abs_co<-abs(co-1)
  s7<-s; s7[abs_co==max(abs_co)]<-"*"
  data.frame(residual=res, s1, standard=sta, s2, 
             student=stu, s3,  hat_matrix=h, s4, 
             DFFITS=d, s5,cooks_distance=c, s6, 
             COVRATIO=co, s7)
}
Reg_Diag(a)

data = analysisdata[c(-2,-45),]
y = log(data $ hp)
x1 = data $ population
x2 = data $ GDP
x3 = data $ Area
x4 = data $ income
x13 = x1 / x3

b = lm(y ~ x2 + x13 + x4, data = data)
summary(b)

#SW test
shapiro.test(b$res)

#y hat,res plot
y.res = resid(b);y.fit = predict(b)
plot(y.res~y.fit)
y.rst = rstandard(b)
plot(y.rst~y.fit)

#heteroscedastic test
library(lmtest)
#H0: have no heteroscedastic
gqtest(b)

#QQ plot
plot(b,2)

library(car)
vif(b)
XX<-cor(data[3:n-1])
kappa(XX, exact = TRUE)

#Durbin–Watson test
dwtest(b)

anova(b)

c = lm(y ~ x2+ x4, data =data)
summary(c)
#SW test
shapiro.test(c$res)


#y hat,res plot
y.res = resid(c);y.fit = predict(c)
plot(y.res~y.fit)
y.rst = rstandard(c)
plot(y.rst~y.fit)

#heteroscedastic test
library(lmtest)
#H0: have no heteroscedastic
gqtest(c)

#QQ plot
plot(c,2)

library(car)
vif(c)
XX<-cor(data[3:n-1])
kappa(XX, exact = TRUE)

#Durbin–Watson test
dwtest(c)

anova(c)

drop1(b)
drop1(b,scale = 1)
drop1(c)
drop1(c,scale = 1)