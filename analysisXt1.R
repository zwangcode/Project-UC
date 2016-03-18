#plot Xt1
plot(Xt1,xlab= "Time",
     ylab="Financial Activites Employment in Ohio")

#remove trend
d1.Xt1 = diff(Xt1)
plot(d1.Xt1,xlab="Time",ylab="First Difference of Xt1");abline(h=0)

#stationary
#DF test
library(urca)
d.df=ur.df(d1.Xt1,lags=12,type='trend',selectlags = "BIC")
summary(d.df)

#ACF
acf(as.vector(d1.Xt1),lag.max=48,ci.type='ma',
    main="Autocorrelation Funnctions For Model")
#PACF
pacf(as.vector(d1.Xt1),lag.max=48,
     main="Partial Autocorrelation Funnctions For Model")
#EACF
eacf(as.vector(d1.Xt1))

#model selete
if(FALSE){
m.Xt1_10 = arima(Xt1,order = c(1,1,0))
m.Xt1_10
Box.test (m.Xt1_10$res, lag = 20, type = "Ljung")

m.Xt1_11 = arima(Xt1,order = c(1,1,1))
m.Xt1_11
Box.test (m.Xt1_11$res, lag = 20, type = "Ljung")

m.Xt1_01 = arima(Xt1,order = c(0,1,1))
m.Xt1_01
Box.test (m.Xt1_01$res, lag = 20, type = "Ljung")

#overfitted test
m.Xt1_21 = arima(Xt1,order= c(2,1,1))
m.Xt1_21
m.Xt1_12 = arima(Xt1,order= c(1,1,2))
m.Xt1_12
}
#model
m.Xt1 = arima(Xt1,order = c(1,1,1))
m.Xt1
Box.test (m.Xt1$res, lag = 20, type = "Ljung")

plot(rstandard(m.Xt1),type='o',xlab= "Time");abline(h=0)
#Res Normal
qqnorm(window(rstandard(m.Xt1),start=c(1990,1)));
qqline(window(rstandard(m.Xt1),start=c(1990,1)))
#shapiro.test(model.Xt2$res)
shapiro.test(rstandard(m.Xt1))

detectAO(m.Xt1);detectIO(m.Xt1)

Xt1[28]
#remove AO outlier
mao.Xt1 = arimax(Xt1,order = c(1,1,1),
                 xreg=data.frame(AO28=1*(seq(Xt1)==28)),
                 method = "ML")
mao.Xt1

plot(rstandard(mao.Xt1),type='o');abline(h=0)
shapiro.test(rstandard(mao.Xt1))
qqnorm(window(rstandard(mao.Xt1),start=c(1990,1)));
qqline(window(rstandard(mao.Xt1),start=c(1990,1)))

detectAO(mao.Xt1);detectIO(mao.Xt1)

#remove IO outlier
mio.Xt1 = arimax(Xt1,order = c(1,1,1),
               method = "ML",io=c(28))
mio.Xt1

plot(rstandard(mio.Xt1),type='o');abline(h=0)
shapiro.test(rstandard(mio.Xt1))
qqnorm(window(rstandard(mio.Xt1),start=c(1990,1)));
qqline(window(rstandard(mio.Xt1),start=c(1990,1)))

detectAO(mio.Xt1);detectIO(mio.Xt1)

#McLeod Li test
McLeod.Li.test(y = mio.Xt1$res)

plot(Xt1,main="Fitted Values",
     ylab="Financial Activites Employment in Ohio",
     xlab="Time")
points(fitted(mio.Xt1),col="red")