#plot Xt2
plot(Xt2,xlab= "Time",
     ylab="Natural Resources and Mining Employment in Ohio")

library(urca)
d.df=ur.df(Xt2,lags=12,type='trend',selectlags = "BIC")
summary(d.df)

#ACF
acf(as.vector(Xt2),lag.max=48,ci.type='ma',
    main="Autocorrelation Funnctions For Model")
#PACF
pacf(as.vector(Xt2),lag.max=48,
     main="Partial Autocorrelation Funnctions For Model")
#EACF
eacf(as.vector(Xt2))

m.Xt2 = arima(Xt2,order = c(1,0,1))
m.Xt2
Box.test (m.Xt2$res, lag = 20, type = "Ljung")

m.Xt2 = arima(Xt2,order = c(1,0,0))
m.Xt2
Box.test (m.Xt2$res, lag = 20, type = "Ljung")
McLeod.Li.test(y = m.Xt2$res)

predict(m.Xt2,n.ahead=12)
Xt2.p = predict(m.Xt2,n.ahead=12)$pred
se = predict(m.Xt2,n.ahead=12)$se

upr = Xt2.p+1.96*se
lwr = Xt2.p-1.96*se

mina = min(Xt2,lwr)
maxa = max(Xt2,upr)

Xt20 <- read.table("~/Dropbox/R/Time Series Project/Xt20", quote="\"")
Xt20 <- ts(Xt20$V1,start = c(2006, 1), end = c(2006, 12), frequency=12)

plot(Xt2.p,
     ylim=c(mina,maxa),ylab="Natural Resources and Mining")
points(Xt20)
lines(upr,col='red',lty="dashed")
lines(lwr,col='red',lty="dashed")