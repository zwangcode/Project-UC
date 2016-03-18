plot(employ.res,type="l");abline(h=0)

acf(employ.res,ci.type= "ma",lag.max=150)
pacf(employ.res,lag.max=150)

library(urca)
d.df=ur.df(employ.res,lags=12,type='trend',selectlags = "BIC")
summary(d.df)

d1.employ.res= diff(employ.res)
d.df=ur.df(d1.employ.res,lags=12,type='trend',selectlags = "BIC")
summary(d.df)

plot(d1.employ.res,type="l");abline(h=0)

acf(d1.employ.res,ci.type= "ma",
    main="Autocorrelation Funnctions For Residuals Model")
pacf(d1.employ.res,
     main="Partial Autocorrelation Funnctions For Residuals Model")
eacf(d1.employ.res)

  m.res = arima(employ.res,order=c(2,1,2))
  m.res

  m.res = arima(employ.res,order=c(2,1,0))
  m.res

  m.res = arima(employ.res,order=c(2,1,0),transform.pars = FALSE,fixed=c(0,NA))
  m.res

Box.test (m.res$res, lag = 20, type = "Ljung")
McLeod.Li.test(y = m.res$res)

plot(rstandard(m.res),type="l");abline(h=0)

shapiro.test(rstandard(m.res))
qqnorm(window(rstandard(m.res)));qqline(window(rstandard(m.res)))

detectAO(m.res);detectIO(m.res)

mio.res = arima(employ.res,order=c(2,1,0),
                transform.pars = FALSE,fixed=c(0,NA,NA),io=c(149))
mio.res


runs(rstudent(mio.res))

plot(employ.res,main="Fitted Values",
     ylab="N(t)",
     xlab="Time",type="l");
lines(fitted(mio.res),col="red")

