res.p = predict(m.res,n.ahead=12)$pred
se = predict(m.res,n.ahead=12)$se

ures = res.p+1.96*se
lres = res.p-1.96*se

C = summary(m.employ)$coefficients
sigma = sqrt(deviance(m.employ))


Xt1P <- read.table("~/Dropbox/R/Time Series Project/Xt1.p", quote="\"")
Xt1.p <- ts(Xt1P$V1,start = c(193), end = c(204))

Xt2p = C[2,1]*Xt1.p + C[1,1]

L = lres+ Xt2p
U = ures + Xt2p

mina = min(Xt2,L)
maxa = max(Xt2,U)


Xt20 <- read.table("~/Dropbox/R/Time Series Project/Xt20", quote="\"")
Xt20 <- ts(Xt20$V1,start = c(193), end = c(204))

plot(res.p + Xt2p,
     ylim=c(mina,maxa),ylab="Natural Resources and Mining Employment")
points(Xt20)
lines(U,col='red',lty="dashed")
lines(L,col='red',lty="dashed")

m.m = arima(Xt1,order=c(2,1,0),transform.pars = FALSE,fixed=c(0,NA,NA),xreg=data.frame(Xt2))
m.m