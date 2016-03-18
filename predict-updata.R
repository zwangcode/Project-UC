n1 = nrow(data)
zz1 = 1:n1+1
zz2 = rep(1:25,ceiling(n1/25))[1:n1]
num = sample(1:65535,1,replace=T)
set.seed(num);
zz2 = sample(zz2,n1)

m = zz1[zz2==1]
newX <- data[m,c(2:5)]
x1 = newX $ population
x2 = newX $ GDP
x3 = newX $ Area
x4 = newX $ income
x13 = x1/x3
y0 = predict(c,newX,interval="prediction",level=0.95)
y0
