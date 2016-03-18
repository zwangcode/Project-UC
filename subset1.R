data <- data.matrix(iris)

n<-nrow(data)
m<-ncol(data)

zz1 = 1:n
zz2 = rep(c(-1,1),ceiling(n/2))[1:n]
zz2 = sample(zz2,n)
