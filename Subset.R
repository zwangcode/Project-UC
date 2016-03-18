n<-nrow(data)
m<-ncol(data)

zz1 = 1:n
zz2 = rep(1:10,ceiling(n/10))[1:n]
zz2 = sample(zz2,n)
