n = nrow(ycount)
zz1 = 1:n
zz2 = rep(1:5, ceiling(n/5))[1:n]
set.seed(7046001)
zz2 = sample(zz2,n)
