beta0 <-0
beta1 <- 0
#beta=c(-60.717,34.270)
beta=c(0,0)
n<-c(6,13,18,28,52,53,61,60)
logdose<-c(1.6907,1.7242,1.7552,1.7842,1.8113,1.8369,1.8610,1.8839)
N<-c(59,60,62,56,63,59,62,60)

X <- as.matrix(cbind(rep(1,8),logdose),crow=2)
beta <- as.vector(beta)
#====================================
p <- 1 / (1 + exp( -X %*% beta ))
W <- diag(as.vector(N * p * (1-p)))
miu = N*p
beta_new <- beta
beta <- beta + solve(t(X) %*% W %*% X) %*% t(X) %*% (n-miu)
norm <- sqrt(t(beta-beta_new) %*% (beta-beta_new))

f1 <- sum((n-n*p)*logdose)
f2 <- -sum(n*p*(1-p)*logdose^2)
