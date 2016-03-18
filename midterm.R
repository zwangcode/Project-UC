logdose<-c(1.690,1.7242,1.7552,1.7842,1.8113,1.8369,1.8610,1.8839)
N<-c(59,60,62,56,63,59,62,60)
killed<-c(6,13,18,28,52,53,61,60)
n<-c(6,13,18,28,52,53,61,60)
beetle <-as.data.frame(cbind(logdose,N,killed))
md1<-glm(cbind(killed,N-killed)~logdose,data=beetle,family=binomial)

summary(md1)
# 1.Based on the discussion in Slides 7 -10 of Lecture 7 Logistic Regression I,
# construct your own R codes to carry out the Newton Raphson methods to obtain
# the MLE of the regression coefficients and the corresponding standard errors
# in model 1.
X <- as.matrix(cbind(rep(1,8),logdose),crow=2)

Newtons<-function (beta0,beta1,ep=1e-5, it_max=100)
{
  index<-0; k<-1
  beta <- as.vector(c(beta0,beta1))
  while (k<=it_max)
  {
    beta_new <- beta
# compute vector p of probabilities for logistic regression with logit link
    p <- 1 / (1 + exp( -X %*% beta ))
# variance matrix
    W <- diag(as.vector(N * p * (1-p)))
#  N * p is mean
    miu = N*p
# NR method  
    beta <- beta + solve(t(X) %*% W %*% X) %*% t(X) %*% (n-miu);
#    
    norm <- sqrt(t(beta-beta_new) %*% (beta-beta_new))
    if (norm < ep)
    {
      index<-1; break
    }
    k<-k+1
  }
  c(beta = beta, True = index, it = k)
}
# Initial value both equal zero
Newtons(0,0)
# Standard Error
NT <-Newtons(0,0)
md <-summary(md1)$coefficients
ebeta1 = (NT[1]-md[1])/sqrt(md[1,2])
ebeta2 = (NT[2]-md[2])/sqrt(md[2,2])
# 2.Based on the given R output, comments on model fitting of Model 1. 
#====================================================================
pred_logit <- predict(md1, beetle, type = "response")
# show the logit fit to the data in a Figure
plot(N*pred_logit,type = "l",col = "red")
points(killed)

# 3.Obtain raw residuals, Pearson residuals, 
# Pearson standardized residuals (adjusted residuals), 
# and deviance standardized residuals. Check if there is any outlier. 
#====================================================================
# raw residuals,
r_res <- beetle$killed-beetle$N*md1$fitted

# Pearson residuals, 
P_res<-(beetle$killed-beetle$N*md1$fitted)/sqrt(beetle$N*md1$fitted*( 1 - md1$fitted))

# Pearson standardized residuals (adjusted residuals)
a_res <- (r_res)/sqrt(var(r_res))

# deviance standardized residuals. 
h <- influence(md1)$hat
d_res <- residuals(md1)/sqrt(1-h)
# Check if there is any outlier. 
par(mfrow = c(2, 2))
boxplot(r_res)
boxplot(P_res)
boxplot(a_res)
boxplot(d_res)

# 4. Construct the added variable plot to check if 
# the quadratic term of logdose is needed. 
# (Note: check if some standardization in the covariates is needed.) 
#====================================================================
logdose2<-(beetle$logdose)^2
W<-diag(beetle$N * md1$fitted * ( 1 - md1$fitted))
X<-cbind(1, beetle$logdose)
H<-sqrt(W) %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% sqrt(W)
eU<-(diag(1,nrow=nrow(beetle))-H)%*%sqrt(W)%*%logdose2
plot(eU, P_res, xlab="Added variable residuals",ylab="Pearson Residuals")
# 5.Use this goodness of link test to test 
# if a logistic link model (Model 1 with only logdose) is adequate. 
#====================================================================


# 6.Clearly specify the likelihood function, 
# the log-likelihood function, the score functions, and the W matrix.
# Model 2: log[-log(1 - pi_i)] = alpha + beta*logdose_i, i = 1...8
#====================================================================


# 7.Show how the AIC values are calculated for the logit link, 
# the probit link and the complementary log-log link. 
#====================================================================
# AIC value for logit link
logaic = 2*2-2*logLik(md1)
# AIC vale for probit link
md2<-md1<-glm(cbind(killed,N-killed)~logdose,data=beetle,family=binomial(link = "probit"))
proaic = 2*2-2*logLik(md2)
# AIC vale for probit link
md3<-md1<-glm(cbind(killed,N-killed)~logdose,data=beetle,family=binomial(link = "cloglog"))
clogaic = 2*2-2*logLik(md3)
