Xcov <- read.table("C:/Users/Wang/Desktop/Xcov.txt", quote="\"")
ycount <- read.table("C:/Users/Wang/Desktop/ycount.txt", header=TRUE, quote="\"")
ybin <- read.table("C:/Users/Wang/Desktop/ybin.txt", header=TRUE, quote="\"")

attach(Xcov)
attach(ycount)
attach(ybin)
#############################
# Data Analysis #
#############################
hist(ybin[,1])
hist(ycount[,1],)

table(ybin$bin)
table(ycount$count)
barplot(table(ycount$count))

#############################
# BIN #
# Logit and Probit #
#############################
data.1 <-cbind(ybin,Xcov[,-1])
#############################
# Logit #
#############################
a = glm(bin ~ .,data.1,family = "binomial")
summary(a)

a.1 =step(a)
summary(a.1)

z=(predict(a.1,data.1,type="response")>.5)
u = rep("0",4863);u[!z]="1"
(zz = table(data.1[,1],u))
(sum(zz)-sum(diag(zz)))/sum(zz)

#############################
# Proit #
#############################
b = glm(bin~., data.1,family = binomial(link = "probit"))
summary(b)
z=(predict(b,data.1,type="response")>.5)
u = rep("0",4863);u[!z]="1"
(zz = table(data.1[,1],u))
(sum(zz)-sum(diag(zz)))/sum(zz)


#############################
# quasibinomial
#############################
q = glm(bin~., data.1,family = quasibinomial)
summary(q)
z=(predict(q,data.1,type="response")>.5)
u = rep("0",4863);u[!z]="1"
(zz = table(data.1[,1],u))
(sum(zz)-sum(diag(zz)))/sum(zz)

#############################
# beta-binomial
#############################
library(aod)
beta = betabin(cbind(bin,1-bin)~.,data.1,random=~1)
summary(beta)
#############################
z=(predict(beta,data.1,type="response")>.5)
u = rep("0",4863);u[!z]="1"
(zz = table(data.1[,1],u))
(sum(zz)-sum(diag(zz)))/sum(zz)

#############################
# Count Data
#############################
data.2 <-cbind(ycount,ybin,Xcov[,-1])
data.2$bin <- factor(data.2$bin)
#############################
# Poisson #
#############################
c = glm(count ~ .,family="poisson",data.2)
summary(c)
c.1 = step(c)
d = glm(count ~ .,family = "poisson",data.2[,-2])
summary(d)
d.1 =step(d)
summary(d.1)
#############################
# NB #
#############################
library(MASS)
e = glm.nb(count ~ ., data.2)
summary(e)
e.1 = step(e)
summary(e.1)

f = glm.nb(count~.,data.2[,-2])
summary(f)
f.1 = step(f)
summary(f.1)

#############################
# ZIP #
library(pscl)
g = zeroinfl(count~V2+V3+V4+V5 | V2+V3+V4+V5,data.2)
summary(g)
#############################
# ZINB #
h = zeroinfl(count~V2+V3+V4+V5 | V2+V3+V4+V5,data.2,dist="negbin")
summary(h)
#############################
# simulation #
#############################

