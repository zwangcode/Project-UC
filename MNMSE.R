# simulation #
# Subset #
n = nrow(ycount)
zz1 = 1:n
zz2 = rep(1:5, ceiling(n/5))[1:n]
set.seed(7046001)
zz2 = sample(zz2,n)
#
NMSE=rep(0,5) ->NMSE0
for(i in 1:5){
m = zz1[zz2==i]
#############################
# data1 = data.2[-m,]
# data2 = data.2[m,]
# data1 = data.2[-m,-2]
# data2 = data.2[m,-2]
# C GLMP
# model <- glm(count ~ .,family="poisson",data1)
# D GLMP
# model <- glm(count ~ .,family="poisson",data1)
# E NB
# model <- glm.nb(count ~ ., data1)
# E.1
# model <- glm.nb(count ~ bin + V2 + V4 + V5, data1)
# F NB
# model <- glm.nb(count ~ ., data1)
# G ZIP
# model = zeroinfl(count~V2+V3+V4+V5 | V2+V3+V4+V5,data1)
# H ZINB
# model = zeroinfl(count~V2+V3+V4+V5 | V2+V3+V4+V5,data.2,dist="negbin")
# I LM
# model = lm(count~.,data1)
#############################

y0 = predict(model,data1)
y1 = predict(model,data2)
NMSE0[i] = mean((data1[,1]-y0)^2)/mean((data1[,1]-mean(data1[,1]))^2)
NMSE[i] = mean((data2[,1]-y1)^2)/mean((data2[,1]-mean(data2[,1]))^2)
}
(MNMSE0 = mean(NMSE0));(MNMSE=mean(NMSE))