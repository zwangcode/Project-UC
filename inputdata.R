input <- read.table("~/Dropbox/R/Time Series Project/input.txt", quote="\"")
output <- read.table("~/Dropbox/R/Time Series Project/output.txt", quote="\"")

Xt1 <- ts(input$V1, start = c(1990, 1), end = c(2005, 12), frequency=12)
Xt2 <- ts(output$V1, start = c(1990, 1), end = c(2005, 12), frequency=12)

cbindata = ts.intersect(Xt1,Xt2)

plot(cbindata, yax.flip=T,
     main = "
     Xt1=Financial Activities,Xt2=Natural Resources and Mining",
     xlab= "Time")

library("TSA")