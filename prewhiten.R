ccf(as.vector(cbindata[,1]),as.vector(cbindata[,2]),
    ylab='CCF',
    main="Sample CCF between Financial and Natural Resources and Mining")

x = as.vector(cbindata[,1])
y = as.vector(cbindata[,2])

model = mio.Xt1$model

filter.mod = function(x, model) {
  if (length(model$Delta) >= 1) 
    x = filter(x, filter = c(1, -model$Delta), method = "convolution", 
               circular = TRUE,
               sides = 1)
  if (length(model$phi) >= 1 && all(model$phi != 0)) 
    x = filter(x, filter = c(1, -model$phi), method = "convolution",
               circular = TRUE,
               sides = 1)
  if (length(model$theta) >= 1 && all(model$theta != 0)) 
    x = filter(x, filter = -model$theta, method = "recursive",
               circular = TRUE,
               sides = 1)
  x
}
x = filter.mod(x, model)
y = filter.mod(y, model)
ccf(as.vector(x),as.vector(y),ylab='CCF',
    main="Sample CCF of prewhitened of Financial and Natural Resources and Mining")