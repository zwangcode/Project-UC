"kprod" <- function( )
  {
  kp<- function(x, y = NULL)
  {
    crossprod(x)
  }
  return(new("kprod",.Data=kp,kpar=list()))
  }


"poly.kernel" <-
  function(x, y=x, param.kernel = 1)
  {
    if(is.null(param.kernel))
      param.kernel <- 1
    K= if(param.kernel == 1)
      x %*% t(y)
    else (x %*% t(y) + 1)^param.kernel
    if(param.kernel==1) attr(K,"linear")=TRUE
    K
  }

"radial.kernel" <-
  function(x, y=x, param.kernel = 1/p)
  {
    n <- nrow(x)
    m <- nrow(y)
    p <- ncol(x)
    normx <- drop((x^2) %*% rep(1, p))
    normy <- drop((y^2) %*% rep(1, p))
    a <- x %*% t(y)
    a <- (-2 * a + normx) + outer(rep(1, n), normy, "*")
    exp( - a* param.kernel)
  }