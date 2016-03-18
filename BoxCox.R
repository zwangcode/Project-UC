BoxCox_Trans<-function(x,interval,loop=1000,epsilon= .Machine$double.eps)
  {
  Likelihood_Log<-function(x,lambda)
    {
    y_lambda<-function(x,lambda)
      {
      gm<-exp(mean(log(x)))
      if (lambda == 0)  log(x) * gm   else (gm^(1 - lambda)) * ((x^lambda) - 1)/lambda  
      }   
    y <- y_lambda(x, lambda)
    (length(y)/2) * log(((length(y) - 1)/length(y)) * var(y))
    }   
  GoldenSecSearch<-function(f,x,interval,loop, epsilon)  
    {
    t<-(sqrt(5) - 1)/2  
    a<-min(interval)  
    b<-max(interval)  
    a_l<-a+(1-t)*(b-a)  
    a_r<-a+t*(b-a)  
    f_l<-f(x,a_l)  
    f_r<-f(x,a_r)  
    i<-1   
    while(abs(b-a)>epsilon)
      {  
      i<-i+1  
      if(f_l<f_r)
        {
        b<-a_r  
        a_r<-a_l  
        f_r<-f_l   
        a_l<-a+(1-t)*(b-a)
        f_l<-f(x,a_l)
        }
      else 
        {
          a<-a_l
          a_l<-a_r
          f_l<-f_r
          a_r<-a+t*(b-a)
          f_r<-f(x,a_r)
        }
      if(i>loop) break
    }
    Result<-list()
    if(f_l<f_r)
    {
      Result$minimum<-a_l
      Result$Objective<-f_l
    }  
    else 
    {
      Result$minimum<-a_r
      Result$Objective<-f_r
    }
    Result
  }
  Output<-list()
  Output<- GoldenSecSearch(Likelihood_Log,x,interval,loop,epsilon)  
  Output
}

