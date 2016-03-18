selectJrand <- function(i,m)
{
  j = i
  while (j==i)
  {
    j = floor(runif(1, min=0, max=m))
  }
  j
}