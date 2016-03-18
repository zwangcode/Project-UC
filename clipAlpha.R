clipAlpha <- function(aj,H,L)
{
  if (aj > H) aj = H
  else if (L > aj) aj = L
  aj
}