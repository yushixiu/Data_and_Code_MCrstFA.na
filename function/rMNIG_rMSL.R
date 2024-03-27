rMNIG = function(n, mu, sigma, la, nu)
{
  p=length(mu)
  z = t(mvtnorm::rmvnorm(n, sigma = sigma))
  W = GIGrvg::rgig(n, lambda = -0.5, chi = 1, psi = nu)
  zw = sweep(z, 2, sqrt(W), FUN = "*")
  x=matrix(NA, p, n)
  for (i in 1:n) x[,i] = zw[,i]+mu + la * W[i]
  return(t(x))
}

rMSL = function(n, mu, sigma, lambda){
  x = matrix(NaN, n, length(mu))
  for(i in 1:n){
    z = mvtnorm::rmvnorm(1, sigma = sigma)
    w = GIGrvg::rgig(1, lambda = 1, chi = 0, psi = 2)
    x[i,] = mu + lambda * w + z * sqrt(w)
  }
  return(x)
}
