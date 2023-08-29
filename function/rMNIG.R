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