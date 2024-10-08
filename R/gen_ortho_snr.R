gen_ortho_snr <- function(n, p, p1=floor(p/2), beta, family=c("gaussian","binomial"), SNR=1,
                      signal = c("homogeneous","heterogeneous")) {
  family <- match.arg(family)
  signal <- match.arg(signal)
  if (p > n) stop("Cannot generate orthonormal design matrix if p is larger than n")
  
  # Gen X
  X <- sqrt(n)*qr.Q(qr(matrix(rnorm(n*p), n, p)))
  
  # Gen beta
  if (missing(beta) || length(beta)==1) {
    j <- 1:p
    s <- c(-1,1)[j%%2+1]
    b <- (j <= p1)
    if (missing(beta)) {
      if (signal=="heterogeneous") b <- b*rev(j)
      b <- b*s
      beta <- b*sqrt(SNR)/sqrt(drop(crossprod(b)))
    } else {
      beta <- b*s*beta
    }
  } else {
    beta <- beta*sqrt(SNR)/sqrt(drop(crossprod(beta)))
  }
  
  # Gen y
  y <- gen_y(X%*%beta, family=family, sigma=1)
  
  # Label and return
  w <- 1 + floor(log10(p))
  vlab <- paste0('V', formatC(1:p, format='d', width=w, flag='0'))
  colnames(X) <- names(beta) <- vlab
  list(X=X, y=y, beta=beta, family=family)
}