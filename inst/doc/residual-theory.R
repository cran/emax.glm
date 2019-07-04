## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 8*0.62
)

## ---- eval = FALSE-------------------------------------------------------
#  rho <- x %*% B
#  mu <- family()$linkinv(rho)
#  
#  # Poisson deviance
#  abs(y - w * mu) * sqrt(2 * (dpois(y, y, log = TRUE) - dpois(y, w * mu, log = TRUE)))
#  
#  # Binomial deviance
#  abs(y - w * mu) * sqrt(2 * (dbinom(y, w, y / w, log = TRUE) - dbinom(y, w, mu, log = TRUE)))

