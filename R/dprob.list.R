#' Build a Poisson log likelihood
#' @inheritParams em.glm
#' @param linkinv  Inverse link function desired
#' @param log     Boolean flag.  If TRUE returns the log dist.
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#' make.dpois(x, y)
#'
#' @return A function to  calculate (log) likelihood for a given set of parameters under a Poisson model.
#'
#' @export
make.dpois <- function(x, y, linkinv=poisson()$linkinv, weight = c(1), log=FALSE){
  function(params){
    sapply(
      params,
      function(i) dpois(y, weight * linkinv(x %*% i), log=log)
    )
  }
}

#' Build a Binomial log likelihood
#' @inherit make.dpois
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#' make.dbinom(x, y)
#'
#' @return A function to  calculate (log) likelihood for a given set of parameters under a Binomial model.
#'
#' @export
make.dbinom <- function(x, y, linkinv=binomial()$linkinv, weight = 1, log=FALSE){
  function(params){
    sapply(
      params,
      function(i) dbinom(y, weight, linkinv(x %*% i), log=log)
    )
  }
}

#' List of distribution functions accessed by family name ("poisson" or "binomial").
#'
#' @export
dprob.list <- list(
  "poisson" = make.dpois,
  "binomial" = make.dbinom
)
