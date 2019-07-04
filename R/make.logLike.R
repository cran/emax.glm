#' Construct a log-likelihood function in the parameters b, for the given link family.
#'
#' @inheritParams em.glm
#' @param class_probs An \emph{n} length vector  of probabilities for the proposed model.
#' @param family The GLM family being considered.
#' @return A model-likelihood function.  Expects one argument which is a *p* length vector of parameters.
#'
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#'
#' make.logLike(x, y)
#'
#' @export
make.logLike <- function(x, y, weight = c(1), class_probs = c(1), family = poisson){


  if (typeof(family) == "closure"){
    model <- family()
  }
  else {
    model <- family
  }

  model.logLike <- function(b){
    rho <- x%*%b
    loglike <- weight * (y * rho - model$linkinv(rho))
    sum(class_probs * loglike)
  }

  model.logLike
}
