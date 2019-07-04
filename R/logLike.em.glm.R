#' Calculate log-likelihood of the EM model.
#' @param object A 'em.glm' class returned by the em.glm function.
#' @param ... optionally more fitted model objects.
#' @inheritParams em.glm
#'
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#' m <- em.glm(x = x, y = y, K = 2, b.init = "random")
#' logLik(m, x, y)
#'
#' @return The log-likelihood of y given the model and x.
#'
#' @export
logLik.em.glm <- function(object, x, y, weight = c(1), ...){

  family <- object$family

  mu <- predict(object, x=x, y=y, weight=weight, type="rate")

  ll <- list(
    "poisson" = function(y, mu, weight) dpois(y, weight * mu, log = TRUE),
    "binomial" = function(y, mu, weight) dbinom(y, weight, mu, log = TRUE)
  )

  dprob <- ll[[family$family]]
  return(sum(dprob(y, mu, weight)))

}


