#' Deviance residuals for an 'em.glm' object.
#' @param object An 'em.glm' object.
#' @inheritParams em.glm
#' @inheritParams stats::residuals
#' @param type Residual type - either deviance or Pearson's residuals.
#' @return An n length vector of residuals.
#'
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#'
#' m <- em.glm(x = x, y = y, K = 2, b.init = "random")
#'
#' residuals(m, x = x, y = y)
#'
#' @export
residuals.em.glm <- function(object, x, y, weight = c(1), type="deviance", ...){

  family <- object$family

  mu <- predict(object, x=x, y=y, weight=weight, type="rate")

  d.deviances <- list(
    "poisson" = function(y, mu, weight) dpois(y, y, log = TRUE) - dpois(y, weight * mu, log = TRUE),
    "binomial" = function(y, mu, weight) dbinom(y, weight, y / weight, log = TRUE) - dbinom(y, weight, mu, log = TRUE)
  )

  if (type == "deviance"){
    dev <- d.deviances[[family$family]]
    return(sign(y - weight * mu) * sqrt(2 * dev(y=y, mu=mu, weight = weight)))
  }
  if (type == "pearson"){
    return((y - weight * mu) / sqrt(weight * family$variance(mu)))
  }
  else{
    stop("Type not implemented.")
  }
}

#' Model deviance (calculated from deviance residuals)
#' @inheritParams residuals.em.glm
#' @inheritParams stats::deviance
#' @return The model deviance statistic.
#'
#' @export
deviance.em.glm <- function(object, x, y, weight = c(1), ...){
  resid <-residuals(object, x=x, y=y, weight=weight, type="deviance")

  return(sum(resid^2))
}
