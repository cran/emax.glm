#' Calculate parameter errors via inversion of the Hessian matrix (either pracma or numeric approximations).
#' @param params Optimal parameters
#' @inheritParams em.glm
#' @param dispersion Model dispersion parameter for over/ under-dispersed models.  Defaults to 1.
#'
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#'
#' m <- em.glm(x = x, y = y, K = 2, b.init = "random")
#' make_param_errors(m$params, x = x, y = y ,weight = c(1))
#'
#' @return Calculate the errors associated with each set of parameters.
#'
#' @export
make_param_errors <- function(params, x, y, weight, family=poisson(), method="numeric", dispersion = 1){

  K <- length(params)
  errors <- list()

  dprob <- dprob.list[[family$family]](x = x, y = y, weight = weight, linkinv = family$linkinv)

  if (length(params) == 1){
    class_probs <- matrix(rep(1, length(y)), ncol=1)
  }
  else{
    class_probs <- update_probabilities(dprob, params = params)
  }

  for (i in 1:K){

    if (method == "pracma"){
      u <- make.logLike(x, y, weight = weight, class_probs = class_probs[, i], family=family)
      hess <- pracma::hessian(u, params[[i]])
    }

    if (method == "numeric"){
      rho <- x %*% params[[i]]

      mu <- family$linkinv(rho)
      variance <-  class_probs[, i] * weight * family$variance(mu)[,1] / dispersion
      hess <- -(t(x) %*% (variance * x))
    }

    hinv <- MASS::ginv(hess, tol=1e-16)

    errors[[i]] <- sqrt(-diag(hinv))
  }

  errors
}
