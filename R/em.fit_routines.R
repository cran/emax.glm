#' Hessian routine
#' @inheritParams em.glm
#' @param b.list List of K-classes each entry being a k length parameter vector,
#' @param class_probs  Matrix (n x K) of normalized class probabilities.
#' @param tol.1   Tolerance of the NR minimization.
#' @param debug  Boolen flag. Turn on to check optimization steps via messages.
#' @param family GLM family to fit with.
#' @param maxiter   Maximum iterations of the NR methods for exiting before convergence.
#'
#' @examples
#' x <- model.matrix(~ 1 , data = warpbreaks)
#' y <- warpbreaks$breaks
#'
#' b.list <- list(1, 1)
#' class_probs = matrix(rep(0.5, 54*2), ncol = 2)
#'
#' em.glm_pracma_fit(x = x, y = y, b.list = b.list, class_probs = class_probs)
#'
#' @return A list of parameter values on convergence for each of k-classes.
#'
#' @export
em.glm_pracma_fit <- function(x, y, b.list, class_probs,  weight = c(1), K = 2, tol.1 = 1e-8, debug=FALSE, family=poisson(), maxiter = Inf){
  u.list <- lapply(
    1:K,
    function(i) make.logLike(
      x, y,
      weight = weight, class_probs =class_probs[,i],
      family = family
    )
  )

  # Optimize each classes paramteres via Newton-Raphson
  for (i in 1:K){
    b.list[[i]] <- em.fit_pracma(
      u.list[[i]], b.list[[i]],
      x, y,
      weight = weight,
      class_probs = class_probs[,i],
      tol=tol.1, debug = debug,
      family = family, maxiter=maxiter
    )
  }

  return(b.list)
}

#' Numeric approximation routine
#'
#' @inheritParams em.glm_pracma_fit
#' @examples
#' x <- model.matrix(~ 1 , data = warpbreaks)
#' y <- warpbreaks$breaks
#'
#' b.list <- list(1, 1)
#' class_probs = matrix(rep(0.5, 54*2), ncol = 2)
#'
#' em.glm_numeric_fit(x = x, y = y, b.list = b.list, class_probs = class_probs)
#'
#' @return A list of parameter values on convergence for each of k-classes.
#'
#' @export
em.glm_numeric_fit <- function(x, y, b.list, class_probs,  weight = c(1), K = 2, tol.1 = 1e-8, debug=TRUE, family=poisson(), maxiter=Inf){
  for (i in 1:K){
    b.list[[i]] <- em.fit_numeric(
      b.list[[i]],
      x, y, weight = weight,
      class_probs = class_probs[,i],
      tol=tol.1, debug = debug,
      family = family, maxiter = maxiter
    )
  }

  return(b.list)
}
