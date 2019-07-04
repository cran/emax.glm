#' General Information Criteria function
#' @param em.glm An emax glm fit.
#' @inheritParams logLik.em.glm
#' @param alpha Scaling factor for information criteria (2 or ln(\emph{n}) for AIC and BIC respectively).
#' @return The IC value of the model for the given value of k.
#'
#' @export
IC.em.glm <- function(em.glm, alpha){

  p <- dim(em.glm$class_probs)[2]
  K <- length(em.glm$params)

  return(K * p * alpha - 2 * em.glm$logLik)
}

#' Calculate the BIC of the em.glm model
#'
#' @inheritParams  logLik.em.glm
#' @inheritParams stats::BIC
#' @return The BIC score of the model.
#' @examples
#' y <- c(AirPassengers)
#' n <- length(y)
#' x <- as.matrix(rep(1, n))
#' m <- em.glm(x = x, y = y, K = 2, b.init = "random")
#' BIC(m)
#'
#' @export
BIC.em.glm <- function(object, ...){

  n <- dim(object$class_probs)[1]

  return(IC.em.glm(object, log(n)))
}

#' Calculate the AIC of the em.glm model
#'
#' @inheritParams  logLik.em.glm
#' @inheritParams stats::AIC
#' @return The AIC score of the model.
#' @examples
#' y <- c(AirPassengers)
#' n <- length(y)
#' x <- as.matrix(rep(1, n))
#' m <- em.glm(x = x, y = y, K = 2, b.init = "random")
#' AIC(m)
#'
#' @export
AIC.em.glm <- function(object, ..., k = 2){

  return(IC.em.glm(object, k))
}
