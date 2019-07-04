#' Predict values from an 'em.glm' model.
#' @param object An em.glm fit object.
#' @inheritParams em.glm
#' @param type Prediction type.  Currently can be 'count' for the weighted prediction, 'rate' for the expected rate or 'rho' for the linear predictor.
#' @param ... optionally more fitted model objects.
#' @return N-length vector of predicted terms.
#'
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#'
#' m <- em.glm(x = x, y = y, K = 2, b.init = "random")
#'
#' predict(m, x = x, y = y, weight = c(1))
#'
#' @export
predict.em.glm <- function(object, x, y, weight, type = "count", ...){

  family <- object$family

  dprob <- dprob.list[[family$family]](x=x, y=y, weight=weight, linkinv = family$linkinv)

  class_probs <- update_probabilities(dprob = dprob, params = object$params)

  #rho <- diag(x %*% sapply(em.glm$params, function(i) i) %*% t(class_probs))
  rho.matrix <- class_probs * (x %*% sapply(object$params, function(i) i))
  rho <- apply(rho.matrix, 1, sum)

  if (type == "count"){
    return(weight * family$linkinv(rho))
  }
  if (type == "rate"){
    return(family$linkinv(rho))
  }
  if (type == "rho"){
    return(rho)
  }

}


