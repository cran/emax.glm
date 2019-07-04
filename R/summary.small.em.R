#' Summarize a small.em class
#' @param object A small.em class
#' @inheritParams summary.em.glm
#' @return A data frame of log-likeliood, BIC and model index.
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#'
#' warm_up <- small.em(x = x, y = y, K = 2, b.init = "random", sample.size = 50)
#' summary(warm_up)
#'
#' @export
summary.small.em <- function(object, ...){

  K <- length(object)
  ll <- as.numeric(sapply(1:K, function(i) object[[i]]$logLik))
  bic <- sapply(1:K, function(i) object[[i]]$bic)

  data.frame(
    index = 1:K,
    logLike = ll,
    bic = bic
  )
}
