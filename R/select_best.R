#' Select the best parameters from a set of results
#'
#' @return The parameters of the best model, as judged by log-likelihood.
#' @export
select_best <- function(x, ...){
  UseMethod("select_best")
}

#' Return the optimal model based on BIC scores
#' @param small.em  A 'small.em' object
#'
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#'
#' warm_up <- small.em(x = x, y = y, K = 2, b.init = "random", sample.size = 20)
#'
#' select_best(warm_up)
#'
#' @export
select_best <- function(small.em){
  s <- summary(small.em)
  ordered <- s[order(s$bic), ]
  idx <- ordered$index[[1]]
  small.em[[idx]]$params
}
