#' Construct normalized class properties for a given set of parameters
#' @param dprob Probability distribution function to call.  See 'dprob.list' for examples.
#' @param params List of class parameters.  Length of list is number of classes
#'
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#'
#' dprob <- make.dpois(x = x, y = y)
#' params <- list(rep(1, 4))
#'
#' update_probabilities(dprob, params)
#'
#' @return A n-by-k matrix of class probabilities (each row normalized to 1).
#'
#' @export
update_probabilities <- function(dprob, params){
  class_prob <- dprob(params)
  total_prob <- apply(class_prob, 1, sum)
  n <- length(total_prob)
  class_prob <- t(sapply(1:n, function(i) class_prob[i,] / total_prob[i]))
  class_prob[is.na(class_prob)] <- 1 / length(params)

  if (dim(class_prob)[1] == 1){
    return(t(class_prob))
  }
  else{
    return(class_prob)
  }
}
