#' #' Return the optimal model based on BIC scores
#' #' @param small.em  A 'small.em' object
#' #' @export
#' select_best <- function(small.em){
#'   s <- summary(small.em)
#'   ordered <- s[order(s$bic), ]
#'   idx <- ordered$index[[1]]
#'   small.em[[idx]]$params
#' }
