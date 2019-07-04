#' Summarize EM glm coefficients.
#' @param object An em.glm object
#' @param ... additional arguments affecting the summary produced
#' @return List of each classes coefficients and errors.
#'
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#'
#' m <- em.glm(x = x, y = y, K = 2, b.init = "random")
#'
#' summary(m)
#'
#' @export
summary.em.glm <- function(object, ...){

  coefs <- list()

  names <- c(sapply(
    1:object$K,
    function(i) sapply(1:length(object$params[[i]]), function(j) paste("K", i, " - X", j, sep=""))
  ))

  coefs$params <- unlist(object$params)
  coefs$errors <- unlist(object$errors)

  df <- data.frame(coefs)
  rownames(df) <- names
  colnames(df) <- c("Parameter", "Error")


  df
}


