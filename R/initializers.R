#' Method to initialize EM parameters.  Carries out a single GLM fit and applies random noise to form starting space.
#' @inheritParams em.glm
#' @return A K-length list, each holding parameters.
#' @examples
#' x <- model.matrix(~ 1 + factor(wool) + factor(tension), data = warpbreaks)
#' y <- warpbreaks$breaks
#'
#' init.fit(y = y, x = x, K = 2)
#'
#' @export
init.fit <- function(y, x, K, weight = c(1), family=poisson(), noise = 1){

  if (length(weight) != length(y)){
    weight <- rep(1, length(y))
  }

  if (family$family == "poisson"){
    model <- glm(y ~ -1 + x, family=family, offset = log(weight))
  }
  else{
    rate <- y / weight
    model <- glm(rate ~ -1 + x, family=family, weights = weight)
  }
  param <- coef(model)
  lapply(1:K, function(i) param * rnorm(length(param), 1, noise))
}

#' Method to initialize EM parameters.  Purely standard normal noise.
#'
#' @inheritParams em.glm
#' @return A K-length list, each holding parameters.
#'
#' @examples
#' x <- model.matrix(~ 1 + factor(wool) + factor(tension), data = warpbreaks)
#'
#' init.random(x = x, K = 2)
#'
#' @export
init.random <- function(x, K, noise = 1){

  p <- dim(x)[2]
  lapply(1:K, function(i) rnorm(p, 1, noise))
}
