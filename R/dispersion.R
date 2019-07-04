#' Pearson-based dispersion  measurements of an 'em.glm' model.
#'
#' @param em.glm An 'em.glm' object.
#' @inheritParams em.glm
#' @examples
#' x <- model.matrix(~ factor(wool) + factor(tension), warpbreaks)
#' y <- warpbreaks$breaks
#' m <- em.glm(x = x, y = y, K = 2, b.init = "random")
#' dispersion(m, x, y, weight = c(1))
#'
#' @return A list of dispersion parameters for the model.
#'
#' @export
dispersion <- function(em.glm, x, y, weight){
  resid <- residuals(em.glm, x = x, y = y, weight = weight, type = "pearson")

  stat <- sum(resid^2)

  d <- dim(x)

  # TODO - Is this true?  Can you do underdispersion and overdispersion?
  overdisp_p_value <- pchisq(stat, d[1] - d[2], lower.tail = FALSE)
  underdisp_p_value <- pchisq(stat, d[1] - d[2], lower.tail = TRUE)

  list(
    "ssr" = stat,
    "pearson_dispersion" = stat / (d[1] - d[2]),
    "Over-dispersed pvalue" = overdisp_p_value,
    "Under-dispersed pvalue" = underdisp_p_value
  )
}


