## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 8*0.62
)
set.seed(3000)
library(knitr)

## ------------------------------------------------------------------------
library(emax.glm)

warm_up <- small.em(
  x = sim.3$x, y = sim.3$y, 
  K = 2, b.init = "random", 
  weight = sim.3$exposure, sample.size = 1000,
  repeats = 30, maxiter = 5
)

df <- summary(warm_up)
kable(head(df))

## ------------------------------------------------------------------------
plot(df$index, df$bic, xlab = "index", ylab = "BIC")

## ------------------------------------------------------------------------

params <- select_best(warm_up)
em <- em.glm(
  x = sim.3$x, y = sim.3$y, 
  K = 2, b.init = params, 
  weight = sim.3$exposure
)


## ---- echo = FALSE, fig.height = 10, fig.cap = "Fitted Parameters and known values "----
{
  par(mfrow = c(2,1))
  plot(em, known_params = sim.3$p1)
  plot(em, known_params = sim.3$p2)
}

