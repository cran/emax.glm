## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 8*0.62
)
library(knitr)
set.seed(4002)

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("Stat-Cook/emax.glm")

## ---- eval = FALSE-------------------------------------------------------
#  em.glm(
#    x, y,
#    family = poisson(),
#    K = 2
#  )

## ---- eval = FALSE-------------------------------------------------------
#  warm.up <- small.em(
#    x, y,
#    b.init = "random", K = K,
#    repeats = 20
#  )
#  
#  params <- select_best(warm.up)
#  
#  fit.K2 <-  em.glm(
#    x, y ,
#    K = 2, b.init = params
#  )

## ---- echo = FALSE-------------------------------------------------------
library(emax.glm)

## ---- fig.height = 10, fig.width = 8-------------------------------------
x <- sim.2$data[, 1:5]
y <- sim.2$data[, 6]

pois.glm <- glm(y ~ . , data = sim.2$data, family = poisson())

## ---- echo = FALSE, fig.height = 7, fig.width = 8, fig.cap = "Residual diagnostic plots for Poisson fit"----
{
  par(mfrow = c(2, 1))
  plot(
    log(y),
    residuals(pois.glm, type = "deviance"),
    xlab = "log(Target)", ylab = "Pearson residual"
  )
  qqnorm(residuals(pois.glm, type = "deviance"))
}

## ---- results = "asis", fig.height = 8, fig.width = 8--------------------
library(emax.glm)

df <- sim.2$data

x <- as.matrix(df[, 1:5])
y <- df$y

pois.em <- em.glm(x = x, y = y, K = 2, b.init = "random", param_errors = FALSE)
dev.residuals <- residuals(pois.em, x = x, y = y, type = "deviance")
kable(summary(pois.em))

## ---- fig.cap = "Normality of EM-Poisson residuals"----------------------
qqnorm(dev.residuals)

## ---- fig.height = 8, fig.width = 8, fig.cap = "Predicted and known parameters"----
{
  par(mfrow = c(2,1))
  plot(pois.em, known_params = sim.2$p1)
  plot(pois.em, known_params = sim.2$p2)
}

## ------------------------------------------------------------------------
quality <- data.frame(
  glm = c(AIC(pois.glm), BIC(pois.glm)),
  em = c(AIC(pois.em), BIC(pois.em))
)

rownames(quality) <- c("AIC", "BIC")
kable(quality)

## ---- results = "asis"---------------------------------------------------
pois.glm <- glm(y ~ ., data = sim.1$data, family = poisson())
qqnorm(residuals(pois.glm, type = "deviance"))

## ------------------------------------------------------------------------
df <- sim.1$data

x <- as.matrix(df[,1:5])
y <- df$y

pois.em <- em.glm(
  x, y,
  family = poisson(),
  b.init = "random",
  K = 2
)

kable(summary(pois.em))

## ---- echo = FALSE, fig.cap = "Single parameter set EM fit"--------------
plot(pois.em, known_params = sim.1$params)

