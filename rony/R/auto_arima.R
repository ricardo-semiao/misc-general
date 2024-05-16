#' Auto ARIMA por Minimização do AIC
#'
#' Encontra as parâmetros p, q e d do modelo ARIMA que minimizem a estatística AIC.
#'
#' @param y A série real para se estimar os coeficientes.
#' @param pmax a maior ordem do ar a ser estimado para procurar o AIC mínimo.
#' @param qmax a maior ordem do ma a ser estimado para procurar o AIC mínimo.
#' @param dmax a maior ordem da diferença a ser estimada para procurar o AIC mínimo.
#'
#' @examples
#' y = simular_serie(
#'   dist="rnorm",
#'   args=list(n=100, mean=0, sd=1),
#'   H0=list(intercepto=0,ar=c(0.5, 0.25),ma=c())
#' )
#' auto_arima(y, pmax=3, qmax=3, dmax=1)
#'
#' @export

auto_arima <- function(y, pmax = 6, qmax = 6, dmax = 2) {
  AIC <- list()
  for (d in 0:dmax) {
    AIC[[d + 1]] <- matrix(data = 0, nrow = pmax + 1, ncol = qmax + 1)
  }

  for (d in 0:dmax) {
    for (p in 0:pmax) {
      for (q in 0:qmax) {
        AIC[[d + 1]][p + 1, q + 1] <- try(stats::arima(y, c(p, d, q))$aic, silent = TRUE)
      }
    }
    AIC[[d + 1]] <- apply(AIC[[d + 1]], 2, as.numeric)
    AIC[[d + 1]][is.na(AIC[[d + 1]])] <- Inf
    colnames(AIC[[d + 1]]) <- 0:qmax
    rownames(AIC[[d + 1]]) <- 0:pmax
  }
  names(AIC) <- 0:dmax

  if (dmax > 0) {
    i <- 0
    while (i < dmax) {
      if (min(AIC[[i + 1]]) < min(AIC[[i + 2]])) {
        d <- i
      } else {
        d <- i + 1
      }
      i <- i + 1
    }
  }
  p <- (which(AIC[[d + 1]] == min(AIC[[d + 1]]), arr.ind = TRUE) - 1)[1]
  q <- (which(AIC[[d + 1]] == min(AIC[[d + 1]]), arr.ind = TRUE) - 1)[2]

  a <- stats::arima(y, order = c(p, d, q))
  H0 <- list(intercepto = a$coef[p + q + 1], ar = a$coef[1:p], ma = a$coef[(p + 1):(p + q)])

  Estimacao <- y - a$residuals
  Indice <- 1:length(y)

  g <- ggplot2::ggplot(mapping = ggplot2::aes(y = Estimacao, x = Indice)) +
    ggplot2::geom_line(size = 0.5)
  plot(g)

  return(list(
    "Model" = a,
    "Plot" = g,
    "Coefficients" = H0,
    "Parameters" = c("p" = p, "d" = d, "q" = q)
  ))
}
