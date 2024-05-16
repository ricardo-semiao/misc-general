#' Gerador de erros
#'
#' Cria um vetor de erros aleatórios, criados de acordo com a distribuição escolhida e seus parâmetros.
#'
#' @param dist O comportamento do erro do modelo. O nome (string) de qualqer função geradora de do pacote {stats} <https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Distributions.html>, ou "arma" para modelar o erro como um arma (o erro desse modelo é uma normal padrão).
#' @param args Uma lista com os parâmetros que a função escolhida em dist precisem. Se escolhido arma, passar uma lista no formato: list(dist = ..distribuição dos erros dentre as opções de stats.., args = ..argumentos da distribuição.., intercepto = ..valor do intercepto.., ar = ..vetor com os coeficientes ar.., ma = ..vetor com os coeficientes ma..).
#'
#' @examples
#' erros("rnorm", args=list(n=10, mean=0, sd=1))
#' erros("arma", args=list(
#'   dist="rnorm",
#'   args=list(n=20, mean=1, sd=1),
#'   intercepto=0,
#'   ar=c(0.3, 0.2, 0.1),
#'   ma=c(0.4, 0.2))
#' )
#'
#' @export

erros <- function(dist, args) {
  if (!grepl("arma", dist)) { # Se meu erro não for um arma
    erro <- do.call(dist, args)
  } else {
    e <- do.call(args$dist, args$args)
    erro <- numeric()
    for (t in 1:length(args$ar)) {
      erro[t] <- mean(replicate(100, do.call(args$dist, args$args)))
    }
    for (t in (length(args$ar) + 1):length(e)) {
      erro[t] <- args$intercepto +
        sum(args$ar * erro[(t - length(args$ar)):(t - 1)]) +
        sum(args$ma * e[(t - length(args$ma)):(t - 1)]) +
        e[t]
    }
  }

  return(erro)
}
