#' Simulação de série
#'
#' Recebe os parâmetros de um modelo ARMA e o formato do erro e simula uma série de tempo.
#'
#' @param dist A ser passado para função erros. O comportamento do erro do modelo. O nome (string) de qualqer função geradora de do pacote {stats} <https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Distributions.html>, ou "arma" para modelar o erro como um arma (o erro desse modelo é uma normal padrão) [EXPANDIR PARAR OUTRAS POSSIBILIDADES].
#' @param args A ser passado para função erros. Uma lista com os parâmetros que a função escolhida em dist precisem. Se escolhido arma, passar uma lista no formato: list(p.erros = ..vetor 1xp com os primeiros p erros.., dist = ..distribuição dos erros dentre as opções de stats.., args = ..argumentos da distribuição.., intercepto = ..valor do intercepto.., ar = ..vetor com os coeficientes ar.., ma = ..vetor com os coeficientes ma..).
#'
#' @param H0 Uma lista com a hipótese nula sobre os parâmetros intercepto, ar e ma passados em model.
#' Formato: list(intercepto = ...., ar=c(....), ma=c(....))
#' @export

# arima, itsmr, mFilter
simulacao.serie = function(dist, args, H0){
  erros = erros(dist, args)
  Y = numeric()
  for(t in 1:length(H0$ar)){Y[t]=erros[t]}
  for(t in (length(H0$ar)+1):length(erros)){
    Y[t] = H0$intercepto + sum(rev(H0$ar)*Y[(t-length(H0$ar)):(t-1)]) + sum(rev(H0$ma)*erros[(t-length(H0$ma)):(t-1)]) + erros[t]}
  return(Y)}

#TESTES:
#simulacao.serie(
#  dist = "rnorm",
#  args = list(n=20, mean=0, sd=1),
#  H0 = list(intercepto = 0, ar = c(0.3,0.2,0.1), ma = c(0.4,0.2)))
