#' Estimação de tendência
#'
#' Estima a tendência de uma série de tempo Y de acordo com opções de formato de tendência.
#'
#' @param Y Um vetor numérico com a série de tempo a ser modelada.
#' @param trend Uma string com o tipo de tendência a ser estimada: "poli.n" para polinômio de ordem n, "expo.n" para exponencial de ordem n, "hpfl.n" para filtro hp de sensibilidade n. Sem nenhuma tendência por padrão.
#' @export

tendencia = function(Y, trend){
  if(grepl("poli",trend)){
    mod = trend(as.matrix(Y), substr(trend, 6, nchar(trend)))}
  else if(grepl("expo",trend)){
    mod = exp(trend(log(as.matrix(Y)), substr(trend, 6, nchar(trend))))}
  else if(grepl("hpfl",trend)){
    mod = mFilter::hpfilter(Y, as.numeric(substr(trend, 6, nchar(trend))))$trend}

  return(mod)}



#TESTES:
#a = "hpfl.1600"
#mod = tendencia(
#  Y = c(1,2,1,3,1,4,1,2,3,1,2,4,1,2,3),
#  trend = a)
#mod
#plot(c(1,2,1,3,1,4,1,2,3,1,2,4,1,2,3))
#plot(mod)
