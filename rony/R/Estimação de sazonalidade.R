#' Estimação de sazonalidade
#'
#' Estima a sazonalidade de uma série de tempo Y de acordo com opções de formato de sazonalidade, sem intercepto.
#'
#' @param Y Um vetor numérico com a série de tempo sem tendência a ser modelada.
#' @param sazon Uma matriz com as dummies de sazonalidade [EXPANDIR PARA MODELOS DE SAZONALIDADE]. Sem nenhuma sazonalidade por padrão.
#' @export

sazonalidade = function(Y, sazon){
  mod = fitted(lm(Y ~ . -1, data=as.data.frame(sazon)))
  return(mod)}

#TESTES:
#sazonalidade(
#  Y     = c(1,2,1,3,1,4,1,2,3,1,2,4,1,2,3),
#  sazon = c(1,0,0,1,0,0,1,0,0,1,0,0,1,0,0))
