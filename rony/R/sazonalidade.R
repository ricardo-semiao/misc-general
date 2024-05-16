#' Estimação de sazonalidade
#'
#' Estima a sazonalidade de uma série de tempo y de acordo com opções de formato de sazonalidade, sem intercepto.
#'
#' @param y Um vetor numérico com a série de tempo sem tendência a ser modelada.
#' @param sazon Uma matriz com as dummies de sazonalidade. Sem nenhuma sazonalidade por padrão.
#'
#' @examples
#' #sazonalidade(y = c(1,2,1,3,1,4,1,2,3,1,2,4,1,2,3), sazon = c(1,0,0,1,0,0,1,0,0,1,0,0,1,0,0)
#'
#' @export

sazonalidade <- function(y, sazon) {
  stats::fitted(stats::lm(y ~ . - 1, data = as.data.frame(sazon)))
}
