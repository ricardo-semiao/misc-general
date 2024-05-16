#' Teste t generalizado
#'
#' Realizar um teste t nos coeficientes de um modelo ARMA(p,q), sensível à tendência e sazonalidade, para distribuições de estatística não-normais padrão
#'
#' @param dist A ser passado para função erros. O comportamento do erro do modelo. O nome (string) de qualqer função geradora de do pacote {stats} <https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Distributions.html>, ou "arma" para modelar o erro como um arma (o erro desse modelo é uma normal padrão) [EXPANDIR PARAR OUTRAS POSSIBILIDADES].
#' @param args A ser passado para função erros. Uma lista com os parâmetros que a função escolhida em dist precisem. Se escolhido arma, passar uma lista no formato: list(p.erros = ..vetor 1xp com os primeiros p erros.., dist = ..distribuição dos erros dentre as opções de stats.., args = ..argumentos da distribuição.., intercepto = ..valor do intercepto.., ar = ..vetor com os coeficientes ar.., ma = ..vetor com os coeficientes ma..).
#'
#' @param H0 Uma lista com a hipótese nula sobre os parâmetros intercepto, ar e ma passados em model.
#' Formato: list(intercepto = ...., ar=c(....), ma=c(....))
#' @param S Número de simulações a serem feitas para obter a distribuição empírica de Y
#' @param a O valor do nível de significãncia (quantil desejado).
#' @param Y A série real para se estimar os coeficientes.
#'
#' @param trend A ser passado para função tendencia. Uma string com o tipo de tendência a ser estimada: "poli.n" para polinômio de ordem n, "exp.n" para exponencial de ordem n, "hp.n" para filtro hp de sensibilidade n. Sem nenhuma tendência por padrão.
#' @param sazon A ser passado para função sazonalidade.  matriz com as dummies de sazonalidade [EXPANDIR PARA MODELOS DE SAZONALIDADE]. Sem nenhuma sazonalidade por padrão.
#' @param cicle Um ciclo já estimado para se controlado por.
#' @export

test.raiz.unit = function(Y, dist, args, H0, S, a=0.05, trend=FALSE, sazon=FALSE, cicle=FALSE){
  # Temos tendência etc no modelo?:
  cond.t = !identical(trend, FALSE)
  cond.s = !identical(sazon, FALSE)
  cond.c = !identical(cicle, FALSE)
  cond = H0$intercepto!=0

  k = as.numeric(H0$intercepto!=0) + length(H0$ar) + length(H0$ma) # número de coefficientes
  order = c(length(H0$ar),0,length(H0$ma)) # ordem do ARMA

  if(cond) {coefs = c(H0$ar, H0$ma, H0$intercepto)} # coeficientes da H0 com intercepto
  else {coefs = c(H0$ar, H0$ma)} # sem

  # Monte carlo, definir distribuição empírica da estatística t para cada parâmetro
  tstat = numeric()
  for(i in 1:S){
    y = simulacao.serie(dist, args, H0)
    mod = try(arima(y, order, include.mean=cond))

    t = numeric()
    for(i in 1:k){
      t = try(c(t, (mod$coef - coefs)[i]/sqrt(diag(mod$var.coef))[i]))}
    tstat = rbind(tstat, t)}
  tstat = na.omit(apply(tstat, 2, as.numeric))


  # Ver o t-valor da estimação coma série original, controlando por tendência e etc
  if(cond.t){f = tendencia(Y, trend)} else {f = 0}
  if(cond.s){s = sazonalidade(Y-f, sazon)} else {s = 0}
  if(cond.c){c = cicle} else {c=0}
  mod = arima(Y - f - s - c, order, include.mean=cond)

  T = numeric()
  for(i in 1:k){
    T[i] = (mod$coef - coefs)[i]/sqrt(diag(mod$var.coef))[i]}
  names = names(mod$coef)

  # Ver qual é o menor nível de significância que o parâmetro k aceita
  passou = numeric()
  for(i in 1:k){
    passou[i] = names(which(T[i] < quantile(tstat[,i], seq(0,1,0.005)))[1])}

  # Gráficos da distribuição da estatística t
  val.crit = numeric()
  for(i in 1:k){
    val.crit = cbind(val.crit, quantile(tstat[,i], 0.05))} # expandir pra teste uni e bi caudais

  #graphs = list()
  #for(i in 1:k){
  #  graphs[i] = list(ggplot(mapping=aes(x=tstat[,i])) + xlab(NULL) + labs(title=names[i]) +
  #    geom_density(aes(color="T-Student")) + # densidade da estatística t
  #    geom_line(stat="function", fun=dnorm, args=list(mean=mean(tstat[,i]), sd=sd(tstat[,i])), aes(color="Normal")) + #normal para comparar
  #      # intervalo de confiânça:
  #    annotate(geom="rect", xmin=val.crit[1,i], xmax=val.crit[2,i], ymin=-Inf, ymax=Inf, alpha=0.5, fill="darkslategrey", aes(color="Confiança")) +
  #    geom_vline(xintercept = val.crit[1,i], size=1.25) +
  #    geom_vline(xintercept = val.crit[2,i], size=1.25) +
  #    scale_colour_manual("Legenda", values = c("darkred", "darkblue", "darkslategrey")))} # legenda
  #names(graphs) = names

  #options(max.print=100)
  T.stat = cbind(round(T,4), passou)
  rownames(T.stat) = names
  colnames(T.stat) = c("Valor", "Signif")

  return(list(
  #  "Graphs" = graphs,
    "Val.crít" = apply(tstat, 2, quantile, probs=a),
    "T.stat" = T.stat))}

#TESTES
#Y = simulacao.serie(dist="rnorm", args=list(n=100, mean=0, sd=1), H0=list(intercepto=0,ar=c(0.5, 0.25),ma=c()))
#test.raiz.unit(dist="rnorm", args=list(n=100, mean=0, sd=1), H0=list(intercepto=0,ar=c(0.5, 0.25),ma=c()), S=1000, Y=Y)

