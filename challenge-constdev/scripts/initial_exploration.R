#==============  PACOTES E FUNÇÕES  ==============

library(tidyverse)
library(magrittr)
library(colorspace)
#library(vars)
#library(forecast)
#library(rugarch) #rugarch or fGarch
#c("defPalette", "section") %>% walk(~ paste0("../function ", ., ".R") %>% source())

#Resolve select() conflict
select = dplyr::select

#Definindo as cores:
pal = list(azul.f = "#081133",
           rosa = "#f751a5",
           roxo = "#914ecd",
           azul = "#264fe8",
           verde = "#05a31a", 
           branco.f = "#f1f1f1",
           preto = "#0d0d0d")

pal$grid = c(pal$azul.f, pal$branco.f) %>%
  hex2RGB(gamma = FALSE) %>%
  {colorspace::mixcolor(0.1, .[1,], .[2,])} %>%
  hex()

#Definindo as fontes:
font = "Montserrat"

#Historical values
ggpHist = function(df){
  df %>%
    ggplot(aes(Data, Fluxo)) +
    geom_line(aes(color = "Fluxo")) +
    labs(y="Valor") +
    scale_x_date(date_labels = "%Y-%m") +
    scale_color_manual(values = c(pal$`Fundo Azul`))}

#ACF and PACF
ggpAcf = function(variable, lag.max = length(variable)-1){
  confInterval = qnorm((1 - 0.95)/2)/sqrt(length(variable)) #ci = 0.95
  tibble(Lag = c(0:(lag.max-1), 1:lag.max),
         Value = c(
           acf(variable, plot = FALSE, lag.max = lag.max-1)$acf[,,1],
           acf(variable, type = "partial", plot = FALSE, lag.max = lag.max)$acf[,,1]),
         Type = c(rep("ACF", lag.max), rep("PACF", lag.max))) %>%
    ggplot(aes(Lag, Value)) +
    geom_segment(mapping = aes(xend = Lag, yend = 0), color = pal$Preto, size = 1) +
    geom_ribbon(aes(ymin = - confInterval, ymax = confInterval),
                linetype = 2, color = pal$`Fundo Azul`, fill = NA) +
    facet_wrap(vars(Type), nrow = 2)
}

#Normality plot
ggpNorm = function(variable){
  data.frame(x = variable) %>%
    ggplot(aes(x)) +
    geom_histogram(aes(y = ..density..), bins = 50) +
    geom_density(aes(color = "Série"), size = 1) +
    stat_function(aes(color = "Curva normal"), fun = dnorm,
                  args = list(sd = sd(variable)), size = 1) +
    xlim(-725, 725) +
    scale_color_manual(values = c(pal$`Fundo Azul`, pal$`Logo Rosa`), name = "")}

#Custom dates
date2period = function(x, period = 6, sep = " S") {
  ym = as.yearmon(x)
  paste0(sep, (cycle(ym) - 1) %/% period + 1)
}

#Root RMSE
rrmse = function(variable, actual = DF.test$Fluxo){
  (sum((actual - variable)^2))^(0.5)}


#====================  DADOS   ====================
DF.raw = read.csv("data/train.csv") %>%
  rename(Data = DT_COMPTC) %>%
  mutate(Data = as.Date(Data))

n.test = 300
DF.train = DF.raw %>%
  slice_head(n = nrow(DF.raw) - n.test)
DF.test = DF.raw %>% slice_tail(n = n.test)
DF.train = DF.raw


#=============  ANÁLISE EXPLORATÓRIA  =============
#base.env = c(ls(), "strucbreaks", "base.env")
#rm(list = setdiff(ls(), base.env))

ggplot(DF.raw, aes(Data, Fluxo/1000)) +
  geom_line(color = pal$branco.f) +
  labs(y = "Fluxo (1.000 R$)", title = "Valores Históricos do Fluxo de Mercado") +
  scale_x_date(date_labels = "%Y")

# Removing outliers
outliers = forecast::tsoutliers(DF.train$Fluxo)
DF.train$Fluxo[outliers$index] = outliers$replacements

ggplot(DF.train, aes(Data, Fluxo/1000)) +
  geom_line(color = pal$branco.f) +
  labs(y = "Fluxo (1.000 R$)", title = "Valores Históricos Sem Outliers") +
  scale_x_date(date_labels = "%Y")

# Testing for normality
ggpNorm(DF.train$Fluxo)

# Testing for stationarity
tseries::adf.test(DF.train$Fluxo)

# ACF and PACF
ggpAcf(DF.train$Fluxo)
ggpAcf(DF.train$Fluxo)

# MA and sazonality decomposition
decomp = decompose(ts(DF.train$Fluxo, freq = 7), type = "multiplicative")
plot(decomp)

tibble(Data = DF.train$Data, Season = decomp$seasonal) %>%
  slice_head(prop = 0.10) %>%
  ggplot(aes(Data, Season)) +
    geom_line()

#------------------  Detrending  ------------------
#strucbreaks = strucchange::breakpoints(Fluxo ~ Data, h = 0.15, breaks = 3,
#                                       data = DF.raw)

# Linear tendency with struc. breaks
mod.SB = DF.raw %>%
  mutate(breaks = cut(row_number(), c(0, strucbreaks$breakpoints, nrow(DF.raw)))) %>%
  lm(Fluxo ~ Data*breaks, data = .)
summary(mod.SB)

# Polinomial tendency
#mod.SB = lm(Fluxo ~ I(poly(Data, 3)), data = DF.train)

ggplot(DF.train, aes(Data)) +
  geom_line(aes(y = Fluxo, color = "Fluxo")) +
  geom_line(mapping = aes(y = fitted(mod.SB), color = "Tendência"), size = 1) +
  geom_vline(xintercept = DF.train$Data[strucbreaks$breakpoints],
             linetype = 2, color = pal$rosa, size = 1) +
  scale_color_manual(values = c(pal$branco.f, pal$azul), name = "") +
  scale_x_date(date_labels = "%Y") +
  labs(title = "Valores Históricos do Fluxo (R$)", subtitle = "Quebras Estruturais") +
  theme(axis.title = element_blank(), legend.title = element_blank(),
        legend.box.spacing = unit(-1, "pt"))
ggsave("figures/hist struc.png", device = "png", width = 12.5, heigth = 9, units = "cm")

# Detrending
DF.train.dt =  DF.train %>% mutate(Fluxo = Fluxo - fitted(mod.SB))
ggpHist(DF.train.dt) + scale_color_manual(values = c(pal[1]), name = "Série")
lm(Fluxo ~ Data, DF.train.dt) %>% summary() # Checking remaining trend

rm(outliers, decomp, mod.SB)


#===================  MODELOS   ===================
#-----------------  White Noise   -----------------
mod.WN = arima.sim(list(), n = n.test, sd = sd(DF.train$Fluxo))

ggpHist(DF.test) +
  geom_line(mapping = aes(y = mod.WN, color = "White Noise"), alpha = 0.5) +
  scale_color_manual(values = c(pal[1], pal[2]), name = "Série")

mod.WN = list(pred = mod.WN, rmse = sum((DF.test$Fluxo - mod.WN)^2))


#------------  Deterministic Models   -------------
# Year and week-level sazonality dummies
formulas.DM = list(c('1',
                     'format(Data, "%a")',
                     'I(format(Data, "%a")=="sex") + I(format(Data, "%a")=="seg")'),
                   c('1',
                     'date2period(Data, 6, "S")',
                     'date2period(Data, 3, "Q")',
                     'format(Data, "%b")'),
                   c('1',
                     'as.numeric(Data)')) %>%
  expand.grid() %>%
  pmap(~ paste(., sep = " + "))

mod.DM.all = map(formulas.DM, ~ lm(paste("Fluxo ~", .), DF.train))

rrmse.DM.all = mod.DM.all %>%
  map_dbl(~ rrmse(predict(., DF.test))) %>% #AIC(.)
  setNames(formulas.DM)

mod.DM = lm(paste("Fluxo ~", names(rrmse.DM.all %>% which.min())), DF.train)

ggpHist(DF.test) +
  geom_line(mapping = aes(y = predict(mod.DM, DF.test), color = "Seasonal dummies")) +
  scale_color_manual(values = c(pal[1], pal[2]), name = "Série")

mod.DM$rmse = rrmse.DM.all %>% .[which.min(.)]

rm(formulas.DM, mod.DM.all, rrmse.DM.all)


#---------------------  SARMA  --------------------
mod.SARIMA.all = list(
  mod.AR =     auto.arima(DF.train$Fluxo %>% ts(), d = 0, max.q = 0),
  mod.MA =     auto.arima(DF.train$Fluxo %>% ts(), d = 0, max.p = 0),
  mod.ARMA =   auto.arima(DF.train$Fluxo %>% ts(), d = 0),
  mod.SARMA =  auto.arima(DF.train$Fluxo %>% ts(freq = 30), d = 0), #freq = 7 or 30
  mod.ARIMA =  auto.arima(DF.train$Fluxo %>% ts()),
  mod.SARIMA = auto.arima(DF.train$Fluxo %>% ts(freq = 30))) #freq = 7 or 30

# Fit
fit.SARIMA = mod.SARIMA.all %>%
  map_dfc(~ fitted(.)) %>%
  mutate(Data = DF.train$Data, .before = 1) %>%
  pivot_longer(-Data)

ggpHist(DF.train.dt) +
  geom_line(data = fit.SARIMA, aes(y = value), color = pal[2], alpha = 0.5) +
  facet_wrap(vars(name), nrow = 3) +
  scale_color_manual(values = c(pal[1]), name = "Série")

# Prediction
pred.SARIMA = mod.SARIMA.all %>%
  map_dfc(~ predict(., n.ahead = n.test)$pred) %>%
  mutate(Data = DF.test$Data, .before = 1) %>%
  pivot_longer(-Data)

a = tibble(value = predict(mod.SARIMA, n.ahead = n.test, newxreg = DF.test$IBOV)$pred,
           Data = DF.test$Data)
ggpHist(DF.test) +
  geom_line(data = a, aes(y = value), color = pal[2], alpha = 0.5) +
  scale_color_manual(values = c(pal[1]), name = "Série")

rrmse.SARIMA.all = mod.SARIMA.all %>% map_dbl(~ rrmse(predict(., n.ahead = n.test)$pred))
mod.SARIMA = mod.SARIMA.all[[which.min(rrmse.SARIMA.all)]]

rm(mod.SARIMA.all, fit.SARIMA, pred.SARIMA, rrmse.SARIMA.all)

########
mod.SARIMAX = arima(DF.train$Fluxo %>% ts(freq = 30), c(5,0,0),
                    list(order = c(1,0,1), period = 30),
                    xreg = DF.train$IBOV)

#--------------------  GARCH   --------------------
mod.GARCH = ugarchspec(mean.model = list(armaOrder = c(5,0)),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       distribution.model = "jsu") %>%
  ugarchfit(DF.train$Fluxo)

pred.GARCH = ugarchforecast(mod.GARCH, n.ahead = n.test)@forecast$seriesFor[,1]

ggpHist(DF.test) +
  geom_line(data = data.frame(Data = DF.test$Data, value = pred.GARCH),
            aes(y = value), color = pal[2], alpha = 0.5) +
  scale_color_manual(values = c(pal[1]), name = "Série")

ggpAcf(residuals(mod.GARCH), 50)
ggpNorm(residuals(mod.GARCH))
mod.GARCH@fit$rmse = rrmse(pred.GARCH)

# Com dummies sazonais (pior)
#sazonal.dummies = as.matrix(format(DF.raw$Data, "%a")=="sex" | format(DF.raw$Data, "%a")=="seg")
#mod.GARCH = ugarchspec(mean.model = list(armaOrder = c(2,1)),
#                       variance.model = list(model = "sGARCH", garchOrder = c(1,1),
#                                             external.regressors = sazonal.dummies),
#                       distribution.model = "norm") %>%
#  ugarchfit(DF.raw$Fluxo)
#sum((DF.test$Fluxo - ugarchforecast(mod.GARCH, n.ahead = n.test)@forecast$seriesFor)^2)

rm(pred.GARCH)

#---------------------  ARDL  ---------------------
predict2 = function(mod, ...){
  DF.test.ARDL = DF.test #"1" because only L(Fluxo, 1) used
  for(i in 2:n.test){
    DF.test.ARDL$Fluxo[i] = predict(mod, DF.test.ARDL[i-1,])}
  pred.ARDL = DF.test.ARDL$Fluxo
}

terms.ARDL = c("inter", "squares", "lag1", "interlag", "squareslag")
formulas.ARDL = list(
  combn(colnames(DF.train)[-c(1,6)], 2) %>%
    apply(2, paste, collapse="*"),
  paste0("I(", colnames(DF.train)[-c(1,6)], "^2)"),
  paste0("L(", colnames(DF.train)[-c(1)], ", 1)"),
  paste0("L(", colnames(DF.train)[-c(1)], ")") %>%
    combn(2) %>% apply(2, paste, collapse="*"),
  paste0("L(", colnames(DF.train)[-c(1)], ")") %>%
    paste0("I(", ., "^2)")) %>%
  map(~ c(., colnames(DF.train)[-c(1,6)])) %>%
  map(~ paste("Fluxo ~", paste(., collapse = " + "))) %>%
  setNames(terms.ARDL)

mod.ARDL.all = map(formulas.ARDL, ~ dynlm(as.formula(.), DF.train.dt %>% ts()))
map(mod.ARDL.all, ~ summary(.))

DF.raw.pred = full_join(DF.train, DF.test %>% select(-Fluxo)) %>% ts()
pred.ARDL = map(mod.ARDL.all, ~ predict2(., DF.raw.pred) %>% .[(length(.)-n.test+1):length(.)])
pred.ARDL %>% map_dbl(~ rrmse(.))

pred.ARDL = pred.ARDL %>% 
  reduce(cbind) %>%
  as_tibble() %>%
  setNames(terms.ARDL) %>%
  mutate(Data = DF.test$Data) %>%
  pivot_longer(-Data)

ggpHist(DF.test) +
  geom_line(data = pred.ARDL, aes(y = value), color = pal[2], alpha = 0.5) +
  facet_wrap(vars(name), nrow = 3) +
  scale_color_manual(values = c(pal[1]), name = "Série") + ylim(-1000, 1800)
  

# All terms
formula.ARDL = list(
  combn(colnames(DF.train)[-c(1,6)], 2) %>%
    apply(2, paste, collapse="*"),
  paste0("I(", colnames(DF.train)[-c(1,6)], "^2)"),
  paste0("L(", colnames(DF.train)[-c(1)], ", 1)"),
  paste0("L(", colnames(DF.train)[-c(1)], ", 2)"),
  paste0("L(", colnames(DF.train)[-c(1)], ", 3)"),
  paste0("L(", colnames(DF.train)[-c(1)], ")") %>%
    combn(2) %>% apply(2, paste, collapse="*"),
  paste0("L(", colnames(DF.train)[-c(1)], ")") %>%
    paste0("I(", ., "^2)")) %>%
  map(~ c(., colnames(DF.train)[-c(1,6)])) %>%
  map(~ paste(., collapse = " + ")) %>%
  paste0(collapse = " + ") %>%
  paste("Fluxo ~", .) %>%
  as.formula()

mod.ARDL = dynlm(formula.ARDL, DF.train.dt %>% ts())
pred.ARDL = full_join(DF.train.dt, DF.test) %>%
  ts() %>%
  predict(mod.ARDL, .) %>%
  .[(length(.)-n.test+1):length(.)]

rrmse(pred.ARDL)

ggpHist(DF.test) +
  geom_line(data = data.frame(Data = DF.test$Data, value = pred.ARDL),
            aes(y = value), color = pal[2], alpha = 0.5) +
  scale_color_manual(values = c(pal[1]), name = "Série")

#########

library(quantmod)

df.symb = c("^N100", "^FTSE", "^N225", "^HSI") %>%
  map(~ getSymbols(., from = "2005-01-01", env=NULL) %>%
        `[`(, grepl("Index|Close", colnames(.)))) %>%
  reduce(cbind.xts) %>% fortify.zoo()

df.curr = paste0(c("EUR", "JPY", "AUD", "GBP", "CNY"), "=X") %>%
  map(~ getSymbols(., from = "2005-01-01", src = "yahoo", env=NULL) %>%
        `[`(, grepl("Index|Close", colnames(.)))) %>%
  reduce(cbind.xts) %>% fortify.zoo() %>% rename_with(~ str_remove(., "\\.X"))

write.csv(merge(df.symb, df.curr), "data/bolsas_moedas_internacionais.csv", row.names = FALSE)

#====================  DADOS   ====================
df = read_delim("data/features.csv") %>% as_tibble()

df = df %>%
  mutate(DT_COMPTC...5 = NULL) %>%
  rename(Data = DT_COMPTC...1)

adf.res = df %>%
  select(-c(Data, Fluxo)) %>%
  map_dbl(~ tseries::adf.test(.)$p.value) %>%
  (function(x){x > 0.05 | names(x) == "PIB mensal"}) %>%
  `>`(., 0.05) %>% which() %>% names()

df.diff = df %>%
  summarise(across(all_of(adf.res), diff),
            across(setdiff(colnames(df), adf.res), ~ .[-1]))

df.merge = merge(df, df.diff, by = "Data", suffixes = c("=level", "=diff"))

df.merge %>%
  pivot_longer(-Data, names_sep = "=", names_to = c("var", "trans")) %>%
  ggplot(aes(Data, value, color = trans)) +
    geom_line(alpha = 0.7) +
    facet_wrap(vars(var), scales = "free_y")

ccfs = function(df){
  confInterval = qnorm((1 - 0.95)/2)/sqrt(nrow(df))
  df %>%
    summarise(across(-c(Data, Fluxo), ~ ccf(Fluxo, ., plot = FALSE, lag.max = 60)$acf),
              Lag = -60:60) %>%
    pivot_longer(-Lag) %>%
    ggplot(aes(x = Lag, y = value)) +
    geom_segment(aes(xend = Lag, yend = 0)) +
    facet_wrap(vars(name), scales = "free_y") +
    geom_ribbon(aes(ymin = - confInterval, ymax = confInterval),
                linetype = 2, color = "blue", fill = NA)
}

confInterval = qnorm((1 - 0.95)/2)/sqrt(nrow(df))
df.merge %>%
  summarise(across(-c(Data, Fluxo), ~ ccf(Fluxo, ., plot = FALSE, lag.max = 60)$acf),
            Lag = -60:60) %>%
  pivot_longer(-Lag) %>%
  ggplot(aes(x = Lag, y = value)) +
  geom_segment(aes(xend = Lag, yend = 0)) +
  facet_wrap(vars(name), scales = "free_y") +
  geom_ribbon(aes(ymin = - confInterval, ymax = confInterval),
              linetype = 2, color = "blue", fill = NA)

ccfs(df)
ccfs(df.diff)

ccf(df$Fluxo, df$IBOV, lag.max = 60)

which(adf.re)
