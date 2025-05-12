#==============  PACOTES E FUNÇÕES  ==============

library(tidyverse)
library(magrittr)
library(colorspace)
library(showtext)
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
  {colorspace::mixcolor(0.9, .[1,], .[2,])} %>%
  hex()

#Definindo as fontes:
font = "Montserrat"
font_add_google(font, font)
showtext_auto()


#====================  DADOS   ====================
df = read.csv("data/train.csv") %>%
  rename(Data = DT_COMPTC) %>%
  mutate(Data = as.Date(Data))

df.out = df


#=============  ANÁLISE EXPLORATÓRIA  =============
#base.env = c(ls(), "base.env")
#save.image(file = "base.env") load("base.env")
#rm(list = setdiff(ls(), base.env))

ggplot(df, aes(Data, Fluxo/1000)) +
  geom_line(color = pal$branco.f) +
  labs(y = "", x = "", title = "Valores Históricos do Fluxo (mil R$)") +
  scale_x_date(date_labels = "%Y")
#ggsave("figures/hist.png", device = "png", width = 12.5, height = 9, units = "cm")

19 %>% {which(abs(df$Fluxo) > .*sd(df$Fluxo))} %>% df[., c("Data", "Fluxo")] #%>% nrow()

# Removing outliers
outliers = forecast::tsoutliers(df$Fluxo)
df.out$Fluxo[outliers$index] = outliers$replacements

ggplot(df.out, aes(Data, Fluxo)) +
  geom_line(color = pal$branco.f) +
  labs(y = "", x = "", title = "Valores Históricos do Fluxo (R$)", subtitle = "Sem outliers") +
  scale_x_date(date_labels = "%Y")
#ggsave("figures/hist sem out.png", device = "png", width = 12.5, height = 9, units = "cm")

# Testing for normality
ggplot(df.out, aes(Fluxo)) +
  geom_histogram(aes(y = ..density..), bins = 150, fill = pal$branco.f) +
  #geom_density(aes(color = "Fluxo"), size = 1) +
  stat_function(aes(color = "Distribuição normal"), fun = dnorm, size = 1,
                args = list(sd = sd(df.out$Fluxo), mean = mean(df.out$Fluxo))) +
  scale_color_manual(values = c(pal$rosa), name = "") +
  labs(y = "", x = "", title = "Distribuição do Fluxo (% das observações)")
#ggsave("figures/dist.png", device = "png", width = 12.5, height = 9, units = "cm")

# ACF and PACF
my.acf = function(type, lag.max, title){
  confInterval = qnorm((1 - 0.95)/2)/sqrt(nrow(df.out)) #ci = 0.95
  
  g = tibble(acf = acf(df.out$Fluxo, lag.max = lag.max - (type == "correlation"),
                   type = type, plot = FALSE)$acf[,,1],
         lag = 1:lag.max) %>%
    ggplot(aes(lag, acf)) +
    geom_segment(mapping = aes(xend = lag, yend = 0), color = pal$branco.f) +
    geom_ribbon(aes(ymin = - confInterval, ymax = confInterval),
                linetype = 2, color = pal$rosa, fill = NA) +
    labs(y = "", x = "", title = title)
  plot(g)
  ggsave(paste0("figures/", type, ".png"), device = "png", width = 12.5, height = 6.5, units = "cm")}

my.acf("correlation", 770, "Autocorrelação")
my.acf("partial", 40, "Autocorrelação Parcial")

#------------------  Detrending  ------------------
#strucbreaks = strucchange::breakpoints(Fluxo ~ Data, h = 0.15, breaks = 3,
#                                       data = df.out)

# Linear tendency with struc. breaks
mod.SB = df.out %>%
  mutate(breaks = cut(row_number(), c(0, strucbreaks$breakpoints, nrow(df.out)))) %>%
  lm(Fluxo ~ Data*breaks, data = .)

ggplot(df.out, aes(Data)) +
  geom_line(aes(y = Fluxo), color = pal$branco.f) +
  geom_line(mapping = aes(y = fitted(mod.SB), color = "Tendência"), size = 1) +
  geom_vline(xintercept = df.out$Data[strucbreaks$breakpoints],
             linetype = 2, color = pal$rosa, size = 1) +
  scale_color_manual(values = c(pal$azul), name = "") +
  scale_x_date(date_labels = "%Y") +
  labs(title = "Valores Históricos do Fluxo (R$)", subtitle = "Quebras Estruturais") 
ggsave("figures/hist struc.png", device = "png", width = 12.5, height = 9, units = "cm")


#================
decomp = decompose(ts(df.out$Fluxo, freq = 30), type = "multiplicative")
plot(decomp)

tibble(Data = df.out$Data, Season = decomp$seasonal) %>%
  slice_head(n = 7) %>%
  ggplot(aes(Data, Season)) +
  geom_line(color = pal$branco.f) +
  scale_x_date(date_labels = "%d")

mod.SARIMA30 = auto.arima(df.out$Fluxo %>% ts(freq = 30))
mod.SARIMA7 = auto.arima(df.out$Fluxo %>% ts(freq = 7))
mod.ARMA = auto.arima(df.out$Fluxo %>% ts(), d = 0)

df.sea = df.out
df.sea %<>% mutate(trend = fitted(mod.ARMA))

weekday = function(Data){factor(format(Data, "%a"), levels = c("seg", "ter", "qua", "qui", "sex"))}
mod.SD = lm(Fluxo - trend ~ weekday(Data) - 1, df.sea)
summary(mod.SD)

df.sea %<>% mutate(sazo = fitted(mod.SD), resid = Fluxo - trend - sazo)

df.sea %>%
  pivot_longer(c(Fluxo, trend, sazo, resid)) %>%
  ggplot(aes(Data, value)) +
  geom_line(color = pal$branco.f) +
  facet_wrap(vars(name), nrow = 4, scales = "free_y")
  



#====================  CORRELAÇÕES   ====================
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


#====================  CORRELAÇÕES   ====================
df.feat = read_delim("data/features_ricardo.csv") %>% as_tibble()

df.feat %<>%
  rename(Data = DT_COMPTC, PIB = `PIB mensal`, IPCA = `IPCA ocorrido`,
         Desocup = `Taxa de Desocupação`) %>%
  mutate(DataQ = as.yearqtr(Data)) %>%
  group_by(DataQ) %>%
  mutate(PIBQ = mean(PIB), SPXQ = mean(PIB)) %>%
  ungroup()

a = tibble(SPX = unique(df.feat$PIBQ),
           DataQ = unique(df.feat$DataQ)) %>%
  summarise(SPX = (SPX-lag(SPX))/lag(SPX), DataQ = DataQ)

df.feat %>%
  ggplot(aes(Data, SPX)) +
    labs(title  = "SPX") +
    geom_col(color = pal$branco.f) + 
    scale_x_date(date_breaks = "1 year") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

#PIB
ggplot(a, aes(as.Date(DataQ), SPX)) +
  labs(title = "PIB") +
  geom_col(fill = pal$branco.f) +
  scale_x_date(date_breaks = "8 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
# 2008 - 2009
# 2012 - 2019

#Selic
df.feat$DataQ[df.feat$SELIC >= 0.05] %>% unique() #2005 - 2006, 2008.3 - 2009.1, 2015.3 - 2017.1
df.feat$DataQ[df.feat$SELIC <= 0.025] %>% unique() #2018 - 2021.2
df.feat$DataQ[df.feat$SELIC <= 0.015] %>% unique() #2020 - 2021.2

#IPCA
df.feat$DataQ[df.feat$IPCA >= 9] %>% unique() #2015.3 - 2016.2
df.feat$DataQ[df.feat$IPCA <= 4.5] %>% unique() #2017.2 - 2020

#Desocupação
df.feat$DataQ[df.feat$Desocup >= 12.5] %>% unique() #2017 - 2021.2
df.feat$DataQ[df.feat$Desocup <= 7.5] %>% unique() #2012.3 - 2015.1

#Dol
df.feat$DataQ[df.feat$Dol >= 4.5] %>% unique() #2020 - 2021.2
df.feat$DataQ[df.feat$Dol <= 2.2] %>% unique() #2005.4 - 2014.2

#SPX
df.feat$DataQ[df.feat$SPX >= 2500] %>% unique() #2017.3 - 2021.2
df.feat$DataQ[df.feat$SPX <= 1500] %>% unique() #2005 - 2013


###################
df.bruno = read_delim("data/extratree.csv", delim = ",") %>% as_tibble()

df.bruno %<>% rename(ETR = Extra_Tree_Regressor)

a = rbind(
  c("IMOB", 0.390, 134.55),
  c("IFNC", 0.341, 98.688),
  c("IEE",  0.256, 52.54),
  c("SPX",  0.213, 35.56),
  c("NDX",  0.197, 30.41),
  c("Dol", -0.140, 14.97)) %>%
  as_tibble() %>%
  mutate(across(2:3, as.numeric)) %>%
  setNames(c("Indice", "Pearson", "K-best")) %>%
  merge(df.bruno)

sq<-function(x){x^3}
isq<-function(x){x^(1/3)}

a %>%
  pivot_longer(-Indice) %>%
  mutate(value = abs(value)) %>%
  ggplot(aes(Indice, value, fill =  name)) +
  geom_bar(stat='identity', position='dodge') +
  labs(title = "Métodos de seleção", fill = "Método: ") +
  scale_fill_manual(values = c("#f751a5", "#914ecd", "#264fe8")) +
  scale_y_sqrt()

ggsave("figures/ETR.png", device = "png", width = 12.5, height = 9, units = "cm")



a = tibble(x = 1:10, y = c(3, 4, 5, 2, 3, 4, 5, 6, 7, 7) + rnorm(10))

ggplot(a, aes(x, y)) +
  geom_line(color = pal$branco.f) +
  geom_vline(xintercept = 4, linetype = 2, color = pal$azul) +
  geom_vline(xintercept = 6, linetype = 2, color = pal$rosa) +
  geom_vline(xintercept = 8, linetype = 2, color = pal$rosa) +
  geom_vline(xintercept = 10, linetype = 2, color = pal$rosa) +
  labs(x = "Tempo", y = "Mercado") +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(color = pal$branco.f, hjust = 0, vjust = 0.8, size = size),
        axis.title.y = element_text(color = pal$branco.f, size = size),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank())
ggsave("figures/operacional.png", device = "png", width = 10, height = 3, units = "cm")