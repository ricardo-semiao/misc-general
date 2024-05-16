library(shiny)
library(shinyjs)
library(glue)
library(base64enc)
#library(tibble); library(rlang); library(magrittr); library(purrr)

questions <- readRDS("data/questions.RData")
answers <- readRDS("data/answers.RData")
#readRDS("data/answers_archive.RData")

source("util_functions.R")
source("util_factories.R")

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
