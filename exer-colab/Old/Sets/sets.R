library(rvest)
library(magrittr)
library(glue)

get_questions <- function(class_name) {
  name <- glue("Aula_{class_name}_exs.html")
  
  html <- read_html(glue("Sets/{name}")) %>%
    html_element("body") %>%
    html_element("div.container-fluid.main-container")
  
  sections <- html %>%
    html_element("section#sections") %>%
    html_text2() %>%
    stringr::str_replace_all(c("\\r" = "", "“" = "'", "”" = "'")) %>%
    parse(text = .) %>%
    eval()
  
  description <- html %>%
    html_element("section#description") %>%
    as.character()# %>% stringr::str_replace_all(c("\\r" = ""))
  
  recap <- html %>%
    html_element("section#recap") %>%
    as.character()# %>% stringr::str_replace_all(c("\\r" = ""))
  
  questions <- html %>%
    html_elements("div.section.level1") %>%
    sapply(\(x) paste(html_children(x)[-1], collapse = ""))
  
  list(description = description, questions = questions, sections = sections, recap = recap)
}

build_set <- function(class_name) {
  set_element <- get_questions(class_name)
  set_element$sizes <- length(set_element$questions)
  set_element$answers_text <- rep("NA", set_element$sizes)
  set_element$answers_code <- rep("NA", set_element$sizes)
  set_element$answers_img <- character(set_element$sizes)
  return(set_element)
}

sets <- list(set1 = build_set("i1"),
             nsets = 1,
             modified = format(Sys.time(), "%Y-%m-%d_%H-%M-%OS"))

override <- TRUE
if (override | !file.exists("Sets/sets.RData")) saveRDS(sets, file = "Sets/sets.RData")
if (FALSE) saveRDS(list(), file = "Sets/sets_archive.RData")
rm(sets, override)

#####
#readRDS(file = "Sets/sets_archive.RData")
#readRDS(file = "Sets/sets.RData")
