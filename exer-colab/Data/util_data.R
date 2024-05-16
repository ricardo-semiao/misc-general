# Packages and functions --------------------------------------------------

library(rvest)
library(magrittr)
library(glue)

questions_get <- function(name_class) {
  name <- glue("aula_{name_class}_exs.html")
  
  html <- read_html(glue("Data/{name}")) %>%
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

questions_format <- function(name_class) {
  questions <- questions_get(name_class)
  c(size = length(questions$questions), questions)
}

answers_format <- function(size) {
  list(text = rep("NA", size),
       code = rep("NA", size),
       img = character(size))
}

update_data <- function(name_data, name_set, x, increment) {
  data <- readRDS(glue("Data/{name_data}.RData"))
  
  if (name_data == "answers") {
    data$modified <- format(Sys.time(), "%Y-%m-%d_%H-%M-%OS")
  } else if (name_data == "questions") {
    data$nsets <- data$nsets + increment
  } else {
    stop("Invalid `name_data`")
  }
  
  data[[name_set]] <- x
  
  check <- readline(glue("Are you sure you want to update {name_data}? (TRUE/FALSE): "))
  if (as.logical(check)) saveRDS(data, file = glue("Data/{name_data}.RData"))
}



# Updating data -----------------------------------------------------------

# Creating initial data:
#saveRDS(list(nsets = 0), file = "Data/questions.RData")
#saveRDS(list(modified = format(Sys.time(), "%Y-%m-%d_%H-%M-%OS")), file = "Data/answers.RData")
#saveRDS(list(), file = "Data/answers_archive.RData")


#Updating data:
name_class <- "i1"; name_set <- "set1"

data_questions_new <- questions_format(name_class)
data_answers_new <- answers_format(data_questions_new$size)

update_data("questions", "set1", data_questions_new, TRUE)
update_data("answers", "set1", data_answers_new, TRUE)

rm(questions_get, questions_format, answers_format, update_data,
   name_class, name_set, data_questions_new, data_answers_new)
