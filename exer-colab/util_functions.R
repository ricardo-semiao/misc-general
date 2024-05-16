# General utility functions -----------------------------------------------

util_format_base64 <- function(input, output, cur_set, cur_quest) {
  base64 <- reactive({
    inFile <- input[[glue("img_{cur_set}_{cur_quest}")]]
    if (!is.null(inFile)) dataURI(file = inFile$datapath, mime = "image/png")
  })
  
  output[[glue("img_{cur_set}_{cur_quest}")]] <- renderUI({
    if (!is.null( base64() )) format_img( base64() )
  })
  
  return( base64() )
}

util_capture_code <- function(x) {
  result <- eval(parse(text = x))
  paste(capture.output(print(result)), collapse = "\n")
}

util_find_changes <- function(x) {
  changes <- cumsum(rle(x)$lengths)
  return(c(1, changes[1:(length(changes) - 1)]))
}



# Formatting output functions (to HTML or others) -------------------------

format_img <- function(base64_src){
  tags$div(
    tags$img(src = base64_src, width = "100%"),
    style = "width: 400px;"
  )
}

format_code <- function(user_input) {
  if (!is.null(user_input) && user_input != "") {
    tryCatch({
      code_output <- lapply(strsplit(user_input, "\n")[[1]],
                            \(x) paste0("> ", x, "\n", util_capture_code(x), "\n"))
      paste(code_output, collapse = "\n")
    }, error = function(e) {
      paste0("> ", user_input, "\n", "Error:", e$message, sep = "\n")
    })
  } else {"User input vazio"}
}



# onStop functions --------------------------------------------------------

onStop_data_update <- function() {
  # Archive
  archive <- readRDS(file = "data/answers_archive.RData")
  archive[[format(Sys.time(), "%Y-%m-%d_%H-%M-%OS")]] <- answers
  saveRDS(archive, file = "data/answers_archive.RData")
  
  # Data update
  answers_old <- readRDS(file = "data/answers.RData")
  
  for (i in glue("set{1:questions$nsets}")) {
    cond <- `&`(
      `|`(answers_old[[i]]$text == "NA", answers_old[[i]]$code == "NA"),
      Reduce(`|`,
             list(answers_old[[i]]$text != answers[[i]]$text,
                  answers_old[[i]]$code != answers[[i]]$code,
                  answers_old[[i]]$img != answers[[i]]$img)
      )
    )
    
    answers_old[[i]]$text[cond] <- answers[[i]]$text[cond]
    answers_old[[i]]$code[cond] <- answers[[i]]$code[cond]
    answers_old[[i]]$img[cond] <- answers[[i]]$img[cond]
  }
  
  answers_old$modified <- format(Sys.time(), "%Y-%m-%d_%H-%M-%OS")
  saveRDS(answers_old, file = "data/answers.RData")
}




# Old functions -----------------------------------------------------------

#cond <- Reduce(
#  `&`,
#  list(`|`(answers_old[[i]]$text == "NA", answers_old[[i]]$code == "NA"),
#       #`|`(answers[[i]]$text != "NA", answers[[i]]$code != "NA"),
#       `|`(answers_old[[i]]$text != answers[[i]]$text,
#           answers_old[[i]]$code != answers[[i]]$code))
#)
