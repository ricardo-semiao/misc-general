# Functions for building server and ui ------------------------------------

buildquest_question <- function(cur_set, cur_quest, cur_size) {
  fluidRow(
    column(width = 12,
           
           if (cur_quest %in% util_find_changes(questions[[cur_set]]$sections)) {#write header
             h4(questions[[cur_set]]$sections[cur_quest]) 
           },
           
           actionLink(inputId = glue("toggle{cur_quest}"),
                      label = glue("Alternar Q{cur_quest}")
           ),
           
           conditionalPanel(
             condition = glue("input.toggle{cur_quest} % 2 == 1"),
             
             column(width = 12,
                    h4(glue("Questão {cur_quest}")),
                    HTML(questions[[cur_set]]$questions[cur_quest])
             ),
             
             column(width = 12, div(class = "half-rule")),
             
             # Editing
             column(width = 4, textAreaInput(inputId = glue("text_{cur_set}_{cur_quest}"),
                                             label = "Texto da resposta",
                                             value = answers[[cur_set]]$text[cur_quest])),
             column(width = 4, textAreaInput(inputId = glue("code_{cur_set}_{cur_quest}"),
                                             label = "Código da resposta",
                                             value = answers[[cur_set]]$code[cur_quest])),
             column(width = 4, fileInput(inputId = glue("img_{cur_set}_{cur_quest}"),
                                         label = "Imagem da resposta",
                                         accept = "image/png")),
             column(width = 12, actionButton(inputId = glue("button_{cur_set}_{cur_quest}"),
                                             label = "Postar resposta")), #, style = "border: 1px solid #800000;"
             
             column(width = 12, div(class = "half-rule")),
             
             # Current answer
             column(width = 12, tags$b("Resposta atual:")),
             column(width = 4, verbatimTextOutput(outputId = glue("text_{cur_set}_{cur_quest}"))),
             column(width = 4, verbatimTextOutput(outputId = glue("code_{cur_set}_{cur_quest}"))),
             column(width = 4, uiOutput(outputId = glue("img_{cur_set}_{cur_quest}"))),
             if (cur_quest != cur_size) column(width = 12, div(class = "red-rule-small"))
           )
    )
  )
}

buildset_server <- function(output, cur_set) {
  cur_size <- questions[[cur_set]]$size
  
  # Build description and recap
  output[[glue("description_{cur_set}")]] <- renderText({ HTML(questions[[cur_set]]$description) })
  output[[glue("recap_{cur_set}")]] <- renderText({ HTML(questions[[cur_set]]$recap) })
  
  # Build initial answers
  for (i in 1:cur_size) {
    output[[glue("text_{cur_set}_{i}")]] <- renderText({ answers[[cur_set]]$text[i] })
    output[[glue("code_{cur_set}_{i}")]] <- renderText({ format_code(answers[[cur_set]]$code[i]) })
    output[[glue("img_{cur_set}_{i}")]] <- renderUI({ format_img(answers[[cur_set]]$img[i]) })
  }
  
  # Build questions
  output[[glue("questions_{cur_set}")]] <- renderUI({
    lapply(1:cur_size, buildquest_question, cur_set = cur_set, cur_size = cur_size)
  })
  
  # Update answers
  for (i in 1:cur_size) {
    observeEvent(input[[glue("button_{cur_set}_{i}")]], {
      answers[[cur_set]]$text[i] <- input[[glue("text_{cur_set}_{i}")]]
      output[[glue("text_{cur_set}_{i}")]] <- answers[[cur_set]]$text[i]
      answers[[cur_set]]$code[i] <- input[[glue("text_{cur_set}_{i}")]]
      output[[glue("text_{cur_set}_{i}")]] <- format_code(answers[[cur_set]]$code[i])
    })
    
    observeEvent(input[[glue("img_{cur_set}_{i}")]], {
      output[[glue("img_{cur_set}_{i}")]] <- util_format_base64(input, output, cur_set, i)
    })
  }
  
  # Toggle all button
  observeEvent(input$toggleAllButton, {
    k <- if (input$toggleAllButton %% 2 == 1) 0 else 1
    for (i in 1:cur_size) {
      if (input[[glue("toggle{i}")]] %% 2 == k) runjs(glue("$('#toggle{i}').click();"))
    }
  })
}

buildset_ui <- function(cur_set) {
  fluidPage(
    
    h2(glue("Lista {substr(cur_set,4,4)}")),
    
    fluidRow(
      column(width = 12, h3("Conteúdo")),
      column(width = 10, uiOutput(glue("description_{cur_set}"))),
      column(width = 2, actionButton("toggleAllButton", label = "Alternar tudo")),
      column(width = 12, div(class = "red-rule")),
      column(width = 12, uiOutput(glue("questions_{cur_set}")))
    ),
    
    div(class = "red-rule"),
    
    fluidRow(
      column(width = 12, h3("Recapitulando"), uiOutput(glue("recap_{cur_set}")))
    )#,uiOutput("test")
  )
}



# Old functions -----------------------------------------------------------

#buildset_answers_init <- function(output, cur_set, cur_size){
#  lapply(1:cur_size, function(i){
#    output[[glue("text_{cur_set}_{i}")]] <<- renderText({ answers[[cur_set]]$text[i] })
#    output[[glue("code_{cur_set}_{i}")]] <<- renderText({ format_code(answers[[cur_set]]$code[i]) })
#    output[[glue("img_{cur_set}_{i}")]] <<- renderUI({ format_img(answers[[cur_set]]$img[i]) })
#  })
#}

#lapply(1:cur_size, function(i){
#  observeEvent(input[[glue("img_{cur_set}_{i}")]], {
#    answers_update[[glue("img_{cur_set}_{i}")]] <- util_format_base64(input, output, cur_set, i)
#  })
#})

#answers_update <- reactiveValues()
#answers_update[[glue("text_{cur_set}_{i}")]] <- answers[[cur_set]]$text[i]
#answers_update[[glue("text_{cur_set}_{i}")]] <- format_code(answers[[cur_set]]$code[i])
#answers_update[[glue("img_{cur_set}_{i}")]] <- util_format_base64(input, output, cur_set, i)


#build_update_exprs <- function(size, s){
#  expr_img <- sapply(1:size, \(i){
#    glue('observeEvent(input$img_{s}_{i}, {{
#            sets[[s]]$answers_img[{i}] <<- build_img(input, output, s, {i})
#          }})')
#  })
#  
#  expr_textcode <- sapply(1:size, \(i){
#    glue('observeEvent(input$button_{s}_{i}, {{
#            sets[[s]]$answers_text[{i}] <<- input$text_{s}_{i}; output$text_{s}_{i} <<- renderText({{sets[[s]]$answers_text[{i}]}})
#            sets[[s]]$answers_code[{i}] <<- input$code_{s}_{i}; output$code_{s}_{i} <<- renderText({{build_code_output(sets[[s]]$answers_code[{i}])}})
#          }})')
#  })
#  
#  cat(c("\n#---- Image updaters ----", expr_img,
#        "\n#---- Text and code updaters ----", expr_textcode,
#        "\n#---- onStop ----"), sep = "\n")
#}

#setfactory_server <- function(s) {
#  function(input, output, session) {
#    size <- sets[[s]]$size
#    
#    output[[glue("description_{s}")]] <- renderText({ sets[[s]]$description })
#    output[[glue("recap_{s}")]] <- renderText({ sets[[s]]$recap })
#    
#    build_current(output, s, size)
#    
#    output[[glue("questions_{s}")]] <- renderUI({
#      lapply(1:size, build_questions, s = s) #do.call(tagList, .)
#    })
#    
#    build_update_exprs(size, s)
#    
#    onStop(function(){
#      saveRDS(sets, file = "Sets/sets.RData")
#    })
#  }
#}