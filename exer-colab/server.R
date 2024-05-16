server <- function(input, output, session) {
  #cur_set <- reactiveVal(NULL)
  #observeEvent(input$tabs, { cur_set(input$tabs) })
  cur_set <- function() "set1"
  
  output$toggleAll <- renderText({ "Mostrar tudo" })
  
  
  #------- Sets -------
  #---- set1 ----
  cur_size <- questions[[cur_set()]]$size
  
  # Build description and recap
  output[[glue("description_{cur_set()}")]] <- renderText({ HTML(questions[[cur_set()]]$description) })
  output[[glue("recap_{cur_set()}")]] <- renderText({ HTML(questions[[cur_set()]]$recap) })
  
  # Build initial answers
  for (q_init in 1:cur_size) {
    output[[glue("text_{cur_set()}_{q_init}")]] <- renderText({ answers[[cur_set()]]$text[q_init] })
    output[[glue("code_{cur_set()}_{q_init}")]] <- renderText({ format_code(answers[[cur_set()]]$code[q_init]) })
    output[[glue("img_{cur_set()}_{q_init}")]] <- renderUI({ format_img(answers[[cur_set()]]$img[q_init]) })
  }
  
  # Build questions
  output[[glue("questions_{cur_set()}")]] <- renderUI({
    lapply(1:cur_size, buildquest_question, cur_set = cur_set(), cur_size = cur_size)
  })
  
  #q_upd <- 1
  #observeEvent(input[[glue("button_{cur_set()}_{1}")]], {
  #  print(q_upd)
  #})
  
  # Update answers
  for (q_upd in 1:cur_size) {
    observeEvent(input[[glue("button_{cur_set()}_{q_upd}")]], {
      print(q_upd)
      answers[[cur_set()]]$text[q_upd] <- input[[glue("text_{cur_set()}_{q_upd}")]]
      output[[glue("text_{cur_set()}_{q_upd}")]] <- answers[[cur_set()]]$text[q_upd]
      answers[[cur_set()]]$code[q_upd] <- input[[glue("text_{cur_set()}_{q_upd}")]]
      output[[glue("text_{cur_set()}_{q_upd}")]] <- format_code(answers[[cur_set()]]$code[q_upd])
    })
    
    observeEvent(input[[glue("img_{cur_set()}_{q_upd}")]], {
      output[[glue("img_{cur_set()}_{q_upd}")]] <- util_format_base64(input, output, cur_set(), q_upd)
    })
  }
  
  # Toggle all button
  observeEvent(input$toggleAllButton, {
    k <- if (input$toggleAllButton %% 2 == 1) 0 else 1
    for (i in 1:cur_size) {
      if (input[[glue("toggle{i}")]] %% 2 == k) runjs(glue("$('#toggle{i}').click();"))
    }
  })
  
  
  #------- onStop -------
  onStop(onStop_data_update)
  
  #output$test <- renderText({ })
}