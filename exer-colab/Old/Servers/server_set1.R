#setfactory_server("set1")

size <- sets[[s]]$size

build_current(output, s, size)

output[[glue("questions_{s}")]] <- renderUI({
  lapply(1:size, build_questions, s = s) #do.call(tagList, .)
})

#---- Image updaters ----
observeEvent(input$img_set1_1, {
  sets[[s]]$answers_img[1] <<- build_img(input, output, s, 1)
})
observeEvent(input$img_set1_2, {
  sets[[s]]$answers_img[2] <<- build_img(input, output, s, 2)
})

#---- Text and code updaters ----
observeEvent(input$button_set1_1, {
  sets[[s]]$answers_text[1] <<- input$text_set1_1; output$text_set1_1 <<- renderText({sets[[s]]$answers_text[1]})
  sets[[s]]$answers_code[1] <<- input$code_set1_1; output$code_set1_1 <<- renderText({sets[[s]]$answers_code[1]})
})
observeEvent(input$button_set1_2, {
  sets[[s]]$answers_text[2] <<- input$text_set1_2; output$text_set1_2 <<- renderText({sets[[s]]$answers_text[2]})
  sets[[s]]$answers_code[2] <<- input$code_set1_2; output$code_set1_2 <<- renderText({sets[[s]]$answers_code[2]})
})

#---- onStop ----
onStop(function(){
  saveRDS(sets, file = "Sets/sets.RData")
})

