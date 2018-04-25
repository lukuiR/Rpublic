library(dplyr)
library(shiny)
library(DT)
library(purrr)

shinyServer(function(input, output, session) {
  
  tab_list <- NULL
  
  # Groupped mtcars
  mtSummatised <- reactive({
    mtcars %>%
      group_by(am) %>%
      summarise(
        `Average mpg` = mean(mpg),
        `Average HP` = mean(hp),
        `Average number of cylinders` = mean(cyl)
      )
  })
  
  mtSummarised2 <- reactive({
    mtcars %>%
      filter(am == mtSummatised()[input$tabela_rows_selected, ]$am) %>% 
      group_by(cyl) %>%
      summarise(
        `Average mpg` = mean(mpg),
        `Average HP` = mean(hp)
      )
  })
  
  # Generate datatable
  output$tabela <- renderDataTable({
    mtSummatised()
  })
  
  # Generate datatable
  output$tabela2 <- renderDataTable({
    if (!is.null(input$tabela_rows_selected)) {
      mtSummarised2()
    }
  })
  
  tabelaProxy <-  dataTableProxy('tabela')
  
  tabelaProxy2 <-  dataTableProxy('tabela2')
  
  # Generate plot
  output$wykres <- renderPlot({
    if (!is.null(input$tabela2_rows_selected)) {
      cylinders <- mtSummarised2()[input$tabela2_rows_selected, ]$cyl
      filteredData <- mtcars %>% filter(cyl == cylinders)
      plot(filteredData$mpg, filteredData$hp)
    }
  })
  
  
  observeEvent(input$tabela_rows_selected,
               {
                 tab_title <- "Drilldown1"
                 
                 if (tab_title %in% tab_list == FALSE){
                   
                   appendTab(inputId = "tabs",
                             tabPanel(
                               tab_title,
                               dataTableOutput("tabela2"),
                               actionLink("remove1", "Go back")
                             ))
                   
                   tab_list <<- c(tab_list, tab_title)
                   
                 }
                 
                 updateTabsetPanel(session, "tabs", selected = tab_title)
                 
               })
  
  observeEvent(input$tabela2_rows_selected,
               {
                 tab_title <- "Drilldown2"
                 
                 if (tab_title %in% tab_list == FALSE){
                   
                   appendTab(inputId = "tabs",
                             tabPanel(
                               tab_title,
                               plotOutput("wykres"),
                               actionLink("remove2", "Go back")
                             ))
                   
                   tab_list <<- c(tab_list, tab_title)
                   
                 }
                 
                 updateTabsetPanel(session, "tabs", selected = tab_title)
                 
               })
  
  observeEvent(input$remove1,{
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    # tab_list %>%
    #   walk(~removeTab("tabs", .x))
    tab_list <<- removeTab("tabs", "Drilldown1")
    tab_list <<- NULL
    tabelaProxy %>% selectRows(NULL)
  })
  
  observeEvent(input$remove2,{
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    # tab_list %>%
    #   walk(~removeTab("tabs", .x))
    tab_list <<- removeTab("tabs", "Drilldown2")
    tab_list <<- NULL
    tabelaProxy2 %>% selectRows(NULL)
    updateTabsetPanel(session, "tabs", selected = "Drilldown1")
  })
  
})