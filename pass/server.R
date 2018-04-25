library(shiny)
library(shinyalert)


server <- function(input, output, session) {


  shinyalert(
    title = "Witaj",
    text = "Wprowadz: uzytkownik/haslo",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "input",
    inputType = "text",
    inputValue = "",
    inputPlaceholder = "",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  
  zz=reactive({input$shinyalert  })
  
  output$aa<-renderText(zz())
  
  z=reactive({na.omit(o_grp[o_grp$MNEMONIK_ODBIORCY== input$shinyalert,] ) })
 
  output$dt2<- renderDataTable(
    datatable(z(),
              escape = -2,
              extensions = c('FixedHeader', 'Buttons', 'ColReorder', 'Scroller'),
              options = list(
                dom = 'Bfrti',
                autoWidth = FALSE,
                colReorder = TRUE,
                deferRender = TRUE,
                scrollX = TRUE,
                scrollY = "51vh",
                scroller = TRUE,
                scollCollapse = TRUE,
                fixedHeader = TRUE,
                columnDefs = list(
                  list(orderable = FALSE, className = 'details-control', targets = 0)
                )
              ))
  )
  
  
}



