# server.R
#info https://rstudio.github.io/leaflet/shiny.html

library(lpSolveAPI)

shinyServer(function(input, output) {
  


  


  ###################################################################
  
  # tabl Spare emision
  
  # data input
  coo <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    zz=read.csv(inFile$datapath, header = input$header,
                sep = input$sep, quote = input$quote)
    zz
  })
  coo2 <- reactive({
    inFile <- input$file2
    if (is.null(inFile))
      return(NULL)
    zz=read.csv(inFile$datapath, header = input$header1,
                sep = input$sep1, quote = input$quote1)
    zz
  })
  
  # render tabble below map of imported data
  output$cont2<- renderDataTable(coo(),  options = list(autoWidth = TRUE)
                                 
  )
  output$cont3<- renderDataTable(coo2(),  options = list(autoWidth = TRUE)
                                 
  )
  #end tab 
  ################################################################################
  #optymalizacja
  
  zz<-reactive({
    prod<-coo()
    opt<-coo2()
    if((dim(prod)[1]>0)& (dim(opt)[1]>0)){
    mylp<-make.lp(dim(opt)[2],dim(prod)[1])
  for(i in 1:dim(prod)[1]){
    set.column(mylp,i,c(prod[i,dim(prod)[2]-1],prod[i,dim(prod)[2]]))
  }
  set.constr.type(mylp,rep(">=",dim(opt)[2]))
  set.rhs(mylp, opt[1,1:dim(opt)[2]])
  set.objfn(mylp,rowSums(prod[,(dim(prod)[2]-1):(dim(prod)[2])]))
  set.type(mylp, 1:dim(prod)[1], "integer")
  #set.type(mylp, 2, "integer")
  
  solve(mylp)
  #get.objective(mylp)
   # get.constraints(mylp)
  cbind(coo()[,1:2], get.variables(mylp))}
  else{
  prod}
  
  })
  
  output$cont4<- renderDataTable(zz(),  options = list(autoWidth = TRUE)
                                 
  )
  #############################
  
})