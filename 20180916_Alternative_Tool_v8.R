# FINAL MODEL SHINY
library(shinydashboard)
library(shinyWidgets)
library(DT)
#library(Cairo)
library(RColorBrewer)
library(plotly)
library(dygraphs)
#options(shiny.usecairo=T)

source('20180912_Alternative_Tool_Backend v2.R')



# UI ----------------------------------------------------------------------


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
              menuItem("Instructions",tabName="tab_instructions"),
              menuItem("Model Setup",tabName="tab_setup"),
              menuItem("Executive View",tabName="tab_executive"),
              menuItem("Deep Dive",tabName="tab_deepdive")
  )
)


# INSTRUCTIONS TAB --------------------------------------------------------
row_instruction_generate <- 
  fluidRow(
    box(p("Welcome to the STC workforce planning tool", 
             style = "font-family: 'Source Sans Pro';")
  ,width=12)
)


# MODEL SETUP TAB ---------------------------------------------------------

# Seperate Rows -----------------------------------------------------------


# ...Setup Rows -----------------------------------------------------------

# row_setup_generate <- fluidRow(
#   wellPanel(
#     column(align="center",
#       div(style="display:inline-block",actionButton("generateall","Generate",
#                    icon("flash"),style="color: #fff; 
#                    background-color: #337ab7; 
#                    border-color: #2e6da4;
#                    padding-left:40%;
#                    padding-right:40%;
#                    font-size:100%"),style="display:center-align"),
#   width=12),status="primary",width=12)
# )

row_setup_generate <- fluidRow(
    column(12,
           wellPanel(align="center",
                     actionButton("generateall","Generate",
                     icon("flash"),style="color: #fff;
                     background-color: #337ab7;
                     border-color: #2e6da4;
                     padding-left:40%;
                     padding-right:40%;
                     font-size:100%")
      ))
)

row_setup_scenario <- fluidRow(
  box(
    title="Scenario Selection",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    radioButtons("setup_scenario",label = "Scenario to adjust parameters for:",
                 choices = c("Conservative","Realistic","Optimistic"),inline=TRUE,selected="Realistic")
  ,width=12)
)

# ...Global Parameters ----------------------------------------------------


# ......Driver tab --------------------------------------------------------

demand_driver_adjustment_row0 <- fluidRow(
  #checkboxInput("ddlock","Maintain subscriber / revenue ratio?",value=TRUE),
  actionButton("reset_dd","Reset Demand Drivers"),
  actionButton("load_dd","Load Demand Drivers"),
  actionButton("save_dd","Save Demand Drivers")
)
  
demand_driver_adjustment_row1 <- 
      fluidRow(column(4,textOutput("driverheader1")),
               column(2,textOutput("driverheader2")),
               column(2,textOutput("driverheader3")),
               column(2,textOutput("driverheader4")),
               column(2,textOutput("driverheader5"))
      )

demand_driver_adjustment_row2 <-
      fluidRow(column(4,textOutput("driverrow1")),
               column(2,numericInput("driver_entrev0",label=NULL,value=0,min=0,max=NA,step=1)),
               column(2,numericInput("driver_entrev1",label=NULL,value=0,min=0,max=NA,step=1)),
               column(2,numericInput("driver_entrev2",label=NULL,value=0,min=0,max=NA,step=1)),
               column(2,numericInput("driver_entrev3",label=NULL,value=0,min=0,max=NA,step=1))
      )

demand_driver_adjustment_row3 <-
  fluidRow(column(4,textOutput("driverrow2")),
           column(2,numericInput("driver_conrev0",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_conrev1",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_conrev2",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_conrev3",label=NULL,value=0,min=0,max=NA,step=1))
  )

demand_driver_adjustment_row4 <-
  fluidRow(column(4,textOutput("driverrow3")),
           column(2,numericInput("driver_whorev0",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_whorev1",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_whorev2",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_whorev3",label=NULL,value=0,min=0,max=NA,step=1))
  )

demand_driver_adjustment_row5 <-
  fluidRow(column(4,textOutput("driverrow4")),
           column(2,numericInput("driver_sub0",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_sub1",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_sub2",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_sub3",label=NULL,value=0,min=0,max=NA,step=1))
  )

demand_driver_adjustment_row6 <-
  fluidRow(column(4,textOutput("driverrow5")),
           column(2,textOutput("driverrevsub0")),
           column(2,textOutput("driverrevsub1")),
           column(2,textOutput("driverrevsub2")),
           column(2,textOutput("driverrevsub3"))
  )

# ...... Baseline Positioning tab -----------------------------------------
baseline_positioning_0 <- fluidRow(
  actionButton("reset_baseline","Reset baseline positioning:"),
  actionButton("load_baseline","Load baseline positioning"),
  actionButton("save_baseline","Save baseline positioning")
)

baseline_positioning_1 <- fluidRow(column(8,sliderInput("baselinepos",
                                                      label = div(style='width:600px;', 
                                                                  div(style='float:left;', 'Conservative end of range'), 
                                                                  div(style='float:right;', 'Aggressive end of range')),
                                                      min=0,max=100,value=50,post=" %")),
                                   column(4,plotOutput("baseline_positioning_graph"))
                                   )


row_global_parameters <- fluidRow(
  box(
    title="Global Parameters",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    tabBox(
      id = "global_parameters_box",
      width=12,
      # height="175px",
      tabPanel("Demand",
               demand_driver_adjustment_row0,
               demand_driver_adjustment_row1,
               demand_driver_adjustment_row2,
               demand_driver_adjustment_row3,
               demand_driver_adjustment_row4,
               demand_driver_adjustment_row5,
               demand_driver_adjustment_row6),
      tabPanel("Baseline Positioning",
               baseline_positioning_0,
               baseline_positioning_1)
    )
    ,width=12)
)


# ...Specific Parameters -----------------------------------------------------

# ......Supply tab --------------------------------------------------------

supply_rate_adjustment <- fluidRow(
  box(title="Supply rate adjustments",solidHeader = TRUE, status="primary",colour="light blue",
      tabBox(
        id = "supply_rate_box",
        width=12,
        # height="175px",
        tabPanel("Year 1",
          fluidRow(column(6,
                          knobInput(
                             inputId = "hire_rate_y1",label = "Year 1 hire percentage:",
                             value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                             fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                   column(6,
                          knobInput(
                            inputId = "exit_rate_y1",label = "Year 1 exit percentage:",
                            value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                            fgColor = "#c9413f",angleOffset = -135,angleArc=270,inputColor = "#c9413f"))
          )),
        tabPanel("Year 2",
          fluidRow(column(6,
                          knobInput(
                            inputId = "hire_rate_y2",label = "Year 2 hire percentage:",
                            value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                            fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                   column(6,
                          knobInput(
                            inputId = "exit_rate_y2",label = "Year 2 exit percentage:",
                            value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                            fgColor = "#c9413f",angleOffset = -135,angleArc=270,inputColor = "#c9413f"))
          )),
        tabPanel("Year 3",
          fluidRow(column(6,
                          knobInput(
                            inputId = "hire_rate_y3",label = "Year 3 hire percentage:",
                            value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                            fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                   column(6,
                          knobInput(
                            inputId = "exit_rate_y3",label = "Year 3 exit percentage:",
                            value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                            fgColor = "#c9413f",angleOffset = -135,angleArc=270,inputColor = "#c9413f"))
          ))),
      width=12
  ))


# ......Automation tab -----------------------------------------------------

automation_rate_adjustment <- fluidRow(
  box(title="Automation rate adjustments",solidHeader = TRUE, status="primary",colour="light blue",
      fluidRow(column(4,sliderInput("aut_slider_y1","Year 1 Automation Level:",
                                           min=0,max=100,value= 50,step=1,post=" %")),
               column(4,sliderInput("aut_slider_y2","Year 2 Automation Level:",
                                    min=0,max=100,value= 50,step=1,post=" %")),
               column(4,sliderInput("aut_slider_y3","Year 3 Automation Level:",
                                    min=0,max=100,value= 50,step=1,post=" %"))
      ),
      fluidRow(column(4,radioButtons("aut_quarter_y1","Quarter it will impact from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1")),
               column(4,radioButtons("aut_quarter_y2","Quarter it will impact from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1")),
               column(4,radioButtons("aut_quarter_y3","Quarter it will impact from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1"))
      )
  ,width=12)
)



# ......Training tab ---------------------------------------------------------
training_spend_adjustment <- fluidRow(
  box(title="Training spend per FTE ($)",solidHeader = TRUE, status="primary",colour="light blue",
      fluidRow(column(3,numericInput("pro_input_y0","Year 0 training spend / FTE :",
                                     min=0,max=50000,value= 2707,step=1)),
               column(3,numericInput("pro_input_y1","Year 1 training spend / FTE:",
                                      min=0,max=50000,value= 2707,step=1)),
               column(3,numericInput("pro_input_y2","Year 2 training spend / FTE:",
                                     min=0,max=50000,value= 2707,step=1)),
               column(3,numericInput("pro_input_y3","Year 3 training spend / FTE:",
                                     min=0,max=50000,value= 2707,step=1))
      )
  ,width=12)
)


# ......Outsourcing tab ---------------------------------------------------

outsourcing_adjustment <- fluidRow(
  box(title="Outsource Section?",solidHeader = TRUE, status="primary",colour="light blue",
      fluidRow(column(4,checkboxInput("out_considered","Outsource identified sections within function?",
                                     value=FALSE)),
               column(4,selectInput("out_year","Year to outsource in:",
                                     c("Y0","Y1","Y2","Y3"))),
               column(4,selectInput("out_quarter","Quarter to outsource in:",
                                    c("Q1","Q2","Q3","Q4")))
      )
      ,width=12)
)

# In-tab horizontal tabs --------------------------------------------------

row_setup_l0l1l2select <- fluidRow(
  box(
    title="Specific Parameters",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    fluidRow(
      column(4,uiOutput("l0_list_indep")),
      column(4,uiOutput("l1_list_indep")),
      column(4,uiOutput("l2_list_indep"))
    ),
    fluidRow(
      column(3,actionButton("generate","Load selected")),
      column(3,actionButton("save","Save selected")),
      column(3,actionButton("reset_select","Reset selected")),
      column(3,actionButton("reset_all","Reset all"))
    ),
    fluidRow(
      tabBox(id = "setup_tabs",
             height="400px",
             tabPanel("Supply",
                      supply_rate_adjustment),
             tabPanel("Automation",
                      automation_rate_adjustment),
             tabPanel("Training",
                      training_spend_adjustment),
             tabPanel("Outsourcing",
                      outsourcing_adjustment),
             width=12
      )
    )
    ,width=12)
)


# EXECUTIVE VIEW TAB -----------------------------------------------------

row_exec_scenario <- fluidRow(
  box(
    title="Scenario Selection",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    radioButtons("exec_scenario",label = "Scenario to display:",
                 choices = c("Conservative","Realistic","Optimistic"),inline=TRUE,selected="Realistic")
    ,width=6),
  box(
    title="View by headcount or cost?",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    radioGroupButtons("headcount_or_cost",
                         choices = c("Headcount","Cost"),
                         justified = TRUE,status="primary",
                         checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
    ),width=6)
)

row_exec_l0l1l2select <- fluidRow(
  box(
    title="Function selection",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    fluidRow(
      column(4,uiOutput("l0_list_exec")),
      column(4,uiOutput("l1_list_exec")),
      column(4,uiOutput("l2_list_exec"))
    )
  ,width=9),
  box(
    title="Segment",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    fluidRow(
      column(12,pickerInput("segment_list_exec","Segment to display",
                         choices = c("Core","Support","Specialist","Critical"),
                         multiple=TRUE,options=list(`actions-box`=TRUE),
                         selected=c("Core","Support","Specialist","Critical"))
             )
      )
  ,width=3)
)

row_executivetest <- fluidRow(
  actionButton("load_exec_data","Load exec filtered table output"),
  actionButton("generate_csv","Generate csv of final data"))

row_executive_waterfall <- fluidRow(
  box(
    title="Workforce Projection",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    plotOutput("line",hover = "lineplot_hover", hoverDelay = 0),width=12)
)

row_executive_barchart <- fluidRow(
  box(
    title="Yearly Demand",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    plotOutput("barchart"),width=6),
  box(
    title="Demand/Supply Gap",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    plotOutput("supplydemandbarchart"),width=6)
)


# DEEP DIVE VIEW TAB ------------------------------------------------------


# ...Section Selection ----------------------------------------------------

row_deep_generate <- fluidRow(
  box(
    column(align="center",
           div(style="display:inline-block",actionButton("deep_generate","Generate",
                                                         icon("flash"),style="color: #fff; 
                                                         background-color: #337ab7; 
                                                         border-color: #2e6da4;
                                                         padding-left:40%;
                                                         padding-right:40%;
                                                         font-size:100%"),style="display:center-align"),
           width=12),status="primary",width=12)
)

row_deep_selection_singlebul <- fluidRow(
  box(selectInput("single_agg_choice","View by: ",
                  choices = c("Single Section Data","Aggregated Data"),
                  selected="Aggregated Data",multiple=FALSE),
      width=6,status="primary"))

row_deep_selection_secselect <- fluidRow(
  box(
    title="Section Selection:",solidHeader = TRUE,status="primary",colour="light blue",
    column(6,uiOutput("deep_l0_list"),uiOutput("deep_l2_list"),uiOutput("deep_l4_list")),
    column(6,uiOutput("deep_l1_list"),uiOutput("deep_l3_list"),pickerInput("segment_list_deep","Segment:",
                                                                           choices = c("Core","Support","Specialist","Critical"),
                                                                           multiple=TRUE,options=list(`actions-box`=TRUE),
                                                                           selected=c("Core","Support","Specialist","Critical"))),
    width=12
  )
)


# ...Summary graphs ----------------------------------------------------------

row_deep_summary_headcount <- fluidRow(box(
  title="Headcount Distribution",solidHeader = TRUE,status="primary",colour="light blue",
  plotOutput("deep_hc_graph",height="125px"),width=12)
)

row_deep_summary_segmenteducation <- fluidRow(
  box(
    title="Segmentation",solidHeader = TRUE,status="primary",colour="light blue",
    plotOutput("deep_segment_graph")
    ,width=6),
  box(
    title="Education",solidHeader = TRUE,status="primary",colour="light blue",
    plotOutput("deep_education_graph")
    ,width=6)
  )

row_deep_summary_agetenure <- fluidRow(
  box(
    title="Age Distribution",solidHeader = TRUE,status="primary",colour="light blue",
    plotOutput("deep_age_graph"),width=6),
  box(
    title="Tenure Distribution",solidHeader = TRUE,status="primary",colour="light blue",
    plotOutput("deep_tenure_graph"),width=6)
)

# ...Output charts -----------------------------------------------------------

row_scenario <- box(
  radioButtons("deep_scenario",label = "Scenario to adjust parameters for:",
               choices = c("Conservative","Realistic","Optimistic"),inline=TRUE,selected="Realistic")
  ,width=12)

row_sliders <- tabBox(
  id="tabs_deepdive_sliders",
  width=12,
  tabPanel("Drivers",
           sliderInput("deep_slider_dd_Y1","Demand driver impact Y1:",
                       min=50,max=150,value=1),
           sliderInput("deep_slider_dd_Y2","Demand driver impact Y2:",
                       min=50,max=150,value=1),
           sliderInput("deep_slider_dd_Y3","Demand driver impact Y3:",
                       min=50,max=150,value=1)
           ),
  tabPanel("Baseline Positioning",
           sliderInput("deep_slider_baseline","Baseline positioning:",
                       min=0,max=100,value=50)),
  tabPanel("Supply",
           tabBox(
             id="tabs_deepdive_sliders_supply",
             tabPanel("Y1",
                       sliderInput("deep_slider_hire_Y1","Y1 Hire Rate:",
                                   min=50,max=150,value=1),
                       sliderInput("deep_slider_exit_Y1","Y1 Exit Rate:",
                                   min=50,max=150,value=1)),
             tabPanel("Y2",
                       sliderInput("deep_slider_hire_Y2","Y2 Hire Rate:",
                                   min=50,max=150,value=1),
                       sliderInput("deep_slider_exit_Y2","Y2 Exit Rate:",
                                   min=50,max=150,value=1)),
             tabPanel("Y3",
                       sliderInput("deep_slider_hire_Y3","Y3 Hire Rate:",
                                   min=50,max=150,value=1),
                       sliderInput("deep_slider_exit_Y3","Y3 Hire Rate:",
                                   min=50,max=150,value=1))
           )
           ),
  tabPanel("Automation",
           fluidRow(
             column(4,sliderInput("deep_slider_aut_y1","Year 1 Automation Level:",
                                    min=0,max=100,value= 50,step=1,post=" %")),
             column(4,sliderInput("deep_slider_aut_y2","Year 2 Automation Level:",
                                  min=0,max=100,value= 50,step=1,post=" %")),
             column(4,sliderInput("deep_slider_aut_y3","Year 3 Automation Level:",
                                  min=0,max=100,value= 50,step=1,post=" %"))
             ),
           fluidRow(column(4,radioButtons("deep_aut_quarter_y1","Quarter it will impact from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1")),
               column(4,radioButtons("deep_aut_quarter_y2","Quarter it will impact from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1")),
               column(4,radioButtons("deep_aut_quarter_y3","Quarter it will impact from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1"))
               )
  ),
  tabPanel("Training",
           fluidRow(
             column(4,sliderInput("deep_slider_pro_y1","Year 1 Training Impact:",
                                  min=0,max=100,value= 50,step=1,post=" %")),
             column(4,sliderInput("deep_slider_pro_y2","Year 2 Training Impact:",
                                  min=0,max=100,value= 50,step=1,post=" %")),
             column(4,sliderInput("deep_slider_pro_y3","Year 3 Training Impact:",
                                  min=0,max=100,value= 50,step=1,post=" %"))
           )
           ),
  tabPanel("Outsourcing",
           fluidRow(column(4,checkboxInput("deep_out_considered","Outsource identified sections within function?",
                                           value=FALSE)),
                    column(4,selectInput("deep_out_year","Year to outsource in:",
                                         c("Y0","Y1","Y2","Y3"))),
                    column(4,selectInput("deep_out_quarter","Quarter to outsource in:",
                                         c("Q1","Q2","Q3","Q4")))
           )
  )
)

   

row_outputtable <- fluidRow(
  dataTableOutput("deep_final_table")
)

row_outputtablesum <- fluidRow(
  dataTableOutput("deep_final_table_summarised")
)

row_outputtableover <- fluidRow(
  dataTableOutput("deep_final_table_overwritten")
)



# ...Bringing all together ------------------------------------------------

tabs_deepdive <- tabBox(
  id = "tabs_deepdive",
  width=12,
  # height="175px",
  tabPanel("Section Selection",
           row_deep_generate,
           row_deep_selection_singlebul,
           row_deep_selection_secselect),
  tabPanel("Section Summary",
           row_deep_summary_headcount,
           row_deep_summary_segmenteducation,
           row_deep_summary_agetenure),
  tabPanel("Workforce Journey",
           row_scenario,
           row_sliders,
           row_outputtable,
           row_outputtablesum,
           row_outputtableover),
  tabPanel("Workforce Forecast")
)
#,width=12)




  
# Body --------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tab_instructions",
            titlePanel("Welcome to the STC Workforce Planning Tool"),
            row_instruction_generate
    ),
    tabItem(tabName = "tab_setup",
            row_setup_generate,
            row_setup_scenario,
            row_global_parameters,
            row_setup_l0l1l2select),
    tabItem(tabName = "tab_executive",
            row_executivetest,
            row_exec_l0l1l2select,
            row_exec_scenario,
            row_executive_waterfall,
            row_executive_barchart
            ),
    tabItem(tabName = "tab_deepdive",
            tabs_deepdive)
  )
)



# Bringing UI together ----------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title="Workforce Planning Tool",titleWidth = 300),
  sidebar,
  body
)

# Server Logic ------------------------------------------------------------

server <- function(input,output,session) {


# Graph theme -------------------------------------------------------------

  CustomTheme <- theme_bw()+
    theme(
      legend.text = element_text(colour="black"),
      legend.title = element_text(colour="black"),
      panel.background = element_rect("white"),
      plot.background = element_rect("white"),
      panel.border = element_rect("grey",fill=NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(hjust=0.5,colour="black"),
      axis.title = element_text(colour="black"),
      axis.text = element_text(colour="black")
    )
  
# Creating reactive values ------------------------------------------------

  
  rvalues <- reactiveValues()
  
  # Check to see if first time page has been loaded, used for setting initial values
  rvalues$loaded <- 0
  

# Check to see if data has been generated ---------------------------------
  rvalues$generated <- 0
  rvalues$deep_generated <- 0

  
  
  # Setting raw data values: This is the full raw data table
  rvalues$rawdata <- output_table
  
  rvalues$rawdata_filtered <- output_table
  
  # Setting default values for final data: This is the full final output data table
  rvalues$final_data <- output_table
  
  # Setting raw values for demand drivers
  rvalues$dd_raw <- data_drivervals
  
  # Setting default values for demand drivers final
  rvalues$dd_final <- filter(data_drivervals,is.na(driver)==FALSE)
  
  # Seting raw values for baseline positioning
  #rvalues$baselinepos <- data_baselinepos

  temporary_data_dd <- reactive({
    rvalues$temporary_table_dd = rvalues$dd_final
  })
  
  # Other values that are created along the way, but not preset
  # rvalues$rawdata_filtered      This is the raw data for the sections selected to adjust
  # rvalues$temporary_table       This is the table of values being adjusted
  # rvalues$final_data_filtered   This is the final data for the sections selected to adjust

  final_data <- reactive(
    return(rvalues$rawdata)
  )

# SETUP TAB ---------------------------------------------------------------
  baseline_positioning_table <- reactive({
    table <- tibble(
      headcount = c(1000,250,750,750-(input$baselinepos/100 * (750-250))),
      scenario = c("Current Headcount","Baseline Minimum","Baseline Maximum","Chosen Projection Start Point"))
  })

  output$baseline_positioning_graph <- renderPlot({
    table <- baseline_positioning_table()
    graph <- ggplot(table,aes(x=1,y=headcount,group=scenario))+
      geom_point(data=table[table$scenario=="Current Headcount",],mapping=aes(shape=16),size=4,colour="royalblue4")+
      geom_point(data=table[table$scenario=="Baseline Maximum",],mapping=aes(shape=95),size=10,colour="darkred")+
      geom_point(data=table[table$scenario=="Baseline Minimum",],mapping=aes(shape=95),size=10,colour="seagreen4")+
      geom_point(data=table[table$scenario=="Chosen Projection Start Point",],mapping=aes(shape=95),size=10,colour="royalblue1")+
      scale_shape_identity()+
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.ticks=element_blank(),
            axis.text = element_blank(),
            axis.title=element_blank(),
            panel.grid = element_blank(),
            panel.border=element_blank(),
            panel.background = element_blank())
      
    graph
  })
  
  observeEvent(input$baselinepos,{
    baseline_positioning_table()
  })


# EXECUTIVE VIEW TAB ------------------------------------------------------

    

# Text outputs ------------------------------------------------------------

  output$driverheader1 <- renderText("Driver")
  output$driverheader2 <- renderText("Year 0 Value")
  output$driverheader3 <- renderText("Year 1 Value")
  output$driverheader4 <- renderText("Year 2 Value")
  output$driverheader5 <- renderText("Year 3 Value")
  output$driverrow1 <- renderText("Enterprise Revenue ($m)")
  output$driverrow2 <- renderText("Consumer Revenue ($m)")
  output$driverrow3 <- renderText("Wholesale Revenue ($m)")
  output$driverrow4 <- renderText("Total STC Subscribers (m)")
  output$driverrow5 <- renderText("Revenue / Subscriber")
  
  output$driverrevsub0 <- renderText(round((input$driver_entrev0+input$driver_conrev0+input$driver_whorev0)/input$driver_sub0,1))
  output$driverrevsub1 <- renderText(round((input$driver_entrev1+input$driver_conrev0+input$driver_whorev1)/input$driver_sub1,1))
  output$driverrevsub2 <- renderText(round((input$driver_entrev2+input$driver_conrev0+input$driver_whorev2)/input$driver_sub2,1))
  output$driverrevsub3 <- renderText(round((input$driver_entrev3+input$driver_conrev0+input$driver_whorev3)/input$driver_sub3,1))
  

# MODEL SETUP TAB CALCULATIONS-------------------------------------
  
# Function to reset all to default values ---------------------------------

  f_reset_all <- function(){
    rvalues$final_data = rvalues$rawdata
    print("All values reset to defaults")
  }
  

# Function to reset selected to default values ---------------------------------

  f_reset_selected <- function(){
    # Loading up the raw filtered table into memory
    raw_filtered_setup()
    raw_filtered_setup_scenario()
    
    # Setting tables to be equal to raw values
    rvalues$temporary_table = rvalues$rawdata_filtered
    rvalues$final_data_filtered = rvalues$rawdata_filtered
    f_overwrite_table()
    print("Selected function values reset to defaults")
  }
  
# Producing dropdowns and inputs: Filtering based on segment ----------------------------------------------
#   This produces the inputs for choosing which area to edit
  # Producing L0 list dropdown
  output$l0_list_indep <- renderUI ({
    selectInput(inputId = "l0_list_indep",label = "Unit to adjust:",
                choices = c("All",output_l0_list),selected="All")
  })
  
  # Producing L1 list dropdown
  output$l1_list_indep <- renderUI ({
    selectInput(inputId = "l1_list_indep",label = "Sector to adjust:",
                choices = c("All",l1_list()),selected="All")
  })
  
  # Producing L2 list dropdown
  output$l2_list_indep <- renderUI ({
    selectInput(inputId = "l2_list_indep",label = "General Directorate to adjust:",
                choices = c("All",l2_list()),selected="All")
  })

# ... Calculating L1 level calculations ---------------------------------------

  # ... Producing list of L1s based off selected L0 -----------------------------
  l1_list <- reactive({
    output = rvalues$rawdata %>%
      filter(l0==input$l0_list_indep)
    output = output$l1
    print("Calculated l1 list")
    print("Using l0 value")
    print("Using raw data:")
    print(output)
    return(output)
  })

# ... Producing list of L2s based off selected L1 -----------------------------
  l2_list <- reactive({
    output = rvalues$rawdata %>%
      filter(l0==input$l0_list_indep & l1==input$l1_list_indep)
    output = output$l2
    return(output)
  })


# Producing filtered tables - raw and final -------------------------------
  
  # ... Producing filtered raw table by selected L0/L1/L2 ----------------------------------

  raw_filtered_setup <- reactive({
    print("Calculating raw filtered setup")
    print("L0 value:")
    print(input$l0_list_indep)
    print("L1 value:")
    print(input$l1_list_indep)
    print("L2 value:")
    print(input$l2_list_indep)
    if(is.null(input$l2_list_indep)){
      output = rvalues$rawdata
    } else {
      if(input$l0_list_indep=="All" & input$l1_list_indep=="All" & input$l2_list_indep=="All"){
        output = rvalues$rawdata
      } else {
        if(input$l1_list_indep=="All" & input$l2_list_indep=="All"){
          output = rvalues$rawdata %>%
            filter(l0==input$l0_list_indep)
        } else {
          if(input$l2_list_indep=="All"){
            output = rvalues$rawdata %>%
              filter(l0==input$l0_list_indep & l1==input$l1_list_indep)
          } else {
            output = rvalues$rawdata %>%
              filter(l0==input$l0_list_indep & l1==input$l1_list_indep & l2==input$l2_list_indep)
          }
        }
      }
    }
    rvalues$rawdata_filtered = output
  })
  

# ...Selecting relevant columns based on scenario dropdown ---------------
  raw_filtered_setup_scenario <- reactive({
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    colnames = colnames(raw_filtered_setup())
    output = rvalues$rawdata_filtered %>%
      select(unique_identifier,l0,l1,l2,l3,l4,baseline_mid,grep(scenario,colnames))

    rvalues$rawdata_filtered = output
  })
  
  output$print_raw_filtered_setup_scenario <- renderTable(rvalues$rawdata_filtered)

# ... Producing filtered final table by selected L0/L1/L2 --------------------------
  final_filtered_setup <- reactive({
    print("Calculating final filtered setup")
    if(input$l0_list_indep=="All" & input$l1_list_indep=="All" & input$l2_list_indep=="All"){
      output = rvalues$final_data
    } else {
      if(input$l1_list_indep=="All" & input$l2_list_indep=="All"){
        output = rvalues$final_data %>%
          filter(l0==input$l0_list_indep)
      } else {
        if(input$l2_list_indep=="All"){
          output = rvalues$final_data %>%
            filter(l0==input$l0_list_indep & l1==input$l1_list_indep)
        } else {
          output = rvalues$final_data %>%
            filter(l0==input$l0_list_indep & l1==input$l1_list_indep & l2==input$l2_list_indep)
        }
      }
    }
    
    rvalues$final_data_filtered = output
  })


# Creating overwrite functionality ----------------------------------------

# ... Creating temporary table --------------------------------------
  # This table will store values that are being edited
  temporary_data_filtered_scenario <- reactive({
    rvalues$temporary_table = rvalues$final_data_filtered
  })

# ... Function to overwrite final slider table with temporary table vals --------

  f_overwrite_table <- function(){
    # Bringing in temporary data table
    tempdata = rvalues$temporary_table # New data
    print("Overwriting table")
    print("Temporary data:")
    #print(tempdata$hire_mid_Y3)
    #final_data # Final data
    
    # Setting output table to be equal to the current final data
    outputtable = rvalues$final_data
    print("Outputtable (final_data)")
    #print(outputtable$hire_mid_Y3)
    # Setting colnames of newdata to have temp in the title
    colnames = colnames(tempdata)
    for(x in colnames){
      inputname <- as.name(x)
      outputname <- paste(x,"temp",sep="_")
      tempdata = tempdata %>%
        mutate(!!outputname := !!inputname) 
      tempdata = tempdata %>%
        select(-!!inputname)
    }
    
    # Joining the temporary and old together, replacing where necessary
    outputtable = left_join(outputtable,tempdata,by=c("unique_identifier"="unique_identifier_temp"))
    outputtable$unique_identifier_temp <- outputtable$unique_identifier
    for(x in colnames){
      inputcolname_orig = as.name(x)
      inputcolname_temp = as.name(paste(x,"temp",sep="_"))
      outputcolname = x
      outputtable = outputtable %>%
        mutate(!!outputcolname := ifelse(is.na(!!inputcolname_temp),!!inputcolname_orig,!!inputcolname_temp)) %>%
        select(-!!inputcolname_temp)
    }
    
    # Setting final data table to be equal to output table
    rvalues$final_data = outputtable
    print("Overwriting final data")
  }
  
  #observeEvent(input$save,{f_overwrite_table()})

# ... Function to work out selection level ------------------------------------

  f_selection_level <- function(){
    if(is.null(input$l2_list_indep)){
      print("Level select: Input is null")
      return("Level All")
    } else {
      if(input$l0_list_indep=="All" & input$l1_list_indep=="All" & input$l2_list_indep=="All"){
        return("Level All")
      } else {
        if(input$l1_list_indep=="All" & input$l2_list_indep=="All"){
          return("Level 0")
        } else {
          if(input$l2_list_indep=="All"){
            return("Level 1")
          } else {
            return("Level 2")
          }
        }
      }
    }
  }
  

# ... Function to pull supply values from filtered table -------------------------------
  # Call on raw_filtered_setup_scenario to get default values
  f_pull_supply <-  function(table) {
    print("Pulling supply values")
    print("Input scenario:")
    print(input$setup_scenario)
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    columnhire_y1 = paste("hire",scenario,"Y1",sep="_")
    columnexit_y1 = paste("exit",scenario,"Y1",sep="_")
    columnhire_y2 = paste("hire",scenario,"Y2",sep="_")
    columnexit_y2 = paste("exit",scenario,"Y2",sep="_")
    columnhire_y3 = paste("hire",scenario,"Y3",sep="_")
    columnexit_y3 = paste("exit",scenario,"Y3",sep="_")
    
    columnhire_name_y1 = as.name(columnhire_y1)
    columnexit_name_y1 = as.name(columnexit_y1)
    columnhire_name_y2 = as.name(columnhire_y2)
    columnexit_name_y2 = as.name(columnexit_y2)
    columnhire_name_y3 = as.name(columnhire_y3)
    columnexit_name_y3 = as.name(columnexit_y3)
    
    level = f_selection_level()
    if(level=="Level All"){
      output = table %>%
        select(baseline_mid,!!columnhire_name_y1,!!columnexit_name_y1,!!columnhire_name_y2,!!columnexit_name_y2,!!columnhire_name_y3,!!columnexit_name_y3) %>%
        group_by()
    } else {
      if(level=="Level 0"){
        output = table %>%
          select(l0,baseline_mid,!!columnhire_name_y1,!!columnexit_name_y1,!!columnhire_name_y2,!!columnexit_name_y2,!!columnhire_name_y3,!!columnexit_name_y3) %>%
          group_by(l0)
      } else {
        if(level=="Level 1"){
          output = table %>%
            select(l1,baseline_mid,!!columnhire_name_y1,!!columnexit_name_y1,!!columnhire_name_y2,!!columnexit_name_y2,!!columnhire_name_y3,!!columnexit_name_y3) %>%
            group_by(l1)
        } else {
          if(level=="Level 2"){
            output = table %>%
              select(l2,baseline_mid,!!columnhire_name_y1,!!columnexit_name_y1,!!columnhire_name_y2,!!columnexit_name_y2,!!columnhire_name_y3,!!columnexit_name_y3) %>%
              group_by(l2)
          }
        }
      }
    }

    # Calculating total headcount of selected
    totalhc = output %>%
      summarise("baseline_mid"=sum(baseline_mid))
    totalhc = as.numeric(totalhc$baseline_mid)
    
    for(x in c("Y1","Y2","Y3")){
      outputcol_hire <- paste("hire",scenario,x,sep="_")
      inputcol_hire <- as.name(paste("hire",scenario,x,sep="_"))
      
      outputcol_exit <- paste("exit",scenario,x,sep="_")
      inputcol_exit <- as.name(paste("exit",scenario,x,sep="_"))
      
      output = output %>%
        mutate(!!outputcol_hire := !!inputcol_hire/totalhc*baseline_mid) %>%
        mutate(!!outputcol_exit := !!inputcol_exit/totalhc*baseline_mid)
      }

    # Weighting every multiplier by the section's proportion of the whole

    
    # Adding up these to get the weighted average
    output = output %>%
      summarise(sum(get(columnhire_y1),na.rm=TRUE),sum(get(columnexit_y1),na.rm=TRUE),
                sum(get(columnhire_y2),na.rm=TRUE),sum(get(columnexit_y2),na.rm=TRUE),
                sum(get(columnhire_y3),na.rm=TRUE),sum(get(columnexit_y3),na.rm=TRUE))

    if(level=="Level All"){
      output = output %>% 
        mutate("fillcol"="All") %>%
        select(fillcol,everything())
    }
    
    return(output)
  }
  
  # Function to set supply rates to default or final
  f_set_supply <- function(default_or_final){
    if(default_or_final=="default"){
      data = rvalues$rawdata_filtered
    } else {
      data = rvalues$final_data_filtered
    }
    table = f_pull_supply(data)
    print(paste0("Setting supply ",default_or_final))
    
    hirevalue_y1 = as.numeric(table[2])
    print(paste0(default_or_final," hire value y1: ",hirevalue_y1))
    exitvalue_y1 = as.numeric(table[3])
    print(paste0(default_or_final," exit value y1: ",exitvalue_y1))
    hirevalue_y2 = as.numeric(table[4])
    exitvalue_y2 = as.numeric(table[5])
    hirevalue_y3 = as.numeric(table[6])
    exitvalue_y3 = as.numeric(table[7])
    updateKnobInput(session=session,inputId="hire_rate_y1",label=NULL,value=(hirevalue_y1-1)*100)
    updateKnobInput(session=session,inputId="exit_rate_y1",label=NULL,value=(1-exitvalue_y1)*100)
    updateKnobInput(session=session,inputId="hire_rate_y2",label=NULL,value=(hirevalue_y2-1)*100)
    updateKnobInput(session=session,inputId="exit_rate_y2",label=NULL,value=(1-exitvalue_y2)*100)
    updateKnobInput(session=session,inputId="hire_rate_y3",label=NULL,value=(hirevalue_y3-1)*100)
    updateKnobInput(session=session,inputId="exit_rate_y3",label=NULL,value=(1-exitvalue_y3)*100)
  }
  

# ... Function to pull automation values from filtered table --------------

  f_pull_automation <-  function(table) {
    print("Pulling automation values")
    print("Input scenario:")
    print(input$setup_scenario)
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    columnaut_y1 = paste("aut",scenario,"Y1",sep="_")
    columnaut_y2 = paste("aut",scenario,"Y2",sep="_")
    columnaut_y3 = paste("aut",scenario,"Y3",sep="_")
    columnautq_y1 = paste("aut",scenario,"Y1","q",sep="_")
    columnautq_y2 = paste("aut",scenario,"Y2","q",sep="_")
    columnautq_y3 = paste("aut",scenario,"Y3","q",sep="_")
    
    columnaut_name_y1 = as.name(columnaut_y1)
    columnaut_name_y2 = as.name(columnaut_y2)
    columnaut_name_y3 = as.name(columnaut_y3)
    columnautq_name_y1 = as.name(columnautq_y1)
    columnautq_name_y2 = as.name(columnautq_y2)
    columnautq_name_y3 = as.name(columnautq_y3)
    
    level = f_selection_level()
    if(level=="Level All"){
      output = table %>%
        select(baseline_mid,!!columnaut_name_y1,!!columnaut_name_y2,!!columnaut_name_y3,!!columnautq_name_y1,!!columnautq_name_y2,!!columnautq_name_y3) %>%
        group_by()
    } else {
      if(level=="Level 0"){
        output = table %>%
          select(l0,baseline_mid,!!columnaut_name_y1,!!columnaut_name_y2,!!columnaut_name_y3,!!columnautq_name_y1,!!columnautq_name_y2,!!columnautq_name_y3) %>%
          group_by(l0)
      } else {
        if(level=="Level 1"){
          output = table %>%
            select(l1,baseline_mid,!!columnaut_name_y1,!!columnaut_name_y2,!!columnaut_name_y3,!!columnautq_name_y1,!!columnautq_name_y2,!!columnautq_name_y3) %>%
            group_by(l1)
        } else {
          if(level=="Level 2"){
            output = table %>%
              select(l2,baseline_mid,!!columnaut_name_y1,!!columnaut_name_y2,!!columnaut_name_y3,!!columnautq_name_y1,!!columnautq_name_y2,!!columnautq_name_y3) %>%
              group_by(l2)
          }
        }
      }
    }
    
    # Calculating total headcount of selected
    totalhc = output %>%
      summarise("baseline_mid"=sum(baseline_mid))
    totalhc = as.numeric(totalhc$baseline_mid)
    
    for(x in c("Y1","Y2","Y3")){
      outputcol_aut <- paste("aut",scenario,x,sep="_")
      inputcol_aut <- as.name(paste("aut",scenario,x,sep="_"))
      outputcol_autq <- paste("aut",scenario,x,"q",sep="_")
      inputcol_autq <- as.name(paste("aut",scenario,x,"q",sep="_"))
      
      output = output %>%
        mutate(!!outputcol_aut := !!inputcol_aut/totalhc*baseline_mid) %>%
        mutate(!!outputcol_autq := !!inputcol_autq/totalhc*baseline_mid)
    }
    
    # Weighting every multiplier by the section's proportion of the whole
    
    # Adding up these to get the weighted average
    output = output %>%
      summarise(sum(get(columnaut_y1)),
                sum(get(columnaut_y2)),
                sum(get(columnaut_y3)),
                sum(get(columnautq_y1)),
                sum(get(columnautq_y2)),
                sum(get(columnautq_y3))
                )
    
    if(level=="Level All"){
      output = output %>% 
        mutate("fillcol"="All") %>%
        select(fillcol,everything())
    }
    
    print(output)
    return(output)
  }
  
  # Function to set automation rates to default or final
  f_set_aut <- function(default_or_final){
    if(default_or_final=="default"){
      data = rvalues$rawdata_filtered
    } else {
      data = rvalues$final_data_filtered
    }
    table = f_pull_automation(data)
    print(paste0("Setting automation ",default_or_final))
    
    autvalue_y1 = as.numeric(table[2])
    autvalue_y2 = as.numeric(table[3])
    autvalue_y3 = as.numeric(table[4])
    print(paste0(default_or_final," aut value y3: ",autvalue_y3))
    autqvalue_y1 = as.numeric(table[5])
    autqvalue_y2 = as.numeric(table[6])
    autqvalue_y3 = as.numeric(table[7])
    print(paste0(default_or_final," autq value y3: ",autqvalue_y3))
    updateSliderInput(session=session,inputId="aut_slider_y1",label=NULL,value=(1-autvalue_y1)*100)
    updateSliderInput(session=session,inputId="aut_slider_y2",label=NULL,value=(1-autvalue_y2)*100)
    updateSliderInput(session=session,inputId="aut_slider_y3",label=NULL,value=(1-autvalue_y3)*100)
    updateRadioButtons(session=session,inputId = "aut_quarter_y1",label=NULL,selected=paste0("Q",round(autqvalue_y1)))
    updateRadioButtons(session=session,inputId = "aut_quarter_y2",label=NULL,selected=paste0("Q",round(autqvalue_y2)))
    updateRadioButtons(session=session,inputId = "aut_quarter_y3",label=NULL,selected=paste0("Q",round(autqvalue_y3)))
  }
  
  output$printer <- renderTable(f_pull_supply(rvalues$final_data_filtered))
  

# Function to pull productivity values from filtered table ----------------

  f_pull_productivity <-  function(table) {
    print("Pulling productivity values")
    print("Input scenario:")
    print(input$setup_scenario)
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    columnpro_y0 = paste("pro",scenario,"Y0",sep="_")
    columnpro_y1 = paste("pro",scenario,"Y1",sep="_")
    columnpro_y2 = paste("pro",scenario,"Y2",sep="_")
    columnpro_y3 = paste("pro",scenario,"Y3",sep="_")
    
    columnpro_name_y0 = as.name(columnpro_y0)
    columnpro_name_y1 = as.name(columnpro_y1)
    columnpro_name_y2 = as.name(columnpro_y2)
    columnpro_name_y3 = as.name(columnpro_y3)

    level = f_selection_level()
    if(level=="Level All"){
      output = table %>%
        select(baseline_mid,!!columnpro_name_y0,!!columnpro_name_y1,!!columnpro_name_y2,!!columnpro_name_y3) %>%
        group_by()
    } else {
      if(level=="Level 0"){
        output = table %>%
          select(l0,baseline_mid,!!columnpro_name_y0,!!columnpro_name_y1,!!columnpro_name_y2,!!columnpro_name_y3) %>%
          group_by(l0)
      } else {
        if(level=="Level 1"){
          output = table %>%
            select(l1,baseline_mid,!!columnpro_name_y0,!!columnpro_name_y1,!!columnpro_name_y2,!!columnpro_name_y3) %>%
            group_by(l1)
        } else {
          if(level=="Level 2"){
            output = table %>%
              select(l2,baseline_mid,baseline_mid,!!columnpro_name_y0,!!columnpro_name_y1,!!columnpro_name_y2,!!columnpro_name_y3) %>%
              group_by(l2)
          }
        }
      }
    }
    
    # Calculating total headcount of selected
    totalhc = output %>%
      summarise("baseline_mid"=sum(baseline_mid))
    totalhc = as.numeric(totalhc$baseline_mid)
    
    for(x in c("Y0","Y1","Y2","Y3")){
      outputcol_pro <- paste("pro",scenario,x,sep="_")
      inputcol_pro <- as.name(paste("pro",scenario,x,sep="_"))
      
      output = output %>%
        mutate(!!outputcol_pro := !!inputcol_pro/totalhc*baseline_mid)
    }
    
    # Weighting every multiplier by the section's proportion of the whole
    
    # Adding up these to get the weighted average
    output = output %>%
      summarise(sum(get(columnpro_y0)),
                sum(get(columnpro_y1)),
                sum(get(columnpro_y2)),
                sum(get(columnpro_y3))
      )
    
    if(level=="Level All"){
      output = output %>% 
        mutate("fillcol"="All") %>%
        select(fillcol,everything())
    }
    
    print(output)
    return(output)
  }
  
  # Function to set productivity rates to default or final
  f_set_pro <- function(default_or_final){
    if(default_or_final=="default"){
      data = rvalues$rawdata_filtered
    } else {
      data = rvalues$final_data_filtered
    }
    table = f_pull_productivity(data)
    print(paste0("Setting productivity ",default_or_final))
    
    provalue_y0 = as.numeric(table[2])
    provalue_y1 = as.numeric(table[3])
    provalue_y2 = as.numeric(table[4])
    provalue_y3 = as.numeric(table[5])

    print(paste0(default_or_final," pro value y3: ",provalue_y3))

    updateNumericInput(session=session,inputId="pro_input_y0",label=NULL,value=provalue_y0)
    updateNumericInput(session=session,inputId="pro_input_y1",label=NULL,value=provalue_y1)
    updateNumericInput(session=session,inputId="pro_input_y2",label=NULL,value=provalue_y2)
    updateNumericInput(session=session,inputId="pro_input_y3",label=NULL,value=provalue_y3)
  }
  

# Function to set demand driver figures to default or final -----
  f_set_dd <- function(default_or_final){
    if(default_or_final=="default"){
      data = rvalues$rvalues$dd_raw
    } else {
      data = rvalues$rvalues$dd_final
    }
    
    
  }
    
  
# Function to pull if considered for outsourcing --------------------------
  
  f_pull_outsourcing <-  function(table) {
    print("Pulling outsourcing consideration status")
    print("Input scenario:")
    print(input$setup_scenario)
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    columnout_considered = paste("out_considered",scenario,sep="_")
    columnout_year = paste("out_year",scenario,sep="_")
    columnout_quarter = paste("out_quarter",scenario,sep="_")
    
    columnout_considered_name = as.name(columnout_considered)
    columnout_year_name = as.name(columnout_year)
    columnout_quarter_name = as.name(columnout_quarter)
    
    level = f_selection_level()
    if(level=="Level All"){
      output = table %>%
        select(baseline_mid,!!columnout_considered_name,!!columnout_year_name,!!columnout_quarter_name) %>%
        group_by()
    } else {
      if(level=="Level 0"){
        output = table %>%
          select(l0,baseline_mid,!!columnout_considered_name,!!columnout_year_name,!!columnout_quarter_name) %>%
          group_by(l0)
      } else {
        if(level=="Level 1"){
          output = table %>%
            select(l1,baseline_mid,!!columnout_considered_name,!!columnout_year_name,!!columnout_quarter_name) %>%
            group_by(l1)
        } else {
          if(level=="Level 2"){
            output = table %>%
              select(l2,baseline_mid,!!columnout_considered_name,!!columnout_year_name,!!columnout_quarter_name) %>%
              group_by(l2)
          }
        }
      }
    }
    
    # Calculating total headcount of selected
    totalhc = output %>%
      summarise("baseline_mid"=sum(baseline_mid))
    totalhc = as.numeric(totalhc$baseline_mid)
    
    # Calculating total headcount of selected which can be outsourced
    out_year_name <- as.name(paste("out_year",scenario,sep="_"))
    totalhc_out = output %>%
      filter(is.na(!!out_year_name)==FALSE)
    if(nrow(totalhc_out)==0){
      totalhc_out=0
    } else {
      totalhc_out <- totalhc_out %>%
        summarise("baseline_mid"=sum(baseline_mid))
      totalhc_out = as.numeric(totalhc_out$baseline_mid)
    }
    
    # Considered for outsourcing = 1, not considered = 0
    # Not to be confused with out_yesno which is if it can be outsourced
    outputcol_outcon <- paste("out_considered",scenario,sep="_")
    inputcol_outcon <- as.name(outputcol_outcon)
    
    # outyesno not considered here as can't be edited by user
    
    outputcol_outyear = paste("out_year",scenario,sep="_")
    inputcol_outyear = as.name(outputcol_outyear)
    
    outputcol_outquarter = paste("out_quarter",scenario,sep="_")
    inputcol_outquarter = as.name(outputcol_outquarter)
    
    # Weighting every factor by the section's proportion of the whole
    
    output = output %>%
      # outcon here decides if whole function should be considered
      mutate(!!outputcol_outcon := !!inputcol_outcon/totalhc*baseline_mid) %>%
      
      # outyear here decides what year, uses headcount only of those with years (totalhc_out)
      mutate(!!outputcol_outyear := !!inputcol_outyear/totalhc_out*baseline_mid) %>%
      mutate(!!outputcol_outquarter := !!inputcol_outquarter/totalhc_out*baseline_mid)
    
    # Adding up these to get the weighted average
    output = output %>%
      summarise(sum(get(outputcol_outcon),na.rm = TRUE),
                sum(get(outputcol_outyear),na.rm = TRUE),
                sum(get(outputcol_outquarter),na.rm = TRUE)
      )
    
    if(level=="Level All"){
      output = output %>% 
        mutate("fillcol"="All") %>%
        select(fillcol,everything())
    }
    
    print(output)
    return(output)
    
  }

  f_set_out <- function(default_or_final){
    if(default_or_final=="default"){
      data = rvalues$rawdata_filtered
    } else {
      data = rvalues$final_data_filtered
    }
    table = f_pull_outsourcing(data)
    print(paste0("Setting outsourcing consideration ",default_or_final))
    
    outcon = ifelse(round(as.numeric(table[2]),0)==0,FALSE,TRUE)
    outyear = paste0("Y",round(as.numeric(table[3]),0))
    outquarter = paste0("Q",round(as.numeric(table[4]),0))
    # Not currently calculating year right - should ignore blanks - or is it because non outsourced
    print(paste0(default_or_final," Outsourcing consideration status: ",outcon))
    print(paste0(default_or_final," Outsourcing year: ",outyear))
    print(paste0(default_or_final," Outsourcing quarter: ",outquarter))
    
    updateCheckboxInput(session=session,inputId="out_considered",value=outcon)
    updateSelectInput(session=session,inputId="out_year",selected=outyear)
    updateSelectInput(session=session,inputId="out_quarter",selected=outquarter)
  }
  

# Function to pull baseline positioning -----------------------------------

  f_pull_baseline <-  function(table) {
    print("Pulling baseline values")
    print("Input scenario:")
    print(input$setup_scenario)
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    columnbaseline = paste("baselinepos",scenario,sep="_")
    
    columnbaseline_name = as.name(columnbaseline)

    output=table
    
    # Calculating total headcount of selected
    # This isn't strictly necessary for baseline as they will all have same default values
    # Keeping it for consistency with others
    totalhc = output %>%
      group_by() %>%
      summarise("baseline_mid"=sum(baseline_mid))
    totalhc = as.numeric(totalhc$baseline_mid)
    
    
    # Weighting every multiplier by the section's proportion of the whole
    outputcol_base<- paste("baselinepos",scenario,sep="_")
    inputcol_base <- as.name(paste("baselinepos",scenario,sep="_"))
      
    output = output %>%
      rowwise() %>%
      mutate(!!outputcol_base := !!inputcol_base/!!totalhc*baseline_mid)
    

    # Adding up these to get the weighted average
    output = output %>%
      group_by() %>%
      summarise(sum(get(outputcol_base))
      )
    
    print(output)
    return(output)
  }
  
  
  f_set_baseline <- function(default_or_final){
    if(default_or_final=="default"){
      data = rvalues$rawdata
    } else {
      data = rvalues$final_data
    }
    table = f_pull_baseline(data)
    print(paste0("Setting baseline ",default_or_final))
    
    baselinevalue = as.numeric(table[1])*100

    print(paste0(default_or_final," baseline value: ",baselinevalue))
    
    updateSliderInput(session=session,inputId="baselinepos",value=baselinevalue)
  }
  
  
  # Function to set demand driver figures to default or final -----
  f_set_dd <- function(default_or_final){
    if(default_or_final=="default"){
      data = rvalues$rvalues$dd_raw
    } else {
      data = rvalues$rvalues$dd_final
    }
  }
  
  #... Observe events fill temporary table -------------------------------------
    
  observeEvent(input$l2_list_indep,{
    print("Observing L2 selection")
    if(is.null(input$l2_list_indep)==FALSE){
      rvalues$final_data
      final_filtered_setup()
    }
  })
  
  observeEvent(input$generate,{
    final_filtered_setup()
    f_set_supply("final")
    f_set_aut("final")
    f_set_pro("final")
    f_set_out("final")
    print("")
  })
  
  observeEvent(input$reset_all,{
    f_reset_all()
    # Running generate functions to reload new reset values
    final_filtered_setup()
    f_set_supply("default")
    f_set_aut("default")
    f_set_pro("default")
    f_set_out("default")
    })
  
  observeEvent(input$reset_select,{
    f_reset_selected()
    # Running generate functions to reload new reset values
    final_filtered_setup()
    f_set_supply("default")
    f_set_aut("default")
    f_set_pro("default")
    f_set_out("default")
  })
  

# ... Pull driver values ------------------------------
  
f_pull_dd <- function(table){
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    table = table %>% select(driver,year,!!scenario)
    colnames(table)=c("driver","year","value")
    
    entrev0 = table %>% filter(driver=="entrev" & year == 0) %>% select(value) %>% as.numeric()
    entrev1 = table %>% filter(driver=="entrev" & year == 1) %>% select(value) %>% as.numeric()
    entrev2 = table %>% filter(driver=="entrev" & year == 2) %>% select(value) %>% as.numeric()
    entrev3 = table %>% filter(driver=="entrev" & year == 3) %>% select(value) %>% as.numeric()
    
    conrev0 = table %>% filter(driver=="conrev" & year == 0) %>% select(value) %>% as.numeric()
    conrev1 = table %>% filter(driver=="conrev" & year == 1) %>% select(value) %>% as.numeric()
    conrev2 = table %>% filter(driver=="conrev" & year == 2) %>% select(value) %>% as.numeric()
    conrev3 = table %>% filter(driver=="conrev" & year == 3) %>% select(value) %>% as.numeric()
    
    whorev0 = table %>% filter(driver=="whorev" & year == 0) %>% select(value) %>% as.numeric()
    whorev1 = table %>% filter(driver=="whorev" & year == 1) %>% select(value) %>% as.numeric()
    whorev2 = table %>% filter(driver=="whorev" & year == 2) %>% select(value) %>% as.numeric()
    whorev3 = table %>% filter(driver=="whorev" & year == 3) %>% select(value) %>% as.numeric()
    
    stcsub0 = table %>% filter(driver=="stcsub" & year == 0) %>% select(value) %>% as.numeric()
    stcsub1 = table %>% filter(driver=="stcsub" & year == 1) %>% select(value) %>% as.numeric()
    stcsub2 = table %>% filter(driver=="stcsub" & year == 2) %>% select(value) %>% as.numeric()
    stcsub3 = table %>% filter(driver=="stcsub" & year == 3) %>% select(value) %>% as.numeric()
    
    updateNumericInput(session=session,"driver_entrev0",value=entrev0)
    updateNumericInput(session=session,"driver_entrev1",value=entrev1)
    updateNumericInput(session=session,"driver_entrev2",value=entrev2)
    updateNumericInput(session=session,"driver_entrev3",value=entrev3)
    
    updateNumericInput(session=session,"driver_conrev0",value=conrev0)
    updateNumericInput(session=session,"driver_conrev1",value=conrev1)
    updateNumericInput(session=session,"driver_conrev2",value=conrev2)
    updateNumericInput(session=session,"driver_conrev3",value=conrev3)
    
    updateNumericInput(session=session,"driver_whorev0",value=whorev0)
    updateNumericInput(session=session,"driver_whorev1",value=whorev1)
    updateNumericInput(session=session,"driver_whorev2",value=whorev2)
    updateNumericInput(session=session,"driver_whorev3",value=whorev3)
    
    updateNumericInput(session=session,"driver_sub0",value=stcsub0)
    updateNumericInput(session=session,"driver_sub1",value=stcsub1)
    updateNumericInput(session=session,"driver_sub2",value=stcsub2)
    updateNumericInput(session=session,"driver_sub3",value=stcsub3)

  }


# Updating subscribers/FTE ratio ------------------------------------------

  observeEvent({
    input$driver_entrev0
    input$driver_conrev0
    input$driver_whorev0
    input$driver_sub0
  },{
    output$driverrevsub0 <- renderText(round((input$driver_entrev0+input$driver_conrev0+input$driver_whorev0)/input$driver_sub0,1))
  })
  
  observeEvent({
    input$driver_entrev1
    input$driver_conrev1
    input$driver_whorev1
    input$driver_sub1
  },{
    output$driverrevsub1 <- renderText(round((input$driver_entrev1+input$driver_conrev1+input$driver_whorev1)/input$driver_sub1,1))
  })
  
  observeEvent({
    input$driver_entrev2
    input$driver_conrev2
    input$driver_whorev2
    input$driver_sub2
  },{
    output$driverrevsub2 <- renderText(round((input$driver_entrev2+input$driver_conrev2+input$driver_whorev2)/input$driver_sub2,1))
  })
  
  observeEvent({
    input$driver_entrev3
    input$driver_conrev3
    input$driver_whorev3
    input$driver_sub3
  },{
    output$driverrevsub3 <- renderText(round((input$driver_entrev3+input$driver_conrev3+input$driver_whorev3)/input$driver_sub3,1))
  })
  
  # ... Loading and resetting demand drivers -----------------------------------------

observeEvent(input$reset_dd,{
  f_pull_dd(rvalues$dd_raw)
})

observeEvent(input$load_dd,{
  f_pull_dd(rvalues$dd_final)
})


# ... Loading and resetting baseline positioning ------------------------------
observeEvent(input$reset_baseline,{
  f_set_baseline("default")
})

observeEvent(input$load_baseline,{
  f_set_baseline("final")
})

observeEvent(input$save_baseline,{
  
  scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
  
  value = input$baselinepos/100
  column =paste("baselinepos",scenario,sep="_")
  
  rvalues$final_data <- rvalues$final_data %>% mutate(!!column:=value)
  
},priority=200)
  
# ... Actions when demand driver save button clicked --------------------------
  
  # Setting ddfinal to be equal to the input drivers
  observeEvent(input$save_dd,{
    
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    table <- tibble(driver=c("entrev","entrev","entrev","entrev",
                             "conrev","conrev","conrev","conrev",
                             "whorev","whorev","whorev","whorev",
                             "stcsub","stcsub","stcsub","stcsub"),
                    year=c(0,1,2,3,
                           0,1,2,3,
                           0,1,2,3,
                           0,1,2,3),
                    value=c(input$driver_entrev0,input$driver_entrev1,input$driver_entrev2,input$driver_entrev3,
                            input$driver_conrev0,input$driver_conrev1,input$driver_conrev2,input$driver_conrev3,
                            input$driver_whorev0,input$driver_whorev1,input$driver_whorev2,input$driver_whorev3,
                            input$driver_sub0,input$driver_sub1,input$driver_sub2,input$driver_sub3))
    colnames(table) = c("driver","year",scenario)
    
    rvalues$dd_final <- left_join(select(rvalues$dd_final,-one_of(scenario)),table)

    },priority=200)




# ...Calculating the multiplier implied by the demand drivers -----
# WILL NEED TO BE UPDATED TO USE BASELINE_MIN, BASELINE_MID AND BASELINE_MAX
observeEvent(input$save_dd,{
    table <- rvalues$dd_final
    data_drivermults_0s <- table %>% filter(year==0) %>% select(-year)
    colnames(data_drivermults_0s) <- paste0(colnames(data_drivermults_0s),"_0")
    data_drivermults <- left_join(table,data_drivermults_0s,by=c("driver"="driver_0"))
    data_drivermults <- data_drivermults %>%
      mutate(min=min/min_0) %>%
      mutate(mid=mid/mid_0) %>%
      mutate(max=max/max_0) %>%
      select(-min_0,-mid_0,-max_0)

    hctable <- left_join(select(rvalues$final_data,unique_identifier,baseline_mid),data_driverhc)
    hctable <- hctable %>%
      select(-unique_identifier) %>%
      group_by(driver_group) %>%
      summarise(sum(baseline_mid))
    colnames(hctable) = c("driver","hc")
    hcmult <- left_join(data_drivermults,hctable) %>%
      mutate(min=min*hc) %>%
      mutate(mid=mid*hc) %>%
      mutate(max=max*hc) %>%
      select(-driver) %>%
      group_by(year) %>%
      summarise(min=sum(min),max=sum(max),mid=sum(mid))
    hcmult_0 <- hcmult %>%
      filter(year==0)
    min_hc = hcmult_0 %>% select(min) %>% as.numeric()
    mid_hc = hcmult_0 %>% select(mid) %>% as.numeric()
    max_hc = hcmult_0 %>% select(max) %>% as.numeric()

    hcmult <- hcmult %>% select(year,min,mid,max)
    
    hcmult[2] = hcmult[2]/min_hc
    hcmult[3] = hcmult[3]/mid_hc
    hcmult[4] = hcmult[4]/max_hc

    hcmult$driver = "headcount"

    data_drivermults <- bind_rows(data_drivermults,hcmult)
    print(data_drivermults)

    rvalues$dd_multiplier <- data_drivermults
    
    data_drivermults_min <- data_drivermults %>%
      select(driver,year,min) %>%
      spread(year,min)
    colnames(data_drivermults_min) <- c("driver",paste0("dd_min_Y",colnames(data_drivermults_min[2:ncol(data_drivermults_min)])))

    data_drivermults_mid <- data_drivermults %>%
      select(driver,year,mid) %>%
      spread(year,mid)
    colnames(data_drivermults_mid) <- c("driver",paste0("dd_mid_Y",colnames(data_drivermults_mid[2:ncol(data_drivermults_mid)])))

    data_drivermults_max <- data_drivermults %>%
      select(driver,year,max) %>%
      spread(year,max)
    colnames(data_drivermults_max) <- c("driver",paste0("dd_max_Y",colnames(data_drivermults_max[2:ncol(data_drivermults_max)])))

    data_drivermults_all <- left_join(data_drivermults_min,data_drivermults_mid)
    data_drivermults_all <- left_join(data_drivermults_all,data_drivermults_max)
    colnames <- colnames(data_drivermults_all[2:ncol(data_drivermults_all)])

    rvalues$final_data <- left_join(rvalues$final_data,data_drivermults_all,by=c("driver_group"="driver"))

    for(x in colnames){
      y=x
      z=as.name(paste0(y,".y"))
      z2=paste0(y,".y")
      z3=paste0(y,".x")

      rvalues$final_data <- rvalues$final_data %>%
        mutate(!!y:=!!z) %>%
        select(-one_of(!!z2)) %>%
        select(-one_of(!!z3))
    }

    print(rvalues$final_data)
  },priority=100)


#... Actions when save button clicked ----------------------------------------

  # Loading temporary data table into memory
  observeEvent(input$save,{
    temporary_data_filtered_scenario()
  },priority=300)
  
  # Setting hire values in temporary table when save clicked
  observeEvent(input$save,{
    hirevalue_y1 = input$hire_rate_y1
    hirevalue_y2 = input$hire_rate_y2
    hirevalue_y3 = input$hire_rate_y3
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    outputcol_y1 = paste("hire",scenario,"Y1",sep="_")
    outputcol_y2 = paste("hire",scenario,"Y2",sep="_")
    outputcol_y3 = paste("hire",scenario,"Y3",sep="_")
    table = rvalues$temporary_table

    table = table %>%
      mutate(!!outputcol_y1 := (100+hirevalue_y1)/100) %>%
      mutate(!!outputcol_y2 := (100+hirevalue_y2)/100) %>%
      mutate(!!outputcol_y3 := (100+hirevalue_y3)/100)
    rvalues$temporary_table = table
    print(paste0("Saved hire value: ",hirevalue_y1))
  },priority=200)

  # Setting exit values in temporary table when save clicked
  observeEvent(input$save,{
    exitvalue_y1 = input$exit_rate_y1
    exitvalue_y2 = input$exit_rate_y2
    exitvalue_y3 = input$exit_rate_y3
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    outputcol_y1 = paste("exit",scenario,"Y1",sep="_")
    outputcol_y2 = paste("exit",scenario,"Y2",sep="_")
    outputcol_y3 = paste("exit",scenario,"Y3",sep="_")
    table = rvalues$temporary_table
    
    table = table %>%
      mutate(!!outputcol_y1 := (100-exitvalue_y1)/100) %>%
      mutate(!!outputcol_y2 := (100-exitvalue_y2)/100) %>%
      mutate(!!outputcol_y3 := (100-exitvalue_y3)/100)
    rvalues$temporary_table = table
    print(paste0("Saved exit value: ",exitvalue_y1))
  },priority=200)
  
  # Setting automation in temporary table when save clicked
  observeEvent(input$save,{
    autvalue_y1 = input$aut_slider_y1
    autvalue_y2 = input$aut_slider_y2
    autvalue_y3 = input$aut_slider_y3
    autqvalue_y1 = as.numeric(substr(input$aut_quarter_y1,nchar(input$aut_quarter_y1),nchar(input$aut_quarter_y1)))
    autqvalue_y2 = as.numeric(substr(input$aut_quarter_y2,nchar(input$aut_quarter_y2),nchar(input$aut_quarter_y2)))
    autqvalue_y3 = as.numeric(substr(input$aut_quarter_y3,nchar(input$aut_quarter_y3),nchar(input$aut_quarter_y3)))
    
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    outputcol_y1 = paste("aut",scenario,"Y1",sep="_")
    outputcol_y2 = paste("aut",scenario,"Y2",sep="_")
    outputcol_y3 = paste("aut",scenario,"Y3",sep="_")
    outputcolq_y1 = paste("aut",scenario,"Y1","q",sep="_")
    outputcolq_y2 = paste("aut",scenario,"Y2","q",sep="_")
    outputcolq_y3 = paste("aut",scenario,"Y3","q",sep="_")
    
    table = rvalues$temporary_table
    
    table = table %>%
      mutate(!!outputcol_y1 := (100-autvalue_y1)/100) %>%
      mutate(!!outputcol_y2 := (100-autvalue_y2)/100) %>%
      mutate(!!outputcol_y3 := (100-autvalue_y3)/100) %>%
      mutate(!!outputcolq_y1 := autqvalue_y1) %>%
      mutate(!!outputcolq_y2 := autqvalue_y2) %>%
      mutate(!!outputcolq_y3 := autqvalue_y3)
    rvalues$temporary_table = table
    print(paste0("Saved automation value: ",autvalue_y1))
    print(paste0("Saved automation quarter value: ",autqvalue_y1))
  },priority=200)
  
  # Setting productivity in temporary table when save clicked
  observeEvent(input$save,{
    provalue_y0 = input$pro_input_y0
    provalue_y1 = input$pro_input_y1
    provalue_y2 = input$pro_input_y2
    provalue_y3 = input$pro_input_y3
    
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    outputcol_y0 = paste("pro",scenario,"Y0",sep="_")
    outputcol_y1 = paste("pro",scenario,"Y1",sep="_")
    outputcol_y2 = paste("pro",scenario,"Y2",sep="_")
    outputcol_y3 = paste("pro",scenario,"Y3",sep="_")
    
    table = rvalues$temporary_table
    
    table = table %>%
      mutate(!!outputcol_y0 := provalue_y0) %>%
      mutate(!!outputcol_y1 := provalue_y1) %>%
      mutate(!!outputcol_y2 := provalue_y2) %>%
      mutate(!!outputcol_y3 := provalue_y3)

    rvalues$temporary_table = table
    print(paste0("Saved y0 productivity value: ",provalue_y0))
    print(paste0("Saved y1 productivity value: ",provalue_y1))
    print(paste0("Saved y2 productivity value: ",provalue_y2))
    print(paste0("Saved y3 productivity value: ",provalue_y3))
    
  },priority=200)
  
  # Setting outsourcing in temporary table when save clicked
  observeEvent(input$save,{
    out_considered = ifelse(input$out_considered==TRUE,1,0)
    out_year = as.numeric(substr(input$out_year,nchar(input$out_year),nchar(input$out_year)))
    out_quarter = as.numeric(substr(input$out_quarter,nchar(input$out_quarter),nchar(input$out_quarter)))
    
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    outputcol_considered = paste("out_considered",scenario,sep="_")
    outputcol_year = paste("out_year",scenario,sep="_")
    outputcol_quarter = paste("out_quarter",scenario,sep="_")
    
    table = rvalues$temporary_table
    
    table = table %>%
      mutate(!!outputcol_considered := out_considered) %>%
      mutate(!!outputcol_year := out_year) %>%
      mutate(!!outputcol_quarter := out_quarter)
    
    rvalues$temporary_table = table
    print(paste0("Saved outsource considered value: ",out_considered))
    print(paste0("Saved outsource year: ",out_year))
    print(paste0("Saved outsource quarter: ",out_quarter))
    
  },priority=200)
  
  # Overwriting data when save clicked
  observeEvent(input$save,{
    f_overwrite_table()
    print("Overwritten table")
  },priority = 100)

  # Setting dropdowns to all when one changed
  observeEvent(input$l0_list_indep,{
    updateSelectInput(session=session,"l1_list_indep",selected = "All")
    updateSelectInput(session=session,"l2_list_indep",selected = "All")
  })
  
  observeEvent(input$l1_list_indep,{
    updateSelectInput(session=session,"l2_list_indep",selected = "All")
  })
  
  # For the first time it is generated, to preload the values
  observeEvent(input$l2_list_indep,{
    if(rvalues$loaded == 0){
      f_set_supply("default")
      f_set_aut("default")
      f_set_pro("default")
      f_set_out("default")
    }
    rvalues$loaded <- 1
  },priority=-1)


# ...Loading all values again when scenario changes -----------------------
observeEvent(input$setup_scenario,{
  # If it is the first time, load default values, otherwise load new values
  if(rvalues$loaded == 0){
    f_pull_dd(rvalues$dd_raw)
    f_set_baseline("default")
  } else {
    f_pull_dd(rvalues$dd_final)
    f_set_baseline("default")
    f_set_supply("final")
    f_set_aut("final")
    f_set_pro("final")
    f_set_out("final")
  }
  #final_filtered_setup()
  
},priority=0)
  
  

# EXECUTIVE DASHBOARD CALCULATIONS ----------------------------------------


# Dropdown creation -------------------------------------------------------

  # ... Producing list of L1s based off selected L0 -----------------------------
  l1_list_exec <- reactive({
    print(input$l0_list_exec)
    output = rvalues$rawdata %>%
      filter(l0 %in% input$l0_list_exec)
    output = unique(output$l1)
    return(output)
  })
  
  # ... Producing list of L2s based off selected L1 -----------------------------
  l2_list_exec <- reactive({
    output = rvalues$rawdata %>%
      filter(l0 %in% input$l0_list_exec & l1 %in% input$l1_list_exec)
    output = unique(output$l2)
    return(output)
  })
  
  # Producing L0 list dropdown
  output$l0_list_exec <- renderUI ({
    pickerInput(inputId = "l0_list_exec",label = "Unit to adjust:",
                choices = c(output_l0_list),multiple=TRUE,selected=apply(output_l0_list,2,as.character),
                options=list(`actions-box`=TRUE))
  })
  
  # Producing L1 list dropdown
  output$l1_list_exec <- renderUI ({
    pickerInput(inputId = "l1_list_exec",label = "Sector to adjust:",
                choices = c(l1_list_exec()),multiple=TRUE,selected=c(l1_list_exec()),
                options=list(`actions-box`=TRUE))
  })
  
  # Producing L2 list dropdown
  output$l2_list_exec <- renderUI ({
    pickerInput(inputId = "l2_list_exec",label = "General Directorate to adjust:",
                choices = c(l2_list_exec()),multiple=TRUE,selected=c(l2_list_exec()),
                options=list(`actions-box`=TRUE))
  })  


# Filtering output table based on selections ------------------------------

  observeEvent({
    input$l2_list_exec
    input$segment_list_exec},{
    rvalues$final_data_exec = rvalues$final_data %>%
      filter(l0 %in% input$l0_list_exec & l1 %in% input$l1_list_exec & l2 %in% input$l2_list_exec,segment %in% input$segment_list_exec)
    print(rvalues$final_data_exec)
  })
  

# Turning final table into final headcount table -----------------------

final_headcount_table <- reactive({

  withProgress(message="Generating data",value=0, {
    
      data_import = rvalues$final_data
      cols_all <- colnames(data_import)
    
      #...Calculating Supply Percentages ------------------------------------
      incProgress(0.1,"Calculating supply impact")
      # We have hire_min_y1 to hire_max_y3, and exit_min_y1 to exit_max_y1
      
      # Need to turn into same, but as percentages
      
      cols_all <- colnames(data_import)
      # Bringing out suppy columns
      data_supply <- select(data_import,unique_identifier,grep("hire_",cols_all),grep("exit_",cols_all))
      
      cols_data_supply <- colnames(data_supply[2:ncol(data_supply)])
      
      # Turning into combined supply column
      for(x in c("min","mid","max")){
        for(y in c("Y1","Y2","Y3")){
          input_hire = as.name(paste("hire",x,y,sep="_"))
          input_exit = as.name(paste("exit",x,y,sep="_"))
          output_supply = paste("supply",x,y,sep="_")
          data_supply <- data_supply %>%
            mutate(!!output_supply := (!!input_hire-1)+(!!input_exit-1)+1) %>%
            select(-!!input_hire,-!!input_exit)
        }
      }
      
      data_supply_cumulative <- data_supply
      
      # Turning it cumulative
      for(x in c("min","mid","max")){
        inputcol_y1 = as.name(paste("supply",x,"Y1",sep="_"))
        inputcol_y2 = as.name(paste("supply",x,"Y2",sep="_"))
        inputcol_y3 = as.name(paste("supply",x,"Y3",sep="_"))
        for(y in c("Y0","Y1","Y2","Y3")){
          outputcol = paste("supply",x,y,"cumu",sep="_")
          data_supply_cumulative <- data_supply_cumulative %>%
            mutate(!!outputcol := ifelse(!!y=="Y0",1,
                                         ifelse(!!y=="Y1",1*!!inputcol_y1,
                                                ifelse(!!y=="Y2",1*!!inputcol_y1*!!inputcol_y2,
                                                       1*!!inputcol_y1*!!inputcol_y2*!!inputcol_y3))))
        }
      }
      cols_data_supply_cumulative <- colnames(data_supply_cumulative)
      data_supply_cumulative <- data_supply_cumulative %>% 
        select(unique_identifier,grep("_cumu",cols_data_supply_cumulative))
      
      # Turning it into quarters
      data_supply_cumulative_qs <- data_supply_cumulative
      colnames(data_supply_cumulative_qs) <- c("unique_identifier",paste0(colnames(data_supply_cumulative_qs[2:ncol(data_supply_cumulative_qs)]),"_q4"))
      for(x in c("min","mid","max")){
        for(y in c("Y1","Y2","Y3")){
          year = as.numeric(substr(y,nchar(y),nchar(y)))
          print(year)
          inputcol_q0 <- as.name(paste("supply",x,paste0("Y",year-1),"cumu","q4",sep="_"))
          inputcol_q4 <- as.name(paste("supply",x,y,"cumu","q4",sep="_"))
          outputcol_q1 <- paste("supply",x,y,"cumu","q1",sep="_")
          outputcol_q2 <- paste("supply",x,y,"cumu","q2",sep="_")
          outputcol_q3 <- paste("supply",x,y,"cumu","q3",sep="_")
          outputcol_q4 <- paste("supply",x,y,"cumu","q4",sep="_")
          
          data_supply_cumulative_qs = data_supply_cumulative_qs %>%
            rowwise %>%
            mutate(!!outputcol_q1:=ifelse(year==0,1,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*1)) %>%
            mutate(!!outputcol_q2:=ifelse(year==0,1-(1-!!inputcol_q4)/3*1,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*2)) %>%
            mutate(!!outputcol_q3:=ifelse(year==0,1-(1-!!inputcol_q4)/3*2,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*3)) %>%
            mutate(!!outputcol_q4:= !!inputcol_q4)
          
        }
      }
      
    # ...Calculating Productivity Percentages ------------------------------------
      incProgress(0.1,"Calculating training impact")
      data_pro <- select(data_import,unique_identifier,grep("pro_",cols_all))
      
      cols_data_pro <- colnames(data_pro[2:ncol(data_pro)])
      
      # Turning into the multiplier
      for(x in cols_data_pro){
        outputcol <- x
        inputcol <- as.name(x)
        data_pro <- data_pro %>% mutate(!!outputcol := ifelse(!!inputcol>6000,(1-0.0225),1-(0.0225*!!inputcol/6000)))
      }
    
      data_pro_cumulative <- data_pro
      
      # Turning it cumulative
      for(x in c("min","mid","max")){
        # inputcol_y0 = as.name(paste("pro",x,"Y0",sep="_"))
        inputcol_y1 = as.name(paste("pro",x,"Y1",sep="_"))
        inputcol_y2 = as.name(paste("pro",x,"Y2",sep="_"))
        inputcol_y3 = as.name(paste("pro",x,"Y3",sep="_"))
        for(y in c("Y0","Y1","Y2","Y3")){
          outputcol = paste("pro",x,y,"cumu",sep="_")
          data_pro_cumulative <- data_pro_cumulative %>%
            mutate(!!outputcol := ifelse(!!y=="Y0",1,
                                         ifelse(!!y=="Y1",1*!!inputcol_y1,
                                                ifelse(!!y=="Y2",1*!!inputcol_y1*!!inputcol_y2,
                                                       1*!!inputcol_y1*!!inputcol_y2*!!inputcol_y3))))
        }
      }
      cols_data_pro_cumulative <- colnames(data_pro_cumulative)
      data_pro_cumulative <- data_pro_cumulative %>% 
        select(unique_identifier,grep("_cumu",cols_data_pro_cumulative))
      
      # Turning it into quarters
      data_pro_cumulative_qs <- data_pro_cumulative
      colnames(data_pro_cumulative_qs) <- c("unique_identifier",paste0(colnames(data_pro_cumulative_qs[2:ncol(data_pro_cumulative_qs)]),"_q4"))
      for(x in c("min","mid","max")){
        for(y in c("Y1","Y2","Y3")){
          year = as.numeric(substr(y,nchar(y),nchar(y)))
          print(year)
          # inputcol_q0 <- ifelse(year==0,1,as.name(paste("pro",x,paste0("Y",year-1),"cumu","q4",sep="_")))
          inputcol_q0 <- as.name(paste("pro",x,paste0("Y",year-1),"cumu","q4",sep="_"))
          inputcol_q4 <- as.name(paste("pro",x,y,"cumu","q4",sep="_"))
          outputcol_q1 <- paste("pro",x,y,"cumu","q1",sep="_")
          outputcol_q2 <- paste("pro",x,y,"cumu","q2",sep="_")
          outputcol_q3 <- paste("pro",x,y,"cumu","q3",sep="_")
          outputcol_q4 <- paste("pro",x,y,"cumu","q4",sep="_")
          
          data_pro_cumulative_qs = data_pro_cumulative_qs %>%
            rowwise %>%
            mutate(!!outputcol_q1:=ifelse(year==0,1,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*1)) %>%
            mutate(!!outputcol_q2:=ifelse(year==0,1-(1-!!inputcol_q4)/3*1,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*2)) %>%
            mutate(!!outputcol_q3:=ifelse(year==0,1-(1-!!inputcol_q4)/3*2,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*3)) %>%
            mutate(!!outputcol_q4:= !!inputcol_q4)
          
        }
      }
      
      # ...Calculating Automation percentages ------------------------------------
      
      incProgress(0.1,"Calculating automation impact")
      
      data_aut <- select(data_import,unique_identifier,grep("aut_",cols_all))
      
      # Turning it into quarters
      data_aut_qs <- data_aut
      
      for(x in c("min","mid","max")){
        for(y in c("Y1","Y2","Y3")){
          for(z in c("q1","q2","q3","q4")){
            year = as.numeric(substr(y,nchar(y),nchar(y)))
            quarter = as.numeric(substr(z,nchar(z),nchar(z)))
            autcol = as.name(paste("aut",x,y,sep="_"))
            
            input_yearcol = as.name(paste("aut",x,y,sep="_"))
            input_quartercol = as.name(paste("aut",x,y,"q",sep="_"))
            
            previouscol = as.name(ifelse(quarter==1,paste("aut",x,paste0("Y",year-1),"q4",sep="_"),paste("aut",x,paste0("Y",year),paste0("q",quarter-1),sep="_")))
            
            outputcol = paste("aut",x,y,z,sep="_")
            
            data_aut_qs = data_aut_qs %>%
              mutate(!!outputcol:=ifelse(!!input_quartercol==quarter,!!input_yearcol,ifelse(year==1 & quarter == 1,1,!!previouscol)))
            
          }
        }
      }
      
      # ...Calculating demand driver percentages -----------------------------------
      
      incProgress(0.1,"Calculating demand impact")
      
      # Bringing out demand columns
      data_dd <- select(data_import,unique_identifier,grep("dd_",cols_all))
      cols_data_dd <- colnames(data_dd[2:ncol(data_dd)])
      
      data_dd_qs <- data_dd
      colnames(data_dd_qs) <- c("unique_identifier",paste0(colnames(data_dd_qs[2:ncol(data_dd_qs)]),"_q4"))
      for(x in c("min","mid","max")){
        for(y in c("Y1","Y2","Y3")){
          year = as.numeric(substr(y,nchar(y),nchar(y)))
          print(year)
          
          inputcol_q0 <- as.name(paste("dd",x,paste0("Y",year-1),"q4",sep="_"))
          inputcol_q4 <- as.name(paste("dd",x,y,"q4",sep="_"))
          outputcol_q1 <- paste("dd",x,y,"q1",sep="_")
          outputcol_q2 <- paste("dd",x,y,"q2",sep="_")
          outputcol_q3 <- paste("dd",x,y,"q3",sep="_")
          outputcol_q4 <- paste("dd",x,y,"q4",sep="_")
          
          data_dd_qs = data_dd_qs %>%
            rowwise() %>%
            mutate(!!outputcol_q1:=ifelse(year==0,1,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*1)) %>%
            mutate(!!outputcol_q2:=ifelse(year==0,1-(1-!!inputcol_q4)/3*1,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*2)) %>%
            mutate(!!outputcol_q3:=ifelse(year==0,1-(1-!!inputcol_q4)/3*2,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*3)) %>%
            mutate(!!outputcol_q4:= !!inputcol_q4)
          
        }
      }
      
      # ...Calculating Outsourcing percentages ------------------------------------
      data_out <- select(data_import,unique_identifier,grep("out_",cols_all))
      
      incProgress(0.1,"Calculating outsourcing impact")
      
      # Turning it into quarters
      data_out_qs <- data_out
      
      for(x in c("min","mid","max")){
        for(y in c("Y1","Y2","Y3")){
          for(z in c("q1","q2","q3","q4")){
            year = as.numeric(substr(y,nchar(y),nchar(y)))
            quarter = as.numeric(substr(z,nchar(z),nchar(z)))
            outcol = as.name(paste("out",x,y,sep="_"))
            
            input_considered = as.name(paste("out_considered",x,sep="_"))
            input_yesno = as.name("out_yesno")
            input_yearcol = as.name(paste("out_year",x,sep="_"))
            input_quartercol = as.name(paste("out_quarter",x,sep="_"))
            
            outputcol = paste("out",x,y,z,sep="_")
            
            data_out_qs = data_out_qs %>%
              rowwise() %>%
              mutate(!!outputcol:=ifelse(!!input_yesno == 0 | !!input_considered==0,1,
                                         ifelse(year<!!input_yearcol,1,
                                                ifelse(year>!!input_yearcol,0,
                                                       ifelse(quarter<!!input_quartercol,1,0)
                                                )
                                         )
              ))
          }
        }
      }
      
      incProgress(0.1,"Calculating cumulative impact")
      
      # ...Multiplying automation, productivity, demand and outsourcing together --------
      
      data_aut_pro_dd_out <- left_join(data_aut_qs,data_pro_cumulative_qs)
      data_aut_pro_dd_out <- left_join(data_aut_pro_dd_out,data_dd_qs)
      data_aut_pro_dd_out <- left_join(data_aut_pro_dd_out,data_out_qs)
      colnames <- colnames(data_aut_pro_dd_out)
      colnames(data_aut_pro_dd_out) <- gsub("_cumu","",colnames)
      
      for(x in c("min","mid","max")){
        for(y in c("Y1","Y2","Y3")){
          for(z in c("q1","q2","q3","q4")) {
            input_aut = as.name(paste("aut",x,y,z,sep="_"))
            input_pro = as.name(paste("pro",x,y,z,sep="_"))
            input_dd = as.name(paste("dd",x,y,z,sep="_"))
            input_out = as.name(paste("out",x,y,z,sep="_"))
            
            outputcol = paste("finalmult",x,y,z,sep="_")
            
            data_aut_pro_dd_out <- data_aut_pro_dd_out %>%
              rowwise() %>%
              mutate(!!outputcol := !!input_aut * !!input_pro * !!input_dd *!!input_out)
          }
        }
      }
      
      # ...Pulling in baseline numbers ---------------------------------------------
      
      incProgress(0.1,"Calculating total headcount impact")
      
      # Bringing out baseline columns
      data_baseline <- select(data_import,unique_identifier,grep("baseline",cols_all))
      
      data_baseline <- data_baseline %>% 
        select(-baseline_mid) %>%
        mutate("baseline_min"=ceiling(baseline_top-(baseline_top-baseline_bottom)*baselinepos_min)) %>%
        mutate("baseline_mid"=ceiling(baseline_top-(baseline_top-baseline_bottom)*baselinepos_mid)) %>%
        mutate("baseline_max"=ceiling(baseline_top-(baseline_top-baseline_bottom)*baselinepos_max))
      
      #...Turning supply into headcount impacts -----------------------------------
      
      final_supply_cols <- colnames(select(data_supply_cumulative_qs,-unique_identifier))
      
      final_supply <- left_join(select(data_hc,unique_identifier,hc_chosen),data_supply_cumulative_qs)
      
      for(x in final_supply_cols){
        inputcol = as.name(x)
        
        scenario = str_split(x,"_")[[1]][2]
        inputheadcount = as.name("hc_chosen")
        
        final_supply <- final_supply %>%
          mutate(!!x := ceiling(!!inputheadcount * !!inputcol))
      }
      
      
      # ...Turning multipliers into headcount impacts ------------------------------
      
      multcols <- colnames(data_aut_pro_dd_out)
      finalcols <- colnames(select(data_aut_pro_dd_out,unique_identifier,grep("_q1",multcols),grep("_q2",multcols),grep("_q3",multcols),grep("_q4",multcols)))
      data_aut_pro_dd_out <- data_aut_pro_dd_out %>% select(finalcols)
      
      data_baseline_aut_pro_dd_out <- left_join(data_baseline,data_aut_pro_dd_out)
      multiplier_cols <- colnames(select(data_aut_pro_dd_out,-unique_identifier))
    
      rvalues$final_multipliers <- data_baseline_aut_pro_dd_out
      
      # Calculating final headcount values
      final_hc <- data_baseline_aut_pro_dd_out
      for(x in multiplier_cols){
        inputcol = as.name(x)
        
        scenario = str_split(x,"_")[[1]][2]
        inputbaseline = as.name(paste("baseline",scenario,sep="_"))
        
        final_hc <- final_hc %>%
          mutate(!!x := ceiling(!!inputbaseline * !!inputcol))
      }
      
      final_hc <- left_join(data_sectioninfo,final_hc,by="unique_identifier")
      
      #Adding in supply
      final_hc <- left_join(final_hc,final_supply,by="unique_identifier")
      incProgress(0.1,"Calculating cost impact")
      
      # Calculating final cost values
      final_cost <- left_join(data_sectioncost,final_hc,by="unique_identifier")
      final_cost <- final_cost %>%
        select(-hc_chosen,-baseline_bottom,-baseline_top,-baselinepos_min,-baselinepos_mid,-baselinepos_max)

      costcols <- colnames(select(final_cost,-unique_identifier,-fte_cost_avg,-l0,-l1,-l2,-l3,-l4,-level,-segment))
      for(x in costcols){
        xname <- as.name(x)
        print(x)
        final_cost <- final_cost %>%
          rowwise() %>%
          mutate(!!x:=!!as.name(x)*fte_cost_avg)
      }
      
      rvalues$data_baseline_aut_pro_dd_out <- data_baseline_aut_pro_dd_out
      rvalues$data_baseline <- data_baseline
      rvalues$data_hc <- data_hc
      rvalues$final_hc <- final_hc
      rvalues$final_cost <- final_cost
      rvalues$final_supply <- final_supply
      
      incProgress(1,"Calculations complete")
  })
})


# Filtering based on exec selection ---------------------------------------

  exec_table_output <- reactive({
    if(input$headcount_or_cost == "Headcount"){
      table <- rvalues$final_hc
    } else {
      table <- rvalues$final_cost
    }

    table <- table %>% 
      filter(l0 %in% input$l0_list_exec) %>%
      filter(l1 %in% input$l1_list_exec) %>%
      filter(l2 %in% input$l2_list_exec) %>%
      filter(segment %in% input$segment_list_exec)
    rvalues$exec_table_output <- table
  })
  
# ...Calculating changes for line --------------------------------------------

final_line_table <- reactive({
  final_hc = rvalues$exec_table_output
  # Taking the "cumu" out of supply columns so str split later works
  colnames(final_hc) <- gsub("_cumu","",colnames(final_hc))
  final_hc2 <- final_hc %>% select(grep("finalmult",colnames(final_hc)),grep("supply",colnames(final_hc))) %>%
    group_by() %>%
    summarise_all(funs(sum),na.rm=TRUE) %>%
    gather("Key","Value")
  final_hc2 <- final_hc2 %>%
    mutate("supplyordemand" = sapply(str_split(Key,"_"),"[[",1)) %>%
    mutate("scenario" = sapply(str_split(Key,"_"),"[[",2)) %>%
    mutate("year" = as.numeric(substr(sapply(str_split(Key,"_"),"[[",3),2,2))) %>%
    mutate("quarter" = as.numeric(substr(sapply(str_split(Key,"_"),"[[",4),2,2))) %>%
    mutate("scenariofull" = paste(supplyordemand,scenario,sep="_")) %>%
    select(-Key) %>%
    mutate("total_quarter" = (year*4)+quarter-4) %>%
    mutate("Value"=ceiling(Value))
  return(final_hc2)
})  
  

# ...Line plot ---------------------------------------------------------------
  output$line <- renderPlot({
    
    if(rvalues$generated == 0){
      text = paste("\n   Please generate data in the model setup tab \n",
                   "          to generate these graphs \n")
      blankchart <- ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = text) + 
        theme_bw() +
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              axis.ticks=element_blank(),
              axis.text = element_blank(),
              axis.title=element_blank(),
              panel.grid = element_blank(),
              panel.border=element_blank())
      return(blankchart)
      
    }
    
    final_hc2 <- final_line_table()
    
    linegraph <- ggplot(final_hc2,aes(x=total_quarter,y=Value,colour=scenariofull)) +
      geom_line(size=1)+
      CustomTheme +
      ggtitle("Headcount Forecast")+
      xlab("Year")+
      ylab("No. of Employees")+
      theme(legend.position = "bottom")+
      #scale_colour_brewer(palette="Blues",direction = -1)
      scale_colour_manual(labels=c("finalmult_min"="Demand - Conservative","finalmult_mid"="Demand - Realistic","finalmult_max"="Demand - Optimistic","supply_min" = "Supply - Conservative","supply_mid"="Supply - Realistic","supply_min"="Supply - Optimistic"),values=c("finalmult_min" = "steelblue","finalmult_mid" = "steelblue3","finalmult_max" = "steelblue1","supply_min" = "gray1","supply_mid"="gray25","supply_min"="gray30"))+
      coord_cartesian(ylim=c(0,max(final_hc2$Value)))+
      scale_x_continuous(breaks=1:12,labels = paste0(c("Y1 Q1","Y1 Q2","Y1 Q3","Y1 Q4","Y2 Q1","Y2 Q2","Y2 Q3","Y2 Q4","Y3 Q1","Y3 Q2","Y3 Q3","Y3 Q4"))) +
      scale_y_continuous(expand=c(0,0),limits=c(0,max(final_hc2$Value)),labels=comma)+
      geom_label_repel(aes(label=ifelse(Value==0,"",Value)),direction=c("y"),size=3,segment.alpha=1,label.r=0.2,box.padding=0.1,label.padding=0.05,alpha=0.8,seed=5,label.size=NA)+
      geom_label_repel(aes(label=ifelse(Value==0,"",Value)),direction=c("y"),size=3,segment.alpha=1,label.r=0.2,box.padding=0.1,label.padding=0.05,fill=NA,alpha=1,seed = 5,label.size=NA)
    
    linegraph
  })
  

# ...Calculating changes for bar ---------------------------------------------

  # Calculating figures for bar charts
  final_barchart_table <- reactive({
    final_hc = rvalues$exec_table_output
    final_hc3 = rvalues$exec_table_output
    final_hc3 <- final_hc %>% select(grep("finalmult",colnames(final_hc))) %>%
      group_by() %>%
      summarise_all(funs(sum),na.rm=TRUE) %>%
      gather("Key","Value")
    final_hc3 <- final_hc3 %>%
      mutate("scenario" = sapply(str_split(Key,"_"),"[[",2)) %>%
      mutate("year" = as.numeric(substr(sapply(str_split(Key,"_"),"[[",3),2,2))) %>%
      mutate("quarter" = as.numeric(substr(sapply(str_split(Key,"_"),"[[",4),2,2))) %>%
      select(-Key) %>%
      mutate("Value"=ceiling(Value)) %>%
      filter(quarter==4) %>%
      select(-quarter)
  })
  

# ...Bar plot -------------------------------------------------------------

  output$barchart <- renderPlot({
    if(rvalues$generated == 0){
      return(NULL)
    }
    # Creating bar charts
    data = final_barchart_table()
    bargraph <- ggplot(data,aes(x=year,y=Value))+
      geom_bar(aes(fill=scenario),stat="identity",position="dodge")+
      CustomTheme + 
      theme(legend.position = "bottom") +
      scale_fill_manual("Key",labels=c("min"="Conservative","mid"="Realistic","max"="Optimistic"),values=c("min" = "steelblue","mid" = "steelblue3","max" = "steelblue1"))+
      labs(x="Year",y="Headcount")
    bargraph
  })
  

  # Calculating changes for supply v demand bar -----------------------------
  
  # Calculating figures for supplydemandbar charts
  final_supplydemandbar_table <- reactive({
    
    final_hc = rvalues$exec_table_output
    scenario = ifelse(input$exec_scenario=="Conservative","min",ifelse(input$exec_scenario=="Realistic","mid","max"))
    colnames(final_hc) = gsub("cumu_","",colnames(final_hc))
    final_hc <- final_hc %>%
      select(grep("finalmult",colnames(final_hc)),grep("supply",colnames(final_hc))) %>%
      group_by() %>%
      summarise_all(funs(sum),na.rm=TRUE) %>%
      gather("Key","Value")
    final_hc <- final_hc %>%
      mutate("supplyordemand" = sapply(str_split(Key,"_"),"[[",1)) %>%
      mutate("scenario" = sapply(str_split(Key,"_"),"[[",2)) %>%
      mutate("year" = as.numeric(substr(sapply(str_split(Key,"_"),"[[",3),2,2))) %>%
      mutate("quarter" = as.numeric(substr(sapply(str_split(Key,"_"),"[[",4),2,2)))
    final_hc <- final_hc %>%
      select(-Key) %>%
      mutate("Value"=ceiling(Value)) %>%
      filter(quarter==4 & scenario==!!scenario) %>%
      select(-quarter) %>%
      filter(year!=0)
    return(final_hc)
  })
  
  
  # ...Supply Demand Bar plot -------------------------------------------------------------
  
  output$supplydemandbarchart <- renderPlot({
    if(rvalues$generated == 0){
      return(NULL)
    }
    # Creating bar charts
    data = final_supplydemandbar_table()
    bargraph <- ggplot(data,aes(x=year,y=Value))+
      geom_bar(aes(fill=supplyordemand),stat="identity",position="dodge")+
      CustomTheme + 
      theme(legend.position = "bottom") +
      scale_fill_manual("Key",labels=c("supply"="Supply","finalmult"="Demand"),values=c("supply" = "steelblue","finalmult" = "steelblue1"))+
      labs(x="Year",y="Headcount")
    bargraph
  })
  

# Calculating changes for waterfall charts --------------------------------
  final_waterfall_table <- reactive({
    final_headcount_table()
    table <- rvalues$final_hc
    final_hc_change <- rvalues$data_baseline_aut_pro_dd_out
    data_baseline <-  rvalues$data_baseline
    data_hc <- rvalues$data_hc
    final_hc <- rvalues$final_hc
    
    # Calculating headcount changes
    for(x in multiplier_cols){
      inputcol = as.name(x)
      outputcol = paste0(x,"_change")
      
      scenario = str_split(x,"_")[[1]][2]
      inputbaseline = as.name(paste("baseline",scenario,sep="_"))
      
      final_hc_change <- final_hc_change %>%
        mutate(!!outputcol := !!inputbaseline * (!!inputcol-1)) %>%
        select(-!!inputcol)
    }
    
    # ...Calculating additions and reductions ------------------------------------
    
    data_baseline_adred <- left_join(data_baseline,data_hc)
    for(x in c("min","mid","max")){
      
      
      input_baseline = as.name(paste("baseline",x,sep="_"))
      input_hc = as.name("hc_chosen")
      output_additions = paste("additions",x,sep="_")
      output_reductions = paste("reductions",x,sep="_")
      
      data_baseline_adred <- data_baseline_adred %>%
        rowwise() %>%
        mutate(!!output_additions := ifelse(!!input_baseline>!!input_hc,!!input_baseline-!!input_hc,0)) %>%
        mutate(!!output_reductions := ifelse(!!input_baseline<!!input_hc,!!input_hc-!!input_baseline,0))
      
    }
    
    # ...Calculating figures for waterfall charts --------------------------------
    data_waterfall_baseline <- data_baseline_adred %>% select(unique_identifier,hc_chosen,baseline_min,baseline_mid,baseline_max,additions_min,additions_mid,additions_max,reductions_min,reductions_mid,reductions_max)
    data_waterfall_finals <- final_hc %>% select(unique_identifier,grep("finalmult",colnames(final_hc)))
    
    data_waterfall_finals <- data_waterfall_finals %>% select(unique_identifier,grep("q4",colnames(data_waterfall_finals)))
    
    data_waterfall_change <- final_hc_change %>% select(unique_identifier,grep("Y3",colnames(final_hc_change)))
    data_waterfall_change <- data_waterfall_change %>% select(unique_identifier,grep("q4",colnames(data_waterfall_change)))
    
    data_final_all <- left_join(data_waterfall_baseline,data_waterfall_finals,by="unique_identifier")
    data_final_all <- left_join(data_final_all,data_waterfall_change,by="unique_identifier")
    
    data_final_all_mid <- select(data_final_all,unique_identifier,hc_chosen,grep("mid",colnames(data_final_all)))
    data_final_all_mid_sum <- data_final_all_mid %>% group_by() %>% summarise_all(funs(sum),na.rm=TRUE)
    
    print("Data final all mid sum:")
    print(data_final_all_mid_sum)
    return(data_final_all_mid_sum)
})
  
  observeEvent(input$load_exec_data,{final_waterfall_table()})
  
  # Waterfall plot ----------------------------------------------------------

output$waterfall <- renderPlot({
  data = final_waterfall_table()
  data_waterfall_baseline <- data %>% select(unique_identifier,hc_chosen,baseline_mid,additions_mid,reductions_mid)
  data_waterfall_finals <- final_hc %>% select(unique_identifier,grep("finalmult",colnames(final_hc)))
  
  data_waterfall_finals <- data_waterfall_finals %>% select(unique_identifier,grep("q4",colnames(data_waterfall_finals)))
  
  data_waterfall_change <- final_hc_change %>% select(unique_identifier,grep("Y3",colnames(final_hc_change)))
  data_waterfall_change <- data_waterfall_change %>% select(unique_identifier,grep("q4",colnames(data_waterfall_change)))
  
  data_final_all <- left_join(data_waterfall_baseline,data_waterfall_finals,by="unique_identifier")
  data_final_all <- left_join(data_final_all,data_waterfall_change,by="unique_identifier")
  
  data_final_all_mid <- select(data_final_all,unique_identifier,hc_chosen,grep("mid",colnames(data_final_all)))
  data_final_all_mid_sum <- data_final_all_mid %>% group_by() %>% summarise_all(funs(sum),na.rm=TRUE)
  write_csv(data_final_all_mid_sum,"Test waterfall output.csv")
  
  waterfall_table <- data_final_all_mid_sum %>%
    select(hc_chosen,additions_mid,reductions_mid,
           aut_mid_Y3_q4_change,pro_mid_Y3_q4_change,dd_mid_Y3_q4_change,out_mid_Y3_q4_change) %>%
    mutate("reductions_mid" = -reductions_mid) %>%
    mutate("final"=dd_mid_Y3_q4_change-out_mid_Y3_q4_change) %>%
    gather(key,value)
  waterfall_table$id <- seq_along(waterfall_table$value)
  waterfall_table$type <- ifelse(waterfall_table$value > 0,"inc","dec")
  waterfall_table$key <- as.factor(waterfall_table$key)
  waterfall_table[waterfall_table$key %in% c("hc_chosen","baseline_mid","finalmult_mid_Y1_q4","finalmult_mid_Y2_q4","finalmult_mid_Y3_q4","final"),"type"] <- "total"
  
  
  waterfall_table$end <- cumsum(waterfall_table$value)
  waterfall_table$end <- c(head(waterfall_table$end,-1),0)
  
  waterfall_table$start <- c(0,head(waterfall_table$end,-1))
  waterfall_table <- waterfall_table[,c(3,1,4,5,6,2)]
  
  ggplot(waterfall_table,aes(desc,fill=type)) + 
    geom_rect(aes(x=key,xmin=id - 0.45,xmax = id+ 0.45,ymin=end,ymax=start))

})  
  

# Observing event to trigger calculation of final table -------------------
observeEvent(input$generateall,{
  rvalues$generated <- 1
  final_headcount_table()
  print("Calculation complete")
})
  

# Observe event to prevent clicking second tab if no data generated -------

observeEvent(input$tabs,{
  if(input$tabs=="tab_executive" & rvalues$generated == 0){
    print("No data generated")
    #updateTabsetPanel(session="session","tabs","tab_setup")
    # showNotification(ui, action = NULL, duration = 5, closeButton = TRUE,
    #                  id = NULL, type = "error",
    #                  session = "session")
    # 
    # removeNotification(id = NULL, session = "session")
  }
})

# Observing event to trigger charts ------------------------------
  
  # Would be good to put trigger on segment_list_exec, but this seems to run immediately and thus break it
  observeEvent(
    {input$l2_list_exec
      input$segment_list_exec
      input$headcount_or_cost
      input$tabs
      },
      {
      if(rvalues$generated!=0){
        table_exec <- exec_table_output()
        table_final_line <- final_line_table()
        table_final_barchart <- final_barchart_table()
        final_supplydemandbar_table <- final_supplydemandbar_table()
        
        print("Final exec table:")
        print(table_exec)
        
        print("Final line table:")
        print(table_final_line)
        
        print("Final full table")
        print(rvalues$final_data)
        
        print("Final data")
        print(rvalues$final_hc)
        
        print("Final hc")
        print(rvalues$final_hc)
        
        print("Final cost")
        print(rvalues$final_cost)
        
        print("")
      }
  #final_waterfall_table()
})
  
  
# Generate csv test -------------------------------------------------------
  observeEvent(input$generate_csv,{
    write_csv(rvalues$final_data,"Final Data Output Values.csv")
  })
  

# DEEP DIVE ---------------------------------------------------------------

  # Section selection ----------------------------------------------------
  
  output$deep_l0_list <- renderUI ({
    selectInput(inputId = "deep_chosenl0",label = "Unit",
                choices = c("All",output_l0_list),
                selected="All")
  })
  
  output$deep_l1_list <- renderUI ({
    if (is.null(input$deep_chosenl0)) {return()
    } else {
      selectInput(inputId = "deep_chosenl1",label = "Sector",
                  choices = c("All",data_sectioninfo$l1[which(
                    data_sectioninfo$l0 == input$deep_chosenl0
                  )]),
                  selected="All")
    }
  })
  
  output$deep_l2_list <- renderUI ({
    if (is.null(input$deep_chosenl1)) {return()
    } else {
      selectInput(inputId = "deep_chosenl2",label = "General Directorate",
                  choices = c("All",data_sectioninfo$l2[which(
                    data_sectioninfo$l0 == input$deep_chosenl0 &
                      data_sectioninfo$l1 == input$deep_chosenl1
                  )]),
                  selected="All")
    }
  })
  
  output$deep_l3_list <- renderUI ({
    if (is.null(input$deep_chosenl2)) {return()
    } else {
      selectInput(inputId = "deep_chosenl3",label = "Department",
                  choices = c("All",data_sectioninfo$l3[which(
                    data_sectioninfo$l0 == input$deep_chosenl0 &
                      data_sectioninfo$l1 == input$deep_chosenl1 &
                      data_sectioninfo$l2 == input$deep_chosenl2
                  )]),
                  selected="All")
    }
  })
  
  output$deep_l4_list <- renderUI ({
    if (is.null(input$deep_chosenl3)) {return()
    } else {
      selectInput(inputId = "deep_chosenl4",label = "Section",
                  choices = c("All",data_sectioninfo$l4[which(
                    data_sectioninfo$l1 == input$deep_chosenl1 & 
                      data_sectioninfo$l2 == input$deep_chosenl2 & 
                      data_sectioninfo$l3 == input$deep_chosenl3
                  )]),
                  selected="All")
    }
  })
  

# Filtered deep dive list of sections -------------------------------------

  deep_sections_list <- reactive({
    output <- data_sectioninfo %>%
      filter(segment %in% input$segment_list_deep)
    #"Single Section Data","Aggregated Data")
    single_or_aggregated <- input$single_agg_choice
    if(single_or_aggregated=="Single Section Data"){
      output <- output %>%
        filter(l0==input$deep_chosenl0 &
                 l1==input$deep_chosenl1 &
                 l2==input$deep_chosenl2 &
                 l3==input$deep_chosenl3 &
                 l4==input$deep_chosenl4)
    } else {
      if(input$deep_chosenl4=="All" & input$deep_chosenl3=="All" & input$deep_chosenl2=="All" & input$deep_chosenl1=="All" & input$deep_chosenl0=="All"){
        output <- output
      } else {
        if(input$deep_chosenl4=="All" & input$deep_chosenl3=="All" & input$deep_chosenl2=="All" & input$deep_chosenl1=="All" & input$deep_chosenl0!="All"){
          output <- output %>%
            filter(l0==input$deep_chosenl0)
        } else {
          if(input$deep_chosenl4=="All" & input$deep_chosenl3=="All" & input$deep_chosenl2=="All" & input$deep_chosenl1!="All" & input$deep_chosenl0!="All"){
            output <- output %>%
              filter(l0==input$deep_chosenl0 & l1==input$deep_chosenl1)
          } else {
            if(input$deep_chosenl4=="All" & input$deep_chosenl3=="All" & input$deep_chosenl2!="All" & input$deep_chosenl1!="All" & input$deep_chosenl0!="All"){
              output <- output %>%
                filter(l0==input$deep_chosenl0 & l1==input$deep_chosenl1 & l2==input$deep_chosenl2)
            } else {
              if(input$deep_chosenl4=="All" & input$deep_chosenl3!="All" & input$deep_chosenl2!="All" & input$deep_chosenl1!="All" & input$deep_chosenl0!="All"){
                output <- output %>%
                  filter(l0==input$deep_chosenl0 & l1==input$deep_chosenl1 & l2==input$deep_chosenl2 & l3==input$deep_chosenl3)
              } else {
                if(input$deep_chosenl4!="All" & input$deep_chosenl3!="All" & input$deep_chosenl2!="All" & input$deep_chosenl1!="All" & input$deep_chosenl0!="All"){
                  output <- output %>%
                    filter(l0==input$deep_chosenl0 & l1==input$deep_chosenl1 & l2==input$deep_chosenl2 & l3==input$deep_chosenl3 & l4==input$deep_chosenl4)
                }
              }
            }
          }
        }
      }
    }
    rvalues$deep_sections_list <- output
  })
  
# Headcount graph ---------------------------------------------------------

    # Headcount table calculations
  deep_hc_table <- reactive({

    table <- rvalues$deep_sections_list
    table <- left_join(table,select(data_hc,unique_identifier,fte,contractor),by="unique_identifier")
    table <- table %>%
      select(fte,contractor) %>%
      group_by() %>%
      summarise_all(funs(sum),na.rm=TRUE) %>%
      gather("Key","Value")
    print(table)
    return(table)
  })
  
  # Headcount graph creation
  output$deep_hc_graph <- renderPlot({
    if(rvalues$deep_generated == 0){
      return(NULL)
    }
    data=deep_hc_table()
    bargraph = ggplot(data,aes(x=1,y=Value,fill=Key))+
      geom_bar(stat="identity",aes(fill=Key)) +
      coord_flip()+
      CustomTheme + 
      theme(legend.position = "bottom") +
      scale_fill_manual("Key",labels=c("fte"="FTE","contractor"="Contractor"),values=c("fte" = "steelblue","contractor" = "steelblue1")) +
      #scale_x_discrete(labels=c("fte"="FTE","contractor"="Contractor"))+
      theme(
        axis.ticks=element_blank(),
        axis.text = element_blank(),
        axis.title=element_blank(),
        panel.grid = element_blank(),
        panel.border=element_blank()
      )+
      geom_text(colour="white",aes(fontface=2,label=Value),position="stack",hjust=1)
    bargraph
  })
  

# Segmentation Graph ------------------------------------------------------
  # Segmentation table calculations
  deep_segment_table <- reactive({

    table <- rvalues$deep_sections_list
    table <- left_join(table,select(data_hc,unique_identifier,hc_chosen),by="unique_identifier")
    table <- table %>%
      select(segment,hc_chosen) %>%
      group_by(segment) %>%
      summarise(hc_chosen=sum(hc_chosen,na.rm=TRUE))
    print(table)
    return(table)
  })
  
  # Segment graph creation
  output$deep_segment_graph <- renderPlot({
    if(rvalues$deep_generated == 0){
      return(NULL)
    }
    data=deep_segment_table()
    bargraph = ggplot(data,aes(x=segment,y=hc_chosen))+
      geom_bar(stat="identity",fill="steelblue1") +
      coord_flip()+
      CustomTheme + 
      theme(legend.position = "bottom") +
      scale_fill_manual("segment",labels=c("Core"="Core","Specialist"="Specialist","Critical"="Critical","Support"="Support","management"="Management"),values=c("Core"="steelblue1","Specialist"="steelblue2","Critical"="steelblue3","Support"="steelblue4","Management"="steelblue")) +
      scale_x_discrete(labels=c("Core"="Core","Specialist"="Specialist","Critical"="Critical","Support"="Support","Management"="Management"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      geom_text(colour="white",aes(fontface=2,label=hc_chosen),hjust=1)+
      labs(x="Headcount",y="Segment")
    bargraph
  })
  
# Education Graph ------------------------------------------------------
  # Segmentation table calculations
  deep_education_table <- reactive({

    table <- rvalues$deep_sections_list
    table <- left_join(table,select(data_degree,unique_identifier,degree_yes,degree_no),by="unique_identifier")
    table <- table %>%
      select(degree_yes,degree_no) %>%
      group_by() %>%
      summarise(degree_yes=sum(degree_yes,na.rm=TRUE),degree_no=sum(degree_no,na.rm=TRUE)) %>%
      gather(Key,Value) %>%
      mutate("valuepercentage" = Value / sum(Value,na.rm=TRUE)) %>%
      select(-Value) %>%
      mutate("label"=paste0(round(valuepercentage*100,0),"%"))
    
    print(table)
    return(table)
  })
  
  # Education graph creation
  output$deep_education_graph <- renderPlot({
    if(rvalues$deep_generated == 0){
      return(NULL)
    }
    data=deep_education_table()
    print(data)
    doughnutgraph = ggplot(data,aes(x=2,y=valuepercentage,fill=Key,label=label))+
      geom_bar(stat="identity",width=1) +
      coord_polar(theta = "y")+
      geom_text(data=data,colour="white",position=position_stack(vjust=0.5),aes(fontface=2))+
      CustomTheme + 
      theme(legend.position = "bottom") +
      scale_fill_manual("Key",labels=c("degree_yes"="Degree","degree_no"="No Degree"),values=c("degree_yes"="steelblue","degree_no"="steelblue2"))+
      theme(
        axis.ticks=element_blank(),
        axis.text = element_blank(),
        axis.title=element_blank(),
        panel.grid = element_blank(),
        panel.border=element_blank()
      )
    doughnutgraph
  })
  
  # Age Graph ------------------------------------------------------
  # Age table calculations
  deep_age_table <- reactive({
    table <- rvalues$deep_sections_list
    table <- left_join(table,data_age,by="unique_identifier")
    table <- table %>%
      select(`age_20-24`,`age_25-29`,`age_30-34`,`age_35-39`,`age_40-44`,`age_45-49`,`age_50-54`,`age_55-59`,`age_60+`) %>%
      group_by() %>%
      summarise_all(funs(sum),na.rm=TRUE) %>%
      gather(Key,Value) %>%
      mutate("valuepercentage" = Value / sum(Value,na.rm=TRUE)) %>%
      select(-Value) %>%
      mutate("label"=paste0(round(valuepercentage*100,0),"%"))
    print(table)
    return(table)
  })
  
  # Age graph creation
  output$deep_age_graph <- renderPlot({
    if(rvalues$deep_generated == 0){
      return(NULL)
    }
    data=deep_age_table()
    bargraph = ggplot(data,aes(x=Key,y=valuepercentage))+
      geom_bar(stat="identity",fill="steelblue1") +
      CustomTheme + 
      theme(legend.position = "bottom",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y= element_blank()
      )+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      labs(x="Age") +
      scale_x_discrete(labels=c(`age_20-24`="20-24",`age_25-29`="25-29",`age_30-34`="30-34",`age_35-39`="35-39",`age_40-44`="40-44",`age_45-49`="45-49",`age_50-54`="50-54",`age_55-59`="55-59",`age_60+`="60+"))+
      geom_text(colour="white",aes(fontface=2,label=label),vjust=1.25)
    bargraph
  })
  
  # Tenure Graph ------------------------------------------------------
  # Tenure table calculations
  deep_tenure_table <- reactive({
    table <- rvalues$deep_sections_list
    table <- left_join(table,data_tenure,by="unique_identifier")
    table <- table %>%
      select(`tenure_0-4`,`tenure_5-9`,`tenure_10-14`,`tenure_15-19`,`tenure_20-24`,`tenure_25+`) %>%
      group_by() %>%
      summarise_all(funs(sum),na.rm=TRUE) %>%
      gather(Key,Value) %>%
      mutate("valuepercentage" = Value / sum(Value,na.rm=TRUE)) %>%
      select(-Value) %>%
      mutate("label"=paste0(round(valuepercentage*100,0),"%"))
    print(table)
    return(table)
  })
  
  # Tenure graph creation
  output$deep_tenure_graph <- renderPlot({
    if(rvalues$deep_generated == 0){
      return(NULL)
    }
    data=deep_tenure_table()
    bargraph = ggplot(data,aes(x=Key,y=valuepercentage))+
      geom_bar(stat="identity",fill="steelblue1") +
      CustomTheme + 
      theme(legend.position = "bottom",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y= element_blank()
            )+
      labs(x="Tenure") +
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      scale_x_discrete(labels=c(`tenure_0-4`="0-4",`tenure_5-9`="5-9",`tenure_10-14`="10-14",`tenure_15-19`="15-19",`tenure_20-24`="20-24",`tenure_25+`="25+"))+
      geom_text(colour="white",aes(fontface=2,label=label),vjust=1.25)

    bargraph
  })
  
  
# Forcing graph refresh ---------------------------------------------------
observeEvent(input$deep_generate,{
  rvalues$deep_generated = 1
  deep_sections_list()
  deep_hc_table()
  deep_segment_table()
  deep_education_table()
  deep_age_table()
  deep_tenure_table()
  deep_final_table()
  deep_final_table_summarised()
  deep_final_table_overwritten()
  })
  

# Line Chart --------------------------------------------------------------
  # 
  # rvalues$final_data
  # 
  # rvalues$final_hc
  # 
  # rvalues$final_cost
  

# Calculating new multipliers ---------------------------------------------

  deep_final_table <- reactive({
    input = rvalues$final_multipliers
    rows = deep_sections_list() %>%
      select(unique_identifier)
    outputtable = left_join(rows,input,by="unique_identifier")
    print(outputtable)
    rvalues$deep_final_table <- outputtable
    return(outputtable)
  })
  
  output$deep_final_table <- renderDataTable({rvalues$deep_final_table},options=list(scrollX=TRUE))
  
  # Work out weighted averages
  deep_final_table_summarised <- reactive({
    fulltable = rvalues$deep_final_table %>%
      select(-unique_identifier,-baseline_bottom,-baseline_top)
    
    totalhc <- sum(fulltable$baseline_mid)
    fulltable$baseline_mid2 <- fulltable$baseline_mid
    cols_fulltable <- colnames(select(fulltable,-baseline_mid2))
    print(cols_fulltable)
    
    fulltable_sum <- fulltable
    
    for(x in cols_fulltable){
      inputcol = as.name(x)
      outputcol = x
      
      fulltable_sum <-  fulltable_sum %>% 
        rowwise() %>%
        mutate(!!outputcol := !!inputcol / totalhc * baseline_mid2)
    }
    
    fulltable_sum <- fulltable_sum %>%
      group_by() %>%
      summarise_all(funs(sum),na.rm=TRUE)
    rvalues$deep_final_table_summarised = fulltable_sum
    print(fulltable)
  }) 
  
  
  output$deep_final_table_summarised <- renderDataTable({rvalues$deep_final_table_summarised},options=list(scrollX=TRUE))
  
  # Overwrite original table with new weighted averages
  
  deep_final_table_overwritten <- reactive({
    
    fulltable = rvalues$deep_final_table
    sumtable = rvalues$deep_final_table_summarised
    outtable = fulltable
    
    cols_sumtable = colnames(sumtable)
    for(x in cols_sumtable){
      outputcol = x
      inputnum = as.numeric(sumtable[[as.name(x)]])
      print(inputnum)
      outtable <- outtable %>%
        mutate(!!outputcol := inputnum)
    }
    rvalues$deep_final_table_overwritten <- outtable
  })
  
  output$deep_final_table_overwritten <- renderDataTable({rvalues$deep_final_table_overwritten},options=list(scrollX=TRUE))
  

# Pull new weighted averages through to sliders ---------------------------

  observeEvent(input$deep_generate,{
    scenario = ifelse(input$deep_scenario=="Conservative","min",ifelse(input$deep_scenario=="Realistic","mid","max"))
    
    table <- rvalues$deep_final_table_summarised
    write_csv(table,"testtablegather")
    cols_final_table <- colnames(table)
    
    table_gathered <- table %>%
      gather(key,value) %>%
      filter(grepl(scenario,key))
    
    print(table_gathered)
    print(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("Y1",key) & grepl("q4",key)))[2])
    
    value_auty1 <- round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)
    value_auty2 <- round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)
    value_auty3 <- round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)
    
    print(paste0("Automation year 3 value: ",value_auty3))
    
    updateSliderInput(session=session,inputId="deep_slider_aut_Y1",label=NULL,value=value_auty1)
    updateSliderInput(session=session,inputId="deep_slider_aut_Y2",label=NULL,value=value_auty2)
    updateSliderInput(session=session,inputId="deep_slider_aut_Y3",label=NULL,value=value_auty3)
    
    value_proy1 <- round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)
    value_proy2 <- round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)
    value_proy3 <- round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)
    
    updateSliderInput(session=session,inputId="deep_slider_pro_Y1",label=NULL,value=value_proy1,step=1)
    updateSliderInput(session=session,inputId="deep_slider_pro_Y2",label=NULL,value=value_proy2)
    updateSliderInput(session=session,inputId="deep_slider_pro_Y3",label=NULL,value=value_proy3)
    
    # column(4,sliderInput("deep_slider_pro_y1","Year 1 Training Impact:",
    #                      min=0,max=100,value= 50,step=1,post=" %")),
    
    value_ddy1 <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)
    value_ddy2 <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)
    value_ddy3 <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)
    
    updateSliderInput(session=session,inputId="deep_slider_dd_Y1",label=NULL,value=value_ddy1)
    updateSliderInput(session=session,inputId="deep_slider_dd_Y2",label=NULL,value=value_ddy2)
    updateSliderInput(session=session,inputId="deep_slider_dd_Y3",label=NULL,value=value_ddy3)
    
    print(value_ddy3)
    

    # updateSliderInput("deep_slider_dd_Y1",value=table$dd)
    
  })
  
# "deep_slider_dd_Y1"
# "deep_slider_dd_Y2"
# "deep_slider_dd_Y3"
# "deep_slider_baseline"
# "deep_slider_hire_Y1"
# "deep_slider_exit_Y1"
# "deep_slider_hire_Y2"
# "deep_slider_exit_Y2"
# "deep_slider_hire_Y3"
# "deep_slider_exit_Y3"
# "deep_slider_aut_y1"
# "deep_slider_aut_y2"
# "deep_slider_aut_y3"
# "deep_aut_quarter_y1"
# "deep_aut_quarter_y2"
# "deep_aut_quarter_y3"
# "deep_slider_pro_y1"
# "deep_slider_pro_y2"
# "deep_slider_pro_y3"
# "deep_out_considered"
# "deep_out_year"
# "deep_out_quarter"
  
  # This produces original slider values
  #   How to deal with quarterly automation and outsourcing
  #   Take automation and outsourcing amounts from q4s
  #   Check when automation and outsourcing occured by year and quarter
  #   This could be different for different sections - if so, take earliest?
  #   This fills automation amount and quarter
  
  # The slider values preload into the sliders
  # If they refresh that chart, it then uses the new slider values to calculate line by line,
  # then sums up
  
  
  

    
}

shinyApp(ui=ui,server=server)