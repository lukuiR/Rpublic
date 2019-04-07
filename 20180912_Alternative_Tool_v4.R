# FINAL MODEL SHINY
library(shinydashboard)
library(DT)
library(Cairo)
library(RColorBrewer)
options(shiny.usecairo=T)

source('20180912_Alternative_Tool_Backend v2.R')


# Notes -------------------------------------------------------------------
# Hire and exit rates will need to be annual - different calculation in input file
#   vs the calculation for the powerpoints
# Automation and productivity will be vs Y0


# UI ----------------------------------------------------------------------


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
              #menuItem("Instructions",tabName="tab_instructions"),
              menuItem("Model Setup",tabName="tab_setup"),
              menuItem("Executive View",tabName="tab_executive"),
              menuItem("Deep Dive",tabName="tab_deepdive")
  )
)


# Seperate Rows -----------------------------------------------------------


# ...Setup Rows -----------------------------------------------------------

row_setup_scenario <- fluidRow(
  box(
    title="Scenario Selection:",solidHeader = TRUE,status="primary",colour="light blue",
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

baseline_positioning_1 <- fluidRow(column(6,sliderInput("baselinepos",
                                                      label = div(style='width:300px;', 
                                                                  div(style='float:left;', 'Conservative end of range'), 
                                                                  div(style='float:right;', 'Aggressive end of range')),
                                                      min=0,max=100,value=50,post=" %"))
)

row_global_parameters <- fluidRow(
  box(
    title="Global Parameters:",solidHeader = TRUE,status="primary",colour="light blue",
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
  box(title="Supply rate adjustments:",solidHeader = TRUE, status="primary",colour="light blue",
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
      ),
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
      ),
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
      ),
      width=12
  ))


# ......Automation tab -----------------------------------------------------

automation_rate_adjustment <- fluidRow(
  box(title="Automation rate adjustments:",solidHeader = TRUE, status="primary",colour="light blue",
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
  box(title="Training spend per FTE ($):",solidHeader = TRUE, status="primary",colour="light blue",
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
    title="Specific Parameters:",solidHeader = TRUE,status="primary",colour="light blue",
    fluidRow(
      column(4,uiOutput("l0_list_indep")),
      column(4,uiOutput("l1_list_indep")),
      column(4,uiOutput("l2_list_indep"))
      # selectInput(inputId = "setup_scenario",label = "Scenario to adjust parameters for:",
      #             choices = c("Conservative","Realistic","Optimistic"),selected="Realistic"),
      # tableOutput("printer"), # For printing value you want printed
      # tableOutput("print_raw_filtered_setup_scenario")
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


# EXECUTIVE VIEW TABS -----------------------------------------------------

row_executivetest <- fluidRow(actionButton("generate_csv","Generate csv of final data"))

# Body --------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tab_setup",
            row_setup_scenario,
            row_global_parameters,
            row_setup_l0l1l2select),
    tabItem(tabName = "tab_executive",
            row_executivetest),
    tabItem(tabName = "tab_deepdive")
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


# Creating reactive values ------------------------------------------------

  
  rvalues <- reactiveValues()
  
  # Setting raw data values: This is the full raw data table
  rvalues$rawdata <- output_table
  
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
    print("Calculated l2 list")
    print(output)
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
        select(baseline_mid,!!columnhire_name_y1,!!columnexit_name_y1,!!columnhire_name_y2,!!columnexit_name_y2,!!columnhire_name_y3,!!columnexit_name_y3)
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
      summarise(sum(get(columnhire_y1)),sum(get(columnexit_y1)),
                sum(get(columnhire_y2)),sum(get(columnexit_y2)),
                sum(get(columnhire_y3)),sum(get(columnexit_y3)))

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
        select(baseline_mid,!!columnaut_name_y1,!!columnaut_name_y2,!!columnaut_name_y3,!!columnautq_name_y1,!!columnautq_name_y2,!!columnautq_name_y3)
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
        select(baseline_mid,!!columnpro_name_y0,!!columnpro_name_y1,!!columnpro_name_y2,!!columnpro_name_y3)
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
        select(baseline_mid,!!columnout_considered_name,!!columnout_year_name,!!columnout_quarter_name)
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
      summarise("baseline_mid"=sum(baseline_mid))
    totalhc = as.numeric(totalhc$baseline_mid)
    
    
    # Weighting every multiplier by the section's proportion of the whole
    outputcol_base<- paste("baselinepos",scenario,sep="_")
    inputcol_base <- as.name(paste("baselinepos",scenario,sep="_"))
      
    output = output %>%
      mutate(!!outputcol_base := !!inputcol_base/totalhc*baseline_mid)
    

    # Adding up these to get the weighted average
    output = output %>%
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




# Calculating the multiplier implied by the demand drivers
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
    updateSelectInput(session=session,"l1_list_indep",selected = "All")
  })


# EXECUTIVE DASHBOARD CALCULATIONS ----------------------------------------


# Generate csv test -------------------------------------------------------
  observeEvent(input$generate_csv,{
    write_csv(rvalues$final_data,"Final Data Output Values.csv")
  })
  
  
}

shinyApp(ui=ui,server=server)