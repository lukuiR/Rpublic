#########     WORKFORCE FORECASTING TOOL: FRONT END     ##########

# This is the frontend code for the workforce forecasting tool. It should not be touched, 
# except to set the working directory as outlined below, or to run the tool using 

##### Setting the source #####

# The source is the name of the backend file, stored in the same folder as this frontend
# code. Replace the file name below with the file name of the backend source code.

source('20180930_Alternative_Tool_Backend v3.R')

##### Running the code #####

# Running this code runs the tool itself. You can run the tool either by using the 
# shortcut ctrl+shift+enter , or by clicking the "Run App" button in the top right
# of the window











########## DO NOT ADJUST ANY CODE BELOW THIS LINE ##########

# Javascript code

jsCode <- 
"
shinyjs.winprint = function(){
window.print();
}

shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}

"

# UI ----------------------------------------------------------------------


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
              tags$head(tags$style(".inactiveLink {
                            pointer-events: none;
                           cursor: default;
                           }")),
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),
              #tags$div(title="Instructions in here",
              menuItem("Instructions",tabName="tab_instructions",icon=icon("book-open")),
              menuItem("Model Initialisation",tabName="tab_setup",icon=icon("cog")),
              menuItem("Executive View",tabName="tab_executive",icon=icon("stats")),
              menuItem("Deep Dive",tabName="tab_deepdive",icon=icon("zoom-in"))
  ),uiOutput('style_tag')
)

# INSTRUCTIONS TAB --------------------------------------------------------

instructions_logo <- uiOutput(outputId = "stc_logo")
instructions_logo_mercer <- uiOutput(outputId = "mercer_logo")

row_instruction_generate <- 
  fluidRow(
    box(p("This tool is designed to enable detailed forecasting of workforce requirements throughout STC's
          workforce over a three year time period.", 
             style = "font-family: 'Source Sans Pro';"),
        h2("Instructions",style = "font-family: 'Source Sans Pro';"),
        p("First, go to the Model Initialisation tab. Here you must generate data with the button at the top, before you 
          are able to access the other tabs. Once data is generated, the executive view and deep dive view show both the 
          resulting workforce forecasts, and the details behind them.", 
          style = "font-family: 'Source Sans Pro';"),
        p("The tool is divided into three sections:", 
          style = "font-family: 'Source Sans Pro';"),
        h4("1. Model Initialisation",style = "font-family: 'Source Sans Pro';"),
        p("The Model Initialisation must be visited first to generate the data, and adjust underlying assumptions if desired. 
Driving the tool is data on both the current workforce state and the key levers that influence future demand: driver changes, automation, training and outsourcing,
          produced from both STC's data and market research. These values are preloaded for every individual section based on 
a combination of research and STC's own data, however should you wish to adjust these underlying assumptions in bulk you may do so in this tab.",style = "font-family: 'Source Sans Pro';"),
        h4("2. Executive View",style = "font-family: 'Source Sans Pro';"),
        p("The Executive View provides a high level overview of STC and its largest functions. 
          It displays the workforce forecasts, details the impact of the different workforce levers, 
          and highlights the supply/demand gap.",style = "font-family: 'Source Sans Pro';"),
        h4("3. Deep Dive",style = "font-family: 'Source Sans Pro';"),
        p("The Deep Dive provides key demographic information at every level of the organisation, and 
          allows for detailed section level planning.",style = "font-family: 'Source Sans Pro';")

  ,width=12)
)




# MODEL SETUP TAB ---------------------------------------------------------

# Seperate Rows -----------------------------------------------------------


# ...Setup Rows -----------------------------------------------------------

row_setup_introduction <- fluidRow(
  box(
    title="Introduction",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    p("This is the Model Initialisation page. Here you are able to adjust the underlying model parameters 
      for the three different scenarios. When you are finished, even if you did not make any 
      changes, click the 'Generate' button just below this box to calculate forecast headcounts and enable the other tabs.",
      style = "font-family: 'Source Sans Pro';"),
    p("All values are preloaded with the results of research and STC data. An average of these, weighted on 
      baseline headcount, is used to preload the sliders below.",
      style = "font-family: 'Source Sans Pro';"),
    p("There are two types of parameter that can be adjusted: Global Parameters, which apply to the whole organisation, and 
Specific Parameters, which apply only to the selected area of the organisation.",
      style = "font-family: 'Source Sans Pro';")
    ,width=12)
)

row_setup_generate <- fluidRow(
    column(12,
           wellPanel(align="center",
                     actionButton("generateall","Generate scenario results and enable output tabs",
                     icon("flash"),style="color: #fff;
                     background-color: #0FB694;
                     border-color: #0FB694;
                     padding-left:20%;
                     padding-right:20%;
                     font-size:100%"),
                     bsPopover(id="generateall",title="Generates the quarterly headcount and cost values based on the 
          parameters defined in this setup initialisation tab. Must be clicked for graphs 
          to be generated.",trigger="hover")
      ))
)


row_setup_scenario <- fluidRow(
  box(
    title="Scenario Selection",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    p("First, select which scenario you would like to adjust parameters for.",
      style = "font-family: 'Source Sans Pro';"),
    radioButtons("setup_scenario",label = "Scenario to adjust parameters for:",
                 choices = c("Conservative","Realistic","Optimistic"),inline=TRUE,selected="Realistic"),
    p("Next, you can adjust global and specific parameters by expanding the boxes below",
      style = "font-family: 'Source Sans Pro';")
  ,width=12)
)

# ...Global Parameters ----------------------------------------------------


# ......Driver tab --------------------------------------------------------

demand_driver_adjustment_row0 <- fluidRow(
  #checkboxInput("ddlock","Maintain subscriber / revenue ratio?",value=TRUE),
  actionButton("reset_dd","Reset to default",
               style="color: #fff;
                     background-color: #ED2C67;
                     border-color: #ED2C67;
                     font-size:100%"),
  bsPopover(id="reset_dd",title="Resets all demand driver values to their defaults for the selected scenario",trigger="hover")
)

demand_driver_adjustment_row9 <- fluidRow(
  actionButton("save_dd","Save",
               style="color: #fff;
                     background-color: #0FB694;
                     border-color: #0FB694;
                     font-size:100%"),
  bsPopover(id="save_dd",title="Saves your changes to the demand driver values for the selected scenario",trigger="hover")
)
  
demand_driver_adjustment_row1 <- 
      fluidRow(column(4,htmlOutput("driverheader1")),
               column(2,htmlOutput("driverheader2")),
               column(2,htmlOutput("driverheader3")),
               column(2,htmlOutput("driverheader4")),
               column(2,htmlOutput("driverheader5"))
      )

demand_driver_adjustment_row2 <-
      fluidRow(column(4,htmlOutput("driverrow1")),
               column(2,numericInput("driver_entrev0",label=NULL,value=0,min=0,max=NA,step=1)),
               column(2,numericInput("driver_entrev1",label=NULL,value=0,min=0,max=NA,step=1)),
               column(2,numericInput("driver_entrev2",label=NULL,value=0,min=0,max=NA,step=1)),
               column(2,numericInput("driver_entrev3",label=NULL,value=0,min=0,max=NA,step=1))
      )

demand_driver_adjustment_row2.5 <- 
  fluidRow(column(4,htmlOutput("driverrow1_per")),
           column(2,textOutput("driver_entrev0_per"),align="left"),
           column(2,textOutput("driver_entrev1_per"),align="left"),
           column(2,textOutput("driver_entrev2_per"),align="left"),
           column(2,textOutput("driver_entrev3_per"),align="left")
  )

demand_driver_adjustment_blank1 <- 
  fluidRow(htmlOutput("blanktext1"))


demand_driver_adjustment_row3 <-
  fluidRow(column(4,htmlOutput("driverrow2")),
           column(2,numericInput("driver_conrev0",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_conrev1",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_conrev2",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_conrev3",label=NULL,value=0,min=0,max=NA,step=1))
  )

demand_driver_adjustment_row3.5 <- 
  fluidRow(column(4,htmlOutput("driverrow2_per")),
           column(2,textOutput("driver_conrev0_per"),align="left"),
           column(2,textOutput("driver_conrev1_per"),align="left"),
           column(2,textOutput("driver_conrev2_per"),align="left"),
           column(2,textOutput("driver_conrev3_per"),align="left")
  )

demand_driver_adjustment_blank2 <- 
  fluidRow(htmlOutput("blanktext2"))

demand_driver_adjustment_row4 <-
  fluidRow(column(4,htmlOutput("driverrow3")),
           column(2,numericInput("driver_whorev0",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_whorev1",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_whorev2",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_whorev3",label=NULL,value=0,min=0,max=NA,step=1))
  )

demand_driver_adjustment_row4.5 <- 
  fluidRow(column(4,htmlOutput("driverrow3_per")),
           column(2,textOutput("driver_whorev0_per"),align="left"),
           column(2,textOutput("driver_whorev1_per"),align="left"),
           column(2,textOutput("driver_whorev2_per"),align="left"),
           column(2,textOutput("driver_whorev3_per"),align="left")
  )

demand_driver_adjustment_blank3 <- 
  fluidRow(htmlOutput("blanktext3"))

demand_driver_adjustment_row5 <-
  fluidRow(column(4,htmlOutput("driverrow4")),
           column(2,numericInput("driver_sub0",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_sub1",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_sub2",label=NULL,value=0,min=0,max=NA,step=1)),
           column(2,numericInput("driver_sub3",label=NULL,value=0,min=0,max=NA,step=1))
  )

demand_driver_adjustment_row5.5 <- 
  fluidRow(column(4,htmlOutput("driverrow4_per")),
           column(2,textOutput("driver_sub0_per"),align="left"),
           column(2,textOutput("driver_sub1_per"),align="left"),
           column(2,textOutput("driver_sub2_per"),align="left"),
           column(2,textOutput("driver_sub3_per"))
  )

demand_driver_adjustment_blank4 <- 
  fluidRow(htmlOutput("blanktext4"))

demand_driver_adjustment_row5.75 <- 
  fluidRow(column(4,htmlOutput("driverrow4.5_totalrev")),
           column(2,textOutput("totalrev0"),align="left"),
           column(2,textOutput("totalrev1"),align="left"),
           column(2,textOutput("totalrev2"),align="left"),
           column(2,textOutput("totalrev3"),align="left")
  )

demand_driver_adjustment_row6 <-
  fluidRow(column(4,htmlOutput("driverrow5")),
           column(2,textOutput("driverrevsub0"),align="left"),
           column(2,textOutput("driverrevsub1"),align="left"),
           column(2,textOutput("driverrevsub2"),align="left"),
           column(2,textOutput("driverrevsub3"),align="left")
  )

demand_driver_adjustment_row6.5 <- 
  fluidRow(column(4,htmlOutput("driverrow5_per")),
           column(2,textOutput("driverrevsub0_per"),align="left"),
           column(2,textOutput("driverrevsub1_per"),align="left"),
           column(2,textOutput("driverrevsub2_per"),align="left"),
           column(2,textOutput("driverrevsub3_per"),align="left")
  )

blankrow <- fluidRow()


# ...... Baseline Positioning tab -----------------------------------------
baseline_positioning_0 <- fluidRow(
  actionButton("reset_baseline","Reset to default",
               style="color: #fff;
                     background-color: #ED2C67;
                     border-color: #ED2C67;
                     font-size:100%"),
  bsPopover(id="reset_baseline",title="Resets baseline positioning to the default for the selected scenario",trigger="hover")
)

baseline_positioning_1 <- fluidRow(
  p(""),
p("A combination of top down benchmarking against peers and internal section comparisons produces a range for the total headcount currently needed - the baseline, 
which acts as the starting point for the projections. This slider allows you to adjust how optimistic you would like the starting point of your projection to be 
  for the selected scenario.", 
  style = "font-family: 'Source Sans Pro';"),
  # p("The graph on the right shows an illustration of the impact on the start of the forecast for your chosen slider value",
  #   style = "font-family: 'Source Sans Pro';"),
  includeScript("slider.js"),
  div(class="my_slider",
    sliderInput("baselinepos",label = "",ticks = F,
                # label = div(
                #             div(style='float:left;', "More Headcount"),
                #             div(style='float:right', "Less Headcount")
                #            ),
                min=0,max=100,value=50))
    #,post=" %")
# column(6,plotOutput("baseline_positioning_graph",height="250px"))
)

baseline_positioning_2 <- fluidRow(
  actionButton("save_baseline","Save",
               style="color: #fff;
               background-color: #0FB694;
               border-color: #0FB694;
               font-size:100%"),
  bsPopover(id="save_baseline",title="Saves your adjusted baseline positioning for the selected scenario",trigger="hover")
  )


row_global_parameters <- fluidRow(
  tags$style(".nav-tabs {
  background-color: transparent;
}

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
background-color: #00A8C8  ;
border-color: #FFFFFF ;
color: #FFFFFF;
}

.nav-tabs-custom .nav-tabs li.active {
    border-top-color: transparent ;
}"),
  box(
    title= textOutput("globalparam_title"),solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,collapsed=TRUE,
    p("Here you can adjust global parameters, which apply to the whole organisation for your selected scenario.",
      style = "font-family: 'Source Sans Pro';"),
    tabBox(
      id = "global_parameters_box",
      width=12,
      # height="175px",
      tabPanel("Demand",
               p("Changes in top level demand drivers are key levers in determining the required headcount in future years. 
Here you can change the forecast values, which will impact the required workforce projections.",
                 style = "font-family: 'Source Sans Pro';"),
               demand_driver_adjustment_row0,
               htmlOutput("blanktext7"),
               demand_driver_adjustment_row1,
               demand_driver_adjustment_row2,
               demand_driver_adjustment_row2.5,
               demand_driver_adjustment_blank1,
               demand_driver_adjustment_row3,
               demand_driver_adjustment_row3.5,
               demand_driver_adjustment_blank2,
               demand_driver_adjustment_row4,
               demand_driver_adjustment_row4.5,
               demand_driver_adjustment_blank3,
               demand_driver_adjustment_row5,
               demand_driver_adjustment_row5.5,
               demand_driver_adjustment_blank4,
               demand_driver_adjustment_row5.75,
               demand_driver_adjustment_row6,
               demand_driver_adjustment_row6.5,
               htmlOutput("blanktext8"),
               demand_driver_adjustment_row9),
      tabPanel("Baseline Positioning",
               baseline_positioning_0,
               baseline_positioning_1,
               baseline_positioning_2)
    )
    ,width=12)
)


# ...Specific Parameters -----------------------------------------------------

# ......Supply tab --------------------------------------------------------

supply_rate_adjustment <- fluidRow(
  box(title="Supply rate adjustments",solidHeader = TRUE, status="primary",colour="light blue",
      p("Adjusting the annual hire and exit rates impacts the supply forecasts",
        style = "font-family: 'Source Sans Pro';"),
      tabBox(
        id = "supply_rate_box",
        width=12,
        # height="175px",
        tabPanel("Year 1",
          fluidRow(column(6,
                          knobInput(
                             inputId = "hire_rate_Y1",label = "Year 1 hire percentage:",
                             value = 0,min = 0,max=200,displayPrevious = TRUE,lineCap = "round",
                             fgColor = "#00A8C8",angleOffset = -135,angleArc=270,inputColor = "#00A8C8")),
                   column(6,
                          knobInput(
                            inputId = "exit_rate_Y1",label = "Year 1 exit percentage:",
                            value = 0,min = 0,max=200,displayPrevious = TRUE,lineCap = "round",
                            fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
          )),
        tabPanel("Year 2",
          fluidRow(column(6,
                          knobInput(
                            inputId = "hire_rate_Y2",label = "Year 2 hire percentage:",
                            value = 0,min = 0,max=200,displayPrevious = TRUE,lineCap = "round",
                            fgColor = "#00A8C8",angleOffset = -135,angleArc=270,inputColor = "#00A8C8")),
                   column(6,
                          knobInput(
                            inputId = "exit_rate_Y2",label = "Year 2 exit percentage:",
                            value = 0,min = 0,max=200,displayPrevious = TRUE,lineCap = "round",
                            fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
          )),
        tabPanel("Year 3",
          fluidRow(column(6,
                          knobInput(
                            inputId = "hire_rate_Y3",label = "Year 3 hire percentage:",
                            value = 0,min = 0,max=200,displayPrevious = TRUE,lineCap = "round",
                            fgColor = "#00A8C8",angleOffset = -135,angleArc=270,inputColor = "#00A8C8")),
                   column(6,
                          knobInput(
                            inputId = "exit_rate_Y3",label = "Year 3 exit percentage:",
                            value = 0,min = 0,max=200,displayPrevious = TRUE,lineCap = "round",
                            fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
          ))),
      width=12
  ))


# ......Automation tab -----------------------------------------------------

automation_rate_adjustment <- fluidRow(
  box(title="Automation rate adjustments",solidHeader = TRUE, status="primary",colour="light blue",
      p("Select both the automation percentage forecast for each year, as well as the quarter of the year 
        in which it will take effect.",
        style = "font-family: 'Source Sans Pro';"),
      fluidRow(column(4,sliderInput("aut_slider_Y1","Year 1 Automation Level:",
                                           min=0,max=100,value= 50,step=1,post=" %")),
               column(4,sliderInput("aut_slider_Y2","Year 2 Automation Level:",
                                    min=0,max=100,value= 50,step=1,post=" %")),
               column(4,sliderInput("aut_slider_Y3","Year 3 Automation Level:",
                                    min=0,max=100,value= 50,step=1,post=" %"))
      ),
      fluidRow(column(4,radioButtons("aut_quarter_Y1","Quarter it will take effect:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE)),
               column(4,radioButtons("aut_quarter_Y2","Quarter it will take effect:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE)),
               column(4,radioButtons("aut_quarter_Y3","Quarter it will take effect:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE))
      )
  ,width=12)
)



# ......Training tab ---------------------------------------------------------
training_spend_adjustment <- fluidRow(
  box(title="Training spend per FTE ($)",solidHeader = TRUE, status="primary",colour="light blue",
      p("Enter the forecast spend on training per employee for each year in USD",
        style = "font-family: 'Source Sans Pro';"),
      fluidRow(column(3,numericInput("pro_input_Y0","Year 0 training spend / FTE :",
                                     min=0,max=50000,value= 2707,step=1)),
               column(3,numericInput("pro_input_Y1","Year 1 training spend / FTE:",
                                      min=0,max=50000,value= 2707,step=1)),
               column(3,numericInput("pro_input_Y2","Year 2 training spend / FTE:",
                                     min=0,max=50000,value= 2707,step=1)),
               column(3,numericInput("pro_input_Y3","Year 3 training spend / FTE:",
                                     min=0,max=50000,value= 2707,step=1))
      )
  ,width=12)
)


# ......Outsourcing tab ---------------------------------------------------

# outsourcing_adjustment <- fluidRow(
#   box(title="Outsource Section?",solidHeader = TRUE, status="primary",colour="light blue",
#       p("All sections in the organisation have been classified as outsourceable or not. Selecting here 
#         to outsource identified sections within the selected function will forecast demand based on those sections being outsourced in 
#         the selected year and quarter. Sections that have not been classed as outsourceable will not be affected. This will 
#         not outsource the entire selected function.",
#         style = "font-family: 'Source Sans Pro';"),
#       fluidRow(column(4,checkboxInput("out_considered","Outsource identified sections within function?",
#                                      value=FALSE)),
#                column(4,selectInput("out_year","Year to outsource in:",
#                                      c("Y0","Y1","Y2","Y3"))),
#                column(4,selectInput("out_quarter","Quarter to outsource in:",
#                                     c("q1","q2","q3","q4")))
#       )
#       ,width=12)
# )

# In-tab horizontal tabs --------------------------------------------------

row_setup_l0l1l2select <- fluidRow(
  box(
    title=textOutput("specificparam_title"),solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,collapsed=TRUE,
    p("Here you can adjust parameters for a specific area of STC. These will overwrite the underlying section values, and give 
      all sections within the selection the new value.",
      style = "font-family: 'Source Sans Pro';"),
    fluidRow(
      column(4,uiOutput("l0_list_indep")),
      column(4,uiOutput("l1_list_indep")),
      column(4,uiOutput("l2_list_indep"))
    ),
    fluidRow(
      column(3,actionButton("generate","Load selected",
                            style="color: #fff;
                     background-color: #FBAE17;
                     border-color: #FBAE17;
                     font-size:100%")),
      bsPopover(id="generate",title="Loads the weighted average values for supply, automation and training for the selected area of STC ",trigger="hover"),
      column(3,actionButton("save","Save selected",
                            style="color: #fff;
                     background-color: #0FB694;
                     border-color: #0FB694;
                     font-size:100%")),
      bsPopover(id="save",title="Applies the below specific parameter values for supply, automation and training to every function in the selected area of STC. Note this will result in all these having the same values, and will overwrite their individual presets",trigger="hover"),
      column(3,actionButton("reset_select","Reset selected",
                            style="color: #fff;
                     background-color: #ED2C67;
                     border-color: #ED2C67;
                     font-size:100%")),
      bsPopover(id="reset_select",title="Resets all specific parameter values for the selected area of STC to their defaults for the selected scenario",trigger="hover"),
      column(3,actionButton("reset_all","Reset all",
                            style="color: #fff;
                     background-color: #ED2C67;
                     border-color: #ED2C67;
                     font-size:100%")),
      bsPopover(id="reset_all",title="Resets all specific parameter values for the whole of STC to their defaults for the selected scenario",trigger="hover")
    ),
    fluidRow(
      tabBox(id = "setup_tabs",
             tabPanel("Supply",
                      supply_rate_adjustment),
             tabPanel("Automation",
                      automation_rate_adjustment),
             tabPanel("Training",
                      training_spend_adjustment),
             # tabPanel("Outsourcing",
             #          outsourcing_adjustment),
             width=12
      )
    )
    ,width=12)
)


# EXECUTIVE VIEW TAB -----------------------------------------------------

row_exec_introduction <- fluidRow(
  box(
    title="Introduction",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    p("The Executive Dashboard shows the workforce forecast for high level areas of STC, along with a breakdown of the impact 
      of the different levers. Expand the boxes below to view the charts. It can be viewed by whole organisation or specific 
units, sectors or general directorates. You can also filter by segment, scenario, and choose whether you want to display headcount or cost information.",
      style = "font-family: 'Source Sans Pro';")
  )
)


row_exec_scenario <- fluidRow(
  box(
    title="Refresh graphs",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    actionButton("exec_refresh_graphs","Refresh Graphs",
                 style="color: #fff;
                 background-color: #0FB694;
                 border-color: #0FB694;
                 font-size:100%"),
    bsPopover(id="exec_refresh_graphs",title="Refreshes graphs to reflect changes in function selection, segment, scenario and choice of headcount or cost",trigger="hover")
    ,width=2),
  box(
    title="Scenario Selection",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    radioButtons("exec_scenario",label = "Scenario to display:",
                 choices = c("Conservative","Realistic","Optimistic"),inline=TRUE,selected="Realistic")
    ,width=5),
  box(
    title="View by headcount or cost?",solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,
    radioGroupButtons("headcount_or_cost",
                         choices = c("Headcount","Cost"),
                         justified = TRUE,status="primary",
                         checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
    ),
    bsPopover(id="headcount_or_cost",title="Cost is caluclated as average employee fixed monthly cost. Where no cost data is available, 
              the average cost for the level above is taken.",trigger="hover")
    ,width=5)
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
                         choices = c("Core","Support","Specialist","Critical","Management"),
                         multiple=TRUE,options=list(`actions-box`=TRUE),
                         selected=c("Core","Support","Specialist","Critical","Management"))
             )
      )
  ,width=3)
)

row_executive_line <- fluidRow(
  box(
    title=textOutput("line_chart_title"),solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,collapsed=TRUE,
    plotlyOutput("line"),
    p("Choose which lines to view by clicking the lines in the legend. You can zoom in to the plot by clicking and dragging, 
      and use the controls in the top right to dynamically adjust the graph and labels.",
      style = "font-family: 'Source Sans Pro';")
    ,width=12)
)

row_executive_waterfall<- fluidRow(
  box(
    title=textOutput("workforce_journey_title"),solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,collapsed=TRUE,
    plotOutput("waterfall"),width=12)
)

row_executive_barchart <- fluidRow(
  box(
    title=textOutput("demand_gap_title"),solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,collapsed=TRUE,
    plotOutput("supplydemandbarchart"),width=6),
  box(
    title=textOutput("demand_scenarios_title"),solidHeader = TRUE,status="primary",colour="light blue",collapsible = TRUE,collapsed=TRUE,
    plotOutput("barchart"),width=6)
)


# DEEP DIVE VIEW TAB ------------------------------------------------------


# ...Section Selection ----------------------------------------------------

row_deep_generate <- fluidRow(
  box(
    column(align="center",
           div(style="display:inline-block",actionButton("deep_generate","Generate deep dive outputs",
                                                         icon("flash"),style="color: #fff; 
                                                         background-color: #0FB694; 
                                                         border-color: #0FB694;
                                                         padding-left:40%;
                                                         padding-right:40%;
                                                         font-size:100%"),style="display:center-align"),
           bsPopover(id="deep_generate",title="Generate deep dive outputs for the selected area of STC. This must be clicked before proceeding to the next tabs, or the data will not be available.",trigger="hover"),
           width=12),status="primary",width=12)
)

# row_deep_selection_singlebul <- fluidRow(
#   box(selectInput("single_agg_choice","View by: ",
#                   choices = c("Single Section Data","Aggregated Data"),
#                   selected="Aggregated Data",multiple=FALSE),
#       width=6,status="primary"))

row_deep_selection_secselect <- fluidRow(
  box(
    title="Section Selection:",solidHeader = TRUE,status="primary",colour="light blue",
    column(6,uiOutput("deep_l0_list"),uiOutput("deep_l2_list"),uiOutput("deep_l4_list")),
    column(6,uiOutput("deep_l1_list"),uiOutput("deep_l3_list"),pickerInput("segment_list_deep","Segment:",
                                                                           choices = c("Core","Support","Specialist","Critical","Management"),
                                                                           multiple=TRUE,options=list(`actions-box`=TRUE),
                                                                           selected=c("Core","Support","Specialist","Critical","Management"))),
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

row_introduction <- fluidRow(
  box(
      p("Once you have generated data in the Section Selection tab, you can view detailed projections below. 
    The levers are preloaded with the average value for the sections selected, as a result of the parameters set in 
    model initialisation, weighted by baseline headcount. 
    These can be adjusted by moving the sliders, and once these are saved the resulting impact can be viewed by refreshing 
    the graph.",
        style = "font-family: 'Source Sans Pro';"),
      p("You can toggle between viewing the original data, and the adjusted data, with the 'Use original values' toggle below. 
    Note that the sliders apply to all sections in the selected area of STC equally, and will thus produce different results to 
    the original values",
        style = "font-family: 'Source Sans Pro';")
  ,width=12)
)

row_scenario <- box(
  title="Scenario adjustment:",solidHeader = TRUE,status="primary",colour="light blue",
  actionButton("deep_reset","Reset all scenarios",
               style="color: #fff;
                     background-color: #ED2C67;
                     border-color: #ED2C67;
                     font-size:100%"),
  bsPopover(id="deep_reset",title="Reset parameter values for all scenarios to their defaults",trigger="hover"),
  radioButtons("deep_scenario",label = "Scenario to adjust parameters for:",
               choices = c("Conservative","Realistic","Optimistic"),inline=TRUE,selected="Realistic"),
  tabPanel("Baseline Positioning",
           sliderInput("deep_slider_baseline","Baseline positioning:",
                       min=0,max=100,value=50,post=" %")),
  tabBox(
    tabPanel("Drivers",
             column(4,sliderInput("deep_slider_dd_Y1","Demand driver impact Y1:",
                                  min=-50,max=50,value=0,post=" %")),
             column(4,sliderInput("deep_slider_dd_Y2","Demand driver impact Y2:",
                         min=-50,max=50,value=0,post=" %")),
             column(4,sliderInput("deep_slider_dd_Y3","Demand driver impact Y3:",
                         min=-50,max=50,value=0,post=" %"))
    ),
    # tabPanel("Supply",
    #          tabBox(
    #            id="tabs_deepdive_sliders_supply",
    #            tabPanel("Y1",
    #                     column(6,sliderInput("deep_slider_hire_Y1","Y1 Hire Rate:",
    #                                 min=50,max=150,value=50,post=" %")),
    #                     column(6,sliderInput("deep_slider_exit_Y1","Y1 Exit Rate:",
    #                                 min=50,max=150,value=50,post=" %"))),
    #            tabPanel("Y2",
    #                     column(6,sliderInput("deep_slider_hire_Y2","Y2 Hire Rate:",
    #                                 min=50,max=150,value=50,post=" %")),
    #                     column(6,sliderInput("deep_slider_exit_Y2","Y2 Exit Rate:",
    #                                 min=50,max=150,value=50,post=" %"))),
    #            tabPanel("Y3",
    #                     column(6,sliderInput("deep_slider_hire_Y3","Y3 Hire Rate:",
    #                                 min=50,max=150,value=50)),
    #                     column(6,sliderInput("deep_slider_exit_Y3","Y3 Hire Rate:",
    #                                 min=50,max=150,value=50,post=" %")))
    #          ,width=12)
    # ),
    tabPanel("Automation",
             fluidRow(
               column(4,sliderInput("deep_slider_aut_Y1","Year 1 Automation Level:",
                                    min=0,max=100,value= 0,post=" %")),
               column(4,sliderInput("deep_slider_aut_Y2","Year 2 Automation Level:",
                                    min=0,max=100,value= 0,post=" %")),
               column(4,sliderInput("deep_slider_aut_Y3","Year 3 Automation Level:",
                                    min=0,max=100,value= 0,post=" %"))
             ),
             fluidRow(column(4,radioButtons("deep_aut_quarter_Y1","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE)),
                      column(4,radioButtons("deep_aut_quarter_Y2","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE)),
                      column(4,radioButtons("deep_aut_quarter_Y3","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE))
             )
    ),
    tabPanel("Training",
             fluidRow(
               column(4,sliderInput("deep_slider_pro_Y1","Year 1 Training Impact:",
                                    min=0,max=100,value= 0,step=1,post=" %")),
               column(4,sliderInput("deep_slider_pro_Y2","Year 2 Training Impact:",
                                    min=0,max=100,value= 0,step=1,post=" %")),
               column(4,sliderInput("deep_slider_pro_Y3","Year 3 Training Impact:",
                                    min=0,max=100,value= 0,step=1,post=" %"))
             )
    )
    # tabPanel("Outsourcing",
    #          fluidRow(column(4,checkboxInput("deep_out_considered","Outsource identified sections within function?",
    #                                          value=FALSE)),
    #                   column(4,selectInput("deep_out_year","Year to outsource in:",
    #                                        c("Y0","Y1","Y2","Y3"))),
    #                   column(4,selectInput("deep_out_quarter","Quarter to outsource in:",
    #                                        c("Q1","Q2","Q3","Q4")))
    #          )
    # )
    ,width=12),
  actionButton("deep_save","Save scenario values",
               style="color: #fff;
                     background-color: #0FB694;
                     border-color: #0FB694;
                     font-size:100%"),
  bsPopover(id="deep_save",title="Save parameter values for the selected scenario",trigger="hover")
  ,width=12,collapsible = TRUE)

row_output_graphsettings <- fluidRow(
  box(
    title="Graph Settings",solidHeader = TRUE,status="primary",colour="light blue",
    column(6,radioGroupButtons("deep_headcount_or_cost",
                               choices = c("Headcount","Cost"),
                               justified = TRUE,status="primary",
                               checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
    )),
    column(6,radioGroupButtons("deep_original_or_new",
                               choices = c("Use original values","Apply sliders to all"),
                               justified = TRUE,status="primary",
                               checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
    ),
    bsPopover(id="deep_original_or_new",title="Applying sliders to all overwrites all individual section parameters, and gives 
              all the values of the sliders set above. Original values use parameters defined in the model initialisation tab instead of the sliders here.",trigger="hover")),
    actionButton("deep_refresh_graphs","Refresh graphs",
                 style="color: #fff;
                     background-color: #0FB694;
                     border-color: #0FB694;
                     font-size:100%"),
    bsPopover(id="deep_refresh_graphs",title="Refresh graphs to reflect saved parameter changes",trigger="hover")
  ,width=12,collapsible = TRUE)
)

row_outputwaterfall_orig <- fluidRow(
  box(
    title="Workforce Projection Original",solidHeader = TRUE,status="primary",colour="light blue",
    plotOutput("waterfall"),width=12,collapsible = TRUE)
)

row_outputwaterfall_deep <- fluidRow(
  box(
    title="Workforce Projection New",solidHeader = TRUE,status="primary",colour="light blue",
    plotOutput("deep_waterfall"),width=12,collapsible = TRUE)
)


row_outputline <- fluidRow(
  box(
    title="Workforce Projection",solidHeader = TRUE,status="primary",colour="light blue",
    plotlyOutput("line_deep"),
    p("Choose which lines to view by clicking the lines in the legend. You can zoom in to the plot by clicking and dragging, 
      and use the controls in the top right to dynamically adjust the graph and labels.",
      style = "font-family: 'Source Sans Pro';")
    ,width=12,collapsible = TRUE)
)

# row_outputtable <- fluidRow(
#   box(dataTableOutput("deep_final_table"),width=12)
# )
# 
# row_outputtablesum <- fluidRow(
#   box(dataTableOutput("deep_final_table_summarised"),width=12)
# )
# 
# row_outputtableover <- fluidRow(
#   box(dataTableOutput("deep_final_table_overwritten"),width=12)
# )



# ...Bringing all together ------------------------------------------------

deepdive_introduction <- fluidRow(
  box(
    title="Introduction",solidHeader = TRUE,status="primary",colour="light blue",
    p("The Deep Dive section provides detailed demographic information and projections for any area of STC down to the 
section level. First, select the area of the organisation you wish to view below. Then click the generate button to generate 
      the outputs, which can be viewed in the tabs below.",
      style = "font-family: 'Source Sans Pro';")
  ,width=12)
)

tabs_deepdive <- fluidRow(tabBox(
  id = "tabs_deepdive",
  width=12,
  # height="175px",
  tabPanel("Section Selection",
           row_deep_generate,
           #row_deep_selection_singlebul,
           row_deep_selection_secselect),
  tabPanel("Section Summary",
           row_deep_summary_headcount,
           row_deep_summary_segmenteducation,
           row_deep_summary_agetenure),
  tabPanel("Workforce Journey",
           row_introduction,
           row_scenario,
           row_output_graphsettings,
           #row_outputwaterfall_orig,
           row_outputwaterfall_deep,
           #row_sliders,
           row_outputline
           #row_outputtable,
           #row_outputtablesum,
           #row_outputtableover
           )
))
#,width=12)




  
# Body --------------------------------------------------------------------

body <- dashboardBody(
  useShinyjs(),
  extendShinyjs(text = jsCode),
  #tags$head(tags$style(".shiny-progress {position: fixed !important; top: 50% !important;left: 50% !important;}")),
  tabItems(
    tabItem(tabName = "tab_instructions",
            instructions_logo,
            h2("Strategic Workforce Interactive Scenario Scoper",align="center",style="color:dark blue"),
            row_instruction_generate,
            instructions_logo_mercer,
            p("Tool code and materials are copyrighted works owned exclusively by Mercer and may not be copied, 
              modified, sold, transformed into any other media, or otherwise transferred in whole or in part 
              to any party other than STC, without prior written consent from Mercer. All rights reserved.",
              style = "font-family: 'Source Sans Pro';")
    ),
    tabItem(tabName = "tab_setup",
            h2("Model Initialisation"),
            row_setup_introduction,
            row_setup_generate,
            row_setup_scenario,
            row_global_parameters,
            row_setup_l0l1l2select),
    tabItem(tabName = "tab_executive",
            titlePanel("Executive Dashboard"),
            row_exec_l0l1l2select,
            row_exec_scenario,
            row_executive_waterfall,
            row_executive_line,
            row_executive_barchart
            ),
    tabItem(tabName = "tab_deepdive",
            titlePanel("Deep Dive"),
            deepdive_introduction,
            tabs_deepdive)
  )
)



# Bringing UI together ----------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title="Workforce Planning Tool", tags$li(class="dropdown",
                                                           actionButton("print","Print",
                                                                        icon("print"),style="color: #fff;
             background-color: #337ab7;
             border-color: #2e6da4;
             font-size:100%")),
                  titleWidth = 300),
  sidebar,
  body
)

# Server Logic ------------------------------------------------------------

server <- function(input,output,session) {

  #Disable analysis tabs when the app loads
  addCssClass(selector = "a[data-value='tab_executive']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='tab_deepdive']", class = "inactiveLink")
  
  observeEvent(input$print, {
    js$winprint()
  })
  
  addTooltip(session,id = "generateall", title = "Click to enable the output tabs and 
generate all scenario outputs using the parameters defined below. 
If no parameters have been adjusted, this will use the default parameters", 
            placement = "bottom", trigger = "hover")
  
  addTooltip(session,id = "tab_executive", title = "Generate data to view tab", 
             placement = "right", trigger = "hover")
  
  addTooltip(session,id = "tab_deepdive", title = "Generate data to view", 
             placement = "right", trigger = "hover")
  
  output$stc_logo <- renderUI({
    tags$img(src="STC-Logo.jpg",style="display: block; margin-left: auto; margin-right: auto; height: 200px;")
  })
  
  output$mercer_logo <- renderUI({
    tags$img(src="mercer-logo.png",style="display: block; margin-left: auto; margin-right: auto; height: 200px;")
  })
  
  output$style_tag <- renderUI({
    if(input$tabs=='tab_instructions')
      return(tags$head(tags$style(HTML('.content-wrapper {background-color:white;}'))))
    
    if(input$tabs=='tab_setup')
      return(tags$head(tags$style(HTML('.content-wrapper {background-color: #f2f3f4 ;}'))))
    
    if(input$tabs=='tab_executive')
      return(tags$head(tags$style(HTML('.content-wrapper {background-color: #f2f3f4 ;}'))))
    
    if(input$tabs=='tab_deepdive')
      return(tags$head(tags$style(HTML('.content-wrapper {background-color: #f2f3f4 ;}'))))

  })
  


  
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
      plot.title = element_text(hjust=0.5,colour="black")
      # axis.title = element_text(colour="black"),
      # axis.text = element_text(colour="black")
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
      geom_text_repel(aes(label=scenario),xlim=c(1.3,NA),hjust=0,segment.color = NA)+
      xlim(0,5)+
      ylab("Headcount")+
      ggtitle("Illustrative impact of baseline positioning adjustment")+
      scale_shape_identity()+
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.ticks=element_blank(),
            axis.line.y = element_line(colour="grey",size=0.5,linetype="solid"),
            axis.text = element_blank(),
            axis.title.x =element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(size=10,face="bold",hjust=0.5),
            #panel.border=element_blank(),
            panel.background = element_blank())
      
    graph
  })
  
  observeEvent(input$baselinepos,{
    baseline_positioning_table()
  })


# EXECUTIVE VIEW TAB ------------------------------------------------------

    

# Text outputs ------------------------------------------------------------

  specificsection <- reactive({
    rvalues$specificsection <- reactiverifelse(input$l2_list_indep!="All",input$l2_list_indep,ifelse(input$l1_list_indep!="All",input$l2_list_indep,input$l0_list_indep))
  })
  
  output$globalparam_title <- renderText(paste0("Global Parameters: " ,input$setup_scenario," scenario"))
  
  output$specificparam_title <- renderText(paste0("Specific Parameters: " ,input$setup_scenario," scenario"))
  
  output$workforce_journey_title <- renderText(paste0("3 Year Workforce Journey: ",rvalues$exec_scenario," Scenario (",rvalues$headcount_or_cost,")"))
  output$line_chart_title <- renderText(paste0("3 Year Workforce Projections: ",rvalues$exec_scenario," Scenario (",rvalues$headcount_or_cost,")"))
  output$demand_gap_title <- renderText(paste0("3 Year Demand / Supply Gap: ",rvalues$exec_scenario," Scenario (",rvalues$headcount_or_cost,")"))
  output$demand_scenarios_title <- renderText(paste0("Yearly Demand: Scenario Comparison: (",rvalues$headcount_or_cost,")"))
  
  exec_scenario_start <- reactive({
     rvalues$exec_scenario <- input$exec_scenario})

  rvalues$exec_scenario <- "Realistic"
  
  headcount_or_cost_start <- reactive({
     rvalues$headcount_or_cost <- input$headcount_or_cost})
  
  rvalues$headcount_or_cost <- "Headcount"
  
  observeEvent(input$exec_refresh_graphs,{
    rvalues$exec_scenario <- input$exec_scenario
    rvalues$headcount_or_cost <- input$headcount_or_cost
  })
  
  output$driverheader1 <- renderText(paste("<b>","Driver","</b>"))
  output$driverheader2 <- renderText(paste("<b>","Year 0 Value","</b>"))
  output$driverheader3 <- renderText(paste("<b>","Year 1 Value","</b>"))
  output$driverheader4 <- renderText(paste("<b>","Year 2 Value","</b>"))
  output$driverheader5 <- renderText(paste("<b>","Year 3 Value","</b>"))
  output$driverrow1 <- renderText("Enterprise Revenue ($m)")
  output$driverrow2 <- renderText("Consumer Revenue ($m)")
  output$driverrow3 <- renderText("Wholesale Revenue ($m)")
  output$driverrow4 <- renderText("Total STC Subscribers (m)")
  output$driverrow5 <- renderText("Revenue / Subscriber")
  
  output$blanktext1 <- renderText(paste("<span style=\"color:white\">.</span>"))
  output$blanktext2 <- renderText(paste("<span style=\"color:white\">.</span>"))
  output$blanktext3 <- renderText(paste("<span style=\"color:white\">.</span>"))
  output$blanktext4 <- renderText(paste("<span style=\"color:white\">.</span>"))
  output$blanktext5 <- renderText(paste("<span style=\"color:white\">.</span>"))
  output$blanktext6 <- renderText(paste("<span style=\"color:white\">.</span>"))
  output$blanktext7 <- renderText(paste("<span style=\"color:white\">.</span>"))
  
  output$blanktext8 <- renderText(paste("<span style=\"color:white\">.</span>"))
  
  output$driverrevsub0 <- renderText(round((input$driver_entrev0+input$driver_conrev0+input$driver_whorev0)/input$driver_sub0,1))
  output$driverrevsub1 <- renderText(round((input$driver_entrev1+input$driver_conrev0+input$driver_whorev1)/input$driver_sub1,1))
  output$driverrevsub2 <- renderText(round((input$driver_entrev2+input$driver_conrev0+input$driver_whorev2)/input$driver_sub2,1))
  output$driverrevsub3 <- renderText(round((input$driver_entrev3+input$driver_conrev0+input$driver_whorev3)/input$driver_sub3,1))
  
  output$driverrow1_per <- renderText(paste("<i>","Percentage change vs Y0:","</i>"))
  output$driver_entrev0_per <- renderText(paste0(round((input$driver_entrev0/input$driver_entrev0-1)*100,1),"%"))
  output$driver_entrev1_per <- renderText(paste0(round((input$driver_entrev1/input$driver_entrev0-1)*100,1),"%"))
  output$driver_entrev2_per <- renderText(paste0(round((input$driver_entrev2/input$driver_entrev0-1)*100,1),"%"))
  output$driver_entrev3_per <- renderText(paste0(round((input$driver_entrev3/input$driver_entrev0-1)*100,1),"%"))
  
  output$driverrow2_per <- renderText(paste("<i>","Percentage change vs Y0:","</i>"))
  output$driver_conrev0_per <- renderText(paste0(round((input$driver_conrev0/input$driver_conrev0-1)*100,1),"%"))
  output$driver_conrev1_per <- renderText(paste0(round((input$driver_conrev1/input$driver_conrev0-1)*100,1),"%"))
  output$driver_conrev2_per <- renderText(paste0(round((input$driver_conrev2/input$driver_conrev0-1)*100,1),"%"))
  output$driver_conrev3_per <- renderText(paste0(round((input$driver_conrev3/input$driver_conrev0-1)*100,1),"%"))
  
  output$driverrow3_per <- renderText(paste("<i>","Percentage change vs Y0:","</i>"))
  output$driver_whorev0_per <- renderText(paste0(round((input$driver_whorev0/input$driver_whorev0-1)*100,1),"%"))
  output$driver_whorev1_per <- renderText(paste0(round((input$driver_whorev1/input$driver_whorev0-1)*100,1),"%"))
  output$driver_whorev2_per <- renderText(paste0(round((input$driver_whorev2/input$driver_whorev0-1)*100,1),"%"))
  output$driver_whorev3_per <- renderText(paste0(round((input$driver_whorev3/input$driver_whorev0-1)*100,1),"%"))
  
  output$driverrow4_per <- renderText(paste("<i>","Percentage change vs Y0:","</i>"))
  output$driver_sub0_per <- renderText(paste0(round((input$driver_sub0/input$driver_sub0-1)*100,1),"%"))
  output$driver_sub1_per <- renderText(paste0(round((input$driver_sub1/input$driver_sub0-1)*100,1),"%"))
  output$driver_sub2_per <- renderText(paste0(round((input$driver_sub2/input$driver_sub0-1)*100,1),"%"))
  output$driver_sub3_per <- renderText(paste0(round((input$driver_sub3/input$driver_sub0-1)*100,1),"%"))
  
  output$driverrow4.5_totalrev <- renderText("Total Revenue:")
  output$totalrev0 <- renderText(paste0(round(input$driver_entrev0+input$driver_conrev0+input$driver_whorev0,0)))
  output$totalrev1 <- renderText(paste0(round(input$driver_entrev1+input$driver_conrev1+input$driver_whorev1,0)))
  output$totalrev2 <- renderText(paste0(round(input$driver_entrev2+input$driver_conrev2+input$driver_whorev2,0)))
  output$totalrev3 <- renderText(paste0(round(input$driver_entrev3+input$driver_conrev3+input$driver_whorev3,0)))
  
  output$driverrow5_per <- renderText(paste("<i>","Percentage change vs Y0:","</i>"))
  output$driverrevsub0_per <- renderText(paste0(round(
    round((input$driver_entrev0+input$driver_conrev0+input$driver_whorev0)/input$driver_sub0,1)/
      round((input$driver_entrev0+input$driver_conrev0+input$driver_whorev0)/input$driver_sub0,1)*100-100
  ,0),"%"))
  output$driverrevsub1_per <- renderText(paste0(round(
    round((input$driver_entrev1+input$driver_conrev1+input$driver_whorev1)/input$driver_sub1,1)/
      round((input$driver_entrev0+input$driver_conrev0+input$driver_whorev0)/input$driver_sub0,1)*100-100
  ,0),"%"))
  output$driverrevsub2_per <- renderText(paste0(round(
    round((input$driver_entrev2+input$driver_conrev2+input$driver_whorev2)/input$driver_sub2,1)/
      round((input$driver_entrev0+input$driver_conrev0+input$driver_whorev0)/input$driver_sub0,1)*100-100
  ,0),"%"))
  
  output$driverrevsub3_per <- renderText(paste0(round(
    round((input$driver_entrev3+input$driver_conrev3+input$driver_whorev3)/input$driver_sub3,1)/
      round((input$driver_entrev0+input$driver_conrev0+input$driver_whorev0)/input$driver_sub0,1)*100-100
  ,0),"%"))
  
  
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
    print("Calculating raw filtered setup start")
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
    print("Calculating raw filtered setup end")
    rvalues$rawdata_filtered = output
  })
  

# ...Selecting relevant columns based on scenario dropdown ---------------
  raw_filtered_setup_scenario <- reactive({
    print("Calculating raw filtered setup scenario start")
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    colnames = colnames(raw_filtered_setup())
    output = rvalues$rawdata_filtered %>%
      select(unique_identifier,l0,l1,l2,l3,l4,baseline_mid,grep(scenario,colnames))
    
    print("Calculating raw filtered setup scenario end")
    rvalues$rawdata_filtered = output
  })
  
  output$print_raw_filtered_setup_scenario <- renderTable(rvalues$rawdata_filtered)

# ... Producing filtered final table by selected L0/L1/L2 --------------------------
  final_filtered_setup <- reactive({
    print("Calculating final filtered setup start")
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
    print("Calculating final filtered setup end")
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
    print("Overwriting table start")
    
    # Setting output table to be equal to the current final data
    outputtable = rvalues$final_data

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
    print("Overwriting final data end")
  }
  
  #observeEvent(input$save,{f_overwrite_table()})

# ... Function to work out selection level ------------------------------------

  f_selection_level <- function(){
    print("Working out selection level start")
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
    print("Working out selection level end")
  }
  

# ... Function to pull supply values from filtered table -------------------------------
  # Call on raw_filtered_setup_scenario to get default values
  f_pull_supply <-  function(table) {
    print("Pulling supply values start")
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    columnhire_Y1 = paste("hire",scenario,"Y1",sep="_")
    columnexit_Y1 = paste("exit",scenario,"Y1",sep="_")
    columnhire_Y2 = paste("hire",scenario,"Y2",sep="_")
    columnexit_Y2 = paste("exit",scenario,"Y2",sep="_")
    columnhire_Y3 = paste("hire",scenario,"Y3",sep="_")
    columnexit_Y3 = paste("exit",scenario,"Y3",sep="_")
    
    columnhire_name_Y1 = as.name(columnhire_Y1)
    columnexit_name_Y1 = as.name(columnexit_Y1)
    columnhire_name_Y2 = as.name(columnhire_Y2)
    columnexit_name_Y2 = as.name(columnexit_Y2)
    columnhire_name_Y3 = as.name(columnhire_Y3)
    columnexit_name_Y3 = as.name(columnexit_Y3)
    
    level = f_selection_level()
    if(level=="Level All"){
      output = table %>%
        select(baseline_mid,!!columnhire_name_Y1,!!columnexit_name_Y1,!!columnhire_name_Y2,!!columnexit_name_Y2,!!columnhire_name_Y3,!!columnexit_name_Y3) %>%
        group_by()
    } else {
      if(level=="Level 0"){
        output = table %>%
          select(l0,baseline_mid,!!columnhire_name_Y1,!!columnexit_name_Y1,!!columnhire_name_Y2,!!columnexit_name_Y2,!!columnhire_name_Y3,!!columnexit_name_Y3) %>%
          group_by(l0)
      } else {
        if(level=="Level 1"){
          output = table %>%
            select(l1,baseline_mid,!!columnhire_name_Y1,!!columnexit_name_Y1,!!columnhire_name_Y2,!!columnexit_name_Y2,!!columnhire_name_Y3,!!columnexit_name_Y3) %>%
            group_by(l1)
        } else {
          if(level=="Level 2"){
            output = table %>%
              select(l2,baseline_mid,!!columnhire_name_Y1,!!columnexit_name_Y1,!!columnhire_name_Y2,!!columnexit_name_Y2,!!columnhire_name_Y3,!!columnexit_name_Y3) %>%
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
      summarise(sum(get(columnhire_Y1),na.rm=TRUE),sum(get(columnexit_Y1),na.rm=TRUE),
                sum(get(columnhire_Y2),na.rm=TRUE),sum(get(columnexit_Y2),na.rm=TRUE),
                sum(get(columnhire_Y3),na.rm=TRUE),sum(get(columnexit_Y3),na.rm=TRUE))

    if(level=="Level All"){
      output = output %>% 
        mutate("fillcol"="All") %>%
        select(fillcol,everything())
    }
    print("Pulling supply levels end")
    
    return(output)
  }
  
  # Function to set supply rates to default or final
  f_set_supply <- function(default_or_final){
    print("Set supply start")
    if(default_or_final=="default"){
      data = rvalues$rawdata_filtered
    } else {
      data = rvalues$final_data_filtered
    }
    table = f_pull_supply(data)
    print(paste0("Setting supply ",default_or_final))
    
    hirevalue_Y1 = as.numeric(table[2])
    exitvalue_Y1 = as.numeric(table[3])
    hirevalue_Y2 = as.numeric(table[4])
    exitvalue_Y2 = as.numeric(table[5])
    hirevalue_Y3 = as.numeric(table[6])
    exitvalue_Y3 = as.numeric(table[7])
    updateKnobInput(session=session,inputId="hire_rate_Y1",label=NULL,value=(hirevalue_Y1-1)*100)
    updateKnobInput(session=session,inputId="exit_rate_Y1",label=NULL,value=(1-exitvalue_Y1)*100)
    updateKnobInput(session=session,inputId="hire_rate_Y2",label=NULL,value=(hirevalue_Y2-1)*100)
    updateKnobInput(session=session,inputId="exit_rate_Y2",label=NULL,value=(1-exitvalue_Y2)*100)
    updateKnobInput(session=session,inputId="hire_rate_Y3",label=NULL,value=(hirevalue_Y3-1)*100)
    updateKnobInput(session=session,inputId="exit_rate_Y3",label=NULL,value=(1-exitvalue_Y3)*100)
    print("Set supply end")
  }
  

# ... Function to pull automation values from filtered table --------------

  f_pull_automation <-  function(table) {
    print("Pulling automation values start")
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    columnaut_Y1 = paste("aut",scenario,"Y1",sep="_")
    columnaut_Y2 = paste("aut",scenario,"Y2",sep="_")
    columnaut_Y3 = paste("aut",scenario,"Y3",sep="_")
    columnautq_Y1 = paste("aut",scenario,"Y1","q",sep="_")
    columnautq_Y2 = paste("aut",scenario,"Y2","q",sep="_")
    columnautq_Y3 = paste("aut",scenario,"Y3","q",sep="_")
    
    columnaut_name_Y1 = as.name(columnaut_Y1)
    columnaut_name_Y2 = as.name(columnaut_Y2)
    columnaut_name_Y3 = as.name(columnaut_Y3)
    columnautq_name_Y1 = as.name(columnautq_Y1)
    columnautq_name_Y2 = as.name(columnautq_Y2)
    columnautq_name_Y3 = as.name(columnautq_Y3)
    
    level = f_selection_level()
    if(level=="Level All"){
      output = table %>%
        select(baseline_mid,!!columnaut_name_Y1,!!columnaut_name_Y2,!!columnaut_name_Y3,!!columnautq_name_Y1,!!columnautq_name_Y2,!!columnautq_name_Y3) %>%
        group_by()
    } else {
      if(level=="Level 0"){
        output = table %>%
          select(l0,baseline_mid,!!columnaut_name_Y1,!!columnaut_name_Y2,!!columnaut_name_Y3,!!columnautq_name_Y1,!!columnautq_name_Y2,!!columnautq_name_Y3) %>%
          group_by(l0)
      } else {
        if(level=="Level 1"){
          output = table %>%
            select(l1,baseline_mid,!!columnaut_name_Y1,!!columnaut_name_Y2,!!columnaut_name_Y3,!!columnautq_name_Y1,!!columnautq_name_Y2,!!columnautq_name_Y3) %>%
            group_by(l1)
        } else {
          if(level=="Level 2"){
            output = table %>%
              select(l2,baseline_mid,!!columnaut_name_Y1,!!columnaut_name_Y2,!!columnaut_name_Y3,!!columnautq_name_Y1,!!columnautq_name_Y2,!!columnautq_name_Y3) %>%
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
      summarise(sum(get(columnaut_Y1)),
                sum(get(columnaut_Y2)),
                sum(get(columnaut_Y3)),
                sum(get(columnautq_Y1)),
                sum(get(columnautq_Y2)),
                sum(get(columnautq_Y3))
                )
    
    if(level=="Level All"){
      output = output %>% 
        mutate("fillcol"="All") %>%
        select(fillcol,everything())
    }
    print("Pulling automation values end")
    return(output)
  }
  
  # Function to set automation rates to default or final
  f_set_aut <- function(default_or_final){
    print("Set automation rates to default or final start")
    if(default_or_final=="default"){
      data = rvalues$rawdata_filtered
    } else {
      data = rvalues$final_data_filtered
    }
    table = f_pull_automation(data)

    autvalue_Y1 = as.numeric(table[2])
    autvalue_Y2 = as.numeric(table[3])
    autvalue_Y3 = as.numeric(table[4])
    autqvalue_Y1 = as.numeric(table[5])
    autqvalue_Y2 = as.numeric(table[6])
    autqvalue_Y3 = as.numeric(table[7])
    
    updateSliderInput(session=session,inputId="aut_slider_Y1",label=NULL,value=(1-autvalue_Y1)*100)
    updateSliderInput(session=session,inputId="aut_slider_Y2",label=NULL,value=(1-autvalue_Y2)*100)
    updateSliderInput(session=session,inputId="aut_slider_Y3",label=NULL,value=(1-autvalue_Y3)*100)
    updateRadioButtons(session=session,inputId = "aut_quarter_Y1",label=NULL,selected=paste0("Q",round(autqvalue_Y1)))
    updateRadioButtons(session=session,inputId = "aut_quarter_Y2",label=NULL,selected=paste0("Q",round(autqvalue_Y2)))
    updateRadioButtons(session=session,inputId = "aut_quarter_Y3",label=NULL,selected=paste0("Q",round(autqvalue_Y3)))
  
    print("Set automation rates to default or final end")
    }
  
  output$printer <- renderTable(f_pull_supply(rvalues$final_data_filtered))
  

# Function to pull productivity values from filtered table ----------------

  f_pull_productivity <-  function(table) {
    print("Pulling productivity values start")
    scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
    
    columnpro_Y0 = paste("pro",scenario,"Y0",sep="_")
    columnpro_Y1 = paste("pro",scenario,"Y1",sep="_")
    columnpro_Y2 = paste("pro",scenario,"Y2",sep="_")
    columnpro_Y3 = paste("pro",scenario,"Y3",sep="_")
    
    columnpro_name_Y0 = as.name(columnpro_Y0)
    columnpro_name_Y1 = as.name(columnpro_Y1)
    columnpro_name_Y2 = as.name(columnpro_Y2)
    columnpro_name_Y3 = as.name(columnpro_Y3)

    level = f_selection_level()
    if(level=="Level All"){
      output = table %>%
        select(baseline_mid,!!columnpro_name_Y0,!!columnpro_name_Y1,!!columnpro_name_Y2,!!columnpro_name_Y3) %>%
        group_by()
    } else {
      if(level=="Level 0"){
        output = table %>%
          select(l0,baseline_mid,!!columnpro_name_Y0,!!columnpro_name_Y1,!!columnpro_name_Y2,!!columnpro_name_Y3) %>%
          group_by(l0)
      } else {
        if(level=="Level 1"){
          output = table %>%
            select(l1,baseline_mid,!!columnpro_name_Y0,!!columnpro_name_Y1,!!columnpro_name_Y2,!!columnpro_name_Y3) %>%
            group_by(l1)
        } else {
          if(level=="Level 2"){
            output = table %>%
              select(l2,baseline_mid,baseline_mid,!!columnpro_name_Y0,!!columnpro_name_Y1,!!columnpro_name_Y2,!!columnpro_name_Y3) %>%
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
      summarise(sum(get(columnpro_Y0)),
                sum(get(columnpro_Y1)),
                sum(get(columnpro_Y2)),
                sum(get(columnpro_Y3))
      )
    
    if(level=="Level All"){
      output = output %>% 
        mutate("fillcol"="All") %>%
        select(fillcol,everything())
    }
    print("Pulling productivity values end")
    return(output)
  }
  
  # Function to set productivity rates to default or final
  f_set_pro <- function(default_or_final){
    print("Set productivity values default or final start")
    if(default_or_final=="default"){
      data = rvalues$rawdata_filtered
    } else {
      data = rvalues$final_data_filtered
    }
    table = f_pull_productivity(data)
    
    provalue_Y0 = as.numeric(table[2])
    provalue_Y1 = as.numeric(table[3])
    provalue_Y2 = as.numeric(table[4])
    provalue_Y3 = as.numeric(table[5])

    updateNumericInput(session=session,inputId="pro_input_Y0",label=NULL,value=provalue_Y0)
    updateNumericInput(session=session,inputId="pro_input_Y1",label=NULL,value=provalue_Y1)
    updateNumericInput(session=session,inputId="pro_input_Y2",label=NULL,value=provalue_Y2)
    updateNumericInput(session=session,inputId="pro_input_Y3",label=NULL,value=provalue_Y3)
    print("Set productivity values default or final end")
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
  
  # f_pull_outsourcing <-  function(table) {
  #   print("Pulling outsourcing consideration status")
  # 
  #   scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
  #   
  #   columnout_considered = paste("out_considered",scenario,sep="_")
  #   columnout_year = paste("out_year",scenario,sep="_")
  #   columnout_quarter = paste("out_quarter",scenario,sep="_")
  #   
  #   columnout_considered_name = as.name(columnout_considered)
  #   columnout_year_name = as.name(columnout_year)
  #   columnout_quarter_name = as.name(columnout_quarter)
  #   
  #   level = f_selection_level()
  #   if(level=="Level All"){
  #     output = table %>%
  #       select(baseline_mid,!!columnout_considered_name,!!columnout_year_name,!!columnout_quarter_name) %>%
  #       group_by()
  #   } else {
  #     if(level=="Level 0"){
  #       output = table %>%
  #         select(l0,baseline_mid,!!columnout_considered_name,!!columnout_year_name,!!columnout_quarter_name) %>%
  #         group_by(l0)
  #     } else {
  #       if(level=="Level 1"){
  #         output = table %>%
  #           select(l1,baseline_mid,!!columnout_considered_name,!!columnout_year_name,!!columnout_quarter_name) %>%
  #           group_by(l1)
  #       } else {
  #         if(level=="Level 2"){
  #           output = table %>%
  #             select(l2,baseline_mid,!!columnout_considered_name,!!columnout_year_name,!!columnout_quarter_name) %>%
  #             group_by(l2)
  #         }
  #       }
  #     }
  #   }
  #   
  #   # Calculating total headcount of selected
  #   totalhc = output %>%
  #     summarise("baseline_mid"=sum(baseline_mid))
  #   totalhc = as.numeric(totalhc$baseline_mid)
  #   
  #   # Calculating total headcount of selected which can be outsourced
  #   out_year_name <- as.name(paste("out_year",scenario,sep="_"))
  #   totalhc_out = output %>%
  #     filter(is.na(!!out_year_name)==FALSE)
  #   if(nrow(totalhc_out)==0){
  #     totalhc_out=0
  #   } else {
  #     totalhc_out <- totalhc_out %>%
  #       summarise("baseline_mid"=sum(baseline_mid))
  #     totalhc_out = as.numeric(totalhc_out$baseline_mid)
  #   }
  #   
  #   # Considered for outsourcing = 1, not considered = 0
  #   # Not to be confused with out_yesno which is if it can be outsourced
  #   outputcol_outcon <- paste("out_considered",scenario,sep="_")
  #   inputcol_outcon <- as.name(outputcol_outcon)
  #   
  #   # outyesno not considered here as can't be edited by user
  #   
  #   outputcol_outyear = paste("out_year",scenario,sep="_")
  #   inputcol_outyear = as.name(outputcol_outyear)
  #   
  #   outputcol_outquarter = paste("out_quarter",scenario,sep="_")
  #   inputcol_outquarter = as.name(outputcol_outquarter)
  #   
  #   # Weighting every factor by the section's proportion of the whole
  #   
  #   output = output %>%
  #     # outcon here decides if whole function should be considered
  #     mutate(!!outputcol_outcon := !!inputcol_outcon/totalhc*baseline_mid) %>%
  #     
  #     # outyear here decides what year, uses headcount only of those with years (totalhc_out)
  #     mutate(!!outputcol_outyear := !!inputcol_outyear/totalhc_out*baseline_mid) %>%
  #     mutate(!!outputcol_outquarter := !!inputcol_outquarter/totalhc_out*baseline_mid)
  #   
  #   # Adding up these to get the weighted average
  #   output = output %>%
  #     summarise(sum(get(outputcol_outcon),na.rm = TRUE),
  #               sum(get(outputcol_outyear),na.rm = TRUE),
  #               sum(get(outputcol_outquarter),na.rm = TRUE)
  #     )
  #   
  #   if(level=="Level All"){
  #     output = output %>% 
  #       mutate("fillcol"="All") %>%
  #       select(fillcol,everything())
  #   }
  #   
  #   return(output)
  #   
  # }
  # 
  # f_set_out <- function(default_or_final){
  #   if(default_or_final=="default"){
  #     data = rvalues$rawdata_filtered
  #   } else {
  #     data = rvalues$final_data_filtered
  #   }
  #   table = f_pull_outsourcing(data)
  #   print(paste0("Setting outsourcing consideration ",default_or_final))
  #   
  #   outcon = ifelse(round(as.numeric(table[2]),0)==0,FALSE,TRUE)
  #   outyear = paste0("Y",round(as.numeric(table[3]),0))
  #   outquarter = paste0("q",round(as.numeric(table[4]),0))
  #   # Not currently calculating year right - should ignore blanks - or is it because non outsourced
  #   
  #   updateCheckboxInput(session=session,inputId="out_considered",value=outcon)
  #   updateSelectInput(session=session,inputId="out_year",selected=outyear)
  #   updateSelectInput(session=session,inputId="out_quarter",selected=outquarter)
  # }
  

# Function to pull baseline positioning -----------------------------------

  f_pull_baseline <-  function(table) {
    print("Pulling baseline values start")
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
    print("Pulling baseline values end")
    return(output)
  }
  
  
  f_set_baseline <- function(default_or_final){
    print("Setting baseline default or final start")
    if(default_or_final=="default"){
      data = rvalues$rawdata
    } else {
      data = rvalues$final_data
    }
    table = f_pull_baseline(data)

    baselinevalue = as.numeric(table[1])*100

    updateSliderInput(session=session,inputId="baselinepos",value=baselinevalue)
    print("Setting baseline default or final end")
  }
  
  
  # Function to set demand driver figures to default or final -----
  f_set_dd <- function(default_or_final){
    print("Setting demand drivers default or final start")
    if(default_or_final=="default"){
      data = rvalues$rvalues$dd_raw
    } else {
      data = rvalues$rvalues$dd_final
    }
    print("Setting demand drivers default or final end")
  }
  
  #... Observe events fill temporary table -------------------------------------
    
  observeEvent(input$l2_list_indep,{
    print("Observing L2 selection")
    if(is.null(input$l2_list_indep)==FALSE){
      rvalues$final_data
      final_filtered_setup()
    }
  })
  
  rvalues$setup_loaded_once <- 0
  
  observeEvent(input$l2_list_indep,{
    if(rvalues$setup_loaded_once==0){
      final_filtered_setup()
      f_set_supply("final")
      f_set_aut("final")
      f_set_pro("final")
      rvalues$setup_loaded_once <- 1
    }
  })
  
  observeEvent(input$generate,{
    final_filtered_setup()
    f_set_supply("final")
    f_set_aut("final")
    f_set_pro("final")
    #f_set_out("final")
    print("")
  })
  
  observeEvent(input$reset_all,{
    f_reset_all()
    # Running generate functions to reload new reset values
    final_filtered_setup()
    f_set_supply("default")
    f_set_aut("default")
    f_set_pro("default")
    #f_set_out("default")
    })
  
  observeEvent(input$reset_select,{
    f_reset_selected()
    # Running generate functions to reload new reset values
    final_filtered_setup()
    f_set_supply("default")
    f_set_aut("default")
    f_set_pro("default")
    #f_set_out("default")
  })
  

# ... Pull driver values ------------------------------
  
f_pull_dd <- function(table){
  print("Pulling demand drivers start")
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
    print("Pulling demand drivers end")
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
  print("Observing event: save demand drivers start")
    table <- rvalues$dd_final
    data_drivermults_0s <- table %>% filter(year==0) %>% select(-year)
    colnames(data_drivermults_0s) <- paste0(colnames(data_drivermults_0s),"_0")
    data_drivermults <- left_join(table,data_drivermults_0s,by=c("driver"="driver_0"))
    data_drivermults <- data_drivermults %>%
      mutate(min=min/min_0) %>%
      mutate(mid=mid/mid_0) %>%
      mutate(max=max/max_0) %>%
      select(-min_0,-mid_0,-max_0)

    hctable <- left_join(select(rvalues$final_data,unique_identifier,baseline_mid),data_driverhc,by="unique_identifier")
    hctable <- hctable %>%
      select(-unique_identifier) %>%
      group_by(driver_group) %>%
      summarise(sum(baseline_mid))
    colnames(hctable) = c("driver","hc")
    hcmult <- left_join(data_drivermults,hctable,by="driver") %>%
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

    print("data_drivermults_all")
    data_drivermults_all <- left_join(data_drivermults_min,data_drivermults_mid,by="driver")
    data_drivermults_all <- left_join(data_drivermults_all,data_drivermults_max,by="driver")
    colnames <- colnames(data_drivermults_all[2:ncol(data_drivermults_all)])

    print("final_data")
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
    print("Observing event: save demand drivers end")
  },priority=100)

#... Actions when save button clicked ----------------------------------------

  # Loading temporary data table into memory
  observeEvent(input$save,{
    temporary_data_filtered_scenario()
  },priority=300)
  
  # Setting hire values in temporary table when save clicked
  observeEvent(input$save,{
    withProgress(message="Saving parameter adjustments",value=0,{
      hirevalue_Y1 = input$hire_rate_Y1
      hirevalue_Y2 = input$hire_rate_Y2
      hirevalue_Y3 = input$hire_rate_Y3
      scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
      outputcol_Y1 = paste("hire",scenario,"Y1",sep="_")
      outputcol_Y2 = paste("hire",scenario,"Y2",sep="_")
      outputcol_Y3 = paste("hire",scenario,"Y3",sep="_")
      table = rvalues$temporary_table
  
      table = table %>%
        mutate(!!outputcol_Y1 := (100+!!hirevalue_Y1)/100) %>%
        mutate(!!outputcol_Y2 := (100+!!hirevalue_Y2)/100) %>%
        mutate(!!outputcol_Y3 := (100+!!hirevalue_Y3)/100)
      rvalues$temporary_table = table
    })
  },priority=200)

  # Setting exit values in temporary table when save clicked
  observeEvent(input$save,{
    withProgress(message="Saving parameter adjustments",value=0.2,{
      exitvalue_Y1 = input$exit_rate_Y1
      exitvalue_Y2 = input$exit_rate_Y2
      exitvalue_Y3 = input$exit_rate_Y3
      scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
      outputcol_Y1 = paste("exit",scenario,"Y1",sep="_")
      outputcol_Y2 = paste("exit",scenario,"Y2",sep="_")
      outputcol_Y3 = paste("exit",scenario,"Y3",sep="_")
      table = rvalues$temporary_table
      
      table = table %>%
        mutate(!!outputcol_Y1 := (100-!!exitvalue_Y1)/100) %>%
        mutate(!!outputcol_Y2 := (100-!!exitvalue_Y2)/100) %>%
        mutate(!!outputcol_Y3 := (100-!!exitvalue_Y3)/100)
      rvalues$temporary_table = table
      })
  },priority=200)
  
  # Setting automation in temporary table when save clicked
  observeEvent(input$save,{
    withProgress(message="Saving parameter adjustments",value=0.4,{
      autvalue_Y1 = input$aut_slider_Y1
      autvalue_Y2 = input$aut_slider_Y2
      autvalue_Y3 = input$aut_slider_Y3
      autqvalue_Y1 = as.numeric(substr(input$aut_quarter_Y1,nchar(input$aut_quarter_Y1),nchar(input$aut_quarter_Y1)))
      autqvalue_Y2 = as.numeric(substr(input$aut_quarter_Y2,nchar(input$aut_quarter_Y2),nchar(input$aut_quarter_Y2)))
      autqvalue_Y3 = as.numeric(substr(input$aut_quarter_Y3,nchar(input$aut_quarter_Y3),nchar(input$aut_quarter_Y3)))
      
      scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
      
      outputcol_Y1 = paste("aut",scenario,"Y1",sep="_")
      outputcol_Y2 = paste("aut",scenario,"Y2",sep="_")
      outputcol_Y3 = paste("aut",scenario,"Y3",sep="_")
      outputcolq_Y1 = paste("aut",scenario,"Y1","q",sep="_")
      outputcolq_Y2 = paste("aut",scenario,"Y2","q",sep="_")
      outputcolq_Y3 = paste("aut",scenario,"Y3","q",sep="_")
      
      table = rvalues$temporary_table
      
      table = table %>%
        mutate(!!outputcol_Y1 := (100-!!autvalue_Y1)/100) %>%
        mutate(!!outputcol_Y2 := (100-!!autvalue_Y2)/100) %>%
        mutate(!!outputcol_Y3 := (100-!!autvalue_Y3)/100) %>%
        mutate(!!outputcolq_Y1 := !!autqvalue_Y1) %>%
        mutate(!!outputcolq_Y2 := !!autqvalue_Y2) %>%
        mutate(!!outputcolq_Y3 := !!autqvalue_Y3)
      rvalues$temporary_table = table
    })

  },priority=200)
  
  # Setting productivity in temporary table when save clicked
  observeEvent(input$save,{
    withProgress(message="Saving parameter adjustments",value=0.6,{
      provalue_Y0 = input$pro_input_Y0
      provalue_Y1 = input$pro_input_Y1
      provalue_Y2 = input$pro_input_Y2
      provalue_Y3 = input$pro_input_Y3
      
      scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
      
      outputcol_Y0 = paste("pro",scenario,"Y0",sep="_")
      outputcol_Y1 = paste("pro",scenario,"Y1",sep="_")
      outputcol_Y2 = paste("pro",scenario,"Y2",sep="_")
      outputcol_Y3 = paste("pro",scenario,"Y3",sep="_")
      
      table = rvalues$temporary_table
      
      table = table %>%
        mutate(!!outputcol_Y0 := !!provalue_Y0) %>%
        mutate(!!outputcol_Y1 := !!provalue_Y1) %>%
        mutate(!!outputcol_Y2 := !!provalue_Y2) %>%
        mutate(!!outputcol_Y3 := !!provalue_Y3)
  
      rvalues$temporary_table = table
    })
  },priority=200)
  
  # Setting outsourcing in temporary table when save clicked
  # observeEvent(input$save,{
  #   withProgress(message="Saving parameter adjustments",value=0.8,{
  #     out_considered = ifelse(input$out_considered==TRUE,1,0)
  #     out_year = as.numeric(substr(input$out_year,nchar(input$out_year),nchar(input$out_year)))
  #     out_quarter = as.numeric(substr(input$out_quarter,nchar(input$out_quarter),nchar(input$out_quarter)))
  #     
  #     scenario = ifelse(input$setup_scenario=="Conservative","min",ifelse(input$setup_scenario=="Realistic","mid","max"))
  #     
  #     outputcol_considered = paste("out_considered",scenario,sep="_")
  #     outputcol_year = paste("out_year",scenario,sep="_")
  #     outputcol_quarter = paste("out_quarter",scenario,sep="_")
  #     
  #     table = rvalues$temporary_table
  #     
  #     table = table %>%
  #       mutate(!!outputcol_considered := out_considered) %>%
  #       mutate(!!outputcol_year := out_year) %>%
  #       mutate(!!outputcol_quarter := out_quarter)
  #     
  #     rvalues$temporary_table = table
  #   })
  # },priority=200)
  
  # Overwriting data when save clicked
  observeEvent(input$save,{
    withProgress(message="Saving parameter adjustments",value=0.9,{
      f_overwrite_table()
      print("Overwritten table")
    })
  }
  ,priority = 100)
  
    # Setting dropdowns to all when one changed
    observeEvent(input$l0_list_indep,{
      updateSelectInput(session=session,"l1_list_indep",selected = "All")
      updateSelectInput(session=session,"l2_list_indep",selected = "All")
    })
    
    observeEvent(input$l1_list_indep,{
      updateSelectInput(session=session,"l2_list_indep",selected = "All")
    },priority=100)
    
    # For the first time it is generated, to preload the values
    observeEvent(input$l2_list_indep,{
      if(rvalues$loaded == 0){
        f_set_supply("default")
        f_set_aut("default")
        f_set_pro("default")
        #f_set_out("default")
      }
      rvalues$loaded <- 1
  },priority=50)


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
    #f_set_out("final")
  }
  #final_filtered_setup()
  
},priority=0)
  
  

# EXECUTIVE DASHBOARD CALCULATIONS ----------------------------------------


# Dropdown creation -------------------------------------------------------

  # ... Producing list of L1s based off selected L0 -----------------------------
  l1_list_exec <- reactive({

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
    pickerInput(inputId = "l0_list_exec",label = "Unit:",
                choices = c(output_l0_list),multiple=TRUE,selected=apply(output_l0_list,2,as.character),
                options=list(`actions-box`=TRUE))
  })
  
  # Producing L1 list dropdown
  output$l1_list_exec <- renderUI ({
    pickerInput(inputId = "l1_list_exec",label = "Sector:",
                choices = c(l1_list_exec()),multiple=TRUE,selected=c(l1_list_exec()),
                options=list(`actions-box`=TRUE))
  })
  
  # Producing L2 list dropdown
  output$l2_list_exec <- renderUI ({
    pickerInput(inputId = "l2_list_exec",label = "General Directorate:",
                choices = c(l2_list_exec()),multiple=TRUE,selected=c(l2_list_exec()),
                options=list(`actions-box`=TRUE))
  })  


# Filtering output table based on selections ------------------------------

  observeEvent({
    # input$l2_list_exec
    # input$segment_list_exec
    input$exec_refresh_graphs
    },{
    rvalues$final_data_exec = rvalues$final_data %>%
      filter(l0 %in% input$l0_list_exec & l1 %in% input$l1_list_exec & l2 %in% input$l2_list_exec,segment %in% input$segment_list_exec)
    exec_table_output()

  })
  

# Turning final table into final headcount table -----------------------

final_headcount_table <- reactive({
  
  print("Final headcount table creation start")
  
  withProgress(message="Generating data",value=0,{
    
      data_import = rvalues$final_data
      cols_all <- colnames(data_import)
    
      #...Calculating Supply Percentages ------------------------------------
      incProgress(0.1,"Calculating supply impact")
      # We have hire_min_Y1 to hire_max_Y3, and exit_min_Y1 to exit_max_Y1
      
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
        inputcol_Y1 = as.name(paste("supply",x,"Y1",sep="_"))
        inputcol_Y2 = as.name(paste("supply",x,"Y2",sep="_"))
        inputcol_Y3 = as.name(paste("supply",x,"Y3",sep="_"))
        for(y in c("Y0","Y1","Y2","Y3")){
          outputcol = paste("supply",x,y,"cumu",sep="_")
          data_supply_cumulative <- data_supply_cumulative %>%
            mutate(!!outputcol := ifelse(!!y=="Y0",1,
                                         ifelse(!!y=="Y1",1*!!inputcol_Y1,
                                                ifelse(!!y=="Y2",1*!!inputcol_Y1*!!inputcol_Y2,
                                                       1*!!inputcol_Y1*!!inputcol_Y2*!!inputcol_Y3))))
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
      #write_csv(data_supply_cumulative_qs,"Quarterly supply.csv")
      
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
        # inputcol_Y0 = as.name(paste("pro",x,"Y0",sep="_"))
        inputcol_Y1 = as.name(paste("pro",x,"Y1",sep="_"))
        inputcol_Y2 = as.name(paste("pro",x,"Y2",sep="_"))
        inputcol_Y3 = as.name(paste("pro",x,"Y3",sep="_"))
        for(y in c("Y0","Y1","Y2","Y3")){
          outputcol = paste("pro",x,y,"cumu",sep="_")
          data_pro_cumulative <- data_pro_cumulative %>%
            mutate(!!outputcol := ifelse(!!y=="Y0",1,
                                         ifelse(!!y=="Y1",1*!!inputcol_Y1,
                                                ifelse(!!y=="Y2",1*!!inputcol_Y1*!!inputcol_Y2,
                                                       1*!!inputcol_Y1*!!inputcol_Y2*!!inputcol_Y3))))
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
      # data_out <- select(data_import,unique_identifier,grep("out_",cols_all))
      # 
      # #incProgress(0.1,"Calculating outsourcing impact")
      # 
      # # Turning it into quarters
      # data_out_qs <- data_out
      # 
      # for(x in c("min","mid","max")){
      #   for(y in c("Y1","Y2","Y3")){
      #     for(z in c("q1","q2","q3","q4")){
      #       year = as.numeric(substr(y,nchar(y),nchar(y)))
      #       quarter = as.numeric(substr(z,nchar(z),nchar(z)))
      #       outcol = as.name(paste("out",x,y,sep="_"))
      #       
      #       input_considered = as.name(paste("out_considered",x,sep="_"))
      #       input_yesno = as.name("out_yesno")
      #       input_yearcol = as.name(paste("out_year",x,sep="_"))
      #       input_quartercol = as.name(paste("out_quarter",x,sep="_"))
      #       
      #       outputcol = paste("out",x,y,z,sep="_")
      #       
      #       data_out_qs = data_out_qs %>%
      #         rowwise() %>%
      #         mutate(!!outputcol:=ifelse(!!input_yesno == 0 | !!input_considered==0,1,
      #                                    ifelse(year<!!input_yearcol,1,
      #                                           ifelse(year>!!input_yearcol,0,
      #                                                  ifelse(quarter<!!input_quartercol,1,0)
      #                                           )
      #                                    )
      #         ))
      #     }
      #   }
      # }
      # 
      # incProgress(0.1,"Calculating cumulative impact")
      
      # ...Multiplying automation, productivity, demand and outsourcing together --------
      
      data_aut_pro_dd_out <- left_join(data_aut_qs,data_pro_cumulative_qs,by="unique_identifier")
      data_aut_pro_dd_out <- left_join(data_aut_pro_dd_out,data_dd_qs,by="unique_identifier")
      #data_aut_pro_dd_out <- left_join(data_aut_pro_dd_out,data_out_qs,by="unique_identifier")
      colnames <- colnames(data_aut_pro_dd_out)
      colnames(data_aut_pro_dd_out) <- gsub("_cumu","",colnames)
      
      for(x in c("min","mid","max")){
        for(y in c("Y1","Y2","Y3")){
          for(z in c("q1","q2","q3","q4")) {
            input_aut = as.name(paste("aut",x,y,z,sep="_"))
            input_pro = as.name(paste("pro",x,y,z,sep="_"))
            input_dd = as.name(paste("dd",x,y,z,sep="_"))
            #input_out = as.name(paste("out",x,y,z,sep="_"))
            
            outputcol = paste("finalmult",x,y,z,sep="_")
            
            data_aut_pro_dd_out <- data_aut_pro_dd_out %>%
              rowwise() %>%
              mutate(!!outputcol := !!input_aut * !!input_pro * !!input_dd)
                     #*!!input_out)
          }
        }
      }
      
      # ...Pulling in baseline numbers ---------------------------------------------
      
      incProgress(0.2,"Calculating total headcount impact")
      
      # Bringing out baseline columns
      data_baseline <- select(data_import,unique_identifier,grep("baseline",cols_all))
      
      data_baseline <- data_baseline %>% 
        select(-baseline_mid) %>%
        mutate("baseline_min"=ceiling(baseline_top-(baseline_top-baseline_bottom)*baselinepos_min)) %>%
        mutate("baseline_mid"=ceiling(baseline_top-(baseline_top-baseline_bottom)*baselinepos_mid)) %>%
        mutate("baseline_max"=ceiling(baseline_top-(baseline_top-baseline_bottom)*baselinepos_max))
      
      #...Turning supply into headcount impacts -----------------------------------
      
      final_supply_cols <- colnames(select(data_supply_cumulative_qs,-unique_identifier))
      
      final_supply <- left_join(select(data_hc,unique_identifier,hc_chosen),data_supply_cumulative_qs,by="unique_identifier")
      #write_csv(final_supply,"Final supply")
      for(x in final_supply_cols){
        inputcol = as.name(x)
        
        scenario = str_split(x,"_")[[1]][2]
        inputheadcount = as.name("hc_chosen")
        
        final_supply <- final_supply %>%
          rowwise() %>%
          mutate(!!x := ceiling(!!inputheadcount * !!inputcol))
      }
      #write_csv(final_supply,"FinalHC supply")
      
      # ...Turning multipliers into headcount impacts ------------------------------
      
      multcols <- colnames(data_aut_pro_dd_out)
      finalcols <- colnames(select(data_aut_pro_dd_out,unique_identifier,grep("_q1",multcols),grep("_q2",multcols),grep("_q3",multcols),grep("_q4",multcols)))
      data_aut_pro_dd_out <- data_aut_pro_dd_out %>% select(finalcols)
      
      data_baseline_aut_pro_dd_out <- left_join(data_baseline,data_aut_pro_dd_out,by="unique_identifier")
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
      incProgress(0.2,"Calculating cost impact")
      
      # Calculating final cost values
      final_cost <- left_join(data_sectioncost,final_hc,by="unique_identifier")
      final_cost <- final_cost %>%
        select(-hc_chosen,-baseline_bottom,-baseline_top,-grep("baselinepos",colnames(final_cost)))

      costcols <- colnames(select(final_cost,-unique_identifier,-fte_cost_avg,-l0,-l1,-l2,-l3,-l4,-level,-segment))
      for(x in costcols){
        xname <- as.name(x)

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
  }
  #,style="old"
  )
  print("Final headcount table creation end")
})


# Filtering based on exec selection ---------------------------------------

  exec_table_output <- reactive({
    
    input$exec_refresh_graphs
    input$deep_refresh_graphs

    if(input$tabs == "tab_executive"){
      if(isolate(input$headcount_or_cost) == "Headcount"){
        table <- isolate(rvalues$final_hc)
      } else {
        table <- isolate(rvalues$final_cost)
      }
    } else {
      if(isolate(input$deep_headcount_or_cost) == "Headcount"){
        table <- isolate(rvalues$final_hc)
      } else {
        table <- isolate(rvalues$final_cost)
      }
    }

    if(input$tabs == "tab_executive"){
      table <- table %>% 
        filter(l0 %in% isolate(input$l0_list_exec)) %>%
        filter(l1 %in% isolate(input$l1_list_exec)) %>%
        filter(l2 %in% isolate(input$l2_list_exec)) %>%
        filter(segment %in% isolate(input$segment_list_exec))
    } else {
      table <- table %>% 
        filter(if(isolate(input$deep_chosenl0)=="All") l0 != "abc" else l0 == isolate(input$deep_chosenl0)) %>%
        filter(if(isolate(input$deep_chosenl1)=="All") l1 != "abc" else l1 == isolate(input$deep_chosenl1)) %>%
        filter(if(isolate(input$deep_chosenl2)=="All") l2 != "abc" else l2 == isolate(input$deep_chosenl2)) %>%
        filter(if(isolate(input$deep_chosenl3)=="All") l3 != "abc" else l3 == isolate(input$deep_chosenl3)) %>%
        filter(if(isolate(input$deep_chosenl4)=="All") l4 != "abc" else l4 == isolate(input$deep_chosenl4)) %>%
        filter(segment %in% isolate(input$segment_list_deep))
    }
    
    rvalues$exec_table_output <- table
  })
  
# ...Calculating changes for line --------------------------------------------

final_line_table <- reactive({
  print("Final line table calculation start")
  exec_table_output()
  input$exec_refresh_graphs
  input$deep_refresh_graphs
  
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
    mutate("Scenario" = paste(supplyordemand,scenario,sep="_")) %>%
    select(-Key) %>%
    mutate("total_quarter" = (year*4)+quarter-4) %>%
    mutate("Value"=ceiling(Value)) %>%
    mutate("Year & Quarter" = paste0("Y",year," ","Q",quarter))
  
  final_hc2$Scenario <- gsub("finalmult_min","Demand - Conservative",final_hc2$Scenario)
  final_hc2$Scenario <- gsub("finalmult_mid","Demand - Realistic",final_hc2$Scenario)
  final_hc2$Scenario <- gsub("finalmult_max","Demand - Optimistic",final_hc2$Scenario)
  final_hc2$Scenario <- gsub("supply_min","Supply - Conservative",final_hc2$Scenario)
  final_hc2$Scenario <- gsub("supply_mid","Supply - Realistic",final_hc2$Scenario)
  final_hc2$Scenario <- gsub("supply_max","Supply - Optimistic",final_hc2$Scenario)
  
  if(input$tabs == "tab_executive"){
    final_hc2 <- final_hc2 %>%
      rowwise() %>%
      mutate("Total" = ifelse(isolate(input$headcount_or_cost)=="Headcount",
                              round(Value,0),
                              ifelse(Value>1000000,paste0(format(round(Value / 1e6, 1), trim = TRUE), "Mn"),
                                     paste0(format(round(Value / 1e3, 1), trim = TRUE), "K")
                              )))
  } else {
    final_hc2 <- final_hc2 %>%
      rowwise() %>%
      mutate("Total" = ifelse(isolate(input$deep_headcount_or_cost)=="Headcount",
                              round(Value,0),
                              ifelse(Value>1000000,paste0(format(round(Value / 1e6, 1), trim = TRUE), "Mn"),
                                     paste0(format(round(Value / 1e3, 1), trim = TRUE), "K")
                              )))
  }

  
  print("Final line table calculation end")
  return(final_hc2)
})  
  

# ...Line plot ---------------------------------------------------------------
  output$line <- renderPlotly({
    print("Final line graph calculation start")
    
    input$exec_refresh_graphs
    input$deep_refresh_graphs
    
    if(rvalues$generated == 0){
      text = paste("\n   Please generate data in the model initialisation tab \n",
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
    
    linegraph <- ggplot(final_hc2,aes(x=total_quarter,y=Value,colour=Scenario,label=Total,text=`Year & Quarter`,group=supplyordemand)) +
      geom_line(size=1)+
      CustomTheme+
      #ggtitle("Headcount Forecast")+
      xlab("Year")+
      labs(x="Quarter (End)",y=ifelse(isolate(input$headcount_or_cost)=="Headcount","Headcount","Monthly Cost (SAR)"))+
      theme(legend.position = "bottom")+
      scale_colour_manual(values=c("Demand - Conservative" = "#002C77","Demand - Realistic" = "#ED2C67","Demand - Optimistic" = "#00A8C8","Supply - Conservative" = "#0FB694","Supply - Realistic"="#FBAE17","Supply - Optimistic"="#72BE44"))+
      scale_x_continuous(breaks=1:12,labels = paste0(c("Year 1 Q1","","","Year 1 Q4","","","","Year 2 Q4","","","","Year 3 Q4"))) +
      scale_y_continuous(labels=comma)
      #scale_y_continuous(breaks=pretty_breaks(n=10),expand=c(0,0),labels=comma)
      #geom_text(aes(label=ifelse(Value==0,"",Value)),direction=c("y"),size=3,segment.alpha=1,label.r=0.2,box.padding=0.1,label.padding=0.05,alpha=0.8,seed=5,label.size=NA)
      # geom_label_repel(aes(label=ifelse(Value==0,"",Value)),direction=c("y"),size=3,segment.alpha=1,label.r=0.2,box.padding=0.1,label.padding=0.05,alpha=0.8,seed=5,label.size=NA)+
      # geom_label_repel(aes(label=ifelse(Value==0,"",Value)),direction=c("y"),size=3,segment.alpha=1,label.r=0.2,box.padding=0.1,label.padding=0.05,fill=NA,alpha=1,seed = 5,label.size=NA)

    linegraph_ly <- ggplotly(linegraph,tooltip=c("Scenario","Year & Quarter","Total"),dynamicTicks=TRUE) %>% 
      config(displaylogo=FALSE,collaborate=FALSE,modeBarButtonsToRemove=c(
        'sendDataToCloud',
        'toImage',
        'toggleSpikelines')) %>%
      layout(legend = list(orientation="h",y=-0.3),font=list(family="Source Sans Pro",size=18))
    
    #%>% layout(legend = list(x = 0.01, y = 0.1),orientation="h")
    print("Final line table calculation end")
    linegraph_ly
  })
  

# ...Calculating changes for bar ---------------------------------------------

  # Calculating figures for bar charts
  final_barchart_table <- reactive({
    print("Final barchart table calculation start")
    input$exec_refresh_graphs
    
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
    final_hc3 <- final_hc3 %>%
      rowwise() %>%
      mutate(label = ifelse(isolate(input$headcount_or_cost)=="Headcount",
                            round(Value,0),
                            ifelse(Value>1000000,paste0(format(round(Value / 1e6, 1), trim = TRUE), "m"),
                                   paste0(format(round(Value / 1e3, 1), trim = TRUE), "k")
                            )))
    return(final_hc3)
    print("Final barchart table calculation end")
  })
  

# ...Bar plot -------------------------------------------------------------

  output$barchart <- renderPlot({
    print("Final barchart plot start")
    input$exec_refresh_graphs
    
    if(rvalues$generated == 0){
      return(NULL)
    }
    # Creating bar charts
    data = final_barchart_table()
    bargraph <- ggplot(data,aes(x=year,y=Value))+
      geom_bar(aes(fill=scenario),stat="identity",position="dodge")+
      CustomTheme + 
      theme(legend.position = "bottom",
            text = element_text(size=16),
            axis.text.y = element_text(size=14)) +
      scale_fill_manual("Key",labels=c("min"="Conservative","mid"="Realistic","max"="Optimistic"),values=c("min" = "steelblue","mid" = "steelblue3","max" = "steelblue1"))+
      scale_y_continuous(labels=comma,limits=c(0,max(data$Value)*1.1))+
      labs(x="Year",y=ifelse(isolate(input$headcount_or_cost)=="Headcount","Headcount","Monthly Cost (SAR)"))+
      geom_text(colour="black",aes(fontface=2,label=label,fill=scenario),position=position_dodge(width = 0.9),vjust=-1)
    
    print("Final barchart plot end")
    bargraph
  })
  

  # Calculating changes for supply v demand bar -----------------------------
  
  # Calculating figures for supplydemandbar charts
  final_supplydemandbar_table <- reactive({
    print("Final supplydemand table start")
    input$exec_refresh_graphs
    
    final_hc = rvalues$exec_table_output
    scenario = ifelse(isolate(input$exec_scenario)=="Conservative","min",ifelse(isolate(isolate(input$exec_scenario))=="Realistic","mid","max"))
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
    final_hc <- final_hc %>%
      rowwise() %>%
      mutate(label = ifelse(isolate(input$headcount_or_cost)=="Headcount",
                            round(Value,0),
                            ifelse(Value>1000000,paste0(format(round(Value / 1e6, 1), trim = TRUE), "m"),
                                   paste0(format(round(value / 1e3, 1), trim = TRUE), "k")
                            )))
    print("Final supplydemand table end")
    return(final_hc)
  })
  
  
  # ...Supply Demand Bar plot -------------------------------------------------------------
  
  output$supplydemandbarchart <- renderPlot({
    print("Generating supply demand bar chart start")
    input$exec_refresh_graphs
    
    if(rvalues$generated == 0){
      return(NULL)
    }
    # Creating bar charts
    data = final_supplydemandbar_table()
    bargraph <- ggplot(data,aes(x=year,y=Value))+
      geom_bar(aes(fill=supplyordemand),stat="identity",position="dodge")+
      CustomTheme + 
      theme(legend.position = "bottom",
            text = element_text(size=16),
            axis.text.y = element_text(size=14)) +
      scale_fill_manual("Key",labels=c("supply"="Supply","finalmult"="Demand"),values=c("supply" = "steelblue","finalmult" = "steelblue1"))+
      scale_y_continuous(labels=comma,limits=c(0,max(data$Value)*1.1))+
      labs(x="Year",y=ifelse(isolate(input$headcount_or_cost)=="Headcount","Headcount","Monthly Cost (SAR)"))+
      geom_text(colour="black",aes(fontface=2,label=label,fill=supplyordemand),position=position_dodge(width = 0.9),vjust=-1)
    
    print("Generating supply demand bar chart end")
    bargraph
  })
  

# Calculating changes for waterfall charts --------------------------------
  final_waterfall_table <- reactive({
    print("Generate waterfall table start")
    final_headcount_table()
    
    input$exec_refresh_graphs
    input$deep_refresh_graphs
    
    #table <- rvalues$final_hc
    #table <- rvalues$exec_table_output
    final_hc_change <- rvalues$data_baseline_aut_pro_dd_out
    data_baseline <-  rvalues$data_baseline
    data_hc <- rvalues$data_hc
    final_hc <- rvalues$final_hc
    
    
    if(input$tabs == "tab_executive"){
      scenario = ifelse(isolate(input$exec_scenario)=="Conservative","min",ifelse(isolate(input$exec_scenario)=="Realistic","mid","max"))
    } else {
      scenario = ifelse(isolate(input$deep_scenario)=="Conservative","min",ifelse(isolate(input$deep_scenario)=="Realistic","mid","max"))
    }
      
    multiplier_cols <- c(
      paste("aut",scenario,"Y3","q4",sep="_"),
      paste("pro",scenario,"Y3","q4",sep="_"),
      paste("dd",scenario,"Y3","q4",sep="_")
      #paste("out",scenario,"Y3","q4",sep="_")
    )
    
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
    
    data_baseline_adred <- left_join(data_baseline,data_hc,by="unique_identifier")
    for(x in c("min","mid","max")){
      
      
      input_baseline = as.name(paste("baseline",x,sep="_"))
      input_hc = as.name("hc_chosen")
      output_additions = paste("additions",x,sep="_")
      output_reductions = paste("reductions",x,sep="_")
      
      data_baseline_adred <- data_baseline_adred %>%
        rowwise() %>%
        mutate(!!output_additions := ifelse(!!input_baseline>!!input_hc,!!input_baseline-!!input_hc,0)) %>%
        mutate(!!output_reductions := ifelse(!!input_baseline<!!input_hc,!!input_baseline-!!input_hc,0))
      
    }
    
    # ...Calculating figures for waterfall charts --------------------------------
    data_waterfall_baseline <- data_baseline_adred %>% select(unique_identifier,hc_chosen,baseline_min,baseline_mid,baseline_max,additions_min,additions_mid,additions_max,reductions_min,reductions_mid,reductions_max)
    data_waterfall_baseline <- left_join(select(data_sectioninfo,unique_identifier,l0,l1,l2,l3,l4,segment),data_waterfall_baseline,by="unique_identifier")
    

# EXEC OR DEEP ------------------------------------------------------------

    if(input$tabs == "tab_executive"){
      data_waterfall_baseline <- data_waterfall_baseline %>% 
        filter(l0 %in% isolate(input$l0_list_exec)) %>%
        filter(l1 %in% isolate(input$l1_list_exec)) %>%
        filter(l2 %in% isolate(input$l2_list_exec)) %>%
        filter(segment %in% isolate(input$segment_list_exec))
    } else {
      data_waterfall_baseline <- data_waterfall_baseline %>% 
        filter(if(isolate(input$deep_chosenl0)=="All") l0 != "abc" else l0 == isolate(input$deep_chosenl0)) %>%
        filter(if(isolate(input$deep_chosenl1)=="All") l1 != "abc" else l1 == isolate(input$deep_chosenl1)) %>%
        filter(if(isolate(input$deep_chosenl2)=="All") l2 != "abc" else l2 == isolate(input$deep_chosenl2)) %>%
        filter(if(isolate(input$deep_chosenl3)=="All") l3 != "abc" else l3 == isolate(input$deep_chosenl3)) %>%
        filter(if(isolate(input$deep_chosenl4)=="All") l4 != "abc" else l4 == isolate(input$deep_chosenl4)) %>%
        filter(segment %in% isolate(input$segment_list_deep))
    }
    
    data_waterfall_finals <- final_hc %>% select(unique_identifier,grep("finalmult",colnames(final_hc)))
    
    data_waterfall_finals <- data_waterfall_finals %>% select(unique_identifier,grep("q4",colnames(data_waterfall_finals)))
    
    data_waterfall_change <- final_hc_change %>% select(unique_identifier,grep("Y3",colnames(final_hc_change)))
    data_waterfall_change <- data_waterfall_change %>% select(unique_identifier,grep("q4",colnames(data_waterfall_change)))
    
    data_final_all <- left_join(data_waterfall_baseline,data_waterfall_finals,by="unique_identifier")

    data_final_all <- left_join(data_final_all,select(data_waterfall_change,
                                                      unique_identifier,grep(scenario,colnames(data_waterfall_change)),-grep("finalmult",colnames(data_waterfall_change))),
                                by="unique_identifier")
    #data_final_all <- left_join(data_final_all,select(data_sectioninfo,unique_identifier,segment),by="unique_identifier")
    
    data_final_all_scenario <- select(data_final_all,unique_identifier,segment,hc_chosen,grep(scenario,colnames(data_final_all)))
    data_final_all_scenario <- left_join(data_final_all_scenario,data_sectioncost,by="unique_identifier")
    
    colnames(data_final_all_scenario) <- gsub(paste0("_",scenario),"",colnames(data_final_all_scenario))
    
    baseline_output = "baseline"
    baseline_input = as.name("baseline")
    additions_output = "additions"
    additions_input = as.name("additions")
    reductions_output = "reductions"
    reductions_input = as.name("reductions")
    aut_output = "aut_Y3_q4_change"
    aut_input = as.name("aut_Y3_q4_change")
    pro_output = "pro_Y3_q4_change"
    pro_input = as.name("pro_Y3_q4_change")
    dd_output = "dd_Y3_q4_change"
    dd_input = as.name("dd_Y3_q4_change")
    #out_output = "out_Y3_q4_change"
    #out_input = as.name("out_Y3_q4_change")
    finalmult_output = "finalmult_Y3_q4"
    finalmult_input = as.name("finalmult_Y3_q4")
    
    data_final_all_scenario_cost <- data_final_all_scenario %>%
      rowwise() %>%
      mutate(hc_chosen = hc_chosen * fte_cost_avg) %>%
      mutate(!!baseline_output := !!baseline_input * fte_cost_avg) %>%
      mutate(!!additions_output := !!additions_input * fte_cost_avg) %>%
      mutate(!!reductions_output := !!reductions_input * fte_cost_avg) %>%
      mutate(!!aut_output := !!aut_input * fte_cost_avg) %>%
      mutate(!!pro_output := !!pro_input * fte_cost_avg) %>%
      mutate(!!dd_output := !!dd_input * fte_cost_avg) %>%
      #mutate(!!out_output := !!out_input * fte_cost_avg) %>%
      mutate(!!finalmult_output := !!finalmult_input * fte_cost_avg)
        
    
    data_final_all_scenario_sum <- data_final_all_scenario %>% group_by(segment) %>% summarise_all(funs(sum),na.rm=TRUE)
    
    data_final_all_scenario_sum_cost <- data_final_all_scenario_cost %>% group_by(segment) %>% summarise_all(funs(sum),na.rm=TRUE)
    
    rvalues$final_waterfall_table_hc <- data_final_all_scenario_sum
    rvalues$final_waterfall_table_cost <- data_final_all_scenario_sum_cost
    
    if(isolate(input$headcount_or_cost)=="Cost"){
      rvalues$final_waterfall_table <- data_final_all_scenario_sum_cost
    } else {
      rvalues$final_waterfall_table <- data_final_all_scenario_sum
    }
        
        
    print("Generate waterfall table end")
    return(data_final_all_scenario_sum)
})
  
  
  # Waterfall plot ----------------------------------------------------------

output$waterfall <- renderPlot({
  print("Generate waterfall start")
  # if(is.null(input$l0_list_exec) | is.null(input$l1_list_exec) | is.null(input$l2_list_exec)){
  #   text = paste("Generating Graph")
  #   blankchart <- ggplot() +
  #     #annotate("text", x = 4, y = 25, size=8, label = text) +
  #     theme_bw() +
  #     theme(panel.grid.major=element_blank(),
  #           panel.grid.minor=element_blank(),
  #           axis.ticks=element_blank(),
  #           axis.text = element_blank(),
  #           axis.title=element_blank(),
  #           panel.grid = element_blank(),
  #           panel.border=element_blank())
  #   return(blankchart)
  # }

  input$exec_refresh_graphs
  
  final_waterfall_table()
  data = rvalues$final_waterfall_table
  print("waterfall table ===================")
  print(data)
    # Waterfall functions
  
  data <- data %>%
    select(-grep("out_",colnames(data)))
  
  which_row <- function(x) {
    tmp <- long[long$segment == "szum" & long$variable == x, ] %>%
      rownames()  %>%
      as.numeric()
    return(tmp)
  }
  
  get_position <- function(x) {
    val <- 0
    if(dim(x)[1]!=1){
      for (i in 2:dim(x)[1]) {
        x$value_half[i] <- abs(x$value[i-1]) + x$value_half[i] + val
        val <- abs(x$value[i-1]) + val
      }
    }
    #x$value_half <- rev(x$value_half)
    return(x)
  }

  # ... Waterfall code ----
  
  scenario = ifelse(isolate(input$exec_scenario)=="Conservative","min",ifelse(isolate(input$exec_scenario)=="Realistic","mid","max"))
  
  tmp <- data
  #write_csv(tmp,"Test CSV waterfall.csv")

  colnames <- colnames(select(tmp,-segment,-unique_identifier))
  print(colnames)
  for(x in colnames){
    input_x <- x
    output_x <- as.name(x)
    tmp <- tmp %>%
      mutate(!!input_x := as.numeric(!!output_x))
  }
  
  colnames(tmp) <- gsub(paste0("_",scenario),"",colnames(tmp))

  # detect whether dd is negative or positive
  if (sum(tmp$dd_Y3_q4_change) > 0) {
    tmp_dd <- "positive"
  } else {
    tmp_dd <- "negative"
  }
  
  #write_csv(tmp,"Waterfall calcs step 1.csv")
  
  # Correcting for the fact that changes stack on top of each other, and might not be great enough
  # for individual sections to cause a drop of a full person
  tmp <- tmp %>%
    mutate(aut_Y3_q4_change_cumu = (aut_Y3_q4_change/baseline)*(baseline)) %>%
    mutate(pro_Y3_q4_change_cumu = (pro_Y3_q4_change/baseline)*(baseline + aut_Y3_q4_change_cumu)) %>%
    mutate(dd_Y3_q4_change_cumu = (dd_Y3_q4_change/baseline)*(baseline + aut_Y3_q4_change_cumu + pro_Y3_q4_change_cumu)) %>%
    
    # Implied change, not taking into account individual sections get rounded up at end to give whole person
    mutate(implied_change = aut_Y3_q4_change_cumu + pro_Y3_q4_change_cumu + dd_Y3_q4_change_cumu) %>% 
    
    # Actual change, with each section rounded up to give whole number of people
    mutate(actual_change = finalmult_Y3_q4 - baseline)
  
  tmp <- tmp %>%
    
    # Works out difference of total implied and actual, then adds a weighted portion of that to the final number for each
    
    mutate(aut_Y3_q4_change_weight = (actual_change - implied_change) * (aut_Y3_q4_change_cumu / implied_change)) %>%
    mutate(pro_Y3_q4_change_weight = (actual_change - implied_change) * (pro_Y3_q4_change_cumu / implied_change)) %>%
    mutate(dd_Y3_q4_change_weight = (actual_change - implied_change) * (dd_Y3_q4_change_cumu / implied_change)) %>%
    
    # Headcount increases above multiplier cannot result in positive aut or pro change, so if result > 0 must be in dd
    mutate(aut_Y3_q4_change_final = ifelse(aut_Y3_q4_change_cumu + aut_Y3_q4_change_weight > 0,0,round(aut_Y3_q4_change_cumu + aut_Y3_q4_change_weight,0))) %>%
    mutate(pro_Y3_q4_change_final = ifelse(pro_Y3_q4_change_cumu + pro_Y3_q4_change_weight > 0,0,round(pro_Y3_q4_change_cumu + pro_Y3_q4_change_weight,0))) %>%
    mutate(dd_Y3_q4_change_final = actual_change - aut_Y3_q4_change_final - pro_Y3_q4_change_final)
    
    
  tmp <- tmp %>%
    # Renaming back to change columns
    mutate(aut_Y3_q4_change = aut_Y3_q4_change_final) %>%
    mutate(pro_Y3_q4_change = pro_Y3_q4_change_final) %>%
    mutate(dd_Y3_q4_change = dd_Y3_q4_change_final) %>%
    
    # Removing unneeded columns
    select(-pro_Y3_q4_change_cumu,-aut_Y3_q4_change_cumu,-dd_Y3_q4_change_cumu,
           -pro_Y3_q4_change_weight,-aut_Y3_q4_change_weight,-dd_Y3_q4_change_weight,
           -pro_Y3_q4_change_final,-aut_Y3_q4_change_final,-dd_Y3_q4_change_final)
    
    #mutate(out_Y3_q4_change_cumu = (out_Y3_q4_change/baseline)*(baseline + aut_Y3_q4_change + pro_Y3_q4_change + dd_Y3_q4_change)) %>%
    #mutate("multiplier" = (finalmult_Y3_q4 - baseline) / (pro_Y3_q4_change_cumu + aut_Y3_q4_change_cumu + dd_Y3_q4_change_cumu))
  #mutate("multiplier" = (finalmult_Y3_q4 - baseline) / (pro_Y3_q4_change_cumu + aut_Y3_q4_change_cumu + dd_Y3_q4_change_cumu + out_Y3_q4_change_cumu))
    
  
  
  #multiplier = (sum(tmp$finalmult_Y3_q4) - sum(tmp$baseline)) / (sum(tmp$pro_Y3_q4_change_cumu) + sum(tmp$aut_Y3_q4_change_cumu) + sum(tmp$dd_Y3_q4_change_cumu) + sum(tmp$out_Y3_q4_change_cumu))
  #write_csv(tmp,"Waterfall calcs step 2.csv")
  
  # tmp <- tmp %>%
  #   mutate(aut_Y3_q4_change = aut_Y3_q4_change_cumu * multiplier) %>%
  #   mutate(pro_Y3_q4_change = pro_Y3_q4_change_cumu * multiplier) %>%
  #   mutate(dd_Y3_q4_change = dd_Y3_q4_change_cumu * multiplier) %>%
  #   #mutate(out_Y3_q4_change = out_Y3_q4_change_cumu * multiplier) %>%
  #   select(-pro_Y3_q4_change_cumu,-aut_Y3_q4_change_cumu,-dd_Y3_q4_change_cumu,-multiplier)
  #   #select(-pro_Y3_q4_change_cumu,-aut_Y3_q4_change_cumu,-dd_Y3_q4_change_cumu,-out_Y3_q4_change_cumu,-multiplier)
  
  #write_csv(tmp,"Waterfall calcs step 3.csv")
  
  tmp$segment <- paste0("segment_", tmp$segment)
  
  
  # finalmult_Y1_q4
  tmp_segment <- c("segment", "hc_chosen", "additions",
                  "reductions", "baseline",
                  "aut_Y3_q4_change", "pro_Y3_q4_change",
                  "dd_Y3_q4_change", 
                  "finalmult_Y3_q4")
  #"out_Y3_q4_change",
  
  # calculate sum for each columns
  tmp <- tmp[tmp_segment]
  tmp[(dim(tmp)[1] + 1), ] <- c("szum", apply(tmp[2:9], 2, sum))
  
  # change from wide to long dataframe format
  long <- melt(tmp, id.vars = "segment")
  # add starting color
  long$colour <- "#000000"
  # ensure that value is numeric
  long$value <- as.numeric(as.character(long$value))
  
  # delete 'segment == szum & variable == reductions'
  # no need it
  long <- long[-which_row("reductions"), ]
  # recalculate the number of rows
  rownames(long) <- 1:dim(long)[1]
  # save original data for further use
  long$placeholder <- long$segment
  # delete 'segment == szum & variable == finalmult_Y1_q4'
  # no need it
  long <- long[-which_row("finalmult_Y3_q4"), ]
  # recalculate the number of rows
  rownames(long) <- 1:dim(long)[1]
  
  # calculate whitespace for additions
  long[which_row("additions"), ]$value <- long[which_row("additions"), ]$value +
    long[which_row("hc_chosen"), ]$value
  # calculate whitespace for reductions
  long[which_row("additions"), ]$value <- long[which_row("additions"), ]$value + 
    sum(long[long$variable == "reductions" & long$segment != "szum", ]$value)
  # change additions to reductions
  long[long$segment == "szum" & long$variable == "additions", ]$variable <- "reductions"
  # change hc_chosen to additions
  long[long$segment == "szum" & long$variable == "hc_chosen", ]$variable <- "additions"
  # recalculate the number of rows
  rownames(long) <- 1:dim(long)[1]
  
  # calculate whitespace for aut_Y3_q4_change
  long[which_row("baseline"), ]$value <- long[which_row("baseline"), ]$value +
    sum(long[long$variable == "aut_Y3_q4_change" & long$segment != "szum", ]$value)
  # calculate whitespace for pro_Y3_q4_change
  long[which_row("aut_Y3_q4_change"), ]$value <- long[which_row("baseline"), ]$value + 
    sum(long[long$variable == "pro_Y3_q4_change"  & long$segment != "szum", ]$value)
  
  if (long[which_row("dd_Y3_q4_change"), ]$value > 0) {
    long[which_row("pro_Y3_q4_change"), ]$value <- long[which_row("aut_Y3_q4_change"), ]$value
  } else {
    long[which_row("pro_Y3_q4_change"), ]$value <- long[which_row("aut_Y3_q4_change"), ]$value +
      sum(long[long$variable == "dd_Y3_q4_change" & long$segment != "szum", ]$value) 
  }
  
  # if (long[which_row("dd_Y3_q4_change"), ]$value > 0) {
  #   long[which_row("dd_Y3_q4_change"), ]$value <- long[which_row("pro_Y3_q4_change"), ]$value +
  #     sum(long[long$variable == "dd_Y3_q4_change" & long$segment != "szum", ]$value) +
  #     sum(long[long$variable == "out_Y3_q4_change" & long$segment != "szum", ]$value)
  # } else {
  #   long[which_row("dd_Y3_q4_change"), ]$value <- long[which_row("pro_Y3_q4_change"), ]$value +
  #     #sum(long[long$variable == "dd_Y3_q4_change" & long$segment != "szum", ]$value) +
  #     sum(long[long$variable == "out_Y3_q4_change" & long$segment != "szum", ]$value)
  # }
  # 
  
  # delete 'segment == szum & variable == out_Y3_q4_change'
  # no need it
  #long <- long[-which_row("out_Y3_q4_change"), ]
  long <- long[-which_row("dd_Y3_q4_change"), ]
  
  #  change dd_Y3_q4_change to out_Y3_q4_change
  #long[long$segment == "szum" & long$variable == "dd_Y3_q4_change", ]$variable <- "out_Y3_q4_change"
  
  #  change pro_Y3_q4_change to dd_Y3_q4_change
  long[long$segment == "szum" & long$variable == "pro_Y3_q4_change", ]$variable <- "dd_Y3_q4_change"
  #  change aut_Y3_q4_change to pro_Y3_q4_change
  long[long$segment == "szum" & long$variable == "aut_Y3_q4_change", ]$variable <- "pro_Y3_q4_change"
  #  change baseline to aut_Y3_q4_change
  long[long$segment == "szum" & long$variable == "baseline", ]$variable <- "aut_Y3_q4_change"
  
  # recalculate the number of rows
  rownames(long) <- 1:dim(long)[1]
  # add rownames to dataframe
  # it needs for the waterfall graph
  long$osztaly <- rownames(long)
  long$osztaly <- as.factor(long$osztaly)
  
  # create classes for the waterfallgraph
  tmp_class <- as.numeric(table(long$variable))
  long$osztaly <- paste0("class", 1:length(tmp_class)) %>%
    rep(., tmp_class) %>%
    as.factor()
  # segment should be unique factor
  long$segment <- as.factor(1:dim(long)[1])
  
  # 
  long$value_half <- abs(long$value) / 2
  
  # split by columns (osztaly)
  long_split <- split(long, long$osztaly)
  # get positions for text ISSUE
  long_split <- lapply(long_split, function(x) get_position(x))
  
  # reorder data
  long_split <- lapply(long_split, function(x) x[order(as.numeric(x$segment), decreasing = TRUE), ])
  long_split <- do.call(rbind, long_split)
  # change negative values to positive
  long_split$value <- abs(long_split$value)
  # recalculate the number of rows
  rownames(long_split) <- 1:dim(long_split)[1]
  # segment should be unique factor
  long_split$segment <- as.factor(1:dim(long_split)[1])
  
  # change the colours of the whitespaces to white
  # long_split$colour[which(long_split$placeholder == "szum")] <- "#FFFFFF"
  # 
  # # generate 5 random colours and add to them to the right category
  # tmp_colour <- randomColor(5)
  # 
  # if("segment_Core" %in% long_split$placeholder){
  #   long_split[long_split$placeholder == "segment_Core", ]$colour <- "steelblue1"
  # }
  # 
  # if("segment_Critical" %in% long_split$placeholder){
  #   long_split[long_split$placeholder == "segment_Critical", ]$colour <- "steelblue2"
  # }
  # 
  # if("segment_Specialist" %in% long_split$placeholder){
  #   long_split[long_split$placeholder == "segment_Specialist", ]$colour <- "steelblue3"
  # }
  # 
  # if("segment_Support" %in% long_split$placeholder){
  #   long_split[long_split$placeholder == "segment_Support", ]$colour <- "steelblue"
  # }
  # 
  # if("segment_Management" %in% long_split$placeholder){
  #   long_split[long_split$placeholder == "segment_Management", ]$colour <- "royalblue2"
  # }
  #long_split[long_split$placeholder == "segment_NA", ]$colour <- tmp_colour[5]
  
  long_split <- long_split %>%
    rowwise() %>%
    mutate(label = ifelse(isolate(input$headcount_or_cost)=="Headcount",
                          round(value,0),
                          ifelse(value>1000000,paste0(format(round(value / 1e6, 1), trim = TRUE), "m"),
                          paste0(format(round(value / 1e3, 1), trim = TRUE), "k")
                          )))
  
  long_split$variable <- gsub("hc_chosen","Current",long_split$variable)
  long_split$variable <- gsub("additions","Additions",long_split$variable)
  long_split$variable <- gsub("reductions","Reductions",long_split$variable)
  long_split$variable <- gsub("baseline","Baseline",long_split$variable)
  long_split$variable <- gsub("aut_Y3_q4_change","Automation",long_split$variable)
  long_split$variable <- gsub("pro_Y3_q4_change","Training",long_split$variable)
  long_split$variable <- gsub("dd_Y3_q4_change","Demand",long_split$variable)
  #long_split$variable <- gsub("out_Y3_q4_change","Outsourcing",long_split$variable)
  long_split$variable <- gsub("finalmult_Y3_q4","Year 3",long_split$variable)
  
  
  long$variable <- gsub("hc_chosen","Current",long$variable)
  long$variable <- gsub("additions","Additions",long$variable)
  long$variable <- gsub("reductions","Reductions",long$variable)
  long$variable <- gsub("baseline","Baseline",long$variable)
  long$variable <- gsub("aut_Y3_q4_change","Automation",long$variable)
  long$variable <- gsub("pro_Y3_q4_change","Training",long$variable)
  long$variable <- gsub("dd_Y3_q4_change","Demand",long$variable)
  #long$variable <- gsub("out_Y3_q4_change","Outsourcing",long$variable)
  long$variable <- gsub("finalmult_Y3_q4","Year 3",long$variable)
  
  long_split$placeholder <- gsub("szum","aasum",long_split$placeholder)
  
  long_split$segment <- long_split$segment %>% as.numeric()
  
  long_split <- add_row(long_split,segment=0,variable="Current",value=0,colour="steelblue1",placeholder="aasum",osztaly="class1",value_half=0,label="")
  
  long_split <- long_split %>%
    arrange(placeholder)
  
  long_split <- long_split %>%
    arrange(osztaly)
  
  long_totals <- long_split %>%
    filter(placeholder != "aasum") %>%
    select(osztaly,value) %>%
    group_by(osztaly) %>%
    summarise(value=round(abs(sum(value)),0))
  
  long_totals <- long_totals %>%
    rowwise() %>%
    mutate(value = ifelse(isolate(input$headcount_or_cost)=="Headcount",
                          round(value,0),
                          ifelse(value>1000000,paste0(format(round(value / 1e6, 1), trim = TRUE), "Mn"),
                                 paste0(format(round(value / 1e3, 1), trim = TRUE), "K")
                          )))

  long_totals_height <- long_split %>%
    select(osztaly,value) %>%
    group_by(osztaly) %>%
    summarise(height=abs(sum(value))*1.02,0)
  
  highest=max(long_totals_height$height)
  
  long_totals <- long_totals %>%
    mutate("height" := !!highest*1.04)
  
  print("Waterfall table final:")
  print(long_totals)
  
  print("Waterfall table final for graph:")
  print(long_split)
  maxval=max(long_totals$height*1.05)
  
  # create waterfall chart
  p <- ggplot(data = long_split, aes(x = osztaly, y = value, fill = fct_rev(placeholder))) + 
    geom_bar(stat = "identity", colour = NA, lwd = 0.2) + 
    geom_label_repel(aes(x=osztaly,label = label, y = value,fill=fct_rev(placeholder)),colour="white", position=position_stack(vjust=0.5),ylim=c(NA,highest),direction="y",segment.size = 0,show.legend = FALSE) +
    geom_label(aes(x=osztaly,y=height,label=value,fill=NULL),data=long_totals,show.legend = FALSE)+
    scale_fill_manual(name="Segment",
                      labels = c(segment_Core="Core",segment_Critical = "Critical",segment_Support = "Support",segment_Specialist = "Specialist",segment_Management = "Management",aasum=""),
                      values = c(segment_Core=Blue2,segment_Critical = Blue3,segment_Support = Green2,segment_Specialist = Red,segment_Management = Green1,aasum="white"))+
    guides(fill = guide_legend(title=NULL,nrow=1)) +
    theme_bw() +
    scale_y_continuous(labels=comma,limits=c(0,maxval))+
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          legend.direction = "horizontal",
          text = element_text(size=16),
          axis.text.y = element_text(size=14)
    )+
    xlab("")+
    ylab(ifelse(isolate(input$headcount_or_cost) == "Headcount","Headcount","Monthly Cost (SAR)"))
  # relabel the x axis

  p <- p + scale_x_discrete(labels= c("Current","Additions","Reductions","Baseline","Automation","Training","Demand","Year 3"))
    
  
  tmp_lines <- list()
  tmp_lines[[1]] <- sum(long_split[long_split$variable == "Current", ]$value)
  tmp_lines[[2]] <- tmp_lines[[1]] + sum(long_split[long_split$variable == "Additions" & long_split$placeholder != "aasum", ]$value)
  tmp_lines[[3]] <- long_split[long_split$variable == "Reductions" & long_split$placeholder == "aasum", ]$value
  tmp_lines[[4]] <- sum(long_split[long_split$variable == "Baseline", ]$value)
  tmp_lines[[5]] <- long_split[long_split$variable == "Automation" & long_split$placeholder == "aasum", ]$value
  
  if (tmp_dd == "positive") {
    tmp_lines[[6]] <- sum(long_split[long_split$variable == "Demand" & long_split$placeholder == "aasum", ]$value)
    
    tmp_lines[[7]] <- sum(long_split[long_split$variable == "Demand" & long_split$placeholder == "aasum", ]$value) +
      sum(long_split[long_split$variable == "Demand" & long_split$placeholder != "aasum", ]$value)
  } else {
    tmp_lines[[6]] <- sum(long_split[long_split$variable == "Training" & long_split$placeholder == "aasum", ]$value)
    tmp_lines[[7]] <- sum(long_split[long_split$variable == "Demand" & long_split$placeholder == "aasum", ]$value)
  }
  
  #tmp_lines[[8]] <- long_split[long_split$variable == "Outsourcing" & long_split$placeholder == "aasum", ]$value
  
  for (i in 1:length(tmp_lines)) {
    p <- p + annotate("segment",
                      x = c(i, i, i+1),
                      xend = c(i, i+1, i+1),
                      y = c(tmp_lines[[i]], tmp_lines[[i]], tmp_lines[[i]]),
                      yend = c(tmp_lines[[i]], tmp_lines[[i]], tmp_lines[[i]]),
                      colour = "black",
                      size = 0.25,
                      linetype = 2)
  }
  
  print("Waterfall creation end")
  p

})  


# Observing event to trigger calculation of final table -------------------
observeEvent(input$generateall,{
  print("Observed generate all start")
  # js$disableTab("tab_executive")
  # js$disableTab("tab_deepdive")
  
  # Disabling analysis tabs while it calculates
  addCssClass(selector = "a[data-value='tab_executive']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='tab_deepdive']", class = "inactiveLink")
  
  # updateTabsetPanel(session, "tabs", "tab_executive")
  # updateTabsetPanel(session, "tabs", "tab_deepdive")
  
  print("Observing setup generation")
  rvalues$generated <- 1
  final_headcount_table()
  print("Setup generation calculation complete")
  
  # Enabling analysis tabs when calculation complete
  removeCssClass(selector = "a[data-value='tab_executive']", class = "inactiveLink")
  removeCssClass(selector = "a[data-value='tab_deepdive']", class = "inactiveLink")
  
  # Switching back to view model initialisation tab
  removeCssClass(selector = "a[data-value='tab_modelsetup']", class = "inactiveLink")
  # js$enableTab("tab_executive")
  # js$enableTab("tab_deepdive")
  # updateTabsetPanel(session, "tabs", "tab_executive")
  # updateTabsetPanel(session, "tabs", "tab_deepdive")
  print("Observed generate all end")
})


# Observing event to trigger charts ------------------------------
  
  # Would be good to put trigger on segment_list_exec, but this seems to run immediately and thus break it
  observeEvent(
    {
      # input$l2_list_exec
      # input$segment_list_exec
      # input$headcount_or_cost 
      input$exec_refresh_graphs
      #input$tabs
      },
      {
      if(rvalues$generated!=0){
        rvalues$exec_scenario
        table_exec <- exec_table_output()
        table_final_line <- final_line_table()
        table_final_waterfall <- final_waterfall_table()
        table_final_barchart <- final_barchart_table()
        final_supplydemandbar_table <- final_supplydemandbar_table()
        final_waterfall_table()
      }
  #final_waterfall_table()
})
  
  
# Generate csv test -------------------------------------------------------
  observeEvent(input$generate_csv,{
    #write_csv(rvalues$final_data,"Final Data Output Values.csv")
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
    # single_or_aggregated <- input$single_agg_choice
    # if(single_or_aggregated=="Single Section Data"){
    #   output <- output %>%
    #     filter(l0==input$deep_chosenl0 &
    #              l1==input$deep_chosenl1 &
    #              l2==input$deep_chosenl2 &
    #              l3==input$deep_chosenl3 &
    #              l4==input$deep_chosenl4)
    
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

  rvalues$deep_sections_list <- output
  })
  
# Headcount graph ---------------------------------------------------------

    # Headcount table calculations
  deep_hc_graph_table <- reactive({

    table <- rvalues$deep_sections_list
    table <- left_join(table,select(data_hc,unique_identifier,fte,contractor),by="unique_identifier")
    table <- table %>%
      select(fte,contractor) %>%
      group_by() %>%
      summarise_all(funs(sum),na.rm=TRUE) %>%
      gather("Key","Value")
    return(table)
  })
  
  # Headcount graph creation
  output$deep_hc_graph <- renderPlot({
    if(rvalues$deep_generated == 0){
      return(NULL)
    }
    data=deep_hc_graph_table()
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
      geom_text(data=subset(data,Value!=0),colour="white",aes(fontface=2,label=Value),position="stack",hjust=1)
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
    fulltable <- tibble(segment=c("Critical","Specialist","Support","Core","Management"))
    fulltable <- left_join(fulltable,table,by="segment")
    
    return(fulltable)
  })
  
  # Segment graph creation
  output$deep_segment_graph <- renderPlot({
    if(rvalues$deep_generated == 0){
      return(NULL)
    }
    data=deep_segment_table()
    bargraph = ggplot(data,aes(x=segment,y=hc_chosen))+
      geom_bar(stat="identity",fill="steelblue1",width=0.8) +
      coord_flip()+
      CustomTheme + 
      theme(legend.position = "bottom") +
      scale_fill_manual("segment",labels=c("Core"="Core","Specialist"="Specialist","Critical"="Critical","Support"="Support","Management"="Management"),values=c("Core"="steelblue1","Specialist"="steelblue2","Critical"="steelblue3","Support"="steelblue4","Management"="steelblue")) +
      scale_x_discrete(labels=c("Core"="Core","Specialist"="Specialist","Critical"="Critical","Support"="Support","Management"="Management"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      geom_text(colour="black",aes(fontface=2,label=hc_chosen),hjust=-0.25)+
      scale_y_continuous(limits=c(0,max(data$hc_chosen)*1.1))+
      labs(x="Segment",y="Headcount (FTE + Contractors)")
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
    
    return(table)
  })
  
  # Education graph creation
  output$deep_education_graph <- renderPlot({
    if(rvalues$deep_generated == 0){
      return(NULL)
    }
    data=deep_education_table()

    doughnutgraph = ggplot(data,aes(x=2,y=valuepercentage,fill=Key,label=label))+
      geom_bar(stat="identity",width=1) +
      coord_polar(theta = "y")+
      geom_text(data=subset(data,valuepercentage!=0),colour="white",position=position_fill(vjust=0.5),aes(fontface=2))+
      CustomTheme + 
      theme(legend.position = "bottom") +
      xlim(0.5,2.5)+
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
      labs(x="Age Band") +
      scale_y_continuous(limits=c(0,max(data$valuepercentage)*1.1))+
      scale_x_discrete(labels=c(`age_20-24`="20-24",`age_25-29`="25-29",`age_30-34`="30-34",`age_35-39`="35-39",`age_40-44`="40-44",`age_45-49`="45-49",`age_50-54`="50-54",`age_55-59`="55-59",`age_60+`="60+"))+
      geom_text(colour="black",aes(fontface=2,label=label),vjust=-1.25)
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
    
    #table$Key <- factor(table$Key,levels=c("1","2","3","4","5","6"))
    # table <- add_column(table,"roworder"=c(1,2,3,4,5,6))
    # table <- table %>% arrange(roworder)
    # table <- table %>% select(-roworder)

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
      labs(x="Tenure Band") +
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      
      scale_y_continuous(limits=c(0,max(data$valuepercentage)*1.1))+
      scale_x_discrete(labels=c(`tenure_0-4`="0-4",`tenure_5-9`="5-9",`tenure_10-14`="10-14",`tenure_15-19`="15-19",`tenure_20-24`="20-24",`tenure_25+`="25+"),
      limits=c("tenure_0-4","tenure_5-9","tenure_10-14","tenure_15-19","tenure_20-24","tenure_25+"))+
      #limits=c("0-4","5-9","10-14","15-19","20-24","25+"))+
      geom_text(colour="black",aes(fontface=2,label=label),vjust=-1.25)

    bargraph
  })
  
  
# Forcing graph refresh ---------------------------------------------------
observeEvent(input$deep_generate,{
  rvalues$deep_generated = 1
  withProgress(message="Generating deep dive data",value=0,{
    
    deep_sections_list()
    
    incProgress(0.1,"Generating deep dive data")
    deep_segment_table()
    incProgress(0.1,"Generating deep dive data")
    deep_hc_graph_table()
    incProgress(0.1,"Generating deep dive data")
    deep_education_table()
    deep_age_table()
    incProgress(0.1,"Generating deep dive data")
    deep_tenure_table()
    deep_final_table()
    incProgress(0.1,"Generating deep dive data")
    deep_final_table_summarised()
    incProgress(0.1,"Generating deep dive data")
    deep_final_table_overwritten()
    incProgress(0.1,"Generating deep dive data")
    deep_hc_table()
    incProgress(0.2,"Generating deep dive data")
    final_line_table_deep()
    incProgress(0.1,"Generating deep dive data")
    deep_sliders_initial()
    
  })
  })
  

# Line Chart --------------------------------------------------------------

# ... Calculating new multipliers ---------------------------------------------

  deep_final_table <- reactive({
    input = rvalues$final_multipliers
    rows = deep_sections_list() %>%
      select(unique_identifier)
    outputtable = left_join(rows,input,by="unique_identifier")
    rvalues$deep_final_table <- left_join(outputtable,select(data_outsourcing,unique_identifier,out_yesno),by="unique_identifier")
    return(outputtable)
  })
  
  output$deep_final_table <- renderDataTable({rvalues$deep_final_table},options=list(scrollX=TRUE))
  
  # Work out weighted averages
  deep_final_table_summarised <- reactive({
    fulltable = rvalues$deep_final_table %>%
      select(-unique_identifier,-baseline_bottom,-baseline_top)
    
    totalhc <- sum(fulltable$baseline_mid)
    fulltable$baseline_mid2 <- fulltable$baseline_mid
    cols_fulltable <- colnames(select(fulltable,-baseline_min,-baseline_mid,-baseline_max,-baseline_mid2))
    
    fulltable_sum <- fulltable
    
    for(x in cols_fulltable){
      inputcol = as.name(x)
      outputcol = x
      
      fulltable_sum <-  fulltable_sum %>% 
        rowwise() %>%
        mutate(!!outputcol := !!inputcol / totalhc * baseline_mid2)
    }
    
    
    fulltable_sum <- fulltable_sum %>%
      select(-baseline_mid2) %>%
      group_by() %>%
      summarise_all(funs(sum),na.rm=TRUE)
    rvalues$deep_final_table_summarised = fulltable_sum

  }) 
  
  
  output$deep_final_table_summarised <- renderDataTable({rvalues$deep_final_table_summarised},options=list(scrollX=TRUE))
  
  # Overwrite original table with new weighted averages
  
  deep_final_table_overwritten <- reactive({
    
    fulltable = rvalues$deep_final_table
    sumtable = rvalues$deep_final_table_summarised
    outtable = fulltable
    
    cols_sumtable = colnames(select(sumtable,-baseline_min,-baseline_mid,-baseline_max))
    for(x in cols_sumtable){
      outputcol = x
      inputnum = as.numeric(sumtable[[as.name(x)]])
      outtable <- outtable %>%
        rowwise() %>%
        mutate(!!outputcol := inputnum)
    }
    
    # Don't do this for out_yesno
    outtable <- outtable %>% select(-out_yesno)
    outtable <- left_join(outtable,select(data_outsourcing,unique_identifier,out_yesno),by="unique_identifier")
    
    rvalues$deep_final_table_overwritten <- outtable
  })
  
  output$deep_final_table_overwritten <- renderDataTable({rvalues$deep_final_table_overwritten},options=list(scrollX=TRUE))
  

# Save initial slider values ----------------------------------
deep_sliders_initial <- reactive({
  
  table <- rvalues$deep_final_table_summarised
  #write_csv(table,"testtablegather")
  cols_final_table <- colnames(table)
  
  table_gathered <- table %>%
    gather(key,value)

  # Baseline positioning
  value_baselinepos_min = as.numeric(filter(table_gathered,grepl("baselinepos",key) & grepl("min",key))[2])*100
  value_baselinepos_mid = as.numeric(filter(table_gathered,grepl("baselinepos",key) & grepl("mid",key))[2])*100
  value_baselinepos_max = as.numeric(filter(table_gathered,grepl("baselinepos",key) & grepl("max",key))[2])*100

  # Automation
  rvalues$value_autY1_min <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("min",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)
  rvalues$value_autY2_min <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("min",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)
  rvalues$value_autY3_min <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("min",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)

  rvalues$value_autY1_mid <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("mid",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)
  rvalues$value_autY2_mid <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("mid",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)
  rvalues$value_autY3_mid <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("mid",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)
  
  rvalues$value_autY1_max <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("max",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)
  rvalues$value_autY2_max <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("max",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)
  rvalues$value_autY3_max <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("max",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)
  
  
  # Training

  rvalues$value_proY1_min <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("min",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)
  rvalues$value_proY2_min <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("min",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)
  rvalues$value_proY3_min <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("min",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)
  
  rvalues$value_proY1_mid <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("mid",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)
  rvalues$value_proY2_mid <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("mid",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)
  rvalues$value_proY3_mid <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("mid",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)
  
  rvalues$value_proY1_max <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("max",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)
  rvalues$value_proY2_max <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("max",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)
  rvalues$value_proY3_max <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("max",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)
  
  # Demand

  rvalues$value_ddY1_min <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("min",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)-100
  rvalues$value_ddY2_min <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("min",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)-100
  rvalues$value_ddY3_min <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("min",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)-100

  rvalues$value_ddY1_mid <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("mid",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)-100
  rvalues$value_ddY2_mid <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("mid",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)-100
  rvalues$value_ddY3_mid <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("mid",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)-100
  
  rvalues$value_ddY1_max <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("max",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)-100
  rvalues$value_ddY2_max <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("max",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)-100
  rvalues$value_ddY3_max <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("max",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)-100
  
})

# ... Pull new weighted averages through to sliders ---------------------------
  
  f_deep_sliders_update <- function(){
    
    withProgress(message="Updating sliders",value=0,{
      scenario = ifelse(input$deep_scenario=="Conservative","min",ifelse(input$deep_scenario=="Realistic","mid","max"))
      
      if(rvalues$deep_updated == 1){
        table <- rvalues$deep_final_table_overwritten_sum
      } else {
        table <- rvalues$deep_final_table_summarised
      }
      
      
      #write_csv(table,"testtablegather")
      cols_final_table <- colnames(table)
      
      table_gathered <- table %>%
        gather(key,value) %>%
        filter(grepl(scenario,key))
      
      # Updating baseline positioning slider
      value_baselinepos = as.numeric(filter(table_gathered,grepl("baselinepos",key))[2])*100
      updateSliderInput(session=session,inputId="deep_slider_baseline",label=NULL,value=value_baselinepos,step=1)
      
      # Updating automation slider

      incProgress(amount=0.2,"Updating automation sliders")
      value_autY1 <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)
      value_autY2 <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)
      value_autY3 <- 100-round(as.numeric(filter(table_gathered,grepl("aut",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)
      
      updateSliderInput(session=session,inputId="deep_slider_aut_Y1",label=NULL,value=value_autY1)
      updateSliderInput(session=session,inputId="deep_slider_aut_Y2",label=NULL,value=value_autY2)
      updateSliderInput(session=session,inputId="deep_slider_aut_Y3",label=NULL,value=value_autY3)
      
      # Updating training slider
      
      incProgress(amount=0.2,"Updating training sliders")
      value_proY1 <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)
      value_proY2 <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)
      value_proY3 <- 100-round(as.numeric(filter(table_gathered,grepl("pro",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)
      
      updateSliderInput(session=session,inputId="deep_slider_pro_Y1",label=NULL,value=value_proY1,step=1)
      updateSliderInput(session=session,inputId="deep_slider_pro_Y2",label=NULL,value=value_proY2)
      updateSliderInput(session=session,inputId="deep_slider_pro_Y3",label=NULL,value=value_proY3)
      
      # Updating demand slider
      
      incProgress(amount=0.2,"Updating demand sliders")
      value_ddY1 <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("Y1",key) & grepl("q4",key))[2])*100)-100
      value_ddY2 <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("Y2",key) & grepl("q4",key))[2])*100)-100
      value_ddY3 <- round(as.numeric(filter(table_gathered,grepl("dd",key) & grepl("Y3",key) & grepl("q4",key))[2])*100)-100
      
      updateSliderInput(session=session,inputId="deep_slider_dd_Y1",label=NULL,value=value_ddY1)
      updateSliderInput(session=session,inputId="deep_slider_dd_Y2",label=NULL,value=value_ddY2)
      updateSliderInput(session=session,inputId="deep_slider_dd_Y3",label=NULL,value=value_ddY3)
      
      incProgress(amount=0.2,"Completed")
    })
  }
  
  observeEvent(input$deep_generate,{
    rvalues$deep_updated <- 0
    f_deep_sliders_update()
  })
  
  observeEvent(input$deep_scenario,{
    if(rvalues$deep_generated!=0){
      f_deep_sliders_update()
    }
  })
  

# ... When slider updated, update column --------------------------------------
#   This will update the summarised table columns for selected scenario
  
  # Function to update baseline positioning
f_deep_update_baseline <- function(){
  print("Updating baseline positioning")
  scenario = ifelse(input$deep_scenario=="Conservative","min",ifelse(input$deep_scenario=="Realistic","mid","max"))
  outputtable = rvalues$deep_final_table_overwritten
  
  outputcol = paste("baselinepos",scenario,sep="_")

  baselineval = input$deep_slider_baseline/100

  # Setting baselinepos values
  outputtable <- outputtable %>%
    rowwise() %>%
    mutate(!!outputcol := !!baselineval)
  
  outputcol2 = paste("baseline",scenario,sep="_")
  
  # Adjusting baselines
  outputtable <- outputtable %>%
    rowwise() %>%
    mutate(!!outputcol2 := baseline_top - ((baseline_top - baseline_bottom) * !!baselineval))
  
  rvalues$deep_final_table_overwritten <- outputtable
}
  
  
  # Function to update automation values
f_deep_update_aut <- function(){
  print("Updating automation values")
  scenario = ifelse(input$deep_scenario=="Conservative","min",ifelse(input$deep_scenario=="Realistic","mid","max"))
  outputtable = rvalues$deep_final_table_overwritten
  
  outputcol_Y1 = paste("aut",scenario,"Y1",sep="_")
  outputcol_Y2 = paste("aut",scenario,"Y2",sep="_")
  outputcol_Y3 = paste("aut",scenario,"Y3",sep="_")
  
  autval_Y1 = 1-input$deep_slider_aut_Y1/100
  autval_Y2 = 1-input$deep_slider_aut_Y2/100
  autval_Y3 = 1-input$deep_slider_aut_Y3/100
  
  autq_Y1 = substr(input$deep_aut_quarter_Y1,2,2)
  autq_Y2 = substr(input$deep_aut_quarter_Y2,2,2)
  autq_Y3 = substr(input$deep_aut_quarter_Y3,2,2)

  # Setting yearly values
  outputtable <- outputtable %>%
    rowwise() %>%
    mutate(!!outputcol_Y1 := !!autval_Y1) %>%
    mutate(!!outputcol_Y2 := !!autval_Y2) %>%
    mutate(!!outputcol_Y3 := !!autval_Y3)
  
  # Converting to quarterly values
  for(x in c("min","mid","max")){
    for(y in c("Y1","Y2","Y3")){
      for(z in c("q1","q2","q3","q4")){
        if(x == scenario){
          year = as.numeric(substr(y,nchar(y),nchar(y)))
          quarter = as.numeric(substr(z,nchar(z),nchar(z)))
          autcol = as.name(paste("aut",x,y,sep="_"))
          
          input_autval = ifelse(year==1,autval_Y1,ifelse(year==2,autval_Y2,autval_Y3))
          input_autq = ifelse(year==1,autq_Y1,ifelse(year==2,autq_Y2,autq_Y3))
          
          input_autval_prev = ifelse(year==1,1,ifelse(year==2,autval_Y1,autval_Y2))
          
          outputcol = paste("aut",x,y,z,sep="_")
          
          outputtable = outputtable %>%
            rowwise() %>%
            mutate(!!outputcol:=ifelse(input_autq<=quarter,input_autval,ifelse(year==1 & quarter == 1,1,input_autval_prev)))
          
        }
      }
    }
  }
  
  rvalues$deep_final_table_overwritten <- outputtable
}
  
  # Function to update productivity values
f_deep_update_pro <- function(){
  print("Updating productivity values")
  scenario = ifelse(input$deep_scenario=="Conservative","min",ifelse(input$deep_scenario=="Realistic","mid","max"))
  outputtable = rvalues$deep_final_table_overwritten
  
  outputcol_Y1 = paste("pro",scenario,"Y1","q4",sep="_")
  outputcol_Y2 = paste("pro",scenario,"Y2","q4",sep="_")
  outputcol_Y3 = paste("pro",scenario,"Y3","q4",sep="_")
  
  proval_Y1 = 1-input$deep_slider_pro_Y1/100
  proval_Y2 = 1-input$deep_slider_pro_Y2/100
  proval_Y3 = 1-input$deep_slider_pro_Y3/100
  
  # Setting yearly values
  outputtable <- outputtable %>%
    rowwise() %>%
    mutate(!!outputcol_Y1 := !!proval_Y1) %>%
    mutate(!!outputcol_Y2 := !!proval_Y2) %>%
    mutate(!!outputcol_Y3 := !!proval_Y3)
  
  # Converting to quarterly values
  for(x in c("min","mid","max")){
    for(y in c("Y1","Y2","Y3")){
      if(x == scenario){
        year = as.numeric(substr(y,nchar(y),nchar(y)))
        procol = as.name(paste("pro",x,y,sep="_"))
        
        input_proval = ifelse(year==1,proval_Y1,ifelse(year==2,proval_Y2,proval_Y3))

        input_proval_prev = ifelse(year==1,1,ifelse(year==2,proval_Y1,proval_Y2))
        
        inputcol_q0 <- as.name(paste("pro",x,paste0("Y",year-1),"q4",sep="_"))
        inputcol_q4 <- as.name(paste("pro",x,y,"q4",sep="_"))
        
        outputcol_q1 <- paste("pro",x,y,"q1",sep="_")
        outputcol_q2 <- paste("pro",x,y,"q2",sep="_")
        outputcol_q3 <- paste("pro",x,y,"q3",sep="_")
        outputcol_q4 <- paste("pro",x,y,"q4",sep="_")
        
        outputtable = outputtable %>%
          rowwise() %>%
          mutate(!!outputcol_q1:=ifelse(year==0,1,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*1)) %>%
          mutate(!!outputcol_q2:=ifelse(year==0,1-(1-!!inputcol_q4)/3*1,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*2)) %>%
          mutate(!!outputcol_q3:=ifelse(year==0,1-(1-!!inputcol_q4)/3*2,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*3)) %>%
          mutate(!!outputcol_q4:= !!inputcol_q4)
          
        }
      }
    }
  rvalues$deep_final_table_overwritten <- outputtable
}

# Function to update demand driver values
f_deep_update_dd <- function(){
  print("Updating demand values")
  scenario = ifelse(input$deep_scenario=="Conservative","min",ifelse(input$deep_scenario=="Realistic","mid","max"))
  outputtable = rvalues$deep_final_table_overwritten
  
  outputcol_Y1 = paste("dd",scenario,"Y1","q4",sep="_")
  outputcol_Y2 = paste("dd",scenario,"Y2","q4",sep="_")
  outputcol_Y3 = paste("dd",scenario,"Y3","q4",sep="_")
  
  ddval_Y1 = input$deep_slider_dd_Y1/100+1
  ddval_Y2 = input$deep_slider_dd_Y2/100+1
  ddval_Y3 = input$deep_slider_dd_Y3/100+1
  
  # Setting yearly values
  outputtable <- outputtable %>%
    rowwise() %>%
    mutate(!!outputcol_Y1 := !!ddval_Y1) %>%
    mutate(!!outputcol_Y2 := !!ddval_Y2) %>%
    mutate(!!outputcol_Y3 := !!ddval_Y3)
  
  # Converting to quarterly values
  for(x in c("min","mid","max")){
    for(y in c("Y1","Y2","Y3")){
      if(x == scenario){
        year = as.numeric(substr(y,nchar(y),nchar(y)))
        ddcol = as.name(paste("dd",x,y,sep="_"))
        
        input_ddval = ifelse(year==1,ddval_Y1,ifelse(year==2,ddval_Y2,ddval_Y3))
        
        input_ddval_prev = ifelse(year==1,1,ifelse(year==2,ddval_Y1,ddval_Y2))
        
        inputcol_q0 <- as.name(paste("dd",x,paste0("Y",year-1),"q4",sep="_"))
        inputcol_q4 <- as.name(paste("dd",x,y,"q4",sep="_"))
        
        outputcol_q1 <- paste("dd",x,y,"q1",sep="_")
        outputcol_q2 <- paste("dd",x,y,"q2",sep="_")
        outputcol_q3 <- paste("dd",x,y,"q3",sep="_")
        outputcol_q4 <- paste("dd",x,y,"q4",sep="_")
        
        outputtable = outputtable %>%
          rowwise() %>%
          mutate(!!outputcol_q1:=ifelse(year==0,1,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*1)) %>%
          mutate(!!outputcol_q2:=ifelse(year==0,1-(1-!!inputcol_q4)/3*1,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*2)) %>%
          mutate(!!outputcol_q3:=ifelse(year==0,1-(1-!!inputcol_q4)/3*2,!!inputcol_q0-(!!inputcol_q0-!!inputcol_q4)/4*3)) %>%
          mutate(!!outputcol_q4:= !!inputcol_q4)
        
      }
    }
  }
  rvalues$deep_final_table_overwritten <- outputtable
}

# Function to update outsourcing values

# f_deep_update_out <- function(){
#   print("Updating outsourcing values")
#   scenario = ifelse(input$deep_scenario=="Conservative","min",ifelse(input$deep_scenario=="Realistic","mid","max"))
#   outputtable = rvalues$deep_final_table_overwritten
# 
#   out_considered = input$deep_out_considered
#   out_year = input$deep_out_year
#   out_quarter = input$deep_out_quarter
# 
#   # Turning it into quarters
#   
#   for(x in c("min","mid","max")){
#     for(y in c("Y1","Y2","Y3")){
#       for(z in c("q1","q2","q3","q4")){
#         if(x == scenario){
#           year = as.numeric(substr(y,nchar(y),nchar(y)))
#           quarter = as.numeric(substr(z,nchar(z),nchar(z)))
#           outcol = as.name(paste("out",x,y,sep="_"))
#           
#           input_considered = out_considered
#           input_yesno = as.name("out_yesno")
#           input_year = as.numeric(str_sub(out_year,2,2))
#           input_quarter = as.numeric(str_sub(out_quarter,2,2))
#           
#           outputcol = paste("out",x,y,z,sep="_")
#           
#           outputtable = outputtable %>%
#             rowwise() %>%
#             mutate(!!outputcol:=ifelse(!!input_yesno == 0 | !!input_considered==FALSE,1,
#                                        ifelse(!!year<!!input_year,1,
#                                               ifelse(!!year>!!input_year,0,
#                                                      ifelse(!!quarter<!!input_quarter,1,0)
#                                               )
#                                        )
#             ))
#         }
#       }
#     }
#   }
#   rvalues$deep_final_table_overwritten <- outputtable
# }

  

# Triggering updates on scenario save  -------------------------------------
rvalues$deep_baseline_changed = 0
rvalues$deep_aut_changed = 0
rvalues$deep_pro_changed = 0
rvalues$deep_dd_changed = 0
rvalues$deep_out_changed = 0

##
observeEvent({
  input$deep_slider_baseline
},
{
  rvalues$deep_baseline_changed = 1
})

observeEvent({
  input$deep_slider_aut_Y1
  input$deep_slider_aut_Y2
  input$deep_slider_aut_Y3
  
  input$deep_aut_quarter_Y1
  input$deep_aut_quarter_Y2
  input$deep_aut_quarter_Y3
},
{
  rvalues$deep_aut_changed = 1
})

observeEvent({
  input$deep_slider_pro_Y1
  input$deep_slider_pro_Y2
  input$deep_slider_pro_Y3
},
{
  rvalues$deep_pro_changed = 1
})

observeEvent({
  input$deep_slider_dd_Y1
  input$deep_slider_dd_Y2
  input$deep_slider_dd_Y3
},
{
  rvalues$deep_dd_changed = 1
})

# observeEvent({
#   input$deep_out_considered
#   input$deep_out_year
#   input$deep_out_quarter
# },
# {
#   rvalues$deep_out_changed = 1
# })






# Saving slider values -----------------------------------------------------------

observeEvent({
  input$deep_save
},{
  print("Observed automation slider change")
  
  withProgress(message="Saving slider values",value=0,{
    
    if(rvalues$deep_generated!=0){
      
      incProgress(0.1)
      if(rvalues$deep_baseline_changed==1){
        print("Updating baseline values")
        f_deep_update_baseline() # Updates baseline values
        rvalues$deep_baseline_changed = 0
      }
      
      incProgress(0.1)
      if(rvalues$deep_aut_changed==1){
        f_deep_update_aut()
        print("autchange: deep_hc_table")
        rvalues$deep_aut_changed = 0
      }
  
      incProgress(0.1)
      if(rvalues$deep_pro_changed==1){
        f_deep_update_pro()
        print("prochange: deep_hc_table")
        rvalues$deep_pro_changed = 0
      }
      
      incProgress(0.1)
      if(rvalues$deep_dd_changed==1){
        f_deep_update_dd()
        print("prochange: deep_hc_table")
        rvalues$deep_dd_changed = 0
      }
      
      incProgress(0.1)
      # if(rvalues$deep_out_changed==1){
      #   f_deep_update_out()
      #   print("outchange: deep_hc_table")
      #   rvalues$deep_out_changed = 0
      # }
      
      incProgress(0.1)

      rvalues$deep_updated <- 1
      

      # Creating summarised table

      fulltable = rvalues$deep_final_table_overwritten %>%
        select(-unique_identifier,-baseline_bottom,-baseline_top)
      
      totalhc <- sum(fulltable$baseline_mid)
      fulltable$baseline_mid2 <- fulltable$baseline_mid
      cols_fulltable <- colnames(select(fulltable,-baseline_min,-baseline_mid,-baseline_max,-baseline_mid2))
      
      fulltable_sum <- fulltable
      
      for(x in cols_fulltable){
        inputcol = as.name(x)
        outputcol = x
        
        fulltable_sum <-  fulltable_sum %>% 
          rowwise() %>%
          mutate(!!outputcol := !!inputcol / totalhc * baseline_mid2)
      }
      
      fulltable_sum <- fulltable_sum %>%
        select(-baseline_mid2) %>%
        group_by() %>%
        summarise_all(funs(sum),na.rm=TRUE)
      
      rvalues$deep_final_table_overwritten_sum <- fulltable_sum

      print("Set deep_final_table_overwritten")
      
    }
    })
})


# Reset values ------------------------------------------------------------

observeEvent(input$deep_reset,{
  rvalues$deep_updated <- 0
  f_deep_sliders_update()
})

# Refreshing graphs -------------------------------------------------------
observeEvent({
  input$deep_refresh_graphs
},{
  print("Refreshing graphs start")
  # Observing event to trigger graphs ---------------------------------------
  deep_hc_table() # Turns multipliers into headcount
  rvalues$deep_scenario
  final_line_table_deep()
  input$deep_original_or_new
  deep_waterfall_table()
  print("Refreshing graphs end")
})


# Updating sliders when scenario changed ----------------------------------

observeEvent(input$deep_scenario,{
  if(rvalues$deep_generated!=0){
    f_deep_sliders_update()
    }
  })
    
# This then overwrites the big table
  
# Then the big table calculates new values
  


# ... Turning multipliers into headcount --------------------------------------

deep_hc_table <- reactive({

  print("deep_hc_table start")
  input$deep_refresh_graphs
  data = isolate(rvalues$deep_final_table_overwritten)
  
  # Calculating new final multiplier values
  for(x in c("min","mid","max")){
    for(y in c("Y1","Y2","Y3")){
      for(z in c("q1","q2","q3","q4")) {
        input_aut = as.name(paste("aut",x,y,z,sep="_"))
        input_pro = as.name(paste("pro",x,y,z,sep="_"))
        input_dd = as.name(paste("dd",x,y,z,sep="_"))
        #input_out = as.name(paste("out",x,y,z,sep="_"))
        
        outputcol = paste("finalmult",x,y,z,sep="_")
        
        data <- data %>%
          rowwise() %>%
          mutate(!!outputcol := !!input_aut * !!input_pro * !!input_dd)
              #   *!!input_out)
      }
    }
  }
  
  data$baseline_min_2 <- data$baseline_min
  data$baseline_mid_2 <- data$baseline_mid
  data$baseline_max_2 <- data$baseline_max
  
  # Without conversion to headcount
  rvalues$deep_final_table_mult <- data
  
  # Converting to headcount
  multipliercols <- colnames(select(data,-unique_identifier,-baseline_bottom,-baseline_top,-baseline_min,-baseline_mid,-baseline_max,-baseline_min_2,-baseline_mid_2,-baseline_max_2,-out_yesno))

  
  for(x in multipliercols){
    inputcol = as.name(x)
    
    scenario = str_split(x,"_")[[1]][2]
    inputbaseline = as.name(paste("baseline",scenario,"2",sep="_"))
    
    data <- data %>%
      rowwise() %>%
      mutate(!!x := ceiling(!!inputbaseline * !!inputcol))
  }
  

# Converting to cost ------------------------------------------------------
  # Calculating final cost values
  final_cost <- left_join(data,data_sectioncost,by="unique_identifier")
  final_cost <- final_cost %>%
    select(-baseline_bottom,-baseline_top,-grep("baselinepos",colnames(final_cost)))
  
  costcols <- colnames(select(final_cost,-unique_identifier,-fte_cost_avg))
  for(x in costcols){
    xname <- as.name(x)
    
    final_cost <- final_cost %>%
      rowwise() %>%
      mutate(!!x:=!!as.name(x)*fte_cost_avg)
  }
  
  rvalues$deep_final_table_cost <- final_cost
  
  print("deep_hc_table end")
  
  rvalues$deep_final_table_hc <- data
})


# Final line table deep ---------------------------------------------------

final_line_table_deep <- reactive({
  print("final_line_table_deep start")
  
  input$deep_refresh_graphs
  
  if(isolate(input$deep_headcount_or_cost) == "Headcount"){
    final_hc = rvalues$deep_final_table_hc
  } else {
    final_hc = rvalues$deep_final_table_cost
  }
  
  # Taking the "cumu" out of supply columns so str split later works
  # colnames(final_hc) <- gsub("_cumu","",colnames(final_hc))
  final_hc2 <- final_hc %>% select(grep("finalmult",colnames(final_hc)),grep("supply",colnames(final_hc))) %>%
    group_by() %>%
    summarise_all(funs(sum),na.rm=TRUE) %>%
    gather("Key","Value")
  final_hc2 <- final_hc2 %>%
    mutate("supplyordemand" = sapply(str_split(Key,"_"),"[[",1)) %>%
    mutate("scenario" = sapply(str_split(Key,"_"),"[[",2)) %>%
    mutate("year" = as.numeric(substr(sapply(str_split(Key,"_"),"[[",3),2,2))) %>%
    mutate("quarter" = as.numeric(substr(sapply(str_split(Key,"_"),"[[",4),2,2))) %>%
    mutate("Scenario" = paste(supplyordemand,scenario,sep="_")) %>%
    select(-Key) %>%
    mutate("total_quarter" = (year*4)+quarter-4) %>%
    mutate("Value"=ceiling(Value)) %>%
    mutate("Year & Quarter" = paste0("Y",year," ","Q",quarter))
  
  print("final_line_table_deep end")
  
  final_hc2$Scenario <- gsub("finalmult_min","Demand - Conservative",final_hc2$Scenario)
  final_hc2$Scenario <- gsub("finalmult_mid","Demand - Realistic",final_hc2$Scenario)
  final_hc2$Scenario <- gsub("finalmult_max","Demand - Optimistic",final_hc2$Scenario)
  final_hc2$Scenario <- gsub("supply_min","Supply - Conservative",final_hc2$Scenario)
  final_hc2$Scenario <- gsub("supply_mid","Supply - Realistic",final_hc2$Scenario)
  final_hc2$Scenario <- gsub("supply_max","Supply - Optimistic",final_hc2$Scenario)
  
  final_hc2 <- final_hc2 %>%
    rowwise() %>%
    mutate("Total" = ifelse(isolate(input$deep_headcount_or_cost)=="Headcount",
                            round(Value,0),
                            ifelse(Value>1000000,paste0(format(round(Value / 1e6, 1), trim = TRUE), "Mn"),
                                   paste0(format(round(Value / 1e3, 1), trim = TRUE), "K")
                            )))
  
  
  return(final_hc2)
})

# Line chart deep --------------------------------------------------------------


output$line_deep <- renderPlotly({
  print(paste0("Rendering deep line, deep generated = ",rvalues$deep_generated))
  
  input$deep_refresh_graphs
  
  if(rvalues$deep_generated == 0){
    text = paste("\n   Please generate data in the section selected tab \n",
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
  

# EXEC OR DEEP ------------------------------------------------------------

  final_hc2 <- if(isolate(input$deep_original_or_new) == "Use original values"){
    # if(isolate(input$deep_headcount_or_cost) == "Headcount"){
      final_hc2 = final_line_table()
    #}
  } else {
    final_hc2 = final_line_table_deep()
  }

  linegraph <- ggplot(final_hc2,aes(x=total_quarter,y=Value,colour=Scenario,label=Total,text=`Year & Quarter`,group=supplyordemand)) +
    geom_line(size=1)+
    CustomTheme +
    #ggtitle("Headcount Forecast")+
    xlab("Year")+
    labs(x="Quarter (End)",y=ifelse(isolate(input$deep_headcount_or_cost)=="Headcount","Headcount","Monthly Cost (SAR)"))+
    theme(legend.position = "bottom")+
    #scale_colour_brewer(palette="Blues",direction = -1)
    scale_colour_manual(values=c("Demand - Conservative" = "#002C77","Demand - Realistic" = "#ED2C67","Demand - Optimistic" = "#00A8C8","Supply - Conservative" = "#0FB694","Supply - Realistic"="#FBAE17","Supply - Optimistic"="#72BE44"))+
    scale_x_continuous(breaks=1:12,labels = paste0(c("Year 1 Q1","","","Year 1 Q4","","","","Year 2 Q4","","","","Year 3 Q4"))) +
    scale_y_continuous(labels=comma)
    #scale_y_continuous(breaks=pretty_breaks(n=10),expand=c(0,0),labels=comma)
  
  linegraph_ly <- ggplotly(linegraph,tooltip=c("Scenario","Year & Quarter","Total"),dynamicTicks=TRUE) %>% 
    config(displaylogo=FALSE,collaborate=FALSE,modeBarButtonsToRemove=c(
      'sendDataToCloud',
      'toImage',
      'toggleSpikelines')) %>%
    layout(legend = list(orientation="h",y=-0.3),font=list(family="Source Sans Pro",size=18))
  
  linegraph_ly

})


# Deep waterfall chart calcs ----------------------------------------------

# ...Calculating changes for waterfall charts --------------------------------
deep_waterfall_table <- reactive({
  deep_hc_table()
  
  input$deep_refresh_graphs
  
  #table <- rvalues$final_hc
  #table <- rvalues$exec_table_output
  
  # final_hc_change > deep_final_hc
  deep_final_hc = rvalues$deep_final_table_mult
  data_baseline <-  rvalues$data_baseline
  data_hc <- rvalues$data_hc
  
  scenario = ifelse(isolate(input$deep_scenario)=="Conservative","min",ifelse(isolate(input$deep_scenario)=="Realistic","mid","max"))
  
  multiplier_cols <- c(
    paste("aut",scenario,"Y3","q4",sep="_"),
    paste("pro",scenario,"Y3","q4",sep="_"),
    paste("dd",scenario,"Y3","q4",sep="_")
    #paste("out",scenario,"Y3","q4",sep="_")
  )
  
  #Calculating headcount changes
  for(x in multiplier_cols){
    inputcol = as.name(x)
    outputcol = paste0(x,"_change")

    scenario = str_split(x,"_")[[1]][2]
    inputbaseline = as.name(paste("baseline",scenario,sep="_"))

    deep_final_hc <- deep_final_hc %>%
      mutate(!!outputcol := !!inputbaseline * (!!inputcol-1)) %>%
      select(-!!inputcol)
  }
  
  # Calculating final headcounts
  multiplier_cols <- c(
    paste("finalmult",scenario,"Y1","q4",sep="_"),
    paste("finalmult",scenario,"Y2","q4",sep="_"),
    paste("finalmult",scenario,"Y3","q4",sep="_")
    )
  
  for(x in multiplier_cols){
    inputcol = as.name(x)
    outputcol = paste0(x)
    
    scenario = str_split(x,"_")[[1]][2]
    inputbaseline = as.name(paste("baseline",scenario,sep="_"))
    
    deep_final_hc <- deep_final_hc %>%
      mutate(!!outputcol := ceiling(!!inputbaseline * !!inputcol))
  }
  
  # ...Calculating additions and reductions ------------------------------------
  
  data_baseline_adred <- left_join(data_baseline,data_hc,by="unique_identifier")
  for(x in c("min","mid","max")){
    
    
    input_baseline = as.name(paste("baseline",x,sep="_"))
    input_hc = as.name("hc_chosen")
    output_additions = paste("additions",x,sep="_")
    output_reductions = paste("reductions",x,sep="_")
    
    data_baseline_adred <- data_baseline_adred %>%
      rowwise() %>%
      mutate(!!output_additions := ifelse(!!input_baseline>!!input_hc,!!input_baseline-!!input_hc,0)) %>%
      mutate(!!output_reductions := ifelse(!!input_baseline<!!input_hc,!!input_baseline-!!input_hc,0))
    
  }
  
  # ...Calculating figures for waterfall charts --------------------------------
  data_waterfall_baseline <- data_baseline_adred %>% select(unique_identifier,hc_chosen,baseline_min,baseline_mid,baseline_max,additions_min,additions_mid,additions_max,reductions_min,reductions_mid,reductions_max)
  data_waterfall_baseline <- left_join(select(data_sectioninfo,unique_identifier,l0,l1,l2,l3,l4,segment),data_waterfall_baseline,by="unique_identifier")
  data_waterfall_baseline <- data_waterfall_baseline %>% 
    filter(if(isolate(input$deep_chosenl0)=="All") l0 != "abc" else l0 == isolate(input$deep_chosenl0)) %>%
    filter(if(isolate(input$deep_chosenl1)=="All") l1 != "abc" else l1 == isolate(input$deep_chosenl1)) %>%
    filter(if(isolate(input$deep_chosenl2)=="All") l2 != "abc" else l2 == isolate(input$deep_chosenl2)) %>%
    filter(if(isolate(input$deep_chosenl3)=="All") l3 != "abc" else l3 == isolate(input$deep_chosenl3)) %>%
    filter(if(isolate(input$deep_chosenl4)=="All") l4 != "abc" else l4 == isolate(input$deep_chosenl4)) %>%
    filter(segment %in% isolate(input$segment_list_deep))
  
  data_waterfall_finals <- deep_final_hc %>% select(unique_identifier,grep("finalmult",colnames(deep_final_hc)))
  
  data_waterfall_finals <- data_waterfall_finals %>% select(unique_identifier,grep("q4",colnames(data_waterfall_finals)))
  
  data_waterfall_change <- deep_final_hc %>% select(unique_identifier,grep("Y3",colnames(deep_final_hc)))
  data_waterfall_change <- data_waterfall_change %>% select(unique_identifier,grep("q4",colnames(data_waterfall_change)))
  
  data_final_all <- left_join(data_waterfall_baseline,data_waterfall_finals,by="unique_identifier")
  
  data_final_all <- left_join(data_final_all,select(data_waterfall_change,
                                                    unique_identifier,grep(scenario,colnames(data_waterfall_change)),-grep("finalmult",colnames(data_waterfall_change))),
                              by="unique_identifier")
  #data_final_all <- left_join(data_final_all,select(data_sectioninfo,unique_identifier,segment),by="unique_identifier")
  
  data_final_all_scenario <- select(data_final_all,unique_identifier,segment,hc_chosen,grep(scenario,colnames(data_final_all)))
  data_final_all_scenario <- left_join(data_final_all_scenario,data_sectioncost,by="unique_identifier")
  
  colnames(data_final_all_scenario) <- gsub(paste0("_",scenario),"",colnames(data_final_all_scenario))
  
  baseline_output = "baseline"
  baseline_input = as.name("baseline")
  additions_output = "additions"
  additions_input = as.name("additions")
  reductions_output = "reductions"
  reductions_input = as.name("reductions")
  aut_output = "aut_Y3_q4_change"
  aut_input = as.name("aut_Y3_q4_change")
  pro_output = "pro_Y3_q4_change"
  pro_input = as.name("pro_Y3_q4_change")
  dd_output = "dd_Y3_q4_change"
  dd_input = as.name("dd_Y3_q4_change")
  #out_output = "out_Y3_q4_change"
  #out_input = as.name("out_Y3_q4_change")
  finalmult_output = "finalmult_Y3_q4"
  finalmult_input = as.name("finalmult_Y3_q4")
  
  if(isolate(input$deep_headcount_or_cost)=="Cost"){
    data_final_all_scenario <- data_final_all_scenario %>%
      rowwise() %>%
      mutate(hc_chosen = hc_chosen * fte_cost_avg) %>%
      mutate(!!baseline_output := !!baseline_input * fte_cost_avg) %>%
      mutate(!!additions_output := !!additions_input * fte_cost_avg) %>%
      mutate(!!reductions_output := !!reductions_input * fte_cost_avg) %>%
      mutate(!!aut_output := !!aut_input * fte_cost_avg) %>%
      mutate(!!pro_output := !!pro_input * fte_cost_avg) %>%
      mutate(!!dd_output := !!dd_input * fte_cost_avg) %>%
      #mutate(!!out_output := !!out_input * fte_cost_avg) %>%
      mutate(!!finalmult_output := !!finalmult_input * fte_cost_avg)
    
  }
  

  
  if(isolate(input$deep_original_or_new) == "Use original values"){
    final_waterfall_table()
    if(isolate(input$deep_headcount_or_cost)=="Headcount"){
      data_final_all_scenario_sum = rvalues$final_waterfall_table_hc
    } else {
      data_final_all_scenario_sum = rvalues$final_waterfall_table_cost
    }
  } else {
    data_final_all_scenario_sum <- data_final_all_scenario %>% group_by(segment) %>% summarise_all(funs(sum),na.rm=TRUE)
  }
  
  rvalues$deep_waterfall_table <- data_final_all_scenario_sum
  return(data_final_all_scenario_sum)
})


# Waterfall plot ----------------------------------------------------------

output$deep_waterfall <- renderPlot({
  
  # if(is.null(input$l0_list_exec) | is.null(input$l1_list_exec) | is.null(input$l2_list_exec)){
  #   text = paste("Generating Graph")
  #   blankchart <- ggplot() +
  #     #annotate("text", x = 4, y = 25, size=8, label = text) +
  #     theme_bw() +
  #     theme(panel.grid.major=element_blank(),
  #           panel.grid.minor=element_blank(),
  #           axis.ticks=element_blank(),
  #           axis.text = element_blank(),
  #           axis.title=element_blank(),
  #           panel.grid = element_blank(),
  #           panel.border=element_blank())
  #   return(blankchart)
  # }
  
  input$deep_refresh_graphs
  
  deep_waterfall_table()
  data = rvalues$deep_waterfall_table
  print("waterfall table Deep ===================")
  print(data)
  # Waterfall functions
  
  data <- data %>%
    select(-grep("out_",colnames(data)))
  
  which_row <- function(x) {
    tmp <- long[long$segment == "szum" & long$variable == x, ] %>%
      rownames()  %>%
      as.numeric()
    return(tmp)
  }
  
  get_position <- function(x) {
    val <- 0
    if(dim(x)[1]!=1){
      for (i in 2:dim(x)[1]) {
        x$value_half[i] <- abs(x$value[i-1]) + x$value_half[i] + val
        val <- abs(x$value[i-1]) + val
      }
    }
    #x$value_half <- rev(x$value_half)
    return(x)
  }
  
  # ... Waterfall code ----
  
  scenario = ifelse(isolate(input$deep_scenario)=="Conservative","min",ifelse(isolate(input$deep_scenario)=="Realistic","mid","max"))
  
  tmp <- data
  #write_csv(tmp,"Test CSV waterfall.csv")
  
  colnames <- colnames(select(tmp,-segment,-unique_identifier))
  print(colnames)
  for(x in colnames){
    input_x <- x
    output_x <- as.name(x)
    tmp <- tmp %>%
      mutate(!!input_x := as.numeric(!!output_x))
  }
  
  colnames(tmp) <- gsub(paste0("_",scenario),"",colnames(tmp))
  
  # detect whether dd is negative or positive
  if (sum(tmp$dd_Y3_q4_change) > 0) {
    tmp_dd <- "positive"
  } else {
    tmp_dd <- "negative"
  }
  
  #write_csv(tmp,"Waterfall calcs step 1.csv")
  
  # Correcting for the fact that changes stack on top of each other, and might not be great enough
  # for individual sections to cause a drop of a full person
  tmp <- tmp %>%
    mutate(aut_Y3_q4_change_cumu = (aut_Y3_q4_change/baseline)*(baseline)) %>%
    mutate(pro_Y3_q4_change_cumu = (pro_Y3_q4_change/baseline)*(baseline + aut_Y3_q4_change_cumu)) %>%
    mutate(dd_Y3_q4_change_cumu = (dd_Y3_q4_change/baseline)*(baseline + aut_Y3_q4_change_cumu + pro_Y3_q4_change_cumu)) %>%
    
    # Implied change, not taking into account individual sections get rounded up at end to give whole person
    mutate(implied_change = aut_Y3_q4_change_cumu + pro_Y3_q4_change_cumu + dd_Y3_q4_change_cumu) %>% 
    
    # Actual change, with each section rounded up to give whole number of people
    mutate(actual_change = finalmult_Y3_q4 - baseline)
  
  tmp <- tmp %>%
    
    # Works out difference of total implied and actual, then adds a weighted portion of that to the final number for each
    
    mutate(aut_Y3_q4_change_weight = (actual_change - implied_change) * (aut_Y3_q4_change_cumu / implied_change)) %>%
    mutate(pro_Y3_q4_change_weight = (actual_change - implied_change) * (pro_Y3_q4_change_cumu / implied_change)) %>%
    mutate(dd_Y3_q4_change_weight = (actual_change - implied_change) * (dd_Y3_q4_change_cumu / implied_change)) %>%
    
    # Headcount increases above multiplier cannot result in positive aut or pro change, so if result > 0 must be in dd
    mutate(aut_Y3_q4_change_final = ifelse(aut_Y3_q4_change_cumu + aut_Y3_q4_change_weight > 0,0,round(aut_Y3_q4_change_cumu + aut_Y3_q4_change_weight,0))) %>%
    mutate(pro_Y3_q4_change_final = ifelse(pro_Y3_q4_change_cumu + pro_Y3_q4_change_weight > 0,0,round(pro_Y3_q4_change_cumu + pro_Y3_q4_change_weight,0))) %>%
    mutate(dd_Y3_q4_change_final = actual_change - aut_Y3_q4_change_final - pro_Y3_q4_change_final)
  
  
  tmp <- tmp %>%
    # Renaming back to change columns
    mutate(aut_Y3_q4_change = aut_Y3_q4_change_final) %>%
    mutate(pro_Y3_q4_change = pro_Y3_q4_change_final) %>%
    mutate(dd_Y3_q4_change = dd_Y3_q4_change_final) %>%
    
    # Removing unneeded columns
    select(-pro_Y3_q4_change_cumu,-aut_Y3_q4_change_cumu,-dd_Y3_q4_change_cumu,
           -pro_Y3_q4_change_weight,-aut_Y3_q4_change_weight,-dd_Y3_q4_change_weight,
           -pro_Y3_q4_change_final,-aut_Y3_q4_change_final,-dd_Y3_q4_change_final)
  
  #mutate(out_Y3_q4_change_cumu = (out_Y3_q4_change/baseline)*(baseline + aut_Y3_q4_change + pro_Y3_q4_change + dd_Y3_q4_change)) %>%
  #mutate("multiplier" = (finalmult_Y3_q4 - baseline) / (pro_Y3_q4_change_cumu + aut_Y3_q4_change_cumu + dd_Y3_q4_change_cumu))
  #mutate("multiplier" = (finalmult_Y3_q4 - baseline) / (pro_Y3_q4_change_cumu + aut_Y3_q4_change_cumu + dd_Y3_q4_change_cumu + out_Y3_q4_change_cumu))
  
  
  
  #multiplier = (sum(tmp$finalmult_Y3_q4) - sum(tmp$baseline)) / (sum(tmp$pro_Y3_q4_change_cumu) + sum(tmp$aut_Y3_q4_change_cumu) + sum(tmp$dd_Y3_q4_change_cumu) + sum(tmp$out_Y3_q4_change_cumu))
  #write_csv(tmp,"Waterfall calcs step 2.csv")
  
  # tmp <- tmp %>%
  #   mutate(aut_Y3_q4_change = aut_Y3_q4_change_cumu * multiplier) %>%
  #   mutate(pro_Y3_q4_change = pro_Y3_q4_change_cumu * multiplier) %>%
  #   mutate(dd_Y3_q4_change = dd_Y3_q4_change_cumu * multiplier) %>%
  #   #mutate(out_Y3_q4_change = out_Y3_q4_change_cumu * multiplier) %>%
  #   select(-pro_Y3_q4_change_cumu,-aut_Y3_q4_change_cumu,-dd_Y3_q4_change_cumu,-multiplier)
  #   #select(-pro_Y3_q4_change_cumu,-aut_Y3_q4_change_cumu,-dd_Y3_q4_change_cumu,-out_Y3_q4_change_cumu,-multiplier)
  
  #write_csv(tmp,"Waterfall calcs step 3.csv")
  
  tmp$segment <- paste0("segment_", tmp$segment)
  
  
  # finalmult_Y1_q4
  tmp_segment <- c("segment", "hc_chosen", "additions",
                   "reductions", "baseline",
                   "aut_Y3_q4_change", "pro_Y3_q4_change",
                   "dd_Y3_q4_change", 
                   "finalmult_Y3_q4")
  #"out_Y3_q4_change",
  
  # calculate sum for each columns
  tmp <- tmp[tmp_segment]
  tmp[(dim(tmp)[1] + 1), ] <- c("szum", apply(tmp[2:9], 2, sum))
  
  # change from wide to long dataframe format
  long <- melt(tmp, id.vars = "segment")
  # add starting color
  long$colour <- "#000000"
  # ensure that value is numeric
  long$value <- as.numeric(as.character(long$value))
  
  # delete 'segment == szum & variable == reductions'
  # no need it
  long <- long[-which_row("reductions"), ]
  # recalculate the number of rows
  rownames(long) <- 1:dim(long)[1]
  # save original data for further use
  long$placeholder <- long$segment
  # delete 'segment == szum & variable == finalmult_Y1_q4'
  # no need it
  long <- long[-which_row("finalmult_Y3_q4"), ]
  # recalculate the number of rows
  rownames(long) <- 1:dim(long)[1]
  
  # calculate whitespace for additions
  long[which_row("additions"), ]$value <- long[which_row("additions"), ]$value +
    long[which_row("hc_chosen"), ]$value
  # calculate whitespace for reductions
  long[which_row("additions"), ]$value <- long[which_row("additions"), ]$value + 
    sum(long[long$variable == "reductions" & long$segment != "szum", ]$value)
  # change additions to reductions
  long[long$segment == "szum" & long$variable == "additions", ]$variable <- "reductions"
  # change hc_chosen to additions
  long[long$segment == "szum" & long$variable == "hc_chosen", ]$variable <- "additions"
  # recalculate the number of rows
  rownames(long) <- 1:dim(long)[1]
  
  # calculate whitespace for aut_Y3_q4_change
  long[which_row("baseline"), ]$value <- long[which_row("baseline"), ]$value +
    sum(long[long$variable == "aut_Y3_q4_change" & long$segment != "szum", ]$value)
  # calculate whitespace for pro_Y3_q4_change
  long[which_row("aut_Y3_q4_change"), ]$value <- long[which_row("baseline"), ]$value + 
    sum(long[long$variable == "pro_Y3_q4_change"  & long$segment != "szum", ]$value)
  
  if (long[which_row("dd_Y3_q4_change"), ]$value > 0) {
    long[which_row("pro_Y3_q4_change"), ]$value <- long[which_row("aut_Y3_q4_change"), ]$value
  } else {
    long[which_row("pro_Y3_q4_change"), ]$value <- long[which_row("aut_Y3_q4_change"), ]$value +
      sum(long[long$variable == "dd_Y3_q4_change" & long$segment != "szum", ]$value) 
  }
  
  # if (long[which_row("dd_Y3_q4_change"), ]$value > 0) {
  #   long[which_row("dd_Y3_q4_change"), ]$value <- long[which_row("pro_Y3_q4_change"), ]$value +
  #     sum(long[long$variable == "dd_Y3_q4_change" & long$segment != "szum", ]$value) +
  #     sum(long[long$variable == "out_Y3_q4_change" & long$segment != "szum", ]$value)
  # } else {
  #   long[which_row("dd_Y3_q4_change"), ]$value <- long[which_row("pro_Y3_q4_change"), ]$value +
  #     #sum(long[long$variable == "dd_Y3_q4_change" & long$segment != "szum", ]$value) +
  #     sum(long[long$variable == "out_Y3_q4_change" & long$segment != "szum", ]$value)
  # }
  # 
  
  # delete 'segment == szum & variable == out_Y3_q4_change'
  # no need it
  #long <- long[-which_row("out_Y3_q4_change"), ]
  long <- long[-which_row("dd_Y3_q4_change"), ]
  
  #  change dd_Y3_q4_change to out_Y3_q4_change
  #long[long$segment == "szum" & long$variable == "dd_Y3_q4_change", ]$variable <- "out_Y3_q4_change"
  
  #  change pro_Y3_q4_change to dd_Y3_q4_change
  long[long$segment == "szum" & long$variable == "pro_Y3_q4_change", ]$variable <- "dd_Y3_q4_change"
  #  change aut_Y3_q4_change to pro_Y3_q4_change
  long[long$segment == "szum" & long$variable == "aut_Y3_q4_change", ]$variable <- "pro_Y3_q4_change"
  #  change baseline to aut_Y3_q4_change
  long[long$segment == "szum" & long$variable == "baseline", ]$variable <- "aut_Y3_q4_change"
  
  # recalculate the number of rows
  rownames(long) <- 1:dim(long)[1]
  # add rownames to dataframe
  # it needs for the waterfall graph
  long$osztaly <- rownames(long)
  long$osztaly <- as.factor(long$osztaly)
  
  # create classes for the waterfallgraph
  tmp_class <- as.numeric(table(long$variable))
  long$osztaly <- paste0("class", 1:length(tmp_class)) %>%
    rep(., tmp_class) %>%
    as.factor()
  # segment should be unique factor
  long$segment <- as.factor(1:dim(long)[1])
  
  # 
  long$value_half <- abs(long$value) / 2
  
  # split by columns (osztaly)
  long_split <- split(long, long$osztaly)
  # get positions for text ISSUE
  long_split <- lapply(long_split, function(x) get_position(x))
  
  # reorder data
  long_split <- lapply(long_split, function(x) x[order(as.numeric(x$segment), decreasing = TRUE), ])
  long_split <- do.call(rbind, long_split)
  # change negative values to positive
  long_split$value <- abs(long_split$value)
  # recalculate the number of rows
  rownames(long_split) <- 1:dim(long_split)[1]
  # segment should be unique factor
  long_split$segment <- as.factor(1:dim(long_split)[1])
  
  # change the colours of the whitespaces to white
  long_split$colour[which(long_split$placeholder == "szum")] <- "#FFFFFF"
  
  # generate 5 random colours and add to them to the right category
  # tmp_colour <- randomColor(5)
  # 
  # if("segment_Core" %in% long_split$placeholder){
  #   long_split[long_split$placeholder == "segment_Core", ]$colour <- "steelblue1"
  # }
  # 
  # if("segment_Critical" %in% long_split$placeholder){
  #   long_split[long_split$placeholder == "segment_Critical", ]$colour <- "steelblue2"
  # }
  # 
  # if("segment_Specialist" %in% long_split$placeholder){
  #   long_split[long_split$placeholder == "segment_Specialist", ]$colour <- "steelblue3"
  # }
  # 
  # if("segment_Support" %in% long_split$placeholder){
  #   long_split[long_split$placeholder == "segment_Support", ]$colour <- "steelblue"
  # }
  # 
  # if("segment_Management" %in% long_split$placeholder){
  #   long_split[long_split$placeholder == "segment_Management", ]$colour <- "royalblue2"
  # }
  #long_split[long_split$placeholder == "segment_NA", ]$colour <- tmp_colour[5]
  
  long_split <- long_split %>%
    rowwise() %>%
    mutate(label = ifelse(isolate(input$deep_headcount_or_cost)=="Headcount",
                          round(value,0),
                          ifelse(value>1000000,paste0(format(round(value / 1e6, 1), trim = TRUE), "m"),
                                 paste0(format(round(value / 1e3, 1), trim = TRUE), "k")
                          )))
  
  long_split$variable <- gsub("hc_chosen","Current",long_split$variable)
  long_split$variable <- gsub("additions","Additions",long_split$variable)
  long_split$variable <- gsub("reductions","Reductions",long_split$variable)
  long_split$variable <- gsub("baseline","Baseline",long_split$variable)
  long_split$variable <- gsub("aut_Y3_q4_change","Automation",long_split$variable)
  long_split$variable <- gsub("pro_Y3_q4_change","Training",long_split$variable)
  long_split$variable <- gsub("dd_Y3_q4_change","Demand",long_split$variable)
  #long_split$variable <- gsub("out_Y3_q4_change","Outsourcing",long_split$variable)
  long_split$variable <- gsub("finalmult_Y3_q4","Year 3",long_split$variable)
  
  
  long$variable <- gsub("hc_chosen","Current",long$variable)
  long$variable <- gsub("additions","Additions",long$variable)
  long$variable <- gsub("reductions","Reductions",long$variable)
  long$variable <- gsub("baseline","Baseline",long$variable)
  long$variable <- gsub("aut_Y3_q4_change","Automation",long$variable)
  long$variable <- gsub("pro_Y3_q4_change","Training",long$variable)
  long$variable <- gsub("dd_Y3_q4_change","Demand",long$variable)
  #long$variable <- gsub("out_Y3_q4_change","Outsourcing",long$variable)
  long$variable <- gsub("finalmult_Y3_q4","Year 3",long$variable)
  
  long_split$placeholder <- gsub("szum","aasum",long_split$placeholder)
  
  long_split$segment <- long_split$segment %>% as.numeric()
  
  long_split <- add_row(long_split,segment=0,variable="Current",value=0,colour="steelblue1",placeholder="aasum",osztaly="class1",value_half=0,label="")
  
  long_split <- long_split %>%
    arrange(placeholder)
  
  long_split <- long_split %>%
    arrange(osztaly)
  
  long_totals <- long_split %>%
    filter(placeholder != "aasum") %>%
    select(osztaly,value) %>%
    group_by(osztaly) %>%
    summarise(value=round(abs(sum(value)),0))
  
  long_totals <- long_totals %>%
    rowwise() %>%
    mutate(value = ifelse(isolate(input$deep_headcount_or_cost)=="Headcount",
                          round(value,0),
                          ifelse(value>1000000,paste0(format(round(value / 1e6, 1), trim = TRUE), "Mn"),
                                 paste0(format(round(value / 1e3, 1), trim = TRUE), "K")
                          )))
  
  long_totals_height <- long_split %>%
    select(osztaly,value) %>%
    group_by(osztaly) %>%
    summarise(height=abs(sum(value))*1.02,0)
  
  highest=max(long_totals_height$height)
  
  long_totals <- long_totals %>%
    mutate("height" := !!highest*1.04)
  
  print("Waterfall table final:")
  print(long_totals)
  
  print("Waterfall table final for graph:")
  print(long_split)
  maxval=max(long_totals$height*1.05)
  
  # create waterfall chart
  p <- ggplot(data = long_split, aes(x = osztaly, y = value, fill = fct_rev(placeholder))) + 
    geom_bar(stat = "identity", colour = NA, lwd = 0.2) + 
    geom_label_repel(aes(x=osztaly,label = label, y = value,fill=fct_rev(placeholder)),colour="white", position=position_stack(vjust=0.5),ylim=c(NA,highest),direction="y",segment.size = 0,show.legend = FALSE) +
    geom_label(aes(x=osztaly,y=height,label=value,fill=NULL),data=long_totals,show.legend = FALSE)+
    scale_fill_manual(name="Segment",
                      labels = c(segment_Core="Core",segment_Critical = "Critical",segment_Support = "Support",segment_Specialist = "Specialist",segment_Management = "Management",aasum=""),
                      values = c(segment_Core=Blue2,segment_Critical = Blue3,segment_Support = Green2,segment_Specialist = Red,segment_Management = Green1,aasum="white"))+
    guides(fill = guide_legend(title=NULL,nrow=1)) +
    theme_bw() +
    scale_y_continuous(labels=comma,limits=c(0,maxval))+
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          legend.direction = "horizontal",
          text = element_text(size=16),
          axis.text.y = element_text(size=14)
    )+
    xlab("")+
    ylab(ifelse(isolate(input$headcount_or_cost) == "Headcount","Headcount","Monthly Cost (SAR)"))
  # relabel the x axis
  
  p <- p + scale_x_discrete(labels= c("Current","Additions","Reductions","Baseline","Automation","Training","Demand","Year 3"))
  
  
  tmp_lines <- list()
  tmp_lines[[1]] <- sum(long_split[long_split$variable == "Current", ]$value)
  tmp_lines[[2]] <- tmp_lines[[1]] + sum(long_split[long_split$variable == "Additions" & long_split$placeholder != "aasum", ]$value)
  tmp_lines[[3]] <- long_split[long_split$variable == "Reductions" & long_split$placeholder == "aasum", ]$value
  tmp_lines[[4]] <- sum(long_split[long_split$variable == "Baseline", ]$value)
  tmp_lines[[5]] <- long_split[long_split$variable == "Automation" & long_split$placeholder == "aasum", ]$value
  
  if (tmp_dd == "positive") {
    tmp_lines[[6]] <- sum(long_split[long_split$variable == "Demand" & long_split$placeholder == "aasum", ]$value)
    
    tmp_lines[[7]] <- sum(long_split[long_split$variable == "Demand" & long_split$placeholder == "aasum", ]$value) +
      sum(long_split[long_split$variable == "Demand" & long_split$placeholder != "aasum", ]$value)
  } else {
    tmp_lines[[6]] <- sum(long_split[long_split$variable == "Training" & long_split$placeholder == "aasum", ]$value)
    tmp_lines[[7]] <- sum(long_split[long_split$variable == "Demand" & long_split$placeholder == "aasum", ]$value)
  }
  
  #tmp_lines[[8]] <- long_split[long_split$variable == "Outsourcing" & long_split$placeholder == "aasum", ]$value
  
  for (i in 1:length(tmp_lines)) {
    p <- p + annotate("segment",
                      x = c(i, i, i+1),
                      xend = c(i, i+1, i+1),
                      y = c(tmp_lines[[i]], tmp_lines[[i]], tmp_lines[[i]]),
                      yend = c(tmp_lines[[i]], tmp_lines[[i]], tmp_lines[[i]]),
                      colour = "black",
                      size = 0.25,
                      linetype = 2)
  }
  
  p
  
})


# End ---------------------------------------------------------------------


# output$chosen_waterfall <- renderPlot({
#   if(input$deep_original_or_new == "Use original values"){
#     plot <- waterfall()
#     } else {
#       plot <- deep_waterfall()
#     }
#   plot
# })






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
# "deep_slider_aut_Y1"
# "deep_slider_aut_Y2"
# "deep_slider_aut_Y3"
# "deep_aut_quarter_Y1"
# "deep_aut_quarter_Y2"
# "deep_aut_quarter_Y3"
# "deep_slider_pro_Y1"
# "deep_slider_pro_Y2"
# "deep_slider_pro_Y3"
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
  outputOptions(output,"l1_list_exec",priority=100)
  outputOptions(output,"l2_list_exec",priority=90)
  outputOptions(output,"line",priority=80)
  outputOptions(output,"waterfall",priority=70)
  
}

shinyApp(ui=ui,server=server)