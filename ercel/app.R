#dt ext https://rstudio.github.io/DT/extensions.html
###
library(shiny)
library(readxl)
library(tidyverse)
library(DT)
options(shiny.maxRequestSize=30*1024^2) 

# Define UI for data upload app ----
ui <- fluidPage(
  
  fileInput("uploadFile", "XLSX file"),
  dataTableOutput(outputId = "dt2")
  
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  dataset<-reactive({ 
    inFile <- input$uploadFile 
    datraw<-read_excel(inFile$datapath, col_names = TRUE,skip=1)
    
    colnames(datraw)[9] <- "Job Family"
    colnames(datraw)[55] <- "Employment__1"
    dat <- select(datraw, Gender, Employment__1 , `Career Level` ,`Chief/Not` , `Company Name`, `SBU/MFG/CF`, Subfamily , Family , RegionCode, `Level 1`, `Year of retirement`, `Job Family`, `Encrypted Ids`)
    
    
    #calculate field
    
    dat <- mutate(dat, 
                  `SBU/MFG/CF Name`= case_when(`SBU/MFG/CF` == "SBU" ~ `Level 1`,  `SBU/MFG/CF` == "MFG Sites" ~ `Company Name`, TRUE ~ `SBU/MFG/CF`)
    )
    
    #Remove not needed job Family
    dat$`Job Family`[!(dat$`Job Family` %in% c('Professionals',	'Technicians', 'Executives',	'Advisor & Consultant',	'Managers',	'Supervisor',	
                                               'Administrators',	'Operators',	'Superintendent & Section Head',	
                                               'Security & Safety', 'Para Professional'))] <- 'Other'
    
    #Aggregate, count id
    ag_dat <- group_by(dat, Employment__1 , `Career Level` ,`Chief/Not` , `Company Name`, `SBU/MFG/CF`, Subfamily , Family , RegionCode, `Level 1`, `Year of retirement`, `Job Family`, `SBU/MFG/CF Name`) %>% 
      summarise(
        n = n()
      )
    
    #spread
    
    #BC
    ag_dat1 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, Employment__1 )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = Employment__1, value = n) #, fill =0)
    
    ag_dat2 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, `Chief/Not` )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = `Chief/Not`, value = n) #, fill =0)
    
    colnames(ag_dat2)[dim(ag_dat2)[2]] <- "`Chief/Not NA`"
    
    ag_dat3 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, `Career Level` )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = `Career Level`, value = n) #, fill =0)
    
    colnames(ag_dat3)[dim(ag_dat3)[2]] <- "`Career Level NA`"
    
    ag_dat4 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, Gender )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = Gender, value = n) #, fill =0)
    
    colnames(ag_dat4)[dim(ag_dat4)[2]] <- "`Gender NA`"
    
    ag_dat5 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, `Job Family` )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = `Job Family`, value = n) #, fill =0)
    
    ag_dat6 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, `Year of retirement` )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = `Year of retirement`, value = n) #, fill =0)
    
    #remove not needed years of retirement
    #ag_dat66=ag_dat6[, c('Subfamily' , 'Family' ,'SBU/MFG/CF', 'SBU/MFG/CF Name', 'RegionCode', '2018', '2019', '2020','2021','2022', '2023')] #1:5,17:22)]
    
    ag_dat66 <- select( ag_dat6, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, '2018',`2019`,`2020`,`2021`,`2022`,`2023`)
    
    ag_dat6 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode )%>% 
      summarise(
        n = n()
      )

    z <- ag_dat1 %>% 
      inner_join(ag_dat2)%>% 
      inner_join(ag_dat3)%>% 
      inner_join(ag_dat4)%>% 
      inner_join(ag_dat5)%>% 
      inner_join(ag_dat66)%>% 
      inner_join(ag_dat6)
    
    head(z,10)
    
    
    return(head(z,10))
  })
  
  output$dt2<- renderDataTable({dataset()})
  

}

# Create Shiny app ----
shinyApp(ui, server)
