#load data

install.packages("tidyverse")
install.packages("readxl")

library(tidyverse)
library(readxl)
datraw <- read_excel("C:/Users/Luke/Documents/r_datamining/data/HRIS data with subfamilies 20180823.xlsx",col_names = TRUE,skip=1)

head(datraw,5)

#select only required columns

dat <- select(datraw, Employment__1 , `Career Level` ,`Chief/Not` , `Company Name`, `SBU/MFG/CF`, Subfamily , Family , RegionCode, `Level 1`, `Year of retirement`, `Job Family`, `Encrypted Ids`)
head(dat,5)

#calculate field

dat <- mutate(dat, 
       `SBU/MFG/CF Name`= case_when(`SBU/MFG/CF` == "SBU" ~ `Level 1`,  `SBU/MFG/CF` == "MFG Sites" ~ `Company Name`, TRUE ~ `SBU/MFG/CF`)
       )
  
#Aggregate, count id
ag_dat <- group_by(dat, Employment__1 , `Career Level` ,`Chief/Not` , `Company Name`, `SBU/MFG/CF`, Subfamily , Family , RegionCode, `Level 1`, `Year of retirement`, `Job Family`, `SBU/MFG/CF Name`) %>% 
  summarise(
    n = n()
  )
