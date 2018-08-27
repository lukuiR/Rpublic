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

ag_dat3 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, `Career Level` )%>% 
  summarise(
    n = n()
  ) %>%
  spread(key = `Career Level`, value = n) #, fill =0)

ag_dat4 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, Gender )%>% 
  summarise(
    n = n()
  ) %>%
  spread(key = Gender, value = n) #, fill =0)

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

ag_dat66=ag_dat6[,c(1:5,17:22)]

zz=merge(ag_dat1,ag_dat2)
zz=merge(zz,ag_dat3)
zz=merge(zz,ag_dat4)
zz=merge(zz,ag_dat5)
zz=merge(zz,ag_dat66)
