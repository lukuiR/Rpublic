#load data and library

#install.packages("tidyverse")
#install.packages("readxl")

library(tidyverse)
library(readxl)

datraw <- read_excel("hr.xlsx",col_names = TRUE,skip=1)

#colnames(datraw)[9] <- "Job Family"
#colnames(datraw)[55] <- "Employment__1"
#head(datraw,5)

#select only required columns

dat <- select(datraw, Gender, Employment__1 , `Career Level` ,`Chief/Not` , `Company Name`, `SBU/MFG/CF`, Subfamily , Family , RegionCode, `Level 1`, `Year of retirement`, `Job Family`, `Encrypted Ids`)
head(dat,5)

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

ag_dat66=select( ag_dat6, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, '2018',`2019`,`2020`,`2021`,`2022`,`2023`)

ag_dat6 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode )%>% 
  summarise(
    n = n()
  )

#join output of spread tales
zz=merge(ag_dat1,ag_dat2)
zz=merge(zz,ag_dat3)
zz=merge(zz,ag_dat4)
zz=merge(zz,ag_dat5)
zz=merge(zz,ag_dat66)
zz=merge(zz,ag_dat6)

head(zz,10)
