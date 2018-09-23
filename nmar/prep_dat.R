#raw data
#####
db <- read_excel("R_RawData.xlsx",2,col_names = TRUE,skip=1)
rs <- read_excel("R_RawData.xlsx",3,col_names = TRUE,skip=1)
nhr <- read_excel("R_RawData.xlsx",4,col_names = TRUE,skip=5)
ld <- read_excel("R_RawData.xlsx",7,col_names = TRUE,skip=5)
#####

db <- mutate(db, 
             n= total
)
db <- mutate(db, 
             Manual= total
)
db[["Modify"]]<-
  paste0('
           <div class="btn-group" role="group" aria-label="Basic example">
           <button type="button" class="btn btn-secondary delete" id=modify_',1:nrow(db),'>Change</button>
           </div>
           
           ')
db[["Edit"]]<-
  paste0('
           <div class="btn-group" role="group" aria-label="Basic example">
           <button type="button" class="btn btn-secondary delete" id=modify_',1:nrow(db),'><span class="glyphicon glyphicon-edit" aria-hidden="true"></span></button>
           </div>
           
           ')
##################nhr
# year<-c(2018:2023)
# MEA <- rep(1.1,6)
# APAC <- rep(1.3,6)
# AMR <- rep(1.4,6)
# EUR <- rep(1.2,6)
# nhr <- data_frame(year, MEA, APAC,AMR, EUR)
##################################################################### cd

db <- mutate(db, 
             NHR= case_when(Region == "MEA" ~ nhr$MEA[1],  Region == "APAC" ~ nhr$APAC[1],
                            Region == "AMR" ~ nhr$AMR[1], Region == "EUR" ~ nhr$EUR[1])
)
#####

