#raw data
#####


pat <- "nmer/R_RawData.xlsx"
pat <- "R_RawData.xlsx"
##
db <- read_excel("R_RawData.xlsx",2,col_names = TRUE,skip=1)
rs <- read_excel(pat,3,col_names = TRUE,skip=1)
nhr <- read_excel(pat,4,col_names = TRUE,skip=5)
ld <- read_excel(pat,7,col_names = TRUE,skip=5)
dd <- read_excel(pat,5,col_names = TRUE,skip=1)
ap <- read_excel(pat,6,col_names = TRUE,skip=1)
ar <- read_excel(pat,9,col_names = TRUE,skip=1)
ap <- merge(ap,ar)
ldp <- read_excel(pat,10,col_names = TRUE,skip=6)
ldp <- ldp[1:3,3:8]

amr <- read_excel(pat,11,col_names = TRUE,skip=2)[,1:6]
amr <- amr[1:3,]
amqs <- read_excel(pat,11,col_names = TRUE,skip=8)[,1:6]

#ds
#####
db <- merge(db,dd)

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



#############################################rs as is
{
#####

rs <- mutate(rs, not_map = rs$total-rs$Core-rs$Specialist-rs$Critical-rs$Support)
rs <- mutate(rs, not_job = rs$total-rs$`Direct Hire`-rs$Contractor)

rs <- mutate(rs, as_is_s = total)
rs <- mutate(rs, opt =case_when(is.na(`Right-Sized Numbers`) ~ as_is_s, TRUE ~ `Right-Sized Numbers`))
rs <- mutate(rs, crvt = (as_is_s+opt)/2)
rs <- mutate(rs, sels = rs$opt)
rs <- mutate(rs, addt = ifelse((rs$as_is_s - rs$sels)>0, 0,-(rs$as_is_s - rs$sels)))
rs <- mutate(rs, subt= ifelse((rs$as_is_s - rs$sels)<0, 0,-(rs$as_is_s - rs$sels)))#rs$sels - rs$as_is_s)
#####
#end rs
#nhr
#####
rs <- mutate(rs, 
             NHR18= case_when(Region == "MEA" ~ nhr$MEA[1],  Region == "APAC" ~ nhr$APAC[1],
                              Region == "AMR" ~ nhr$AMR[1], Region == "EUR" ~ nhr$EUR[1])
)

rs <- mutate(rs, 
             NHR19= case_when(Region == "MEA" ~ nhr$MEA[2],  Region == "APAC" ~ nhr$APAC[2],
                              Region == "AMR" ~ nhr$AMR[2], Region == "EUR" ~ nhr$EUR[2])
)

rs <- mutate(rs, 
             NHR20= case_when(Region == "MEA" ~ nhr$MEA[3],  Region == "APAC" ~ nhr$APAC[3],
                              Region == "AMR" ~ nhr$AMR[3], Region == "EUR" ~ nhr$EUR[3])
)

rs <- mutate(rs, 
             NHR21= case_when(Region == "MEA" ~ nhr$MEA[4],  Region == "APAC" ~ nhr$APAC[4],
                              Region == "AMR" ~ nhr$AMR[4], Region == "EUR" ~ nhr$EUR[4])
)

rs <- mutate(rs, 
             NHR22= case_when(Region == "MEA" ~ nhr$MEA[5],  Region == "APAC" ~ nhr$APAC[5],
                              Region == "AMR" ~ nhr$AMR[5], Region == "EUR" ~ nhr$EUR[5])
)

rs <- mutate(rs, 
             NHR23= case_when(Region == "MEA" ~ nhr$MEA[6],  Region == "APAC" ~ nhr$APAC[6],
                              Region == "AMR" ~ nhr$AMR[6], Region == "EUR" ~ nhr$EUR[6])
)

nhros <- rs

nhros <- mutate(nhros, sels = nhros$as_is_s)
nhros <- mutate(nhros, addt = ifelse((nhros$as_is_s - nhros$sels)>0, 0,-(nhros$as_is_s - nhros$sels)),
                                           subt= ifelse((nhros$as_is_s - nhros$sels)<0, 0,-(nhros$as_is_s - nhros$sels)),
                                           o18 = nhros$as_is_s - ifelse(is.na(nhros$`2018`),0,nhros$`2018`))

nhros <- mutate(nhros,s18 = nhros$o18*(1+nhros$NHR19))

nhros <- mutate(nhros,o19 = nhros$s18 - ifelse(is.na(nhros$`2019`),0,nhros$`2019`))
nhros <- mutate(nhros,s19 = nhros$o19*(1+nhros$NHR20))

nhros <- mutate(nhros,o20 = nhros$s19 - ifelse(is.na(nhros$`2020`),0,nhros$`2020`))
nhros <- mutate(nhros,s20 = nhros$o20*(1+nhros$NHR21))

nhros <- mutate(nhros,o21 = nhros$s20 - ifelse(is.na(nhros$`2021`),0,nhros$`2021`))
nhros <- mutate(nhros,s21 = nhros$o21*(1+nhros$NHR22))

nhros <- mutate(nhros,o22 = nhros$s21 - ifelse(is.na(nhros$`2022`),0,nhros$`2022`))
nhros <- mutate(nhros,s22 = nhros$o22*(1+nhros$NHR23))


rs <- merge(rs,dd)
rs <- merge(rs,ap)

#ld
#####
rs <- mutate(rs, ld1 = ld$`T=1`)
rs <- mutate(rs, ld2 = ld$`T=2`)
rs <- mutate(rs, ld3 = ld$`T=3`)
rs <- mutate(rs, ld4 = ld$`T=4`)
rs <- mutate(rs, ld5 = ld$`T=5`)
#ld procent cal training
 rs <- mutate(rs, ldt1 = ldp$`t=1`[3]*rs$ld1/2000)
 rs <- mutate(rs, ldt2 = ldt1 + ldp$`t=2`[3]*rs$ld2/2000)
 rs <- mutate(rs, ldt3 = ldt2 + ldp$`t=3`[3]*rs$ld3/2000)
 rs <- mutate(rs, ldt4 = ldt3 + ldp$`t=4`[3]*rs$ld4/2000)
 rs <- mutate(rs, ldt5 = ldt4 + ldp$`t=5`[3]*rs$ld5/2000)

#####
 luk1=rs 

luk1[is.na(luk1$Family),c('Family')]='empty'
luk1[is.na(luk1$`Sub Family`),c('Sub Family')]='empty'

luk1 <- luk1 %>% mutate(id = row_number())

segem <- select(luk1, id, Core, Specialist, Critical, Support, not_map, 'Direct Hire', Contractor, not_job)

luk1$Modify<-
  paste0('
         <div class="btn-group" role="group" aria-label="Basic example">
         <button type="button" class="btn btn-secondary delete" id=modify_',1:nrow(luk1),'>Change</button>
         </div>')
luk1[["Edit"]]<-
  paste0('
         <div class="btn-group" role="group" aria-label="Basic example">
         <button type="button" class="btn btn-secondary delete" id=modify_',1:nrow(luk1),'><span class="glyphicon glyphicon-edit" aria-hidden="true"></span></button>
         </div>
         ')

luk1 <- luk1 %>% mutate(aiy= case_when(luk1$`Automation Project`=='N' ~ -50,
                                       is.na( luk1$`Impact Year`) ~ -50,
                                       luk1$`Impact Year`==2019 ~ 0,
                                       luk1$`Impact Year`==2020 ~ 4,
                                       luk1$`Impact Year`==2021 ~ 8,
                                       luk1$`Impact Year`==2022 ~ 12,
                                       luk1$`Impact Year`==2023 ~ 16,
                                       TRUE ~ -50
                                       )
                        )
luk1 <- luk1 %>% mutate(aiq= case_when(is.na( luk1$`Impact Quarter`) ~ 0,
                                       luk1$`Impact Quarter`==1 ~ 0,
                                       luk1$`Impact Quarter`==2 ~ 1,
                                       luk1$`Impact Quarter`==3 ~ 2,
                                       luk1$`Impact Quarter`==4 ~ 3,
                                       TRUE ~ -50
                                       )
                        )


luk1 <- luk1 %>% mutate(yai19q1=0)
luk1 <- luk1 %>% mutate(yai19q2=0)
luk1 <- luk1 %>% mutate(yai19q3=0)
luk1 <- luk1 %>% mutate(yai19q4=0)
luk1 <- luk1 %>% mutate(yai20q1=0)
luk1 <- luk1 %>% mutate(yai20q2=0)
luk1 <- luk1 %>% mutate(yai20q3=0)
luk1 <- luk1 %>% mutate(yai20q4=0)
luk1 <- luk1 %>% mutate(yai21q1=0)
luk1 <- luk1 %>% mutate(yai21q2=0)
luk1 <- luk1 %>% mutate(yai21q3=0)
luk1 <- luk1 %>% mutate(yai21q4=0)
luk1 <- luk1 %>% mutate(yai22q1=0)
luk1 <- luk1 %>% mutate(yai22q2=0)
luk1 <- luk1 %>% mutate(yai22q3=0)
luk1 <- luk1 %>% mutate(yai22q4=0)
luk1 <- luk1 %>% mutate(yai23q1=0)
luk1 <- luk1 %>% mutate(yai23q2=0)
luk1 <- luk1 %>% mutate(yai23q3=0)
luk1 <- luk1 %>% mutate(yai23q4=0)

x=which( colnames(luk1)=="aiq")+1
x1=x+19

paui<-rep(1:4,5)*c(rep(as.numeric(amqs[1,2]),4) ,rep(as.numeric(amqs[1,3]),4),rep(as.numeric(amqs[1,4]),4),rep(as.numeric(amqs[1,5]),4),rep(as.numeric(amqs[1,6]),4))
paui[17:20]=paui[17:20]+paui[16]
paui[13:20]=paui[13:20]+paui[12]
paui[9:20]=paui[9:20]+paui[8]
paui[5:20]=paui[5:20]+paui[4]
for (i in 1:dim(luk1)[1]) {
  if(!is.na(luk1$aiq[i]+luk1$aiy[i])){
  if(luk1$aiq[i]+luk1$aiy[i]>-1){
    y=luk1$aiq[i]+luk1$aiy[i]
    luk1[i,x:x1]=c(rep(0,y),paui[1:(20-y)])*luk1$`LCM Automation Potential`[i]/4*ifelse(is.na(luk1$`Secondary Research Automation Potential`[i]),
                                                                                          0.5 ,luk1$`Secondary Research Automation Potential`[i])
  }}
}

}
