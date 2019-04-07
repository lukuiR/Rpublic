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
ldp <- read_excel(pat,11,col_names = TRUE,skip=6)
ldp <- ldp[1:3,3:8]
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

#rs
#####
rs <- merge(rs,dd)
rs <- merge(rs,ap)

rs <- mutate(rs, as_is_s = total)
rs <- mutate(rs, opt =case_when(is.na(`Right-Sized Numbers`) ~ as_is_s, TRUE ~ `Right-Sized Numbers`))
rs <- mutate(rs, crvt = (as_is_s+opt)/2)
rs <- mutate(rs, sels = crvt)
rs <- mutate(rs, subt = as_is_s - sels)
rs <- mutate(rs, addt = sels - as_is_s)
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

rs <- mutate(rs, o18 = as_is_s - `2018`)
rs <- mutate(rs, s18 = o18*(1+NHR19))

rs <- mutate(rs, o19 = as_is_s - `2019`)
rs <- mutate(rs, s19 = o19*(1+NHR20))

rs <- mutate(rs, o20 = as_is_s - `2020`)
rs <- mutate(rs, s20 = o20*(1+NHR21))

rs <- mutate(rs, o21 = as_is_s - `2021`)
rs <- mutate(rs, s21 = o21*(1+NHR22))

rs <- mutate(rs, o22 = as_is_s - `2022`)
rs <- mutate(rs, s22 = o22*(1+NHR23))

rs <- mutate(rs, o23 = as_is_s - `2023`)
#####
#end nhr
# demand Driver
#####
rowSums(rs[,c(rs$`DD1 Weightage`,rs$`DD2 Weightage`)], na.rm=TRUE)
rs <- mutate(rs, dd_test = rowSums(rs[,c("DD1 Weightage","DD2 Weightage","DD3 Weightage","DD4 Weightage","DD5 Weightage")], na.rm=TRUE) )
rs <- mutate(rs, `DD1 Weightage` = case_when(is.na( `DD1 Weightage`) ~ 1, TRUE ~ `DD1 Weightage`))
rs <- mutate(rs, `DD1 2018` = case_when(is.na( `DD1 2018`) ~ 1, TRUE ~ `DD1 2018`))
rs <- mutate(rs, `DD1 2019` = case_when(is.na( `DD1 2019`) ~ 1, TRUE ~ `DD1 2019`))
rs <- mutate(rs, `DD1 2020` = case_when(is.na( `DD1 2020`) ~ 1, TRUE ~ `DD1 2020`))
rs <- mutate(rs, `DD1 2021` = case_when(is.na( `DD1 2021`) ~ 1, TRUE ~ `DD1 2021`))
rs <- mutate(rs, `DD1 2022` = case_when(is.na( `DD1 2022`) ~ 1, TRUE ~ `DD1 2022`))
rs <- mutate(rs, `DD1 2023` = case_when(is.na( `DD1 2023`) ~ 1, TRUE ~ `DD1 2023`))
rs <- mutate(rs, dd_c = `DD1 Weightage` + 1 - dd_test)
rs <- mutate(rs, dds19 = sels * dd_c * `DD1 2019` / `DD1 2018`+ case_when(is.na( `DD2 2018`) ~ 0, TRUE ~  sels *dd_c *`DD2 Weightage`*`DD2 2019` / `DD2 2018`))
rs <- mutate(rs, dds20 = sels * dd_c * `DD1 2020` / `DD1 2018`+ case_when(is.na( `DD2 2019`) ~ 0, TRUE ~  sels *dd_c *`DD2 Weightage`*`DD2 2020` / `DD2 2018`))
rs <- mutate(rs, dds21 = sels * dd_c * `DD1 2021` / `DD1 2018`+ case_when(is.na( `DD2 2020`) ~ 0, TRUE ~  sels *dd_c *`DD2 Weightage`*`DD2 2021` / `DD2 2018`))
rs <- mutate(rs, dds22 = sels * dd_c * `DD1 2022` / `DD1 2018`+ case_when(is.na( `DD2 2021`) ~ 0, TRUE ~  sels *dd_c *`DD2 Weightage`*`DD2 2022` / `DD2 2018`))
rs <- mutate(rs, dds23 = sels * dd_c * `DD1 2023` / `DD1 2018`+ case_when(is.na( `DD2 2022`) ~ 0, TRUE ~  sels *dd_c *`DD2 Weightage`*`DD2 2023` / `DD2 2018`))
#####
#end DD
#ld
#####
rs <- mutate(rs, ld1 = ld$`T=1`)
rs <- mutate(rs, ld2 = ld$`T=2`)
rs <- mutate(rs, ld3 = ld$`T=3`)
rs <- mutate(rs, ld4 = ld$`T=4`)
rs <- mutate(rs, ld5 = ld$`T=5`)
#ld procent cal
rs <- mutate(rs, ldt1 = ldp$`t=1`[3]*ld1/2000)
rs <- mutate(rs, ldt2 = (ldp$`t=1`[3]+ldp$`t=2`[3])*ld2/2000)
rs <- mutate(rs, ldt3 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3])*ld3/2000)
rs <- mutate(rs, ldt4 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3]+ldp$`t=4`[3])*ld4/2000)
rs <- mutate(rs, ldt5 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3]+ldp$`t=4`[3]+ldp$`t=5`[3])*ld5/2000)
#ld FTE Impact
rs <- mutate(rs, fte1 = (1-ldt1)*dds19)
rs <- mutate(rs, fte2 = (1-ldt2)*dds20)
rs <- mutate(rs, fte3 = (1-ldt3)*dds21)
rs <- mutate(rs, fte4 = (1-ldt4)*dds22)
rs <- mutate(rs, fte5 = (1-ldt5)*dds23)

#####
#end

