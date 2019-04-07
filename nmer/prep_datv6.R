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

amr <- read_excel(pat,12,col_names = TRUE,skip=2)[,1:6]
amr <- amr[1:3,]
amq <- read_excel(pat,12,col_names = TRUE,skip=8)[,1:6]
amqs <-amq
amqs <- mutate(amqs, `T=2`=`T=1`+`T=2`)
amqs <- mutate(amqs, `T=3`=`T=3`+`T=2`)
amqs <- mutate(amqs, `T=4`=`T=3`+`T=4`)
amqs <- mutate(amqs, `T=5`=`T=5`+`T=4`)
#ds
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

#############################################rs as is
{
#####

rs <- merge(rs,dd)
rs <- merge(rs,ap)
rs <- mutate(rs, not_map = rs$total-rs$Core-rs$Specialist-rs$Critical-rs$Support)

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

rs <- mutate(rs, o18 = rs$as_is_s - rs$`2018`)
rs <- mutate(rs, s18 = rs$o18*(1+rs$NHR19))

rs <- mutate(rs, o19 = rs$as_is_s - rs$`2019`)
rs <- mutate(rs, s19 = rs$o19*(1+rs$NHR20))

rs <- mutate(rs, o20 = rs$as_is_s - rs$`2020`)
rs <- mutate(rs, s20 = rs$o20*(1+rs$NHR21))

rs <- mutate(rs, o21 = rs$as_is_s - rs$`2021`)
rs <- mutate(rs, s21 = rs$o21*(1+rs$NHR22))

rs <- mutate(rs, o22 = rs$as_is_s - rs$`2022`)
rs <- mutate(rs, s22 = rs$o22*(1+rs$NHR23))

rs <- mutate(rs, o23 = rs$as_is_s - rs$`2023`)
#####
#end nhr
# demand Driver
#####
rs <- mutate(rs, `DD1 Weightage` = case_when(is.na( rs$`DD1 Weightage`) ~ 1, TRUE ~ rs$`DD1 Weightage`))
rs <- mutate(rs, dd_test = rowSums(rs[,c("DD1 Weightage","DD2 Weightage","DD3 Weightage","DD4 Weightage","DD5 Weightage")], na.rm=TRUE) )
rs <- mutate(rs, `DD1 2018` = case_when(is.na( rs$`DD1 2018`) ~ 1, TRUE ~ rs$`DD1 2018`))
rs <- mutate(rs, `DD1 2019` = case_when(is.na( rs$`DD1 2019`) ~ 1, TRUE ~ rs$`DD1 2019`))
rs <- mutate(rs, `DD1 2020` = case_when(is.na( rs$`DD1 2020`) ~ 1, TRUE ~ rs$`DD1 2020`))
rs <- mutate(rs, `DD1 2021` = case_when(is.na( rs$`DD1 2021`) ~ 1, TRUE ~ rs$`DD1 2021`))
rs <- mutate(rs, `DD1 2022` = case_when(is.na( rs$`DD1 2022`) ~ 1, TRUE ~ rs$`DD1 2022`))
rs <- mutate(rs, `DD1 2023` = case_when(is.na( rs$`DD1 2023`) ~ 1, TRUE ~ rs$`DD1 2023`))
rs <- mutate(rs, dd_c = `DD1 Weightage` + 1 - rs$dd_test)
#do poprawy
rs <- mutate(rs, dds19 = rs$sels * rs$dd_c * rs$`DD1 2019` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2018`) ~ 0, TRUE ~  rs$sels *dd_c *rs$`DD2 Weightage`*rs$`DD2 2019` / rs$`DD2 2018`))
rs <- mutate(rs, dds20 = rs$sels * rs$dd_c * rs$`DD1 2020` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2019`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2020` / rs$`DD2 2018`))
rs <- mutate(rs, dds21 = rs$sels * rs$dd_c * rs$`DD1 2021` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2020`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2021` / rs$`DD2 2018`))
rs <- mutate(rs, dds22 = rs$sels * rs$dd_c * rs$`DD1 2022` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2021`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2022` / rs$`DD2 2018`))
rs <- mutate(rs, dds23 = rs$sels * rs$dd_c * rs$`DD1 2023` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2022`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2023` / rs$`DD2 2018`))
#####
#end DD
#ld
#####
rs <- mutate(rs, ld1 = ld$`T=1`)
rs <- mutate(rs, ld2 = ld$`T=2`)
rs <- mutate(rs, ld3 = ld$`T=3`)
rs <- mutate(rs, ld4 = ld$`T=4`)
rs <- mutate(rs, ld5 = ld$`T=5`)
#ld procent cal training
# rs <- mutate(rs, ldt1 = ldp$`t=1`[3]*rs$ld1/2000)
# rs <- mutate(rs, ldt2 = (ldp$`t=1`[3]+ldp$`t=2`[3])*rs$ld2/2000)
# rs <- mutate(rs, ldt3 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3])*rs$ld3/2000)
# rs <- mutate(rs, ldt4 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3]+ldp$`t=4`[3])*rs$ld4/2000)
# rs <- mutate(rs, ldt5 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3]+ldp$`t=4`[3]+ldp$`t=5`[3])*rs$ld5/2000)
rs <- mutate(rs, ldt1 = ldp$`t=1`[3]*rs$ld1/2000)
rs <- mutate(rs, ldt2 = ldt1+(ldp$`t=2`[3])*rs$ld2/2000)
rs <- mutate(rs, ldt3 = ldt2+(ldp$`t=3`[3])*rs$ld3/2000)
rs <- mutate(rs, ldt4 = ldt3+(ldp$`t=4`[3])*rs$ld4/2000)
rs <- mutate(rs, ldt5 = ldt4+(ldp$`t=4`[3])*rs$ld3/2000)
#ld FTE Impact training
rs <- mutate(rs, fte1 = (1-rs$ldt1)*rs$dds19)
rs <- mutate(rs, fte2 = (1-rs$ldt2)*rs$dds20)
rs <- mutate(rs, fte3 = (1-rs$ldt3)*rs$dds21)
rs <- mutate(rs, fte4 = (1-rs$ldt4)*rs$dds22)
rs <- mutate(rs, fte5 = (1-rs$ldt5)*rs$dds23)

#####
#end

#autiomation
#####
rs <- mutate(rs, sray5 = case_when(is.na( rs$`Secondary Research Automation Potential`) ~ 0.125, TRUE ~ `Secondary Research Automation Potential`/4))
###do poprawy automation
rs <- mutate(rs, sray19 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=1`))))
rs <- mutate(rs, sray20 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=2`))))
rs <- mutate(rs, sray21 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=3`))))
rs <- mutate(rs, sray22 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=4`))))
rs <- mutate(rs, sray23 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=5`))))

rs <- mutate(rs, dsray19 = rs$sray19*rs$dds19/rs$sels)
rs <- mutate(rs, dsray20 = rs$sray20*rs$dds20/rs$sels)
rs <- mutate(rs, dsray21 = rs$sray21*rs$dds21/rs$sels)
rs <- mutate(rs, dsray22 = rs$sray22*rs$dds22/rs$sels)
rs <- mutate(rs, dsray23 = rs$sray23*rs$dds23/rs$sels)
#####
#end
r_opt_s=rs
}

{
  rs <- mutate(rs, sels = rs$crvt)
  rs <- mutate(rs, addt = ifelse((rs$as_is_s - rs$sels)>0, 0,-(rs$as_is_s - rs$sels)))
  rs <- mutate(rs, subt= ifelse((rs$as_is_s - rs$sels)<0, 0,-(rs$as_is_s - rs$sels)))
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
  
  rs <- mutate(rs, o18 = rs$as_is_s - rs$`2018`)
  rs <- mutate(rs, s18 = rs$o18*(1+rs$NHR19))
  
  rs <- mutate(rs, o19 = rs$as_is_s - rs$`2019`)
  rs <- mutate(rs, s19 = rs$o19*(1+rs$NHR20))
  
  rs <- mutate(rs, o20 = rs$as_is_s - rs$`2020`)
  rs <- mutate(rs, s20 = rs$o20*(1+rs$NHR21))
  
  rs <- mutate(rs, o21 = rs$as_is_s - rs$`2021`)
  rs <- mutate(rs, s21 = rs$o21*(1+rs$NHR22))
  
  rs <- mutate(rs, o22 = rs$as_is_s - rs$`2022`)
  rs <- mutate(rs, s22 = rs$o22*(1+rs$NHR23))
  
  rs <- mutate(rs, o23 = rs$as_is_s - rs$`2023`)
  #####
  #end nhr
  # demand Driver
  #####
  rs <- mutate(rs, `DD1 Weightage` = case_when(is.na( rs$`DD1 Weightage`) ~ 1, TRUE ~ rs$`DD1 Weightage`))
  rs <- mutate(rs, dd_test = rowSums(rs[,c("DD1 Weightage","DD2 Weightage","DD3 Weightage","DD4 Weightage","DD5 Weightage")], na.rm=TRUE) )
  rs <- mutate(rs, `DD1 2018` = case_when(is.na( rs$`DD1 2018`) ~ 1, TRUE ~ rs$`DD1 2018`))
  rs <- mutate(rs, `DD1 2019` = case_when(is.na( rs$`DD1 2019`) ~ 1, TRUE ~ rs$`DD1 2019`))
  rs <- mutate(rs, `DD1 2020` = case_when(is.na( rs$`DD1 2020`) ~ 1, TRUE ~ rs$`DD1 2020`))
  rs <- mutate(rs, `DD1 2021` = case_when(is.na( rs$`DD1 2021`) ~ 1, TRUE ~ rs$`DD1 2021`))
  rs <- mutate(rs, `DD1 2022` = case_when(is.na( rs$`DD1 2022`) ~ 1, TRUE ~ rs$`DD1 2022`))
  rs <- mutate(rs, `DD1 2023` = case_when(is.na( rs$`DD1 2023`) ~ 1, TRUE ~ rs$`DD1 2023`))
  rs <- mutate(rs, dd_c = `DD1 Weightage` + 1 - rs$dd_test)
  #do poprawy
  rs <- mutate(rs, dds19 = rs$sels * rs$dd_c * rs$`DD1 2019` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2018`) ~ 0, TRUE ~  rs$sels *dd_c *rs$`DD2 Weightage`*rs$`DD2 2019` / rs$`DD2 2018`))
  rs <- mutate(rs, dds20 = rs$sels * rs$dd_c * rs$`DD1 2020` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2019`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2020` / rs$`DD2 2018`))
  rs <- mutate(rs, dds21 = rs$sels * rs$dd_c * rs$`DD1 2021` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2020`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2021` / rs$`DD2 2018`))
  rs <- mutate(rs, dds22 = rs$sels * rs$dd_c * rs$`DD1 2022` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2021`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2022` / rs$`DD2 2018`))
  rs <- mutate(rs, dds23 = rs$sels * rs$dd_c * rs$`DD1 2023` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2022`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2023` / rs$`DD2 2018`))
  #####
  #end DD
  #ld
  #####
  rs <- mutate(rs, ld1 = ld$`T=1`)
  rs <- mutate(rs, ld2 = ld$`T=2`)
  rs <- mutate(rs, ld3 = ld$`T=3`)
  rs <- mutate(rs, ld4 = ld$`T=4`)
  rs <- mutate(rs, ld5 = ld$`T=5`)
  #ld procent cal training
  # rs <- mutate(rs, ldt1 = ldp$`t=1`[3]*rs$ld1/2000)
  # rs <- mutate(rs, ldt2 = (ldp$`t=1`[3]+ldp$`t=2`[3])*rs$ld2/2000)
  # rs <- mutate(rs, ldt3 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3])*rs$ld3/2000)
  # rs <- mutate(rs, ldt4 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3]+ldp$`t=4`[3])*rs$ld4/2000)
  # rs <- mutate(rs, ldt5 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3]+ldp$`t=4`[3]+ldp$`t=5`[3])*rs$ld5/2000)
  rs <- mutate(rs, ldt1 = ldp$`t=1`[3]*rs$ld1/2000)
  rs <- mutate(rs, ldt2 = ldt1+(ldp$`t=2`[3])*rs$ld2/2000)
  rs <- mutate(rs, ldt3 = ldt2+(ldp$`t=3`[3])*rs$ld3/2000)
  rs <- mutate(rs, ldt4 = ldt3+(ldp$`t=4`[3])*rs$ld4/2000)
  rs <- mutate(rs, ldt5 = ldt4+(ldp$`t=4`[3])*rs$ld3/2000)
  #ld FTE Impact training
  rs <- mutate(rs, fte1 = (1-rs$ldt1)*rs$dds19)
  rs <- mutate(rs, fte2 = (1-rs$ldt2)*rs$dds20)
  rs <- mutate(rs, fte3 = (1-rs$ldt3)*rs$dds21)
  rs <- mutate(rs, fte4 = (1-rs$ldt4)*rs$dds22)
  rs <- mutate(rs, fte5 = (1-rs$ldt5)*rs$dds23)
  
  #####
  #end
  
  #autiomation
  #####
  rs <- mutate(rs, sray5 = case_when(is.na( rs$`Secondary Research Automation Potential`) ~ 0.125, TRUE ~ `Secondary Research Automation Potential`/4))
  ###do poprawy automation
  rs <- mutate(rs, sray19 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=1`))))
  rs <- mutate(rs, sray20 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=2`))))
  rs <- mutate(rs, sray21 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=3`))))
  rs <- mutate(rs, sray22 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=4`))))
  rs <- mutate(rs, sray23 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=5`))))
  
  rs <- mutate(rs, dsray19 = rs$sray19*rs$dds19/rs$sels)
  rs <- mutate(rs, dsray20 = rs$sray20*rs$dds20/rs$sels)
  rs <- mutate(rs, dsray21 = rs$sray21*rs$dds21/rs$sels)
  rs <- mutate(rs, dsray22 = rs$sray22*rs$dds22/rs$sels)
  rs <- mutate(rs, dsray23 = rs$sray23*rs$dds23/rs$sels)
  #####
  #end
  r_crvt_s=rs
  }

{
  rs <- mutate(rs, sels = rs$as_is_s)
  rs <- mutate(rs, addt = ifelse((rs$as_is_s - rs$sels)>0, 0,-(rs$as_is_s - rs$sels)))
  rs <- mutate(rs, subt= ifelse((rs$as_is_s - rs$sels)<0, 0,-(rs$as_is_s - rs$sels)))
  # rs <- mutate(rs, subt = rs$as_is_s - rs$sels)
  # rs <- mutate(rs, addt = 0)#rs$sels - rs$as_is_s)
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
  
  rs <- mutate(rs, o18 = rs$as_is_s - rs$`2018`)
  rs <- mutate(rs, s18 = rs$o18*(1+rs$NHR19))
  
  rs <- mutate(rs, o19 = rs$as_is_s - rs$`2019`)
  rs <- mutate(rs, s19 = rs$o19*(1+rs$NHR20))
  
  rs <- mutate(rs, o20 = rs$as_is_s - rs$`2020`)
  rs <- mutate(rs, s20 = rs$o20*(1+rs$NHR21))
  
  rs <- mutate(rs, o21 = rs$as_is_s - rs$`2021`)
  rs <- mutate(rs, s21 = rs$o21*(1+rs$NHR22))
  
  rs <- mutate(rs, o22 = rs$as_is_s - rs$`2022`)
  rs <- mutate(rs, s22 = rs$o22*(1+rs$NHR23))
  
  rs <- mutate(rs, o23 = rs$as_is_s - rs$`2023`)
  #####
  #end nhr
  # demand Driver
  #####
  rs <- mutate(rs, `DD1 Weightage` = case_when(is.na( rs$`DD1 Weightage`) ~ 1, TRUE ~ rs$`DD1 Weightage`))
  rs <- mutate(rs, dd_test = rowSums(rs[,c("DD1 Weightage","DD2 Weightage","DD3 Weightage","DD4 Weightage","DD5 Weightage")], na.rm=TRUE) )
  rs <- mutate(rs, `DD1 2018` = case_when(is.na( rs$`DD1 2018`) ~ 1, TRUE ~ rs$`DD1 2018`))
  rs <- mutate(rs, `DD1 2019` = case_when(is.na( rs$`DD1 2019`) ~ 1, TRUE ~ rs$`DD1 2019`))
  rs <- mutate(rs, `DD1 2020` = case_when(is.na( rs$`DD1 2020`) ~ 1, TRUE ~ rs$`DD1 2020`))
  rs <- mutate(rs, `DD1 2021` = case_when(is.na( rs$`DD1 2021`) ~ 1, TRUE ~ rs$`DD1 2021`))
  rs <- mutate(rs, `DD1 2022` = case_when(is.na( rs$`DD1 2022`) ~ 1, TRUE ~ rs$`DD1 2022`))
  rs <- mutate(rs, `DD1 2023` = case_when(is.na( rs$`DD1 2023`) ~ 1, TRUE ~ rs$`DD1 2023`))
  rs <- mutate(rs, dd_c = `DD1 Weightage` + 1 - rs$dd_test)
  #do poprawy
  rs <- mutate(rs, dds19 = rs$sels * rs$dd_c * rs$`DD1 2019` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2018`) ~ 0, TRUE ~  rs$sels *dd_c *rs$`DD2 Weightage`*rs$`DD2 2019` / rs$`DD2 2018`))
  rs <- mutate(rs, dds20 = rs$sels * rs$dd_c * rs$`DD1 2020` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2019`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2020` / rs$`DD2 2018`))
  rs <- mutate(rs, dds21 = rs$sels * rs$dd_c * rs$`DD1 2021` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2020`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2021` / rs$`DD2 2018`))
  rs <- mutate(rs, dds22 = rs$sels * rs$dd_c * rs$`DD1 2022` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2021`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2022` / rs$`DD2 2018`))
  rs <- mutate(rs, dds23 = rs$sels * rs$dd_c * rs$`DD1 2023` / rs$`DD1 2018`+ case_when(is.na( rs$`DD2 2022`) ~ 0, TRUE ~  rs$sels *rs$dd_c *rs$`DD2 Weightage`*rs$`DD2 2023` / rs$`DD2 2018`))
  #####
  #end DD
  #ld
  #####
  rs <- mutate(rs, ld1 = ld$`T=1`)
  rs <- mutate(rs, ld2 = ld$`T=2`)
  rs <- mutate(rs, ld3 = ld$`T=3`)
  rs <- mutate(rs, ld4 = ld$`T=4`)
  rs <- mutate(rs, ld5 = ld$`T=5`)
  #ld procent cal training
  ################################################################################# do poprawy
  # rs <- mutate(rs, ldt1 = ldp$`t=1`[3]*rs$ld1/2000)
  # rs <- mutate(rs, ldt2 = (ldp$`t=1`[3]+ldp$`t=2`[3])*rs$ld2/2000)
  # rs <- mutate(rs, ldt3 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3])*rs$ld3/2000)
  # rs <- mutate(rs, ldt4 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3]+ldp$`t=4`[3])*rs$ld4/2000)
  # rs <- mutate(rs, ldt5 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3]+ldp$`t=4`[3]+ldp$`t=5`[3])*rs$ld5/2000)
  rs <- mutate(rs, ldt1 = ldp$`t=1`[3]*rs$ld1/2000)
  rs <- mutate(rs, ldt2 = ldt1+(ldp$`t=2`[3])*rs$ld2/2000)
  rs <- mutate(rs, ldt3 = ldt2+(ldp$`t=3`[3])*rs$ld3/2000)
  rs <- mutate(rs, ldt4 = ldt3+(ldp$`t=4`[3])*rs$ld4/2000)
  rs <- mutate(rs, ldt5 = ldt4+(ldp$`t=4`[3])*rs$ld3/2000)
  #ld FTE Impact training
  rs <- mutate(rs, fte1 = (1-rs$ldt1)*rs$dds19)
  rs <- mutate(rs, fte2 = (1-rs$ldt2)*rs$dds20)
  rs <- mutate(rs, fte3 = (1-rs$ldt3)*rs$dds21)
  rs <- mutate(rs, fte4 = (1-rs$ldt4)*rs$dds22)
  rs <- mutate(rs, fte5 = (1-rs$ldt5)*rs$dds23)
  
  #####
  #end
  
  #autiomation
  #####
  rs <- mutate(rs, sray5 = case_when(is.na( rs$`Secondary Research Automation Potential`) ~ 0.125, TRUE ~ `Secondary Research Automation Potential`/4))
  ###do poprawy automation
  rs <- mutate(rs, sray19 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=1`))))
  rs <- mutate(rs, sray20 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=2`))))
  rs <- mutate(rs, sray21 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=3`))))
  rs <- mutate(rs, sray22 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=4`))))
  rs <- mutate(rs, sray23 = case_when(is.na( rs$`Impact Year`) ~ rs$sels, TRUE ~ rs$sels*(1-rs$sray5*rs$`LCM Automation Potential`*sum(amqs$`T=5`))))
  
  rs <- mutate(rs, dsray19 = rs$sray19*rs$dds19/rs$sels)
  rs <- mutate(rs, dsray20 = rs$sray20*rs$dds20/rs$sels)
  rs <- mutate(rs, dsray21 = rs$sray21*rs$dds21/rs$sels)
  rs <- mutate(rs, dsray22 = rs$sray22*rs$dds22/rs$sels)
  rs <- mutate(rs, dsray23 = rs$sray23*rs$dds23/rs$sels)
}

###########################




