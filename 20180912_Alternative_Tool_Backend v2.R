# FINAL MODEL BACKEND
# Remove outsourcing from all graphs
# Table to have total multiplier

library(tidyverse)
library(rlang)
library(scales)
library(RColorBrewer)
library(grid)
#library(officer)
library(purrr)
library(gridExtra)
#library(flextable)
#library(rvg)
library(ggrepel)
library(shinyWidgets)

#working_directory <- "C:/Users/charlie-kershaw/Documents/Saudi/STC/Workforce Model/R/Tool"
#1setwd(working_directory)

CustomThemePres <- theme_bw()+
  theme(
    legend.text = element_text(colour="grey47"),
    legend.title = element_text(colour="grey47"),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=0.5,colour="grey47"),
    plot.subtitle = element_text(colour="grey47"),
    axis.title = element_text(colour="grey47"),
    axis.text = element_text(colour="grey47"),
    strip.background = element_rect(colour="grey70",fill="grey98"),
    strip.text = element_text(colour="grey47")
  )

# Data Import Section -----------------------------------------------------

# Importing raw data
raw_data <- read_csv("20180918_Data_Import_Tool_v2.csv")
colnames <- colnames(raw_data)


data_sectioninfo <- raw_data %>% select(unique_identifier,l0,l1,l2,l3,l4,level,segment)
data_sectioninfo[c("l1","l2","l3","l4")][is.na(data_sectioninfo[c("l1","l2","l3","l4")])] <- "N/A"
data_sectioninfo[c("l1","l2","l3","l4")][data_sectioninfo[c("l1","l2","l3","l4")]=="None"] <- "All"

data_sectioncost <- raw_data %>% select(unique_identifier,fte_cost_avg)
data_sectioncost <- data_sectioncost %>%
  mutate("fte_cost_avg"=as.numeric(gsub(",","",fte_cost_avg)))

data_hc <- raw_data %>% 
  select(unique_identifier,fte,contractor,fte_cost_avg) %>%
  rowwise() %>%
  mutate(hc_chosen = fte + contractor)

data_automation <- raw_data %>% select(unique_identifier,
                                       aut_min_Y1,aut_min_Y2,aut_min_Y3,
                                       aut_mid_Y1,aut_mid_Y2,aut_mid_Y3,
                                       aut_max_Y1,aut_max_Y2,aut_max_Y3,
                                       aut_min_Y1_q,aut_min_Y2_q,aut_min_Y3_q,
                                       aut_mid_Y1_q,aut_mid_Y2_q,aut_mid_Y3_q,
                                       aut_max_Y1_q,aut_max_Y2_q,aut_max_Y3_q
                                       )

data_time <- raw_data %>% select(unique_identifier,lcm)
data_productivity <- raw_data %>% select(unique_identifier,
                                         pro_min_Y0,pro_min_Y1,pro_min_Y2,pro_min_Y3,
                                         pro_mid_Y0,pro_mid_Y1,pro_mid_Y2,pro_mid_Y3,
                                         pro_max_Y0,pro_max_Y1,pro_max_Y2,pro_max_Y3)
data_baseline <- raw_data %>% select(unique_identifier,baseline_bottom,baseline_top,baselinepos_min,baselinepos_mid,baselinepos_max)
data_drivers <- raw_data %>% select(unique_identifier,
                                    dd_min_Y0,dd_min_Y1,dd_min_Y2,dd_min_Y3,
                                    dd_mid_Y0,dd_mid_Y1,dd_mid_Y2,dd_mid_Y3,
                                    dd_max_Y0,dd_max_Y1,dd_max_Y2,dd_max_Y3)
data_outsourcing <- raw_data %>% select(unique_identifier,out_considered_min,out_considered_mid,out_considered_max,out_yesno,out_year_min,out_year_mid,out_year_max,out_quarter_min,out_quarter_mid,out_quarter_max)
data_supply <- raw_data %>% select(unique_identifier,grep("hire",colnames),grep("exit",colnames))
data_drivervals <- raw_data %>% select(driver,year,min,mid,max)
data_driverhc <- raw_data %>% select(unique_identifier,driver_group)
data_tenure <- raw_data %>% select(unique_identifier,grep("tenure",colnames(raw_data)))
data_age <- raw_data %>% select(unique_identifier,grep("age_",colnames(raw_data)))
data_degree <- raw_data %>% select(unique_identifier,degree_yes,degree_no)

# data_drivermults_0s <- data_drivervals %>% filter(year==0) %>% select(-year)
# colnames(data_drivermults_0s) <- paste0(colnames(data_drivermults_0s),"_0")
# data_drivermults <- left_join(data_drivervals,data_drivermults_0s,by=c("driver"="driver_0"))
# data_drivermults <- data_drivermults %>%
#   mutate(min=min/min_0) %>%
#   mutate(mid=mid/mid_0) %>%
#   mutate(max=max/max_0) %>%
#   select(-min_0,-mid_0,-max_0)
# Turning all automation, productivity, drivers and outsourcing columns to numeric
data_automation[2:ncol(data_automation)] <- as.numeric(as.character(unlist(data_automation[2:ncol(data_automation)])))
data_productivity[2:ncol(data_productivity)] <- as.numeric(as.character(unlist(data_productivity[2:ncol(data_productivity)])))
data_drivers[2:ncol(data_drivers)] <- as.numeric(as.character(unlist(data_drivers[2:ncol(data_drivers)])))
data_outsourcing[2:ncol(data_outsourcing)] <- as.numeric(as.character(unlist(data_outsourcing[2:ncol(data_outsourcing)])))

# Calculations using chosen variables -----------------------------------------------------

# Producing list of L0s ---------------------------------------------------
output_l0_list <- data_sectioninfo %>%
  select(l0) %>%
  unique()
colnames(output_l0_list) <- "Unit"

# Producing list of L1s ---------------------------------------------------
output_l1_list <- data_sectioninfo %>%
  filter(l4=="N/A" & l3=="N/A" & l2=="N/A" & l1!="N/A") %>%
  select(l1) %>%
  unique()
colnames(output_l1_list) <- "Sector"

# Automation Calculations -------------------------------------------------

# Pulling in automation impact from research
b_output_forecast_automation <- unique(left_join(data_time,data_automation,by="unique_identifier"))

# Caluclating if LCM or max automation Y3 is greater (note higher the percentage, lower the impact)
b_output_forecast_automation <- b_output_forecast_automation %>%
  rowwise() %>%
  mutate("lcm_or_aut"= ifelse(is.na(lcm),"aut",ifelse(aut_max_Y3>(1-lcm),"aut","lcm")))

  # Pulling through max, mid and min LCM final values based on aut max, mid, min relationship
b_output_forecast_automation <- b_output_forecast_automation %>%
  rowwise() %>%
  mutate("lcm_max" = 1-lcm) %>%
  mutate("lcm_mid" = (1-(lcm * ((1-aut_mid_Y3) / (1-aut_max_Y3))))) %>%
  mutate("lcm_min" = (1-(lcm * ((1-aut_min_Y3) / (1-aut_max_Y3)))))

for(x in c("min","mid","max")){
  for(y in c("Y1","Y2","Y3")){
    outputcol <- paste("lcm",x,y,sep="_")
    inputcolY1 <- as.name(paste("aut",x,"Y1",sep="_"))
    inputcolY2 <- as.name(paste("aut",x,"Y2",sep="_"))
    inputcolY3 <- as.name(paste("aut",x,"Y3",sep="_"))
    maxcol <- as.name(paste("lcm",x,sep="_"))
    b_output_forecast_automation <- b_output_forecast_automation %>%
      rowwise() %>%
      mutate(!!outputcol := ifelse(is.na(lcm_or_aut)==TRUE,NA,
                                  ifelse(y=="Y3",!!maxcol,
                                         ifelse(y=="Y1",1-((1-!!maxcol)/(1-!!inputcolY3))*(1-!!inputcolY1),
                                                1-((1-!!maxcol)/(1-!!inputcolY3))*(1-!!inputcolY2)
                                                )
                                       )
                                )
      )
  }
}
# Pulling through lower of LCM and research number
for(x in c("min","mid","max")){
  for(y in c("Y1","Y2","Y3")){
    outputcol <- paste("autorlcm",x,y,sep="_")
    inputcolaut <- as.name(paste("aut",x,y,sep="_"))
    inputcollcm <- as.name(paste("lcm",x,y,sep="_"))
    b_output_forecast_automation <- b_output_forecast_automation %>%
      rowwise() %>%
      mutate(!!outputcol := ifelse(lcm_or_aut=="lcm",!!inputcollcm,!!inputcolaut))
  }
}

# Replacing NAs with 100%s, as these are sections we won't be optimising
b_output_forecast_automation[is.na(b_output_forecast_automation)] <- 1


# Bringing all together --------------------------------------

b_output_forecast_aut_prod <- unique(left_join(b_output_forecast_automation,data_productivity,by="unique_identifier"))
b_output_forecast_aut_prod_dd <- unique(left_join(b_output_forecast_aut_prod,data_drivers,by="unique_identifier"))
b_output_forecast_aut_prod_dd_out <- unique(left_join(b_output_forecast_aut_prod_dd,data_outsourcing,by="unique_identifier"))
b_output_forecast_aut_prod_dd_out_supply <- left_join(b_output_forecast_aut_prod_dd_out,data_supply,by="unique_identifier")
colnames=colnames(b_output_forecast_aut_prod_dd_out_supply) 

output_table <- left_join(data_sectioninfo,b_output_forecast_aut_prod_dd_out_supply,by="unique_identifier")

output_table <- left_join(data_baseline,output_table)
output_table <- left_join(output_table,data_driverhc)
output_table <- output_table %>%
  mutate(baseline_mid=(baseline_bottom+baseline_top)/2)

output_table <- left_join(data_hc,output_table)
