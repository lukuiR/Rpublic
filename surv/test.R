# trt:	1=standard 2=test
# celltype:	1=squamous, 2=smallcell, 3=adeno, 4=large
# time:	survival time
# status:	censoring status
# karno:	Karnofsky performance score (100=good)
# diagtime:	months from diagnosis to randomisation
# age:	in years
# prior:	prior therapy 0=no, 10=yes

#http://www.sbim-paris7.com/
#http://www.sbim-paris7.com/member/sylvie-chevret/


#https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/

library(survival)
head(veteran ,5)

library(plotly)
library(GGally)
library(ranger)

km <- with(veteran, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=veteran)
summary(km_fit, times = c(1,30,60,90*(1:10)))

head(km_fit,80)

autoplot(km_fit)

plot_ly(x=~km_fit$time,y=~km_fit$upper, type ="scatter",mode = "lines", showlegend = FALSE, name = 'Upper 95%',
        line = list(color = 'transparent'))%>% 
  add_trace(x=~km_fit$time,y= km_fit$lower,
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Lower 95%') %>%
  add_trace(x=~km_fit$time,y=km_fit$surv,line = list(color='rgb(0,100,80)'),
            name = 'Survive')

km_trt_fit <- survfit(Surv(time, status) ~ trt, data=veteran)

plot_ly(x=~km_trt_fit$time,y=~km_trt_fit$upper, type ="scatter",mode = "lines", showlegend = FALSE, name = 'Upper 95%',
        line = list(color = 'transparent'))%>% 
  add_trace(x=~km_trt_fit$time,y=km_trt_fit$lower,
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Lower 95%') %>%
  add_trace(x=~km_trt_fit$time,y=km_trt_fit$surv,line = list(color='rgb(0,100,80)'),
            name = 'Survive')

km_trt1_fit <- survfit(Surv(time, status)~1 , data=veteran[veteran$trt==1,])
plot_ly(x=~km_trt1_fit$time,y=~km_trt1_fit$upper, type ="scatter",mode = "lines", showlegend = FALSE, name = 'Upper 95%',
        line = list(color = 'transparent'))%>% 
  add_trace(x=~km_trt1_fit$time,y=km_trt1_fit$lower,
            fill = 'tonexty', fillcolor='rgba(100,0,80,0.2)', line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Lower 95%') %>%
  add_trace(x=~km_trt1_fit$time,y=km_trt1_fit$surv,line = list(color='rgb(100,0,80)'),
            name = 'Survive')%>% 
  layout(yaxis = list(tickformat = "%"))

km_trt2_fit <- survfit(Surv(time, status)~1 , data=veteran[veteran$trt==2,])

plot_ly(x=~km_trt2_fit$time,y=~km_trt2_fit$upper, type ="scatter",mode = "lines",  name = 'Upper 95% trt 2',showlegend = FALSE,
        line = list(color = 'transparent'))%>% 
  add_trace(x=~km_trt2_fit$time,y=km_trt2_fit$lower,
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
            showlegend = TRUE, name = 'Lower 95% trt 2') %>%
  add_trace(x=~km_trt2_fit$time,y=km_trt2_fit$surv,line = list(color='rgb(0,100,80)'),
            showlegend = TRUE, name = 'Survive trt 2')%>% 
  add_trace(x=~km_trt1_fit$time,y=~km_trt1_fit$upper, type ="scatter",mode = "lines",  name = 'Upper 95% trt 1',showlegend = FALSE,
            line = list(color = 'transparent'))%>% 
  add_trace(x=~km_trt1_fit$time,y=km_trt1_fit$lower,
            fill = 'tonexty', fillcolor='rgba(100,0,80,0.2)', line = list(color = 'transparent'),
            showlegend = TRUE, name = 'Lower 95% trt1') %>%
  add_trace(x=~km_trt1_fit$time,y=km_trt1_fit$surv,line = list(color='rgb(100,0,80)'),
            showlegend = TRUE, name = 'Survive trt 1')%>% 
  layout(xaxis = list(title= "Survival time"),yaxis = list(title= "Surv %",tickformat = "%"))



vet <- mutate(veteran, AG = ifelse((age < 60), "LT60", "OV60"),
              AG = factor(AG),
              trt = factor(trt,labels=c("standard","test")),
              prior = factor(prior,labels=c("N0","Yes")))

cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior , data = vet)
summary(cox)
cox_fit <- survfit(cox)

aa_fit <-aareg(Surv(time, status) ~ trt + celltype +
                 karno + diagtime + age + prior , 
               data = vet)
aa_fit$times
plot_ly(x=~aa_fit$times,y=~aa_fit$tweight, type ="scatter",mode = "lines")



r_fit <- ranger(Surv(time, status) ~ trt + celltype + 
                  karno + diagtime + age + prior,
                data = vet,
                mtry = 4,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)

# Average the survival models
death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)

# Plot the survival models for each patient
plot(r_fit$unique.death.times,r_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")

#
cols <- colors()
for (n in sample(c(2:dim(vet)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))

##all
kmi <- rep("KM",length(km_fit$time))
km_df <- data.frame(km_fit$time,km_fit$surv,kmi)
names(km_df) <- c("Time","Surv","Model")

coxi <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")

rfi <- rep("RF",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")

plot_df <- rbind(km_df,cox_df,rf_df)

plot_ly(plot_df,x=~Time, y=~Surv, color=~Model, type ="scatter",mode = "lines")


