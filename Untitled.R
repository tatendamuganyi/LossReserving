library(rsconnect)
library(shiny)
library(googleVis)
library(shinyIncubator)
library(devtools)
library(htmlwidgets)
library(sparkline)
library(dplyr)
library(foreach)
library(BH)
library(DT)
library(zoo)
library(mondate)
library(plyr)
library(RCurl)
library(RMySQL)
library(shinyjs)
library(RSQLite)
library(reticulate)
library(gsheet)
library(reshape2)
library(dplyr)
library(googlesheets)
library(formattable)
#reticulate::use_python('/Volumes/Martinez/anaconda3/lib/python3.6/site-packages', required = TRUE)
source_python('ref_functions.py')
result = TriangleGenerator(all_rows,'Acc_Q','Dev_Q','Claims_Amount','Aviation')
cumulative_triangle<-Cumulative_Sum(result)
linkratios<-Link_Ratios(cumulative_triangle)
ZeroLink(linkratios)
dev<-Development_Factors(boot,linksd)
dev
fore<-Forecasted_Triangle(Development_Factors(cumulative_triangle,adj),cumulative_triangle)
linksd<-Bootstrap_init(linkratios,cumulative_triangle)
ZeroLink(linksd)

result[,1]<-row.names(result)
EP<-Premiums_Group(all_premiums,'Dev_Q','Earned_Premiums','Aviation')
lr<-result[,2]/EP[,2]
lr_a<-data.frame(Period = EP[,1],Loss_ratios = lr)
sess<-Seasonality(lr_a,EP[,2])
sess["Q1",]
sess_a<-data.frame(Period = row.names(sess),sess, check.names = FALSE)
plot(gvisComboChart(sess_a,options = list(seriesType = "line")))

test<-Bootstrap_init(linkratios,cumulative_triangle)
boot<-Boot_Cum(cumulative_triangle,test)
dev<-Development_Factors(boot,test)
Forecasted_Triangle(dev,cumulative_triangle)

simulations = Bootstrap_Simu(1000,linkratios,cumulative_triangle)
x<-data.frame(Boot_Sum(simulations))
colnames(x)<-"df"
hist(t(x))
plot(gvisHistogram(x))
quantile(x[,1])
percentile<-ecdf(x[,1])
percentile(242882721)

shiny::runGitHub("rhandsontable", "jrowen", subdir = "inst/examples/rhandsontable_corr")
if((dbReadTable(stats_db,"Adjusted_Links"))){0}
an.error.occured<<-TRUE
tryCatch({dbReadTable(stats_db,"Adjusted_Links");0},error = function(e){an.error.occured<<-TRUE})
print(an.error.occured)
adj<-dbReadTable(stats_db,"Adjusted_Links")[,-1]

source_python('PrimeGen.py')
Prime_Numbers(3)

