library(rsconnect)
library(shiny)
library(googleVis)
#library(shinyIncubator)
library(devtools)
library(dplyr)
library(foreach)
library(BH)
library(DT)
library(zoo)
library(mondate)
library(plyr)
library(RSQLite)
library(reticulate)
library(gsheet)
library(dplyr)
library(data.table)
library(rhandsontable)
library(reticulate)
source("global.R")

#reticulate::use_python("/usr/bin/python3", required = TRUE)

#initialising all python modules
source_python('DBconnect.py')
source_python('genfunc.py')
source_python('bcl.py')
source_python('boot.py')

shinyServer(function(input,output,session){
  
  #Class of Business selection
  updateSelectInput(session, 'class_selection', choices = GNFunctions$LOBList(all_rows,'Class'))
  updateSelectInput(session, 'boot_class_selection', choices = GNFunctions$LOBList(all_rows,'Class'))
  
  ##Controllers for BCL Method
  Acc <- reactive({
    switch(input$radio_csuite_freq,
           "Annually" = 'ACC_A',
           "Quarterly"= 'ACC_Q',
           "Monthly"  = 'ACC_M')
  })
  
  Dev <- reactive({
    switch(input$radio_csuite_freq,
           "Annually" = 'DEV_A',
           "Quarterly"= 'DEV_Q',
           "Monthly"  = 'DEV_M')
  })
  
  event_listener <- reactive({
    paste(input$class_selection,input$radio_csuite_freq)
  })
  
  ##Controllers for bootstrapping method
  Acc_boot <- reactive({
    switch(input$radio_boot_freq,
           "Annually" = 'ACC_A',
           "Quarterly"= 'ACC_Q',
           "Monthly"  = 'ACC_M')
  })
  
  Dev_boot <- reactive({
    switch(input$radio_boot_freq,
           "Annually" = 'DEV_A',
           "Quarterly"= 'DEV_Q',
           "Monthly"  = 'DEV_M')
  })
  
  event_listener_boot <- reactive({
    paste(input$boot_class_selection,input$radio_boot_freq)
  })

  ##Loading the claims data into variables
  result <- eventReactive(event_listener(),{GNFunctions$TriangleGenerator(all_rows,Acc(),Dev(),'Losses',input$class_selection)})
  result_boot <- eventReactive(event_listener_boot(),{GNFunctions$TriangleGenerator(all_rows,Acc_boot(),Dev_boot(),'Losses',input$boot_class_selection)})
  
  ##Loading Earned Premiums Data
  EarnedPremiums<-eventReactive(event_listener(),
    {
      GNFunctions$Premiums_Group(all_premiums,Dev(),"Earned_Premiums",input$class_selection)
    }
  )
  
  ##Development chart axis
  chart_axis<-reactive({
    switch(input$radio_csuite_freq,
           "Annually" = "{title: 'Development Years'}",
           "Quarterly"= "{title: 'Development Quarters'}",
           "Monthly" = "{title: 'Development Months'}")
  })
  
  ##Error handle function
  Error_handle<-reactive({
    validate(
      need(input$class_selection!="" ,"Please select a valid class of business")
    )
  })

#BCL Method
##BCL Triangle Table
  output$triangle_table<-renderDataTable({
    ##Error handling
    Error_handle()
    
    cumulative_triangle<<-GNFunctions$Cumulative_Sum(result())
    link_ratios<-BCLMethod$Link_Ratios(cumulative_triangle)
    cumulative_triangle_vF<-BCLMethod$Boot_Cum(cumulative_triangle,link_ratios)
    
    forecasted_triangle<<-BCLMethod$Forecasted_Triangle(BCLMethod$Development_Factors(cumulative_triangle_vF,link_ratios),cumulative_triangle)
    DT::datatable(forecasted_triangle,escape=F,extensions = c('FixedColumns','Scroller'),options=list(dom = 't',scrollX=T,scroller=T, scrollY = 500, fixedColumns = TRUE))%>%formatCurrency(1:ncol(forecasted_triangle),'')
  },sanitize.text.function = function(x)x)

  ##Link Ratio Table
  output$link_ratios<-renderRHandsontable({
    ##Error handling
    Error_handle()
    
    cumulative_triangle<-GNFunctions$Cumulative_Sum(result())
    linkratios<-BCLMethod$Link_Ratios(cumulative_triangle)
    linkratios_ed <<- linkratios
    rhandsontable(linkratios, width="100%",height = "100%")%>%hot_cols(renderer = "
                                         function(instance,td,row,col,prop,value,cellProperties){
                                         Handsontable.renderers.NumericRenderer.apply(this, arguments); 
                                          if (value < 1 | value > 9){td.style.background = 'pink';
                                          } 
                                         }",
                                        format = "0.0000", "factor_allow", allowInvalid = TRUE
                                                                       
                                                                       )
  })
  

  
  #Try Loading Assumptions table
  lrAssmptionsTB<-reactive({
    #an.error.occured<-FALSE
    
    #Reading the Loss Ratio Assumptions Table
    #tryCatch({dbReadTable(stats_db,"LR_Assumptions");0},error = function(e){an.error.occured<<-TRUE})
    TB<-if(dbExistsTable(stats_db,"LR_Assumptions")==TRUE){
    data.frame(dbReadTable(stats_db,"LR_Assumptions"),stringsAsFactors = FALSE, check.names = TRUE)
      print(data.frame(dbReadTable(stats_db,"LR_Assumptions"),stringsAsFactors = FALSE, check.names = TRUE))
    }else{
    cbind(Period=adjusted_table()[,0], "Method Selected" = "N/A","Description of Adjustment" = "", "Buffers" = 0)
    }
    
    return(TB)
  })
  
  
  ##Assumptions and adjustments to be made to the table
  output$reserving_assumptions_adjustments<-renderRHandsontable(
    {
      
      
      
      #z_adjlist <- list("N/A","Weighted Average","Seasonality")
      z_slctmethod <- list("BCL","BF","ULR")
      z_dfassumptions<-lrAssmptionsTB()#if(an.error.occured==FALSE){
        #dbReadTable(stats_db,"LR_Assumptions")
        #}else{
          #cbind(Period=adjusted_table()[,0], "Method Selected" = "N/A","Description of Adjustment" = "", "Buffers" = 0)
          #} #,"Loss Ratio Adjustments"="N/A"
      
      
      rhandsontable(z_dfassumptions)%>%
       # hot_col(col = "Loss Ratio Adjustments",type = "dropdown", source = z_adjlist, renderer = "
       #                                  function(instance,td,row,col,prop,value,cellProperties){
       #         Handsontable.renderers.TextRenderer.apply(this, arguments); 
       #         if (value == 'Weighted Average'){td.style.background = 'yellow';
       #         }
       #         else{
       #         if(value == 'Seasonality'){td.style.background = 'orange';
       #             }
       #         }
       #       }")%>%
        hot_col(col = "Buffers", renderer = "
                                         function(instance,td,row,col,prop,value,cellProperties){
                                         Handsontable.renderers.NumericRenderer.apply(this, arguments); 
                                          if (value <= 0.025 & value > 0.005){td.style.background = '#99ff99';
                                          }
                                          else{
                                              if(value > 0.025 & value <= 0.05){td.style.background = '#ffffcc';
                                              }
                                              else{
                                                   if(value > 0.05 ){td.style.background = '#ffe6cc';} 
                                              }
                                          }
                                         }", format = "%")%>%
        hot_col(col = "Method Selected",type = "dropdown", source = z_slctmethod, renderer = "
                                         function(instance,td,row,col,prop,value,cellProperties){
                Handsontable.renderers.TextRenderer.apply(this, arguments); 
                if (value == 'BCL'){td.style.background = 'yellow';
                }
                else if (value=='BF'){td.style.background = 'orange';

                }
                else{
                if(value == 'ULR'){td.style.background = '#99ff99';
                }
                }
    }")

    }
  )
 
  ##Clearing existing Loss Ratio Assumptions Table
  observeEvent(event_listener(),{
    dbSendQuery(stats_db, paste0(
      "DROP TABLE IF EXISTS LR_Assumptions;"
    )
    )
  })
  
  ##pushing the Loss Ratio Assumptions table to the database
  observeEvent(input$apply_adjustments,{
    if(is.null(stats_db)) return()
    dbWriteTable(stats_db,"LR_Assumptions",hot_to_r(input$reserving_assumptions_adjustments), row.names = TRUE, overwrite = TRUE)
    
  })
  
  
  ##Clearing existing link ratio table
  observeEvent(event_listener(),{
    dbSendQuery(stats_db, paste0(
      "DROP TABLE IF EXISTS Adjusted_Links;"
    )
    )
  })
  
  ##pushing the edited link ratio table to the database
  observeEvent(input$save_links,{
    if(is.null(stats_db)) return()
    dbWriteTable(stats_db,"Adjusted_Links",hot_to_r(input$link_ratios), row.names = TRUE, overwrite = TRUE)
    
  })
  

  
  
  ##Table for the Adjustments / Assumptions to the triangles
  adjusted_table<-eventReactive(paste(event_listener(),input$save_links,input$apply_adjustments),{
    an.error.occured<-FALSE
    
    #Reading the Adjusted Link Ratio Table
    tryCatch({dbReadTable(stats_db,"Adjusted_Links");0},error = function(e){an.error.occured<<-TRUE})
    #adjusted_linkratios<-dbReadTable(stats_db,"Adjusted_Links")
    adjusted_linkratios<-if(an.error.occured==FALSE){dbReadTable(stats_db,"Adjusted_Links")[,-1]}else{linkratios_ed}
    
    #Reading the Loss Ratio Assumptions Table
    tryCatch({dbReadTable(stats_db,"LR_Assumptions");0},error = function(e){an.error.occured<<-TRUE})
    
    
    cumulative_triangle_vF<-BCLMethod$Boot_Cum(cumulative_triangle,adjusted_linkratios)
    adjusted_triangle<-BCLMethod$Forecasted_Triangle(BCLMethod$Development_Factors(cumulative_triangle_vF,adjusted_linkratios),cumulative_triangle)
    
    adjusted_dp<-cumulative_triangle[,ncol(cumulative_triangle)]/adjusted_triangle[,ncol(adjusted_triangle)]
    adjusted_Ult<-adjusted_triangle[,ncol(adjusted_triangle)]
    bf_ultimates<-adjusted_dp*adjusted_triangle[,ncol(adjusted_triangle)] + (1-adjusted_dp)*0.8*EarnedPremiums()[,2]
    
    #selected ultimate claims
    selectedUlt<-if(an.error.occured==FALSE){
      lr_assumpt<-dbReadTable(stats_db,"LR_Assumptions")
      data.frame(GNFunctions$UpdateLRAssumpt(lr_assumpt,adjusted_Ult,bf_ultimates))
    }else(adjusted_Ult)
    
    ##Selected Projected Claims
    selectedUltimates<-data.frame(selectedUlt)
    colnames(selectedUltimates)<-"Selected Ultimate Claims"
    
    ##Final Ultimate Loss Ratio
    FinUltLR<-data.frame(selectedUlt/EarnedPremiums()[,2])
    colnames(FinUltLR)<-"Final Ultimate Loss Ratios"
    
    ##IBNR Derived
    IBNR<-data.frame(selectedUlt - cumulative_triangle[,ncol(cumulative_triangle)])
    colnames(IBNR)<-"IBNR (& IBNER)"
    
    
    build_up_LR<-cbind("Accident Period" = cumulative_triangle[,0],
                       "Earned Premiums" = EarnedPremiums()[,2],
                       "Paid-to-Date Claims" = cumulative_triangle[,ncol(cumulative_triangle)],
                       "Paid-to-Date Loss Ratios" = cumulative_triangle[,ncol(cumulative_triangle)]/EarnedPremiums()[,2],
                       "Outstanding Claims" = 0,
                       "Reported-to-Date Claims" = cumulative_triangle[,ncol(cumulative_triangle)],
                       "Reported-to-Date Loss Ratios" = cumulative_triangle[,ncol(cumulative_triangle)]/EarnedPremiums()[,2],
                       "ELR" = 0.8,
                       "Development to Ultimate (Unadjusted)" = cumulative_triangle[,ncol(cumulative_triangle)]/forecasted_triangle[,ncol(forecasted_triangle)],
                       "Unadjusted Ultimate Claims" = forecasted_triangle[,ncol(forecasted_triangle)],
                       "Unadjusted Ultimate Loss Ratios" = forecasted_triangle[,ncol(forecasted_triangle)]/EarnedPremiums()[,2],
                       "Development to Ultimate (Link Ratio Adjusted)" = adjusted_dp,
                       "Link Ratio Adjusted Ultimate Claims" = adjusted_Ult,
                       "Impact of Link Ratio Adjustments" = adjusted_triangle[,ncol(adjusted_triangle)] - forecasted_triangle[,ncol(forecasted_triangle)],
                       "Link Ratio Adjusted Ultimate Loss Ratios" = adjusted_Ult/EarnedPremiums()[,2],
                       "BF Method Ultimate Claims" = bf_ultimates,
                       "Selected Ultimate Claims" = selectedUltimates,
                       "Final Ultimate Loss Ratios" = FinUltLR, #SelectedUltimate()/EarnedPremiums()[,2],
                       "IBNR (& IBNER)" = IBNR
    )
    
    return((build_up_LR))#,"Total" = colSums(build_up_LR)))
    
  })
  
  ##Weighted Average Loss Ratio Table
  #output$weighted_average_table<-renderRHandsontable(
    #wa_table<-data.frame("Period from" = 2020)
    #rhandsontable(rbind("Period from" = "2020", "Period to" = "2020", "WALR" =1))
  #)
  
  ##Seasonality data table
 # seasonality_table<-reactive(
 #   {
 #     adjusted_table<-adjusted_table()
 #     lr_df<-data.frame(Period = row.names(adjusted_table),LR = adjusted_table[,"Link Ratio Adjusted Ultimate Loss Ratios"])
 #     ep_df<-data.frame(adjusted_table[,"Earned Premiums"])
 #     GNFunctions$Seasonality(lr_df,ep_df)
 #   }
 # )
  
  ##Seasonality Factors Output Table
  #output$seasonality_table<-renderRHandsontable(
  #  {
  #    if(input$radio_csuite_freq=="Annually"){} else {rhandsontable(seasonality_table())}
  #  }
  #)
  
  ##Seasonality Line Chart
  #output$seasonality_charts<-renderGvis(
  #  {
  #    seas_tab<-data.frame(Period = row.names(seasonality_table()),seasonality_table(),check.names = F, stringsAsFactors = F)
  #    if(input$radio_csuite_freq=="Annually"){}
  #    else{
  #      gvisComboChart(seas_tab,
  #                     options=list(title="Seasonality",legend="none",seriesType = "line", isStacked = T, vAxis="{format:'#,###%'}"))
  #      }
  #  }
  #)
  
  ##IBNR Margin Charts
  output$ibnr_chart<-renderGvis(
    {
      at<-adjusted_table()
      ibnr_margin<-data.frame(Period=row.names(at),LR=(at[,"Final Ultimate Loss Ratios"]-at[,"Reported-to-Date Loss Ratios"]))
      gvisComboChart(ibnr_margin,
                     options=list(title="IBNR Margin",legend="none",seriesType = "line", isStacked = T, vAxis="{title:'IBNR as % of EP',format:'#,###%'}"
                                  , hAxis="{title:'Accident Period'}"))
    }
  )
  
  #Output for Ultimate / Reserve Calculation
  output$reserving_basis<-renderRHandsontable({

    rhandsontable(adjusted_table())%>%
      hot_cols(format = "0,000,000.00")%>%
      hot_col(col = c("Paid-to-Date Loss Ratios","Reported-to-Date Loss Ratios","Unadjusted Ultimate Loss Ratios",
                      "Link Ratio Adjusted Ultimate Loss Ratios","Development to Ultimate (Unadjusted)","Development to Ultimate (Link Ratio Adjusted)",
                      "ELR","Final Ultimate Loss Ratios" ),
              format = "0.00%", renderer = "
                                         function(instance,td,row,col,prop,value,cellProperties){
                                         Handsontable.renderers.NumericRenderer.apply(this, arguments); 
                                          if (value <= 0.5 ){td.style.background = '#99ff99';
                                          }
                                          else{
                                              if(value > 1 & value <= 1.05){td.style.background = '#fff2e6';
                                              }
                                              else{
                                                   if(value > 1.05 & value < 1.25){td.style.background = '#ffe6cc';} 
                                                    else{
                                                      if(value > 1.25){td.style.background = 'pink';}
                                                      else if(value > 0.5 & value <=0.75){td.style.background = '#ccffcc';}
                                                      else if(value < 1 & value >0.75){td.style.background = '#ffffcc';}
                                                    }
                                              }
                                          }
                                         }")
  })

  output$development_patterns<-renderGvis({
    ##Error handling
    Error_handle()
    
    cumulative_triangle<-GNFunctions$Cumulative_Sum(result())
    dev_pattern <- BCLMethod$Development_Pattern(BCLMethod$Link_Ratios(cumulative_triangle))
    gvisComboChart(data.frame(rownames(dev_pattern),dev_pattern, check.names = F),options=list(title="Development Patterns",legend="none",seriesType = "line", isStacked = T, height=400,vAxis="{maxValue:1.1, format:'#,###%'}",hAxis = chart_axis()))
    
  })
  
#Bootstrapping Method

  ##Bootstrapping Triangle forecasted using BCL
  output$triangle_table_boot<-renderDataTable({
    
    validate(
      need(input$boot_class_selection!="" ,"Please select a valid class of business")
    )
    cumulative_triangle<-GNFunctions$Cumulative_Sum(result_boot())
    link_ratios<-BCLMethod$Link_Ratios(cumulative_triangle)
    cumulative_triangle_vF<-BCLMethod$Boot_Cum(cumulative_triangle,link_ratios)
    
    forecasted_triangle<-BCLMethod$Forecasted_Triangle(BCLMethod$Development_Factors(cumulative_triangle_vF,link_ratios),cumulative_triangle)
    percentile<<-forecasted_triangle[,ncol(forecasted_triangle)] - cumulative_triangle[,ncol(cumulative_triangle)]
    DT::datatable(forecasted_triangle,escape=F,extensions = c('FixedColumns','Scroller'),options=list(title=paste(input$boot_class_selection," Reserve Projections"),dom = 't',scrollX=T,scroller=T, scrollY = 500, fixedColumns = TRUE))%>%formatCurrency(1:ncol(forecasted_triangle),'')
  },sanitize.text.function = function(x)x)
  
  ##Link Ratio Table for bootstrapping
  output$link_ratios_boot<-renderDataTable({
    validate(
      need(input$boot_class_selection!="" ,"Please select a valid class of business")
    )
    cumulative_triangle<-GNFunctions$Cumulative_Sum(result_boot())
    linkratios<-BCLMethod$Link_Ratios(cumulative_triangle)
    DT::datatable(linkratios,escape=F,editable = T,extensions = c('FixedColumns','Scroller'),options=list(dom = 't',scrollX=T,scroller=T, scrollY = 500, fixedColumns = TRUE))%>%formatSignif(1:ncol(linkratios),5)
  },sanitize.text.function = function(x)x)
  

    
  ##Bootstrapping Results
  boot_results<-eventReactive(input$simulate_boot,{
    cumulative_triangle<-GNFunctions$Cumulative_Sum(result_boot())
    linkratios<-BCLMethod$Link_Ratios(cumulative_triangle)
    resulting_boot<-BTMethod$Bootstrap_Simu(input$boot_number,linkratios,cumulative_triangle)
    boot_out<-BTMethod$Boot_Sum(resulting_boot)

    return(data.frame(resulting_boot, check.names = F))
  })
  
  #Histogram Data for the histogram chart
  boot_out<-eventReactive(input$simulate_boot,{
    BTMethod$Boot_Sum(boot_results())
  })
  
  #Reserve Percentiles
  boot_q<-eventReactive(input$simulate_boot,{
    data.frame("Reserve Percentiles" = quantile(boot_out()[,1]),check.names = F)
  })
  
  #Cumulative Distribution Function
  perc_ecdf<-eventReactive(input$simulate_boot,{
    perc_cdf<-ecdf(boot_out()[,1])
    string_per<-as.numeric(sum(data.frame(percentile)))
    return(perc_cdf(string_per)*100)#percentiles of the reserves
  })
  
  string_perc<-eventReactive(input$simulate_boot,{
      as.numeric(sum(data.frame(percentile)))
    })
  
  #the string to render on the interface with the percentile info
  str1<-eventReactive(input$simulate_boot,{
    paste("The derived reserves of: ",format(string_perc(),big.mark=",")," falls at the ",perc_ecdf()," percentile")
    })
  

  
  output$simulated_results_boot<-renderDataTable({
    DT::datatable(boot_results(),escape=F,editable = T,extensions = c('FixedColumns','Scroller'),options=list(dom = 't',scrollX=T,scroller=T, scrollY = 500, fixedColumns = TRUE))%>%formatCurrency(1:ncol(boot_results()),'')
  },sanitize.text.function = function(x)x)
  
  #Bootstrap Simulations Progress bar
  Progress_Bar<-function(simulations){
    withProgress(message = 'Simulations',
                 detail = "0%",value=0,{
                   for (i in 1:simulations){
                     incProgress(1/simulations, detail = paste(i," - Progress",i/simulations*100,"%"))
                     if (input$radio_boot_freq=="Annually"){
                       Sys.sleep(1)
                     } else if(input$radio_boot_freq=="Quarterly") {
                       Sys.sleep(5)
                     } else {
                       Sys.sleep(12)
                     }
                   }
                 })
              }
  
  ##Histogram of ultimate claims / reserves
  output$boot_histogram<-renderGvis({
    Progress_Bar(input$boot_number)
    gvisHistogram(boot_out(),options=list(height=337,title=paste("Histogram of",input$boot_class_selection," Simulated Reserves"),legend="none",seriesType = "bar",isStacked=F,hAxis="{title:'Reserves'}",vAxis="{title:'Frequency'}", height=400))
  })
  
  ##Quantiles of the Simulated Reserves
  output$boot_quantiles<-renderDataTable({
    
    DT::datatable(boot_q(),escape=F)%>%formatCurrency(1:ncol(boot_results()),'')
  })
  
  ##Percentile Text Output
  output$percentile_text<-renderUI({

    HTML(paste(str1()))
  })
  
  # Data upload fileinput rendering and event handler-----------------------------------------
  ##For uploading  data into the googlesheets
  event_listening <- reactive({
    paste(input$data_upload)
  })
  
  observeEvent(event_listening(),{
    output$Financials <-renderUI({
      fileInput("File","Select File")
    })
  })
  
#Jumping to the Simulated Results Tab on Simulation button click
  observeEvent(input$simulate_boot,{
    updateTabsetPanel(session,"boot_panel",selected = "ru_boot")
  })

    observeEvent(input$File,{
    #gs_edit_cells(input_data(),ws=1,input=data.frame(read.csv(input$File$datapath),check.names=FALSE),anchor='A1')
  })
  
  #Refreshing the application
  observeEvent(input$refresh_button,{
    session$reload()
  })
  
  
  #========================================================================================================================================================================
  #Terminating the Application on closing the browser
  #session$onSessionEnded(stopApp)
  
})