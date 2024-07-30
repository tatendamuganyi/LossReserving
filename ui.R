library(shiny)
library(shinydashboard)
#library(shinyIncubator)
library(shinyBS)
library(DT)
library(BH)
library(zoo)
library(mondate)
library(shinysky)
library(rhandsontable)

Date.auto<-as.Date(as.yearmon(Sys.Date()),frac=0)-1
Date.auto.lag<-as.Date(as.yearmon(Sys.Date()-350),frac=1)
shinyUI(

  navbarPage(tagList(("Loss"),shiny::icon("sun-o fa-spin text-yellow"),("Reserving")),id="tabs",
             footer=tagList(icon('copyright'),paste( year(strptime(as.Date(as.yearmon(Sys.Date()),frac=0),format='%Y-%m-%d')),"Loss Reserving by Tatenda Muganyi | All rights reserved | Version 0.1.0")),
             windowTitle = "loss_reserving",inverse = TRUE, collapsible = TRUE,fluid = TRUE,#theme="grayscale.css",

             ##Home Tab
             tabPanel(tagList(icon("home"),"Home"),
                      tags$div(img(src='cogs.jpg',width="100%",height="25%"))
                      ),
             ##Claims Triangulation Tab#########################################################
                    
                    tabPanel(tagList(icon("line-chart"),"Claims Projections"),
                             tabsetPanel(
                               tabPanel("Claims Triangle",
                                        column(
                                          width=6, radioButtons("radio_csuite_freq","Period",c("Monthly","Quarterly","Annually"),inline = TRUE)
                                        ),
                                        column(
                                          width =6 ,selectInput("class_selection","Select Class of Business:",choices = NULL, multiple = TRUE)
                                        ),
                                        wellPanel(width=12,"Table showing the derivation of ultimate claims based on Basic Chain Ladder Method",
                                                      dataTableOutput("triangle_table")
                                               )
                                        ),
                               tabPanel("Development Pattern",
                                 wellPanel(width=12,"Chart showing the development patterns for each accident period",
                                           htmlOutput("development_patterns")
                                           )
                               ),
                               tabPanel("Link Ratios",
                                        wellPanel(width=12, "Adjustments can be made to the link ratios, i.e. neutralising the link ratios of a particular accident period / development period",
                                                  actionButton("save_links",  label=tagList("Save Edited Link Ratios",icon("upload")),style = "primary"),
                                                  rHandsontableOutput("link_ratios"),
                                                  actionButton('add_assumptions',label= "Additional Assumptions", style = "primary")
                                                  )
                                        ,
                               bsModal('addAssumptions', 

                                        "Adjustments / Assumptions", 'add_assumptions', size ='large',
                                        wellPanel("Paid-to-Date, Reported-to-Date and Ultimate Loss Ratios Table:",width='auto',
                                                  rHandsontableOutput("reserving_basis")
                                               ),
                                        
                                                      wellPanel(h3("Adjustments / Assumptions"),
                                                                actionButton("apply_adjustments",  label=tagList("Apply Adjustments",icon("ok-circle",lib="glyphicon")),
                                                                             style = "primary"),
                                                                splitLayout(cellWidths = c("55%","45%"),
                                                                            wellPanel("Apply the necessary adjustments in the table below:",rHandsontableOutput("reserving_assumptions_adjustments")),
                                                                            #wellPanel("Weighted Average LR",rHandsontableOutput("weighted_average_table"))
                                                                                    #),         
                                                                            #wellPanel("Seasonality Factors:",rHandsontableOutput("seasonality_table")),
                                                                                      #wellPanel("Chart showing seasonality for each accident year:",htmlOutput("seasonality_charts")),
                                                                                      wellPanel(htmlOutput("ibnr_chart"))
                                                                                      
                                                                    )
                                                                )
                                                      
                                        )
                               ),
                               tabPanel("Final Reserves",
                                        wellPanel(width=12,
                                                  dataTableOutput("dummytable")
                                                  )
                                        )
                             )
                    ),
             ##Bootstrapping tab#################################################################
             
                    tabPanel(
                      tagList(icon("bar-chart"),"Bootstrapping"),
                      tabsetPanel(id = "boot_panel",
                        tabPanel("Claims Triangle", value = "ct_boot" ,
                                 column(width=4,
                                        radioButtons("radio_boot_freq","Period",c("Monthly","Quarterly","Annually"),inline = TRUE)
                                        ),
                                 column(width=4,
                                        selectInput("boot_class_selection","Select Class of Business:",choices = NULL, multiple = TRUE)
                                        ),
                                 column(width=4,
                                        conditionalPanel("input.boot_class_selection",
                                                        numericInput("boot_number", "Number of Simulations:", 100, min = 1, max = 10000),
                                                        actionButton("simulate_boot", label=tagList("Simulate",icon("equalizer",lib="glyphicon")), 
                                                                     style="primary")
                                                         )
                                        ),

                                 wellPanel(width=12, "Table showing the derivation of ultimate claims based on Basic Chain Ladder Method to be used for Simulations",
                                           dataTableOutput("triangle_table_boot")
                                           )
                                 ),
                        tabPanel("Link Ratios",value="lr_boot",
                                 wellPanel(width=12,"These link ratios are used for simulation purposes only, no adjustments are possible in the cells.",
                                           dataTableOutput("link_ratios_boot")
                                           )
                                 ),
                        tabPanel("Simulated Results",value = "sr_boot",
                                 wellPanel(width=12, "Below are the resulting ultimate claims derived from bootstrapping.",
                                           dataTableOutput("simulated_results_boot")
                                           )
                                 ),
                        tabPanel("Reserve Uncertainty", value = "ru_boot",
                                 splitLayout(cellWidths = c("50%","50%"),
                                              wellPanel(width=6,htmlOutput("boot_histogram")),
                                              wellPanel(width=6,"The table shows the percentiles of the Simulated Reserves",dataTableOutput("boot_quantiles"),
                                                  htmlOutput("percentile_text")
                                                        )
                                            )
                                           
                                 )
                      )
                    ),
                    tabPanel(
                      tagList(icon("file"),"Reporting")
                    ),

                    tabPanel(tagList(icon("database"),"Upload/Download"),
                            fluidRow(
                              column(width=12,height=580,
                                     box(width=NULL, title="Data Input",column(width=6,selectInput("data_upload","Select Input File Name",choice=c("Premiums","Paid Claims","Outstanding Claims")), actionButton("refresh_button", "Retrieve data")),
                                         column(width=6,uiOutput("Financials"), 
                                         actionButton("upload_dowload_info","Upload / Download Info")),
                                         bsModal("upload_download", "Information on the format of the data to upload", "upload_dowload_info", size = "large",
                                                 htmlOutput("upload_format_info"))
                                     )
                              )
                            )
                            
                    )
                )
)