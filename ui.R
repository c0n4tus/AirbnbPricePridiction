library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(ggmap)
library(dplyr)
library(ggrepel)
library(gridExtra)
library(tidyverse)
library(splitstackshape)
library(utils)
library(caret)
library(LambertW)
library(rsconnect)
library(shinythemes)
rsconnect::setAccountInfo(name='airbnbprediction', token='0EFA6600187FCD39BF71AEE71FC5E908', secret='ImrFzML7ahewL6MuMRPvKU79hXBOnlUIPA6LdFxU')
source("server.R")
shinyUI(fluidPage(theme=shinytheme("flatly"),
                  
                  navbarPage(title='Airbnb',
                    tabPanel(
                      'Prediction Model',
                      sidebarLayout(
                        sidebarPanel(
                          
                          # Row 1
                          fluidRow(
                            
                            # Column 1
                            column(width = 6,
                                   selectInput("selectBeds", label = strong("Beds"),
                                               choices = list(choices = sort(unique(mymap$beds))),
                                               selected = 1),
                                   selectInput("selectBathrooms", label = strong("Bathrooms"),
                                               choices = list(choices = sort(unique(mymap$bathrooms))),
                                               selected = 1),
                                   selectInput("city", label = strong("City"),
                                               choices = list(choices = sort(unique(mymap$city))))
                            ),
                            
                            # Column 2
                            column(width = 6,
                                   selectInput("selectPropertyType", label = strong("Property Type"),
                                               choices = list('Boat',
                                                              'Boutique Hotel',
                                                              'Condominium',
                                                              'Dorm',
                                                              'Guest Suite',
                                                              'House',
                                                              'In Law',
                                                              'Loft',
                                                              'Other',
                                                              'Timeshare'),
                                               , multiple=FALSE),
                                   radioButtons("roomType", label = strong("Room Type"),
                                                choices = list("Private Room" = 1, "Shared Room" = 0), selected = 1, inline = FALSE)
                            )
                          ),
                          
                          hr(),
                          
                          # SecondRow
                          fluidRow(
                            
                            # Column 1
                            column(width = 6,
                                   sliderInput("noReviews", label = strong("Number Of Reviews"), min = 1,
                                               max = max(as.integer(mymap$number_of_reviews)), value = 50, step = 1)
                            ),
                            
                            # Column 2
                            column(width = 6,
                                   
                                   sliderInput("scoreRating", label = strong("Review Score Rating"), min = 1,
                                               max = 100, value = 85, step = 1)
                            )
                            
                          ),
                          
                          hr(),
                          
                          # ThirdRow
                          fluidRow(
                            
                            # Column 1
                            column(width = 6,
                                   checkboxGroupInput("amenities", "Amenities:",
                                                      c("Air conditioning" = "ac",
                                                        "Essentials" = "essn",
                                                        "Familykid friendly" = "ff",
                                                        "Hair dryer" = "hd",
                                                        "Hangers" = "han",
                                                        "Heating" = "ht",
                                                        "Laptop Friendly workspace" = "lfw",
                                                        "Shampoo" = "shm",
                                                        "Smoke Detector" = "sd",
                                                        "Washer" = "wsh"))
                            )
                          )
                        ),
                        
                        mainPanel(
                          h2("Airbnb Prediction Model"),
                          hr(),
                          h5(""),
                          h5("Developed by: Wasinee Opal Sriapha - Liangzi Zhang - Yiqun Liu - Shivam Jakhanwal"),
                          h5("Date: April 4th, 2020"),
                          
                          hr(),
                          
                          tabsetPanel(
                            tabPanel("Predict House Price", 
                                     
                                     
                                     fluidRow(
                                       tags$br(),
                                       column(4,
                                              strong("Predicted Price: "),
                                              verbatimTextOutput("predVal")
                                              #verbatimTextOutput("txt")
                                       ),
                                       
                                       column(4,
                                              strong("Lower Estimate: "),
                                              verbatimTextOutput("lwrPredVal")
                                       ),
                                       
                                       column(4,
                                              strong("Upper Estimate: "),
                                              verbatimTextOutput("uprPredVal")
                                       )
                                       
                                     ),fluidRow(
                                       hr(),
                                       h4("Price Predicted for all Cities with the Selected Features"),
                                       plotlyOutput("allCityPredict")
                                     ),
                          
                                     fluidRow(
                                       hr(),
                                       h4("Checkout the factors that affect Property Prices"),
                                       plotOutput("barPlot")
                                     )
                                     
                            ) 
                          )
                        )
                      )
                    ),
                    tabPanel(
                      'Data Visualization',
                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(width = 4,
                          fluidRow(
                            column(width = 5,
                                   selectInput("selectCity", label = strong("City"),
                                               choices = list('DC','SF','Boston','LA','NYC','Chicago','AllCity'),
                                               , multiple=FALSE)),
                            column(width = 5, selectInput("selectVisualizationType", label = strong("Visualization Type"),
                                               choices = list('Map','Amentity','RoomType','PropertyType'),
                                               ,multiple=FALSE)
                            )
                          )
                        ),
                        mainPanel(
                          h2("Airbnb Data Visualization"),
                          hr(),
                          h5("Developed by: Wasinee Opal Sriapha - Liangzi Zhang - Yiqun Liu - Shivam Jakhanwal"),
                          h5("Date: April 4th, 2020"),
                          hr(),
                          tabsetPanel(
                            tabPanel("Visualization", 
                          fluidRow(column(6,
                          tags$br(),
                          plotOutput("citymap", width = "600px", height = "500px")))),
                          tabPanel("Amenities", 
                          fluidRow(column(6,
                           tags$br(),
                          plotlyOutput('top10', width = "250%", height = "300%"))))
                          )
                        )
                      )
                    )
                    
                  )
))
