#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
library(leaflet)
library(leaflet.minicharts)

source("Viz6600_shivam_opal.R")
mymap<- read.csv("df_new10.csv")
aaa <- read.csv("df_new55.csv")
mymap1 <- subset(aaa, aaa$property_type== c("Apartment","House","Condominium","Townhouse","Loft"))

source("Viz6600_shivam_opal.R")

nyc<-subset(mymap1, mymap1$city=="NYC")
sf<-subset(mymap1, mymap1$city=="SF")
bos<-subset(mymap1, mymap1$city=="Boston")
chi<-subset(mymap1, mymap1$city=="Chicago")
dc<-subset(mymap1, mymap1$city=="DC")
la<-subset(mymap1, mymap1$city=="LA")

Chicagorange <- c(left = -87.936287,
                  bottom = 41.679835,
                  right = -87.447052,
                  top = 42.000835)
DCrange <- c(left = -77.11979522,
             bottom = 38.79164435,
             right = -76.867218,
             top = 39.031386)
Bostonrange<- c(left = -71.2,
                bottom = 42.23,
                right = -70.95,
                top = 42.4)
Larange<- c(left = -119,
            bottom = 33.7,
            right = -117.7,
            top = 34.55)
NYCrange<- c(left = -74.3,
             bottom = 40.4772,
             right = -73.7,
             top = 41)
SFrange<- c(left = -122.6,
            bottom = 37.69,
            right = -122.3,
            top = 37.82)


# Define server logic required to draw a histogram
#'DC','SF','Boston','LA','NYC','Chicago'
#'Map','Amentity','RoomType','PropertyType'
shinyServer(function(input, output, session) {
  
  
  
  dataHandled<-reactive({
    
    property_type_Boat <- 0
    property_type_Boutique_hotel <- 0
    property_type_Condominium <- 0
    property_type_Dorm <- 0
    property_type_Guest_suite <- 0
    property_type_House <- 0
    property_type_In_law <- 0
    property_type_Loft <- 0
    property_type_Other <- 0
    property_type_Timeshare <- 0
    
    room_type_Private_room <- 0
    room_type_Shared_room <- 0
    
    airconditioning <- 0
    essentials <- 0
    familykidfriendly <- 0
    hairdryer <- 0
    hangers <- 0
    heating <- 0
    laptopfriendlyworkspace <- 0
    shampoo <- 0
    smokedetector <- 0
    washer <- 0
    
    beds<-as.numeric(input$selectBeds)
    bathRooms<-as.numeric(input$selectBathrooms)
    city<-input$city
    
    selectPropertyType<-input$selectPropertyType
    if(selectPropertyType == 'Boat'){
      property_type_Boat <- 1
    }else if(selectPropertyType == 'Boutique Hotel'){
      property_type_Boutique_hotel <- 1
    }else if(selectPropertyType == 'Condominium'){
      property_type_Condominium <- 1
    }else if(selectPropertyType == 'Dorm'){
      property_type_Dorm <- 1
    }else if(selectPropertyType == 'Guest Suite'){
      property_type_Guest_suite <- 1
    }else if(selectPropertyType == 'House'){
      property_type_House <- 1
    }else if(selectPropertyType == 'In Law'){
      property_type_In_law <- 1
    }else if(selectPropertyType == 'Loft'){
      property_type_Loft <- 1
    }else if(selectPropertyType == 'Other'){
      property_type_Other <- 1
    }else if(selectPropertyType == 'Timeshare'){
      property_type_Timeshare <- 1
    }
    roomType <- input$roomType
    if(roomType == 1){
      room_type_Private_room <- 1
    }else{
      room_type_Shared_room <- 1
    }
    noReviews <- input$noReviews
    scoreRating <- input$scoreRating
    
    amenities <- paste(input$amenities, collapse = ", ")
    
    for(amnts in amenities) {
      if(amnts == "ac"){
        airconditioning = 1
      }else if(amnts == "essn"){
        essentials = 1
      }else if(amnts == "ff"){
        familykidfriendly = 1
      }else if(amnts == "hd"){
        hairdryer = 1
      }else if(amnts == "han"){
        hangers = 1
      }else if(amnts == "ht"){
        heating = 1
      }else if(amnts == "lfw"){
        laptopfriendlyworkspace = 1
      }else if(amnts == "shm"){
        shampoo = 1
      }else if(amnts == "sd"){
        smokedetector = 1
      }else if(amnts == "wsh"){
        washer = 1
      }
      
    }
    
    # observe({
    # updateCheckboxGroupInput(session,"amenities","Amenities:",choices=amenities,selected=amenities)
    # })
    
    citydf <- data.frame("city" = c("NYC", "SF", "LA", "Chicago", "Boston"),
                         "predictedPrice" = c(0,0,0,0,0))
    
    #citydf <- data.frame(c("NYC", "SF", "LA", "Chicago", "Boston")) 
    
    validate <-data.frame('property_type.Boat' = property_type_Boat, 'property_type.Boutique.hotel' = property_type_Boutique_hotel, 'property_type.Condominium' = property_type_Condominium,
                          'property_type.Dorm' = property_type_Dorm, 'property_type.Guest.suite' = property_type_Guest_suite, 'property_type.House'=property_type_House,
                          'property_type.In.law'=property_type_In_law, 'property_type.Loft'= property_type_Loft, 'property_type.Other'= property_type_Other,
                          'property_type.Timeshare' = property_type_Timeshare, 
                          'room_type.Private.room' = room_type_Private_room, 'room_type.Shared.room'= room_type_Shared_room,
                          "number_of_reviews" = noReviews, "bathrooms" = bathRooms, "review_scores_rating" = scoreRating,
                          "beds" = beds, "city" = city,
                          "Essentials" = essentials, "Family_kid_friendly" = familykidfriendly, "Hair_dryer" = hairdryer, "Hangers" = hangers, "Heating" = heating, 
                          "Laptop_friendly_workspace" = laptopfriendlyworkspace, "Shampoo" = shampoo, "Smoke_detector" = smokedetector, "Washer" = washer, "Air_conditioning" = airconditioning)
    
    
    for (i in 1:nrow(citydf)) {
    validate1 <-data.frame('property_type.Boat' = property_type_Boat, 'property_type.Boutique.hotel' = property_type_Boutique_hotel, 'property_type.Condominium' = property_type_Condominium,
                          'property_type.Dorm' = property_type_Dorm, 'property_type.Guest.suite' = property_type_Guest_suite, 'property_type.House'=property_type_House,
                          'property_type.In.law'=property_type_In_law, 'property_type.Loft'= property_type_Loft, 'property_type.Other'= property_type_Other,
                          'property_type.Timeshare' = property_type_Timeshare, 
                          'room_type.Private.room' = room_type_Private_room, 'room_type.Shared.room'= room_type_Shared_room,
                          "number_of_reviews" = noReviews, "bathrooms" = bathRooms, "review_scores_rating" = scoreRating,
                          "beds" = beds, "city" = citydf$city[i],
                          "Essentials" = essentials, "Family_kid_friendly" = familykidfriendly, "Hair_dryer" = hairdryer, "Hangers" = hangers, "Heating" = heating, 
                          "Laptop_friendly_workspace" = laptopfriendlyworkspace, "Shampoo" = shampoo, "Smoke_detector" = smokedetector, "Washer" = washer, "Air_conditioning" = airconditioning)
   
     citydf$predictedPrice[i]<-predict(fit_lm4, newdata = validate1)
    
    }
    
    df1 <- citydf %>% group_by(city)
    fig <- df1 %>% plot_ly(labels = ~city, values = ~predictedPrice, textinfo='text+label', text=round(citydf$predictedPrice,2), colors = ~citydf$city, hoverinfo = "label+value" )
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(showlegend = F,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    output$allCityPredict <- renderPlotly({fig})
    
    pred<-predict(fit_lm4, newdata = validate, interval = "confidence")
    #dd <- data.frame("Predicted"= pred[1], "Actual"=validate$price[1], "Residul"= res)
    dd<-data.frame(whichPred = c('Lower Estimate', 'Predicted Price', 'Upper Estimate'), predVals = c(pred[2], pred[1], pred[3]))
    return(list(pr = dd))
  })
  
  
  output$predVal<-renderPrint({cat(dataHandled()$pr[2,2])})
  output$lwrPredVal<-renderPrint({cat(dataHandled()$pr[1,2])})
  output$uprPredVal<-renderPrint({cat(dataHandled()$pr[3,2])})
  
  output$barPlot<-renderPlot({
    p4<-ggplot(data = mymap, aes(x = beds, y = price)) + geom_smooth(method = 'lm');
    p5<-ggplot(data = mymap, aes(x = bathrooms, y = price)) + geom_smooth(method = 'lm');
    p6<-ggplot(data = mymap, aes(x = number_of_reviews, y = price)) + geom_smooth(method = 'lm');
    p7<-ggplot(data = mymap, aes(x = review_scores_rating, y = price)) + geom_smooth(method = 'lm');
    grid.arrange(p4, p5, p6, p7, nrow = 2, ncol = 4)
  })
  
  ###########################################
  output$citymap <-  renderPlot({
    if(input$selectVisualizationType == 'Map'){
      drawmap()  
    } else if(input$selectVisualizationType == 'Amentity') {
      drawamenity()
    } else if(input$selectVisualizationType == 'PropertyType'){
      drawProperty()
    }else if(input$selectVisualizationType == 'RoomType'){
      drawRoomType()
    }
  })
  
  output$top10 <- renderPlotly({
    
      amentinities <- c('Smoke detector','Hair dryer', 'Heating','Laptop friendly workspace' , 'Shampoo' ,
                        'Washer/Dryer' , 'WirelessInternet','Airconditioning','Essentials','Familykidfriendly',
                        'Hangers','Kitchen','Carbonmonoxide.detector','Iron',"Smoke detector")
      Top10numbers <- c(61727,43330,67073,43703,49465,43169,71265,0,0,0,0,0,47190,41687,61727)
      AIC_Recommened <- c(61727,43330,67073,43703,49465,43169,71265,55210,64005,37026,49173,67526,0,0,61727)
      radarplot <- data.frame(amentinities,Top10numbers,AIC_Recommened)
      
      # dfa2
      fig <- plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
      ) 
      fig <- fig %>%
        add_trace(
          r = Top10numbers,
          theta = amentinities,
          name = 'Top10 amenities '
        ) 
      fig <- fig %>%
        add_trace(
          r = AIC_Recommened,
          theta = amentinities,
          name = 'Selected amenities'
        )
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,72000)
            )
          )
        )
  })
  
  
  
  drawmap <- function() { 
    if (input$selectCity == 'DC') {
      cityrange <- DCrange
      citydata <- dc
    } else if (input$selectCity == 'SF') {
      cityrange <- SFrange
      citydata <- sf
    }else if(input$selectCity == 'Boston') {
      cityrange <- Bostonrange
      citydata <- bos 
    } else if(input$selectCity == 'LA') {
      cityrange <- Larange
      citydata <- la 
    }else if(input$selectCity == 'NYC') {
      cityrange <- NYCrange
      citydata <- nyc 
    }else if (input$selectCity == 'Chicago') {
      cityrange <- Chicagorange
      citydata <- chi
    }
    
    if (input$selectCity == 'AllCity'){
      us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
      map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
      ggmap(map)+geom_point(data=mymap1, 
                            aes(x=longitude, y=latitude, colour=property_type), size=5, alpha=I(0.7)) + labs(color = "Property Types")
      
    } else{
      city_stamenmap <- get_stamenmap(cityrange, zoom = 11, maptype = "toner-lite")
      city <- city_stamenmap
      ggmap(city) +
        geom_point(data = citydata,
                   aes(x = longitude,
                       y = latitude, colour=property_type),
                   size = 0.5,
                   alpha = I(0.7)) + labs(color = "Property Types")
    }
  } 
  drawamenity <- function(){
    NYC <- c(19819, 20072, 9725, 13177, 21516,15007,13007,14256,18788,8391)
    SF <- c(428,3963,2083,2992,4063,3103,2846,3343,3866,2853)
    LA <- c(10464,13065,8251,9461,13210,10552,9353,11388,12841,10872)
    DC <- c(3490,3275,2185,2230,3519,2581,2297,2745,3234,2933)
    Boston <- c(1971,2289,1254,1695,2410,1942,1727,1797,2285,1666)
    Chicago <- c(2537,2588,1642,1947,2763,2096,1916,2333,2599,2131)
    
    amentinities1 <- c('Airconditioning','Essentials','Familykid friendly','Hair dryer', 'Heating','Hangers',
                       'Laptop friendly workspace' , 'Shampoo' ,"Smoke detector",'Washer/Dryer')
    bar_plot <- data.frame(amentinities1,NYC,SF,LA,DC,Boston,Chicago)
    
    if (input$selectCity == 'DC') {
      city1 <- DC
    } else if (input$selectCity == 'SF') {
      city1 <-SF
    }else if(input$selectCity == 'Boston') {
      city1 <- Boston
    } else if(input$selectCity == 'LA') {
      city1 <- LA
    }else if(input$selectCity == 'NYC') {
      city1 <- NYC
    }else if (input$selectCity == 'Chicago') {
      city1 <- Chicago
    }
    
    if (input$selectCity =='AllCity')  {
      ggplot(data = bar_plot %>% gather(Variable, Numbers, -amentinities1), 
             aes(x = amentinities1, y = Numbers, fill = Variable)) + 
        geom_bar(stat = 'identity', position = 'dodge')+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        scale_fill_viridis_d()+coord_polar() + xlab("Amenities") + ylab("Count")
    }else{
      ggplot(data=bar_plot, aes(x=amentinities1, y=city1)) +
        geom_bar(stat="identity", fill="steelblue")+
        geom_text(aes(label=city1), vjust=1.6, color="white", size=3.5)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_viridis_d()+coord_polar() + xlab("Amenities") + ylab("Count")
    }
  }  
  drawRoomType <- function(){
    
    
    
    room_type <- c("Shared ","Private ")
    NYCRoom <-c(522,11101) 
    SFRoom <- c(49,1827)
    LARoomc <- c(476,5778)
    DCRoom <- c(70,1179)
    BostonRoom <- c(33,1001)
    ChicagoRoom<- c(76,1183)
    AllcityRoom <- c(2163,30638)
    donut1<- data.frame(category=room_type,count=NYCRoom)
    donut2<- data.frame(category=room_type,count=SFRoom)
    donut3<- data.frame(category=room_type,count=LARoomc)
    donut4<- data.frame(category=room_type,count=DCRoom)
    donut5<- data.frame(category=room_type,count=BostonRoom)
    donut6<- data.frame(category=room_type,count=ChicagoRoom)
    donut7<- data.frame(category=room_type,count=AllcityRoom)
    
    if (input$selectCity == 'DC') {
      data <- donut4
    } else if (input$selectCity == 'SF') {
      data <- donut2
    }else if(input$selectCity == 'Boston') {
      data <- donut5
    } else if(input$selectCity == 'LA') {
      data <- donut3
    }else if(input$selectCity == 'NYC') {
      data <- donut1
    }else if (input$selectCity == 'Chicago') {
      data <- donut6
    }else{
      data <- donut7
    }   
    
    # Compute percentages
    data$fraction <- data$count / sum(data$count)
    data$ymax <- cumsum(data$fraction)
    data$ymin <- c(0, head(data$ymax, n=-1))
    data$labelPosition <- (data$ymax + data$ymin) / 2
    data$label <- paste0(data$category, "\n value: ", data$count)
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
      scale_fill_brewer(palette=5) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
    
    
    
  }
  drawProperty <- function(){
    property_type <- c('Boat', 'InLaw', 'Boutique.hotel' ,  'Dorm' , 'Timeshare','Guest.suite' , 'Other','Condominium'  , 'Loft', 'House') 
    NYCProperty <-c(2,3,7,13,15,30,129,284,432,1835)
    SFProperty <- c(4,45,22,8,13,35,40,332,66,1252)
    LAProperty <- c(17,1,3,24,0,12,145,424,290,5582)
    DCProperty <- c(1,5,8,3,0,4,18,185,20,1039)
    BostonProperty <- c(8,4,1,3,1,7,24,250,20,401)
    ChicagoProperty <- c(5,1,2,5,0,4,13,307,57,392)
    Numbers_All_City = c(65,71,69,142,77,123,607,2658,1244,16511)
    property_bar <- data.frame(property_type,NYCProperty,SFProperty,LAProperty,DCProperty,BostonProperty,ChicagoProperty,Numbers_All_City)
    
    
    if (input$selectCity == 'DC') {
      cityProperty <- DCProperty
    } else if (input$selectCity == 'SF') {
      cityProperty <-SFProperty
    }else if(input$selectCity == 'Boston') {
      cityProperty <- BostonProperty
    } else if(input$selectCity == 'LA') {
      cityProperty <- LAProperty
    }else if(input$selectCity == 'NYC') {
      cityProperty <- NYCProperty
    }else if (input$selectCity == 'Chicago') {
      cityProperty <- ChicagoProperty
    } else {
      cityProperty <- Numbers_All_City
    }
    
    ggplot(data=property_bar, aes(x=property_type, y=cityProperty)) +
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=cityProperty), vjust=1.6, color="white", size=3.5)+
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Property Types") + ylab("Count") + labs(color = "Property Types")
  }
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
