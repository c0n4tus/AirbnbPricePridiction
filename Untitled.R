# title: "IE6600 - hw5"
# subtitle: "<br/>Due on 3/18/2020 11:30am PT"
# author: "Zhenyuan Lu"


# IE6600 Homework Instructions

# Once all the to-do code is completed, you need to have your homework saved
# in the current .r file.
#
#   **hw5YourFullName.R**
#
# Please make sure the shiny app in the current .r file can be runned successfully
#
# Submission will only accepted on Piazza:
# Create a new post only to **Instructor(s)** and type **Instructors**
# > Select **hw5** tag > Summary/Title should be **hw5-submission-YourFullName**
# > Click insert > Insert file > Post the note.
#
#
# ----
library(DT)
library(shiny)
library(tidyverse)
library(ggplot2)
library(datasets)

############# You should put your created function chartCol() here #############
chartCol <- function(df,colN,nBin) {
  if (!is.numeric(df[[colN]])) {
    ggplot(data=df, mapping = aes_string(colN)) + geom_bar() + theme(axis.text.x = element_text(angle = 60))
  }
  else { 
    ggplot(data=df, mapping = aes_string(colN)) + geom_histogram(bins = nBin)
  }
}
################################################################################


ui <- fluidPage(titlePanel("hw5 - IE6600"),
                sidebarLayout(
                  sidebarPanel(
                    width = 2,
                    selectInput(
                      width = "100%",
                      inputId = "dbList1",
                      label = "Default Dataset List",
                      choices = c(choose = "List of data frame...",
                                  "mpg", "diamonds", "msleep"),
                      selectize = FALSE
                    ),
                    uiOutput("obs1"),
                    actionButton(
                      inputId = "reset",
                      label = "Reset Data",
                      icon = icon("refresh"),
                      width = "100%"
                    ),
                    verbatimTextOutput("aaa")
                  ),
                  mainPanel(fluidPage(fluidRow(
                    column(6,
                           DT::dataTableOutput("dataSet")),
                    column(6,
                           plotOutput(
                             "plotChart",
                             width = "100%",
                             height = "300px"
                           ))
                  )))
                ))



server <- function(input, output) {
  values <- reactiveValues(tbl = NULL,
                           obsList = NULL,
                           plot.df = NULL)
  
  observeEvent(input$dbList1, {
    if (!NA %in% match(input$dbList1, c("mpg", "diamonds", "msleep"))) {
      values$tbl <- as.data.frame(get(input$dbList1))
      values$obsList <- colnames(values$tbl)
      
      output$obs1 <- renderUI({
        selectInput(
          inputId = "observationInput1",
          label = "1st observation",
          choices =  values$obsList
        )
      })
    }
  })
  
  observeEvent(input$observationInput1, {
    values$plot.df <-
      as.data.frame(values$tbl[, input$observationInput1])
    colnames(values$plot.df) <- input$observationInput1
    output$dataSet <- DT::renderDataTable({
      values$tbl
    },
    extensions = c('Scroller', 'FixedColumns'),
    options = list(
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 200,
      scroller = TRUE,
      dom = 'Bfrtip',
      fixedColumns = TRUE
    ))
  })
  
  observe({
    ############## Complete the output settings with chartCol() here ################
    # hint: values$plot.df is the data frame should be processed in chartCol()
    
    dataFrame <- values$plot.df
    colmnName <- input$observationInput1
    output$ui<- renderUI({
      
    })
    
    output$plotChart <- renderPlot({ 
      shiny::validate(need(values$plot.df, ""))
      chartCol(dataFrame,colmnName,input$bins) 
    })
    
  }) 
  
  observeEvent(input$reset, {
    values$tbl <- NULL
    output$obs1 <- NULL
  })
  
  output$aaa <- renderPrint({
    values$obs1
  })
}

################################################################################

shinyApp(ui,server)