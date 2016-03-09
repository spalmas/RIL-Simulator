library(shiny)

#########################Importing source files
source("helpers.R")
source("simulator.R")

ui <- fluidPage(
  titlePanel("RIL Simulator"),
  
  #############input for Stand Variables
  ###harvesting scenario 1
  fluidRow(
    #Growth Zone
    column(3,  
           selectInput(inputId = 'hs1', label = 'Harvesting Scenario 1',
                       choices = c('Conventional Logging','Reduced Impact Logging'),
                       selected = 'Conventional Logging')),
    column(3, 
           selectInput(inputId = 'int1', label = "Intensity of logging",
                        choices = c('Normal', 'High'),
                        selected = c('Normal'))),
    column(3, 
           numericInput(inputId = 'bos1', label = "Bosquetes/ha",
                        min = 0, max = 5, value = 0))
  ),
  #Harvesting scenario 2
  fluidRow(
    #Growth Zone
    column(3,  
           selectInput(inputId = 'hs2', label = 'Harvesting Scenario 2',
                       choices = c('Conventional Logging','Reduced Impact Logging'),
                       selected = 'Reduced Impact Logging')),
    column(3, 
           selectInput(inputId = 'int2', label = "Intensity of logging",
                       choices = c('Normal', 'High'),
                       selected = c('Normal'))),
    column(3, 
           numericInput(inputId = 'bos2', label = "Bosquetes/ha",
                        min = 0, max = 5, value = 1))
    ),
  
  #Simulator Year
  fluidRow(
    column(3, 
           numericInput(inputId = 'sy', label = "Simulation Years (0-75)",
                       min = 1, max = 75, value = 25)),
    column(3,
           numericInput(inputId = 'rep', label = "Repetitions (0-1000)",
                        min = 1, max = 1000, value = 5))
    ),
  
  #TREES and SPECIES table input
  fluidRow(
    column(4, fileInput('trees.tab', label = 'Tree Inventory')),
    column(4, fileInput('diameter.eqs', label = 'Diameter Growth')),
    column(4, fileInput('volume.eqs', label = 'Volume Growth'))
  ),
  
  actionButton(inputId = 'run', label = 'Run!', 
               icon = icon('line-chart', class = NULL, lib = "font-awesome")),
  
  #plotOutput("hist"),
  plotOutput("agb.plot"),
  #textOutput("BA"),
  tableOutput("filetable")
)

server <- function(input, output) {

  #---------------READING FILES----------------
  #If the RUN! button is pushed, this code will make a trees()
  trees.tab <- eventReactive(input$run,{
    infile <- input$trees.tab
    if (is.null(infile)) {
      return(NULL)}  # User has not uploaded a file yet
    read.csv(infile$datapath)
    })
  
  #Diameter growth Equation
  diameter.eqs <- eventReactive(input$run,{
    infile <- input$diameter.eqs
    if (is.null(infile)) {
      return(NULL)}  # User has not uploaded a file yet
    read.csv(infile$datapath)
    })

  #Volume ~ Diameter Equation
  volume.eqs <- eventReactive(input$run,{
    infile <- input$volume.eqs
    if (is.null(infile)) {
      return(NULL)}  # User has not uploaded a file yet
    read.csv(infile$datapath)
  })
  
  
  #---------------SIMULATOR----------------
  table.results  <- eventReactive(input$run,{
    YieldSimulator(hs = input$hs,
                   sy = input$sy,
                   trees.tab = trees.tab(),
                   diameter.eqs = diameter.eqs(),
                   volume.eqs = volume.eqs())
  })

  
  #---------------OUTPUT FUNCIONS----------------
  
  output$hist <- renderPlot({
    hist(trees.tab()$DBH)
  })
  
  output$agb.plot <- renderPlot({
    plot(table.results()$AGB)
  })

  #output$BA <- renderText({standcalc(trees.tab())})
    
  output$filetable <- renderTable({
    #trees.tab()
    table.results()
  })

}

shinyApp(ui = ui, server = server)
