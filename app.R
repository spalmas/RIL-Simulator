######################### loading packages
library(shiny)
library(truncnorm)

#########################Importing source files
source("helpers.R")
source("simulator.R")

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
             selectInput(paste0("hs", '.', prefix), 'Harvesting Scenario', choices = c('Conventional Logging','Reduced Impact Logging'), selected = 'Conventional Logging'),
             selectInput(paste0("int", '.', prefix), "Intensity of logging", choices = c('Normal', 'High'), selected = c('Normal')),
             sliderInput(paste0("bos",'.', prefix), "Bosquetes/ha", min = 0, max = 5, value = 0, step = 1)
      ),
      column(6,
             selectInput(paste0("areas", '.', prefix), 'Size of Area', choices = c('Conventional Logging','Reduced Impact Logging'), selected = 'Conventional Logging'),
             selectInput(paste0("damages", '.', prefix), "Number of damage", choices = c('Normal', 'High'), selected = c('Normal')),
             sliderInput(paste0("ks",'.', prefix), "Otra cosa", min = 0, max = 5, value = 0, step = 1)
      )
    )
  )
}

ui <- fluidPage(
  titlePanel("RIL Simulator"),
  #This is a RIL Simulator with parameters from the Yucatan Peninsula
  #############input for Stand Variables
  fluidRow(
    column(6, tags$h3("Scenario A")),
    column(6, tags$h3("Scenario B"))
  ),
  fluidRow(
    column(6, renderInputs("a")),
    column(6, renderInputs("b"))
  ),

  #Simulator Year
  fluidRow(
    column(3, 
           sliderInput(inputId = 'sy', label = "Simulation Years (1-75)",
                       min = 1, max = 75, value = 25, step = 1)),
    column(3,
           sliderInput(inputId = 'it', label = "Iterations (1-1000)",
                        min = 1, max = 1000, value = 5, step = 10))
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
      return(stand.randomizer())}  # User has not uploaded a file yet
    read.csv(infile$datapath)
    })
  
  #Diameter growth Equation
  diameter.eqs <- eventReactive(input$run,{
    infile <- input$diameter.eqs
    if (is.null(infile)) {
      return(diameter.default.eqs())}  # User has not uploaded a file yet
    read.csv(infile$datapath)
    })

  #Volume ~ Diameter Equation
  volume.eqs <- eventReactive(input$run,{
    infile <- input$volume.eqs
    if (is.null(infile)) {
      return(volume.default.eqs())}  # User has not uploaded a file yet
    read.csv(infile$datapath)
  })
  
  
  #---------------SIMULATOR----------------
  table.results  <- eventReactive(input$run,{
    YieldSimulator(hs = input$hs.a,
                   bos = input$bos.a,
                   sy = input$sy,
                   it = input$it,
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
