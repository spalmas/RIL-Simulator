######################### loading packages
library(ggplot2)
library(mgcv)
library(shiny)
library(truncnorm)

#########################Importing source files
file.sources = list.files(path = 'R/', pattern="*.R")
sapply(paste0('R/',file.sources), source, .GlobalEnv)
#source('R/simulator.R')

ui <- fluidPage(
  titlePanel("RIL Simulator"),
  #This is a RIL Simulator with parameters from the Yucatan Peninsula
  #############input for Stand Variables
  fluidRow(
    column(9, tags$h3("Scenario inputs"))
  ),
  fluidRow(
    column(3,
           textInput('scenario', "Scenario", value = 'A'),
           sliderInput("rotation", "Rotation Years", min = 1, max = 50, value = 0, step = 1),
           sliderInput("w.dist", "Winching distance", min = 0, max = 30, value = 0, step = 1)
    ),
    column(3,
           selectInput("intensity", "Intensity of logging", choices = c('No Logging', 'Normal', 'High'), selected = c('Normal')),
           checkboxInput("dir.felling", label = 'Directional felling', value = TRUE),
           checkboxInput("improved.trail", label = 'Improved Skid Trail planning', value = TRUE),
           checkboxInput("lower.impact", label = 'Lower-impact skidding', value = TRUE),
           checkboxInput("enrich.bosquete", "Enrichment of bosquetes", value = TRUE)
    ),
    column(3,
           sliderInput(inputId = 'sy', label = "Simulation Years (1-75)", min = 5, max = 80, value = 25, step = 1),
           sliderInput(inputId = 'it', label = "Iterations (1-500)", min = 5, max = 500, value = 5, step = 10),
           fileInput('trees.tab', label = 'Tree Inventory')
    )
  ),
  
  actionButton(inputId = 'run', label = 'Run!', 
               icon = icon('line-chart', class = NULL, lib = "font-awesome")),
  
  plotOutput(outputId = "agb.plot"),
  
  #Button to download results data.
  downloadButton(outputId = 'downloadData', label =  'Download')
)

server <- function(input, output) {
  #trees tab information
  trees.tab <- eventReactive(eventExpr = input$run,{
    infile <- input$trees.tab
    if (!is.null(infile)) {  # User has not uploaded a file yet
      read.csv(infile$datapath)
    }
    stand.randomizer()
  })
  
  
  #---------------SIMULATOR----------------
  table.results  <- eventReactive(input$run,{
    simulator(scenario = input$scenario,
              it = input$it,
              sy = input$sy,
              rotation = input$rotation,
              intensity = input$intensity,
              enrich.bosquete = input$enrich.bosquete,
              w.dist = input$w.dist,
              dir.felling = input$dir.felling,
              improved.trail = input$improved.trail,
              lower.impact = input$lower.impact,
              trees.tab = trees.tab())
  })
  
  #---------------OUTPUT FUNCIONS----------------
  #eventReactive() returns NULL until the action button is
  #clicked. As a result, the graph does not appear until 
  #the user asks for it by clicking “Go”.
  output$agb.plot <- renderPlot({
    g <- ggplot(rbind(table.results()), aes(x = YEAR, y = AGB/1000))
    g + stat_summary(fun.data = 'mean_sdl', geom = 'ribbon', mult = 1, alpha =0.3) + #adding standard deviation shading
      stat_summary(fun.y = 'mean', geom = 'line', size = 1) + #adding mean line
      ggplot_params() + #General graph Parameters. Found in Helpers.R
      scale_colour_manual(values=line.colors, name = '') + #assigning colors to lines
      scale_fill_manual(values=line.colors, name = '')  #assigning colors to ribbons
  })
  
  #---------------DOWNLOAD TABLE OF RESULTS----------------
  #Downloading works only outside Rstudio. It needs to run externally to work
  output$downloadData <- downloadHandler(
    # This function returns a string which tells the client browser what name to use when saving the file.
    filename = function() {
      paste(input$scenario,'.csv', sep = '')
    },
    
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.csv(x = table.results(), file = file,
                  row.names = FALSE)
    }
  )
  #output$BA <- renderText({standcalc(trees.tab())})
    
  #output$filetable <- renderTable({
  #  #trees.tab()
  #  table.results()
  #})

}

shinyApp(ui = ui, server = server)
