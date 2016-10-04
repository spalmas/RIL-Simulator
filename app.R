######################### loading packages
library(ggplot2)
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
  
  plotOutput("agb.plot")
  #plotOutput("agb.plot2"),
  #textOutput("BA"),
  #tableOutput("filetable")
)

server <- function(input, output) {
  #trees tab information
  trees.tab <- eventReactive(input$run,{
    infile <- input$trees.tab
    if (!is.null(infile)) {  # User has not uploaded a file yet
      read.csv(infile$datapath)
    }
    stand.randomizer()
  })
  
  
  #---------------SIMULATOR----------------
  table.results.a  <- eventReactive(input$run,{
    simulator(scenario = 'A' , sy = input$sy, it = input$it,
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
  output$agb.plot <- renderPlot({
    g <- ggplot(rbind(table.results.a()), aes(x = YEAR, y = AGB))
    g + stat_summary(aes(fill = SCENARIO), fun.data = 'mean_sdl', geom = 'ribbon', mult = 1, alpha =0.3) + #adding standard deviation shading
      stat_summary(aes(colour = SCENARIO), fun.y = 'mean', geom = 'line', size = 1) + #adding mean line
      ggplot_params() + #General graph Parameters. Found in Helpers.R
      scale_colour_manual(values=line.colors, name = '') + #assigning colors to lines
      scale_fill_manual(values=line.colors, name = '')  #assigning colors to ribbons
  })

  #output$BA <- renderText({standcalc(trees.tab())})
    
  #output$filetable <- renderTable({
  #  #trees.tab()
  #  table.results()
  #})

}

shinyApp(ui = ui, server = server)
