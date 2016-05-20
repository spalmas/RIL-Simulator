######################### loading packages
library(ggplot2)
library(shiny)
library(truncnorm)

#########################Importing source files
file.sources = list.files(path = 'R/', pattern="*.R")
sapply(paste0('R/',file.sources), source, .GlobalEnv)
#source('R/simulator.R')

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
             sliderInput(paste0("rotation",'.', prefix), "Rotation Years", min = 1, max = 50, value = 0, step = 1),
             sliderInput(paste0("bos",'.', prefix), "Bosquetes/ha", min = 0, max = 5, value = 0, step = 1),
             sliderInput(paste0("w.dist",'.', prefix), "Winching distance", min = 0, max = 30, value = 0, step = 1)
      ),
      column(6,
             selectInput(paste0("intensity", '.', prefix), "Intensity of logging", choices = c('Normal', 'High'), selected = c('Normal')),
             checkboxInput(paste0("dir.felling", '.', prefix), label = 'Directional felling', value = TRUE),
             checkboxInput(paste0("improved.trail", '.', prefix), label = 'Improved Skid Trail planning', value = TRUE),
             checkboxInput(paste0("lower.impact", '.', prefix), label = 'Lower-impact skidding', value = TRUE)
      )
    )
  )
}

ui <- fluidPage(
  titlePanel("RIL Simulator"),
  #This is a RIL Simulator with parameters from the Yucatan Peninsula
  #############input for Stand Variables
  fluidRow(
    column(6, tags$h3("Scenario inputs"))
  ),
  fluidRow(
    column(6, renderInputs("a"))
  ),

  #Simulator Year
  fluidRow(
    column(3, 
           sliderInput(inputId = 'sy', label = "Simulation Years (1-75)",
                       min = 5, max = 80, value = 25, step = 1)),
    column(3,
           sliderInput(inputId = 'it', label = "Iterations (1-1000)",
                        min = 5, max = 1000, value = 5, step = 10))
    ),
  
  #TREES and SPECIES table input
  fluidRow(
    column(4, fileInput('trees.tab', label = 'Tree Inventory')),
    column(4, fileInput('diameter.eqs', label = 'Diameter Growth')),
    column(4, fileInput('volume.eqs', label = 'Volume Growth'))
  ),
  
  actionButton(inputId = 'run', label = 'Run!', 
               icon = icon('line-chart', class = NULL, lib = "font-awesome")),
  
  plotOutput("agb.plot")
  #plotOutput("agb.plot2"),
  #textOutput("BA"),
  #tableOutput("filetable")
)

server <- function(input, output) {

  #---------------READING FILES----------------
  #If the RUN! button is pushed, this code will make a trees()
  trees.tab <- eventReactive(input$run,{
    infile <- input$trees.tab
    if (!is.null(infile)) {  # User has not uploaded a file yet
      read.csv(infile$datapath)
      }
    stand.randomizer()
    })
  
  #Diameter growth parameters
  diameter.eqs <- eventReactive(input$run,{
    infile <- input$diameter.eqs
    if (!is.null(infile)) {    # User has not uploaded a file yet
      read.csv(infile$datapath)
      #return(diameter_growth_default)
    }  
    read.csv(file = "Data/diameter_growth.csv")
    })

  #Volume ~ Diameter Equation
  volume.eqs <- eventReactive(input$run,{
    infile <- input$volume.eqs
    if (!is.null(infile)) {   # if user has not uploaded a file
      read.csv(infile$datapath)
      #return(volume_equations_default)
      } 
    read.csv(file = "Data/volume_equations.csv")
  })
  
  
  #---------------SIMULATOR----------------
  table.results.a  <- eventReactive(input$run,{
    simulator(scenario = 'A' , sy = input$sy, it = input$it,
                   rotation = input$rotation.a,
                   intensity = input$intensity.a,
                   bos = input$bos.a,
                   w.dist = input$w.dist.a,
                   dir.felling = input$dir.felling.a,
                   improved.trail = input$improved.trail.a,
                   lower.impact = input$lower.impact.a,
                   trees.tab = trees.tab(),
                   diameter.eqs = diameter.eqs(),
                   volume.eqs = volume.eqs())
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
