#---------------IMPORTING PACKAGES----------------
library(ggplot2)   #For plotting
library(grid)      #For making multiplots
library(gridExtra) #For making multiplots 
library(mgcv)
library(shiny)     #For app 
library(truncnorm) #Truncated normal distribution package for ,,,

#---------------IMPORTING SOURCE FILES----------------

file.sources = list.files(path = 'R/', pattern="*.R")
sapply(paste0('R/',file.sources), source, .GlobalEnv)

regen.params <- read.csv('data/regenation_params.csv', sep = ',')

#---------------UI----------------
ui <- fluidPage(
  titlePanel("RIL Simulator"),
  #This is a RIL Simulator with parameters from the Yucatan Peninsula
  #############input for Stand Variables
  fluidRow(
    column(9, tags$h3("Scenario inputs"))
  ),
  fluidRow(
    column(3,
           textInput('scenario', "Scenario name", value = 'RIL'),
           sliderInput("rotation", "Rotation Years", min = 1, max = 50, value = 15, step = 1),
           sliderInput("w.dist", "Winching distance", min = 0, max = 30, value = 10, step = 1)
    ),
    column(3,
           selectInput("intensity", "Intensity of logging", choices = c('No Logging', 'Normal', 'High'), selected = c('Normal')),
           checkboxInput("dir.felling", label = 'Directional felling', value = TRUE),
           #checkboxInput("improved.trail", label = 'Improved Skid Trail planning', value = TRUE),
           #checkboxInput("lower.impact", label = 'Lower-impact skidding', value = TRUE),
           checkboxInput("enrich.bosquete", "Enrichment of bosquetes", value = TRUE)
    ),
    column(3,
           sliderInput(inputId = 'sy', label = "Simulation Years", min = 5, max = 50, value = 35, step = 1),
           sliderInput(inputId = 'it', label = "Iterations", min = 5, max = 500, value = 5, step = 10),
           fileInput('trees.tab', label = 'Tree Inventory')
    )
  ),
  
  actionButton(inputId = 'run', label = 'Run!', 
               icon = icon('line-chart', class = NULL, lib = "font-awesome")),
  
  #Syummary Table of results
  tableOutput(outputId = 'table.summary'),
  
  #Button to download results data. Should only work if soimulation is done. Downloads only scenario table.
  downloadButton(outputId = 'downloadData', label =  'Download'),
  
  #Plots
  plotOutput(outputId = "plots")
  
  
)

#---------------SERVER----------------
server <- function(input, output) {
  #trees tab information
  trees.tab <- eventReactive(eventExpr = input$run,{
    infile <- input$trees.tab
    if (!is.null(infile)) {  # User has not uploaded a file yet
      read.csv(infile$datapath)
    }
    stand.randomizer()
  })
  
  
  #---------------SIMULATOR SCENARIO----------------
  SCENARIO.results  <- eventReactive(input$run,{
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
  
  #---------------SIMULATOR BAU----------------
  BAU.results  <- eventReactive(input$run,{
    simulator(scenario = 'BAU',
              it = input$it,
              sy = input$sy,
              rotation = input$rotation,
              intensity = 'High',
              enrich.bosquete = FALSE,
              w.dist = 0,
              dir.felling = FALSE,
              improved.trail = FALSE,
              lower.impact = FALSE,
              trees.tab = trees.tab())
  })
  

  #---------------OUTPUT PLOTS----------------
  #eventReactive() returns NULL until the action button is
  #clicked. As a result, the graph does not appear until 
  #the user asks for it by clicking “Go”.
  output$plots <- renderPlot({
    
    results <- rbind(SCENARIO.results(), BAU.results())
    
    AGB.plot <- ggplot(results, aes(x = YEAR, y = AGB, colour = SCENARIO)) + 
      #stat_summary(fun.data = 'mean_sdl', geom = 'ribbon', mult = 1, alpha =0.3) + #adding standard deviation shading
      #stat_summary(fun.y = 'mean', geom = 'line', size = 1) + #adding mean line
      geom_smooth(span=0.2, aes(fill=SCENARIO)) +
      ggplot_params() + #General graph Parameters. Found in Helpers.R
      scale_colour_manual(values=line.colors, name = '') + #assigning colors to lines
      scale_fill_manual(values=line.colors, name = '') +  #assigning colors to ribbons
      xlab(paste ("Year")) +  # X Label
      ylab(paste ("MgC")) +  #Y Label
      labs(title = 'AGB')
    
    BA.plot <- ggplot(results, aes(x = YEAR, y = BA, colour = SCENARIO)) +
      #stat_summary(fun.data = 'mean_sdl', geom = 'ribbon', mult = 1, alpha =0.3) + #adding standard deviation shading
      #stat_summary(fun.y = 'mean', geom = 'line', size = 1) + #adding mean line
      geom_smooth(span=0.2, aes(fill=SCENARIO)) +
      ggplot_params() + #General graph Parameters. Found in Helpers.R
      scale_colour_manual(values=line.colors, name = '') + #assigning colors to lines
      scale_fill_manual(values=line.colors, name = '')  + #assigning colors to ribbons
      xlab(paste ("Year")) +  # X Label
      ylab(paste ("m2")) +  #Y Label
      labs(title = 'Basal Area')
    
    NET.SEQUESTERED.plot <- ggplot(results, aes(x = YEAR, y = NET.SEQUESTERED, colour = SCENARIO)) +
      geom_smooth(span=0.2, aes(fill=SCENARIO)) +
      ggplot_params() + #General graph Parameters. Found in Helpers.R
      scale_colour_manual(values=line.colors, name = '') + #assigning colors to lines
      scale_fill_manual(values=line.colors, name = '')  + #assigning colors to ribbons
      xlab(paste ("Year")) +  # X Label
      ylab(paste ("MgC")) +  #Y Label
      labs(title = 'Sequestered Carbon')
    
    INCOME.plot <- ggplot(results[!is.na(results$INCOME),], aes(x = factor(YEAR), y = INCOME, colour = SCENARIO)) +
      geom_boxplot() +
      #stat_ecdf() +
      ggplot_params() + #General graph Parameters. Found in Helpers.R
      scale_colour_manual(values=line.colors, name = '') + #assigning colors to lines
      scale_fill_manual(values=line.colors, name = '')  + #assigning colors to ribbons
      xlab(paste ("Year")) +  # X Label
      ylab(paste ("Thousands MXN")) +  #Y Label
      labs(title = "Income")

    multiplot(BA.plot, AGB.plot, NET.SEQUESTERED.plot, INCOME.plot, cols=2)
    
  }, width = 1000, height = 800)
  
  #Summary of results 

  output$table.summary <- renderTable({
    table.summary <- matrix(data = c( mean.sd(results.column = 'N.HARVESTED', data = SCENARIO.results()),
                                      mean.sd(results.column = 'VOL.HARVESTED', data = SCENARIO.results()),                                   
                                      mean.sd(results.column = 'EMISSIONS', data = SCENARIO.results()),                                     
                                      mean.sd(results.column = 'EMISSIONSperm3', data = SCENARIO.results()),                                     
                                      mean.sd(results.column = 'INCOME', data = SCENARIO.results()),                                    
                                      
                                      mean.sd(results.column = 'N.HARVESTED', data = BAU.results()),
                                      mean.sd(results.column = 'VOL.HARVESTED', data = BAU.results()),
                                      mean.sd(results.column = 'EMISSIONS', data = BAU.results()),
                                      mean.sd(results.column = 'EMISSIONSperm3', data = BAU.results()),
                                      mean.sd(results.column = 'INCOME', data = BAU.results())
                                      ),
    ncol = 2)
    table.summary <- as.data.frame(table.summary)
    rownames(table.summary) <- c('Trees harvested / ha (All years)',
                                 'm3 Harvested / ha (All years)',
                                 'MgC Emissions from harvest (All years)',
                                 'MgC Emissions per m3 harvested (All years)',
                                 'Income (1000 MXN)')
    colnames(table.summary) <- c('Scenario', 'BAU')
    
    #Emissions per m3 harvested
    table.summary
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

}

#---------------SHINY APP DEFINITION----------------

shinyApp(ui = ui, server = server)
