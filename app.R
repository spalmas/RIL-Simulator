###---
# IMPORTING PACKAGES, FUNCTIONS AND DATA ----
###---

source('startup.R')

###---
# APP DEFINITION --------------------------
###---

shinyApp(

  ###---
  #  *USER INTERFACE ----------------
  ###---

  ui <- tagList(
    navbarPage(
      theme = "darkly",  # <--- To use a theme, uncomment this
      "Reduced-Impact-Logging Simulator",

      tabPanel("Simulator",
               ###---
               # ** Main sidebar panel ----
               ###---
               sidebarPanel(width = 4,
                            h3("Input parameters"),
                            fileInput("treelist", "Tree list:"),
                            selectInput("intensity", "Intensity of logging", choices = c('No Logging', 'Low','BAU', 'High', 'Highest'), selected = c('BAU')),
                            numericInput(inputId="rotation", label="Rotation Years", value = 25, min = 1, max = 50),
                            sliderInput("w.dist", "Winching distance", min = 0, max = 25, value = 10, step = 1),
                            checkboxInput("enrich.bosquete", "Enrichment of bosquetes", value = TRUE),
                            checkboxInput("dir.felling", label = 'Directional felling', value = TRUE),
                            numericInput(inputId="it", label="Iterations", value = 10, min = 1, max = 100),
                            textInput(inputId = 'scname', label = 'Scenario name', placeholder = 'Scenario name'),
                            sliderInput(inputId = 'sy', label = "Simulation Years", min = 5, max = 50, value = 35, step = 1),
                            actionButton(inputId = "run",label =  "Run!", class = "btn-primary")
               ),
               
               ###---
               # ** Results panel ----
               ###---
               mainPanel(width = 7,
                         tabsetPanel(
                           ###---
                           # *** Main results tab ----
                           ###---
                           tabPanel(title = "Results",
                                    h4("Plots"),
                                    #Summary Table of results
                                    tableOutput(outputId = 'table.summary'),
                                    #Button to download results data. Should only work if soimulation is done. Downloads only scenario table.
                                    downloadButton(outputId = 'downloadData', label =  'Download'),
                                    #Plots
                                    plotOutput(outputId = "plots")
                                    
                           ),
                           ###---
                           # *** Rauli tab ----
                           ###---
                           tabPanel(title = "Other Results")
                         )
               )
      ),

      ###---
      #** ADVANVCED RESULTS TAB: ONLY INPUT----
      ###---
      tabPanel("Advanced",
               sidebarPanel(verbatimTextOutput("txtout"),
                            h3("Advanced Parameters"),
                            h5("This tab allows for more control of simulation parameters.
                               For more information on each parameter see Manual")
               ),
               mainPanel(numericInput(inputId="lambda", label="lambda parameter", value = 70, min = 1, max = 200),
                         checkboxInput(inputId = 'comp', label = 'Make compatibility', value = TRUE)
               )
      ),

      ###---
      # **Contact Tab----
      ###---
      tabPanel("Contact",
               sidebarPanel(verbatimTextOutput("txtout"),
                            h3("Contact"),
                            h5("Sebastian Palmas, Jack Putz"),
                            h3("Github repository"),
                            h5( a("github.com/spalmas/RIL-Simulator", href="https://github.com/spalmas/RIL-Simulator"))
               )
      )
    )
  ),

  ###---
  # SERVER FUNCTIONS ----
  ###---

  server <- function(input, output) {

    trees.tab <- eventReactive(eventExpr = input$run,{
      infile <- input$trees.tab
      if (!is.null(infile)) {  # User has not uploaded a file yet
        read.csv(infile$datapath)
      }
      stand.randomizer()
    })
    
    
    #---------------SIMULATOR SCENARIO----------------
    SCENARIO.results  <- eventReactive(eventExpr = input$run,{
      simulator(scenario = input$scenario,
                it = input$it,
                sy = input$sy,
                rotation = input$rotation,
                intensity = input$intensity,
                enrich.bosquete = input$enrich.bosquete,
                w.dist = input$w.dist,
                dir.felling = input$dir.felling,
                improved.trail = input$improved.trail,
                trees.tab = trees.tab())
    })
    
    #---------------SIMULATOR BAU----------------
    BAU.results  <- eventReactive(eventExpr = input$run,{
      simulator(scenario = 'BAU',
                it = input$it,
                sy = input$sy,
                rotation = input$rotation,
                intensity = 'High',
                enrich.bosquete = FALSE,
                w.dist = 0,
                dir.felling = FALSE,
                improved.trail = FALSE,
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
)
