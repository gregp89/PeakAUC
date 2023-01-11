library(shiny)
library(ggplot2)
library(MESS)
library(RSEIS)
library(signal)
library(dplyr)
library(plyr)


ui <- fluidPage(
      tabsetPanel(
        tabPanel('Standard',
                 sidebarLayout(
                   sidebarPanel(
                     fileInput(inputId = 'Standards_File', label = 'Upload Standards File'),
                     uiOutput("Standard_x_ui"),
                     uiOutput('Standard_y_ui'),
                     numericInput('Standard_BaselineLength', 'Baseline Length', value = .5, min = 0, step = .5),
                     textInput('Standard_PeakName','Enter Standard Name', value = 'Standard'),
                     numericInput('Standard_PeakAmount','Enter Standard Amount', value = 0),
                     actionButton("Standard_PeakSave", "Save Peak"),
                     actionButton('Standard_DeletePeak','Delete Last Peak'),
                     actionButton('Standard_Calculate','Calculate'),
                     downloadButton("Standard_Download", "Download")),
                 
                 mainPanel(
                            plotOutput(outputId = 'Standard_Plot',brush = brushOpts('Standard_plot_brush', direction = 'x')),
                            tableOutput("Standard_tempdata"),
                            plotOutput('Standard_TempPeak'),
                            tableOutput("Standard_Outfile"),
                            plotOutput(outputId = 'StandardCurve'),
                            tableOutput("Standard_Outfile2")
                 )
                 )
                 ),
          tabPanel('Sample',
                   sidebarLayout(
                     sidebarPanel(
                       fileInput(inputId = 'Sample_File', label = 'Upload Sample File'),
                       fileInput(inputId = 'StandardCurveFile', label = 'Upload Standard Curve'),
                       uiOutput("Sample_x_ui"),
                       uiOutput('Sample_y_ui'),
                       numericInput('Sample_BaselineLength', 'Baseline Length', value = .5, min = 0, step = .5),
                       textInput('Sample_PeakName','Enter Sample Name'),
                       actionButton("Sample_PeakSave", "Save Sample Peak"),
                       actionButton('Sample_DeletePeak','Delete Last Peak'),
                       actionButton('Sample_Calculate','Calculate'),
                       downloadButton("Sample_Download", "Download")),
                     
                   mainPanel(
                   plotOutput(outputId = 'Sample_Plot',brush = brushOpts('Sample_plot_brush', direction = 'x')),
                   tableOutput("Sample_tempdata"),
                   plotOutput('Sample_TempPeak'),
                   tableOutput("Sample_Outfile"),
                   tableOutput("Sample_Outfile2")
                   )
                   )
                   ) 
                 )
      )


server <- function(input, output) {
  
  Standard_RawDataFile <- eventReactive(input$Standards_File, {
    na.omit(read.csv(input$Standards_File$datapath))
  })
  
  output$Standard_x_ui <- renderUI({
    selectInput(inputId = "Standard_x_ui", label = "Select X (Time):", choices= names(Standard_RawDataFile()))
  })
  
  output$Standard_y_ui <- renderUI({
    selectInput(inputId = "Standard_y_ui", label = "Select Y (mVH):", choices= names(Standard_RawDataFile()))
  })
  

  output$Standard_Plot <- renderPlot({
    
    ggplot(Standard_RawDataFile(),
           aes(x =Standard_RawDataFile() [,input$Standard_x_ui], y =Standard_RawDataFile()[,input$Standard_y_ui])) +
      geom_point() +
      geom_line(aes(x =Standard_RawDataFile()[,input$Standard_x_ui],
                    y =signal::sgolayfilt(Standard_RawDataFile()[,input$Standard_y_ui]),
                    colour= 'red'),
                show.legend = FALSE) +
      theme_bw()
    
  })
  
  ##set up reactive data frame for temporary peaks (observed on click)
    Standard_temp.values<- reactiveValues()
    
    observeEvent(input$Standard_plot_brush, {
      Standard_temp.values$xmin <- input$Standard_plot_brush$xmin
      Standard_temp.values$xmax <- input$Standard_plot_brush$xmax
      
    })
    
    output$Standard_TempPeak <- renderPlot({
      Standard_Peaki <- subset(Standard_RawDataFile(),
                     Standard_RawDataFile()[,input$Standard_x_ui] > Standard_temp.values$xmin &
                     Standard_RawDataFile()[,input$Standard_x_ui] < Standard_temp.values$xmax)
      
      ggplot(Standard_Peaki,
             aes(x =Standard_Peaki[,input$Standard_x_ui], y = Standard_Peaki[,input$Standard_y_ui])) +
        geom_point() +
        geom_line(aes(x =Standard_Peaki[,input$Standard_x_ui],
                      y =signal::sgolayfilt(Standard_Peaki[,input$Standard_y_ui]),
                      color = 'red'), show.legend = FALSE) +
        geom_hline(yintercept =  mean(
          Standard_RawDataFile()[which(Standard_RawDataFile()[,input$Standard_x_ui]
                                       == round(Standard_temp.values$xmin,
                                                digits=1)):which(Standard_RawDataFile()[,input$Standard_x_ui]
                                                                 == round(Standard_temp.values$xmin-input$Standard_BaselineLength,
                                                                          digits=1)),
                                 input$Standard_y_ui]),
          col = 'yellow') +
        theme_bw()
      
    })
    
    output$Standard_tempdata <- renderTable({
    
        Standard_Peaki <- subset(Standard_RawDataFile(),
                        Standard_RawDataFile()[,input$Standard_x_ui] > Standard_temp.values$xmin &
                          Standard_RawDataFile()[,input$Standard_x_ui] < Standard_temp.values$xmax)
        
        Standard_tempdataframe <- data.frame(
          PeakStart = Standard_temp.values$xmin,
          PeakEnd = Standard_temp.values$xmax,
          Baseline = mean(
            Standard_RawDataFile()[which(Standard_RawDataFile()[,input$Standard_x_ui] 
                                         == round(Standard_temp.values$xmin,
                                                  digits=1)):which(Standard_RawDataFile()[,input$Standard_x_ui] 
                                                                   == round(Standard_temp.values$xmin-input$Standard_BaselineLength,
                                                                            digits=1)),
                                   input$Standard_y_ui]),
          AUC = MESS::auc(x = Standard_Peaki[,input$Standard_x_ui],
                               y = Standard_Peaki[,input$Standard_y_ui] - mean(
                                 Standard_RawDataFile()[which(Standard_RawDataFile()[,input$Standard_x_ui] 
                                                              == round(Standard_temp.values$xmin,
                                                                       digits=1)):which(Standard_RawDataFile()[,input$Standard_x_ui] 
                                                                                        == round(Standard_temp.values$xmin-input$Standard_BaselineLength,
                                                                                                 digits=1)),
                                                        input$Standard_y_ui]),
                               absolutearea = F))
      
        Standard_tempdataframe
         })
    
  ##set up reactive dataframe for data frame that will save peaks##
  Standard_values <- reactiveValues()
  Standard_values$DT <- data.frame(SampleName = character(),
                          PeakAmount = numeric(),
                          PeakStart = numeric(),
                          PeakEnd = numeric(),
                          Baseline = numeric(),
                          AUC = numeric())
  
  Standard_values2 <- reactiveValues()
  Standard_values2$DT <- data.frame(SampleName = character(),
                                  PeakAmount = numeric(),
                                  AUC = numeric())

  ##add new row to reactive dataframe upon clicking save button ##
  observeEvent(input$Standard_PeakSave, {
    Standard_Peaki <- subset(Standard_RawDataFile(),
                    Standard_RawDataFile()[,input$Standard_x_ui] > Standard_temp.values$xmin &
                      Standard_RawDataFile()[,input$Standard_x_ui] < Standard_temp.values$xmax)
    
    # each input is a factor so levels are consistent for plotting characteristics
      add_row <- data.frame(SampleName = input$Standard_PeakName,
                            PeakAmount = input$Standard_PeakAmount,
                            PeakStart = input$Standard_plot_brush$xmin,
                            PeakEnd = input$Standard_plot_brush$xmax,
                            Baseline = mean(
                              Standard_RawDataFile()[which(Standard_RawDataFile()[,input$Standard_x_ui] 
                                                           == round(Standard_temp.values$xmin,
                                                                    digits=1)):which(Standard_RawDataFile()[,input$Standard_x_ui] 
                                                                                     == round(Standard_temp.values$xmin-input$Standard_BaselineLength,
                                                                                              digits=1)),
                                                     input$Standard_y_ui]),
                            AUC = MESS::auc(x = Standard_Peaki[,input$Standard_x_ui],
                                            y = Standard_Peaki[,input$Standard_y_ui] - mean(
                                              Standard_RawDataFile()[which(Standard_RawDataFile()[,input$Standard_x_ui] 
                                                                           == round(Standard_temp.values$xmin,
                                                                                    digits=1)):which(Standard_RawDataFile()[,input$Standard_x_ui] 
                                                                                                     == round(Standard_temp.values$xmin-input$Standard_BaselineLength,
                                                                                                              digits=1)),
                                                                     input$Standard_y_ui]),
                                            absolutearea = F))
      Standard_values$DT <- rbind(Standard_values$DT, add_row)
      
      
  })
  
  #delete a row when clicking delete botton and then re-calculate ddply dataframe for plots/tables
  observeEvent(input$Standard_DeletePeak, {
    Standard_values$DT <- head(Standard_values$DT,-1)
    
  })
  
  observeEvent(input$Standard_Calculate, {
    Standard_values2$DT <- data.frame(ddply(Standard_values$DT,.(SampleName,PeakAmount),plyr::summarize,
                                            AUC = mean(AUC, na.rm = T)))
    
    Standard_values2$DT <-  mutate(Standard_values2$DT, 
                                   Predicted = predict(lm(PeakAmount ~ AUC, data = Standard_values2$DT),
                                                              newdata = Standard_values2$DT),
                                   Equation = paste0(
                                     'Amount =',lm(PeakAmount ~ AUC, data = Standard_values2$DT)[["coefficients"]][["AUC"]],
                                     'x +',
                                    lm(PeakAmount ~ AUC, data = Standard_values2$DT)[["coefficients"]][["(Intercept)"]]),
                                   R2 = summary(lm(PeakAmount ~ AUC, data = Standard_values2$DT))$r.squared
    )
    
  })
  
  output$Standard_Outfile = renderTable({
    Standard_values$DT
    })
    
  output$Standard_Outfile2 <- renderTable({
    Standard_values2$DT
  })
  
  output$StandardCurve <- renderPlot({
   
   ggplot(Standard_values$DT, aes(x =Standard_values$DT$PeakAmount , y = Standard_values$DT$AUC)) + 
      geom_point() + 
      geom_smooth(method = "lm", formula = y ~ x,  se = F) +
      labs(x = "Amount", y = 'AUC', title = 'Standard Curve') +
      theme_bw()
  })
  
  output$Standard_Download <- downloadHandler(
     filename = function() {
      paste0("NOx_Standard",format(Sys.time(), "%Y-%m-%d_%H-%M"),".tar")
     },
     content = function(file) {
      standardpath <- "./Standard Outputs"
      dir.create(standardpath)

      write.csv(Standard_values2$DT,
                paste0(standardpath,"/Standard_AUCs_",Sys.Date(),".csv"))

      StandardRaw_Plot <- ggplot(Standard_RawDataFile(),
                                 aes(x =Standard_RawDataFile()[,input$Standard_x_ui],
                                     y = Standard_RawDataFile()[,input$Standard_y_ui])) +
        geom_line() +
        geom_line(aes(x =Standard_RawDataFile()[,input$Standard_x_ui],
                      y = signal::sgolayfilt(Standard_RawDataFile()[,input$Standard_y_ui])),
                  color = 'red') +
        labs(x = "Time", y = 'Signal', title = 'Standard Data')+
        theme_bw()
      ggsave(paste0(standardpath,'/Standard Data Plot_',Sys.Date(),'.pdf'),plot = StandardRaw_Plot, height = 6, width = 6)

      SC_Plot <- ggplot(Standard_values2$DT, aes(x =Standard_values2$DT$PeakAmount , y = Standard_values2$DT$AUC)) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ x,  se = F) +
        labs(x = "Amount", y = 'AUC', title = 'Standard Curve') +
        theme_bw()
      ggsave(paste0(standardpath,'/Standard Curve_',Sys.Date(),'.pdf'),plot = SC_Plot, height = 6, width = 6)

      tar(tarfile = file, files = standardpath)
    }
  )

  
  ###Sample Stuff
  
  Sample_RawDataFile <- eventReactive(input$Sample_File, {
    na.omit(read.csv(input$Sample_File$datapath))
  })
  
  ProcessedStandardCurve <- eventReactive(input$StandardCurveFile, {
    na.omit(read.csv(input$StandardCurveFile$datapath))
  })

  output$Sample_x_ui <- renderUI({
    selectInput(inputId = "Sample_x_ui", label = "Select X (Time):", choices= names(Sample_RawDataFile()))
  })
  
  output$Sample_y_ui <- renderUI({
    selectInput(inputId = "Sample_y_ui", label = "Select Y (mVH):", choices= names(Sample_RawDataFile()))
  })

  
  output$Sample_Plot <- renderPlot({
    
    ggplot(Sample_RawDataFile(),
           aes(x = Sample_RawDataFile()[,input$Sample_x_ui], 
               y= Sample_RawDataFile()[,input$Sample_y_ui])) +
      geom_point() +
      geom_line(aes(x =Sample_RawDataFile()[,input$Sample_x_ui],
                    signal::sgolayfilt(Sample_RawDataFile()[,input$Sample_y_ui]), 
                    col = 'red'),
              show.legend = FALSE) +
      theme_bw()
  
  })
  
  ##set up reactive data frame for temporary peaks (observed on click)
  Sample_temp.values<- reactiveValues()
  observeEvent(input$Sample_plot_brush, {
    Sample_temp.values$xmin <- input$Sample_plot_brush$xmin
    Sample_temp.values$xmax <- input$Sample_plot_brush$xmax
  })
  
  output$Sample_TempPeak <- renderPlot({
    
    Sample_Peaki <- subset(Sample_RawDataFile(),
                    Sample_RawDataFile()[,input$Sample_x_ui] > Sample_temp.values$xmin &
                      Sample_RawDataFile()[,input$Sample_x_ui] < Sample_temp.values$xmax)
    
    
    ggplot(Sample_Peaki,
           aes(x =Sample_Peaki[,input$Sample_x_ui], y = Sample_Peaki[,input$Sample_y_ui])) +
      geom_point() +
      geom_line(aes(x =Sample_Peaki[,input$Sample_x_ui],
                    y =signal::sgolayfilt(Sample_Peaki[,input$Sample_y_ui]),
                    color = 'red'), show.legend = FALSE) +
      geom_hline(yintercept =  mean(
        Sample_RawDataFile()[which(Sample_RawDataFile()[,input$Sample_x_ui]
                                     == round(Sample_temp.values$xmin,
                                              digits=1)):which(Sample_RawDataFile()[,input$Sample_x_ui]
                                                               == round(Sample_temp.values$xmin-input$Sample_BaselineLength,
                                                                        digits=1)),
                               input$Sample_y_ui]),
        col = 'yellow') +
      theme_bw()
    
  })
  
  
  output$Sample_tempdata <- renderTable({

      Sample_Peaki <- subset(Sample_RawDataFile(),
                               Sample_RawDataFile()[,input$Sample_x_ui] > Sample_temp.values$xmin &
                                 Sample_RawDataFile()[,input$Sample_x_ui] < Sample_temp.values$xmax)
      
      Sample_tempdataframe <- data.frame(
        PeakStart = Sample_temp.values$xmin,
        PeakEnd = Sample_temp.values$xmax,
        Baseline = mean(
          Sample_RawDataFile()[which(Sample_RawDataFile()[,input$Sample_x_ui] 
                                     == round(Sample_temp.values$xmin,
                                              digits=1)):which(Sample_RawDataFile()[,input$Sample_x_ui] 
                                                               == round(Sample_temp.values$xmin-input$Sample_BaselineLength,
                                                                        digits=1)),
                               input$Sample_y_ui]),
        AUC = MESS::auc(x = Sample_Peaki[,input$Sample_x_ui],
                        y = Sample_Peaki[,input$Sample_y_ui] - mean(
                          Sample_RawDataFile()[which(Sample_RawDataFile()[,input$Sample_x_ui] 
                                                     == round(Sample_temp.values$xmin,
                                                              digits=1)):which(Sample_RawDataFile()[,input$Sample_x_ui] 
                                                                               == round(Sample_temp.values$xmin-input$Sample_BaselineLength,
                                                                                        digits=1)),
                                               input$Sample_y_ui]),
                        absolutearea = F))
      
      Sample_tempdataframe
      
      
  })
  
  ##set up reactive dataframe for data frame that will save peaks##
  Sample_values <- reactiveValues()
  Sample_values$DT <- data.frame(SampleName = character(),
                                   PeakStart = numeric(),
                                   PeakEnd = numeric(),
                                 Baseline = numeric(),
                                 AUC = numeric())
  
  Sample_values2 <- reactiveValues()
  Sample_values2$DT <- data.frame(SampleName = character(),
                                  PeakStart = numeric(),
                                  PeakEnd = numeric(),
                                  Baseline = numeric(),
                                  AUC = numeric())
  
  ##add new row to reactive dataframe upon clicking save button ##
  observeEvent(input$Sample_PeakSave, {
    # each input is a factor so levels are consistent for plotting characteristics
    Sample_Peaki <- subset(Sample_RawDataFile(),
                    Sample_RawDataFile()[,input$Sample_x_ui] > Sample_temp.values$xmin &
                      Sample_RawDataFile()[,input$Sample_x_ui] < Sample_temp.values$xmax)
    
      Sample_add_row <- data.frame(SampleName = input$Sample_PeakName,
                            PeakStart = input$Sample_plot_brush$xmin,
                            PeakEnd = input$Sample_plot_brush$xmax,
                            Baseline = mean(
                              Sample_RawDataFile()[which(Sample_RawDataFile()[,input$Sample_x_ui] 
                                                         == round(Sample_temp.values$xmin,
                                                                  digits=1)):which(Sample_RawDataFile()[,input$Sample_x_ui] 
                                                                                   == round(Sample_temp.values$xmin-input$Sample_BaselineLength,
                                                                                            digits=1)),
                                                   input$Sample_y_ui]),
                            AUC = MESS::auc(x = Sample_Peaki[,input$Sample_x_ui],
                                            y = Sample_Peaki[,input$Sample_y_ui] - mean(
                                              Sample_RawDataFile()[which(Sample_RawDataFile()[,input$Sample_x_ui] 
                                                                         == round(Sample_temp.values$xmin,
                                                                                  digits=1)):which(Sample_RawDataFile()[,input$Sample_x_ui] 
                                                                                                   == round(Sample_temp.values$xmin-input$Sample_BaselineLength,
                                                                                                            digits=1)),
                                                                   input$Sample_y_ui]),
                                            absolutearea = F))
                            
      Sample_values$DT <- rbind(Sample_values$DT, Sample_add_row)
      
  })
  
  
  observeEvent(input$Sample_DeletePeak, {
    Sample_values$DT <- head(Sample_values$DT,-1)
    
  })
  
  observeEvent(input$Sample_Calculate, {

    Sample_values2$DT <-  mutate(Sample_values$DT, 
                                 Predicted = predict(lm(PeakAmount ~ AUC, data = ProcessedStandardCurve()),
                                                            newdata = Sample_values$DT),
                                 Equation = paste0(
                                   'Amount =',
                                   lm(PeakAmount ~ AUC, data = ProcessedStandardCurve())[["coefficients"]][["AUC"]],
                                   'x +',
                                   lm(PeakAmount ~ AUC, data = ProcessedStandardCurve())[["coefficients"]][["(Intercept)"]]),
                                   R2 = summary(lm(PeakAmount ~ AUC, data = ProcessedStandardCurve()))$r.squared)
                              
    
  })
  
  output$Sample_Outfile <- renderTable({
    Sample_values$DT
  })
  
  output$Sample_Outfile2 <- renderTable({
    Sample_values2$DT
  })
  
  output$Sample_Download <- downloadHandler(
    filename = function() {
      paste0("NOx_Sample",format(Sys.time(), "%Y-%m-%d_%H-%M"),".tar")
    },
    content = function(file) {
      samplepath <- "./Sample Outputs"
      dir.create(samplepath)
      
      write.csv(Sample_values2$DT,
                paste0(samplepath,"/SampleOutput_",Sys.Date(),".csv"))

      
      SampleRaw_Plot <- ggplot(Sample_RawDataFile()
                               , aes(x =Sample_RawDataFile()[,input$Sample_x_ui], 
                                     y = Sample_RawDataFile()[,input$Sample_y_ui])) + 
        geom_line() +
        geom_line(aes(x =Sample_RawDataFile()[,input$Sample_x_ui], 
                      y = signal::sgolayfilt(Sample_RawDataFile()[,input$Sample_y_ui])),
                  color = 'red') +
        labs(x = "Time", y = 'Signal', title = 'Sample Data') +
        theme_bw()
      ggsave(paste0(samplepath,'/Sample Data Plot_',Sys.Date(),'.pdf'),plot = SampleRaw_Plot, height = 6, width = 6)
      

      
      tar(tarfile = file, files = samplepath)
    }
  )
  
  
}


shinyApp(ui = ui, server = server)
