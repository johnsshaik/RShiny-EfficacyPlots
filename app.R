library(shiny)
library(ggplot2)
library(plotly)
library(survival)
library(survminer)
library(viridis)
library(readxl)
library(haven)
library(vroom)
library(DT)
library(psych)
library(plotly)

ui <- fluidPage(
  navbarPage("User Interface:",tabPanel("Data",
                                        titlePanel("Select Input data"),
                                        sidebarLayout(
                                          sidebarPanel(
                                            fileInput("file1", "Choose Dataset/File",
                                                      multiple = TRUE,
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv"))
                                            # ,
                                            # tags$hr(),
                                            # checkboxInput("header", "Header", TRUE),
                                            # radioButtons("sep", "Separator",
                                            #              choices = c(Comma = ",",
                                            #                          Semicolon = ";",
                                            #                          Tab = "\t"),
                                            #              selected = ","),
                                            # tags$hr(),
                                            # radioButtons("disp", "Display",
                                            #              choices = c(Head = "head",
                                            #                          All = "all"),
                                            #              selected = "head"),
                                            # radioButtons("quote", "Quote",
                                            #              choices = c(None = "",
                                            #                          "Double Quote" = '"',
                                            #                          "Single Quote" = "'"),
                                            #              selected = '"')
                                            ),
                                          mainPanel(
                                            verbatimTextOutput("summary"),
                                            #tableOutput("contents")
                                            dataTableOutput("contents")
                                          ))), 
             
             
             
             
             
             tabPanel(
               "Plots",
               tabsetPanel(
                 tabPanel(
                   "KM Plot",
                   # fluidRow(
                   #   # column(width = 6,
                   #   #        plotOutput("km", height = "350px")
                   #   # )
                   # ),
                   
                   pageWithSidebar(
                     headerPanel(''),
                     # sidebarPanel(
                     #   selectInput('anlvar', 'Analysis Variable', "", selected = ""),
                     #   selectInput('cnsrvar', 'Censor Variable', "", selected = ""),
                     #   selectInput('bygrpvar', 'By Group Variable', "", selected = "")
                     #   
                     # ),
                     
                     sidebarPanel( uiOutput("variable_kanl"),
                                   uiOutput("variable_cnsr"),
                                   uiOutput("variable_bygrp")
                                   ),
                     
                     mainPanel(plotlyOutput('plot4'))
                   )
                   
                   
                 ),
                 tabPanel(
                   "Waterfall Plot",
                   pageWithSidebar(
                     headerPanel(''),
                     # sidebarPanel(
                     #   selectInput('pchgvar', 'Analysis Variable', "", selected = ""),
                     #   selectInput('colorvar', 'Color Variable', "", selected = "")
                     #   
                     #   
                     # ),
                     
                     sidebarPanel( uiOutput("variable_wfanl"),
                                   uiOutput("variable_wfgrp"),
                                   uiOutput("variable_wfxvar")                         ),
                     mainPanel(plotlyOutput('plot5'))
                   )
                   
                 ),
                 
                 tabPanel(
                   "Box Plot",
                   pageWithSidebar(
                     headerPanel(''),
                     # sidebarPanel(
                     #   selectInput('xa1', 'X-axis Variable', ""),
                     #   selectInput('ya1', 'Y-axis Variable', "", selected = "")
                     #   
                     #   
                     # ),
                     sidebarPanel( uiOutput("variable_boxx"),
                                   uiOutput("variable_boxy"),
                                   uiOutput("variable_boxgrp")                         ),
                     mainPanel(plotlyOutput('plot1'))
                   )
                   
                 ),
                 tabPanel(
                   "Scatter Plot",
                   pageWithSidebar(
                     headerPanel(''),
                     # sidebarPanel(
                     #   selectInput('xa2', 'X-axis Variable', ""),
                     #   selectInput('ya2', 'Y-axis Variable', "", selected = "")
                     #   
                     #   
                     # ),
                     sidebarPanel( uiOutput("variable_sctx"),
                                   uiOutput("variable_scty"),
                                   uiOutput("variable_sctgrp")                         ),
                     mainPanel(plotlyOutput('plot2'))
                   )
                   
                 )
                 
                
               )
             ),
             
             
             # tabPanel("Graphing",
             #          titlePanel("Plotting Graphs"),
             #          sidebarLayout(
             #            sidebarPanel( uiOutput("variable_x"),
             #                          uiOutput("variable_y")),
             #            mainPanel(
             #              h3(textOutput("caption")),
             #              plotOutput("plot")
             #            )
             #          ))
  ))

server <- function(input, output, session) {
  onSessionEnded(stopApp)
  data <- reactive({
    req(input$file1)
    

    
    
     if(stringr::str_ends(input$file1$datapath, "csv")) {
      df <- read.csv(input$file1$datapath)
    } else if (stringr::str_ends(input$file1$datapath, "(xlsx|xls)")) {
      df <- read_excel(input$file1$datapath)
    } else if (stringr::str_ends(input$file1$datapath, "sas7bdat")) {
      df <- read_sas(input$file1$datapath)
    }
    
    #df <- read_sas(input$file1$datapath)
    
    
    #df <- read.csv(input$file1$datapath)
    #df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote)
    #return(df)
  })
  
  output$contents <- DT::renderDataTable(
    
      return(data())
    # data(),
    # options = list(scrollX = TRUE),
    # filter = "top",
    # rownames = FALSE
    
  )
  output$summary <- renderPrint({
    #summary(data())
    str(as.data.frame(data()))
    #describe(data())
    
  })
  
  
  output$variable_kanl <- renderUI({
    selectInput("variableNames_kanl", label = "Analysis Variable", choices = names(data()))  
  })
  
  output$variable_cnsr <- renderUI({
    selectInput("variableNames_cnsr", label = "Censor Variable", choices = names(data()) ) 
  })
  
  output$variable_bygrp <- renderUI({
    selectInput("variableNames_bygrp", label = "By Group Variable", choices = names(data()))  
  })
  
  output$variable_wfanl <- renderUI({
    selectInput("variableNames_wfanl", label = "Analysis Variable", choices = names(data()) ) 
  })
  
  output$variable_wfgrp <- renderUI({
    selectInput("variableNames_wfgrp", label = "Response Variable", choices = names(data()))  
  })
  
  output$variable_wfxvar <- renderUI({
    selectInput("variableNames_wfxvar", label = "X-axis Variable", choices = names(data()))  
  })
  
  
  output$variable_boxx <- renderUI({
    selectInput("variableNames_boxx", label = "X-Axis variable", choices = names(data()) ) 
  })
  output$variable_boxy <- renderUI({
    selectInput("variableNames_boxy", label = "Y-axis variable", choices = names(data()) ) 
  })
  output$variable_boxgrp <- renderUI({
    selectInput("variableNames_boxgrp", label = "Color fill - grouping variable", choices = names(data()) ) 
  })
  
  output$variable_sctx <- renderUI({
    selectInput("variableNames_sctx", label = "X-Axis variable", choices = names(data()) ) 
  })
  output$variable_scty <- renderUI({
    selectInput("variableNames_scty", label = "Y-axis variable", choices = names(data()) ) 
  })
  output$variable_sctgrp <- renderUI({
    selectInput("variableNames_sctgrp", label = "Color fill - grouping variable", choices = names(data()) ) 
  })
  
  survdat <- reactive({
    test <- data.frame(data()[[input$variableNames_kanl]], data()[[input$variableNames_cnsr]],data()[[input$variableNames_bygrp]])
    colnames(test) <- c("AVAL", "CNSR", "BYGRP")
    return(test)
    
    #test <- data() %>% select(input$variableNames_kanl,input$variableNames_cnsr,input$variableNames_bygrp)
    #return(test)
  })
  
  wfdat <- reactive({
    wftest <- data.frame(data()[[input$variableNames_wfanl]], data()[[input$variableNames_wfgrp]], data()[[input$variableNames_wfxvar]])
    colnames(wftest) <- c("PCHG",  "WFGRP", "SUBJECT")
    wftest <- subset(wftest, SUBJECT < 50 & PCHG > -100)
    wftest <- wftest[order(wftest$PCHG),]
    return(wftest)
  })
  
  boxdat <- reactive({
    boxtest <- data.frame(data()[[input$variableNames_boxx]], data()[[input$variableNames_boxy]], data()[[input$variableNames_boxgrp]])
    colnames(boxtest) <- c("BOXX",  "BOXY", "BOXGRP")
    boxtest$BOXGRP <- factor(boxtest$BOXGRP)
    #wftest <- subset(wftest, SUBJECT < 50 & PCHG > -100)
    #wftest <- wftest[order(wftest$PCHG),]
    return(boxtest)
  })
  
  sctdat <- reactive({
    scttest <- data.frame(data()[[input$variableNames_sctx]], data()[[input$variableNames_scty]], data()[[input$variableNames_sctgrp]])
    colnames(scttest) <- c("SCTX",  "SCTY", "SCTGRP")
    #wftest <- subset(wftest, SUBJECT < 50 & PCHG > -100)
    #wftest <- wftest[order(wftest$PCHG),]
    return(scttest)
  })
  
  output$plot4 <- renderPlotly({
    if (is.null(data)) { return(NULL)
    } else {
      # ggplot(dat(),aes(x = X,y = Y)) + geom_point(colour = 'red',height = 400,width = 600) +
      #   labs(y = input$variableNames_y,
      #        x = input$variableNames_x,
      #        title = "ggplot")
      
      srvpplt <- ggsurvplot(survfit(Surv(AVAL , CNSR) ~ BYGRP , data = survdat() ) , risk.table = TRUE, pval = TRUE, data = survdat() , main = "KM estimates of Overall Survival")
      ggplotly(srvpplt[[1]])
      #ggsurvplot(survfit(Surv(input$variableNames_kanl , input$variableNames_cnsr) ~ input$variableNames_bygrp , data = survdat() ) , risk.table = TRUE, pval = TRUE, data = survdat() , main = "KM estimates of Overall Survival")
      
    }
  })
  
  output$plot5 <- renderPlotly({
    if (is.null(data)) { return(NULL)
    } else {
      
      ggplotly(
        ggplot(data = wfdat(), aes(SUBJECT, PCHG)) +
        geom_bar(aes(fill = WFGRP), stat = "identity") +
        #scale_fill_manual(values = c("grey60", "coral2")) +
        theme_minimal() +
        theme(
          
          axis.text.x = element_text(angle = 90)
        )
      )
      
    }
  })
  
  
  output$plot1 <- renderPlotly({
    if (is.null(data)) { return(NULL)
    } else {
      ggplotly(
      ggplot(boxdat(),
             aes(
               x = BOXX,
               y = BOXY,
               fill = BOXGRP
             )) + geom_boxplot(
               # custom boxes
               
               
               # custom outliers
               outlier.colour = "red",
               outlier.fill = "red",
               outlier.size = 3
             ) +
        scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
        geom_jitter(color = "blue",
                    size = 0.4,
                    alpha = 0.9) +
        theme(legend.position = "top",
              plot.title = element_text(size = 14)) +
        ggtitle("A boxplot with jitter")
      )
    }
  })
  
  output$plot2 <- renderPlotly({
    if (is.null(data)) { return(NULL)
    } else {
      ggplotly(
      ggplot(sctdat(), aes(x = SCTX, y = SCTY, fill = SCTGRP)) + geom_point(
        shape = 21,
        color = "black",
        # fill = SCTGRP,
        size = 3
      )
  )
    }
  })
  
  
  
  
}

shinyApp(ui, server)