library("quantmod")
library("timeSeries")
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(PortfolioAnalytics)
library(fPortfolio)
library(forecast)
library(shiny)

# Define UI ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Análisis de Portafolios y Pronóstico de Acciones"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # File input for uploading data ----
      fileInput("file", "Subir archivo de datos CSV:",
                accept = c(".csv")),
      
      # Date inputs for selecting start and end dates ----
      dateInput("startDate", "Fecha de inicio:", value = Sys.Date() - 365),
      dateInput("endDate", "Fecha de fin:", value = Sys.Date()),
      
      # Action button to process data ----
      actionButton("processData", "Procesar datos"),
      
      hr(),
      
      # Text input for adding stock tickers ----
      textInput("tickerInput", "Ingrese la sigla de la acción:"),
      actionButton("addTicker", "Agregar"),
      actionButton("reset", "Borrar todo"),
      hr(),
      verbatimTextOutput("tickersDisplay")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Placeholder for output ----
      textOutput("status"),
      
      # Output: Plot of forecast ----
      plotOutput("forecastPlot"),
      
      # Output: Plot of efficient frontier ----
      plotOutput("plot1"),
      
      # Output: Bar plot of weights on efficient frontier ----
      plotOutput("plot2"),
      
      # Output: Table of weights for minimum variance portfolio ----
      tableOutput("table")
      
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # Vector to store uploaded data
  serie <- reactiveVal(NULL)
  
  # Vector to store stock tickers
  tickers <- reactiveVal(character(0))
  
  # Observe file upload and store data in 'serie' vector
  observeEvent(input$file, {
    serie(read.csv(input$file$datapath))
  })
  
  # Process data when button is clicked
  observeEvent(input$processData, {
    if (!is.null(serie())) {
      # Convert data to time series
      serie_ts <- ts(serie()$datos, start = as.Date(input$startDate), end = as.Date(input$endDate), frequency = 1)
      
      # Make forecast
      forecast_plot <- forecast(serie_ts)
      
      # Plot forecast
      output$forecastPlot <- renderPlot({
        plot(forecast_plot)
      })
      
      # Display status
      output$status <- renderText({
        "Pronóstico generado correctamente."
      })
      
    } else {
      output$status <- renderText({
        "Por favor, sube un archivo de datos primero."
      })
    }
  })
  
  # Add stock ticker
  observeEvent(input$addTicker, {
    ticker <- toupper(input$tickerInput)
    if (nchar(ticker) > 0 && !ticker %in% tickers()) {
      tickers(c(tickers(), ticker))
    }
  })
  
  # Display stock tickers
  output$tickersDisplay <- renderPrint({
    tickers()
  })
  
  # Reset stock tickers
  observeEvent(input$reset, {
    tickers(NULL)
  })
  
  # Portfolio data for efficient frontier
  portfolioData <- reactive({
    if (length(tickers()) > 0) {
      PrecPort <- NULL
      for (Ticker in tickers())
        PrecPort <- cbind(PrecPort, getSymbols(Ticker, from = '2023-04-25', auto.assign = FALSE)[,4])
      colnames(PrecPort)<-tickers()
      RetPort <- na.omit(ROC(PrecPort, type = "discrete"))
      RetPort <- as.timeSeries(RetPort)
      list(RetPort = RetPort, PrecPort = PrecPort)
    }
  })
  
  # Plot efficient frontier
  output$plot1 <- renderPlot({
    if (!is.null(portfolioData())) {
      fronteraEff <- portfolioFrontier(portfolioData()$RetPort, constraints = "LongOnly")
      plot(fronteraEff, c(1,2,7,8))
    }
  })
  
  # Plot weights on efficient frontier
  output$plot2 <- renderPlot({
    if (!is.null(portfolioData())) {
      fronteraEff <- portfolioFrontier(portfolioData()$RetPort, constraints = "LongOnly")
      fronteraPesos <- getWeights(fronteraEff)
      colnames(fronteraPesos) <- tickers()
      barplot(t(fronteraPesos), 
              main = "Pesos de los activos en la frontera Eficiente", 
              col = cm.colors(ncol(fronteraPesos) + 2), 
              legend = colnames(fronteraPesos))
    }
  })
  
  # Table of weights for minimum variance portfolio
  output$table <- renderTable({
    if (!is.null(portfolioData())) {
      VMG <- minvariancePortfolio(portfolioData()$RetPort, spec = portfolioSpec(), constraints = "LongOnly")
      VMG_Pesos <- getWeights(VMG) * 100
      DF_VMG_Pesos <- data.frame(VMG_Pesos)
      colnames(DF_VMG_Pesos) <- "Pesos (%)"
      DF_VMG_Pesos$Acciones <- rownames(DF_VMG_Pesos)
      rownames(DF_VMG_Pesos) <- NULL
      DF_VMG_Pesos
    }
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)
