# Hola
install.packages("shiny")
install.packages("ggplot2")
library(shiny)
library(shinythemes)
library(ggplot2)
library(quantmod)
library(shinyjs)
library(rmarkdown)

ui <- fluidPage(
  theme = shinytheme('united'),
  navbarPage("Simulación de Monte Carlo en Finanzas",
             tabPanel("Inicio",
                        includeMarkdown('docs/Project_description.md')
                      ),
             tabPanel("Dashboard",
                      titlePanel("Interactive QQQ Stock Data Visualization"),
                      sidebarLayout(
                        sidebarPanel(
                          dateInput("startDate", "Start Date", value = "2020-10-01", min = "2000-01-01"),
                          dateInput("endDate", "End Date", value = "2021-01-01", min = "2000-01-01"),            
                          #agregado los indicadores:
                          checkboxInput("showMA", "Show Moving Average", value = FALSE),
                          checkboxInput("showWMA", "Show Weighted Moving Average", value = FALSE)
                          ),
                        mainPanel(
                          plotOutput("stockPlot")
                        )
                      )
             ),
             tabPanel("Resultados",'Vacio por el momento'),
             tabPanel("Referencias",'Vacio por el momento'),
             navbarMenu('Explicación Teórica',
                        tabPanel("Método de Monte Carlo"),
                        tabPanel("Movimiento browniano geométrico"),
                        "----",
                        "Explicacion de Indices",
                        tabPanel("QQQ"))
  ),
             )

#
server <- function(input, output, session) {
  output$stockPlot <- renderPlot({
    # Fetch dates from input
    inicio <- input$startDate
    fin <- input$endDate
    
    # Fetch stock data
    getSymbols("QQQ", from = inicio, to = fin, src = "yahoo")
    
    
#--------------------------------------------    
    # Plotting the stock data
#    chartSeries(QQQ,
#                theme = chartTheme("white"),
#                name = "QQQ",  
#                TA = list("addBBands(n = 10)",
#                          "addVo()",
#                          "addEMA(64)",
#                          "addEMA(10, col = 2)"))
#------------------------------------------  
   
#---TODO ESTO ES AGREGADO DE LOS INDICADORES---------------------
    
    # Configura el gráfico inicial
    chart <- chartSeries(QQQ, theme = chartTheme("white"))
    
    # Añade Bandas de Bollinger
    addBBands(n = 20, on = 1)  # asegúrate de que el parámetro 'on' está correctamente asignado
    
    # Añade volumen de trading
    addVo()
    
    # Condicionales para EMA y WMA
    if (input$showMA) {
      addEMA(QQQ, n = 10, col = "blue", on = 1)  # Añade la EMA de 10 períodos en color azul
    }
    
    if (input$showWMA) {
      addWMA(QQQ, n = 10, col = "red", on = 1)  # Añade la WMA de 10 períodos en color rojo
    }
    
  })
}

shinyApp(ui = ui, server = server)

# Get QQQ data from Yahoo Finance
getSymbols("QQQ", src = "yahoo", from = "2023-10-01", to = "2025-01-01")

# View the first few rows of the data
head(QQQ)

# Plot the closing prices
chartSeries(QQQ, type = "line", TA = "addVo();addBBands()")

# Calculate returns
qqq_returns <- dailyReturn(QQQ)

# Plot returns
chartSeries(qqq_returns, type = "line")


# Omega Strikers
# Palia

# Fechas
inicio <- "2020-10-01"
fin <- "2021-01-01"

# Obtenemos los datos
getSymbols("QQQ", 
           from = inicio, to = fin,
           src = "yahoo") 

# Dibujamos la serie
chartSeries(QQQ,
            theme = chartTheme("white"),
            name = "QQQ",  
            TA = list("addBBands(n = 10)",
                      "addVo()",
                      "addEMA(64)",
                      "addEMA(10, col = 2)"))


