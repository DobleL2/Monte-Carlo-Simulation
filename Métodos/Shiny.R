
source("Métodos/GBM.R")
source("Métodos/Volatilidad_estocastica.R")
source("Métodos/Parametros_volatilidad.R")
source("Métodos/Parametro_media_ret_y_vola.R")
source("Métodos/Varios.R")
source("Métodos/Tablas_1.R")

library(shiny)
library(shinythemes)
library(plotly)

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
                          numericInput("percentage", label = h3("Porcentaje dispuesto a perder (Drop Down)"), 
                                       value = 50,    # Initial value
                                       min = 0,       # Minimum value
                                       max = 100,     # Maximum value
                                       step = 1       # Step size
                          ),
                          numericInput("k", label = h3("Valor de inversión $"), value = 10000),
                          dateRangeInput("fecha", h3("Selecciona un rango de fechas:"),
                                         start = Sys.Date() - 30, # Fecha inicial (hace 30 días desde hoy)
                                         end = Sys.Date(),       # Fecha final (hoy)
                                         min = Sys.Date() - 365*3, # Fecha mínima
                                         max = Sys.Date()        # Fecha máxima
                          ),
                          
                         
                          #agregado los indicadores:
                          
                          numericInput("nsim", label = h3("Numero de Simulaciones"), value = 1000),
                          # Input: Seleccionar tipo de gráfico
                          selectInput("chartType", "Select chart type:",
                                      choices = c("Scatter Plot" = "scatter",
                                                  "Bar Chart" = 'bar')),
                          checkboxInput("showGraph", "Show Graph", value = TRUE)
                        ),
                        mainPanel(
                          textOutput("rango"),
                          plotlyOutput("plot"),
                          plotlyOutput("plot2")
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


server <- function(input, output, session) {
  simulated <- reactive({

  })
  
  
  
  output$rango <- renderText({
    paste("Has seleccionado desde", input$fecha[1], "hasta", input$fecha[2])
  })
  output$plot <- renderPlotly({
    if (input$showGraph){
      # Datos de ejemplo
      set.seed(123)
      data <- data.frame(
        x = 1:100,
        y = rnorm(100)
      )
      
      # Generar un gráfico basado en la elección del usuario
      p <- plot_ly(data, x = ~x, y = ~y, type = input$chartType)
      p  
    } else {
      return(NULL)
    }})
  output$plot2 <- renderPlotly({
    data <- data.frame(
      x = 1:length(simulated()),
      y = simulated()
    )
      p <- plot_ly(data, x = ~x, y = ~y)
      p
  })
}

shinyApp(ui = ui, server = server)