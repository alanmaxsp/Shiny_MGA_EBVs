#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)

# Función para simular datos de EBV y asignar percentiles
simular_y_asignar_percentiles <- function(n_toros, percentiles) {
    set.seed(123)
    data <- data.frame(Toro = 1:n_toros, EBV = rnorm(n_toros, mean = 100, sd = 10))
    data$Percentil <- (1- ecdf(data$EBV)(data$EBV))*100
    return(data)
}

# Función para calcular promedio y desviación estándar de los EBVs
calcular_promedio_desvio <- function(data) {
    promedio <- mean(data$EBV)
    desvio <- sd(data$EBV)
    mensaje <- paste("Promedio: ", round(promedio, 2), "\nDesvío estándar: ", round(desvio, 2))
    return(mensaje)
}

# Función para graficar el cambio en el promedio de los datos filtrados
graficar_cambio_promedio <- function(data, toros_filtrados) {
    promedio_original <- mean(data$EBV)
    promedio_filtrado <- mean(toros_filtrados$EBV)
    
    p <- ggplot() +
        geom_density(data = data, aes(x = EBV), fill = "blue", alpha = 0.5) +
        geom_density(data = toros_filtrados, aes(x = EBV), fill = "red", alpha = 0.5) +
        geom_vline(xintercept = promedio_original, linetype = "dashed", color = "blue", size = 1) +
        geom_vline(xintercept = promedio_filtrado, linetype = "dashed", color = "red", size = 1) +
        labs(x = "EBV", y = "Densidad", title = "Cambio en el Promedio de EBV")
    
    return(p)
}

# Definir UI
ui <- fluidPage(
    titlePanel("EBV de Toros"),
    
    sidebarLayout(
        sidebarPanel(
            numericInput("n_toros", "Número de toros:", value = 100, min = 1),
            actionButton("simular", "Simular datos y asignar percentiles"),
            sliderInput("rango_ebv", "Rango de EBV:", min = 0, max = 200, value = c(0, 200)),
            sliderInput("rango_percentil", "Rango de Percentiles:", min = 0, max = 100, value = c(0, 100), step = 1),
            numericInput("id_toro", "ID del Toro:", value = NA),
            dataTableOutput("tabla_resultado"),
            verbatimTextOutput("estadisticas")
        ),
        
        mainPanel(
            plotOutput("grafico_cambio_promedio")
        )
    )
)

# Definir server
server <- function(input, output, session) {
    datos <- reactiveValues(data = NULL)
    toros_filtrados <- reactiveValues(data = NULL)
    
    # Simular datos y asignar percentiles al hacer clic en el botón
    observeEvent(input$simular, {
        percentiles <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)
        datos$data <- simular_y_asignar_percentiles(input$n_toros, percentiles)
        toros_filtrados$data <- NULL
    })
    
    # Graficar el cambio en el promedio de los datos filtrados
    output$grafico_cambio_promedio <- renderPlot({
        if (!is.null(datos$data) && !is.null(toros_filtrados$data)) {
            graficar_cambio_promedio(datos$data, toros_filtrados$data)
        }
    })
    
    # Filtrar los datos según los criterios de búsqueda
    filtered_data <- reactive({
        if (!is.null(datos$data)) {
            filter_data <- datos$data
            
            # Filtrar por rango de EBV
            filter_data <- filter_data[filter_data$EBV >= input$rango_ebv[1] & filter_data$EBV <= input$rango_ebv[2], ]
            
            # Filtrar por rango de percentiles
            filter_data <- filter_data[filter_data$Percentil >= input$rango_percentil[1] & filter_data$Percentil <= input$rango_percentil[2], ]
            
            # Filtrar por ID del toro
            if (!is.na(input$id_toro)) {
                filter_data <- filter_data[filter_data$Toro == input$id_toro, ]
            }
            
            toros_filtrados$data <- filter_data
            
            return(filter_data)
        }
    })
    
    # Mostrar el resultado de la búsqueda en la tabla y calcular estadísticas
    output$tabla_resultado <- renderDataTable({
        if (!is.null(filtered_data())) {
            datatable(filtered_data())
        }
    })
    
    # Calcular promedio y desviación estándar
    output$estadisticas <- renderPrint({
        if (!is.null(datos$data)) {
            mensaje_original <- calcular_promedio_desvio(datos$data)
            mensaje_filtrado <- calcular_promedio_desvio(toros_filtrados$data)
            
            paste("Estadísticas EBV Original:\n", mensaje_original, "\nEstadísticas EBV Filtrado:\n", mensaje_filtrado)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
