#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Configurar working directory
# setwd("D:\__ProjectsDev\EAE\06_shinny_grupal\App")

# Instalar librerias
# install.packages("shinydashboard")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("DT")
# install.packages("ggplot2")

library(shinydashboard)
library(dplyr)
library(tidyverse)
library(DT)
library(ggplot2)


# Carga de dataset
file <- "datos_uso_catastral_2018.csv"
data <- read.csv(file=file.path(file), header=T, sep=";", encoding = "UTF-8")


# Limpieza y formateo del dataset
data$val_cat_barrio <- gsub(" €", "", data$val_cat_barrio)
data$val_cat_medio <- gsub(" €", "", data$val_cat_medio)

data$barrio_nombre <- gsub(" ", "-", data$barrio_nombre)

data$val_cat_barrio <- gsub(",", ".", data$val_cat_barrio)
data$val_cat_medio  <- gsub(",", ".", data$val_cat_medio)

data$anio_cons_medio <- gsub(",", ".", data$anio_cons_medio)
data$sup_cons_barrio <- gsub(",", ".", data$sup_cons_barrio)
data$sup_cons_media <- gsub(",", ".", data$sup_cons_media)

data <- transform(data, anio_cons_medio = as.integer(anio_cons_medio))
data <- transform(data, sup_cons_barrio = as.integer(sup_cons_barrio))
data <- transform(data, sup_cons_media = as.integer(sup_cons_media))
data <- transform(data, sup_suelo_barrio = as.integer(sup_suelo_barrio))
data <- transform(data, sup_suelo_media  = as.integer(sup_suelo_media))
data <- transform(data, val_cat_barrio = as.double(val_cat_barrio))
data <- transform(data, val_cat_medio = as.double(val_cat_medio))

data 

stUso <- data %>%
  group_by(uso_deno) %>%
  slice(1) %>%
  arrange(uso_deno)

stDistritos <- data %>%
  group_by(distrito_cod) %>%
  slice(1) %>%
  arrange(distrito_nombre)

vUso <- as.vector(stUso$uso_deno)
vDistrictos <- as.vector(stDistritos$distrito_nombre)


ui <- dashboardPage(
  dashboardHeader(title = "Grupo N"),
  dashboardSidebar(
    selectInput(
      inputId = "uso",
      label="Uso Catastral:",
      choices= vUso,
      selected = c("Comercial"),
      multiple = FALSE
    ),
    
    radioButtons(inputId = "distrito", 
                 label = "Distritos:",
                 choices =  vDistrictos,
                 selected = c("Chamberí")
    )
  ),
  dashboardBody(
    fluidRow(
      infoBoxOutput("valorMedio"),
      infoBoxOutput("inmueblesBario"),
      infoBoxOutput("anioMedio")
      
    ),
    
    fluidRow(
      box(plotOutput("plot1")),
      box(plotOutput("plot2"))
    ),
    
    fluidRow(
      DTOutput('tablaDatos')
    )
  )
)

server <- function(input, output) {
  observe({
    print(input$distrito)
  })
  
  output$inmueblesBario <- renderInfoBox({
    data2 <- 0
    
    if (!is.null(input$distrito)) {
      data2 <- data %>%
        filter(data$uso_deno == input$uso & data$distrito_nombre == input$distrito) %>%
        summarise(total = sum(inmuebles_barrio))
    }
  
    infoBox("Inmuebles por barrio", 
            data2, 
            icon = icon("credit-card"),
            color = "purple"
    )
  })
  
  output$anioMedio <- renderInfoBox({
    data2 <- 0
    
    if (!is.null(input$distrito)) {
      data2 <- data %>%
        filter(data$uso_deno == input$uso & data$distrito_nombre == input$distrito) %>%
        summarise(total = mean(anio_cons_medio, na.rm=TRUE))
      
      data2 <- round(data2, digits = 0)
    }
    
    infoBox("Año medio de construcción", 
            data2, 
            icon = icon("credit-card"),
            color = "purple"
    )
  })
  
  output$valorMedio <- renderInfoBox({
    data2 <- 0
    
    if (!is.null(input$distrito)) {
      data2 <- data %>%
        filter(data$uso_deno == input$uso & data$distrito_nombre == input$distrito) %>%
        summarise(total = mean(val_cat_barrio, na.rm=TRUE))
      
      data2 <- round(data2, digits = 2)
    }
    
    infoBox("Valor medio", 
            paste0("€ ", format(round(data2/1e6, 1), trim=TRUE), "M"), 
            icon = icon("credit-card"),
            color = "purple"
    )
  })
  
  # Gráfica 1
  output$plot1 <- renderPlot({
    data2 <- data %>%
      filter(data$uso_deno == input$uso & data$distrito_nombre == input$distrito) %>%
      arrange(desc(val_cat_medio))
    
    grafica <- ggplot(data=data2, mapping = aes_string(data2$barrio_cod, data2$val_cat_medio)) + 
      geom_point(mapping = aes_string(color = data2$barrio_cod), size=data2$inmuebles_barrio/100) + 
      xlab("Códigos de Barrios") +
      ylab("Valor medio catastral") +
      labs(title="Valor / Número de inmuebles por barrio", subtitle="El tamaño de los puntos indica el número de inmuebles por barrio") +
      theme_minimal()
    
    grafica
  })
  
  # Gráfica 2
  output$plot2 <- renderPlot({
    data2 <- data %>%
      filter(data$uso_deno == input$uso & data$distrito_nombre == input$distrito) %>%
      arrange(desc(anio_cons_medio))
    
    grafica <- ggplot(data=data2, aes(x=reorder(barrio_nombre, anio_cons_medio), y=anio_cons_medio)) +
      geom_bar(stat="identity", width=0.5, fill="steelblue") +
      geom_text(aes(label=paste0(anio_cons_medio)), position=position_dodge(width=0.9), vjust=0.3, hjust=1.5) +
      coord_flip() +
      ylab("Año") +
      xlab("Barrios") +
      ggtitle("Año medio de construcción por barrio")
    
    grafica  })
  
  # Tabla
  output$tablaDatos <- renderDT({
    data2 <- data %>%
      filter(data$uso_deno == input$uso & data$distrito_nombre == input$distrito) %>%
      select(distrito_nombre, barrio_cod, barrio_nombre, uso_deno, inmuebles_barrio, anio_cons_medio, val_cat_barrio, val_cat_medio) %>%
      arrange(distrito_nombre)
    
    colnames(data2) <- c("Distrito", "Barrio Codigo", "Barrio", "Uso", "Cantidad Inmuebles", "Año medio de construcción", "Valor catastro", "Valor catastro medio")
    
    
    datatable(data2)
  })
}

shinyApp(ui, server)