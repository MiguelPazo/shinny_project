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
# install.packages("randomcoloR")

library(shinydashboard)
library(dplyr)
library(tidyverse)
library(DT)
library(ggplot2)
library(randomcoloR)
library(scales)


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


# Asignando color por barrio
dataBarrios <- data %>%
  distinct(barrio_cod)

pal <- distinctColorPalette(nrow(dataBarrios))
dataBarrios$barrio_color <- pal 

data <- merge(data, dataBarrios, by="barrio_cod")


# Filtros del sidebar
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
  dashboardHeader(title = "Grupo 6"),
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
    print(input$uso)
  })
  
  output$inmueblesBario <- renderInfoBox({
    data2 <- 0
    
    if (!is.null(input$distrito)) {
      data2 <- data %>%
        filter(data$uso_deno == input$uso & data$distrito_nombre == input$distrito) %>%
        summarise(total = sum(inmuebles_barrio))
    }
    
    data2 <- format(round(as.numeric(data2), 0), nsmall=0, big.mark=",")
  
    infoBox("Inmuebles por barrio", 
            data2, 
            icon = icon("fa-solid fa-archway"),
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
            icon = icon("fa-solid fa-calendar"),
            color = "purple"
    )
  })
  
  output$valorMedio <- renderInfoBox({
    data2 <- 0
    
    if (!is.null(input$distrito)) {
      data2 <- data %>%
        filter(data$uso_deno == input$uso & data$distrito_nombre == input$distrito) %>%
        summarise(total = mean(val_cat_medio, na.rm=TRUE))
      
      data2 <- round(data2, digits = 2)
    }
    
    infoBox("Valor medio en miles de €", 
            paste0("€ ", format(round(data2/1e3, 1), trim=TRUE), "M"), 
            icon = icon("glyphicon glyphicon-eur"),
            color = "purple"
    )
  })
  
  # Gráfica 1
  output$plot1 <- renderPlot({
    data2 <- data %>%
      filter(data$uso_deno == input$uso & data$distrito_nombre == input$distrito) %>%
      arrange(desc(val_cat_medio))
    
    maxValue <- max(data2$inmuebles_barrio, na.rm = TRUE)
    divisor <- 100
    
    if(maxValue < 1000) {
      divisor <- 20
    }
    
    grafica <- ggplot(data=data2, mapping = aes(barrio_nombre, val_cat_medio)) + 
      geom_point(mapping = aes(color = barrio_color), size=data2$inmuebles_barrio/divisor) + 
      geom_text(aes(label=format(as.numeric(inmuebles_barrio), 0, nsmall=0, big.mark=",")), position=position_dodge(width=0.9), vjust=0.3, hjust=1.5) +
      scale_color_identity() +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-3)) +
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
      geom_bar(stat="identity", width=0.5, fill=data2$barrio_color) +
      geom_text(aes(label=paste0(anio_cons_medio)), position=position_dodge(width=0.9), vjust=0.3, hjust=1.5) +
      coord_flip() +
      ylab("Año") +
      xlab("Barrios") +
      ggtitle("Año medio de construcción por barrio")
    
    grafica  
  })
  
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