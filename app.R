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

stDistritos <- data %>%
  group_by(distrito_cod) %>%
  slice(1) %>%
  arrange(distrito_nombre)
  
vDistrictos <- as.vector(stDistritos$distrito_nombre)


ui <- dashboardPage(
  dashboardHeader(title = "Grupo N"),
  dashboardSidebar(
    checkboxGroupInput(inputId = "distrito", 
                       label = "Distritos:",
                      choices =  vDistrictos,
                      selected = c("Arganzuela")
                      )
  ),
  dashboardBody(
    fluidRow(
      infoBoxOutput("inmueblesBario"),
      infoBoxOutput("anioMedio"),
      infoBoxOutput("valorMedio")
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
        filter(data$distrito_nombre %in% input$distrito) %>%
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
        filter(data$distrito_nombre %in% input$distrito) %>%
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
        filter(data$distrito_nombre %in% input$distrito) %>%
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
      filter(data$distrito_nombre %in% input$distrito) %>%
      group_by(barrio_nombre) %>% 
      summarise(total = sum(inmuebles_barrio)) %>% 
      arrange(desc(total))
    
    grafica <- ggplot(data=data2, aes(x=reorder(barrio_nombre, total), y=total)) + 
      geom_bar(stat="identity", width=0.5, fill="steelblue") +
      coord_flip() +
      geom_text(aes(label=total), position=position_dodge(width=0.9), vjust=0.3, hjust=-0.5) +
      ylab("Cantidad de Inmuebles") +
      xlab("Barrios") +
      ggtitle("Innmuebles por Barrio") +
      theme_minimal()
    
    grafica
  })
  
  # Gráfica 2
  output$plot2 <- renderPlot({
    data2 <- data %>%
      filter(data$distrito_nombre %in% input$distrito) %>%
      group_by(barrio_nombre) %>%
      summarise(total = sum(val_cat_barrio)) %>%
      arrange(desc(total))
    
    grafica <- ggplot(data=data2, aes(x=reorder(barrio_nombre, total), y=total)) +
      geom_bar(stat="identity", width=0.5, fill="steelblue") +
      geom_text(aes(label=paste0("€ ", format(round(total/1e6, 1), trim=TRUE), "M")), position=position_dodge(width=0.9), vjust=0.3, hjust=-0.2) +
      coord_flip() +
      ylab("Valor Catastro") +
      xlab("Barrios") +
      ggtitle("Valor Catastro por Barrio") +
      theme_minimal()
    
    grafica
  })
  
  # Tabla
  output$tablaDatos <- renderDT({
    data2 <- data %>%
      filter(data$distrito_nombre %in% input$distrito) %>%
      arrange(distrito_nombre)
    
    datatable(data2)
  })
}

shinyApp(ui, server)