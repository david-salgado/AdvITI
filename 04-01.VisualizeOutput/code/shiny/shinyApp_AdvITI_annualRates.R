# Cargar paquetes                        ####
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(ggthemes)
library(data.table)
library(gridExtra)
library(ggh4x)
library(latex2exp)

# Definir ruta                            ####
# Ejecutar solo en local las siguientes dos líneas
# path_shiny <- 'N:/UDMTD/UDTMDCOM/AdvITI_github/04-01.VisualizeOutput/code/shiny'
# setwd(path_shiny)


# Cargar scripts ui y server              ####
source(file.path("ui_annual_rates.R"))
source(file.path("server_annual_rates.R"))

# Ejecutar Shiny                          ####
shinyApp(ui = ui, server = server)


