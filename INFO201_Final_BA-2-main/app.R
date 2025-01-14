# Analyzing Netflix Shows Across Various Platforms 
# my app.R only runs some of the variables, but not all
library("dplyr")
library(shiny)
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)