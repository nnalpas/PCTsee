


### List of required packages --------------------------------------------

# Start fresh
rm(list = ls())

#setwd("C:/Users/kxmna01/Documents/GitHub/PCTsee/R/")
setwd("C:/Users/nalpa/Documents/GitHub/PCTsee/R/")
#setwd("./R/")

# Source the user's general functions used across this script
source(file = "helpers.R")

# Load the required packages (or install if not already in library)
library(magrittr)
library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggpubr)
library(gplots)
library(dendextend)
library(mgcv)
library(plotly)
library(DT)
library(shinydashboard)
library(shinydashboardPlus)
library(shiny)

# Increase the size limit for file upload
#options(shiny.maxRequestSize = 2000*1024^2)



### Render as a shiny APP ------------------------------------------------

# Source the UI and server scripts
source(
    file = paste0(
        "ui.R"))
source(
    file = paste0(
        "server.R"))

# Make a shiny APP
app <- shinyApp(ui, server)
#runApp(app)



### END ------------------------------------------------------------------


