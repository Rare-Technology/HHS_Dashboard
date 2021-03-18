## HHS Dashboard Global ##
## By Abel Valdivia, PhD
## updated July 1 2020

#Load required libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(plyr)
library(tidyr)
library(tidyverse)
library(readr)
library(sf)
library(grid)
library(gridExtra)
library(car)
library(data.table)
library(tools)
library(Hmisc)
library(scales)
library(leaflet)
library(leafem)
library(rmarkdown)
library(plotly)
library(quantmod)


source("R/read-data.R")
source("R/table-summary.R")
source("R/utils-stats.R")
source("R/utils-plots.R")
source("R/utils-selections.R")
source("R/utils.R")

  