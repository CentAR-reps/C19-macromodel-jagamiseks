
#' Loeme sisse paketid
#' 
#' Skript loeb sisse paketid, mida on vaja 
#' mudeli jooksutamiseks ja raportite loomiseks

library(data.table)
#library(tidyverse)
library(ggplot2)
library(readxl)
#library(httr)
#library(ggthemr)
#library(networkD3)
#library(htmlwidgets)
#library(flextable)
library(shiny)
#library(DiagrammeR)
#library(DiagrammeRsvg)
#library(rsvg)
library(shinyWidgets)
library(shinybusy)

library(dplyr) # ekspordimoodul
library(tidyr) # ekspordimoodul
library(openxlsx) # ekspordimoodul

#' Esmakordne käivitamine
#' 
#' Paketi ggthemr installeerimiseks 
#' devtools::install_github('Mikata-Project/ggthemr')
