library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(ggthemes)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(markdown)
library(dplyr)
library(shinyjqui)
library(bsplus)
library(htmltools)
library(shinyBS)

options(scipen=999)
# Read in population data, historic loss data, and simulated loss profile data
popn.data <- read.csv("../lib/RD_POP.csv",header=TRUE,stringsAsFactors=F)
archetype.data <- read.csv("../lib/RD_NNDISHistoricLosses.csv",header=TRUE,stringsAsFactors = F)
simulation.data <- read.csv("../lib/RD_NNDISSimulatedLossProfile.csv",header=TRUE,stringsAsFactors = F)
source("../lib/cost_benefit_calculations_upd.R")
source("../lib/functions.R")

# confirm the data sets have correct variable types 
popn.data$Region <- as.factor(popn.data$Region)
popn.data$Year <- as.factor(popn.data$Year)
archetype.data$Peril <- as.factor(archetype.data$Peril)
archetype.data$Year <- as.factor(archetype.data$Year)


# define a vector of countries and currencies to be used in the dropdown (add more later)
regions <- unique(popn.data$Region)
currencies <- c('USD', 'Other')

# create a placeholer for other currency codes
other_currencies <- c('EUR', 'CNY')





# PARAMETRIC DISTRIBUTION: The user gets a default distribution but can chose from others (historical data)

