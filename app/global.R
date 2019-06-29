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

# define a vector of countries and currencies to be used in the dropdown (add more later)
countries <- c('Sri Lanka')
currencies <- c('USD', 'Other')

# create a placeholer for other currency codes
other_currencies <- c('EUR', 'CNY')

# LINEAR DETRENDING: The user is able to linearly detrend the loss data to retrospectively correct any 
# linear trend in the data by adjusting past values.
