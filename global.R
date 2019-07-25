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
library(aTSA)
library(MASS)
library(databrew)
library(reshape2)

options(scipen = '999')
# read in prepopulated raw data 
raw_data <- read.csv('raw_data.csv', header = FALSE)

# rename columns
names(raw_data) <- c('Country', 'Year', 'Peril', 'Loss')

# read in MLE and aic data
mle_data <- read.csv('mle_output.csv')
aic_data <- read.csv('aic_dis.csv')

# read in output graph data for output (later needs to be created through simulations)
exhibit_1 <- read.csv('exhibit_1.csv')
exhibit_2_4 <- read.csv('exhibit_2_4.csv')
exhibit_3 <- read.csv('exhibit_3.csv')


# define a vector of countries and currencies to be used in the dropdown (add more later)
country <- c('Afghanistan')
currencies <- c('USD', 'Other')

# create a placeholer for other currency codes
other_currencies <- c('EUR', 'CNY')

# create a placeholder for disturbution types
basic_parametric <- c('Lognormal', 'Beta', 'Gamma', 
                      'Frechet', 'Gumbel', 'Weilbull',
                      'Poisson', ' Bernoulli')
advanced_parametric <- c('Lognormal', 'Beta', 'Gamma', 
                         'Frechet', 'Gumbel', 'Weilbull',
                         'Pareto', 'Poisson', ' Bernoulli')




# PARAMETRIC DISTRIBUTION: The user gets a default distribution but can chose from others (historical data)

