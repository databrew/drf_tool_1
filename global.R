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

countries <- c('Afghanistan', 'Somalia', 'Malaysia', 'Senegal')

##########
# Afghanistan
##########


# read in prepopulated raw data 
raw_data_afghan <- read.csv('data/raw_data.csv', header = FALSE)

# rename columns
names(raw_data_afghan) <- c('Country', 'Year', 'Peril', 'Loss')

# read in MLE and aic data
mle_data <- read.csv('data/mle_output.csv')
aic_data <- read.csv('data/aic_dis.csv')

# read in output graph data for output (later needs to be created through simulations)
exhibit_1 <- read.csv('data/exhibit_1.csv')
exhibit_2_4 <- read.csv('data/exhibit_2_4.csv')
exhibit_3 <- read.csv('data/exhibit_3.csv')


# keep only relevant row
dat <- exhibit_1
dat <- dat[1,]
dat <- melt(dat, id.vars = 'item' )
dat$item <- NULL

# remove X from variable 
dat$variable <- gsub('X1', '1', dat$variable)
dat$variable <- gsub('_', ' ', dat$variable)
dat$variable <- Hmisc::capitalize(dat$variable)

# recode variable so that there is a break in long text
dat$variable <- ifelse(grepl('Highest', dat$variable), 
                       paste0('Highest historical, \n','annual loss 2018'),
                       ifelse(grepl('recent', dat$variable), 
                              paste0('Most recent', '\n','annual loss 2018'), 
                              dat$variable))

# reoreer vaurablke 
dat$variable <- factor(dat$variable, c('Annual avg', '1 in 5 year', 
                                       '1 in 10 year',
                                       '1 in 25 year',
                                       '1 in 50 year',
                                       '1 in 100 year', 
                                       'Highest historical, \nannual loss 2018', 
                                       'Most recent\nannual loss 2018'))






# define a vector of countries and currencies to be used in the dropdown (add more later)
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

