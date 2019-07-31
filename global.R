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


##########
# Afghanistan
##########

# read in prepopulated raw data 
raw_data_af <- read.csv('data/Afghanistan/raw_data_all.csv', header = FALSE)

# rename columns
names(raw_data_af) <- c('Country', 'Year', 'Peril', 'Loss')

# read in MLE and aic data
mle_af <- read.csv('data/Afghanistan/mle.csv')
aic_af <- read.csv('data/Afghanistan/aic.csv')

# read in output graph data for output (later needs to be created through simulations)
exhibit_1_af <- read.csv('data/Afghanistan/exhibit_1.csv')

# keep only relevant row
exhibit_1_af <- exhibit_1_af[1,]
exhibit_1_af <- melt(exhibit_1_af, id.vars = 'item' )
exhibit_1_af$item <- NULL

# remove X from variable 
exhibit_1_af$variable <- gsub('X1', '1', exhibit_1_af$variable)
exhibit_1_af$variable <- gsub('_', ' ', exhibit_1_af$variable)
exhibit_1_af$variable <- Hmisc::capitalize(exhibit_1_af$variable)

# recode variable so that there is a break in long text
exhibit_1_af$variable <- ifelse(grepl('Highest', exhibit_1_af$variable), 
                                paste0('Highest historical, \n','annual loss 2018'),
                                ifelse(grepl('recent', exhibit_1_af$variable), 
                                       paste0('Most recent', '\n','annual loss 2018'), 
                                       exhibit_1_af$variable))


# reoreer vaurablke 
exhibit_1_af$variable <- factor(exhibit_1_af$variable, c('Annual avg', '1 in 5 year', 
                                                         '1 in 10 year',
                                                         '1 in 25 year',
                                                         '1 in 50 year',
                                                         '1 in 100 year', 
                                                         'Highest historical, \nannual loss 2018', 
                                                         'Most recent\nannual loss 2018'))

# get exhibit 2 from combined data
exhibit_2_4_af <- read.csv('data/Afghanistan/exhibit_2_4.csv')
exhibit_2_af <- exhibit_2_4_af[,c('percent', 'funding_gap')]

# scale back to plot data
exhibit_2_af$funding_gap <- exhibit_2_af$funding_gap/1000000

# get exhibit 4 from combined data
exhibit_4_af <- exhibit_2_4_af[,c('percent', 'graph')]

# scale back to plot data
exhibit_4_af$graph <- exhibit_4_af$graph/1000000

# read in exhibit 3
exhibit_3_af <- read.csv('data/Afghanistan/exhibit_3.csv')

# clean exhibit_3_afa
exhibit_3_af <- exhibit_3_af[1,]

# rescale 
names(exhibit_3_af) <- c('Drought', 'Annual Average', 'Severe', 'Extreme')
exhibit_3_af <- melt(exhibit_3_af, id.vars = 'Drought')
exhibit_3_af$Drought <- NULL
exhibit_3_af$value <- exhibit_3_af$value/1000000

# read in gamma simulations
sim_af <- read.csv('data/Afghanistan/gamma_simulations.csv')

# recode variables 
sim_af$X <- NULL
names(sim_af) <- c('unfts', 'people_based')

# store data in list, starting with raw, mle, aic, simulation, and exhibits 1 through 4
afghanistan_data <- list('raw_data_af' = raw_data_af, 
                         'mle_af'= mle_af,
                         'aic_af' = aic_af,
                         'sim_af' = sim_af,
                         'exhibit_1_af' = exhibit_1_af,
                         'exhibit_2_af' = exhibit_2_af,
                         'exhibit_3_af' = exhibit_3_af,
                         'exhibit_4_af' = exhibit_4_af)

rm(raw_data_af,
   mle_af,
   aic_af,
   sim_af,
   exhibit_1_af,
   exhibit_2_af,
   exhibit_3_af,
   exhibit_4_af,
   exhibit_2_4_af)


##########
# Somalia
##########

# read in prepopulated raw data 
raw_data_som <- read.csv('data/Somalia/raw_data_all.csv', header = TRUE)

# rename columns
names(raw_data_som) <- c('Country', 'Year', 'Peril', 'Loss')

# read in MLE and aic data
mle_som <- read.csv('data/Somalia/mle.csv')
aic_som <- read.csv('data/Somalia/aic.csv')

# read in output graph data for output (later needs to be created through simulations)
exhibit_1_som <- read.csv('data/Somalia/exhibit_1.csv')

# keep only relevant row
exhibit_1_som <- exhibit_1_som[1,]
exhibit_1_som <- melt(exhibit_1_som, id.vars = 'X')
exhibit_1_som$X <- NULL

# remove X from variable 
exhibit_1_som$variable <- gsub('X1', '1', exhibit_1_som$variable)
exhibit_1_som$variable <- gsub('.', ' ', exhibit_1_som$variable, fixed = TRUE)
exhibit_1_som$variable <- gsub('  ', ' ', exhibit_1_som$variable, fixed = TRUE)
exhibit_1_som$variable <- trimws(exhibit_1_som$variable, which = 'both')

# recode to make Annual Average to Annaual avg
exhibit_1_som$variable <- gsub('Average', 'avg', exhibit_1_som$variable)

# recode variable so that there is a break in long text
exhibit_1_som$variable <- ifelse(grepl('Highest', exhibit_1_som$variable), 
                                paste0('Highest historical, \n','annual loss 2011'),
                                ifelse(grepl('Recent', exhibit_1_som$variable), 
                                       paste0('Most recent', '\n','annual loss 2018'), 
                                       exhibit_1_som$variable))

# reoreer vaurablke 
exhibit_1_som$variable <- factor(exhibit_1_som$variable, c('Annual avg', '1 in 5 Year', 
                                                         '1 in 10 Year',
                                                         '1 in 25 Year',
                                                         '1 in 50 Year',
                                                         '1 in 100 Year', 
                                                         'Highest historical, \nannual loss 2011', 
                                                         'Most recent\nannual loss 2018'))

exhibit_1_som$value <- exhibit_1_som$value/1000000

# get exhibit 2 from combined data
exhibit_2_4_som <- read.csv('data/Somalia/exhibit_2_4.csv')
names(exhibit_2_4_som) <- c('percent', 'drought','drought_1', 'drought_2','drought_3', 'graph', 'funding_gap')
exhibit_2_som <- exhibit_2_4_som[,c('percent', 'funding_gap')]

# scale back to plot data
exhibit_2_som$funding_gap <- exhibit_2_som$funding_gap/1000000

# get exhibit 4 from combined data
exhibit_4_som <- exhibit_2_4_som[,c('percent', 'graph')]

# scale back to plot data
exhibit_4_som$graph <- exhibit_4_som$graph/1000000

# read in exhibit 3
exhibit_3_som <- read.csv('data/Somalia/exhibit_3.csv')

# clean exhibit_3_soma
exhibit_3_som <- exhibit_3_som[1,]

# rescale 
names(exhibit_3_som) <- c('Drought', 'Annual Average', 'Severe', 'Extreme')
exhibit_3_som <- melt(exhibit_3_som, id.vars = 'Drought')
exhibit_3_som$Drought <- NULL
exhibit_3_som$value <- exhibit_3_som$value/1000000

# read in gamma simulations
sim_som <- read.csv('data/Somalia/gamma_simulation.csv')

# rename variables 
sim_som <- as.data.frame(sim_som[, c('Aggregate.Annual.Cost')])
names(sim_som) <- 'aggregate_annual_cost'

# store data in list, starting with raw, mle, aic, simulation, and exhibits 1 through 4
somalia_data <- list('raw_data_som' = raw_data_som, 
                         'mle_som'= mle_som,
                         'aic_som' = aic_som,
                         'sim_som' = sim_som,
                         'exhibit_1_som' = exhibit_1_som,
                         'exhibit_2_som' = exhibit_2_som,
                         'exhibit_3_som' = exhibit_3_som,
                         'exhibit_4_som' = exhibit_4_som)

rm(raw_data_som,
   mle_som,
   aic_som,
   sim_som,
   exhibit_1_som,
   exhibit_2_som,
   exhibit_3_som,
   exhibit_4_som,
   exhibit_2_4_som)



##########
# malaysia
##########

# read in prepopulated raw data 
raw_data_malay <- read.csv('data/Malaysia/raw_data_flood.csv', header = TRUE)

# rename columns
names(raw_data_malay) <- c('Country', 'Year', 'Peril', 'Loss')

# read in MLE and aic data
mle_malay <- read.csv('data/Malaysia/mle.csv')
aic_malay <- read.csv('data/Malaysia/aic.csv')

# read in output graph data for output (later needs to be created through simulations)
exhibit_1_malay <- read.csv('data/Malaysia/exhibit_1.csv', header = TRUE)


# keep only relevant row
exhibit_1_malay <- exhibit_1_malay[1,]
exhibit_1_malay <- melt(exhibit_1_malay, id.vars = 'X')
exhibit_1_malay$X <- NULL

# remove X from variable 
exhibit_1_malay$variable <- gsub('X1', '1', exhibit_1_malay$variable)
exhibit_1_malay$variable <- gsub('.', ' ', exhibit_1_malay$variable, fixed = TRUE)
exhibit_1_malay$variable <- gsub('  ', ' ', exhibit_1_malay$variable, fixed = TRUE)
exhibit_1_malay$variable <- trimws(exhibit_1_malay$variable, which = 'both')

# recode variable so that there is a break in long text
exhibit_1_malay$variable <- ifelse(grepl('Highest', exhibit_1_malay$variable), 
                                 paste0('Highest historical, \n','annual loss'),
                                 ifelse(grepl('Recent', exhibit_1_malay$variable), 
                                        paste0('Most recent', '\n','annual loss'), 
                                        exhibit_1_malay$variable))

# recode to make Annual Average to Annaual avg
exhibit_1_som$variable <- gsub('Average', 'avg', exhibit_1_som$variable)


# reoreer vaurablke 
exhibit_1_som$variable <- factor(exhibit_1_som$variable, c('Long term avg', '1 in 5 Year', 
                                                           '1 in 10 Year',
                                                           '1 in 25 Year',
                                                           '1 in 50 Year',
                                                           '1 in 100 Year', 
                                                           'Highest historical, \nannual loss 2018', 
                                                           'Most recent\nannual loss 2018'))

exhibit_1_malay$value <- exhibit_1_malay$value/1000000

# get exhibit 2 from combined data
exhibit_2_4_malay <- read.csv('data/Malaysia/exhibit_2_4.csv')
names(exhibit_2_4_malay) <- c('percent', 'flood','flood_1', 'flood_2','flood_3', 'graph', 'funding_gap')
exhibit_2_malay <- exhibit_2_4_malay[,c('percent', 'funding_gap')]

# scale back to plot data
exhibit_2_malay$funding_gap <- exhibit_2_malay$funding_gap/1000000

# get exhibit 4 from combined data
exhibit_4_malay <- exhibit_2_4_malay[,c('percent', 'graph')]

# scale back to plot data
exhibit_4_malay$graph <- exhibit_4_malay$graph/1000000

# read in exhibit 3
exhibit_3_malay <- read.csv('data/Malaysia/exhibit_3.csv')

# clean exhibit_3_malaya
exhibit_3_malay <- exhibit_3_malay[1,]

# rescale 
names(exhibit_3_malay) <- c('Drought', 'Annual Average', 'Severe', 'Extreme')
exhibit_3_malay <- melt(exhibit_3_malay, id.vars = 'Drought')
exhibit_3_malay$Drought <- NULL
exhibit_3_malay$value <- exhibit_3_malay$value/1000000

# read in gamma simulations
sim_malay <- read.csv('data/Malaysia/gamma_simulations.csv')

# rename variables 
sim_malay <- as.data.frame(sim_malay[, c('Aggregate.Annual.Cost')])
names(sim_malay) <- 'aggregate_annual_cost'

# store data in list, starting with raw, mle, aic, simulation, and exhibits 1 through 4
malaysia_data <- list('raw_data_malay' = raw_data_malay, 
                     'mle_malay'= mle_malay,
                     'aic_malay' = aic_malay,
                     'sim_malay' = sim_malay,
                     'exhibit_1_malay' = exhibit_1_malay,
                     'exhibit_2_malay' = exhibit_2_malay,
                     'exhibit_3_malay' = exhibit_3_malay,
                     'exhibit_4_malay' = exhibit_4_malay)

rm(raw_data_malay,
   mle_malay,
   aic_malay,
   sim_malay,
   exhibit_1_malay,
   exhibit_2_malay,
   exhibit_3_malay,
   exhibit_4_malay,
   exhibit_2_4_malay)



##########
# senegal
##########

# read in prepopulated raw data 
raw_data_sen <- read.csv('data/Senegal/raw_data_flood.csv', header = TRUE)

# rename columns
names(raw_data_sen) <- c('Country', 'Year', 'Peril', 'Loss')

# read in MLE and aic data
mle_sen <- read.csv('data/Senegal/mle.csv')
aic_sen <- read.csv('data/Senegal/aic.csv')

# read in output graph data for output (later needs to be created through simulations)
exhibit_1_sen <- read.csv('data/Senegal/exhibit_1.csv', header = TRUE)

# keep only relevant row
exhibit_1_sen <- exhibit_1_sen[1,]
exhibit_1_sen <- melt(exhibit_1_sen, id.vars = 'X')
exhibit_1_sen$X <- NULL

# remove X from variable 
exhibit_1_sen$variable <- gsub('X1', '1', exhibit_1_sen$variable)
exhibit_1_sen$variable <- gsub('.', ' ', exhibit_1_sen$variable, fixed = TRUE)
exhibit_1_sen$variable <- gsub('  ', ' ', exhibit_1_sen$variable, fixed = TRUE)
exhibit_1_sen$variable <- trimws(exhibit_1_sen$variable, which = 'both')

# recode variable so that there is a break in long text
exhibit_1_sen$variable <- ifelse(grepl('Highest', exhibit_1_sen$variable), 
                                   paste0('Highest historical, \n','annual loss 1992'),
                                   ifelse(grepl('Recent', exhibit_1_sen$variable), 
                                          paste0('Most recent', '\n','annual loss 2018'), 
                                          exhibit_1_sen$variable))

# recode to make Annual Average to Annaual avg
exhibit_1_sen$variable <- gsub('Average', 'avg', exhibit_1_sen$variable)


# reoreer vaurablke 
exhibit_1_sen$variable <- factor(exhibit_1_sen$variable, c('Annual avg', '1 in 5 Year', 
                                                               '1 in 10 Year',
                                                               '1 in 25 Year',
                                                               '1 in 50 Year',
                                                               '1 in 100 Year', 
                                                               'Highest historical, \nannual loss 1992', 
                                                               'Most recent\nannual loss 2018'))

exhibit_1_sen$value <- exhibit_1_sem$value/1000000

# get exhibit 2 from combined data
exhibit_2_4_sen <- read.csv('data/Senegal/exhibit_2_4.csv')
names(exhibit_2_4_sen) <- c('percent', 'flood','flood_1', 'flood_2','flood_3', 'graph', 'funding_gap')
exhibit_2_sen <- exhibit_2_4_sen[,c('percent', 'funding_gap')]

# scale back to plot data
exhibit_2_sen$funding_gap <- exhibit_2_sen$funding_gap/1000000

# get exhibit 4 from combined data
exhibit_4_sen <- exhibit_2_4_sen[,c('percent', 'graph')]

# scale back to plot data
exhibit_4_sen$graph <- exhibit_4_sen$graph/1000000

# read in exhibit 3
exhibit_3_sen <- read.csv('data/Senegal/exhibit_3.csv')

# clean exhibit_3_sena
exhibit_3_sen <- exhibit_3_sen[1,]

# rescale 
names(exhibit_3_sen) <- c('Drought', 'Annual Average', 'Severe', 'Extreme')
exhibit_3_sen <- melt(exhibit_3_sen, id.vars = 'Drought')
exhibit_3_sen$Drought <- NULL
exhibit_3_sen$value <- exhibit_3_sen$value/1000000

# read in gamma simulations
sim_sen <- read.csv('data/Senegal/gamma_simulations.csv')

# rename variables 
sim_sen <- as.data.frame(sim_sen[, c('Aggregate.Annual.Cost')])
names(sim_sen) <- 'aggregate_annual_cost'

# store data in list, starting with raw, mle, aic, simulation, and exhibits 1 through 4
Senegal_data <- list('raw_data_sen' = raw_data_sen, 
                      'mle_sen'= mle_sen,
                      'aic_sen' = aic_sen,
                      'sim_sen' = sim_sen,
                      'exhibit_1_sen' = exhibit_1_sen,
                      'exhibit_2_sen' = exhibit_2_sen,
                      'exhibit_3_sen' = exhibit_3_sen,
                      'exhibit_4_sen' = exhibit_4_sen)

rm(raw_data_sen,
   mle_sen,
   aic_sen,
   sim_sen,
   exhibit_1_sen,
   exhibit_2_sen,
   exhibit_3_sen,
   exhibit_4_sen,
   exhibit_2_4_sen)
# PARAMETRIC DISTRIBUTION: The user gets a default distribution but can chose from others (historical data)

