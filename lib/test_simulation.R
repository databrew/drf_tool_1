library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggthemes)

# source global 
source('global.R')

#########
# frequency analysis with Afghan data
#########

# get raw data
dat <- afghanistan_data[[1]]

# remove first row (2000) so that we have 15 years of data
dat <- dat[-1,]

# get mle for data - number of "success" over trils
num_success <- 9
num_frequency <- 15
mle_bernoulli <- num_success/num_frequency


# use a uniform distribution, with each 1000k samples representing a year. 
uniform <- runif(n = 15000, min = 0, max = 1)

# combine uniform and 1:15000
sim_freq_data <- as.data.frame(cbind(simulation_num = 1:15000, uniform = uniform))

# create a pass or fail vector
sim_freq_data$outcome <- ifelse(sim_freq_data$uniform < mle_bernoulli, 'success', 'fail')

# create a year vector based on 1 through every 1k (15 years total)
sim_freq_data$year <- rep(1:15, each=1000)

# group by year and sum by success and fail
temp <- sim_freq_data %>% 
  group_by(year) %>%
  summarise(sum_success = sum(outcome == 'success'),
            sum_failure = sum(outcome == 'fail'),
            counts = n(),
            percent_success = round(sum_success/counts, 2))

# ge


# test for best distribution using aic 


log_normal <- fitdistr(dat$Loss, "lognormal")
