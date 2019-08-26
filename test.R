source('global.R')


# create a function that takes a given country and runs all distribution fits and simulations to compare to excel tools.
test_dis <- function(data, dis) {
  
  if(dis == 'gamma'){
    gamma <- fitdistrplus::fitdist(data$Loss, "gamma", start=list(shape=0.5, scale=1), method="mle")
    gamma_aic <- round(gamma$aic, 4)
    
    # get mle 
    gamma_mle <- paste0(gamma$estimate[1], ' ', gamma$estimate[2])
    
    gamma_data <- data_frame(name = 'gamma',
                             aic = gamma_aic, 
                             mle_1 = gamma$estimate[1],
                             mle_2 = gamma$estimate[2])
    
    return(gamma_data)
    
  } else if(dis == 'log_normal'){
    log_normal <-  fitdistr(data$Loss, "lognormal")
    log_normal_aic <- round(AIC(log_normal), 4)
    
    # get MLE 
    log_normal_mle <- paste0(log_normal$estimate[1], ' ', log_normal$estimate[2])
    
    # create data frame to store aic and MLEs
    log_normal_data <- data_frame(name = 'log_normal',
                                  aic = log_normal_aic, 
                                  mle_1 = log_normal$estimate[1],
                                  mle_2 = log_normal$estimate[2])
    
    return(log_normal_data)
  } else if(dis == 'beta'){
    beta <- try(eBeta_ab(data$Loss, method = "numerical.MLE"), silent = TRUE)
    beta_ll <- lBeta_ab(X = data$Loss, params = beta, logL = TRUE)
    beta_aic <- -(2*beta_ll + 2) 
    beta_mle <- c(beta$shape1, beta$shape2)
    beta_data <- data_frame(name = 'beta',
                            aic = round(beta_aic, 4), 
                            mle_1 = beta_mle[1],
                            mle_2 = beta_mle[2])
    return(beta_data)
    
  } else if(dis == 'frechet'){
    frechet<-  fitdistrplus::fitdist(data$Loss, "frechet", start=list(scale=0.1, shape=0.1), method="mle")
    frechet_aic <- round(frechet$aic, 4)
    frechet_mle <- paste0(frechet$estimate[1], ' ', frechet$estimate[2])
    
    frechet_data <- data_frame(name = 'frechet',
                               aic = frechet_aic, 
                               mle_1 = frechet$estimate[1],
                               mle_2 = frechet$estimate[2])
    
    return(frechet_data)
    
  } else if(dis == 'gumbel'){
    gumbel_fit<- fitdistrplus::fitdist(data$Loss, "gumbel", start=list(mu=0, s= 1), method="mle")
    gumbel_aic <- round(gumbel_fit$aic, 4)
    gumbel_mle <- paste0(gumbel_fit$estimate[1], ' ', gumbel_fit$estimate[2])
    
    gumbel_data <- data_frame(name = 'gumbel',
                              aic = gumbel_aic, 
                              mle_1 = gumbel_fit$estimate[1],
                              mle_2 = gumbel_fit$estimate[2])
    return(gumbel_data)
    
  } else if(dis == 'pareto'){
    pareto <- ParetoPosStable::pareto.fit(data$Loss, estim.method = 'MLE')
    pareto_aic <- round(-(2*pareto$loglik) + 2, 4)
    # get mle
    pareto_mle <- paste0(pareto$estimate[1], ' ', pareto$estimate[2])
    
    pareto_data <- data_frame(name = 'pareto',
                              aic = pareto_aic, 
                              mle_1 = pareto$estimate[[1]],
                              mle_2 = pareto$estimate[[2]])
    return(pareto_data)
    
  } else if(dis == 'weibull'){
    weibull <-  fitdistrplus::fitdist(data$Loss, "weibull", start=list(shape=0.1, scale=1), method="mle")
    weibull_aic <- round(weibull$aic, 4)
    message('weibull AIC is ', weibull_aic)
    
    # get mle
    weibull_mle <- paste0(weibull$estimate[1], ' ', weibull$estimate[2])
    message('weibull mle is ', weibull_mle)
    
    weibull_data <- data_frame(name = 'weibull',
                               aic = weibull_aic, 
                               mle_1 = weibull$estimate[1],
                               mle_2 = weibull$estimate[2])
    
    return(weibull_data)
    
  }
}

output <- function(dat, data, dis){
  if(dis == 'gamma'){
    dat_sim <- rgamma(n = 15000, shape = dat$mle_1, scale = dat$mle_2)
    output <- quantile(dat_sim,c(0.8,0.9, 0.96,0.98,0.99)) 
    annual_avg <- mean(dat_sim)
    
    # create data frame dat to store output with chart labels 
    dat <- data_frame(`Annual average` = annual_avg, 
                      `1 in 5 Years` = output[1],
                      `1 in 10 Years` = output[2],
                      `1 in 25 Years` = output[3],
                      `1 in 50 Years` = output[4],
                      `1 in 100 Years` = output[5],
                      `Highest historical annual loss` = max(data$Loss),
                      `Most recent annual loss` = data$Loss[nrow(data)])
    
  } else if(dis == 'log_normal'){
    dat_sim <- rlnorm(n = 15000, meanlog = dat$mle_1, sdlog = dat$mle_2)
    
    output <- quantile(dat_sim,c(0.8,0.9, 0.96,0.98,0.99)) 
    annual_avg <- mean(dat_sim)
    
    # create data frame dat to store output with chart labels 
    dat <- data_frame(`Annual average` = annual_avg, 
                      `1 in 5 Years` = output[1],
                      `1 in 10 Years` = output[2],
                      `1 in 25 Years` = output[3],
                      `1 in 50 Years` = output[4],
                      `1 in 100 Years` = output[5],
                      `Highest historical annual loss` = max(data$Loss),
                      `Most recent annual loss` = data$Loss[nrow(data)])
  } else if(dis == 'weibull') {
    dat_sim <- stats::rweibull(n = 15000, shape = dat$mle_1, scale = dat$mle_2)
    output <- quantile(dat_sim,c(0.8,0.9, 0.96,0.98,0.99)) 
    annual_avg <- mean(dat_sim)
    
    # create data frame dat to store output with chart labels 
    dat <- data_frame(`Annual average` = annual_avg, 
                      `1 in 5 Years` = output[1],
                      `1 in 10 Years` = output[2],
                      `1 in 25 Years` = output[3],
                      `1 in 50 Years` = output[4],
                      `1 in 100 Years` = output[5],
                      `Highest historical annual loss` = max(data$Loss),
                      `Most recent annual loss` = data$Loss[nrow(data)])
  }
}

#########
# test afghanistan data each distribution compare to excel tool
#########

# Log normal 
af_log_normal <- test_dis(data = raw_data_af, dis = 'log_normal')
af_log_normal
# from tool aic = 96.8, mle 1.95, mle 12.52

output_data <- output(dat = af_log_normal, data = raw_data_af, dis = 'log_normal')
output_data
# from tool
# avg = 3130, 1 in 5 = 137, 1 in 10 = 644, 1 in 25 = 3188, 1 in 50 = 8906, 1 in 100 = 22906


# GAMMA
af_gamma <- test_dis(data = raw_data_af, dis = 'gamma')
print(af_gamma)
# from tool aic = 93.8, mle 0.269, mle 357

# use mle to get simulation and output data
output_data <- output(dat = af_gamma, data = raw_data_af, dis = 'gamma')
output_data
# from tool
# avg = 95, 1 in 5 = 142, 1 in 10 = 284, 1 in 25 = 500, 1 in 50 = 677, 1 in 100 = 866

# Weibull
af_weibull <- test_dis(data = raw_data_af, dis = 'weibull')
print(af_weibull)
# from tool aic = 95.5, mle 0.373, mle 37

# use mle to get simulation and output data
output_data <- output(dat = af_weibull,data = raw_data_af,  dis = 'weibull')
output_data
# from tool
# avg = 252, 1 in 5 = 161, 1 in 10 = 489, 1 in 25 = 1342 1 in 50 = 2414, 1 in 100 = 3990

# Beta
af_beta <- test_dis(data = raw_data_af, dis = 'beta')
print(af_beta)
# from tool aic = 5879, mle 95, mle 376

# gumbel
af_gumbel <- test_dis(data = raw_data_af, dis = 'gumbel')
print(af_gumbel)
# from tool aic = NA, mle 91, mle 41


# pareto
af_pareto <- test_dis(data = raw_data_af, dis = 'pareto')
print(af_pareto)
# from tool aic = NA, mle 0.16, mle 0.015


#frechet
af_frechet <- test_dis(data = raw_data_af, dis = 'frechet')
print(af_frechet)
# from tool aic = NA, mle 0.24, 1.11



#########
# test malaysian data each distribution compare to excel tool
#########

# Log normal 
malay_log_normal <- test_dis(data = raw_data_malay, dis = 'log_normal')
malay_log_normal
# from tool aic = 729, mle 9.58, mle 4.02 (variance, not std)

output_data <- output(dat = malay_log_normal, data = raw_data_malay, dis = 'log_normal')
output_data
# from tool
# avg = 107,271, 1 in 5 = 77657, 1 in 10 = 186694, 1 in 25 = 462237, 1 in 50 = 827,471, 1 in 100 = 1413544


# GAMMA
malay_gamma <- test_dis(data = raw_data_malay,dis = 'gamma')
print(malay_gamma)
# from tool aic = 744, mle 0.33 , mle 339354 

# use mle to get simulation and output data
output_data <- output(dat = malay_gamma,  data = raw_data_malay,dis = 'gamma')
output_data
# from tool
# avg = 111065, 1 in 5 = 174853, 1 in 10 = 324175, 1 in 25 = 541860, 1 in 50 = 717248, 1 in 100 = 903602

# Weibull
malay_weibull <- test_dis(data = raw_data_malay, dis = 'weibull')
print(malay_weibull)
# from tool aic = 733, mle 0.47 , mle 38420 

# use mle to get simulation and output data
output_data <- output(dat = malay_weibull,  data = raw_data_malay, dis = 'weibull')
output_data
# from tool
# avg = 83066, 1 in 5 = 103460 , 1 in 10 = 218801, 1 in 25 = 432205 , 1 in 50 = 641846 , 1 in 100 = 900692 


# Beta
malay_beta <- test_dis(data = raw_data_malay, dis = 'beta')
print(malay_beta)
# from tool aic = NA, mle 19307, mle 451288

# gumbel
malay_gumbel <- test_dis(data = raw_data_malay, dis = 'gumbel')
print(malay_gumbel)
# from tool aic = NA, mle 97463, mle 28460


# pareto
malay_pareto <- test_dis(data = raw_data_malay, dis = 'pareto')
print(malay_pareto)
# from tool aic = NA, mle 0.17, mle 50


#frechet
malay_frechet <- test_dis(data = raw_data_malay, dis = 'frechet')
print(malay_frechet)
# from tool aic = NA, mle 0.41, 5061
