source('global.R')

# get afghan, malay, and senegal data and truncated a5 year versions
af <- raw_data_af
malay <- raw_data_malay
sen <- raw_data_sen

# mle and aic for each 
af_aic <- fitdistrplus::fitdist(af$Loss, "gamma", start=list(shape=0.5, scale=1), method="mle")$aic
af_mle <- fitdistrplus::fitdist(af$Loss, "gamma", start=list(shape=0.5, scale=1), method="mle")$estimate

malay_aic <- fitdistrplus::fitdist(malay$Loss, "gamma", start=list(shape=0.5, scale=1), method="mle")$aic
malay_mle <- fitdistrplus::fitdist(malay$Loss, "gamma", start=list(shape=0.5, scale=1), method="mle")$estimate


sen_aic <- fitdistrplus::fitdist(sen$Loss, "gamma", start=list(shape=0.5, scale=1), method="mle")$aic
sen_mle <- fitdistrplus::fitdist(sen$Loss, "gamma", start=list(shape=0.5, scale=1), method="mle")$estimate

# now get output for each data set
af_sim <- rgamma(n = 15000, shape = af_mle[1], scale = af_mle[2])
af_output <-  quantile(af_sim,c(0.5,0.8,0.9, 0.96,0.98,0.99)) 
af_output

