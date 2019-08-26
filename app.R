source('global.R')


ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = "DRF Tool 1"),
                    dashboardSidebar(
                      sidebarMenu( id = 'menu',
                                   menuItem("Overview", tabName = 'overview', icon = icon('home')),
                                   menuItem("Assumptions & Methodology", tabName = 'methodology', icon = icon('info')),
                                   menuItem("User inputs", tabName = 'parameters', icon = icon('edit')),
                                   menuItem("Data", tabName = 'data', icon = icon('table')),
                                   menuItem("Simulations", tabName = 'simulations', icon = icon('cog')),
                                   menuItem("Output", tabName = 'output', icon = icon('signal')),
                                   actionButton("simulate", "Run Tool", icon('refresh'), style = "color: #fff;background-color: #337ab7; border-color: #2e6da4", width="70%"),
                                   br(),
                                   br(),
                                   awesomeCheckbox("advanced", "Advanced Settings", FALSE,status = 'danger')
                                   # conditionalPanel("input.advanced",
                                   #                  textInput('nndisfac',label= 'NNDIS Factor (%)', value = "2.5", width = "60%")) 
                      )),
                    dashboardBody( 
                      tabItems( 
                        tabItem(tabName = 'overview',
                                br(),
                                img(src="DRFIP_Logo.png", height = 50, width = 100, align = 'top'),
                                br("  "),
                                h2("Disaster Risk Financing"),
                                tags$hr(), 
                                h3("Overview"),
                                p(),
                                p(), 
                                br(),
                                h3("Authorship"),
                                p("The development of this Tool was led by the Disaster Risk Financing and Insurance 
                                  Program (DRFIP), a partnership of the World Bank Group's Finance Competitiveness and 
                                  Innovation Global Practice and the Global Facility for Disaster Reduction and Recovery 
                                  (GFDRR).", style = "font-size:100%"), 
                                br(),
                                br(),
                                h3("Disclaimer"),
                                
                                p(),
                                br(), 
                                h3("Confidentiality"),
                                p("The World Bank invests substantial resources in the development of its models, 
                                  modelling methodologies and databases. This Tool contains proprietary and confidential 
                                  information and is intended for the exclusive use of World Bank partners with whom this 
                                  Tool has been shared. Any user is subject to the restrictions of the confidentiality 
                                  provisions set forth in license and other nondisclosure agreements.", 
                                  style = "font-size:100%"), br(),
                                br()
                                ), # end tabItem
                        
                        tabItem(tabName = 'methodology'), # end tabItem
                        tabItem(tabName = 'parameters',
                                fluidRow(
                                  column(6, 
                                         radioButtons('amend_upload', # option to use preloaded data or amend preloaded data, or upload own data
                                                      'Choose a data source',
                                                      choices = c('Use preloaded data',
                                                                  'Amend preloaded data',
                                                                  'Upload data'),
                                                      selected = 'Use preloaded data',
                                                      inline = TRUE)),
                               
                                column(7,
                                       fluidRow(
                                         column(6, 
                                                radioButtons('damage_type', # If cost per person, must specify the amount. Otherwise it's monetary loss data
                                                             'Data type',
                                                             choices = c('Total damage', 'Cost per person'),
                                                             selected = 'Total damage',
                                                             inline = TRUE)),
                                         column(6,
                                                uiOutput('cost_per_person'))
                                         
                                       ),
                                       fluidRow(
                                         column(6, 
                                                selectInput('country',
                                                            'Choose a country',
                                                            choices = countries,
                                                            selected = countries[1])),
                                         column(6,
                                                radioButtons('currency',
                                                             'Choose a currency',
                                                             choices = currencies,
                                                             selected = 'USD'))
                                       ),
                                       fluidRow(
                                         column(6, 
                                                uiOutput('code')),
                                         column(6,
                                                uiOutput('rate')),
                                         column(6,
                                                textOutput('display_rate'))
                                       ),
                                       box(title = '',
                                           width = 12,
                                         fluidRow(
                                           column(12,
                                                  DT::dataTableOutput('data_table_peril'))), 
                                       
                                       fluidRow(
                                         column(12,
                                                uiOutput('upload_data')) # action button to upload user data
                                       ))
                                  
                                ),
                                column(5,
                                       align = 'center',
                                       plotOutput('advanced_detrend_plot'))) # not currently in use
                        ), # end tabItem
                        tabItem(tabName = 'data',
                                fluidRow(
                                  h3('There is currently no data to scale by for the country selected'),
                                  column(6,
                                         uiOutput('scaling')), # not currently in use
                                  column(6, 
                                         uiOutput('detrend')) # not currently in use
                                  
                                ),
                                fluidRow(
                                  box(
                                    title = 'Data',
                                    width = 12,
                                    status = 'primary',
                                  column(12,
                                         plotOutput('data_plot'))
                                ))
                                ), # end tabItem
                        
                        tabItem(tabName = 'simulations',
                                fluidRow(
                                  column(6,
                                         uiOutput('prob_dis')) # advanced user only
                                ),
                                fluidRow(
                                  br(),
                          
                                  column(12,
                                         box(
                                           title = 'AIC',
                                           width = 12,
                                           status = 'primary',
                                         DT::dataTableOutput('aic_table')))
                                ),
                                fluidRow(
                                  column(6,
                                         box(
                                           title = 'Peril data',
                                           width = 12,
                                           status = 'primary',
                                           plotOutput('hist_plot'))),
                                  column(6,
                                         box(
                                           title = 'Simulation data',
                                           width = 12,
                                           status = 'primary',
                                           plotOutput('sim_plot')))
                                ),
                                fluidRow(
                                  column(6,
                                         plotOutput('rag_ratings'))
                                ),
                                fluidRow(
                                  column(6,
                                         plotOutput('dist_plot')),
                                  column(6,
                                         plotOutput('grouped_plot'))
                                )), # end tab iten
                        tabItem(tabName = 'output', # end tabItem
                             fluidRow(
                               box(title = 'Estimated Average Annual Loss', 
                                   width = 6,
                                   status = 'danger', # success= green, info = lighterblue, warning = orange, 
                                   solidHeader = TRUE,
                               column(12,
                                      plotOutput('annual_loss'))),
                               box(title = 'Loss Exceedance Curve', 
                                   width = 6,
                                   status = 'danger', # success= green, info = lighterblue, warning = orange, 
                                   solidHeader = TRUE,
                               column(12,
                                      plotOutput('loss_exceedance')))
                               ),
                             fluidRow(
                               box(title = 'Estimated Avg Annual Loss', 
                                   width = 6,
                                   status = 'danger', # success= green, info = lighterblue, warning = orange, 
                                   solidHeader = TRUE,
                                   column(12,
                                          plotOutput('annual_loss_gap'))),
                               box(title = 'Loss Exceedance Curve of funding gap', 
                                   width = 6,
                                   status = 'danger', # success= green, info = lighterblue, warning = orange, 
                                   solidHeader = TRUE,
                                   column(12,
                                          plotOutput('loss_exceedance_gap')))
                             )
                             
                        ))
                   )
)

server <- function(input, output) {
  
 ###  OUTPUT page
  
  # get a reactive object that selects country data (list) based on imput
  selected_country <- reactive({
    
    country_name <- input$country
    
    if(country_name == 'Afghanistan'){
      country_data <- raw_data_af
    } else if (country_name == 'Malaysia'){
      country_data <- raw_data_malay
    } else if (country_name == 'Senegal'){
      country_data <- raw_data_sen
    } else {
      country_data <- raw_data_som
    }
    return(country_data)
  })
  
  # DONT DO ANYTHING WITH BERNOULLI UNTILL YOU GET MORE INFO
  # # The basic user will not see this, only the advanced user
  # frequency_distribution_bernoulli <- reactive({
  #   # will have other options for different scales later
  #   freq_data <- scale_by_pop()
  #   # temporarily do simulation 
  #   freq_data <- freq_data[complete.cases(freq_data),]
  #   # sum of success (disaster) over sum if trials (years). 6 success in 8 years
  #   # get trials max year minus min year
  #   num_trials <- as.numeric(as.character(max(freq_data$Year))) - min(as.numeric(as.character(freq_data$Year)))
  #   num_trials <- num_trials + 1
  #   mle_bern <- sum(nrow(freq_data)/num_trials)
  #   uniform_dis <- runif(1000, 0, 1)
  #   sim_freq_data <- as.data.frame(cbind(simulation_num = 1:1000, uniform_dis = uniform_dis))
  #   # create a variable to show success (mle?uniform_dis, then success)
  #   sim_freq_data$outcome <- ifelse(sim_freq_data$uniform_dis < mle_bern, 'success', 'fail')
  #   
  # })
  # 
  # DONT USE YET

  # 
  # get_poisson({
  #   # poisson <- fitdistr(data$Loss, "Poisson")
  #   # poisson_aic <- AIC(poisson)
  #   
  # })
  
  # HERE: MAKE SURE YOU HAVE THE RIGHT MLES BEFORE SIMULATIONS, GAMMA NEEDS AN INVERSE AND ONE OTHER NEEDS A SQUARED, BOTH FROM SECOND MLE
  # 
  # CREATE A REACTIVE OBJECT THAT GETS AIC SCORES FOR EACH PARAMETRIC LOSS DISTRIBUTION
  get_aic_mle <- reactive({
    
    # get country data
    data  <- selected_country()
    
    ##########
    # fit lognormal
    #########
    log_normal <- try(fitdistr(data$Loss, "lognormal"),silent = TRUE)
    if(class(log_normal) == 'try-error'){
      log_normal <- NULL
      log_normal_aic <- NA
      log_normal$estimate[1] <- NA
      log_normal$estimate[2] <- NA
      
    } else {
      # get aic
      log_normal_aic <- round(AIC(log_normal), 4)
      
      # if there is an error, fill object with NA
      message('log normal AIC is ', log_normal_aic)
     
      # get MLE 
      log_normal_mle <- paste0(log_normal$estimate[1], ' ', log_normal$estimate[2])
      message('log normal mle is ', log_normal_mle)
    }
      # create data frame to store aic and MLEs
      log_normal_data <- data_frame(name = 'log_normal',
                                    aic = log_normal_aic, 
                                    mle_1 = log_normal$estimate[1],
                                    mle_2 = log_normal$estimate[2])
      
    
   
    # fit beta (only one not replicating)
    # normalize data to 0, 1
    beta <- try(eBeta_ab(data$Loss, method = "numerical.MLE"), silent = TRUE)
    if(class(beta) == 'try-error'){
      beta <- NULL
      beta_aic <- NA
      beta$shape1 <- NA
      beta$shape2 <- NA
      beta_mle <- c(beta$shape1, beta$shape2)
    } else {
      beta_ll <- lBeta_ab(X = data$Loss, params = beta, logL = TRUE)
      beta_aic <- -(2*beta_ll + 2) 
      beta_mle <- c(beta$shape1, beta$shape2)
      
      # beta_aic <- round(beta$aic, 4)
      message('beta AIC is ', beta_aic)
      message('beta mle is ', beta_mle)
    }
      beta_data <- data_frame(name = 'beta',
                              aic = round(beta_aic, 4), 
                              mle_1 = beta_mle[1],
                              mle_2 = beta_mle[2])
      
    
   
    # EQUATION FOR AIC 
    # -2*loglikihood + k*npar, where k is generally 2 and npar is number of parameters in the model.

    # fit gamma
    # gamma <- fitdistr(data$Loss, 'gamma')
    gamma <- try(fitdistrplus::fitdist(data$Loss, "gamma", start=list(shape=0.5, scale=1), method="mle"), silent = TRUE)
    
    if(class(gamma) == 'try-error'){
      gamma <- NULL
      gamma_aic <- NA
      gamma$estimate[1] <- NA
      gamma$estimate[2] <- NA
      
    } else {
      # get aic
      gamma_aic <- round(gamma$aic, 4)
      message('gamma AIC is ', gamma_aic)
      
      # get mle 
      gamma_mle <- paste0(gamma$estimate[1], ' ', gamma$estimate[2])
      message('gamme mle is ', gamma_mle)
    }
      gamma_data <- data_frame(name = 'gamma',
                               aic = gamma_aic, 
                               mle_1 = gamma$estimate[1],
                               mle_2 = gamma$estimate[2])
    
    
    
    # fit frechet
    # dfrechet(data$Loss, lambda = 1, mu = 1, sigma = 1, log = TRUE)
    frechet <- try(fitdistrplus::fitdist(data$Loss, "frechet", start=list(scale=0.1, shape=0.1), method="mle"), 
                   silent = TRUE)
    if(class(frechet) == 'try-error'){
      frechet <- NULL
      frechet_aic <- NA
      frechet$estimate[1] <- NA
      frechet$estimate[2] <- NA
      
    } else {
      frechet_aic <- round(frechet$aic, 4)
      message('frechet AIC is ', frechet_aic)
      # get mle 
      frechet_mle <- paste0(frechet$estimate[1], ' ', frechet$estimate[2])
      message('frechet mle is ', frechet_mle) 
      }
      frechet_data <- data_frame(name = 'frechet',
                                 aic = frechet_aic, 
                                 mle_1 = frechet$estimate[1],
                                 mle_2 = frechet$estimate[2])
      
    
    
    # git gumbel
    gumbel_fit <- try(fit_gumbel(data$Loss), silent = TRUE)
    if(class(gumbel_fit) == 'try-error'){
      gumbel_fit <- NULL
      gumbel_aic <- NA
      gumbel_fit$estimate[1] <- NA
      gumbel_fit$estimate[2] <- NA
      
    } else {
      gumbel_aic <- round(gumbel_fit$aic, 4)
      message('gumbel AIC is ', gumbel_aic)
      # get mle
      gumbel_mle <- paste0(gumbel_fit$estimate[1], ' ', gumbel_fit$estimate[2])
      message('gumbel mle is ', gumbel_mle)
    }
      gumbel_data <- data_frame(name = 'gumbel',
                                aic = gumbel_aic, 
                                mle_1 = gumbel_fit$estimate[1],
                                mle_2 = gumbel_fit$estimate[2])
      
    
    
    # fit weibull
    weibull <- try(fitdistrplus::fitdist(data$Loss, "weibull", start=list(shape=0.1, scale=1), method="mle"), silent = TRUE)
    if(class(weibull) == 'try-error'){
      weibull <- NULL
      weibull_aic <- NA
      weibull$estimate[1] <- NA
      weibull$estimate[2] <- NA
      
    } else {
      weibull_aic <- round(weibull$aic, 4)
      message('weibull AIC is ', weibull_aic)
    
      # get mle
      weibull_mle <- paste0(weibull$estimate[1], ' ', weibull$estimate[2])
      message('weibull mle is ', weibull_mle)
    }
      weibull_data <- data_frame(name = 'weibull',
                                 aic = weibull_aic, 
                                 mle_1 = weibull$estimate[1],
                                 mle_2 = weibull$estimate[2])
      
    
    
   # fit pareto
   pareto <-ParetoPosStable::pareto.fit(data$Loss, estim.method = 'MLE')
   if(class(pareto) == 'try-error'){
     pareto <- NULL
     pareto_aic <- NA
     pareto_fit$estimate[1] <- NA
     pareto_fit$estimate[2] <- NA
     
   } else { 
     pareto_aic <- round(-(2*pareto$loglik) + 2, 4)
     message('pareto AIC is ', pareto_aic)
     # get mle
     pareto_mle <- paste0(pareto$estimate[1], ' ', pareto$estimate[2])
     message('pareto mle is ', pareto_mle)
   }
     pareto_data <- data_frame(name = 'pareto',
                               aic = pareto_aic, 
                               mle_1 = pareto$estimate[[1]],
                               mle_2 = pareto$estimate[[2]])
     
     
    
 
   # create a data frame out of data results
   aic_mle_data <- rbind(log_normal_data,
                         gamma_data,
                         beta_data,
                         frechet_data,
                         gumbel_data,
                         weibull_data,
                         pareto_data)
   
   # change names of variable
   names(aic_mle_data) <- c('Distribution', 'AIC', 'MLE 1', 'MLE 2')
   
   # capitalize and remove underscore of Distribution
   aic_mle_data$Distribution <- Hmisc::capitalize(aic_mle_data$Distribution)
   aic_mle_data$Distribution <- gsub('_', ' ', aic_mle_data$Distribution)
    
   return(aic_mle_data)
  })
  
 # create table for aic
  output$aic_table <- renderDataTable({
    
    aic_mle_data <- get_aic_mle()
    DT::datatable(aic_mle_data, options = list(dom = 't'))
    
    
  }) 


  # # create a reactive function to get rag ratings
  # get_rag_ratings <- reactive({
  #   aic_mle_data <- get_aic_mle()
  #   # assign red, amber, or green to the distributions 
  #  
  # })
  # 
  # create a reactive object that takes the aic_mle_data runs simulations on the 
  # best distribution (found from min aic)
  run_best_simulation <- reactive({
    set.seed(11)
    
    dat <- get_aic_mle()
    # for now remove beta
    dat <- dat[dat$Distribution != 'Beta',]
    # get index for minimum aic
    aic_min_ind <- which(dat$AIC == min(dat$AIC, na.rm = T))
    # # subset my index
    dat <- dat[aic_min_ind,]

    # set conditions for each distribution
    if(dat$Distribution == 'Log normal'){
      if(any(is.na(dat$AIC))){
        sim <- NA
      } 
      else {
        sim <- rlnorm(n = 15000, meanlog = dat$`MLE 1`, sdlog = dat$`MLE 2`)
      }
    } else if (dat$Distribution == 'Gamma'){
      if(any(is.na(dat$AIC))){
        sim <- NA
      }  else {
        # check to see how much seed matters
        sim <- rgamma(n = 15000, shape = dat$`MLE 1`, scale = dat$`MLE 2`)
      }
    } else if (dat$Distribution == 'Beta'){
      if(any(is.na(dat$AIC))){
        sim <- NA
      } else {
        sim <- rbeta(n = 15000, shape1 = dat$`MLE 1`, scale2 = dat$`MLE 2`)
      }
    }  else if (dat$Distribution == 'Frechet'){
      if(any(is.na(dat$AIC))){
        sim <- NA
      }  else {
        sim <- rfrechet(n = 15000, loc=0, scale=dat$`MLE 1`, shape=dat$`MLE 2`)
      }
    } else if (dat$Distribution == 'Gumbel'){
      if(any(is.na(dat$AIC))){
        sim <- NA 
      } else {
        sim <- actuar::rgumbel(n = 15000, alpha = dat$`MLE 1`, scale = dat$`MLE 2`)
      }
    } else if (dat$Distribution == 'Weibull'){
      if(any(is.na(dat$AIC))){
        sim <- NA
      }  else {
        sim <- rweibull(n = 15000, shape = dat$`MLE 1`, scale = dat$`MLE 2`)
      }
    } else {
      if(any(is.na(dat$AIC))){
        sim <- NA
      }  else {
        sim <- extraDistr::rpareto(n = 15000, a = dat$`MLE 1`, b = dat$`MLE 2`)
      }
    }
    return(sim)
  })
  
  # create a ouput plot that draws a density of the distribution over the 
  # histogram of raw data
  
  output$hist_plot <- renderPlot({
    data <- selected_country()
    
    ggplot(data, aes(data$Loss)) +
      geom_histogram(bins = 5, fill = 'black', alpha = 0.6) + 
      labs(x = 'Loss', 
           y = 'Counts') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 12)) 
    
  })
  output$sim_plot <- renderPlot({
    dat_sim <- run_best_simulation()
    dat_sim <- as.data.frame(dat_sim)
    names(dat_sim) <- 'Simulated loss'
    ggplot(dat_sim, aes(`Simulated loss`)) +
      geom_density(fill = 'black', alpha = 0.5) +
      labs(y = 'Density') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 12)) 
    
  })
  
  # annual loss exhibit 1
  output$annual_loss <- renderPlot({
    
    # get country data
    data <- selected_country()
    data <- data[order(data$Year, decreasing = FALSE),]
    # get country input for plot title
    plot_title <- input$country
    if(plot_title == 'Afghanistan'){
      scale_by = 1
    } else {
      scale_by = 1000000
    }
    # get best distirbution 
    dat_sim <- run_best_simulation()
    # get quaintles 
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
    
    # melt the data frame to get value and variable 
    dat <- melt(dat)
    
    plot_bar(temp_dat = dat, 
             bar_color = 'black', 
             border_color = 'black', 
             alpha = 0.8,
             plot_title = plot_title)
      
    
  })
  
 
  # exhibit 2
  output$loss_exceedance <- renderPlot({
    data <- selected_country()
    country_name <- unique(data$Country)
   
    data <- data[order(data$Year, decreasing = FALSE),]
    largest_loss_num <- max(data$Loss)
    largest_loss_year <- data$Year[data$Loss == max(data$Loss)]
    
    # get country input for plot title
    plot_title <- input$country
    if(plot_title == 'Afghanistan'){
      scale_by = 1
    } else {
      scale_by = 1000000
    }
    # get best distirbution 
    dat_sim <- run_best_simulation()
    peril_exceedance_curve <- as.data.frame(quantile(dat_sim,seq(0.5,0.98,by=0.002)))
    peril_exceedance_curve$x <- rownames(peril_exceedance_curve)
    rownames(peril_exceedance_curve) <- NULL
    names(peril_exceedance_curve)[1] <- 'y'
    
    # remove percent and turn numeric
    peril_exceedance_curve$x <- gsub('%', '', peril_exceedance_curve$x)
    peril_exceedance_curve$x <- as.numeric(peril_exceedance_curve$x)

    # divide y by 100k, so get data in millions 
    plot_line(temp_dat = peril_exceedance_curve, 
              line_color = 'black', 
              line_size = 2, 
              alpha = 0.7, 
              exhibit_2 = TRUE, 
              largest_loss_num = largest_loss_num,
              largest_loss_year= largest_loss_year,
              plot_title = plot_title)
  })
  
  # annual loss (exhibit 3)
  output$annual_loss_gap <- renderPlot({
    # get country data

    data <- selected_country()
    data <- data[order(data$Year, decreasing = FALSE),]
    # get country input for plot title
    plot_title <- input$country
    if(plot_title == 'Afghanistan'){
      scale_by = 1
    } else {
      scale_by = 1000000
    }
    # get best distirbution 
    dat_sim <- run_best_simulation()
    # get quaintles 
    output <- quantile(dat_sim,c(0.8,0.9, 0.96,0.98,0.99)) 
    annual_avg <- mean(dat_sim)
    
    # create data frame dat to store output with chart labels 
    dat <- data_frame(`Average` = annual_avg, 
                      `Severe` = output[2],
                      `Extreme` = output[5])
    
    # melt the data frame to get value and variable 
    dat <- melt(dat)
    
    # divide valueb by 100k
    dat$value <- dat$value/scale_by
    
    plot_bar(temp_dat = dat, 
             bar_color = 'black', 
             border_color = 'black', 
             alpha = 0.8,
             plot_title = plot_title)
    
  })
  
  # exhibit 4
  
  output$loss_exceedance_gap <- renderPlot({

    data <- selected_country()
    data <- data[order(data$Year, decreasing = FALSE),]
    largest_loss_num <- max(data$Loss)
    largest_loss_year <- data$Year[data$Loss == max(data$Loss)]
    
    # get country input for plot title
    plot_title <- input$country
    if(plot_title == 'Afghanistan'){
      scale_by = 1
    } else {
      scale_by = 1000000
    }
    # get best distirbution 
    dat_sim <- run_best_simulation()
    funding_gap_curve <- as.data.frame(quantile(dat_sim,seq(0.5,0.98,by=0.002)))
    funding_gap_curve$x <- rownames(funding_gap_curve)
    rownames(funding_gap_curve) <- NULL
    names(funding_gap_curve)[1] <- 'y'
    
    # remove percent and turn numeric
    funding_gap_curve$x <- gsub('%', '', funding_gap_curve$x)
    funding_gap_curve$x <- as.numeric(funding_gap_curve$x)/100

    # divide y by 100k, so get data in millions 
    funding_gap_curve$x <- (1 - funding_gap_curve$x)
    funding_gap_curve$y <- funding_gap_curve$y/scale_by
    
    plot_line(temp_dat = funding_gap_curve, 
              line_color = 'black', 
              line_size = 2, 
              alpha = 0.7, 
              exhibit_2 = FALSE, 
              plot_title = plot_title)
    
    
    
  })
  

  # # create table for aic
  # output$mle_table <- renderDataTable({
  #   
  #     country_data <- selected_country()
  #     mle_data <- country_data[[2]]
  #     mle_data <- apply(mle_data, 2, function(x) as.numeric(x))
  #     mle_data <- apply(mle_data, 2, function(x) round(x, 4))
  #     DT::datatable(mle_data, options = list(dom = 't'))
  #     
  #   
  # }) 
  # 
  # create an amendable table for table_1 if input$
  output$data_table_peril <- renderDataTable({
    amend_upload <- input$amend_upload
    if(amend_upload == 'Use preloaded data' | amend_upload == 'Amend preloaded data'){
      country_data <- selected_country()
      DT::datatable(country_data)
    } else {
      NULL
    }
  })  
  
  # uioutput to give action button 
  output$upload_data <- renderUI({
    amend_upload <- input$amend_upload
    if(amend_upload == 'Use preloaded data' | amend_upload == 'Amend preloaded data'){
     NULL
    } else {
     actionButton('uplod_data',
                  'Upload data')
    }
  })

  # create a data table  
  output$data_table <- DT::renderDataTable({
    country_data <- selected_country()
    raw_data <- country_data[[1]]
    datatable(raw_data)
  })
  
  # create a data plot
  output$data_plot <- renderPlot({
    if(is.null(input$advanced)){
      NULL
    } 
    else {
      country_data <- selected_country()
      # get data based on country input
      ggplot(country_data, aes(Year, Loss)) +
        geom_bar(stat = 'identity', fill = 'darkblue',
                 color = 'blue', 
                 alpha = 0.7) +
               labs(title = 'Peril Data')+
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 12)) 
    }
    
  })
  
  # # create a reactive data set to select region
  # pop_data <- reactive({
  #   region_name <- input$region
  #   region_pop <- popn.data[popn.data$Region == region_name,]
  # })
  # 
  # # create a reactive dataset for scaled by population, gdp growth (advanced settings), or inflation (advanced settings)
  # # calculate the scaling factors for each historic year relative to the most recent population figure using the formula:
  # scale_by_pop <- reactive({
  #   # get data from reactive data subsetted by region above
  #   data <- pop_data()
  #   
  #   # current pop/histic pop in year i, where i represents each of the years prior to the current population in the modelled time horizon.
  #   # The current pop is defined as the most recent population figure available in the Tool or entered by the user.
  #   data <- data[order(data$Year, decreasing = TRUE),]
  #   
  #   data$scaling_factor <- data$Population[1]/data$Population
  #   
  #   # Multiply the original loss data value by the respective scaling factor based on the year the loss data value is from.
  #   # join peril data with data
  #   data <- inner_join(data, archetype.data, by = 'Year')
  #   data$scaled_loss <- data$scaling_factor*data$Total_NNDIS_Losses
  #   return(data)
  #   
  # })
  # 
  # # LINEAR DETRENDING: The user is able to linearly detrend the loss data to retrospectively correct any 
  # # linear trend in the data by adjusting past values.
  # # create a reactive dataset to further detrend (advanced settings) after scaling has already been applied
  # # Right now just by population, but will eventually be by GDP and inflation
  # advanced_detrend_p_value <- reactive({
  #   # get subsetted (by region) and scaled data 
  #   scaled_data <- scale_by_pop()
  #   
  #   # test for any remaining trend in the scaled loss (this time scaled by population, 
  #   # but can do GDP and inflation too) 
  #   # Stuck here and will ask about further detrending with t test.
  #   # in the meantime use cox stuart detrending to get p value.
  #   p_value <- trend.test(scaled_data$scaled_loss,plot = FALSE) # i
  #   p_value <- p_value$p.value
  #   return(p_value)
  # })
  
  
  
  # create reactive object for each frequency and loss distribution
  
  # # create plot with p value for advanced users who also selected further linear scaling (detrend)
  # output$advanced_detrend_plot <- renderPlot({
  #   
  #   # if user choses advanced settings AND wants to test for further detrending, then show plot, else 
  #   # keep null
  #   advanced <- input$advanced
  #   detrend <- input$detrend
  #   print(detrend)
  #   print(advanced)
  #   if(is.null(detrend)){
  #     return(NULL)
  #   } else {
  #     if(!advanced | !detrend){
  #       return(NULL)
  #     } else {
  #       # get scaled data
  #       data <- scale_by_pop()
  #       
  #       # get p value from cox test
  #       p_value <- advanced_detrend_p_value()
  #       p_value <- round(p_value, 2)
  #       
  #       # set condition for if p value is greater than 0.05, no need to detrend further
  #       if(p_value <= 0.05) {
  #         p_value_message <- 'statistically significant trend still exists'
  #       } else {
  #         p_value_message <- 'no further detrending necessary'
  #       }
  #       data$Year <- as.numeric(data$Year)
  #       p <- ggplot(data, aes(Year, scaled_loss)) + 
  #         geom_point(size = 2) +
  #         geom_line() + 
  #         labs(x = 'Year',
  #              y = 'Scaled loss',
  #              subtitle = paste0('The P-value for the Cox Stuart trend test is\n', p_value, ' = ', 
  #                                p_value_message)) +
  #         theme_databrew()
  #       return(p)
  #     }
  #     
  #   }
  #   
  # })
  
  
  # create a uioutput for when data type == cost per person
  output$cost_per_person <- renderUI({
    if(input$damage_type != 'Cost per person'){
      NULL
    } else {
      numericInput('cost_per_person',
                   'Enter coster per person USD', 
                   min = 0,
                   max = 1000,
                   step = 10,
                   value = 50)
    }
  })
  
  # create uioutput code for cases where the user choses a currency other than USD
  output$code <- renderUI({
    if(input$currency == 'USD' | input$damage_type == 'Cost per person'){
      NULL
    } else {
      selectInput('code',
                  'Choose a currency code', 
                  choices = other_currencies, 
                  selected = NULL)
    }
  })
  
  # create a uiouput for if an invalid code is selected once "other" is chosen for currency
  output$display_rate <- renderText({
    if(is.null(input$code) | input$damage_type == 'Cost per person'){
      NULL
    } else {
      "Conversion rate only required when data type is 'Total damage'"
    }
    
  })
  
  
  # create uioutput rate for cases where the user choses a currency other than USD
  output$rate <- renderUI({
    if(input$currency == 'USD' | input$damage_type == 'Cost per person'){
      return(NULL) 
    } else {
      numericInput('rate',
                   'Enter conversion rate',
                   min = 0,
                   max = 100,
                   step = 1,
                   value = 1)
    }
  })
  
  # create a uiOutput to show scaling options advanced version is chosen (input$advanced)
  output$scaling <- renderUI({
    if(!input$advanced){
      NULL
    } else {
      selectInput('scaling',
                  'Scale by',
                  choices = c('Population', 
                              'GDP',
                              'Inflation'),
                  selected = 'Population')
    }
    
  })
  
  # create a uioutput for when advanced setting is chosen and the user and detrend data 
  #  HERE is where we are - this should give p value for any existing trend after population scaling 
  # and have an option to apply it. ALSO make sure detrending ends with mean = 0, as investopida implies...
  output$detrend <- renderUI({
    if(!input$advanced){
      NULL
    } else {
      checkboxInput('detrend',
                    'Perform further linear detrending',
                    value= FALSE)
    }
    
  })
  
  # create a uioutput for selecting conditional loss distribution if advanced user
  output$prob_dis <- renderUI({
    if(!input$advanced){
      NULL
    } else {
      selectInput('prob_dis',
                  'Conditional loss distribution',
                  choices = c('Lognormal', 'Beta', 'Gamma', 
                              'Frechet', 'Gumbel', 'weibull',
                              'Pareto', 'Poisson', ' Bernoulli'))
    }
    
  })
  
} # end server

# Run the application 
shinyApp(ui = ui, server = server) 

