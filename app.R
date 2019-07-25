source('global.R')


ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = "DRF Tool 1"),
                    dashboardSidebar(
                      sidebarMenu( id = 'menu',
                                   menuItem("Overview", tabName = 'overview', icon = icon('home')),
                                   menuItem("Assumptions & Methodology", tabName = 'methodology', icon = icon('info')),
                                   menuItem("Parameters", tabName = 'parameters', icon = icon('edit')),
                                   menuItem("Data", tabName = 'data', icon = icon('table')),
                                   menuItem("Output", tabName = 'output', icon = icon('signal')),
                                   actionButton("simulate", "Run Tool", icon('refresh'), style = "color: #fff;background-color: #337ab7; border-color: #2e6da4", width="70%"),
                                   br(),
                                   br(),
                                   awesomeCheckbox("advanced", "Advanced Settings", FALSE,status = 'danger')
                                   # conditionalPanel("input.advanced",
                                   #                  textInput('nndisfac',label= 'NNDIS Factor (%)', value = "2.5", width = "60%")) 
                      )),
                    dashboardBody( 
                      # tags$head(tags$style(HTML('
                      #                           /* body */
                      #                           .content-wrapper, .right-side {
                      #                           background-color: #FFFFFF;
                      #                           }'))),
                      
                      
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
                                         radioButtons('amend_upload',
                                                      'Choose a data source',
                                                      choices = c('Use preloaded data',
                                                                  'Amend preloaded data',
                                                                  'Upload data'),
                                                      selected = 'Use preloaded data',
                                                      inline = TRUE)),
                               
                                column(7,
                                       fluidRow(
                                         column(6, 
                                                radioButtons('damage_type',
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
                                                            choices = country,
                                                            selected = country[1])),
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
                                                  DT::dataTableOutput('table_1'))),
                                       
                                       fluidRow(
                                         column(12,
                                                uiOutput('upload_data'))
                                       ))
                                         
                                       
                                      
                                       
                                ),
                                column(5,
                                       align = 'center',
                                       plotOutput('advanced_detrend_plot')))
                        ), # end tabItem
                        tabItem(tabName = 'data',
                                fluidRow(
                                  column(6,
                                         uiOutput('scaling')),
                                  column(6,
                                         uiOutput('detrend'))
                                  
                                ),
                                fluidRow(
                                  column(6,
                                         selectInput('frequency',
                                                     'Frequency of event distribution',
                                                     choices = c('Bernoulli',
                                                                 'Poisson'))),
                                  column(6,
                                         uiOutput('prob_dis'))
                                ),
                                fluidRow(
                                  br(),
                                  column(12,
                                         DT::dataTableOutput('mle_table')),
                                  br(),
                                  column(12, 
                                         DT::dataTableOutput('aic_table')
                                  )
                                ),
                                fluidRow(
                                  box(
                                    title = 'Drought Data',
                                    width = 12,
                                    status = 'primary',
                                  column(12,
                                         plotOutput('data_plot'))
                                ))
                                ), # end tabItem
                        tabItem(tabName = 'output', # end tabItem
                             fluidRow(
                               box(title = 'Estimated Average Annual Loss by droughts in Afghanistan', 
                                   width = 6,
                                   status = 'primary',
                               column(12,
                                      plotOutput('annual_loss'))),
                               box(title = 'Loss Exceedance Curve for droughts in Afghanistan', 
                                   width = 6,
                                   status = 'primary',
                               column(12,
                                      plotOutput('loss_exceedance')))
                               ),
                             fluidRow(
                               box(title = 'Estimated Avg Annual Loss by droughts in Afghanistan', 
                                   width = 6,
                                   status = 'primary',
                                   column(12,
                                          plotOutput('annual_loss_gap'))),
                               box(title = 'Loss Exceedance Curve of funding gap for droughts in Afghanistan', 
                                   width = 6,
                                   status = 'primary',
                                   column(12,
                                          plotOutput('loss_exceedance_gap')))
                             )
                             
                        ))
                   )
)

server <- function(input, output) {
  
 ###  OUTPUT page
  
  # annual loss
  output$annual_loss <- renderPlot({
    
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
    
    
    
    
    ggplot(dat, aes(variable, value)) +
      geom_bar(stat = 'identity') +
      labs(x = 'Return period',
           y = 'Estimated annual loss (million USD')+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
      
    
  })
  
 
  output$loss_exceedance <- renderPlot({
    dat <- exhibit_2_4[,c('percent', 'graph')]
    # scale back to plot data
    dat$graph <- dat$graph/1000000
    
    # get text vector
    dat$text <- ifelse(dat$graph == 0.0004, 'Largest Loss 2018', NA)
    
    ggplot(dat, aes(-percent, graph)) +
      geom_line(size = 1.5, color = 'blue') +
      labs(x = 'Probability of Exceeding Loss',
           y = 'Value of loss (million USD)',
           caption = 'dotted line represents largest loss 2018') +
      geom_hline(yintercept  = 0.0004, linetype= 2) + 
      theme_bw() 
  })
  
  # annual loss
  output$annual_loss_gap <- renderPlot({
    
    # keep only relevant row
    dat <- exhibit_3
    
    # clean data
    dat <- dat[1,]
    
    # rescale 
    names(dat) <- c('Drought', 'Annual Average', 'Severe', 'Extreme')
    dat <- melt(dat, id.vars = 'Drought')
    dat$Drought <- NULL
    dat$value <- dat$value/1000000
    
    # plot
    ggplot(dat, aes(variable, value)) +
      geom_bar(stat = 'identity') +
      labs(x = '',
           y = 'Estimated potential annual loss (million USD')+
      theme_bw() +
      ylim(c(0.000, 0.001)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
  })
  
  output$loss_exceedance_gap <- renderPlot({
    dat <- exhibit_2_4[,c('percent', 'funding_gap')]
    # scale back to plot data
    dat$funding_gap <- dat$funding_gap/1000000
    
    ggplot(dat, aes(percent, sort(funding_gap))) +
      geom_line(size = 1.5, color = 'blue') +
      labs(x = 'Probability of Exceeding Loss',
           y = 'Funding gap (million USD)') +
      theme_bw() 
  })
  
  # create table for aic
  output$aic_table <- renderDataTable({
    advanced <- input$advanced
    if(!advanced){
      NULL
    } else {
      DT::datatable(aic_data, caption = 'AIC Scores')
      
    }
  }) 
  
  # create table for aic
  output$mle_table <- renderDataTable({
    advanced <- input$advanced
    if(!advanced){
      NULL
    } else {
      DT::datatable(mle_data, caption = 'MLE')
      
    }
  }) 
  
  # create an amendable table for table_1 if input$
  output$table_1 <- renderDataTable({
    amend_upload <- input$amend_upload
    if(amend_upload == 'Use preloaded data' | amend_upload == 'Amend preloaded data'){
      DT::datatable(raw_data)
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
    datatable(raw_data)
  })
  
  # create a data plot
  output$data_plot <- renderPlot({
    if(!input$advanced){
      NULL
    } 
    else {
      ggplot(raw_data, aes(Year, Loss)) + 
        geom_bar(stat = 'identity') +
        theme_bw()
    }
    
  })
  
  # create a reactive data set to select region
  pop_data <- reactive({
    region_name <- input$region
    region_pop <- popn.data[popn.data$Region == region_name,]
  })
  
  # create a reactive dataset for scaled by population, gdp growth (advanced settings), or inflation (advanced settings)
  # calculate the scaling factors for each historic year relative to the most recent population figure using the formula:
  scale_by_pop <- reactive({
    # get data from reactive data subsetted by region above
    data <- pop_data()
    
    # current pop/histic pop in year i, where i represents each of the years prior to the current population in the modelled time horizon.
    # The current pop is defined as the most recent population figure available in the Tool or entered by the user.
    data <- data[order(data$Year, decreasing = TRUE),]
    
    data$scaling_factor <- data$Population[1]/data$Population
    
    # Multiply the original loss data value by the respective scaling factor based on the year the loss data value is from.
    # join peril data with data
    data <- inner_join(data, archetype.data, by = 'Year')
    data$scaled_loss <- data$scaling_factor*data$Total_NNDIS_Losses
    return(data)
    
  })
  
  # LINEAR DETRENDING: The user is able to linearly detrend the loss data to retrospectively correct any 
  # linear trend in the data by adjusting past values.
  # create a reactive dataset to further detrend (advanced settings) after scaling has already been applied
  # Right now just by population, but will eventually be by GDP and inflation
  advanced_detrend_p_value <- reactive({
    # get subsetted (by region) and scaled data 
    scaled_data <- scale_by_pop()
    
    # test for any remaining trend in the scaled loss (this time scaled by population, 
    # but can do GDP and inflation too) 
    # Stuck here and will ask about further detrending with t test.
    # in the meantime use cox stuart detrending to get p value.
    p_value <- trend.test(scaled_data$scaled_loss,plot = FALSE) # i
    p_value <- p_value$p.value
    return(p_value)
  })
  
  # The basic user will not see this, only the advanced user
  frequency_distribution_bernoulli <- reactive({
    # will have other options for different scales later
    freq_data <- scale_by_pop()
    # temporarily do simulation 
    freq_data <- freq_data[complete.cases(freq_data),]
    # sum of success (disaster) over sum if trials (years). 6 success in 8 years
    # get trials max year minus min year
    num_trials <- as.numeric(as.character(max(freq_data$Year))) - min(as.numeric(as.character(freq_data$Year)))
    num_trials <- num_trials + 1
    mle_bern <- sum(nrow(freq_data)/num_trials)
    uniform_dis <- runif(1000, 0, 1)
    sim_freq_data <- as.data.frame(cbind(simulation_num = 1:1000, uniform_dis = uniform_dis))
    # create a variable to show success (mle?uniform_dis, then success)
    sim_freq_data$outcome <- ifelse(sim_freq_data$uniform_dis < mle_bern, 'success', 'fail')
    
  })
  
  # # The basic user will not see this, only the advanced user
  frequency_distribution_poisson <- reactive({
    # will have other options for different scales later
    freq_data <- scale_by_pop()
    #   # temporarily do simulation
    freq_data <- freq_data[complete.cases(freq_data),]
    #   # sum of success (disaster) over sum if trials (years). 6 success in 8 years
    #   # get trials max year minus min year
    #
  })
  
  # get log normal aic
  log_normal_loss_dis <- reactive({
    
    # get scaled data
    data <- scale_by_pop
    
    # get complete cases
    data <- data[complete.cases(data),]
    
    log_normal <- fitdistr(data$scaled_loss, "lognormal")
    log_normal_aic <- AIC(log_normal)
    
    return(log_normal_aic)
    
  })
  
  # get beta aic
  beta_loss_dis <- reactive({
    # get scaled data
    data <- scale_by_pop
    
    # get complete cases
    data <- data[complete.cases(data),]
    
    beta<- fitdistr(data$scaled_loss, "beta")
    beta_aic <- AIC(beta)
    # ISSUE need names list
    return(beta_aic)
  })
  
  # get gamme aic
  gamma_loss_dis <- reactive({
    # get scaled data
    data <- scale_by_pop
    
    # get complete cases
    data <- data[complete.cases(data),]
    
    gamma <- fitdistr(data$scaled_loss, "gamma")
    gamma_aic <- AIC(gamma)
    # Error
    return(gamma_aic)
    
  })
  
  # get frechet aic
  frechet_loss_dis <- reactive({
    # get scaled data
    data <- scale_by_pop
    
    # get complete cases
    data <- data[complete.cases(data),]
    
    frechet <- fitdistr(data$scaled_loss, "frechet")
    frechet_aic <- AIC(frechet)
    return(frechet_aic)
    
  })
  
  # get gumbel aic
  gumbel_loss_dis <- reactive({
    # get scaled data
    data <- scale_by_pop
    
    # get complete cases
    data <- data[complete.cases(data),]
    
    gumbel <- fitdistr(data$scaled_loss, "gumbel")
    gumbel_aic <- AIC(gumbel)
    return(gumbel_aic)
  })
  
  # get weilbull aic
  weilbull_loss_dis <- reactive({
    # get scaled data
    data <- scale_by_pop
    
    # get complete cases
    data <- data[complete.cases(data),]
    
    weilbull <- fitdistr(data$scaled_loss, "weilbull")
    weilbull_aic <- AIC(weilbull)
    return(weilbull_aic)
    
  })
  
  # get pareto aic
  pareto_loss_dis <- reactive({
    # get scaled data
    data <- scale_by_pop
    
    # get complete cases
    data <- data[complete.cases(data),]
    
    pareto <- fitdistr(data$scaled_loss, "pareto")
    pareto_aic <- AIC(pareto)
    return(pareto)
  })
  
  
  # make reactive object to compare and find best AIC of parametic distributions
  # the distr function allows for Distributions "beta", "cauchy", "chi-squared", "exponential", 
  # "gamma", "geometric", "log-normal", "lognormal", "logistic", "negative binomial", "normal",
  # "Poisson", "t" and "weibull" are recognised, case being ignored.
  best_loss_dis <- reactive({
    # will have other options for different scales later
    pareto_aic <- pareto_loss_dis()
    weilbull_aic <- weilbull_loss_dis()
    log_normal_aic <- log_normal_loss_dis()
    beta_aic <- beta_loss_dis()
    frechet_aic <- frechet_loss_dis()
    gamma_aic <- gamme_loss_dis()
    gumbel_aic <- gumbel_loss_dis
  })
  
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
                              'Frechet', 'Gumbel', 'Weilbull',
                              'Pareto', 'Poisson', ' Bernoulli'))
    }
    
  })
  
} # end server

# Run the application 
shinyApp(ui = ui, server = server) 

