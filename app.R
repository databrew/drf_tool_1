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
                                         selectInput('frequency',
                                                     'Frequency of event distribution',
                                                     choices = c('Bernoulli',
                                                                 'Poisson'))), # basic user and advanced user
                                  column(6,
                                         uiOutput('prob_dis')) # advanced user only
                                ),
                                fluidRow(
                                  br(),
                                  column(6,
                                         box(
                                           title = 'MLE',
                                           width = 12,
                                           status = 'primary',
                                         DT::dataTableOutput('mle_table'))), # maximum liklihood estimations for each distribution or the  one chosen.
                                  column(6,
                                         box(
                                           title = 'AIC',
                                           width = 12,
                                           status = 'primary',
                                         DT::dataTableOutput('aic_table')))
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
  # CREATE A REACTIVE OBJECT THAT GETS AIC SCORES FOR EACH PARAMETRIC LOSS DISTRIBUTION
  get_aic <- reactive({
    
    # get country data
    data  <- selected_country()
    
    # fit lognormal
    log_normal <- fitdistr(data$Loss, "lognormal")
    log_normal_aic <- round(AIC(log_normal), 4)
    message('log normal AIC is ', log_normal_aic)
    
    # fit beta (only one not replicating)
    # normalize data to 0, 1
    x <- normalize_data(data$Loss, add_decimal = TRUE, dec = 0.001)
    beta <- fitdistrplus::fitdist(data = x, distr = 'beta', method = 'mle')
    beta_aic <- NA
    # beta_aic <- round(beta$aic, 4)
    message('beta AIC is ', beta_aic)
    

    # EQUATION FOR AIC 
    # -2*loglikihood + k*npar, where k is generally 2 and npar is number of parameters in the model.
    
    # fit gamma
    gamma <- fitdistr(data$Loss, "gamma")
    gamma_aic <- round(AIC(gamma), 4)
    message('gamma AIC is ', gamma_aic)
    
    # fit frechet
    dfrechet(data$Loss, lambda = 1, mu = 0, sigma = 1, log = TRUE)
    frechet <- fitdistrplus::fitdist(data$Loss, "frechet", start=list( mu=0, sigma=1), method="mle")
    frechet_aic <- round(frechet$aic, 4)
    message('frechet AIC is ', frechet_aic)
    
    # git gumbel
    gumbel_fit <- fit_gumbel(data$Loss)
    gumble_aic <- round(gumbel_fit$aic, 4)
    message('gumble AIC is ', gumble_aic)
    
    # fit weilbull
    weibull <- MASS::fitdistr(data$Loss, "weibull", lower = c(0.1, 0.1))
    weibull_aic <- round(AIC(weibull), 4)
    message('weilbull AIC is ', weibull_aic)
    
    
    # fit pareto
    # not working 
    # pareto <- fitdistrplus::fitdist(data = data$Loss, distr = 'pareto', method = 'mme',  start = list(shape = 1, scale = 500) )
    # pareto_aic <- round(pareto$aic, 4)
    # MASS::fitdistr(data$Loss, dpareto, list(shape=1, scale=1))

  })
  
  
  # annual loss exhibit 1
  output$annual_loss <- renderPlot({
   
    # a <- afghanistan_data[[5]]
    # m <- malaysia_data[[5]]
    # s <- Senegal_data[[5]]
    # so <- somalia_data[[5]]
    
    plot_title <- input$country
    country_data <- selected_country()
    dat <- country_data[[5]]
    plot_bar(temp_dat = dat, 
             bar_color = 'black', 
             border_color = 'black', 
             alpha = 0.8,
             plot_title = plot_title)
      
    
  })
  
 
  # exhibit 2
  output$loss_exceedance <- renderPlot({
    
    country_data <- selected_country()
    dat <- country_data[[8]]
    plot_title <- input$country
    plot_line(temp_dat = dat, 
              line_color = 'black', 
              line_size = 2, 
              alpha = 0.7, 
              exhibit_2 = TRUE, 
              plot_title = plot_title)
  })
  
  # annual loss (exhibit 3)
  output$annual_loss_gap <- renderPlot({
    plot_title <- input$country
    
    country_data <- selected_country()
    dat <- country_data[[7]]
  
    plot_bar(temp_dat = dat, 
             bar_color = 'black', 
             border_color = 'black', 
             alpha = 0.8,
             plot_title = plot_title)
    
  })
  
  # exhibit 4
  
  output$loss_exceedance_gap <- renderPlot({
  
    plot_title <- input$country
    country_data <- selected_country()
    dat <- country_data[[6]]
    
    plot_line(temp_dat = dat, 
              line_color = 'black', 
              line_size = 2, 
              alpha = 0.7, 
              exhibit_2 = FALSE, 
              plot_title = plot_title)
    
    
    
  })
  
  # create table for aic
  output$aic_table <- renderDataTable({

      country_data <- selected_country()
      aic_data <- country_data[[3]]
      aic_data <- apply(aic_data, 2, function(x) as.numeric(x))
      aic_data <- apply(aic_data, 2, function(x) round(x, 4))
      DT::datatable(aic_data, options = list(dom = 't'))
      
  
  }) 
  
  # create table for aic
  output$mle_table <- renderDataTable({
    
      country_data <- selected_country()
      mle_data <- country_data[[2]]
      mle_data <- apply(mle_data, 2, function(x) as.numeric(x))
      mle_data <- apply(mle_data, 2, function(x) round(x, 4))
      DT::datatable(mle_data, options = list(dom = 't'))
      
    
  }) 
  
  # create an amendable table for table_1 if input$
  output$data_table_peril <- renderDataTable({
    amend_upload <- input$amend_upload
    if(amend_upload == 'Use preloaded data' | amend_upload == 'Amend preloaded data'){
      country_data <- selected_country()
      raw_data <- country_data[[1]]
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
      raw_data <- country_data[[1]]
      # get data based on country input
      ggplot(raw_data, aes(Year, Loss)) +
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
                              'Frechet', 'Gumbel', 'Weilbull',
                              'Pareto', 'Poisson', ' Bernoulli'))
    }
    
  })
  
} # end server

# Run the application 
shinyApp(ui = ui, server = server) 

