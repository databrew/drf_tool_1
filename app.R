# DRF Financing Tool Sri Lanka
source('global.R')

# setwd('C:\\Users\\wb529583\\OneDrive - WBG\\Sri Lanka\\2019_Analysis,Tool,andNNDISReview\\StructuredFinancingTools\\Bangkok_Tool\\Bangkok_Tool\\Contingent_Liability\\')

ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = "Sri Lanka"),
                    dashboardSidebar(
                      sidebarMenu( id = 'menu',
                                   menuItem("Overview", tabName = 'overview', icon = icon('home')),
                                   menuItem("Data", tabName = 'data', icon = icon('table')),
                                   menuItem("User inputs", tabName = 'stats', icon = icon('bar-chart-o')),
                                   menuItem("Financial Strategy", tabName = 'parameters', icon = icon('coins')),
                                   menuItem("Parameters", tabName = 'cbaparameters', icon = icon('edit')),
                                   menuItem("Output", tabName = 'output', icon = icon('signal')),
                                   menuItem("Assumptions & Methodology", tabName = 'methodology', icon = icon('info'))),
                      actionButton("simulate", "Run Tool", icon('refresh'), style = "color: #fff;background-color: #337ab7; border-color: #2e6da4", width="70%"),
                      br(),
                      br(),
                      awesomeCheckbox("advanced", "Advanced Settings", FALSE,status = 'danger'),
                      conditionalPanel("input.advanced",
                                       textInput('nndisfac',label= 'NNDIS Factor (%)', value = "2.5", width = "60%")) 
                    ),
                    dashboardBody( 
                      # tags$head(tags$style(HTML('
                      #                           /* body */
                      #                           .content-wrapper, .right-side {
                      #                           background-color: #FFFFFF;
                      #                           }'))),
                      
                      
                      tabItems( 
                        tabItem(tabName = 'overview',
                                br(),
                                img(src="SL_Flag.png", height = 50, width = 80, align = 'top'), br(" "),
                                img(src="DRFIP_Logo.png", height = 50, width = 100, align = 'top'),
                                br("  "),
                                h2("Disaster Risk Financing in Sri Lanka"),
                                tags$hr(), 
                                h3("Overview"),
                                p("This Tool has been developed by the World Bank, in partnership with the Government of Sri Lanka, to support better understandinding of the financial contingent liabilities resulting from natural disaster risk. The tool is designed to support Government and other stakeholders to understand the financial impact of key decisions in the design of a disaster risk financing strategy. 
                                  The Tool is pre-loaded with an estimate of the distribution of potential financial losses from natural disasters. This is based on analysis using historical data from different sources - the methodology, data sources, and assumptions underlying this analysis is provided in the World Bank's 'Review of the National Natural Disaster Insurance Scheme in Sri Lanka', 2019.
                                  The Tool is limited by the quality of the data from these sources, and hence this will affect the accuracy of the indicative fiscal costing obtained. 
                                  The output from this Tool remains an indication of the costs associated with the inputed strategy, and the total fiscal cost may differ significantly from the Tool's output.", style = "font-size:100%"),
                                p("The tool is currently in DRAFT for discussion and feedback only.", style = "font-size:100%"), 
                                br(),
                                h3("Authorship"),
                                p("The development of this Tool was led by the Disaster Risk Financing and Insurance Program (DRFIP), a partnership of the World Bank Group's Finance Competitiveness and Innovation Global Practice and the Global Facility for Disaster Reduction and Recovery (GFDRR).", style = "font-size:100%"), br(),
                                br(),
                                h3("Disclaimer"),
                                p("This Tool has been developed by the World Bank to develop the capacity of Government and other stakeholders in analysis and understanding of the impact of key decisions they must make during disaster risk financing. The Tool is intended for use as outlined above and it should not be used for any other purposes. The Tool should not be used to inform real financial decisions.
                                  Information in the Tool is provided for educational purposes only and does not constitute legal or scientific advice or service. 
                                  The World Bank makes no warranties or representations, express or implied as to the accuracy or reliability of the Tool or the data contained therein. A user of the Tool should seek qualified expert advice for specific diagnosis and analysis of a particular project. Any use thereof or reliance thereon is at the sole and independent discretion and responsibility of the user. No 
                                  conclusions or inferences drawn from the Tool should be attributed to the World Bank, its Board of Executive Directors, its Management, or any of its member countries.
                                  This Tool does not imply any judgement or endorsement on the part of the World Bank. In no event will the World Bank be liable for any form of damage arising from the application or misapplication of the Tool, or any associated materials.", style = "font-size:100%"),
                                br(), 
                                h3("Confidentiality"),
                                p("The World Bank invests substantial resources in the development of its models, modelling methodologies and databases. This Tool contains proprietary and confidential information and is intended for the exclusive use of World Bank partners with whom this Tool has been shared. Any user is subject to the restrictions of the confidentiality provisions set forth in license and other nondisclosure agreements.", style = "font-size:100%"), br(),
                                br()), # end tabItem
                        
                        
                        tabItem(tabName = 'methodology',
                                br(),
                                h3('Input Loss Distribution Data'),
                                h5('The methodology builds on the approach taken in the report "Review of the National Natural Disaster Scheme" (World Bank, 2019).
                                   1.	Historical losses were obtained from the EM-DAT database (https://emdat.be) for Sri Lanka. 
                                   2.	These values are then adjusted by 6 percent each year to allow for inflation and population growth to bring all the values to 2018 values. They are then increased by a further 10 percent to allow for additional unknown factors such as climate change, increased urbanization, and any other unmodeled factors.
                                   3.	Statistical methods (outlined below) are then used to assess the risk distribution for the NNDIS and calculate the expected NNDIS experience.'),
                                h4('Key Assumptions:'),
                                h5('.	Inflation of 6 percent annually is appropriate to account for the increase in the present value of losses by comparison to historical losses; specifically, it accounts for increased property values, increased urbanization, and any other increases to losses.
                                   .	An increase of 10 percent is appropriate to allow for climate change, increased urbanization, and other factors, though this assumption should be developed in future so the risks facing the NNDIS can be better understood.
                                   .	NNDIS claims will always represent the same proportion of total economic loss to Sri Lanka. This is a simplifying assumption and is particularly open to challenge.
                                   .	Historical losses are a good indicator for future experience.
                                   .	The analysis does not allow for future losses to be higher than historical experience as a result of more frequent natural disasters. Further studies could be carried out to understand the potential impact of this.'),
                                h4('Fitted distribution'),
                                h5('A statistical distribution has been fitted to the 10 years of losses to extrapolate from the existing data concerning events that 
                                   have not been observed, and project what potential future losses might be. The analysis uses a methodology called frequency-severity, 
                                   in which a year of experience for the NNDIS is simulated 10,000 times. In each year, zero, one, or two natural disaster events can occur 
                                   (this assumption remains a simplification because it excludes the possibility that three or more events could occur). Events occur with a 
                                   frequency in line with the last 10 years of disasters in Sri Lanka. For each event simulated, the severity (or amount of loss) is generated from 
                                   thousands of possible values from a statistical distribution that was selected based on the last 10 years of disasters in Sri Lanka. Note that in 
                                   addition to the assumptions above, these values could reasonably be different if a different statistical distribution had been selected. '),
                                h3('Calculation methodology'),
                                h5('The methodology adopted for the analysis of relative cost of different financial strategies is in line with the analysis in the following paper https://hubs.worldbank.org/docs/imagebank/pages/docprofile.aspx?nodeid=26510520'),
                                br()), # end tabItem
                        
                        tabItem(tabName = 'stats',
                                column(7,
                                       fluidRow(
                                         column(6, 
                                                radioButtons('data_type',
                                                             'Data type',
                                                             choices = c('Total damage', 'Cost per person'),
                                                             selected = 'Total damage')),
                                         column(6,
                                                uiOutput('cost_per_person'))
                                         
                                       ),
                                       fluidRow(
                                         column(6, 
                                                selectInput('region',
                                                            'Choose a region',
                                                            choices = regions,
                                                            selected = regions[1])),
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
                                         )
                                       
                                ),
                                column(5,
                                       align = 'center',
                                       plotOutput('advanced_detrend_plot'))
                                
                        ),
                        tabItem(tabName = 'parameters',
                                h2("Financial Strategy"),
                                br(),
                                h5("Select parameters to apply when you run the Tool"),
                                circleButton('paramdefs', icon = icon('info'), status = "default",
                                             size = "sm", style = "color: #fff;background-color: #337ab7; border-color: #2e6da4"),
                                br(),
                                br(),
                                radioButtons('nstrategies','Select No. of Financial Strategies:',choices = c('1'=1,'2'=2,'3'=3),inline=T,selected=1),
                                useShinyjs(),
                                column(12,box( width = 4, title = '1. Select Financial Strategy A', status = 'info', solidHeader = T,
                                               br(),
                                               sliderInput('layer1','Contingency Fund:',
                                                           min = 0, max = 40000,
                                                           value = 10000, step = 1000),
                                               sliderInput('layer2','Contingent Credit:',
                                                           min = 0, max = 40000,
                                                           value = 10000, step = 1000),
                                               sliderInput('layer3','Maximum Budget Reallocation:',
                                                           min = 0, max = 40000,
                                                           value = 0000, step = 1000),
                                               #sliderInput('layer4','Maximum Ex-Post Borrowing:',
                                               #           min = 0, max = 16000000,
                                               #            value = 16000000, step = 10000),
                                               sliderInput('sov_ins', 'Sovereign Insurance:',
                                                           min = 0, max = 100000,
                                                           value = c(40000,60000), step = 10000),
                                               sliderInput('nndis_ins', 'NNDIS coverage limit:',
                                                           min=0, max=15000,
                                                           value = 15000,step = 1000)
                                ),
                                box(id = "stratb", width = 4, title = '2. Select Financial Strategy B', status = 'warning', solidHeader = T,
                                    br(),
                                    sliderInput('layer1B','Contingency Fund:',
                                                min = 0, max = 16000,
                                                value = 10000, step = 1000),
                                    sliderInput('layer2B','Contingent Credit:',
                                                min = 0, max = 16000,
                                                value = 10000, step = 1000),
                                    sliderInput('layer3B','Maximum Budget Reallocation:',
                                                min = 0, max = 16000,
                                                value = 0000, step = 1000),
                                    #  sliderInput('layer4B','Maximum Ex-Post Borrowing:',
                                    #             min = 0, max = 16000000,
                                    #            value = 16000000, step = 10000),
                                    sliderInput('sov_insB', 'Sovereign Insurance:',
                                                min = 0, max = 100000,
                                                value = c(40000,60000), step = 10000),
                                    sliderInput('nndis_insB', 'NNDIS coverage limit:',
                                                min=0, max=15000,
                                                value = 15000,step = 1000)
                                ),
                                box( id = "stratc", width = 4, title = '3. Select Financial Strategy C', status = 'success', solidHeader = T,
                                     br(),
                                     sliderInput('layer1C','Contingency Fund:',
                                                 min = 0, max = 16000,
                                                 value = 10000, step = 1000),
                                     sliderInput('layer2C','Contingent Credit:',
                                                 min = 0, max = 16000,
                                                 value = 10000, step = 1000),
                                     sliderInput('layer3C','Maximum Budget Reallocation:',
                                                 min = 0, max = 16000,
                                                 value = 0000, step = 1000),
                                     # sliderInput('layer4C','Maximum Ex-Post Borrowing:',
                                     #            min = 0, max = 16000000,
                                     #           value = 16000000, step = 10000),
                                     sliderInput('sov_insC', 'Sovereign Insurance:',
                                                 min = 0, max = 100000,
                                                 value = c(40000,60000), step = 10000),
                                     sliderInput('nndis_insC', 'NNDIS coverage limit:',
                                                 min=0, max=15000,
                                                 value = 15000,step = 1000)
                                ))), # end box, column, tabItem
                        
                        tabItem(tabName = 'cbaparameters',
                                fluidRow(
                                  column(3,
                                         h2("Parameters")),
                                  column(9,
                                         circleButton('defs', icon = icon('info'), status = "default",
                                                      # style = "color: #fff;background-color: #337ab7; border-color: #2e6da4",
                                                      size = "sm")
                                  )
                                ),
                                fluidRow(
                                  column(12,
                                         h5('Edit the indicative assumptions below, percentages should be input as decimals'))
                                ),
                                box( width = 12, 
                                     title = '4. Cost-Benefit Parameters', 
                                     status  = 'primary', solidHeader = T,
                                     fluidRow(
                                       column(6,
                                              DTOutput('cbparams'),
                                              # br(),
                                              # br(),
                                              # br(),
                                              DTOutput('cbparamsreserve'),
                                              # br(),
                                              # br(),
                                              DTOutput('cbparamsccredit')
                                       ),
                                       
                                       column(6,
                                              DTOutput('cbparamsborrowing'),
                                              # br(),
                                              # br(),
                                              DTOutput('cbparamsins'),
                                              # br(),
                                              # br(),
                                              DTOutput('cbparamsbudget')
                                       )
                                     ))
                                
                        ),   # end tab item
                        tabItem(tabName = 'data',
                                h2("NNDIS Loss Data"),
                                br(),
                                fluidRow(
                                  column(6,
                                         plotOutput('archetype_plot')),
                                  column(6,
                                         plotOutput('pop_plot'))
                                  
                                ) # end fluid row
                        ), # end tab item
                        
                        tabItem(tabName = 'output',
                                fluidRow( 
                                  column(10, offset = 1,
                                         box(width = 12, 
                                             # height = 400, 
                                             title = "Exceedance loss curve", 
                                             status = 'primary', 
                                             solidHeader = T,
                                             h5('This chart shows the extent of the total annual loss (y-axis) from natural disasters with varying probabilities (x-axis). For example, the loss at the 50% level is the total loss in a year which is expected to be exceeded once in every two years. Similarly, the loss at the 10% level is the total loss in a year which is expected to be exceeded once in every ten years. It is important to note that losses may occur more or less frequently than implied by this analysis, especially over the short term, due to the uncertain nature of natural disasters. The information shown is also heavily dependent on historical data and the methodology used (shown in the methodology tab) - other views of risk may also be valid, and so other sources of data, such as risk models, should be also considered.'),
                                             uiOutput('ui_exceedanceplot')))),
                                fluidRow(column(10, 
                                                offset = 1,
                                                box(width = 12, 
                                                    # height = 400, 
                                                    title = 'Table - relative cost of each DRF strategies under different loss scenarios', status = 'primary', solidHeader = T,
                                                    h5('The table below shows the relative cost of each financing strategy for a year. For each strategy, costs are given on an average basis, but also under increasingly severe loss years. For example, 1 in 50 represent a year with losses which would only be expected to be exceeded once in every 50 years (note that this is not an attempt to forecast losses into the future, rather a way of communicating how likely a certain level of losses is for the next year). The values in this table allow for the relative efficiency of different sources of finance for different costs. For example, holding money in a contingency fund may be effective for frequently observed losses, but holding money beyond the level needed to fund such losses will come with a cost of not using the resources for other projects and so may be less effective for extreme years. Key assumptions, to be discussed with World Bank technical advisors in conjunction with Government financial specialists, are contained in the Paramters sheet. '),
                                                    uiOutput('ui_oppcosttable')))),
                                fluidRow(
                                  column(10,
                                         offset = 1,
                                         box(width= 12, 
                                             #height = 400, 
                                             title = 'Chart - relative cost of each DRF strategy under different loss scenarios',
                                             status = 'primary', solidHeader = T,
                                             h5('This chart communicates the same information as is contained in the table above, but in pictoral format for ease of comparison of strategies.'),
                                             uiOutput('ui_oppcostplot')))),
                                fluidRow(
                                  column(10, offset = 1,
                                         box(width = 12, 
                                             # height = 400, 
                                             title = 'Breakdown of loss funded by each funding source', status = 'primary', solidHeader = T,
                                             h5('This chart shows how annual losses would be financed by different financial instruments under different severities of loss. Note that this is not the cost of financial instruments as is shown in the chart to the right. For example, the amount financed by insurance is the insurance payout in a given year, not the insurance premium (which is what is considered in the chart showing the cost).'),
                                             uiOutput('ui_financialplot'))))
                                
                        ) # end tab item
                        
                                ) # end tabItems
                      
                        )
                        )

server <- function(input, output) {
  # 
  # # placeholders
  # region_name <- '3 Region Total'
  # data <- popn.data[popn.data$Region == region_name,]
  # data <- data[order(data$Year, decreasing = TRUE),]
  # data$scaling_factor <- data$Population[1]/data$Population
  # data <- left_join(data, archetype.data, by = 'Year')
  # data$scaled_loss <- data$scaling_factor*data$Total_NNDIS_Losses
  # p_value <- trend.test(data$scaled_loss,plot = FALSE) # i
  # p_value <- p_value$p.value
  # 
  # # temporarily do simulation 
  # data <- data[complete.cases(data),]
  # # sum of success (disaster) over sum if trials (years). 6 success in 8 years
  # # get trials max year minus min year
  # num_trials <- as.numeric(as.character(max(data$Year))) - min(as.numeric(as.character(data$Year)))
  # num_trials <- num_trials + 1
  # mle_bern <- sum(nrow(data)/num_trials)
  # uniform_dis <- runif(1000, 0, 1)
  # sim_data <- as.data.frame(cbind(simulation_num = 1:1000, uniform_dis = uniform_dis))
  # # create a variable to show success (mle?uniform_dis, then success)
  # sim_data$outcome <- ifelse(sim_data$uniform_dis < mle_bern, 'success', 'fail')
  # 
  # # temporary code for fitting distribution to the loss data
  # x <- data$Total_NNDIS_Losses
  # weibull <- fitdistr(x, "weibull")
  # weibull_aic <- AIC(weibull)
  # lognormal <- fitdistr(x, "lognormal")
  # gamma <- fitdistr(x, "gamma")
  # 
  # 
  # 
  
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
  
  # create plot with p value for advanced users who also selected further linear scaling (detrend)
  output$advanced_detrend_plot <- renderPlot({
    
    # if user choses advanced settings AND wants to test for further detrending, then show plot, else 
    # keep null
    advanced <- input$advanced
    detrend <- input$detrend
    print(detrend)
    print(advanced)
    if(is.null(detrend)){
      return(NULL)
    } else {
      if(!advanced | !detrend){
        return(NULL)
      } else {
        # get scaled data
        data <- scale_by_pop()
        
        # get p value from cox test
        p_value <- advanced_detrend_p_value()
        p_value <- round(p_value, 2)
        
        # set condition for if p value is greater than 0.05, no need to detrend further
        if(p_value <= 0.05) {
          p_value_message <- 'statistically significant trend still exists'
        } else {
          p_value_message <- 'no further detrending necessary'
        }
        data$Year <- as.numeric(data$Year)
        p <- ggplot(data, aes(Year, scaled_loss)) + 
          geom_point(size = 2) +
          geom_line() + 
          labs(x = 'Year',
               y = 'Scaled loss',
               subtitle = paste0('The P-value for the Cox Stuart trend test is\n', p_value, ' = ', 
                                 p_value_message)) +
          theme_databrew()
        return(p)
      }
      
    }
    
  })
  
  
  # create a uioutput for when data type == cost per person
  output$cost_per_person <- renderUI({
    if(input$data_type != 'Cost per person'){
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
    if(input$currency == 'USD' | input$data_type == 'Cost per person'){
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
    if(is.null(input$code) | input$data_type == 'Cost per person'){
      NULL
    } else {
      "Conversion rate only required when data type is 'Total damage'"
    }
    
  })
  
  
  # create uioutput rate for cases where the user choses a currency other than USD
  output$rate <- renderUI({
    if(input$currency == 'USD' | input$data_type == 'Cost per person'){
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
  

  
  
  observeEvent(input$paramdefs, {
    showModal(modalDialog(
      title = "Financial Strategy Definitions",
      strong("Contingency Fund:"),'definition'  ,
      easyClose = TRUE
    ))
  })
  
  
  
  observeEvent(input$defs, {
    showModal(modalDialog(
      title = "Parameter Definitions",
      strong("Marginal interest rate on sovereign debt:"),'definition'  ,
      easyClose = TRUE
    ))
  })
  
  
  
  
  
  
  observeEvent(input$advanced, {
    
    if (input$advanced == F) shinyjs::hide(selector = "a[data-value='methodology']" )
    if (input$advanced == T) shinyjs::show(selector = "a[data-value='methodology']" )
  })
  
  
  ## No. of financial strategies ##
  observeEvent(input$nstrategies, {
    
    if(input$nstrategies == "1" ){
      shinyjs::hide(id = "stratb")
      shinyjs::hide(id = "stratc")
    }
    
    if (input$nstrategies == "2") {
      shinyjs::show(id = "stratb")  
      shinyjs::hide(id = "stratc")
    }
    
    if (input$nstrategies == "3") {
      shinyjs::show(id = "stratb")  
      shinyjs::show(id = "stratc")
    }  
    
    
    
  })
  
  
  ### PLOTS ##
  
  ## Maximum historical and recent historical
  output$archetype_plot <- renderPlot(plot_archetype(archetype.data,
                                                     region = 'NNDIS'))
  
  output$pop_plot <- renderPlot(plot_pop(popn.data))
  
  output$sim_plot <- renderPlot(plot_sim(simulation.data))
  
  
  ## -------------------------------------------------- ##
  ## Parameters Box                                     ##
  ## -------------------------------------------------- ##
  
  
  default_params <- array(data=c(0.13,0.15),dim=c(2,1))
  
  y <- as.data.frame(default_params,
                     row.names = c("Marginal interest rate on sovereign debt (%)",
                                   "Discount factor (%)"))
  
  output$cbparams = renderDT({
    dat <- datatable(y, selection = 'none', editable = TRUE, options = list(dom='t',bSort = FALSE), colnames = c('General Assumptions' = 1, 'Indicative Assumption' = 2))
  })
  
  
  proxy1 = dataTableProxy('cbparams')
  
  observeEvent(input$cbparams_cell_edit, {
    info = input$cbparams_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    if (info$value > 1) {
      showModal(modalDialog(
        title = "Parameter Value Error",
        paste0("Value must be a decimal between 0 and 1 eg. 0.5 = 50%"),
        easyClose = TRUE,
        footer = NULL))
      if (i == 1) y[i, j] <<- DT::coerceValue(0.13, y[i, j])
      if (i == 2) y[i, j] <<- DT::coerceValue(0.15, y[i, j])
      replaceData(proxy1, y, resetPaging = FALSE)          
    } else {
      y[i, j] <<- DT::coerceValue(v, y[i, j])
      replaceData(proxy1, y, resetPaging = FALSE)}  # important
  })
  
  default_params <- array(data=c(0.005,10,0.038),dim=c(3,1))
  
  cc <- as.data.frame(default_params,
                      row.names = c("Arrangement fee for contingent credit (%)",
                                    "Repayment term of contingent credit (months)",
                                    "Interest rate on contingent credit (%)"))
  
  output$cbparamsccredit = renderDT({
    dat <- datatable(cc, selection = 'none', editable = TRUE, options = list(dom='t', bSort = F), colnames = c('Contingent Credit Assumptions' = 1, " " = 2))
  })
  
  proxy7 = dataTableProxy('cbparamsccredit')
  
  observeEvent(input$cbparamsccredit_cell_edit, {
    info = input$cbparamsccredit_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    if (info$value > 1 & i != 2) {
      showModal(modalDialog(
        title = "Parameter Value Error",
        paste0("Value must be a decimal between 0 and 1 eg. 0.5 = 50%"),
        easyClose = TRUE,
        footer = NULL))
      if (i == 1)  cc[i, j] <<- DT::coerceValue(0.005, cc[i, j])
      if (i == 3)  cc[i, j] <<- DT::coerceValue(0.038, cc[i, j])
      replaceData(proxy7, cc, resetPaging = FALSE)          
    } else {
      cc[i, j] <<- DT::coerceValue(v, cc[i, j])
      replaceData(proxy7, cc, resetPaging = FALSE) }  # important
  })
  
  default_params <- array(data=c(0.01),dim=c(1,1))
  
  z <- as.data.frame(default_params,
                     row.names = c("Investment on unspent reserves (%)"))
  
  output$cbparamsreserve = renderDT({
    dat <- datatable(z, selection = 'none', editable = TRUE, options = list(dom='t', bSort = F), colnames = c('Reserve Fund Assumptions' = 1, " " = 2))
  })
  
  proxy2 = dataTableProxy('cbparamsreserve')
  
  observeEvent(input$cbparamsreserve_cell_edit, {
    
    info = input$cbparamsreserve_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    if (info$value > 1) {
      showModal(modalDialog(
        title = "Parameter Value Error",
        paste0("Value must be a decimal between 0 and 1 eg. 0.5 = 50%"),
        easyClose = TRUE,
        footer = NULL))
      z[i, j] <<- DT::coerceValue(0.01, z[i, j])
      replaceData(proxy2, z, resetPaging = FALSE)          
    } else {
      z[i, j] <<- DT::coerceValue(v, z[i, j])
      replaceData(proxy2, z, resetPaging = FALSE)  # important 
    } # end else 
  })
  
  default_params <- array(data=c(0.12),dim=c(1,1))
  
  zz <- as.data.frame(default_params,
                      row.names = c("Social rate of return on projects not funded due to reallocation of budgets (%)"))
  
  output$cbparamsbudget = renderDT({
    dat <- datatable(zz, selection = 'none', editable = TRUE, options = list(dom='t', bSort = F), colnames = c('Emergency ex-post budget reallocation Assumptions' = 1, " " = 2))
  })
  
  proxy3 = dataTableProxy('cbparamsbudget')
  
  observeEvent(input$cbparamsbudget_cell_edit, {
    info = input$cbparamsbudget_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    if (v > 1) {
      showModal(modalDialog(
        title = "Parameter Value Error",
        paste0("Value must be a decimal between 0 and 1 eg. 0.5 = 50%"),
        easyClose = TRUE,
        footer = NULL)) 
      zz[i, j] <<- DT::coerceValue(0.12, zz[i, j])
      replaceData(proxy3, zz, resetPaging = FALSE) }
    else {
      zz[i, j] <<- DT::coerceValue(v, zz[i, j])
      replaceData(proxy3, zz, resetPaging = FALSE)}  # important
  })
  
  default_params <- array(data=c(0.15,10,0.4,24),dim=c(4,1))
  
  zzz <- as.data.frame(default_params,
                       row.names = c("Marginal interest rate on ex-post borrowing (%)",
                                     "Repayment term of ex-post borrowing (months)",
                                     "Annual effective increase in cost financing through ex-post borrowing (%)",
                                     "Delay period in financing through ex-post borrowing (months)"))
  
  output$cbparamsborrowing = renderDT({
    dat <- datatable(zzz, selection = 'none', editable = TRUE, options = list(dom='t', bSort = F), colnames = c('Ex-post Sovereign Borrowing Assumptions' = 1, " " = 2))
  })
  
  proxy4 = dataTableProxy('cbparamsborrowing')
  
  observeEvent(input$cbparamsborrowing_cell_edit, {
    info = input$cbparamsborrowing_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    if (v > 1 & i != 2 & i !=4) {
      showModal(modalDialog(
        title = "Parameter Value Error",
        paste0("Value must be a decimal between 0 and 1 eg. 0.5 = 50%"),
        easyClose = TRUE,
        footer = NULL)) 
      if (i == 1)  zzz[i, j] <<- DT::coerceValue(0.15, zzz[i, j])
      if (i == 3)  zzz[i, j] <<- DT::coerceValue(0.4, zzz[i, j])
      replaceData(proxy4, zzz, resetPaging = FALSE) }
    else {
      zzz[i, j] <<- DT::coerceValue(v, zzz[i, j])
      replaceData(proxy4, zzz, resetPaging = FALSE) } # important
  })
  
  
  default_params <- array(data=c(1.5),dim=c(1,1))
  
  zy <- as.data.frame(default_params,
                      row.names = c("Sovereign Insurance pricing multiple"))
  
  output$cbparamsins = renderDT({
    dat <- datatable(zy, selection = 'none', editable = TRUE, options = list(dom='t', bSort = F), colnames = c('(Re) Insurance Assumptions' = 1, " " = 2))
  })
  
  proxy5 = dataTableProxy('cbparamsins')
  
  observeEvent(input$cbparamsins_cell_edit, {
    info = input$cbparamsins_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    zy[i, j] <<- DT::coerceValue(v, zy[i, j])
    replaceData(proxy5, zy, resetPaging = FALSE)  # important
  })
  
  ### Create the financial strategy input
  ### NNDIS, Contingency Fund, Contingent Credit, Budget Reallocation, Reins, Borrowing
  
  nndis.fact <- reactive({input$nndisfac})
  nndis.factor <- reactive({as.numeric(nndis.fact())})
  
  A.attach <- reactive({c(0,0,0,0,input$sov_ins[1]*1e+6,0)})
  A.exhaust <- reactive({c(input$nndis_ins*(100/nndis.factor())*1e+6,input$layer1*1e+6,input$layer2*1e+6,input$layer3*1e+6,input$sov_ins[2]*1e+6,16e+6*1e+6)})
  A.flag <- c(1,0,0,0,1,0)
  A.pct.loss <- reactive({c(nndis.factor(),100,100,100,100,100)})
  
  B.attach <- reactive({c(0,0,0,0,input$sov_insB[1]*1e+6,0)})
  B.exhaust <- reactive({c(input$nndis_insB*(100/nndis.factor())*1e+6,input$layer1B*1e+6,input$layer2B*1e+6,input$layer3B*1e+6,input$sov_insB[2]*1e+6,16e+6*1e+6)})
  B.flag <- c(1,0,0,0,1,0)
  B.pct.loss <- reactive({c(nndis.factor(),100,100,100,100,100)})
  
  
  C.attach <- reactive({c(0,0,0,0,input$sov_insC[1]*1e+6,0)})
  C.exhaust <- reactive({c(input$nndis_insC*(100/nndis.factor())*1e+6,input$layer1C*1e+6,input$layer2C*1e+6,input$layer3C*1e+6,input$sov_insC[2]*1e+6,16e+6*1e+6)})
  C.flag <- c(1,0,0,0,1,0)
  C.pct.loss <- reactive({c(nndis.factor(),100,100,100,100,100)})
  
  
  scale.up <- 100
  
  
  
  ###################################################################
  ## CBA Parameters                                                ##
  ###################################################################
  
  param_b = zzz[3,1] ##0.4
  param_d = y[2,1] ## 0.15
  param_t = zzz[4,1] ## 24
  param_e = zzz[1,1] ## 0.15
  param_n = zzz[2,1] ## 10
  param_m = zy[1,1] ## 1.5
  param_i = y[1,1] ## 0.13
  param_r = z[1,1] ## 0.01
  param_delta = cc[1,1] ## 0.005
  param_p = cc[2,1] ## 10
  param_c = cc[3,1] ## 0.038
  param_h = zz[1,1]
  
  switcher <- reactiveVal(value = FALSE)
  
  observeEvent(input$simulate,
               {
                 # Mark that the simulate button has been pushed
                 switcher(TRUE)
                 sim.losses <- simulation.data$SimulatedNNDISLoss*1000000*scale.up/isolate(nndis.factor())
                 showNotification("Running Tool")

                 ## ========================================== ##
                 ## Strategy A & B & C                         ##
                 ## ========================================== ##
                 
                 perils.percentiles <- calculate.percentiles(sim.losses,c(0.5,0.8,0.9,0.98,0.995))
                 peril.annual.average <- mean(sim.losses)
                 peril.exceedance.curve <- calculate.percentiles(sim.losses,seq(0.5,0.98,by=0.002))
                 
                 
                 ## Set strategy B and Strategy C attach and exhast to 0 depending on number of strategies
                 
                 if (input$nstrategies == "1") {
                   B.attach <- reactive({rep(0,6)})
                   B.exhaust <- reactive({rep(0,6)})
                   C.attach <- reactive({rep(0,6)})
                   C.exhaust <- reactive({rep(0,6)}) }
                 
                 if (input$nstrategies == "2") {
                   C.attach <- reactive({rep(0,6)})
                   C.exhaust <- reactive({rep(0,6)}) }
                 
                 
                 strategy.A.output <- calculate.financial.strategy.ceding.fast(sim.losses,A.attach(),A.exhaust(),A.flag,A.pct.loss(),scale.up)
                 average.fin.loss <- strategy.A.output[which(abs(strategy.A.output$loss - peril.annual.average)==min(abs(strategy.A.output$loss - peril.annual.average))),]
                 peril_1234.RP_losses <- c(peril.annual.average,perils.percentiles[c(1,3,4)])
                 peril_1234.fin.loss <- sapply(peril_1234.RP_losses, function(x)
                   strategy.A.output[which(abs(strategy.A.output$loss - x)==min(abs(strategy.A.output$loss - x)))[1],1:6])
                 
                 peril_1234.fin.loss <- unlist(peril_1234.fin.loss)
                 
                 
                 ## Strategy B ##
                 
                 strategy.B.output <- calculate.financial.strategy.ceding.fast(sim.losses,B.attach(),B.exhaust(),B.flag,B.pct.loss(),scale.up)
                 average.fin.lossB <- strategy.B.output[which(abs(strategy.B.output$loss - peril.annual.average)==min(abs(strategy.B.output$loss - peril.annual.average))),]
                 peril_1234.fin.lossB <- sapply(peril_1234.RP_losses, function(x)
                   strategy.B.output[which(abs(strategy.B.output$loss - x)==min(abs(strategy.B.output$loss - x)))[1],1:6])
                 
                 peril_1234.fin.lossB <- unlist(peril_1234.fin.lossB)
                 
                 
                 ## Strategy C ##
                 
                 strategy.C.output <- calculate.financial.strategy.ceding.fast(sim.losses,C.attach(),C.exhaust(),C.flag,C.pct.loss(),scale.up)
                 average.fin.lossC <- strategy.C.output[which(abs(strategy.C.output$loss - peril.annual.average)==min(abs(strategy.C.output$loss - peril.annual.average))),]
                 peril_1234.fin.lossC <- sapply(peril_1234.RP_losses, function(x)
                   strategy.C.output[which(abs(strategy.C.output$loss - x)==min(abs(strategy.C.output$loss - x)))[1],1:6])
                 
                 peril_1234.fin.lossC <- unlist(peril_1234.fin.lossC)
                 
                 
                 ## Strategy A & B & C- CBA Analysis
                 
                 
                 AAL_Layer11 <- rep(mean(strategy.A.output$Layer1.used),length(strategy.A.output$Layer1.used)) ## NNDIS insurance layer
                 AAL_Layer22 <- rep(mean(strategy.A.output$Layer5.used),length(strategy.A.output$Layer5.used)) ## Sovereign insurance layer
                 
                 cost_strategy_A <- cost_borrowing(strategy.A.output$Layer6.used,param_b,param_d,param_t,param_e,param_n) + cost_insurance(AAL_Layer11,param_m) +
                   cost_insurance(AAL_Layer22,param_m) + cost_reserve_fund(strategy.A.output$Layer2.used,A.exhaust()[2],param_i,param_r,param_d) +
                   cost_contingent_credit(strategy.A.output$Layer3.used,A.exhaust()[3],param_delta,param_p,param_d,param_c) +
                   cost_post_budget(strategy.A.output$Layer4.used,param_h,param_d)
                 
                 
                 AAL_Layer11B <- rep(mean(strategy.B.output$Layer1.used),length(strategy.B.output$Layer1.used)) ## NNDIS insurance layer
                 AAL_Layer22B <- rep(mean(strategy.B.output$Layer5.used),length(strategy.B.output$Layer5.used)) ## Sovereign insurance layer
                 
                 cost_strategy_B <- cost_borrowing(strategy.B.output$Layer6.used,param_b,param_d,param_t,param_e,param_n) + cost_insurance(AAL_Layer11B,param_m) +
                   cost_insurance(AAL_Layer22B,param_m) + cost_reserve_fund(strategy.B.output$Layer2.used,B.exhaust()[2],param_i,param_r,param_d) +
                   cost_contingent_credit(strategy.B.output$Layer3.used,B.exhaust()[3],param_delta,param_p,param_d,param_c) +
                   cost_post_budget(strategy.B.output$Layer4.used,param_h,param_d)
                 
                 
                 AAL_Layer11C <- rep(mean(strategy.C.output$Layer1.used),length(strategy.C.output$Layer1.used)) ## NNDIS insurance layer
                 AAL_Layer22C <- rep(mean(strategy.C.output$Layer5.used),length(strategy.C.output$Layer5.used)) ## Sovereign insurance layer
                 
                 cost_strategy_C <- cost_borrowing(strategy.C.output$Layer6.used,param_b,param_d,param_t,param_e,param_n) + cost_insurance(AAL_Layer11C,param_m) +
                   cost_insurance(AAL_Layer22C,param_m) + cost_reserve_fund(strategy.C.output$Layer2.used,C.exhaust()[2],param_i,param_r,param_d) +
                   cost_contingent_credit(strategy.C.output$Layer3.used,C.exhaust()[3],param_delta,param_p,param_d,param_c) +
                   cost_post_budget(strategy.C.output$Layer4.used,param_h,param_d)
                 
                 
                 write.csv(strategy.A.output,'Strategy_A_Test.csv',row.names=F)
                 write.csv(strategy.A.output,'Strategy_B_Test.csv',row.names=F)
                 write.csv(strategy.A.output,'Strategy_C_Test.csv',row.names=F)
                 
                 
                 cost_strategy_A_Average <- mean(cost_strategy_A)
                 cost.benefit.percentiles.A <- c(cost_strategy_A_Average,calculate.percentiles(cost_strategy_A,c(0.5,0.8,0.9,0.98,0.99))[c(1,3,4)])
                 
                 cost_strategy_B_Average <- mean(cost_strategy_B)
                 cost.benefit.percentiles.B <- c(cost_strategy_B_Average,calculate.percentiles(cost_strategy_B,c(0.5,0.8,0.9,0.98,0.99))[c(1,3,4)])
                 
                 cost_strategy_C_Average <- mean(cost_strategy_C)
                 cost.benefit.percentiles.C <- c(cost_strategy_C_Average,calculate.percentiles(cost_strategy_C,c(0.5,0.8,0.9,0.98,0.99))[c(1,3,4)])
                 
                 output$exceedanceplot <- renderPlot({
                   x <- peril.exceedance.curve
                   message('x is ...')
                   print(x)
                   plot.peril.exceedance.curve(
                     x)
                 })
                 output$oppcostplot <- renderPlot(plot.opportunity.cost(build.cba.plot.array(cost.benefit.percentiles.A,cost.benefit.percentiles.B,cost.benefit.percentiles.C)))
                 output$financialplot <- renderPlot(plot.financial.strategy(build.plot.array(peril_1234.fin.loss,peril_1234.fin.lossB,peril_1234.fin.lossC)))
                 
                 ## Data table output
                 opp.cost.table <- rbind(cost.benefit.percentiles.A,cost.benefit.percentiles.B,cost.benefit.percentiles.C)
                 row.names(opp.cost.table) <- c("Strategy A", "Strategy B", "Strategy C")
                 colnames(opp.cost.table) <- c("Average Cost", "1 in 2", "1 in 10", "1 in 50")
                 output$oppcosttable = renderDT({
                   dat <- datatable(round(opp.cost.table/1000000), selection = 'none', editable = F, options = list(dom='t',bSort = FALSE))
                   return(dat)})
                 
                 
               }) # end observe event
  
  output$ui_exceedanceplot <- renderUI({
    ss <- switcher()
    if(ss){
      plotOutput('exceedanceplot')
    } else {
      NULL
    }
  })
  output$ui_oppcosttable <- renderUI({
    ss <- switcher()
    if(ss){
      DTOutput('oppcosttable')
    } else {
      NULL
    }
  })
  output$ui_financialplot <- renderUI({
    ss <- switcher()
    if(ss){
      plotOutput('financialplot')
    } else {
      NULL
    }
  })
  output$ui_oppcostplot <- renderUI({
    ss <- switcher()
    if(ss){
      plotOutput('oppcostplot')
    } else {
      NULL
    }
  })
  
  
} # end server

# Run the application 
shinyApp(ui = ui, server = server) 

