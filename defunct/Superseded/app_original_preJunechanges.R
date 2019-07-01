## Contingent Liability tool for John

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

setwd('C:\\Users\\WB541925\\OneDrive - WBG\\2019\\03. Tools\\Bangkok_Tool\\Contingent_Liability\\')

popn.data <- read.csv(".\\lib\\RD_POP.csv",header=TRUE,stringsAsFactors=F)
archetype.data <- read.csv(".\\lib\\RD_NNDISHistoricLosses.csv",header=TRUE,stringsAsFactors = F)
simulation.data <- read.csv(".\\lib\\RD_NNDISSimulatedLossProfile.csv",header=TRUE,stringsAsFactors = F)
source(".\\lib\\cost_benefit_calculations_upd.R")
source(".\\lib\\functions.R")


ui <- dashboardPage(skin = 'blue',
        dashboardHeader(title = "Sri Lanka"),
        dashboardSidebar(
          sidebarMenu( id = 'menu',
          menuItem("Overview", tabName = 'overview', icon = icon('home')),
          menuItem("Data", tabName = 'data', icon = icon('table')),
          menuItem("Financial Strategy", tabName = 'parameters', icon = icon('coins')),
          menuItem("Parameters", tabName = 'cbaparameters', icon = icon('edit')),
          menuItem("Output", tabName = 'output', icon = icon('signal'))),
          actionButton("simulate", "Run Tool", icon('refresh'), style = "color: #fff;background-color: #337ab7; border-color: #2e6da4", width="70%"),
          br(),
          br(),
          awesomeCheckbox("advanced", "Advanced Settings", FALSE,status = 'danger'),
          conditionalPanel("input.advanced",
                            textInput('nndisfac',label= 'NNDIS Factor (%)', value = "2.5", width = "60%")) 
          ),
        dashboardBody( 
          tags$head(tags$style(HTML('
                                                 /* body */
                                                 .content-wrapper, .right-side {
                                                 background-color: #FFFFFF;
                                                 }'))),
          
         
          tabItems( 
            tabItem(tabName = 'overview',
          br(),
          img(src="SL_Flag.png", height = 50, width = 80, align = 'top'), br(" "),
          img(src="DRFIP_Logo.png", height = 50, width = 100, align = 'top'),
          br("  "),
          
          h2("Disaster Risk Financing in Sri Lanka"),
          tags$hr(), 
          h3("Overview"),
          p("This Tool has been developed by the World Bank, with the support of the NNDIS, to support better understandinding withing the 
            NNDIS and it's stakeholders on key decisions it must make during financial risk management. 
            The Tool is pre-loaded with burning cost loss data for the NNDIS and a loss distribution based on this data.
            The Tool is limited by the quality of the data from these sources, and hence this will affect the accuracy of the indicative fiscal costing obtained. 
            The output from this Tool remains an indication of the costs associated with the inputed strategy, and the total fiscal cost may differ significantly from the Tool's output.", style = "font-size:100%"),
          p("The tool is currently in DRAFT for discussion and feedback only.", style = "font-size:100%"), 
          br(),
          h3("Authorship"),
          p("The development of this Tool was led by the Disaster Risk Financing and Insurance Program (DRFIP), a partnership of the World Bank Group's Finance Competitiveness and Innovation Global Practice and the Global Facility for Disaster Reduction and Recovery (GFDRR).", style = "font-size:100%"), br(),
          br(),
          h3("Disclaimer"),
          p("This Tool has been developed by the World Bank to develop capacity of the World Bank partners on key decisions they must make during disaster risk financing. The Tool is intended for use as outlined above and it should not be used for any other purposes. The Tool should not be used to inform real financial decisions.
            Information in the Tool is provided for educational purposes only and does not constitute legal or scientific advice or service. 
            The World Bank makes no warranties or representations, express or implied as to the accuracy or reliability of the Tool or the data contained therein. A user of the Tool should seek qualified expert advice for specific diagnosis and analysis of a particular project. Any use thereof or reliance thereon is at the sole and independent discretion and responsibility of the user. No 
            conclusions or inferences drawn from the Tool should be attributed to the World Bank, its Board of Executive Directors, its Management, or any of its member countries.
            This Tool does not imply any judgement or endorsement on the part of the World Bank. In no event will GAD or the World Bank be liable for any form of damage arising from the application or misapplication of the Tool, or any associated materials.", style = "font-size:100%"),
          br(), 
          h3("Confidentiality"),
          p("The World Bank invests substantial resources in the development of its models, modelling methodologies and databases. This Tool contains proprietary and confidential information and is intended for the exclusive use of World Bank partners with whom this Tool has been shared. Any user is subject to the restrictions of the confidentiality provisions set forth in license and other nondisclosure agreements.", style = "font-size:100%"), br(),
          br()), # end tabItem
        tabItem(tabName = 'parameters',
                h2("Financial Strategy"),
                h5("Select parameters to apply when you run the Tool"),
                br(),
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
                    sliderInput('layer4','Maximum Ex-Post Borrowing:',
                                min = 0, max = 16000000,
                                  value = 16000000, step = 10000),
                    sliderInput('sov_ins', 'Sovereign Insurance:',
                                min = 0, max = 100000,
                                value = c(40000,60000), step = 10000),
                    sliderInput('nndis_ins', 'NNDIS:',
                                min=0, max=15000,
                                value = 15000,step = 1000)
                    ),
                    box( width = 4, title = '2. Select Financial Strategy B', status = 'warning', solidHeader = T,
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
                         sliderInput('layer4B','Maximum Ex-Post Borrowing:',
                                     min = 0, max = 16000000,
                                     value = 16000000, step = 10000),
                         sliderInput('sov_insB', 'Sovereign Insurance:',
                                     min = 0, max = 100000,
                                     value = c(40000,60000), step = 10000),
                         sliderInput('nndis_insB', 'NNDIS:',
                                     min=0, max=16000,
                                     value = 15000,step = 1000)
                    ),
                    box( width = 4, title = '3. Select Financial Strategy C', status = 'success', solidHeader = T,
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
                         sliderInput('layer4C','Maximum Ex-Post Borrowing:',
                                     min = 0, max = 16000000,
                                     value = 16000000, step = 10000),
                         sliderInput('sov_insC', 'Sovereign Insurance:',
                                     min = 0, max = 100000,
                                     value = c(40000,60000), step = 10000),
                         sliderInput('nndis_insC', 'NNDIS:',
                                     min=0, max=16000,
                                     value = 15000,step = 1000)
                    ))), # end box, column, tabItem
                   
        tabItem(tabName = 'cbaparameters',
                h2("Parameters"),   
                h5('Edit the indicative assumptions below, percentages should be input as decimals'),
                br(),
                box( width = 8, title = '4. Cost-Benefit Parameters', status  = 'primary', solidHeader = T,
                         column(6,
                                DTOutput('cbparams'),
                                br(),
                                br(),
                                br(),
                                DTOutput('cbparamsreserve'),
                                br(),
                                br(),
                                DTOutput('cbparamsccredit')
                                ),
                         
                         column(6,
                                DTOutput('cbparamsborrowing'),
                                br(),
                                br(),
                                DTOutput('cbparamsins'),
                                br(),
                                br(),
                                DTOutput('cbparamsbudget')
                         ))),   # end tab item
        tabItem(tabName = 'data',
                h2("NNDIS Loss Data"),
                br(),
                fluidRow( column (10, offset = 1,
                                  br(),    
                                  plotOutput('historicalplot'))
                  ) # end fluid row
        ), # end tab item
        
        tabItem(tabName = 'output',
                fluidRow( 
                  column(5, offset = 1,
                         box(width = 12, height = 400, title = "Exceedance Loss Curve", status = 'primary', solidHeader = T,
                             br(),
                             plotOutput('exceedanceplot'))),
                  column(5, 
                         box(width = 12, height = 400, title = 'Opportunity Cost Table', status = 'primary', solidHeader = T,
                             DTOutput('oppcosttable')))),
                fluidRow(
                  column(5, offset = 1,
                         box(width = 12, height = 400, title = 'Breakdown of Loss Funded by Each Funding Source', status = 'primary', solidHeader = T,
                             plotOutput('financialplot'))),
                  column(5,
                         box(width= 12, height = 400, title = 'Potential opportunity cost of funding losses for different \n magnitudes of loss over the next year under each DRF strategy',
                             status = 'primary', solidHeader = T,
                             plotOutput('oppcostplot'))))
                
        ) # end tab item
        
        ) # end tabItems
           
        )
        )


server <- function(input, output) {
  
  
  ### PLOTS ##
  
  ## Maximum historical and recent historical
  output$historicalplot <- renderPlot(plot.historical(archetype.data, 'NNDIS'), width = 800, height = 300)
  
  
  ## -------------------------------------------------- ##
  ## Parameters Box                                     ##
  ## -------------------------------------------------- ##
  
  
  default_params <- array(data=c(0.13,0.15),dim=c(2,1))
  
  y <- as.data.frame(default_params,
                     row.names = c("Marginal interest rate on sovereign debt",
                                   "Discount factor"))
  
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
    y[i, j] <<- DT::coerceValue(v, y[i, j])
    replaceData(proxy1, y, resetPaging = FALSE)  # important
  })
  
  default_params <- array(data=c(0.005,10,0.038),dim=c(3,1))
  
  cc <- as.data.frame(default_params,
                     row.names = c("Arrangement fee for contingent credit",
                                   "Repayment term of contingenet credit",
                                   "Interest rate on contingent credit"))
  
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
    cc[i, j] <<- DT::coerceValue(v, c[i, j])
    replaceData(proxy7, cc, resetPaging = FALSE)  # important
  })
  
  default_params <- array(data=c(0.01),dim=c(1,1))
  
  z <- as.data.frame(default_params,
                     row.names = c("Investment on unspent reserves"))
  
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
    z[i, j] <<- DT::coerceValue(v, z[i, j])
    replaceData(proxy2, z, resetPaging = FALSE)  # important
  })
  
  default_params <- array(data=c(0.12),dim=c(1,1))
  
  zz <- as.data.frame(default_params,
                      row.names = c("Social rate of return on projects not funded due to reallocation of budgets"))
  
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
    zz[i, j] <<- DT::coerceValue(v, zz[i, j])
    replaceData(proxy3, zz, resetPaging = FALSE)  # important
  })
  
  default_params <- array(data=c(0.15,10,0.4,24),dim=c(4,1))
  
  zzz <- as.data.frame(default_params,
                       row.names = c("Marginal interest rate on ex-post borrowing",
                                     "Repayment term of ex-post borrowing ",
                                     "Annual effective increase in cost financing through ex-post borrowing",
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
    zzz[i, j] <<- DT::coerceValue(v, zzz[i, j])
    replaceData(proxy4, zzz, resetPaging = FALSE)  # important
  })
  
  
  default_params <- array(data=c(1.5),dim=c(1,1))
  
  zy <- as.data.frame(default_params,
                      row.names = c("Sovereign Insurance pricing multiple"))
  
  output$cbparamsins = renderDT({
    dat <- datatable(zy, selection = 'none', editable = TRUE, options = list(dom='t', bSort = F), colnames = c('(Re)Insurance Assumptions' = 1, " " = 2))
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
  A.exhaust <- reactive({c(input$nndis_ins*(100/nndis.factor())*1e+6,input$layer1*1e+6,input$layer2*1e+6,input$layer3*1e+6,input$sov_ins[2]*1e+6,input$layer4*1e+6)})
  A.flag <- c(1,0,0,0,1,0)
  A.pct.loss <- reactive({c(nndis.factor(),100,100,100,100,100)})
  
  B.attach <- reactive({c(0,0,0,0,input$sov_insB[1]*1e+6,0)})
  B.exhaust <- reactive({c(input$nndis_insB*(100/nndis.factor())*1e+6,input$layer1B*1e+6,input$layer2B*1e+6,input$layer3B*1e+6,input$sov_insB[2]*1e+6,input$layer4B*1e+6)})
  B.flag <- c(1,0,0,0,1,0)
  B.pct.loss <- reactive({c(nndis.factor(),100,100,100,100,100)})
  
  
  C.attach <- reactive({c(0,0,0,0,input$sov_insC[1]*1e+6,0)})
  C.exhaust <- reactive({c(input$nndis_insC*(100/nndis.factor())*1e+6,input$layer1C*1e+6,input$layer2C*1e+6,input$layer3C*1e+6,input$sov_insC[2]*1e+6,input$layer4C*1e+6)})
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
  
  observeEvent(input$simulate,
               {
                 sim.losses <- simulation.data$SimulatedNNDISLoss*1000000*scale.up/isolate(nndis.factor())
                 showNotification("Running Tool")
                 
                 ## ========================================== ##
                 ## Strategy A & B & C                         ##
                 ## ========================================== ##
                 
                 perils.percentiles <- calculate.percentiles(sim.losses,c(0.5,0.8,0.9,0.98,0.995))
                 peril.annual.average <- mean(sim.losses)
                 peril.exceedance.curve <- calculate.percentiles(sim.losses,seq(0.5,0.98,by=0.002))
                 
                 ## Strategy A ##
                 
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
                 
                 output$exceedanceplot <- renderPlot(plot.peril.exceedance.curve(peril.exceedance.curve), width = 600, height = 300)
                 output$oppcostplot <- renderPlot(plot.opportunity.cost(build.cba.plot.array(cost.benefit.percentiles.A,cost.benefit.percentiles.B,cost.benefit.percentiles.C)), width = 600, height = 300)
                 output$financialplot <- renderPlot(plot.financial.strategy(build.plot.array(peril_1234.fin.loss,peril_1234.fin.lossB,peril_1234.fin.lossC)), width = 600, height = 300)
                 
                 ## Data table output
                 opp.cost.table <- rbind(cost.benefit.percentiles.A,cost.benefit.percentiles.B,cost.benefit.percentiles.C)
                 row.names(opp.cost.table) <- c("Strategy A", "Strategy B", "Strategy C")
                 colnames(opp.cost.table) <- c("Average Cost", "1 in 2", "1 in 10", "1 in 50")
                 output$oppcosttable = renderDT({
                   dat <- datatable(round(opp.cost.table/1000000), selection = 'none', editable = F, options = list(dom='t',bSort = FALSE))
                   return(dat)})
                 
                 
                 }) # end observe event
  
  
  
    
} # end server

# Run the application 
shinyApp(ui = ui, server = server) 

