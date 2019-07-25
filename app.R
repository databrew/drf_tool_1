# DRF Financing Tool Sri Lanka
source('global.R')

# setwd('C:\\Users\\wb529583\\OneDrive - WBG\\Sri Lanka\\2019_Analysis,Tool,andNNDISReview\\StructuredFinancingTools\\Bangkok_Tool\\Bangkok_Tool\\Contingent_Liability\\')

ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = "DRF Tool 1"),
                    dashboardSidebar(
                      sidebarMenu( id = 'menu',
                                   menuItem("Overview", tabName = 'overview', icon = icon('home')),
                                   menuItem("Assumptions & Methodology", tabName = 'methodology', icon = icon('info')),
                                   menuItem("Parameters", tabName = 'cbaparameters', icon = icon('edit')),
                                   menuItem("Data", tabName = 'data', icon = icon('table')),
                                   menuItem("Output", tabName = 'output', icon = icon('signal')),
                                   actionButton("simulate", "Run Tool", icon('refresh'), style = "color: #fff;background-color: #337ab7; border-color: #2e6da4", width="70%"),
                                   br(),
                                   br(),
                                   awesomeCheckbox("advanced", "Advanced Settings", FALSE,status = 'danger'),
                                   conditionalPanel("input.advanced",
                                                    textInput('nndisfac',label= 'NNDIS Factor (%)', value = "2.5", width = "60%")) 
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
                        
                        tabItem(tabName = 'methodology') # end tabItem
                      
                             
                        )
                        )
)

server <- function(input, output) {
 
  
} # end server

# Run the application 
shinyApp(ui = ui, server = server) 

