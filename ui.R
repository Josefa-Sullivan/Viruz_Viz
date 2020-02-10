# bugs in app mean check UI
library(markdown)

navbarPage("Coronavirus Visualization",
           inverse=T,
           tabPanel("Welcome",
                      mainPanel(fluidRow(column(8, align="center", offset = 2,
                                                box(htmlOutput('intro_header'), br(),htmlOutput('intro_author'),
                                                    htmlOutput('intro_contact'), uiOutput("tab"), width = 20),
                                                tags$style(type="text/css", "#string { text-align:center }"))),
                                br(),
                                fluidRow(column(10, align="center", offset = 1,
                                                div(img(
                                                  src="https://globalbiodefense.com/wp-content/uploads/2020/02/coronavirus-entering-lungs-protein-databank-david-s-goodsell.jpg", height=350, width=350)),
                                                br(),
                                                br(),
                                                box(htmlOutput("intro_body1"), width=20),
                                                
                                                tags$style(type="text/css", "#string { text-align:justified }")))
                      
                    )
           ),
           
           tabPanel("Map",
                 sidebarLayout(
                     sidebarPanel(
                         h2("Global Spread of the Coronavirus"),
                         br(),
                         chooseSliderSkin("Flat"),
                         sliderInput(
                             label=h4('Date'),
                             inputId = "date_slide",
                             width= "25%",
                             max = slider_dates[1],
                             min = tail(slider_dates,1),
                             value = slider_dates[1],
                             animate= animationOptions(interval=500))
                         ),
                     
                 
                     mainPanel(
                         leafletOutput("virus_map")
                     )
                 )
           ),
           
           tabPanel("Timeline",
                    sidebarLayout(
                        sidebarPanel(
                            h2("Timeline of Infections and Deaths"),
                            br(),
                            checkboxInput("cases", "Cases", value = TRUE),
                            checkboxInput("deaths", "Deaths", value = FALSE),
                            checkboxInput("recoveries", "Recoveries", value = FALSE)
                            ),
                    
                    mainPanel(
                           fluidRow(column(6, htmlOutput('virus_timeline_plot')),
                                    column(6, htmlOutput('ebola_timeline_plot')))
                    )
                    )
           ),
           
           tabPanel("Comparisons",
                    sidebarLayout(
                        sidebarPanel(
                            h2("Comparing to other Virus Outbreaks"),
                            br(),
                            checkboxInput("cases_all", "Cases", value = TRUE),
                            checkboxInput("deaths_all", "Deaths", value = FALSE),
                            checkboxInput("mort_rate", "Mortality Rate", value = FALSE)
                        ),

                        mainPanel(
                            fluidRow(htmlOutput('comparison_plot'))
                        )
                    )
           )
)
