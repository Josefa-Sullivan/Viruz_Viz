

navbarPage("Coronavirus Visualization",
           inverse=T,
           tabPanel("Welcome",
                      mainPanel(fluidRow(column(8, align="center", offset = 4,
                                                box(htmlOutput('intro_header'), br(),htmlOutput('intro_author'),
                                                    uiOutput('here'), uiOutput("tab"), width = 20),
                                                tags$style(type="text/css", "#string { text-align:center }",
                                                           "#intro_header{ font-size: 20px}"))),
                                br(),
                                fluidRow(column(8, align="center", offset = 4,
                                                div(img(
                                                  src="https://globalbiodefense.com/wp-content/uploads/2020/02/coronavirus-entering-lungs-protein-databank-david-s-goodsell.jpg", height=450, width=450)),
                                                htmlOutput("intro_credit"),
                                                br(),
                                                box(htmlOutput("intro_body1"), width=20),
                                                tags$style(type="text/css", "#string { text-align:justified }", "#intro_body1 { font-size: 20px}")))
                                 )
           ),
           
           tabPanel("Map",
                 sidebarLayout(
                     sidebarPanel(
                         h2("Global Spread of the Coronavirus"),
                         h5('Coronavirus Cases confirmed by the World Health Organization shown in red'),
                         chooseSliderSkin("Flat"),
                         sliderInput(
                             label=NULL,
                             inputId = "date_slide",
                             width= "50%",
                             max = slider_dates[1],
                             min = tail(slider_dates,1),
                             value = tail(slider_dates,1),
                             animate= animationOptions(interval=500))
                         
                         ),
                 
                     mainPanel(
                         leafletOutput("virus_map")
                     )
                 )
           ),
           
           tabPanel("Timeline",
                    sidebarLayout(
                        sidebarPanel(width=3,
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
           ),
           
           tabPanel("Sources",
                      mainPanel(h3("Datasets Sources"),br(),
                                uiOutput("source1"), br(),
                                uiOutput("source2"), br(),
                                uiOutput("source3"), br(),
                                uiOutput("source4"))

                      )
                    

)
