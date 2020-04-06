
function(input, output, session){
    

############### Create an Intro Page ##############################################################
    
    output$intro_header = renderUI({
        h1('Tracking the Coronavirus')
    })
    
    output$intro_author = renderUI({
        h3('By Josefa Sullivan')
    })
    
    output$here <- renderUI({
        url = a("here!", href="josefa.sullivan@gmail.com")
        tagList("Have Questions? Contact me ", url)
    })
    
    output$tab <- renderUI({
        url = a("GitHub", href="https://github.com/Josefa-Sullivan/Viruz_Viz")
        tagList("All code is also available on ", url)
    })
    
    output$intro_body1= renderUI({
        p('A new coronavirus, SARS-CoV-2, emerged in Wuhan, China in late 2019. 
          This app tracks the pandemic as case numbers increase across the globe 
          and compares patient outcomes to previous viral outbreaks.')
    })
    
    output$intro_credit= renderUI({
        p('Image Credit: David S. Goodsell, RCSB Protein Data Bank')
    })
    
    output$source1= renderUI({
        url = a("HDX", href="https://data.humdata.org/dataset/ebola-cases-2014")
        tagList("Ebola Outbreak 2014: ", url)
    })
    
    output$source2= renderUI({
        url = a("JHU CSSE", href="https://github.com/CSSEGISandData/COVID-19")
        tagList("Coronavirus Outbreak 2019: ", url)
    })
    
    output$source3= renderUI({
        url = a("WHO", href="https://www.who.int/csr/sars/country/en/")
        tagList("Severe Acute Respiratory Syndrome (SARS) Outbreak 2003: ", url)
    })
    
    output$source4= renderUI({
        url = a("CDC", href="https://www.cdc.gov/flu/about/burden/preliminary-in-season-estimates.htm")
        tagList("Influenza (Flu) Outbreak 2020: ", url)
    })
    
    
  #  

    
############ Plot the number of Coronavirus cases per country over time #############################
    
    map_date = reactive({
      country_date_df[which(country_date_df$date == input$date_slide),]
    })


    
    observeEvent(input$date_slide, {
      countries$Value = map_date()$cases[ match(countries$country, map_date()$Country.Region) ]
      
      labels <- paste("Country: ", countries$country,"<br/>",
                      "Total Cases: ", countries$Value, "<br/>",
                      sep="") %>%
                lapply(htmltools::HTML)
      
      leafletProxy("virus_map") %>%
            clearGroup('polygons') %>% 
            addPolygons(data=countries, 
                        group = 'polygons',
                        weight=0.5,
                        opacity = 1,
                        color = "black",
                        fillColor = ~pal(countries$Value),
                        label = ~labels,
                        fillOpacity = 1,
                        highlightOptions = highlightOptions(color = "white", 
                                                            weight = 2,
                                                            bringToFront = TRUE))
    })  
    
    pal <- colorBin("YlOrRd", domain = countries$Value, bins = bins, na.color = 'transparent')

    output$virus_map = renderLeaflet({
            countries$Value = map_date()$cases[ match(countries$country, map_date()$Country.Region) ]
            
            labels <- paste("Country: ", countries$country,"<br/>",
                            "Total Cases: ", countries$Value, "<br/>",
                            sep="") %>%
              lapply(htmltools::HTML)
            
            leaflet() %>% 
                  addTiles(options = providerTileOptions(noWrap = TRUE)) %>% 
                  addFullscreenControl(position = "topleft", pseudoFullscreen = T) %>%
                  setView(0.5,1,2.2) %>% 
                  addLegend(pal = pal, values = countries$Value, 
                            opacity = 0.7, title = NULL,
                            position = "bottomright") %>% 
                  addPolygons(data=countries, 
                              group = 'polygons',
                              weight=0.5,
                              opacity = 1,
                              color = "black",
                              fillColor = ~pal(countries$Value),
                              label = ~labels,
                              fillOpacity = 1,
                              highlightOptions = highlightOptions(color = "white", 
                                                                  weight = 2,
                                                                  bringToFront = TRUE))
        })
      

    
    
      
      
      
      
    
#########  Plot Daily and Cumulative Numbers for COVID-19 ###########################
     # Calculate the total number of cases, deaths and recoveries for each date
    timeline= reactive({
         time_df = timeseries_df %>% group_by(date) %>%
             summarise(cases = sum(case_number),
                       deaths = sum(death_number),
                       recoveries = sum(recovered_number))
         time_df
     })

    
    # Create line graph of Coronavirus cases, deaths and recoveries over time based on user input
    output$virus_timeline_plot = renderGvis({
        plot_picks = c()
        
        if (input$cases){
            plot_picks = append(plot_picks, "cases")
        }
        
        if (input$deaths){
            plot_picks = append(plot_picks, "deaths")
        }
        
        if (input$recoveries){
            plot_picks = append(plot_picks, "recoveries")
        }
        
        my_options = list(
            title="Cumulative Timeline",
            hAxis="{title:'Date', fontName:'Tahoma',fontSize:25}",
            vAxis="{title:'Number', fontName:'Tahoma',fontSize:25}",
            width='100%',
            height=400,
            colors = "['#bc0025','#710026', '#ee7516']",
            titleTextStyle="{color: '#3d4854', fontName:'Tahoma', fontSize:15}",
            legend = "{position: 'bottom'}")
        
        
        # Plot timecourse of total coronavirus data in a line graph
        gvisLineChart(timeline(), xvar="date", 
                      yvar = plot_picks,
                      options=my_options)
        
    })
    

    
    # Make cumlate df reactive
    daily_timeline= reactive({
        daily_numbers_df
    })
    

    # Plot timecourse of total Ebola Cases in a GVis line chart   
    output$daily_timeline_plot = renderGvis({
       
        plot_picks_2 = c()
        if (input$cases){
            plot_picks_2 = append(plot_picks_2, "cases")
        }
        if (input$deaths){
            plot_picks_2 = append(plot_picks_2, "deaths")
        } 
        if (input$recoveries){
          plot_picks_2 = append(plot_picks_2, "recoveries")
        }

        my_options2 = list(
            title="Daily Timeline",
            hAxis="{title:'Date', fontSize:25}",
            vAxis="{title:'Number', fontSize:25}", 
            width='100%',
            height=400,
            bar="{groupWidth:50}",
            colors = "['#bc0025','#710026', '#ee7516']",
            titleTextStyle="{color: '#3d4854', fontName:'Verdana', fontSize:15}",
            legend = "{position: 'bottom'}")
        
        
        # Plot timeline
        gvisColumnChart(daily_timeline(), xvar="date", 
                      yvar = plot_picks_2,
                      options=my_options2)
        })
        
    
######### Add a barplot to compare the different viruses ###########################
    # Make a reactive df for barchart
    virus_bar = reactive({
        virus_bar_df = stats_df
        virus_bar_df
    })

    # Bar plots of case number, deaths and mortality rates for 5 viruses (Flu 1918, Flu 2020, Coronavirus, Ebola, SARS)
    output$comparison_plot = renderGvis({
        
        my_list = list(title="Comparison of Viruses",
                       height=600,
                       width=1000,
                       vAxis="{scaleType: 'log', title:'Number (log)', fontName:'Verdana', fontSize:20}",
                       colors = "['#bc0025','#710026', '#ee7516']",
                       titleTextStyle="{color: '#3d4854', fontName:'Verdana', fontSize:20}",
                       legend = "{position: 'right'}")
        
        plot_picks_3 = c()
        
        if (input$cases_all){
            plot_picks_3 = append(plot_picks_3, "Cases")
        }
        
        if (input$deaths_all){
            plot_picks_3 = append(plot_picks_3, "Deaths")
        }
        
        if (input$mort_rate){
            plot_picks_3 = append(plot_picks_3, "Mortality_Rate")
            my_list = list(title="Comparison of Viruses",
                           height=600,
                           width=1000,
                           vAxis="{title:'Number',fontName:'Verdana', fontSize:20}",
                           colors = "['#bc0025','#710026', '#ee7516']",
                           titleTextStyle="{color: '#3d4854', fontName:'Verdana', fontSize:20}",
                           legend = "{position: 'right'}")
        }

        print(plot_picks_3)
         gvisColumnChart(virus_bar(),
                         xvar='virus',
                         yvar=plot_picks_3,
                         options=my_list)

    })

}


