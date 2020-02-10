
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
        p('A new coronavirus has emerged in Wuhan, China. 
          With more cases every day, there is concern that this is a deadly pandemic like none other. My goal is to track the Coronavirus and compare patient outcomes to 
          previous viral outbreaks. By looking at the data, I hope to dispel the fear surrounding the coronavirus.')
    })
    
    output$intro_credit= renderUI({
        p('Image Credit: David S. Goodsell, RCSB Protein Data Bank')
    })
    
    output$source1= renderUI({
        url = a("HDX", href="https://github.com/Josefa-Sullivan/Viruz_Viz")
        tagList("Ebola Outbreak 2014: ", url)
    })
    
    output$source2= renderUI({
        url = a("JHU CSSE", href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6")
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
    
    

    
############ Plot the number of Coronavirus cases per country over time #############################
## Future Work: Add Scale legend tied to size of circleMarkers, Add popup values when you hover, 
## Future Work: Color-coding the countries based on case number would be better for visualization than the circles
    
    map_date = reactive({
        df = timeseries_df %>% mutate(date=as.Date(timeseries_df$date)) %>%
            filter(date == input$date_slide)
        df
    })


    
    output$virus_map = renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite) %>% 
            setView(0, 0, zoom=1.25) %>% 
            addFullscreenControl(position = "topleft", pseudoFullscreen = T)

    })

    observeEvent(input$date_slide, {
        leafletProxy("virus_map") %>% 
            clearMarkers() %>% 
            addCircleMarkers(lat = map_date()$Lat,
                             lng = map_date()$Long,
                             radius= sqrt(map_date()$case_number)/4,
                             fillOpacity = 0.5,
                             opacity=0.1,
                             color ='red')
                             
    })
    
    
######### Add Timelines of Coronavirus and Ebola data  ###########################
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
            title="Timeline of Coronavirus Outcomes",
            hAxis="{title:'Date', fontName:'Tahoma',fontSize:25}",
            vAxis="{title:'Number', fontName:'Tahoma',fontSize:25}", 
            width=700,
            height=500,
            colors = "['#970b13','#feb441', '#710026']",
            titleTextStyle="{color: '#3d4854', fontName:'Tahoma', fontSize:20}",
            legend = "{position: 'bottom'}")
        
        
        # Plot timecourse of total coronavirus data in a line graph
        gvisLineChart(timeline(), xvar="date", 
                      yvar = plot_picks,
                      options=my_options)
        
    })
    
    # Make ebola df reactive
    ebola_timeline= reactive({
        ebola_time_df = ebola_timeseries
        ebola_time_df
    })
    

    # Plot timecourse of total Ebola Cases in a GVis line chart   
    output$ebola_timeline_plot = renderGvis({
       
        plot_picks_2 = c()
        if (input$cases){
            plot_picks_2 = append(plot_picks_2, "cases")
        }
        if (input$deaths){
            plot_picks_2 = append(plot_picks_2, "deaths")
        }

        my_options2 = list(
            title="Timeline of Ebola Outcomes",
            hAxis="{title:'Date', fontSize:25}",
            vAxis="{title:'Number', fontSize:25}", 
            width=700,
            height=500,
            colors = "['#970b13','#feb441', '#710026']",
            titleTextStyle="{color: '#3d4854', fontName:'Verdana', fontSize:20}",
            legend = "{position: 'bottom'}")
        
        
        # Plot timeline
        gvisLineChart(ebola_timeline(), xvar="date", 
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
                       colors = "['#970b13','#feb441', '#710026']",
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
                           colors = "['#970b13','#feb441', '#710026']",
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


