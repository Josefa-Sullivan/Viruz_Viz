
function(input, output, session){
############### Create an Intro Page
    
    output$intro_header = renderUI({
        h1('Tracking the Coronavirus')
    })
    
    output$intro_author = renderUI({
        h4('Josefa Sullivan')
    })
    
    output$intro_contact = renderUI({
        h5('Have Questions? Contact me @ZefaSullivan or josefa.sullivan@gmail.com
           ')
    })
    
    output$tab <- renderUI({
        url = a("GitHub", href="https://github.com/Josefa-Sullivan")
        tagList("All code is also available on GitHub", url)
    })

    
    output$intro_body1= renderUI({
        p('A new coronavirus has emerged in Wuhan, China. 
          With more and more people becoming infected every day, there is a lot of concern that 
          this virus is a deadly pandemic like none other. My goal was to track the Coronavirus and compare patient outcomes to 
          previous viral outbreaks. By looking at the data, I hope to dispel some of the fear dominating our current news cycle.')
        
    })
    
    output$myImage = renderImage({
        
    })
    
############ Plot the number of Coronavirus cases per country over time #############################
    map_date = reactive({
        df = timeseries_df %>% mutate(date=as.Date(timeseries_df$date)) %>%
            filter(date == input$date_slide)
        df
    })


    
    output$virus_map = renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite) %>% 
            setView(0, 0, zoom=1.25) %>% 
            addFullscreenControl(position = "topleft", pseudoFullscreen = F)

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

    
    # Plot timecourse of total coronavirus data in a line graph
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
            hAxis="{title:'Date '}",
            vAxis="{title:'Number'}", 
            width=700,
            height=500,
            colors = "['#da602f', '#67001f','#fcdb6d']",
            titleTextStyle="{color: '#3d4854', fontName:'Verdana', fontSize:15}",
            legend = "{position: 'bottom'}")
        
        
        # Plot timeline
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

        my_options = list(
            title="Timeline of Ebola Outcomes",
            hAxis="{title:'Date '}",
            vAxis="{title:'Number'}", 
            width=700,
            height=500,
            colors = "['#da602f', '#67001f','#fcdb6d']",
            titleTextStyle="{color: '#3d4854', fontName:'Verdana', fontSize:15}",
            legend = "{position: 'bottom'}")
        
        
        # Plot timeline
        gvisLineChart(ebola_timeline(), xvar="date", 
                      yvar = plot_picks_2,
                      options=my_options)
        })
        
    
######### Add a barplot to compare the different viruses ###########################
    # Make a reactive df for barchart
    virus_bar = reactive({
        virus_bar_df = stats_df
        virus_bar_df
    })

    output$comparison_plot = renderGvis({
        
        my_list = list(title="Comparison of Viruses",
                       height=800,
                       width=1000,
                       hAxis="{title:'Virus', fontName:'Verdana', fontSize:15}",
                       vAxis="{scaleType: 'log', title:'Number (log)', fontName:'Verdana', fontSize:15}",
                       colors = "['#da602f', '#67001f','#fcdb6d']",
                       titleTextStyle="{color: '#3d4854', fontName:'Verdana', fontSize:15}",
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
                           width=800,
                           hAxis="{title:'Virus',fontName:'Verdana', fontSize:15}",
                           vAxis="{title:'Number',fontName:'Verdana', fontSize:15}",
                           colors = "['#da602f', '#67001f','#fcdb6d']",
                           titleTextStyle="{color: '#3d4854', fontName:'Verdana', fontSize:15}",
                           legend = "{position: 'right'}")
        }

        print(plot_picks_3)
         gvisColumnChart(virus_bar(),
                         xvar='virus',
                         yvar=plot_picks_3,
                         options=my_list)

    })

}

