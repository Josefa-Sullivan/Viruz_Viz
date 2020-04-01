
library(lubridate)
library(tidyverse)
library(leaflet.extras)
library(leaflet)
library(maps)
library(dplyr)
library(maptools)


#### Data Processing #####

# Load data into dataframes
data_github_confirmed = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
cases_timeseries = read.csv(data_github_confirmed, header=T, stringsAsFactors = F)

# gather time columns into one row
last_date = cases_timeseries %>% colnames() %>% tail(1)
first_date= colnames(cases_timeseries)[5]

cases_timeseries = cases_timeseries %>% gather(key='date', value='case_number', 
                                               first_date:last_date)

# Convert date column from character to Date.Time
fmt_df_dates = function(df){
  df$date = parse_date_time(gsub("X","", df$date), "%m.%d.%y")
  return(df)
}

df_timeseries = fmt_df_dates(cases_timeseries)

# Convert NA to 0
df_timeseries[is.na(df_timeseries)] = 0


# Sort df by most recent date and then by Country and Province
df_timeseries = df_timeseries %>% arrange(desc(date), Country.Region, Province.State)

# Load world data set with Lat and Long coordinates
world = ggplot2::map_data('world')

# Check to see if countries match those in the world dataset
df_timeseries$Country.Region[!df_timeseries$Country.Region %in% world$region] %>% unique()

### Convert Country Names to match ggplot names to plot worldmap

new_names = c('Antigua', 'Cape Verde', 'Republic of Congo', 'Democratic Republic of the Congo', 'Ivory Coast',
              'Czech Republic','Swaziland','Vatican','South Korea','Macedonia','Saint Vincent','Taiwan',
              'Trinidad','UK','USA','Palestine','Saint Kitts','Myanmar')


names_to_change = c('Antigua and Barbuda', 'Cabo Verde', "Congo [(]Brazzaville[)]", "Congo [(]Kinshasa[)]",
                    "Cote d'Ivoire", 'Czechia', 'Eswatini', 'Holy See', 'Korea, South', 'North Macedonia',
                    'Saint Vincent and the Grenadines', 'Taiwan[*]', 'Trinidad and Tobago', 'United Kingdom',
                    'US', 'West Bank and Gaza', 'Saint Kitts and Nevis','Burma')


i = 1

while (i < length(names_to_change)+1) {
  df_timeseries$Country.Region = gsub(names_to_change[i], new_names[i], df_timeseries$Country.Region)
  i = i+1
}


## Just take the most recent numbers (from 3/31/2020)
test2 = df_timeseries %>% filter(date==unique(df_timeseries$date)[1])

## Sum all numbers for each country
test3 = data.frame(test2 %>% group_by(test2$Country.Region) %>% summarise(cases = sum(case_number)))

## Join with world dataset
test4 = left_join(world, test3, by=c('region' = "test2.Country.Region"), )



##### Leaflet Map

# country = map('world', fill=T, plot=F)
# IDs <- sapply(strsplit(country$region, ":"), function(x) x[1])
# Country <- map2SpatialPolygons(country, 
#                                IDs=country$region, 
#                                proj4string=CRS("+proj=longlat +datum=WGS84"))

bins = c(1,100,1000,10000, 1e6)
countries = map('world', fill=T, plot=F, region=test2$region)
pal <- colorBin("YlOrRd", domain = test4$cases, bins = bins)

# labels <- sprintf(
#   "<strong>%s</strong><br/>%g cases",
#   test3$test2.Country.Region, test3$cases
# ) %>% lapply(htmltools::HTML)

mymap = leaflet() %>% 
        addTiles() %>% 
        addPolygons(data=countries, weight=0.5,
                    opacity = 1,
                    color = "black",
                    fillColor = ~pal(test4$cases),
                    label = ~paste0(test4$region, ': ', test4$cases),
                    fillOpacity = 0.7,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE)) %>% 
                    # highlight = highlightOptions(
                    #   weight = 5,
                    #   color = "#666",
                    #   dashArray = "",
                    #   fillOpacity = 0.7,
                    #   bringToFront = TRUE),
                    # label = labels,
                    # labelOptions = labelOptions(
                    #   style = list("font-weight" = "normal", padding = "3px 8px"),
                    #   textsize = "15px",
                    #   direction = "auto")) %>% 
      addLegend(pal = pal, values = test4$cases, 
                  opacity = 0.7, title = NULL,
                  position = "bottomright")
mymap

# colorNumeric("viridis", domain = test2$case_number)


