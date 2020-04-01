library(lubridate)
library(shiny)
library(tidyverse)
library(googleVis)
library(leaflet)
library(shinyWidgets)
library(leaflet.extras)
library(markdown)
library(shinydashboard)



################### Coronavirus data cleaning ###################################################

# Get latest data from JHU Github (https://github.com/CSSEGISandData/COVID-19)
data_github_deaths = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
data_github_confirmed = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
data_github_recovered = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

# Load data into dataframes
deaths_timeseries = read.csv(data_github_deaths, header=T, stringsAsFactors = F)
cases_timeseries = read.csv(data_github_confirmed, header=T, stringsAsFactors = F)
recovered_timeseries = read.csv(data_github_recovered, header=T, stringsAsFactors = F)

# Get dataframe that contains number of cases, deaths and recovered by date
# non-cumulative 
df= data.frame('cases'=colSums(cases_timeseries[-c(1,2,3,4)]),
               'deaths' = colSums(deaths_timeseries[-c(1,2,3,4)]),
               'recoveries'=colSums(recovered_timeseries[-c(1,2,3,4)]))



df = df %>% mutate(case_diff = cases - lag(cases, default = first(cases)),
                   death_diff = deaths - lag(deaths, default = first(deaths)),
                   recovery_diff = recoveries - lag(recoveries, default = first(recoveries)))

daily_numbers_df = df[,c(4,5,6)]
daily_numbers_df[1,1]=555
daily_numbers_df[1,2]=17
daily_numbers_df[1,3]=28

daily_numbers_df = daily_numbers_df %>% rename(cases=case_diff,
                            deaths = death_diff,
                            recoveries = recovery_diff)


# gather time columns into one row
last_date = deaths_timeseries %>% colnames() %>% tail(1)
first_date= colnames(deaths_timeseries)[5]
  
deaths_timeseries = deaths_timeseries %>% gather(key='date', value='death_number', 
                                                 first_date:last_date)
cases_timeseries = cases_timeseries %>% gather(key='date', value='case_number', 
                                              first_date:last_date)
recovered_timeseries = recovered_timeseries %>% gather(key='date', value='recovered_number', 
                                                       first_date:last_date)

# Convert date column from character to Date.Time
fmt_df_dates = function(df){
  df$date = parse_date_time(gsub("X","", df$date), "%m.%d.%y")
  return(df)
}

deaths_timeseries = fmt_df_dates(deaths_timeseries)
cases_timeseries = fmt_df_dates(cases_timeseries)
recovered_timeseries = fmt_df_dates(recovered_timeseries)


# Joining, by = c("Province.State", "Country.Region", "Lat", "Long", "date")
# Cumulative Case, Death and Recovered Numbers for each country and date
# Also Contains Latitude (Lat) and Longitude (Long)
timeseries_df = full_join(cases_timeseries, deaths_timeseries)
timeseries_df = full_join(timeseries_df, recovered_timeseries)


daily_numbers_df['date'] = timeseries_df$date %>% unique() %>% sort()

# Convert NA to 0
timeseries_df[is.na(timeseries_df)] = 0


# Sort df by most recent date and then by Country and Province
timeseries_df = timeseries_df %>% arrange(desc(date), Country.Region, Province.State)

# create a df of the latest case numbers
latest_numbers = timeseries_df %>% filter(date==unique(timeseries_df$date)[1])

current_totals = colSums(latest_numbers[c(6,7,8)])
# Create list of dates to use for input on map
slider_dates = timeseries_df$date %>% as.Date() %>% unique()



################### Ebola data cleaning ###################################################

ebola = read.csv("./Data/ebola_data.csv", stringsAsFactors = FALSE)

# convert columns to lowercase
colnames(ebola) = tolower(colnames(ebola))


# Combine Guinea and Guinea 2 and repeat for Liberia/Liberia 2
ebola[ebola$country=='Guinea 2',]$country="Guinea"

ebola[ebola$country=='Liberia 2',]$country="Liberia"


# Convert to date object
ebola$date=as.Date(ebola$date, "%Y-%m-%d")

# Create timeline of Ebola deaths and cases
df1 = ebola %>% filter(indicator=="Cumulative number of confirmed Ebola cases") %>% 
  group_by(date) %>% 
  summarise(cases = sum(value)) %>% 
  arrange(date) 


df2 = ebola %>% filter(indicator=="Cumulative number of confirmed Ebola deaths") %>% 
  group_by(date) %>% 
  summarise(deaths = sum(value)) %>% 
  arrange(date)

ebola_timeseries = full_join(df1,df2)

# Impute missing Liberia value on 10/17/14
ebola_timeseries[15,2] = 5252


#Liberia is dropped from dataset after 2/13/15. Add last confirmed number to subsequent times
ebola %>% filter(indicator=="Cumulative number of confirmed Ebola deaths") %>% 
  group_by(country) %>% summarise(n=n())

ebola %>% filter(country=="Liberia" &
                   indicator=="Cumulative number of confirmed Ebola deaths"&
                   date>as.Date("02/13/15", "%m/%d/%y"))

ebola_timeseries[ebola_timeseries$date>as.Date("02/13/15", "%m/%d/%y"),]$deaths= ebola_timeseries[ebola_timeseries$date>as.Date("02/13/15", "%m/%d/%y"),]$deaths+3858

# Fix Irregular Liberia dates
ebola_timeseries[ebola_timeseries$date==as.Date("10/17/14", "%m/%d/%y"),]$deaths= ebola_timeseries[ebola_timeseries$date==as.Date("10/17/14", "%m/%d/%y"),]$deaths+1157
ebola_timeseries[ebola_timeseries$date==as.Date("10/15/14", "%m/%d/%y"),]$deaths= ebola_timeseries[ebola_timeseries$date==as.Date("10/15/14", "%m/%d/%y"),]$deaths+1157


list_ = filter(ebola_timeseries, ebola_timeseries$date>as.Date("10/25/14", "%m/%d/%y")&
                 ebola_timeseries$date <as.Date("02/10/15", "%m/%d/%y"))$deaths
list_[1]=2999
i=2
while (i < length(list_)+1){
  temp = list_[i-1]
  list_[i]=temp +118
  i = i+1
}

ebola_timeseries[ebola_timeseries$date>as.Date("10/25/14", "%m/%d/%y")&
                   ebola_timeseries$date <as.Date("02/10/15", "%m/%d/%y") ,]$deaths = list_


# Limit timeframe visualized to July 2015
ebola_timeseries = ebola_timeseries %>% filter(date<as.Date("07/5/15", "%m/%d/%y"))

# Recoveries not included in dataset, add dummy value of 1 to prevent bug in timeline graph
ebola_timeseries$recoveries = 0


################### SARS data cleaning ###################################################
sars_df = read.csv("./Data/sars_2003_cumulative.csv", header = T, stringsAsFactors = FALSE)


sars_df$Total[3] = "5327"
sars_df$Total = as.numeric(sars_df$Total)

sars_df = sars_df %>% transform(Total=as.numeric(Total))
sars_total = sars_df %>% summarise(total = sum(Total),
                                   deaths = sum(Number.of.deaths.a),
                                   mortality_rate = deaths/total*100)



# Combine data from different viruses

stats_df = data.frame(virus = c('COVID-19','SARS', 'Influenza 1918', 'Influenza 2020','Ebola'),
           Cases = c(sum(latest_numbers$case_number),8098,500e6,31e6, 15138),
           Deaths = c(sum(latest_numbers$death_number),774, 50e6, 30000, 9478)) %>% 
          mutate(Mortality_Rate=Deaths/Cases*100) %>% 
          arrange(virus)

stats_df=stats_df[c(1,5,4,3,2),]




########################## World Map Function for Visualization ########################################
# # ISO-3 Country codes adapted from wikipedia ('https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3')
# iso_codes_wiki = read.csv('Data/country_iso_codes.csv', header=F, stringsAsFactors = F)
# 
# # Cruise Ships are not included in ISO codes and will not be used for country counts
# covid_countries[!covid_countries %in% iso_codes_wiki$V2]


# library(maps)
# library(ggplot2)
# world_data <- ggplot2::map_data('world')
# world_data <- fortify(world_data) %>% rmapshaper::ms_simplify(keep=0.1, keep_shapes=T)
# head(world_data)
# 
# ### Convert Country Names to match ggplot names to plot worldmap
# new_names = c('Antigua', 'Cape Verde', 'Republic of Congo', 'Democratic Republic of the Congo', 'Ivory Coast',
#               'Czech Republic','Swaziland','Vatican','South Korea','Macedonia','Saint Vincent','Taiwan',
#               'Trinidad','UK','USA','Palestine','Saint Kitts','Myanmar')
# 
# 
# names_to_change = c('Antigua and Barbuda', 'Cabo Verde', 'Congo (Brazzaville)', 'Congo (Kinshasa)',
#                     "Cote d'Ivoire", 'Czechia', 'Eswatini', 'Holy See', 'Korea, South', 'North Macedonia',
#                     'Saint Vincent and the Grenadines', 'Taiwan[*]', 'Trinidad and Tobago', 'United Kingdom',
#                     'US', 'West Bank and Gaza', 'Saint Kitts and Nevis','Burma')
# 
# i = 1
# 
# while (i < length(names_to_change)) {
#   timeseries_df$Country.Region = gsub(names_to_change[i], new_names[i],
#                                       timeseries_df$Country.Region)
#   i = i+1
# }
# 
# 
# 
# 
# 
# worldMaps <- function(df, world_data, data_type, period, indicator){
#   
#   # Function for setting the aesthetics of the plot
#   my_theme <- function () { 
#     theme_bw() + theme(axis.title = element_blank(),
#                        axis.text = element_blank(),
#                        axis.ticks = element_blank(),
#                        panel.grid.major = element_blank(), 
#                        panel.grid.minor = element_blank(),
#                        panel.background = element_blank(), 
#                        legend.position = "bottom",
#                        panel.border = element_blank(), 
#                        strip.background = element_rect(fill = 'white', colour = 'white'))
#   }
#   
#   # Select only the data that the user has selected to view
#   plotdf <- df[df$date == indicator & df$DataType == data_type & df$Period == period,]
#   plotdf <- plotdf[!is.na(plotdf$ISO3), ]
#   
#   # Add the data the user wants to see to the geographical world data
#   world_data['DataType'] <- rep(data_type, nrow(world_data))
#   world_data['Period'] <- rep(period, nrow(world_data))
#   world_data['Indicator'] <- rep(indicator, nrow(world_data))
#   world_data['Value'] <- plotdf$Value[match(world_data$ISO3, plotdf$ISO3)]
#   
#   # Create caption with the data source to show underneath the map
#   capt <- paste0("Source: ", ifelse(data_type == "Childlessness", "United Nations" , "World Bank"))
#   
#   # Specify the plot for the world map
#   library(RColorBrewer)
#   library(ggiraph)
#   g <- ggplot() + 
#     geom_polygon_interactive(data = subset(world_data, lat >= -60 & lat <= 90), color = 'gray70', size = 0.1,
#                              aes(x = long, y = lat, fill = Value, group = group, 
#                                  tooltip = sprintf("%s<br/>%s", ISO3, Value))) + 
#     scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
#     labs(fill = data_type, color = data_type, title = NULL, x = NULL, y = NULL, caption = capt) + 
#     my_theme()
#   
#   return(g)
# }
