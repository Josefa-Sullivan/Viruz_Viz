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
data_github_deaths = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
data_github_confirmed = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
data_github_recovered = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

# Load data into dataframes
deaths_timeseries = read.csv(data_github_deaths, header=T, stringsAsFactors = F)
cases_timeseries = read.csv(data_github_confirmed, header=T, stringsAsFactors = F)
recovered_timeseries = read.csv(data_github_recovered, header=T, stringsAsFactors = F)

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
timeseries_df = full_join(cases_timeseries, deaths_timeseries)
timeseries_df = full_join(timeseries_df, recovered_timeseries)


# Convert NA to 0
timeseries_df[is.na(timeseries_df)] = 0


# Sort df by most recent date and then by Country and Province
timeseries_df = timeseries_df %>% arrange(desc(date), Country.Region, Province.State)

# create a df of the latest case numbers
latest_numbers = timeseries_df %>% filter(date==unique(timeseries_df$date)[1])


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

stats_df = data.frame(virus = c('Ebola','COVID-19','SARS', 'Influenza 1918', 'Influenza 2020'),
           Cases = c(15138,sum(latest_numbers$case_number),8098,500e6,31e6),
           Deaths = c(9478,sum(latest_numbers$death_number),774, 50e6, 30000)) %>% 
          mutate(Mortality_Rate=Deaths/Cases*100) %>% 
          arrange(virus)

stats_df
