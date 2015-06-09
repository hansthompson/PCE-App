#install_github('quandl/R-package')
library(Quandl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggmap)
#read in data
pce <- read.csv("data/power-cost-equalization-pce-data.csv")
latlon <- read.csv("latlon.csv")

pce<- select(pce, community_names, plant__name, plant__utility__name, plant__akeps_region__name,
       year, month, intertied, residential_rate, pce_rate, pro_rata_rate,
       effective_rate, fuel_price, fuel_used_gal, fuel_cost, nonfuel_expenses,
       diesel_efficiency, diesel_kwh_generated, peak_consumption_kw, 
       residential_kwh_sold, commercial_kwh_sold, community_kwh_sold, 
       government_kwh_sold, unbilled_kwh, residential_customers, commercial_customers, 
       community_customers, government_customers, unbilled_customers, 
       other_customers)

pce <- inner_join(latlon, pce, by = "community_names")

pce$Date <- as.Date(ymd(paste(pce$year, pce$month, "01")))
#Remove empty factor level for region
pce <- filter(pce, plant__akeps_region__name != "")

summarized_pce <- pce %>% 
    group_by(Date) %>%
    summarize(sum = mean(fuel_price, na.rm = T))

ggplot(summarized_pce, aes(x = Date, y = sum)) + geom_line()

#Diesel Fuel Refiner Wholesale Price, Monthly
#https://www.quandl.com/data/EIA/STEO_DSWHUUS_M-Diesel-Fuel-Refiner-Wholesale-Price-Monthly 
#Only 50 requests a day. 
wholesale_fuel <- Quandl("EIA/STEO_DSWHUUS_M")
#add a day. This offsets the time of the price by a day. 
wholesale_fuel$Date <- as.Date(wholesale_fuel$Date + days(1) )
#convert to dolllars from cents
wholesale_fuel$Value <- wholesale_fuel$Value / 100

combined_price <- inner_join(wholesale_fuel, pce, by = "Date")



#Patterns of price inflation by geographic region
ggplot(data = combined_price, aes(x = fuel_price, y = Value, color = plant__akeps_region__name, alpha = 0.1)) + geom_point() + coord_fixed() + geom_smooth(method = "lm")
#distribution of cost difference from wholesale and fuel_price to village
ggplot(data = combined_price, aes(x = fuel_price - Value, fill = plant__akeps_region__name)) + geom_histogram()
#change in difference over time
ggplot(data = combined_price, aes(x = factor(Date), y = fuel_price - Value)) + geom_boxplot() + facet_wrap(~plant__akeps_region__name) + ggtitle("From 07/2001-06/2013")
#show 
black_bg<- theme(panel.background = element_rect(fill = "black"))
most_recent <- filter(combined_price, Date == "2013-06-01")
ggplot(data = most_recent, aes(x = lon, y = lat, label = community_names, color = fuel_price)) + geom_text() + scale_colour_gradientn(colours=rainbow(4)) + black_bg
ggplot(data = most_recent, aes(x = lon, y = lat, label = community_names, color = fuel_cost) ) + geom_text() + scale_colour_gradientn(colours=rainbow(4)) + black_bg
ggplot(data = most_recent, aes(x = lon, y = lat, label = community_names, color = fuel_price - Value)) + geom_text() + scale_colour_gradientn(colours=rainbow(4)) + black_bg
mean(most_recent$fuel_used_gal, na.rm = T)


hist(filter(combined_price, peak_consumption_kw < 1000))
ave_max <- combined_price %>%
      mutate(month = month(Date)) %>%
      filter(peak_consumption_kw < 1000) %>%
      group_by(community_names, month) %>%
      summarise(mean = mean(peak_consumption_kw))

ggplot(ave_max, aes(x = month, y = mean, color = community_names)) + geom_line() + facet_wrap(~ plant__akeps_region__name)


