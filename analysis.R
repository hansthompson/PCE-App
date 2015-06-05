library(lubridate)
library(dplyr)
library(ggplot2)
#read in data
pce <- read.csv("data/power-cost-equalization-pce-data.csv")

dim(pce)
summary(pce)

pce<- select(pce, community_names, plant__name, plant__utility__name,
       year, month, intertied, residential_rate, pce_rate, pro_rata_rate,
       effective_rate, fuel_price, fuel_used_gal, fuel_cost, nonfuel_expenses,
       diesel_efficiency, diesel_kwh_generated, peak_consumption_kw, 
       residential_kwh_sold, commercial_kwh_sold, community_kwh_sold, 
       government_kwh_sold, unbilled_kwh, residential_customers, commercial_customers, 
       community_customers, government_customers, unbilled_customers, 
       other_customers)

pce$time <- ymd(paste(pce$year, pce$month, "01"))

ggplot(data = filter(pce, community_names == "Sleetmute"), aes(x = time, y = peak_consumption_kw)) + geom_line()

summarized_pce <- pce %>% 
    group_by(time) %>%
    summarize(sum = mean(fuel_price, na.rm = T))

ggplot(summarized_pce, aes(x = time, y = sum)) + geom_line()



