library(ggmap)

pce <- read.csv("data/power-cost-equalization-pce-data.csv")
 
latlon <- geocode(paste0(levels(pce$community_names), ", ", "Alaska") )
latlon <- cbind(community_names = levels(pce$community_names), latlon)

write.csv(latlon, file = "latlon.csv")

ggplot(latlon, aes(x = lon, y = lat, label = community_names)) + geom_text()