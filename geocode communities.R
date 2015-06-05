library(ggmap)

pce <- read.csv("data/power-cost-equalization-pce-data.csv")
 
latlon <- geocode(paste0(levels(pce$community_names), ", ", "Alaska") )
latlon <- cbind(community_names = levels(pce$community_names), latlon)
latlon[latlon$community_names == "Eagle, Eagle Village",2:3] <- c(-141.114722, 64.781389)
write.csv(latlon, file = "latlon.csv", row.names = FALSE)

ggplot(latlon, aes(x = lon, y = lat, label = community_names)) + geom_text()
#Does anything look wierd? It should not. 