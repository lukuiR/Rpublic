ad<-yd$city
ad<-paste(ad,',',yd$kraj)
nominatim_osm(ad)


d<-suppressWarnings(lapply(ad, function(ad){
  #set the elapsed time counter to 0
  t <- Sys.time()
  #calling the nominatim OSM API
  api_output <- nominatim_osm(ad)
  #return data.frame with the input address, output of the nominatim_osm function and elapsed time
  data.frame(ad = ad, api_output)
})%>%
  bind_rows()%>%
  data.frame())

replicate(ad, nominatim_osm(ad),simplify = "array")

for(i in 1:length(ad)){
z=rbind(z, nominatim_osm(ad[i]))
print(i)
}
y=data.frame( yd,z)


greenLeafIcon <- makeIcon(
  iconUrl = y$V9,
  iconWidth = 90, iconHeight = 95,
  iconAnchorX = 90, iconAnchorY = 94
)

leaflet(data = y) %>% addTiles() %>%
  addMarkers(~lon, ~lat, icon = greenLeafIcon)
