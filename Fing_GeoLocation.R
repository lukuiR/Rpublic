#Getting geolocation
library("jsonlite")
nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

addresses <- c("Baker Street 221b, London", "Brandenburger Tor, Berlin", 
               "Platz der Deutschen Einheit 1, Hamburg", "Arc de Triomphe de l’Etoile, Paris",
               "Дворцовая пл., Санкт-Петербург, Россия")


nominatim_osm(addresses[2])

nominatim_osm("Warsaw, Poland")
