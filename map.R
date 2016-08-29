########################Header################################################
# title         : Z-1map
# purpose       : plot the coordinates of survey fields;
# producer      : prepared by S. Jaisong (s.jaisong@irri.org);
# last update   : in Los Baños, Laguna, PHL, Jan 2015;
# inputs        : 3-consistant.output.RData;
# outputs       : png;
# remarks 1     : ;
# remarks 2     : ;
#####################################################
library(maps)
library(maptools)
library(ggplot2)
library(ggmap)
library(ggsn)

#load(file = "survey.RData")
survey_site <- survey %>% select(id, country, year, province, latitude, longitude)
names(survey_site) <- c("prod_env",  "season",    "id"  ,      "country"  , "year"   ,   "province" , "lat" , "long")
#---------------------

# loading the required packages

# getting the map
#ALL locations

map <- get_map(location = c(lon = 90, lat = 15), zoom = 4,  maptype =  "terrain", scale = 2)


# plotting the map with some points on it
ggmap(map)  + geom_point(data = survey_site, aes(x = long, y = lat, color = prod_env, alpha = 0.8), size = 2) +  
  guides(alpha =FALSE, size =FALSE) +
  ylim(c(-10, 25)) + xlim(c(75, 110)) +
  scale_color_brewer(name = 'Production \n environment', palette="Dark2")

ggsave(filename = "SKEP2_survey_map.pdf",
       scale =2,
       path = "figs")


# Tami Nadu
IN_TN <- survey_site %>% filter(province == "IN-TN") %>% select(year, latitude, longitude)

map <- get_map(location = c(lon = mean(IN_TN$longitude), 
                            lat = mean(IN_TN$latitude)), 
               zoom = 10,
               maptype = "terrain", 
               scale = 2)

ggmap(map) +
  geom_point(data = IN_TN, 
             aes(x = longitude, y = latitude, fill = year, alpha = 0.8), 
             size = 4, shape = 21
  ) +  theme(legend.text = element_text(size = 12)) +
  guides(alpha=FALSE, 
         size=FALSE)

ggsave(filename = "IN_TN_survey_map.png",
       scale =2,
       path = "figs")

# ==

IN_OR <- survey_site %>% filter(province == "IN-OR") %>% select(year, latitude, longitude)

map <- get_map(location = c(lon = mean(IN_OR$longitude), 
                            lat = mean(IN_OR$latitude)), 
               zoom = 10,
               maptype = "terrain", 
               scale = 2)

ggmap(map) +
  geom_point(data = IN_OR, 
             aes(x = longitude, y = latitude, fill = year, alpha = 0.8), 
             size = 4, shape = 21
  ) +  theme(legend.text = element_text(size = 12)) +
  guides(alpha =FALSE, 
         size =FALSE) 

ggsave(filename = "IN_OR_survey_map.png",
       scale =2,
       path = "figs")

# = Indonesia

ID <- survey_site %>% filter(province == "ID-JB") %>% select(year, latitude, longitude)

map <- get_map(location = c(lon = 107.25, 
                            lat = -6.5), 
               zoom = 9,
               maptype = "terrain", 
               scale = 2)

ggmap(map)
ggmap(map) +
  geom_point(data = ID, 
             aes(x = longitude, y = latitude, fill = year, alpha = 0.8), 
             size = 4, shape = 21
  ) +  theme(legend.text = element_text(size = 12)) +
  guides(alpha =FALSE, 
         size =FALSE) 

ggsave(filename = "ID_survey_map.png",
       scale =2,
       path = "figs")

# = Thailand

TH <- survey_site %>% filter(province == "TH-72") %>% select(id,year, latitude, longitude)

map <- get_map(location = c(lon = mean(TH$longitude), 
                            lat = mean(TH$latitude)), 
               zoom = 9,
               maptype = "terrain", 
               scale = 2)


ggmap(map) +
  geom_point(data = TH, 
             aes(x = longitude, y = latitude, fill = year, alpha = 0.8), 
             size = 4, shape = 21
  ) +  theme(legend.text = element_text(size = 12)) +
  guides(alpha =FALSE, 
         size =FALSE) 

ggsave(filename = "TH_survey_map.png",
       scale =2,
       path = "figs")

#== VN

VN <- survey_site %>% filter(province == "VN-61") %>% select(id,year, latitude, longitude)

map <- get_map(location = c(lon = 106.25, 
                            lat = 20.75), 
               zoom = 10,
               maptype = "terrain", 
               scale = 2)

ggmap(map) +
  geom_point(data = VN, 
             aes(x = longitude, y = latitude, fill = year, alpha = 0.8), 
             size = 4, shape = 21
  ) +  theme(legend.text = element_text(size = 12)) +
  guides(alpha =FALSE, 
         size =FALSE) 


ggsave(filename = "VN_survey_map.png",
       scale =2,
       path = "figs")


#
# Result #
#--------#
# Return a list whose elements are :
#   - rectangle : a data.frame containing the coordinates to draw the first rectangle ;
#   - rectangle2 : a data.frame containing the coordinates to draw the second rectangle ;
#   - legend : a data.frame containing the coordinates of the legend texts, and the texts as well.
#
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distanceLon : length of each rectangle ;
# distanceLat : width of each rectangle ;
# distanceLegend : distance between rectangles and legend texts ;
# dist.units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles). 

scaleBar <- function(lon, lat, distanceLon,distanceLat,distanceLegend, dist.units =
                       "km"){
  # First rectangle
  bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon, dist.units = dist.units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLat, dist.units = dist.units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"], bottomRight[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon*2, dist.units = dist.units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"], bottomRight2[1,"long"], bottomRight2[1,"long"],
                              bottomRight[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLegend, dist.units = dist.units, model = "WGS84")
  onTop2 <- onTop3 <- onTop
  onTop2[1,"long"] <- bottomRight[1,"long"]
  onTop3[1,"long"] <- bottomRight2[1,"long"]
  
  legend <- rbind(onTop, onTop2, onTop3)
  legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
} 

#We also need a function to obtain the coordinates of the North arrow:

#
# Result #
#--------#
