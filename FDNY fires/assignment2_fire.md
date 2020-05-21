---
title: "Assignment 2: Mapping Fire Incidents and FDNY Response Times"
author: Timothy Lee
date: 2020-05-21
always_allow_html: yes
output: 
  html_document:
    keep_md: true
---

Fires in NYC and FDNY Response
================================




```r
library(readr)
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.6.3
```

```r
library(tidyr)
```

```
## Warning: package 'tidyr' was built under R version 3.6.3
```

```r
library(stringr)
library(lubridate)

library(ggplot2)

library(sp)
library(rgdal)
library(tmaptools)
```

```
## Warning: package 'tmaptools' was built under R version 3.6.3
```

```r
library(leaflet)
```

```
## Warning: package 'leaflet' was built under R version 3.6.3
```

```r
library(ggmap)
```

```
## Warning: package 'ggmap' was built under R version 3.6.3
```

## Overview

For this assignment, we are going to investigate fires requiring the fire department to respond. Using data about the locations of firehouses and fires occurring in New York City, we want to know whether response times to fires differ across the city. Second, we will try to focus on one possible variable that could affect response times -- the distance from the firehouse -- and see whether we find the (expected) effect.

To keep this homework manageable, I am leaving out another part of the investigation: What is the effect of demographic and/or income characteristics of the neighborhood on response times. This is likely a bit more sensitive but also relevant from a public policy perspective.  

## Data

We rely on two data sets.

#### Incidents responded to by fire companies

NYC Open Data has data on all [incidents responded to by fire companies](https://data.cityofnewyork.us/Public-Safety/Incidents-Responded-to-by-Fire-Companies/tm6d-hbzd). I have included the variable description file in the exercise folder. The following variables are available:

  - IM_INCIDENT_KEY:	Unique identifier for each incident which serves
  - INCIDENT_TYPE_DESC	The code and description of the incident category type
  - INCIDENT_DATE_TIME	The date and time that the incident was logged into the Computer Aided Dispatch system
  - ARRIVAL_DATE_TIME	The date and time that the first unit arrived on scene
  - UNITS_ONSCENE	Total number of units that arrived on scene
  - LAST_UNIT_CLEARED_DATETIME	The date and time that the incident was completed and the last unit cleared the scene
  - HIGHEST_LEVEL_DESC	The highest alarm level that the incident received
  - TOTAL_INCIDENT_DURATION	The total number of seconds from when then incident was created to when the incident was closed
  - ACTION_TAKEN1_DESC	The code and description of the first action taken
  - ACTION_TAKEN2_DESC	The code and description of the second action taken
  - ACTION_TAKEN3_DESC	The code and description of the third action taken
  - PROPERTY_USE_DESC	The code and description of the type of street or building where the incident took place
  - STREET_HIGHWAY	The name of the street where the incident_took place
  - ZIP_CODE	The postal zip code where the incident took place
  - BOROUGH_DESC	The borough where the incident took place
  - FLOOR	The floor of the building where the incident took place
  - CO_DETECTOR_PRESENT_DESC	Indicator for when a CO detector was present
  - FIRE_ORIGIN_BELOW_GRADE_FLAG	Indicator for when the fire originated below grade
  - STORY_FIRE_ORIGIN_COUNT	Story in which the fire originated
  - FIRE_SPREAD_DESC	How far the fire spread from the object of origin
  - DETECTOR_PRESENCE_DESC	Indicator for when a  detector was present
  - AES_PRESENCE_DESC	Indicator for when an Automatic Extinguishing System is present
  - STANDPIPE_SYS_PRESENT_FLAG	Indicator for when a standpipe was present in the area of origin of a fire

This dataset is only updated annually, and thus far only data from 2013 to 2018 is contained. The full dataset is also somewhat too large for an exercise (2.5M rows), so I suggest to limit yourself to a subset. I have added a file containing the subset of of only building fires (`INCIDENT_TYPE_DESC == "111 - Building fire"`) for 2013 to 2018 only which yields about 14,000 incidents.


```r
library(tidyverse)
fire_all <- read_csv("no_upload/Incidents_Responded_to_by_Fire_Companies.csv")
fire <- fire_all %>%
  filter(INCIDENT_TYPE_DESC == "111 - Building fire")
```

Unfortunately, the addresses of the incidents were not geocoded yet. Ideally, I would like you to know how to do this but am mindful about the hour or so required to get this done. So, here is the code. The geocodes (as far as they were returned successfully) are part of the data (as variables `lat` and `lon`).


```r
library(ggmap)

# Register Google API Key
register_google(key = Sys.getenv("GOOGLE_MAPS_API_KEY"))

# Create and geocode addresses
fire <- fire %>%
  mutate(address = str_c( str_to_title(fire$STREET_HIGHWAY),
                  "New York, NY",
                  fire$ZIP_CODE,
                  sep=", ")) %>%
  filter(is.na(address)==FALSE) %>%
  mutate_geocode(address)

# Save File
write_csv(fire, "building_fires.csv")
```

#### FDNY Firehouse Listing

NYC Open Data also provides data on the [location of all 218 firehouses in NYC](https://data.cityofnewyork.us/Public-Safety/FDNY-Firehouse-Listing/hc8x-tcnd). Relevant for our analysis are the following variables: `FacilityName`, `Borough`, `Latitude`, `Longitude`


```r
library(tidyverse)
firehouses <- read_csv("FDNY_Firehouse_Listing.csv") %>%
  dplyr::filter(!is.na(Latitude))
```

_Note:_ 5 entries contain missing information, including on the spatial coordinates. We can exclude these for the exercise. 

## Tasks

#### 1. Location of Severe Fires

Provide a `leaflet` map of the highest severity fires (i.e. subset to the highest category in `HIGHEST_LEVEL_DESC`)  contained in the file `buiding_fires.csv`. Ignore locations that fall outside the five boroughs of New York City. Provide at least three pieces of information on the incident in a popup.


```r
# Read in data
building_fires <- read_csv("building_fires.csv") 

# What can I filter?
table(building_fires$HIGHEST_LEVEL_DESC)
```

```
## 
##                                 0 - Initial alarm 
##                                                51 
## 1 - More than initial alarm, less than Signal 7-5 
##                                              3701 
##                                  11 - First Alarm 
##                                               281 
##                                     2 - 2nd alarm 
##                                               636 
##                                 22 - Second Alarm 
##                                                56 
##                                     3 - 3rd alarm 
##                                               138 
##                                  33 - Third Alarm 
##                                                15 
##                                     4 - 4th alarm 
##                                                47 
##                                 44 - Fourth Alarm 
##                                                 6 
##                                     5 - 5th alarm 
##                                                43 
##                                  55 - Fifth Alarm 
##                                                 4 
##                                    7 - Signal 7-5 
##                                              8451 
##                            75 - All Hands Working 
##                                               653
```

```r
table(building_fires$BOROUGH_DESC)
```

```
## 
##     1 - Manhattan         2 - Bronx 3 - Staten Island      4 - Brooklyn 
##              2774              2933               823              4256 
##        5 - Queens 
##              3414
```



```r
building_fires <- building_fires %>%
  filter(HIGHEST_LEVEL_DESC == "55 - Fifth Alarm" | HIGHEST_LEVEL_DESC == "5 - 5th alarm")
```



```r
popup_fires <- paste("Address:", building_fires$address, "<br>",
                     "Fire Severity:", building_fires$FIRE_SPREAD_DESC, "<br>",
                     "Incident Date/Time:", building_fires$INCIDENT_DATE_TIME, "<br>")

building_fires_leaflet <- leaflet(building_fires) %>%
  addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
           attribution = '<a href = "maps.google.com">Google Maps</a>') %>%
  setView(lat = 40.7484405, lng = -73.9878531, zoom = 13) %>%
  addMarkers(~lon, ~lat, popup = popup_fires, clusterOptions = markerClusterOptions())

building_fires_leaflet
```

<!--html_preserve--><div id="htmlwidget-b117a02db0a1676dd59c" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-b117a02db0a1676dd59c">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"<a href = \"maps.google.com\">Google Maps<\/a>"}]},{"method":"addMarkers","args":[[40.7233127,40.892043,40.8348028,40.8206784,40.7110625,40.8216804,40.7226986,40.7991517,40.7499236,40.6158732,40.8151609,40.8515333,40.6768721,40.6827699,40.7312885,40.7164946,40.6316739,40.7413184,40.8497801,40.8525315,40.698161,40.7511988,40.6552926,40.680811,40.7810491,40.639716,40.783266,40.7455865,40.723143,40.8059074,40.6870165,40.7286886,40.7418559,40.5614573,40.832241,40.7012329,40.7291949,40.7142035,40.8521389,40.8275142,40.8236333,40.8426909,40.8348028,40.834443,40.8266969,40.8641499,40.5919782],[-73.9993783,-73.858116,-73.8690163,-73.9486635,-73.8753396,-73.9391747,-73.9405144,-73.9686566,-73.8826722,-74.0724274,-73.8864169,-73.9144357,-73.8403393,-73.9765248,-73.9857092,-73.7385782,-73.9956158,-74.0002966,-73.9192496,-73.9000278,-73.925273,-73.9463421,-73.960771,-73.884893,-73.8459904,-74.0835728,-73.9507404,-73.9875248,-73.8204787,-73.9212625,-73.822181,-73.8227026,-73.86974,-74.1122509,-73.8542408,-73.8892971,-73.9872407,-74.0097319,-73.9076711,-73.8675633,-73.9459027,-73.890488,-73.8690163,-73.9490966,-73.943001,-73.8913106,-74.1624124],null,null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Address: Spring St, New York, NY, 10012 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/10/2013 06:42:55 PM <br>","Address: White Plains Rd, New York, NY, 10466 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 05/02/2013 06:16:33 AM <br>","Address: Commonwealth Ave, New York, NY, 10460 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 07/17/2013 02:48:46 AM <br>","Address: W 139 St, New York, NY, 10031 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 07/22/2013 03:45:20 PM <br>","Address: 68 Ave, New York, NY, 11379 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 09/09/2013 08:29:54 PM <br>","Address: W 145 St, New York, NY, 10039 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 12/13/2013 07:46:38 PM <br>","Address: Meeker Ave, New York, NY, 11222 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 01/04/2014 12:13:31 AM <br>","Address: Broadway, New York, NY, 10025 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/11/2014 05:20:26 AM <br>","Address: 37 Ave, New York, NY, 11372 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 04/21/2014 05:45:58 PM <br>","Address: Chestnut Ave, New York, NY, 10305 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/05/2014 01:07:50 AM <br>","Address: Hunts Point Ave, New York, NY, 10474 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 07/25/2014 12:32:08 AM <br>","Address: University Ave, New York, NY, 10453 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/17/2014 05:29:09 AM <br>","Address: 97 St, New York, NY, 11417 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/18/2014 04:19:49 PM <br>","Address: Flatbush Ave, New York, NY, 11217 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 01/21/2015 03:28:28 AM <br>","Address: 2nd Ave, New York, NY, 10009 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 03/26/2015 03:17:40 PM <br>","Address: 98 Ave, New York, NY, 11429 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 05/31/2015 06:20:32 PM <br>","Address: 13 Ave, New York, NY, 11219 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 10/03/2015 01:05:17 PM <br>","Address: W 17 St, New York, NY, 10011 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 10/20/2015 02:55:55 AM <br>","Address: Montgomery Ave, New York, NY, 10453 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 11/30/2015 11:48:30 AM <br>","Address: Valentine Ave, New York, NY, 10457 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 12/10/2015 11:42:06 PM <br>","Address: Dekalb Ave, New York, NY, 11237 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 03/29/2016 09:51:25 PM <br>","Address: 13 St, New York, NY, 11101 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 04/03/2016 03:42:30 PM <br>","Address: Parkside Ave, New York, NY, 11226 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 04/03/2016 10:47:44 PM <br>","Address: Arlington Ave, New York, NY, 11208 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 04/24/2016 06:23:09 PM <br>","Address: College Point Blvd, New York, NY, 11356 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 07/21/2016 09:51:03 PM <br>","Address: Benziger Ave, New York, NY, 10301 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 08/18/2016 06:09:05 PM <br>","Address: E 93 St, New York, NY, 10128 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 10/27/2016 03:24:27 AM <br>","Address: W 29 St, New York, NY, 10016 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 10/30/2016 01:04:09 AM <br>","Address: Vleigh Pl, New York, NY, 11367 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/30/2016 06:24:06 PM <br>","Address: E 135 St, New York, NY, 10454 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/11/2017 12:56:16 AM <br>","Address: Liberty Ave, New York, NY, 11419 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 03/04/2017 10:52:09 PM <br>","Address: Main St, New York, NY, 11367 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 03/12/2017 03:41:16 PM <br>","Address: 94 St, New York, NY, 11373 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 04/11/2017 06:25:45 PM <br>","Address: Mill Rd, New York, NY, 10306 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 05/21/2017 04:10:00 AM <br>","Address: Olmstead Ave, New York, NY, 10462 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/22/2017 03:40:35 PM <br>","Address: Myrtle Ave, New York, NY, 11385 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/24/2017 09:10:13 PM <br>","Address: E 9 St, New York, NY, 10003 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 06/28/2017 05:45:34 PM <br>","Address: Murray St, New York, NY, 10007 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 09/01/2017 06:47:28 PM <br>","Address: Walton Ave, New York, NY, 10453 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 09/27/2017 06:28:13 AM <br>","Address: Watson Ave, New York, NY, 10472 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 10/01/2017 02:39:55 PM <br>","Address: W 144 St, New York, NY, 10031 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 11/17/2017 03:14:09 PM <br>","Address: Prospect Ave, New York, NY, 10460 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/28/2017 06:51:30 PM <br>","Address: Commonwealth Ave, New York, NY, 10460 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/02/2018 05:29:06 AM <br>","Address: Riverside Dr, New York, NY, 10032 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/08/2018 01:47:17 PM <br>","Address: St Nicholas Ave, New York, NY, 10031 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 03/22/2018 10:50:30 PM <br>","Address: E 194 St, New York, NY, 10458 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 04/24/2018 05:27:47 AM <br>","Address: Steinway Ave, New York, NY, 10314 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/16/2018 07:04:53 PM <br>"],null,{"showCoverageOnHover":true,"zoomToBoundsOnClick":true,"spiderfyOnMaxZoom":true,"removeOutsideVisibleBounds":true,"spiderLegPolylineOptions":{"weight":1.5,"color":"#222","opacity":0.5},"freezeAtZoom":false},null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"setView":[[40.7484405,-73.9878531],13,[]],"limits":{"lat":[40.5614573,40.892043],"lng":[-74.1624124,-73.7385782]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#### 2. Layers and Clusters

##### a) Color by Type of Property

Start with the previous map. Now, distinguish the markers of the fire locations by `PROPERTY_USE_DESC`, i.e. what kind of property was affected. If there are too many categories, collapse some categories. Choose an appropriate coloring scheme to map the locations by type of affected property. Add a legend informing the user about the color scheme. Also make sure that the information about the type of affected property is now contained in the popup information. Show this map.

##### b) Cluster

Add marker clustering, so that zooming in will reveal the individual locations but the zoomed out map only shows the clusters. Show the map with clusters.


```r
# How many property uses are there?
table(building_fires$PROPERTY_USE_DESC)
```

```
## 
##                       000 - Property Use, other 
##                                               1 
## 131 - Church, mosque, synagogue, temple, chapel 
##                                               1 
##                   161 - Restaurant or cafeteria 
##                                               1 
##                    419 - 1 or 2 family dwelling 
##                                               4 
##                      429 - Multifamily dwelling 
##                                              23 
##               500 - Mercantile, business, other 
##                                              12 
##    519 - Food and beverage sales, grocery store 
##                                               2 
##                           599 - Business office 
##                                               2 
##                                 891 - Warehouse 
##                                               1
```


```r
building_fires$property_types <- building_fires$PROPERTY_USE_DESC
building_fires$property_types[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^0")] <- "000 - Other"
building_fires$property_types[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^1")] <- "100 - Transport, Religion, Recreation"
building_fires$property_types[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^2")] <- "200 - Education"
building_fires$property_types[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^3")] <- "300 - Medical"
building_fires$property_types[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^4")] <- "400 - Residential"
building_fires$property_types[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^5")] <- "500 - Commercial Buildings"
building_fires$property_types[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^8")] <- "800 - Storage and Indoor Parking"
building_fires$property_types[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^9")] <- "900 - Outdoor Location"

building_fires$property_mkcol <- building_fires$PROPERTY_USE_DESC
building_fires$property_mkcol[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^0")] <- "black"
building_fires$property_mkcol[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^1")] <- "pink"
building_fires$property_mkcol[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^2")] <- "orange"
building_fires$property_mkcol[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^3")] <- "red"
building_fires$property_mkcol[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^4")] <- "purple"
building_fires$property_mkcol[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^5")] <- "darkblue"
building_fires$property_mkcol[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^7")] <- "lightgray"
building_fires$property_mkcol[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^8")] <- "gray"
building_fires$property_mkcol[str_detect(building_fires$PROPERTY_USE_DESC, pattern = "^9")] <- "green"

table(building_fires$property_types)
```

```
## 
##                           000 - Other 
##                                     1 
## 100 - Transport, Religion, Recreation 
##                                     2 
##                     400 - Residential 
##                                    27 
##            500 - Commercial Buildings 
##                                    16 
##      800 - Storage and Indoor Parking 
##                                     1
```

```r
table(building_fires$property_mkcol)
```

```
## 
##    black darkblue     gray     pink   purple 
##        1       16        1        2       27
```


```r
icons <- awesomeIcons(
  icon = 'map-marker',
  iconColor = 'black',
  library = 'fa',
  markerColor = building_fires$property_mkcol
)


building_fires_leaflet <- leaflet(building_fires) %>%
  addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
           attribution = '<a href = "maps.google.com">Google Maps</a>') %>%
  setView(lat = 40.7484405, lng = -73.9878531, zoom = 13) %>%
  addAwesomeMarkers(~lon, ~lat, icon = icons, popup = popup_fires, clusterOptions = markerClusterOptions()) %>%
  addLegend(colors = c("black", "pink", "purple", "darkblue", "gray"),
            values = NULL,
            labels = sort(unique(building_fires$property_types)),
            title = "Building Type")

building_fires_leaflet
```

<!--html_preserve--><div id="htmlwidget-014d7125884b802982bc" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-014d7125884b802982bc">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"<a href = \"maps.google.com\">Google Maps<\/a>"}]},{"method":"addAwesomeMarkers","args":[[40.7233127,40.892043,40.8348028,40.8206784,40.7110625,40.8216804,40.7226986,40.7991517,40.7499236,40.6158732,40.8151609,40.8515333,40.6768721,40.6827699,40.7312885,40.7164946,40.6316739,40.7413184,40.8497801,40.8525315,40.698161,40.7511988,40.6552926,40.680811,40.7810491,40.639716,40.783266,40.7455865,40.723143,40.8059074,40.6870165,40.7286886,40.7418559,40.5614573,40.832241,40.7012329,40.7291949,40.7142035,40.8521389,40.8275142,40.8236333,40.8426909,40.8348028,40.834443,40.8266969,40.8641499,40.5919782],[-73.9993783,-73.858116,-73.8690163,-73.9486635,-73.8753396,-73.9391747,-73.9405144,-73.9686566,-73.8826722,-74.0724274,-73.8864169,-73.9144357,-73.8403393,-73.9765248,-73.9857092,-73.7385782,-73.9956158,-74.0002966,-73.9192496,-73.9000278,-73.925273,-73.9463421,-73.960771,-73.884893,-73.8459904,-74.0835728,-73.9507404,-73.9875248,-73.8204787,-73.9212625,-73.822181,-73.8227026,-73.86974,-74.1122509,-73.8542408,-73.8892971,-73.9872407,-74.0097319,-73.9076711,-73.8675633,-73.9459027,-73.890488,-73.8690163,-73.9490966,-73.943001,-73.8913106,-74.1624124],{"icon":"map-marker","markerColor":["purple","darkblue","purple","purple","purple","pink","darkblue","darkblue","darkblue","purple","darkblue","darkblue","purple","purple","purple","gray","purple","darkblue","purple","purple","purple","darkblue","darkblue","purple","darkblue","purple","purple","darkblue","darkblue","black","darkblue","pink","purple","purple","purple","darkblue","purple","darkblue","purple","purple","purple","purple","purple","purple","purple","darkblue","purple"],"iconColor":"black","spin":false,"squareMarker":false,"iconRotate":0,"font":"monospace","prefix":"fa"},null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Address: Spring St, New York, NY, 10012 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/10/2013 06:42:55 PM <br>","Address: White Plains Rd, New York, NY, 10466 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 05/02/2013 06:16:33 AM <br>","Address: Commonwealth Ave, New York, NY, 10460 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 07/17/2013 02:48:46 AM <br>","Address: W 139 St, New York, NY, 10031 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 07/22/2013 03:45:20 PM <br>","Address: 68 Ave, New York, NY, 11379 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 09/09/2013 08:29:54 PM <br>","Address: W 145 St, New York, NY, 10039 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 12/13/2013 07:46:38 PM <br>","Address: Meeker Ave, New York, NY, 11222 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 01/04/2014 12:13:31 AM <br>","Address: Broadway, New York, NY, 10025 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/11/2014 05:20:26 AM <br>","Address: 37 Ave, New York, NY, 11372 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 04/21/2014 05:45:58 PM <br>","Address: Chestnut Ave, New York, NY, 10305 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/05/2014 01:07:50 AM <br>","Address: Hunts Point Ave, New York, NY, 10474 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 07/25/2014 12:32:08 AM <br>","Address: University Ave, New York, NY, 10453 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/17/2014 05:29:09 AM <br>","Address: 97 St, New York, NY, 11417 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/18/2014 04:19:49 PM <br>","Address: Flatbush Ave, New York, NY, 11217 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 01/21/2015 03:28:28 AM <br>","Address: 2nd Ave, New York, NY, 10009 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 03/26/2015 03:17:40 PM <br>","Address: 98 Ave, New York, NY, 11429 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 05/31/2015 06:20:32 PM <br>","Address: 13 Ave, New York, NY, 11219 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 10/03/2015 01:05:17 PM <br>","Address: W 17 St, New York, NY, 10011 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 10/20/2015 02:55:55 AM <br>","Address: Montgomery Ave, New York, NY, 10453 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 11/30/2015 11:48:30 AM <br>","Address: Valentine Ave, New York, NY, 10457 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 12/10/2015 11:42:06 PM <br>","Address: Dekalb Ave, New York, NY, 11237 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 03/29/2016 09:51:25 PM <br>","Address: 13 St, New York, NY, 11101 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 04/03/2016 03:42:30 PM <br>","Address: Parkside Ave, New York, NY, 11226 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 04/03/2016 10:47:44 PM <br>","Address: Arlington Ave, New York, NY, 11208 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 04/24/2016 06:23:09 PM <br>","Address: College Point Blvd, New York, NY, 11356 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 07/21/2016 09:51:03 PM <br>","Address: Benziger Ave, New York, NY, 10301 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 08/18/2016 06:09:05 PM <br>","Address: E 93 St, New York, NY, 10128 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 10/27/2016 03:24:27 AM <br>","Address: W 29 St, New York, NY, 10016 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 10/30/2016 01:04:09 AM <br>","Address: Vleigh Pl, New York, NY, 11367 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/30/2016 06:24:06 PM <br>","Address: E 135 St, New York, NY, 10454 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/11/2017 12:56:16 AM <br>","Address: Liberty Ave, New York, NY, 11419 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 03/04/2017 10:52:09 PM <br>","Address: Main St, New York, NY, 11367 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 03/12/2017 03:41:16 PM <br>","Address: 94 St, New York, NY, 11373 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 04/11/2017 06:25:45 PM <br>","Address: Mill Rd, New York, NY, 10306 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 05/21/2017 04:10:00 AM <br>","Address: Olmstead Ave, New York, NY, 10462 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/22/2017 03:40:35 PM <br>","Address: Myrtle Ave, New York, NY, 11385 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/24/2017 09:10:13 PM <br>","Address: E 9 St, New York, NY, 10003 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 06/28/2017 05:45:34 PM <br>","Address: Murray St, New York, NY, 10007 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 09/01/2017 06:47:28 PM <br>","Address: Walton Ave, New York, NY, 10453 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 09/27/2017 06:28:13 AM <br>","Address: Watson Ave, New York, NY, 10472 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 10/01/2017 02:39:55 PM <br>","Address: W 144 St, New York, NY, 10031 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 11/17/2017 03:14:09 PM <br>","Address: Prospect Ave, New York, NY, 10460 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/28/2017 06:51:30 PM <br>","Address: Commonwealth Ave, New York, NY, 10460 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/02/2018 05:29:06 AM <br>","Address: Riverside Dr, New York, NY, 10032 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/08/2018 01:47:17 PM <br>","Address: St Nicholas Ave, New York, NY, 10031 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 03/22/2018 10:50:30 PM <br>","Address: E 194 St, New York, NY, 10458 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 04/24/2018 05:27:47 AM <br>","Address: Steinway Ave, New York, NY, 10314 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/16/2018 07:04:53 PM <br>"],null,{"showCoverageOnHover":true,"zoomToBoundsOnClick":true,"spiderfyOnMaxZoom":true,"removeOutsideVisibleBounds":true,"spiderLegPolylineOptions":{"weight":1.5,"color":"#222","opacity":0.5},"freezeAtZoom":false},null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLegend","args":[{"colors":["black","pink","purple","darkblue","gray"],"labels":["000 - Other","100 - Transport, Religion, Recreation","400 - Residential","500 - Commercial Buildings","800 - Storage and Indoor Parking"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"unknown","title":"Building Type","extra":null,"layerId":null,"className":"info legend","group":null}]}],"setView":[[40.7484405,-73.9878531],13,[]],"limits":{"lat":[40.5614573,40.892043],"lng":[-74.1624124,-73.7385782]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#### 3. Fire Houses

The second data file contains the locations of the 218 firehouses in New York City. Start with the non-clustered map (2b) and now adjust the size of the circle markers by severity (`TOTAL_INCIDENT_DURATION` or `UNITS_ONSCENE` seem plausible options). More severe incidents should have larger circles on the map. On the map, also add the locations of the fire houses. Add two layers ("Incidents", "Firehouses") that allow the user to select which information to show. 


```r
# Read in data, fix those with missing coordinates by fixing addresses,
# then geocoding
firehouses <- read_csv("FDNY_Firehouse_Listing.csv") %>%
  mutate(FacilityAddress = ifelse(.data$FacilityName == "Marine 1",
                                  "Bloomfield St",
                                  FacilityAddress)) %>%
  mutate(FacilityAddress = ifelse(str_detect(.data$FacilityAddress, "Saint John.+"),
                                  "St Johns Place",
                                  FacilityAddress)) %>%
  mutate(FacilityAddress = ifelse(str_detect(.data$FacilityAddress,"81-17 North Boulevard"),
                                  "81-17 Northern Boulevard Queens",
                                  FacilityAddress))

firehouses_to_geocode <- firehouses %>% filter(is.na(Latitude))
geocoded <- geocode_OSM(paste(firehouses_to_geocode$FacilityAddress, firehouses_to_geocode$Borough))
firehouses_to_geocode$Latitude <- geocoded$lat
firehouses_to_geocode$Longitude <- geocoded$lon

firehouses <- rbind(firehouses, firehouses_to_geocode) %>% 
  filter(!is.na(Latitude))
```


```r
popup_firehouses <- paste("Unit:", firehouses$FacilityName, "<br>",
                          "Address:", firehouses$FacilityAddress, "<br>")

building_fires_firehouse_leaflet <- leaflet(building_fires) %>%
  addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
           attribution = '<a href = "maps.google.com">Google Maps</a>') %>%
  setView(lat = 40.7484405,
          lng = -73.9878531,
          zoom = 13) %>%
  addCircleMarkers(~lon, ~lat, 
                   popup = popup_fires,
                   color = building_fires$property_mkcol, 
                   radius = building_fires$UNITS_ONSCENE * 0.3,
                   group = "Fires") %>%
  addMarkers(data = firehouses,
             lng = ~Longitude,
             lat = ~Latitude,
             popup = popup_firehouses,
             group = "Firehouses") %>%
  addLegend(colors = c("black", "pink", "purple", "darkblue", "gray"),
            values = NULL,
            labels = sort(unique(building_fires$property_types)),
            title = "Building Type") %>%
  addLayersControl(overlayGroups = c("Fires", "Firehouses"))

building_fires_firehouse_leaflet
```

<!--html_preserve--><div id="htmlwidget-4c41b8ce11d039e58c1f" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-4c41b8ce11d039e58c1f">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"<a href = \"maps.google.com\">Google Maps<\/a>"}]},{"method":"addCircleMarkers","args":[[40.7233127,40.892043,40.8348028,40.8206784,40.7110625,40.8216804,40.7226986,40.7991517,40.7499236,40.6158732,40.8151609,40.8515333,40.6768721,40.6827699,40.7312885,40.7164946,40.6316739,40.7413184,40.8497801,40.8525315,40.698161,40.7511988,40.6552926,40.680811,40.7810491,40.639716,40.783266,40.7455865,40.723143,40.8059074,40.6870165,40.7286886,40.7418559,40.5614573,40.832241,40.7012329,40.7291949,40.7142035,40.8521389,40.8275142,40.8236333,40.8426909,40.8348028,40.834443,40.8266969,40.8641499,40.5919782],[-73.9993783,-73.858116,-73.8690163,-73.9486635,-73.8753396,-73.9391747,-73.9405144,-73.9686566,-73.8826722,-74.0724274,-73.8864169,-73.9144357,-73.8403393,-73.9765248,-73.9857092,-73.7385782,-73.9956158,-74.0002966,-73.9192496,-73.9000278,-73.925273,-73.9463421,-73.960771,-73.884893,-73.8459904,-74.0835728,-73.9507404,-73.9875248,-73.8204787,-73.9212625,-73.822181,-73.8227026,-73.86974,-74.1122509,-73.8542408,-73.8892971,-73.9872407,-74.0097319,-73.9076711,-73.8675633,-73.9459027,-73.890488,-73.8690163,-73.9490966,-73.943001,-73.8913106,-74.1624124],[16.2,24.9,14.7,16.5,15.9,20.7,24,25.2,25.5,16.2,14.4,16.2,14.7,17.1,62.1,19.8,37.8,20.1,14.1,25.8,25.2,18,19.2,26.1,15.3,18,27,33.3,22.2,16.2,26.1,18.9,27,15.9,18.9,17.4,20.1,31.2,15,15.3,31.5,23.1,25.8,25.2,27.9,18.9,16.8],null,"Fires",{"interactive":true,"className":"","stroke":true,"color":["purple","darkblue","purple","purple","purple","pink","darkblue","darkblue","darkblue","purple","darkblue","darkblue","purple","purple","purple","gray","purple","darkblue","purple","purple","purple","darkblue","darkblue","purple","darkblue","purple","purple","darkblue","darkblue","black","darkblue","pink","purple","purple","purple","darkblue","purple","darkblue","purple","purple","purple","purple","purple","purple","purple","darkblue","purple"],"weight":5,"opacity":0.5,"fill":true,"fillColor":["purple","darkblue","purple","purple","purple","pink","darkblue","darkblue","darkblue","purple","darkblue","darkblue","purple","purple","purple","gray","purple","darkblue","purple","purple","purple","darkblue","darkblue","purple","darkblue","purple","purple","darkblue","darkblue","black","darkblue","pink","purple","purple","purple","darkblue","purple","darkblue","purple","purple","purple","purple","purple","purple","purple","darkblue","purple"],"fillOpacity":0.2},null,null,["Address: Spring St, New York, NY, 10012 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/10/2013 06:42:55 PM <br>","Address: White Plains Rd, New York, NY, 10466 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 05/02/2013 06:16:33 AM <br>","Address: Commonwealth Ave, New York, NY, 10460 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 07/17/2013 02:48:46 AM <br>","Address: W 139 St, New York, NY, 10031 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 07/22/2013 03:45:20 PM <br>","Address: 68 Ave, New York, NY, 11379 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 09/09/2013 08:29:54 PM <br>","Address: W 145 St, New York, NY, 10039 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 12/13/2013 07:46:38 PM <br>","Address: Meeker Ave, New York, NY, 11222 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 01/04/2014 12:13:31 AM <br>","Address: Broadway, New York, NY, 10025 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/11/2014 05:20:26 AM <br>","Address: 37 Ave, New York, NY, 11372 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 04/21/2014 05:45:58 PM <br>","Address: Chestnut Ave, New York, NY, 10305 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/05/2014 01:07:50 AM <br>","Address: Hunts Point Ave, New York, NY, 10474 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 07/25/2014 12:32:08 AM <br>","Address: University Ave, New York, NY, 10453 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/17/2014 05:29:09 AM <br>","Address: 97 St, New York, NY, 11417 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/18/2014 04:19:49 PM <br>","Address: Flatbush Ave, New York, NY, 11217 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 01/21/2015 03:28:28 AM <br>","Address: 2nd Ave, New York, NY, 10009 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 03/26/2015 03:17:40 PM <br>","Address: 98 Ave, New York, NY, 11429 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 05/31/2015 06:20:32 PM <br>","Address: 13 Ave, New York, NY, 11219 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 10/03/2015 01:05:17 PM <br>","Address: W 17 St, New York, NY, 10011 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 10/20/2015 02:55:55 AM <br>","Address: Montgomery Ave, New York, NY, 10453 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 11/30/2015 11:48:30 AM <br>","Address: Valentine Ave, New York, NY, 10457 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 12/10/2015 11:42:06 PM <br>","Address: Dekalb Ave, New York, NY, 11237 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 03/29/2016 09:51:25 PM <br>","Address: 13 St, New York, NY, 11101 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 04/03/2016 03:42:30 PM <br>","Address: Parkside Ave, New York, NY, 11226 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 04/03/2016 10:47:44 PM <br>","Address: Arlington Ave, New York, NY, 11208 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 04/24/2016 06:23:09 PM <br>","Address: College Point Blvd, New York, NY, 11356 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 07/21/2016 09:51:03 PM <br>","Address: Benziger Ave, New York, NY, 10301 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 08/18/2016 06:09:05 PM <br>","Address: E 93 St, New York, NY, 10128 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 10/27/2016 03:24:27 AM <br>","Address: W 29 St, New York, NY, 10016 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 10/30/2016 01:04:09 AM <br>","Address: Vleigh Pl, New York, NY, 11367 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/30/2016 06:24:06 PM <br>","Address: E 135 St, New York, NY, 10454 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/11/2017 12:56:16 AM <br>","Address: Liberty Ave, New York, NY, 11419 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 03/04/2017 10:52:09 PM <br>","Address: Main St, New York, NY, 11367 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 03/12/2017 03:41:16 PM <br>","Address: 94 St, New York, NY, 11373 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 04/11/2017 06:25:45 PM <br>","Address: Mill Rd, New York, NY, 10306 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 05/21/2017 04:10:00 AM <br>","Address: Olmstead Ave, New York, NY, 10462 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/22/2017 03:40:35 PM <br>","Address: Myrtle Ave, New York, NY, 11385 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/24/2017 09:10:13 PM <br>","Address: E 9 St, New York, NY, 10003 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 06/28/2017 05:45:34 PM <br>","Address: Murray St, New York, NY, 10007 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 09/01/2017 06:47:28 PM <br>","Address: Walton Ave, New York, NY, 10453 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 09/27/2017 06:28:13 AM <br>","Address: Watson Ave, New York, NY, 10472 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 10/01/2017 02:39:55 PM <br>","Address: W 144 St, New York, NY, 10031 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 11/17/2017 03:14:09 PM <br>","Address: Prospect Ave, New York, NY, 10460 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 12/28/2017 06:51:30 PM <br>","Address: Commonwealth Ave, New York, NY, 10460 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/02/2018 05:29:06 AM <br>","Address: Riverside Dr, New York, NY, 10032 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 01/08/2018 01:47:17 PM <br>","Address: St Nicholas Ave, New York, NY, 10031 <br> Fire Severity: 4 - Confined to building of origin <br> Incident Date/Time: 03/22/2018 10:50:30 PM <br>","Address: E 194 St, New York, NY, 10458 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 04/24/2018 05:27:47 AM <br>","Address: Steinway Ave, New York, NY, 10314 <br> Fire Severity: 5 - Beyond building of origin <br> Incident Date/Time: 06/16/2018 07:04:53 PM <br>"],null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addMarkers","args":[[40.703466,40.710072,40.710048,40.715463,40.719759,40.715213,40.716395,40.721682,40.731752,40.720033,40.723072,40.727914,40.726815,40.733329,40.734625,40.756635,40.753239,40.741044,40.74843,40.738143,40.742209,40.748845,40.760792,40.766544,40.760892,40.774505,40.781992,40.784881,40.754676,40.756812,40.76696,40.771427,40.778584,40.798873,40.788714,40.794327,40.803129,40.795924,40.805068,40.813057,40.81177,40.821105,40.821586,40.83564,40.841394,40.848398,40.864975,40.813024,40.817502,40.820422,40.829652,40.834627,40.835037,40.836711,40.830637,40.815233,40.818659,40.846283,40.854413,40.841012,40.844121,40.84622,40.828518,40.8594,40.851495,40.84172,40.856116,40.876728,40.870433,40.822794,40.832288,40.846244,40.844894,40.859597,40.876176,40.877627,40.892803,40.870016,40.83332,40.821377,40.845338,40.695983,40.686982,40.700172,40.692948,40.672335,40.680489,40.652045,40.645773,40.674858,40.671829,40.665115,40.681357,40.675264,40.674798,40.682616,40.692896,40.689385,40.684161,40.697792,40.685357,40.695635,40.702562,40.692758,40.701506,40.712978,40.706024,40.730511,40.718405,40.715147,40.705334,40.698379,40.703721,40.696564,40.688473,40.670374,40.660877,40.675359,40.665206,40.677864,40.669602,40.65019,40.660226,40.648867,40.64012,40.636755,40.651165,40.640157,40.62835,40.638064,40.617597,40.619961,40.606301,40.627771,40.612343,40.609428,40.598744,40.578073,40.59823,40.576795,40.646469,40.619881,40.615732,40.584374,40.602306,40.745286,40.745335,40.735961,40.759696,40.767664,40.768508,40.773796,40.746168,40.762621,40.737732,40.741049,40.72625,40.736156,40.71295,40.718522,40.701448,40.712445,40.784806,40.789118,40.760003,40.762562,40.763428,40.764348,40.720329,40.738613,40.747071,40.766659,40.744582,40.689493,40.682684,40.695038,40.695411,40.684043,40.70731,40.693876,40.694052,40.714494,40.71706,40.673836,40.69301,40.663404,40.663992,40.65993,40.604897,40.593377,40.58633,40.580627,40.566034,40.644112,40.637734,40.63415,40.631093,40.615251,40.625167,40.575827,40.636052,40.635547,40.614265,40.598213,40.611873,40.596826,40.607485,40.590935,40.543205,40.554174,40.535332,40.553977,40.512518,40.7405824,40.6730215,40.6730215,40.7386146,40.762283],[-74.007538,-74.012523,-74.005245,-74.005938,-74.006678,-73.992901,-73.983478,-73.982622,-73.983536,-73.995689,-73.996804,-74.003157,-73.992643,-73.989421,-74.000346,-73.996217,-73.990078,-73.996041,-73.990447,-73.990964,-73.979535,-73.973888,-73.987182,-73.98035,-73.996405,-73.984619,-73.979627,-73.974434,-73.98161,-73.971142,-73.963856,-73.958521,-73.955516,-73.947759,-73.946774,-73.941345,-73.936244,-73.966726,-73.962929,-73.942358,-73.954858,-73.941849,-73.950689,-73.940856,-73.936836,-73.931516,-73.925403,-73.922395,-73.92037,-73.915778,-73.907831,-73.91331,-73.928176,-73.927231,-73.900986,-73.903418,-73.888853,-73.906532,-73.917148,-73.901458,-73.900366,-73.899437,-73.894951,-73.893546,-73.88775,-73.883628,-73.904179,-73.903539,-73.886796,-73.869368,-73.851115,-73.866503,-73.846663,-73.844218,-73.867003,-73.846678,-73.855483,-73.830873,-73.827418,-73.818597,-73.784737,-73.983159,-73.982876,-73.992239,-73.996971,-73.999611,-74.006522,-74.005218,-74.01329,-73.976491,-73.987476,-73.981408,-73.973561,-73.934917,-73.922015,-73.911687,-73.938066,-73.924006,-73.935957,-73.947874,-73.951464,-73.956361,-73.972211,-73.972774,-73.962398,-73.961259,-73.950317,-73.951056,-73.949027,-73.927956,-73.93162,-73.926877,-73.916253,-73.914423,-73.908246,-73.908033,-73.918214,-73.892863,-73.895179,-73.872853,-73.865685,-73.929035,-73.953483,-73.950471,-73.966847,-73.950974,-73.975736,-73.990379,-73.997741,-74.024825,-74.029547,-74.012986,-74.003637,-73.976148,-73.978973,-73.959346,-73.988542,-73.992901,-73.961967,-73.976468,-73.903789,-73.915718,-73.928488,-73.959127,-73.935658,-73.95215,-73.913527,-73.933706,-73.940603,-73.929574,-73.908822,-73.910194,-73.866158,-73.869024,-73.851904,-73.900971,-73.896512,-73.87905,-73.906791,-73.837437,-73.88674,-73.875009,-73.848387,-73.81633,-73.826312,-73.812512,-73.786616,-73.769596,-73.807761,-73.792967,-73.755681,-73.742451,-73.716559,-73.856104,-73.842194,-73.846268,-73.82626,-73.823106,-73.804242,-73.806943,-73.781115,-73.762763,-73.735871,-73.795461,-73.755814,-73.759473,-73.740515,-73.840045,-73.752226,-73.778978,-73.815775,-73.837683,-73.881874,-74.072559,-74.087746,-74.122199,-74.116574,-74.130971,-74.077772,-74.123727,-74.135343,-74.160224,-74.157735,-74.180462,-74.070467,-74.070138,-74.089111,-74.100895,-74.147097,-74.175809,-74.195611,-74.212812,-74.238822,-74.0100887,-73.9602304,-73.9602304,-73.9397516,-73.7566501],null,null,"Firehouses",{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Unit: Engine 4/Ladder 15 <br> Address: 42 South Street <br>","Unit: Engine 10/Ladder 10 <br> Address: 124 Liberty Street <br>","Unit: Engine 6 <br> Address: 49 Beekman Street <br>","Unit: Engine 7/Ladder 1/Battalion 1/Manhattan Borough Command <br> Address: 100-104 Duane Street <br>","Unit: Ladder 8 <br> Address: 14 North Moore Street <br>","Unit: Engine 9/Ladder 6 <br> Address: 75 Canal Street <br>","Unit: Engine 15/Ladder 18/Battalion 4 <br> Address: 25 Pitt Street <br>","Unit: Engine 28/Ladder 11 <br> Address: 222 East 2nd Street <br>","Unit: Engine 5 <br> Address: 340 East 14th Street <br>","Unit: Engine 55 <br> Address: 363 Broome Street <br>","Unit: Ladder 20/Division 1 <br> Address: 253 Lafayette Street <br>","Unit: Engine 24/Ladder 5/Battalion 2 <br> Address: 227-29 6th Avenue <br>","Unit: Engine 33/Ladder 9 <br> Address: 42 Great Jones Street <br>","Unit: Ladder 3/Battalion 6 <br> Address: 108 East 13th Street <br>","Unit: Squad 18 <br> Address: 132 West 10th Street <br>","Unit: Engine 34/Ladder 21 <br> Address: 440 West 38th Street <br>","Unit: Engine 26 <br> Address: 220 West 37th Street <br>","Unit: Engine 3/Ladder 12/Battalion 7 <br> Address: 150 West 19th Street <br>","Unit: Engine 1/Ladder 24 <br> Address: 142-46 West 31st Street <br>","Unit: Engine 14 <br> Address: 14 East 18th Street <br>","Unit: Engine 16 / Ladder 7 <br> Address: 234 East 29th Street <br>","Unit: Engine 21 <br> Address: 238 East 40th Street <br>","Unit: Engine 54/Ladder 4/Battalion 9 <br> Address: 782 8th Avenue <br>","Unit: Engine 23 <br> Address: 215 West 58th Street <br>","Unit: Rescue 1 <br> Address: 530 West 43rd Street <br>","Unit: Engine 40/Ladder 35 <br> Address: 131 Amsterdam Avenue <br>","Unit: Ladder 25/District Office 4/Division 3 <br> Address: 205-207 West 77th Street <br>","Unit: Engine 74 <br> Address: 120 West 83rd Street <br>","Unit: Engine 65 <br> Address: 33 West 43rd Street <br>","Unit: Engine 8 / Ladder 2 / Battalion 8 <br> Address: 167 East 51st Street <br>","Unit: Engine 39/Ladder 16 <br> Address: 157-59 East 67th Street <br>","Unit: Engine 44 <br> Address: 221 East 75th Street <br>","Unit: Engine 22/Ladder 13/Battalion 10 <br> Address: 159 East 85th Street <br>","Unit: Engine 58/Ladder 26 <br> Address: 1367 5th Avenue <br>","Unit: Engine 53/Ladder 43 <br> Address: 1836-46 3rd  Avenue <br>","Unit: Engine 91 <br> Address: 242 East 111th Street <br>","Unit: Engine 35/Ladder 14 <br> Address: 2282 3rd Avenue <br>","Unit: Engine 76/Ladder 22/Battalion 11 <br> Address: 145-51 West 100th Street <br>","Unit: Engine 47 <br> Address: 502 West 113th Street <br>","Unit: Engine 59/Ladder 30 <br> Address: 111 West 133rd Street <br>","Unit: Engine 37/Ladder 40 <br> Address: 415 West 125th Street <br>","Unit: Engine 69/Ladder 28/Battalion 16 <br> Address: 248 West 143rd Street <br>","Unit: Engine 80/Ladder 23 <br> Address: 503 West 139th Street <br>","Unit: Engine 84/Ladder 34 <br> Address: 513 West 161st Street <br>","Unit: Engine 67 <br> Address: 518 West 170th Street <br>","Unit: Engine 93/Ladder 45/Battalion 13 <br> Address: 515 West 181st Street <br>","Unit: Engine 95/Ladder 36 <br> Address: 29 Vermilyea Avenue <br>","Unit: Engine 60/Ladder 17/Battalion 14 <br> Address: 341 East 143rd Street <br>","Unit: Squad 41 <br> Address: 330 East 150th Street <br>","Unit: Engine 71/Ladder 55/Division 6 <br> Address: 720 Melrose Avenue <br>","Unit: Engine 50/Ladder 19 <br> Address: 1155 Washington Avenue <br>","Unit: Engine 92/Ladder 44/Battalion 17 <br> Address: 1259 Morris Avenue <br>","Unit: EMS Station 17 <br> Address: 1080 Ogden Avenue <br>","Unit: Engine 68/Ladder 49 <br> Address: 1160 Ogden Avenue <br>","Unit: EMS Station 26 <br> Address: 1264 Boston Road <br>","Unit: Engine 73/Ladder 42/Battalion 26 <br> Address: 655 Prospect Avenue <br>","Unit: Engine 94/Ladder 48/Battalion 3 <br> Address: 1226 Seneca Avenue <br>","Unit: Engine 42 <br> Address: 1781 Monroe Avenue <br>","Unit: Engine 43/Ladder 59 <br> Address: 1901 Sedgwick Avenue <br>","Unit: Rescue 3 <br> Address: 1655 Washington Avenue <br>","Unit: Engine 46, Ladder 27 <br> Address: 460 Cross Bronx Expressway <br>","Unit: Bronx Borough Command, District Office 6 & 7 <br> Address: 451 East 176th Street <br>","Unit: Engine 82/Ladder 31 <br> Address: 1213 Intervale Avenue <br>","Unit: Engine 48/Ladder 56/Division 7 <br> Address: 2417 Webster Avenue <br>","Unit: Engine 88/Ladder 38 <br> Address: 2225 Belmont Avenue <br>","Unit: Engine 45/Ladder 58/Battalion 18 <br> Address: 925 East Tremont Avenue <br>","Unit: Engine 75/Ladder 33/Battalion 19 <br> Address: 2175 Walton Avenue <br>","Unit: Engine 81/Ladder 46 <br> Address: 3025 Bailey Avenue <br>","Unit: Engine 79/Ladder 37/Battalion 27 <br> Address: 2928 Briggs Avenue <br>","Unit: Engine 96/Ladder 54 <br> Address: 1689 Story Avenue <br>","Unit: Engine 64/Ladder 47/District Office 7 <br> Address: 1214 Castle Hill Avenue <br>","Unit: Engine 90/Ladder 41 <br> Address: 1843 White Plains Road <br>","Unit: Squad 61/Battalion 20 <br> Address: 1518 Williamsbridge Road <br>","Unit: Engine 97 <br> Address: 1454 Astor Avenue <br>","Unit: Engine 62/Ladder 32 <br> Address: 3431 White Plains Road <br>","Unit: Engine 38/Ladder 51 <br> Address: 3446 Eastchester Road <br>","Unit: Engine 63/Ladder 39/Battalion 15 <br> Address: 755 East 233rd Street <br>","Unit: Engine 66/Ladder 61 <br> Address: 21 Asch Loop West <br>","Unit: Engine 89/Ladder 50 <br> Address: 2924 Bruckner Boulevard <br>","Unit: Engine 72 <br> Address: 3929 East Tremont Avenue <br>","Unit: Engine 70/Ladder 53 <br> Address: 169 Schofield Street <br>","Unit: Engine 207/Ladder 110/Battalion 31/Division 11/Brooklyn Borough Command <br> Address: 172 Tillary Street <br>","Unit: Engine 226 <br> Address: 409 State Street <br>","Unit: Engine 205/Ladder 118 <br> Address: 74 Middagh Street <br>","Unit: Engine 224 <br> Address: 274 Hicks Street <br>","Unit: Engine 279/Ladder 131 <br> Address: 252 Lorraine Street <br>","Unit: Engine 202/Ladder 101/Battalion 32 <br> Address: 31 Richards Street <br>","Unit: Engine 228 <br> Address: 436 39th Street <br>","Unit: Engine 201/Ladder 114/Battalion 40 <br> Address: 5113-5117 4th Avenue <br>","Unit: Squad 1 <br> Address: 788 Union Street <br>","Unit: Engine 239 <br> Address: 395 4th Avenue <br>","Unit: Engine 220/Ladder 122 <br> Address: 530-532 11th Street <br>","Unit: Ladder 105 <br> Address: 494 Dean Street <br>","Unit: Rescue 2 <br> Address: 1472 Bergen Street <br>","Unit: Engine 227 <br> Address: 423-25 Ralph Avenue <br>","Unit: Engine 233/Ladder 176 <br> Address: 25 Rockaway Avenue <br>","Unit: Engine 217 <br> Address: 940 Dekalb Avenue <br>","Unit: Engine 222/Battalion 37 <br> Address: 32 Ralph Avenue <br>","Unit: Engine 214/Ladder 111 <br> Address: 495 Hancock Street <br>","Unit: Engine 230 <br> Address: 701 Park Avenue <br>","Unit: Engine 235/Battalion 57 <br> Address: 206 Monroe Street <br>","Unit: Ladder 102 <br> Address: 850-56 Bedford Avenue <br>","Unit: Marine 6 <br> Address: Building 292 Brooklyn Navy Yard <br>","Unit: Engine 210 <br> Address: 160 Carlton Avenue <br>","Unit: Engine 211/Ladder 119 <br> Address: 26 Hooper Street <br>","Unit: Engine 221/Ladder 104 <br> Address: 161 South 2nd Street <br>","Unit: Engine 216/Ladder 108/Battalion 35 <br> Address: 187 Union Avenue <br>","Unit: Engine 238/Ladder 106 <br> Address: 205 Greenpoint Avenue <br>","Unit: Engine 229/Ladder 146 <br> Address: 75-77 Richardson Street <br>","Unit: Engine 206 <br> Address: 1201 Grand Street <br>","Unit: Engine 237 <br> Address: 43 Morgan Avenue <br>","Unit: Engine 218 <br> Address: 650 Hart Street <br>","Unit: Engine 271/Ladder 124/Battalion 28 <br> Address: 392 Himrod Street <br>","Unit: Engine 277/Ladder 112 <br> Address: 582 Knickerbocker Avenue <br>","Unit: Squad 252 <br> Address: 617 Central Avenue <br>","Unit: Engine 231/Ladder 120/Battalion 44 <br> Address: 107 Watkins Street <br>","Unit: Engine 283/Division 15 <br> Address: 885 Howard Avenue <br>","Unit: Engine 332/Ladder 175 <br> Address: 165 Bradford Street <br>","Unit: Engine 290/Ladder 103 <br> Address: 480 Sheffield Avenue <br>","Unit: Engine 236 <br> Address: 998 Liberty Avenue <br>","Unit: Engine 225/Ladder 107/Battalion 39 <br> Address: 799 Lincoln Avenue <br>","Unit: Engine 310/Ladder 174/Battalion 58 <br> Address: 5105 Snyder Avenue <br>","Unit: Engine 249/Ladder 113 <br> Address: 491 Rogers Avenue <br>","Unit: Engine 248 <br> Address: 2900 Snyder Avenue <br>","Unit: Engine 281/Ladder 147 <br> Address: 1210 Cortelyou Road <br>","Unit: Engine 255/Ladder 157 <br> Address: 1367 Rogers Avenue <br>","Unit: Engine 240/Battalion 48 <br> Address: 1307 Prospect Avenue <br>","Unit: Engine 282/Ladder 148 <br> Address: 4210-12 12th Avenue <br>","Unit: Engine 247 <br> Address: 1336 60th Street <br>","Unit: Engine 241/Ladder 109 <br> Address: 6630 3rd Avenue <br>","Unit: Engine 242 <br> Address: 9219 5th Avenue <br>","Unit: Engine 284/Ladder 149 <br> Address: 1157 79th Street <br>","Unit: Engine 243/Ladder 168/Battalion 42 <br> Address: 8653 18th Avenue <br>","Unit: Engine 250 <br> Address: 126 Foster Avenue <br>","Unit: Engine 330/Ladder 172 <br> Address: 2312 65th Street <br>","Unit: Engine 276/Ladder 156/Battalion 33 <br> Address: 1635 East 14th Street <br>","Unit: Engine 253 <br> Address: 2429 86th Street <br>","Unit: Engine 318/Ladder 166 <br> Address: 2510-14 Neptune Avenue <br>","Unit: Engine 254/Ladder 153 <br> Address: 901 Avenue U <br>","Unit: Engine 245/Ladder 161/Battalion 43 <br> Address: 2929 West 8th Street <br>","Unit: Engine 257/Ladder 170 <br> Address: 1361 Rockaway Parkway <br>","Unit: Engine 323 <br> Address: 6405 Avenue N <br>","Unit: Engine 309/Ladder 159 <br> Address: 1851 East 48th Street <br>","Unit: Engine 246/Ladder 169 <br> Address: 2732 East 11th Street <br>","Unit: Engine 321 <br> Address: 2165 Gerritsen Avenue <br>","Unit: Engine 258/Ladder 115 <br> Address: 10-40 47th Avenue <br>","Unit: Engine 325/Ladder 163 <br> Address: 41-24 51st Street <br>","Unit: Engine 259/Ladder 128/Battalion 45 <br> Address: 33-51 Greenpoint Avenue <br>","Unit: Engine 260 <br> Address: 11-15 37th Avenue <br>","Unit: Engine 262 <br> Address: 30-89 21st Street <br>","Unit: Engine 263/Ladder 117/Battalion 49 <br> Address: 42-06 Astoria Boulevard <br>","Unit: Engine 312 <br> Address: 22-63 35th Street <br>","Unit: Engine 289/Ladder 138 <br> Address: 97-28 43rd Avenue <br>","Unit: Engine 316 <br> Address: 27-12 Kearney Street <br>","Unit: Engine 324/Division 14 <br> Address: 108-01 Horace Harding Blvd. <br>","Unit: Eng 292/Rescue 4 <br> Address: 64-18 Queens Boulevard <br>","Unit: Squad 288/Hazmat 1 <br> Address: 56-29 68th Street <br>","Unit: Engine 287/Ladder 136/Battalion 46 <br> Address: 86-53 Grand Avenue <br>","Unit: Engine 291/Ladder 140 <br> Address: 56-07 Metropolitan Avenue <br>","Unit: Engine 305/Ladder 151 <br> Address: 111-02 Queens Boulevard <br>","Unit: Engine 286/Ladder 135 <br> Address: 66-44 Myrtle Avenue <br>","Unit: Engine 319 <br> Address: 78-11 67th Road <br>","Unit: Engine 297/Ladder 130 <br> Address: 119-11 14th Road <br>","Unit: Engine 295/Ladder 144 <br> Address: 12-49 149th Street <br>","Unit: Engine 273/Ladder 129 <br> Address: 40-18 Union Street <br>","Unit: Engine 274/Battalion 52 <br> Address: 41-20 Murray Street <br>","Unit: Engine 320/Ladder 167 <br> Address: 36-18 Francis Lewis Boulevard <br>","Unit: Engine 306/Battalion 53 <br> Address: 40-18 214th Place <br>","Unit: Engine 315/Ladder 125 <br> Address: 159-06 Union Turnpike <br>","Unit: Engine 299/Ladder 152 <br> Address: 61-20 Utopia Parkway <br>","Unit: Engine 326/Ladder 160 <br> Address: 64-04 Springfield Boulevard <br>","Unit: Engine 313/Ladder 164 <br> Address: 44-01 244th Street <br>","Unit: Engine 251 <br> Address: 254-20 Union Turnpike <br>","Unit: Engine 293 <br> Address: 89-40 87th Street <br>","Unit: Engine 285/Ladder 142 <br> Address: 103-17 98th Street <br>","Unit: Ladder 143 <br> Address: 101-02 Jamaica Avenue <br>","Unit: Squad 270/Division 13 <br> Address: 91-45 121st Street <br>","Unit: Engine 308/Battalion 51 <br> Address: 107-12 Lefferts Boulevard <br>","Unit: Engine 298/Ladder 127/Battalion 50 <br> Address: 153-11 Hillside Avenue <br>","Unit: Engine 303/Ladder 126 <br> Address: 104-12 Princeton Street <br>","Unit: Engine 275 <br> Address: 111-36 Merrick Boulevard <br>","Unit: Engine 301/Ladder 150 <br> Address: 91-04 197th Street <br>","Unit: Engine 304/Ladder 162 <br> Address: 218-44 97th Avenue <br>","Unit: Engine 302/Ladder 155 <br> Address: 143-15 Rockaway Boulevard <br>","Unit: Engine 317/Ladder 165/Battalion 54 <br> Address: 117-11 196th Street <br>","Unit: Engine 311/Ladder 158 <br> Address: 145-50 Springfield Boulevard <br>","Unit: Engine 314 <br> Address: 142-04 Brookville Boulevard <br>","Unit: Engine 331/Ladder 173 <br> Address: 158-57 Cross Bay Boulevard <br>","Unit: Engine 264/Engine 328/Ladder 134 <br> Address: 16-15 Central Avenue <br>","Unit: Engine 265/Ladder 121/Battalion 47 EMS Station 47 <br> Address: 48-06 Rockaway Beach Boulevard <br>","Unit: Engine 266/Battalion 47 <br> Address: 92-20 Rockaway Beach Boulevard <br>","Unit: Engine 268/Ladder 137 <br> Address: 257 Beach 116th Street <br>","Unit: Engine 329 <br> Address: 402 Beach 169th Street <br>","Unit: Marine 9 <br> Address: Saint George Ferry Terminal <br>","Unit: Engine 155/Ladder 78 <br> Address: 14 Brighton Avenue <br>","Unit: Ladder 79/Battalion 22 <br> Address: 1189 Castleton Avenue <br>","Unit: Engine 156 <br> Address: 412 Broadway <br>","Unit: Engine 163/Ladder 83 <br> Address: 875 Jewett Avenue <br>","Unit: Engine 153/Ladder 77 <br> Address: 74 Broad Street <br>","Unit: Engine 165/Ladder 85 <br> Address: 3067 Richmond Road <br>","Unit: Engine 157/Ladder 80 <br> Address: 1573 Castleton Avenue <br>","Unit: Engine 158 <br> Address: 65 Harbor Road <br>","Unit: Engine 166/Ladder 86 <br> Address: 1400 Richmond Avenue <br>","Unit: Engine 154/District Office 8/ Staten Island Borough Command <br> Address: 3730 Victory Boulevard <br>","Unit: Engine 152/Battalion 21 <br> Address: 256 Hylan Boulevard <br>","Unit: Engine 161/Ladder 81 <br> Address: 278 Mc Clean Avenue <br>","Unit: Engine 160/Rescue 5/Division 8 <br> Address: 1850 Clove Road <br>","Unit: Engine 159 <br> Address: 1592 Richmond Road <br>","Unit: Engine 162/Ladder 82/Battalion 23 <br> Address: 256 Nelson Avenue <br>","Unit: Engine 167/Ladder 87 <br> Address: 345 Annadale Road <br>","Unit: Engine 164/Ladder 84 <br> Address: 1560 Drumgoole Road West <br>","Unit: Engine 168/EMS Station 23 <br> Address: 1100 Rossville Avenue <br>","Unit: Engine 151/Ladder 76 <br> Address: 7219 Amboy Road <br>","Unit: Marine 1 <br> Address: Bloomfield St <br>","Unit: Engine 280/Ladder 132 <br> Address: St Johns Place <br>","Unit: Engine 234/Ladder 123/Battalion 38 <br> Address: St Johns Place <br>","Unit: Engine 261/Ladder 116 <br> Address: 3720-22 29th Street <br>","Unit: Engine 307/Ladder 154 <br> Address: 81-17 Northern Boulevard Queens <br>"],null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLegend","args":[{"colors":["black","pink","purple","darkblue","gray"],"labels":["000 - Other","100 - Transport, Religion, Recreation","400 - Residential","500 - Commercial Buildings","800 - Storage and Indoor Parking"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"unknown","title":"Building Type","extra":null,"layerId":null,"className":"info legend","group":null}]},{"method":"addLayersControl","args":[[],["Fires","Firehouses"],{"collapsed":true,"autoZIndex":true,"position":"topright"}]}],"setView":[[40.7484405,-73.9878531],13,[]],"limits":{"lat":[40.512518,40.892803],"lng":[-74.238822,-73.716559]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#### 4. Distance from Firehouse and Response Time

We now want to investigate whether the distance of the incident from the nearest firehouse varies across the city. 

##### a) Calculate Distance

For all incident locations (independent of severity), identify the nearest firehouse and calculate the distance between the firehouse and the incident location. Provide a scatter plot showing the time until the first engine arrived (the variables `INCIDENT_DATE_TIME`  and `ARRIVAL_DATE_TIME`) will be helpful. 

Now also visualize the patterns separately for severe and non-severe incidents (use `HIGHEST_LEVEL_DESC` but feel free to reduce the number of categories). What do you find?


```r
# Load in data
full_building_fires <- read_csv("building_fires.csv")
```


```r
# Closest firehouse and distance
fire_dist <- spDists(x = as.matrix(select(full_building_fires, lon, lat)),
                     y = as.matrix(select(firehouses, Longitude, Latitude)),
                     longlat = TRUE)
colnames(fire_dist) <- firehouses$FacilityName

fire_dist <- as.data.frame(fire_dist) %>% 
  cbind(full_building_fires$IM_INCIDENT_KEY) %>%
  select(`full_building_fires$IM_INCIDENT_KEY`, everything()) %>%
  rename(Incident_key = `full_building_fires$IM_INCIDENT_KEY`)

fire_dist_long <- pivot_longer(fire_dist, cols = -Incident_key,
                               names_to = "closest_firehouse",
                               values_to = "distance")

fire_dist_closest <- fire_dist_long %>%
  group_by(Incident_key) %>%
  filter(distance == max(distance))

full_building_fires <- left_join(full_building_fires,
                                 fire_dist_closest,
                                 by = c("IM_INCIDENT_KEY" = "Incident_key"))
```


```r
# Response time
full_building_fires <- full_building_fires %>% 
  mutate(INCIDENT_DATE_TIME = parse_date_time(full_building_fires$INCIDENT_DATE_TIME,
                                              orders = "/%m/%d/%Y %I:%M:%S %p",
                                              tz = "America/New_York"),
         ARRIVAL_DATE_TIME = parse_date_time(full_building_fires$ARRIVAL_DATE_TIME,
                                              orders = "/%m/%d/%Y %I:%M:%S %p",
                                              tz = "America/New_York")
         )

full_building_fires$response_time <- as.numeric(difftime(full_building_fires$ARRIVAL_DATE_TIME,
                                                         full_building_fires$INCIDENT_DATE_TIME))
```


```r
# Scatterplot (first remove 3 outliers)
full_building_fires %>% filter(response_time < 2000) %>%  
  filter(distance < 80) %>%
  ggplot(aes(x = response_time, y = distance)) + 
  geom_point(alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~HIGHEST_LEVEL_DESC) +
  labs(x = "Response time (seconds)",
       y = "Distance of fire from nearest firehouse (km)")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

```
## Warning: Computation failed in `stat_smooth()`:
## x has insufficient unique values to support 10 knots: reduce k.

## Warning: Computation failed in `stat_smooth()`:
## x has insufficient unique values to support 10 knots: reduce k.
```

![](assignment2_fire_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

##### b) Map of Response Times

Provide a map visualization of response times. Investigate whether the type of property affected (`PROPERTY_USE_DESC`) or fire severity (`HIGHEST_LEVEL_DESC`) play a role here.


```r
# Filter building fires to include only some of the most and least severe
# to keep things manageable - otherwise the whole map floods with dots
some_building_fires <- full_building_fires %>%
  filter(HIGHEST_LEVEL_DESC == "55 - Fifth Alarm" | 
           HIGHEST_LEVEL_DESC == "5 - 5th alarm" | 
           HIGHEST_LEVEL_DESC == "0 - Initial alarm"
           )
```



```r
nyc_bb <- c(bottom = 40.49612,
            top = 40.91553,
            left = -74.25559,
            right = -73.70001)

nyc_stamen <- get_stamenmap(bbox = nyc_bb, zoom = 10, maptype = "toner-background")
```

```
## Source : http://tile.stamen.com/toner-background/10/300/384.png
```

```
## Source : http://tile.stamen.com/toner-background/10/301/384.png
```

```
## Source : http://tile.stamen.com/toner-background/10/302/384.png
```

```
## Source : http://tile.stamen.com/toner-background/10/300/385.png
```

```
## Source : http://tile.stamen.com/toner-background/10/301/385.png
```

```
## Source : http://tile.stamen.com/toner-background/10/302/385.png
```


```r
ggmap(nyc_stamen) + 
  geom_point(data = some_building_fires,
             aes(x = lon, y = lat, colour = response_time),
             alpha = 0.5) +
  scale_colour_gradient(low = "blue", high = "red") +
  facet_wrap(~HIGHEST_LEVEL_DESC, ncol = 2)
```

![](assignment2_fire_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


Show a faceted choropleth map indicating how response times have developed over the years. What do you find?


```r
borough_response_time <- full_building_fires %>%
  mutate(year = year(full_building_fires$INCIDENT_DATE_TIME)) %>%
  group_by(BOROUGH_DESC, year) %>%
  summarise(mean_response_time = mean(response_time, na.rm = TRUE))
```


```r
borough_shape <- readOGR("Borough Boundaries/.",
                         "geo_export_c91e0d53-70a6-4c87-b4ce-37705e4ef829") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84")) %>%
  fortify()
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "C:\Users\timlx\Google Drive\TC Stuff\Analytics\GR 5063 - Data Visualisation\maps\FDNY fires\Borough Boundaries", layer: "geo_export_c91e0d53-70a6-4c87-b4ce-37705e4ef829"
## with 5 features
## It has 4 fields
```

```
## Regions defined for each Polygons
```

```r
borough_shape$id[borough_shape$id == "0"] <- "2 - Bronx"
borough_shape$id[borough_shape$id == "1"] <- "3 - Staten Island"
borough_shape$id[borough_shape$id == "2"] <- "4 - Brooklyn"
borough_shape$id[borough_shape$id == "3"] <- "5 - Queens"
borough_shape$id[borough_shape$id == "4"] <- "1 - Manhattan"

borough_shape <- left_join(borough_shape, borough_response_time, by = c("id" = "BOROUGH_DESC"))
```



```r
ggplot(borough_shape) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean_response_time)) + 
  facet_wrap(~year) + 
  theme_void()
```

![](assignment2_fire_files/figure-html/unnamed-chunk-22-1.png)<!-- -->


## Submission

Please follow the [instructions](/Exercises/homework_submission_instructions.md) to submit your homework. The homework is due on Wednesday, March 25.

## Please stay honest!

If you do come across something online that provides part of the analysis / code etc., please no wholesale copying of other ideas. We are trying to evaluate your abilities to visualize data, not the ability to do internet searches. Also, this is an individually assigned exercise -- please keep your solution to yourself.
