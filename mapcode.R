``` r code

#First we open our libraries. //Tidyverse for data processing //readxl to work with excel files
#//htmltools and widgets to export an html object //sf, simple feature, to work with spatial data
#//leaflet to map our data 

#---OPEN LIBRARIES---------------------

library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(htmltools)

#----CODE -----------------------------

#We open our excel file
volcans=read_excel(path = "paht",range = "B2:K1572")

#We place our spatial columns at the end of our dataframe
volcans=select(volcans,-Latitude,-Longitude,Longitude,Latitude)

#We transform our dataframe into a sf object. I will come back latter on what is a sf object
volcans_sf=st_as_sf(volcans,coords = c("Longitude","Latitude"))

#We can rename our attributes variables
names(volcans_sf)[2]="Volcano_Name"

#We need to give a coordinate reference system (crs) to our sf object. Why and what is a crs?
#Our planet is more or less a 3D sphere when we project it on a 2D, we need to make a projection. There are
#multiple projections possibles. They are two types of crs (or projections) possible : distance (e.g meters) and
#geographic coordinates system (latitude longitude). In the latter category, the most common projection 
#is the WGS84 (world geodetic system). If you are insterested in knowing more about how humans accommodate #both the curved and flat views of the world (link: https://gisgeography.com/wgs84-world-geodetic-system/).
#The 4326 is acutally the id number (called EPSG) of the WGS84 crs.EPSG stands for European Petroleum Survey #Group and is an organization that maintains a geodetic parameter database with standard codes, the EPSG codes.

volcans_sf=st_set_crs(volcans_sf,4326)

#You can also clean your data in R
#REPLACE CODE BY ERUPTION TIME
volcans_sf$Eruption=str_replace(volcans_sf$Eruption, "Unknown", "Not Known")
volcans_sf$Eruption=str_replace(volcans_sf$Eruption, "D1", "1964 or later")
volcans_sf$Eruption=str_replace(volcans_sf$Eruption, "D2", "1900-1963")
volcans_sf$Eruption=str_replace(volcans_sf$Eruption, "D3", "1800-1899")
volcans_sf$Eruption=str_replace(volcans_sf$Eruption, "D4", "1700-1799")
volcans_sf$Eruption=str_replace(volcans_sf$Eruption, "D5", "1500-1699")
volcans_sf$Eruption=str_replace(volcans_sf$Eruption, "D6", "A.D. 1-1499")
volcans_sf$Eruption=str_replace(volcans_sf$Eruption, "D7", "B.C. (Holocene)")
volcans_sf$Eruption=str_replace(volcans_sf$Eruption, "U", "B.C. (Holocene)")
volcans_sf$Eruption=str_replace(volcans_sf$Eruption, "Q", "Quaternary eruption(s)")

#We set Eruption as a factor variable which means as a categorical variable. This variable can
#take only a limited number of different values (called levels).

volcans_sf$Eruption=as.factor(volcans_sf$Eruption)

#One interesting analysis is to see which volcanoes are above or below the sea surface. So we create
#a conditionnal variable called "binary" (only two outputs possible)
volcans_sf=volcans_sf %>% 
  mutate(binary=as.factor(
    ifelse(Elev>0,
           "positive",
           "negative")
  )
  )
#We create two dataframes for above or under sea level
volcans_sf_positive=volcans_sf %>% filter(binary>0)
volcans_sf_negative=volcans_sf %>% filter(binary<0)

#Download tectonic plates boundaries data

platesboundaries <- geojsonsf::geojson_sf("https://raw.githubusercontent.com/fraxen/tectonicplates/master/GeoJSON/PB2002_plates.json")


#MAPPING--------------------------------------------------------------------------

#we stylize our popup with the info we want to display. To style a bit our popup, we use html code
# <br/> to break lin, we also refer to three variables (infos): the volcano name, elevation and last # eruption data.
content=paste("<b>",volcans_sf$Volcano_Name,"</b>","<br/>",
              "<p>Elevation:",volcans_sf$Elev,"m<p/>",
              "<p>Last Eruption:",volcans_sf$Eruption,"<p/>"
)
# We set a color palette Red/Green for our makers of elevation above or below sea level
pal=colorFactor(palette=c("red","green"),volcans_sf$binary)

#On top of our earth map, we will display 2 layers: one with volcanoes location and one color coded 
#on volcanoes elevation compared to the sea level
leafletMap=leaflet(volcans_sf) %>% 
  addTiles() %>% 
  addPolylines(data = platesboundaries) %>% #Add tectonic plates boundaries
  addMarkers(icon=makeIcon("iconpath",iconWidth = 40,iconHeight = 40), #With icon path we load a png #image for our icon. Replace iconpath with your own path
             label =lapply(content,HTML), #lapply will read content variable as an HTML object
             labelOptions = labelOptions(style=list("font-size" = "15px")),
             group="Volcanoes general info") %>% 
  addCircleMarkers(color= ~pal(binary), 
                   group="Under or over sea level ?",
                   label =lapply(content,HTML),
                   labelOptions = labelOptions(style=list("font-size" = "15px"))) %>% 
  addLegend(position="bottomright",pal=pal,
            values=~binary,
            title="Under or over sea level ?", 
            opacity=1) %>% 
  addLayersControl( #Function added to control our 2 layers
    overlayGroups = c("Volcanoes general info","Under or over sea level ?"),
    options=layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Under or over sea level ?")) #We want to display only one layer at a time
leafletMap 


saveWidget(leafletMap, "leafMap.html")  #we can save our map as an html object and therefore use it 
# in a website, as I did under the code.
```