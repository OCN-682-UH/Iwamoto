## Week 7 Lecture Script: Maps in R
## Mei Iwamoto
## October 6th, 2026

#install packages
#install.packages(c("maps", "mapdata", "mapproj"))

#load libaries
library(tidyverse)
library(here)
library(maps) #simple polygon basemaps (country, state, county)
library(mapdata)
library(mapproj)

# Population in California by county
popdata <- read_csv(here("Week_07", "Data", "CApopdata.csv"))

# Seastar counts at field sites in California
stars <- read_csv(here("Week_07", "Data", "stars.csv"))

#this pulls polygon boundaries for all countries in the world 
world <- map_data("world") 

#could also do for separate countries
usa <- map_data("usa")

states <- map_data("state") #here are the US states (contiguous only :()

counties <- map_data("county") #us counties

#structure of this data includes longitude, latitude, order (order in which ggplot connects the dots), region/subregion, group (this is critical, controls whether adjacent points are connected by lines, points in the same group get connected and different groups are not)

#make a map of the world
ggplot() +
  geom_polygon(data= world,
                aes(x=long, y=lat, group=group)) #group = group very important here 

#by using ggplot, you are able to add additional points from different datasets onto this polygon?

#add colors to lines and fill
ggplot() +
  geom_polygon(data= world,
               aes(x=long, y=lat, group=group, fill = region), #here region is all the different countries, so each country has a diff color 
               color="purple") +
  guides(fill="none") + #need to remove the legend here because there are too many countries in this map 
  theme_minimal() +
  theme(panel.background =
          element_rect(fill = "lightblue")) + #change theme and color of ocean to blue 
  coord_map(projection = "mercator", #this changes the map projection to mercator
            xlim = c(-180, 180))

#filter the states to show just california 
CA_data <- states |> 
  filter(region == "california")

#make map of california 
ggplot() +
  geom_polygon(data=CA_data,
               aes(x=long, y=lat, group=group, fill="orange"), #here thereʻs no regions since its just california, so can do color 
               color="black") +
  guides(fill="none") +
  theme_minimal() +
  theme(panel.background =
          element_rect(fill="lightblue"))


#adding multiple layers of data
#plot the population of every county in california 

head(counties)[1:3, ]

head(popdata)

#use inner join, rename column names to match them up 
CApop_county <- popdata |>
  select("subregion" = County, Population) |>   # rename to match counties
  inner_join(counties) |>
  filter(region == "california")                 # some county names repeat in other states

head(CApop_county)

#make map
ggplot() +
  geom_polygon(data = CApop_county,
               aes(x = long, y = lat,
                   group = group,
                   fill = Population), #here do fill = population
               color = "black") +
  coord_map() +
  theme_void() +
  scale_fill_gradient(transform = "log10") #do a log transformation to make it easier to interpret, so that the less populated counties are still visible alongside LA


#add another layer of points with the stars dataset, seastar counts at field sites
head(stars)

ggplot() +
  geom_polygon(data = CApop_county,
               aes(x = long, y = lat,
                   group = group,
                   fill = Population),
               color = "black") +
  geom_point(data = stars,        #here you can add another geom layer to the map to add the stars data 
             aes(x = long,
                 y = lat, size = star_no)) + #change the size to reflect star number here 
  coord_map() +
  theme_void() +
  scale_fill_gradient(transform = "log10") +
  labs(size = "# stars/m²") #here you can change the legend title 

#save the map
ggsave(here("Week_07", "Outputs", "CApop.pdf"))


# the sf package is more modern approach, also rnaturalearth package
#with sf, you do geom_sf rather than geom_polygon, also no need to do group=group
#compatible with other packages

#install packages and load libaries
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#here, theres no group or lat/long to specify like with maps package
world_sf <- ne_countries(scale = "medium",
                         returnclass = "sf")

ggplot(world_sf) +
  geom_sf(fill = "lightgray",
          color = "white") +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme_void() +
  labs(title = "Robinson Projection")

#lecture part 2
#install packages
#install.packages(c(
"maptiles",      # download tile basemaps (satellite, street, terrain)
  "tidyterra",     # plot terra rasters in ggplot2
  "leaflet",       # interactive HTML maps
  "tidygeocoder",  # free geocoding (OpenStreetMap / Census)
  "ggspatial"))      # scale bars and north arrows


#load libraries
library(tidyverse)
library(here)
library(sf)
library(maptiles)
library(tidyterra)
library(leaflet)
library(tidygeocoder)
library(ggspatial)

#data for today
ChemData <- read_csv(here("Week_07", "Data", "chemicaldata_maunalua.csv"))

glimpse(ChemData)

#get_tiles() needs to know what area to download, so we give it a bounding box like below

# Define a bounding box around Wailupe, Oahu
# xmin/xmax = westmost/eastmost longitude
# ymin/ymax = southernmost/northernmost latitude
wailupe_bbox <- st_as_sfc(   #this converts it to an sf geometry object so that maptiles can read it 
  st_bbox(c(xmin = -157.772, ymin = 21.267, #this recreates the bounding rectangle
            xmax = -157.752, ymax = 21.281),
          crs = 4326) # crs = 4326 means WGS84 — the lat/lon system your GPS uses
)


#step 2, download the tiles 

tiles_satellite <- get_tiles(
  wailupe_bbox,               # the area to download
  provider = "Esri.WorldImagery", # which tile source to use
  zoom = 16,                  # level of detail (3 = continent, 20 = building)
  crop = TRUE,                # clip to our exact bbox (vs. slightly larger)
  cachedir = here("Week_07", "data", "tiles_cache") # save tiles to disk
)

#what does it then return? it gives you a spatraster which is a raster image (grid of pixels)
class(tiles_satellite)

terra::nlyr(tiles_satellite) # number of layers (R, G, B), has 3

# geom_spatraster_rgb() is from {tidyterra}
# It reads the 3 color layers and renders them as a photo, 
ggplot() +
  geom_spatraster_rgb(data = tiles_satellite) +
  coord_sf(crs = 4326) +
  theme_minimal()

#you can also derive the bounding box from your data without manually typing the coords
# Step 1: convert your data frame to an sf object
# coords tells sf which columns hold longitude and latitude
ChemData_sf <- st_as_sf(ChemData,
                        coords = c("Long", "Lat"),
                        crs = 4326)

# Step 2: get the bounding box and add a small buffer
# the + c(...) pads all four sides by ~0.4 km so points aren't at the edge
bbox_chem <- st_bbox(ChemData_sf) + c(-0.004, -0.004, 0.004, 0.004)

# Step 3: download tiles for that extent
Map_satellite <- get_tiles(
  st_as_sfc(bbox_chem),
  provider = "Esri.WorldImagery",
  zoom = 15,
  crop = TRUE,
  cachedir = here("Week_07", "data", "tiles_cache")
)

#once you have a tile basemap, add ggplot layers on top exactly as you normally would

ggplot() +
  geom_spatraster_rgb(data = Map_satellite) +
  geom_point(data = ChemData,
             aes(x = Long, y = Lat,
                 color = Salinity),
             size = 4) +
  scale_color_viridis_c() +
  coord_sf(crs = 4326) +
  theme_minimal() +
  annotation_scale(           # scale bar, from ggspatial
    location = "bl",          # bl = bottom-left
    bar_cols = c("yellow", "white")) +
  annotation_north_arrow(     # north arrow
    location = "tl")        # tl = top-left

#polish it up and save the map!
ggplot() +
  geom_spatraster_rgb(data = Map_satellite) +
  geom_point(data = ChemData,
             aes(x = Long, y = Lat, color = Salinity),
             size = 4) +
  scale_color_viridis_c(name = "Salinity (ppt)") +
  annotation_scale(location = "bl",
                   bar_cols = c("yellow", "white")) +
  annotation_north_arrow(location = "tl") +
  coord_sf(crs = 4326) +
  labs(title = "Maunalua Bay — Water Chemistry",
       caption = "Basemap: ESRI World Imagery") +
  theme_minimal()

ggsave(here("Week_07", "Outputs", "maunalua_map.pdf"), width = 8, height = 6)


#how to do interactive maps with leaflet, where you can zoom and pan and click on your data 

#you use leaflet by piping layers together 
leaflet() |>                       # 1. create the map widget
  addProviderTiles(...) |>         # 2. add a basemap tile layer
  addCircleMarkers(...) |>         # 3. add your data as markers
  addLegend(...)                   # 4. add a color legend

#first leaflet map
leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery) |>
  setView(lng = -157.762, lat = 21.274, zoom = 14) #this centers the map on a specific location

#in leaflet, column names are written with a ~ prefix 
addCircleMarkers(lng = ~Long, lat = ~Lat, color = ~pal(Salinity)) #where Long is the column name from the dataframe

#leaflet needs a color palette
# colorNumeric() returns a FUNCTION, not a color vector
pal <- colorNumeric(
  palette = "viridis",            # any RColorBrewer or viridis palette name
  domain = ChemData$Salinity      # the full range of values to map
)

# Use it by calling pal(value) — it returns a hex color
pal(30)  # what color does salinity = 30 get?

pal(40)  # higher salinity → different color

#add data points with color
pal <- colorNumeric("viridis", domain = ChemData$Salinity)

leaflet(ChemData) |>           # pass data frame here
  addProviderTiles(providers$Esri.WorldImagery) |>
  addCircleMarkers(
    lng = ~Long, lat = ~Lat,   # which columns are coordinates
    color = ~pal(Salinity),    # apply our color function to Salinity
    radius = 6,
    fillOpacity = 0.9,
    popup = ~paste("Salinity:", Salinity, "ppt",  # click a point to see this
                   "<br>Site:", Site)) |>
  addLegend(
    pal = pal,                 # the same color function
    values = ~Salinity,        # which column drives the legend
    title = "Salinity (ppt)",
    position = "bottomright")

#popup vs label, where popup appears when you click a marker, label appears when you hover over
#addCircleMarkers(popup = ~paste("Site:", Site,"<br>Salinity:", Salinity))

#addCircleMarkers(label = ~Site)


#geocoding with tidygeocoder
#geocoding = converting a place name/ address to lat and long coords 
#OpenStreetMap (method = "osm") — best for landmarks and place names worldwide
#US Census (method = "census") — best for US street addresses

#geo() geocodes a single location string and returns a one-row tibble:
geo("University of Hawaii at Manoa", method = "osm")

oahu_sites <- tribble( #geocode a dataframe
  ~place,
  "University of Hawaii at Manoa",
  "Honolulu International Airport",
  "Pearl Harbor National Memorial, Hawaii",
  "Diamond Head State Monument, Hawaii"
)

oahu_geocoded <- oahu_sites |>
  geocode(place, method = "osm") # geocode the 'place' column

oahu_geocoded

#to map geocoded location
leaflet(oahu_geocoded) |>
  addProviderTiles(providers$Esri.WorldImagery) |>
  addMarkers(
    lng = ~long, lat = ~lat,   # tidygeocoder always returns 'lat' and 'long'
    label = ~place)            # hover to see place name
