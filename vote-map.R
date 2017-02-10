# Mapping Midwestern county votes, 2012-2016
# Jason A. Heppler <jason@jasonheppler.org>

library(leaflet)
library(rio)
library(tidyr)
library(scales)
library(tigris)
library(dplyr)
library(htmlwidgets)
library(shiny)

# Read in data and join to shapefile
datafile <- "~/Desktop/github/county_elections/2016_US_County_Level_Presidential_Results.csv"
flippedCounties <- "~/Desktop/github/county_elections/US_County_Level_Presidential_Results_12-16.csv"
election <- rio::import(datafile)
flipped <- rio::import(flippedCounties)

shp_data <- counties(state = c("MN","SD", "ND", "NE", "WI", "IA", "IL", "OH", "MI", "IN", "MO", "KS"), cb = TRUE, resolution = "20m")
shp_data@data <- shp_data@data %>%
                    mutate(combined_fips = paste(STATEFP, COUNTYFP, sep=""))

library(raster)
midwestMap <-  merge(shp_data, election, by.x = "combined_fips", by.y = "combined_fips")
midwestFlippedData <- merge(midwestMap, flipped)

# Determine which counties flipped in the 2016 election
## thinking through this: if 2012 dem < 2012 gop = flipped
## if 2012 dem >= 2012 dem = incumbent
## if 2012 gop >= 2012 gop = incumbent
## if 2012 gop < 2016 gop = flipped
midwestFlippedData@data$flipped_counties <- 

# Find min and max percentages for the palette
minPct <- min(c(midwestMap@data$per_dem, midwestMap@data$per_gop))
maxPct <- max(c(midwestMap@data$per_dem, midwestMap@data$per_gop))
midwestPalette <- colorBin(palette = "Blues", domain = c(minPct, maxPct), bins = c(0, .3, .4, .5, .6, .7, .8, .9, 1), pretty = FALSE)
midwestFlippedPalette <- colorBin(palette = "RdBu", domain = c(1, 0), bins = 2)

midwestPopup <- paste0("<b>", "County: ", "</b>", midwestMap@data$county_name, "<br>",
                       "<b>", "GOP votes: ", "</b>", midwestMap@data$votes_gop, "<br>",
                       "<b>", "Dem votes: ", "</b>", midwestMap@data$votes_dem, "<br>",
                       "<b>", "Difference: ", "</b>", midwestMap@data$diff, "<br>")

baseMap <- "https://api.mapbox.com/styles/v1/hepplerj/cih5dktre003ba0m3p7raqa7p/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiaGVwcGxlcmoiLCJhIjoiMjNqTEVBNCJ9.pGqKqkUDlcFmKMPeoARwkg"
mbAttribution <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>"

# Map
midwestResults <- leaflet(shp_data) %>%
  addTiles(urlTemplate = baseMap, attribution = mbAttribution) %>%
  addPolygons(stroke = TRUE,
              smoothFactor = 0.2,
              weight = 1,
              fillOpacity = 0.6,
              color = ~midwestPalette(midwestMap@data$per_dem),
              popup = midwestPopup,
              group = "Democratic Vote"
             ) %>%
  addPolygons(stroke = TRUE,
              smoothFactor = 0.2,
              weight = 1,
              fillOpacity = 0.6,
              color = ~midwestPalette(midwestMap@data$per_gop),
              popup = midwestPopup,
              group = "Republican Vote"
              ) %>%
   addPolygons(stroke = TRUE,
              smoothFactor = 0.2,
              weight = 1,
              fillOpacity = 0.6,
              color = ~midwestPalette(midwestFlippedData@data$),
              popup = midwestPopup,
              group = "Flipped Counties"
              ) %>%
  addLegend(position="bottomright", title = "Flipped Counties", colors=c('#2166ac', '#f5f5f5', '#b2182b'), labels=c("Dem", "Incumbent", "Repub"))  %>%
  addLegend("bottomright", pal = midwestPalette, values = ~midwestMap@data$per_dem, title = "Results",
             labFormat = labelFormat(suffix = '%', between = '% - ',
                                     transform = function(x) 100 * x)
           ) %>%
  addLayersControl(
    baseGroups=c("Democratic Vote", "Republican Vote", "Flipped Counties"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  )

midwestResults

# Shiny

ui <- bootstrapPage(div(class="outer",
                        tags$style(type = "text/css", ".outer {position: fixed; top: 0; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0;"),
                        leafletOutput("map", width = "100%", height = "100%"),
                        absolutePanel(top = 60, right = 10, draggable = TRUE)
                        ))
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    midwestResults
  })
}

shinyApp(ui, server)
