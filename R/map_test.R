
library(rgdal)
library(spatialEco)
library(leaflet)
library(leaflet.providers)

districts <- readOGR("./data/app/school_districts.geojson")
supts <- read_csv("./data/app/superintendents.csv")

app_data <- readRDS("./data/app/app_data.rds")

districts@data$NAME <- trim(districts@data$NAME)
app_data$Enrollment$Divisions$Summary$CurrentEnrollment$DivisionName <- trim(appData$Enrollment$Divisions$Summary$CurrentEnrollment$DivisionName)

x <- districts@data %>%
  left_join(
    app_data$Enrollment$Divisions$Summary$CurrentEnrollment,
    by = c("NAME" = "DivisionName")
  ) %>%
  mutate(
    Enrollment = parse_number(Enrollment)
  ) %>%
  left_join(
    supts,
    by = c("NAME" = "DivisionName")
  )

districts@data <- x

bins <- c(0, 500, 1000, 5000, 10000, 20000, 50000, 100000, 200000, Inf)
pal <- colorBin(viridisLite::viridis(999), NULL, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>
  <hr>
  %s students reported for 2020-2021",
  districts@data$NAME, format(districts@data$Enrollment, big.mark = ",")
) %>% lapply(htmltools::HTML)

star_icon <- makeIcon(
  iconUrl = "./www/img/red-star.png",
  iconWidth = 10,
  iconHeight = 10
)

leaflet(data = districts) %>%
  addProviderTiles("CartoDB.Positron") %>%
  clearShapes() %>%
  addPolygons(
    stroke = TRUE,
    weight = 1,
    color = "#003",
    smoothFactor = 0,
    fillColor = ~ pal(Enrollment),
    fillOpacity = 0.7,
    layerId = ~GEOID,
    label = labels
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = districts@data$Enrollment,
    title = "Enrollment"
  )
