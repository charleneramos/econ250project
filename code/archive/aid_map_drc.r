# Install necessary packages if not installed
install.packages(c("sf", "tmap", "dplyr", "readr"))


library(sf)
library(tmap)
library(dplyr)
library(readr)

# 1️⃣ Load your GeoQuery CSV
# data <- read.csv("C:/Users/riana/Dropbox/data/DRC AidData/68e2dbe64349810dd408a212_results.csv", header=FALSE, row.names=1)
df <- read.csv('68e2dbe64349810dd408a212_results.csv')
data <- df


# 2️⃣ Load ADM3 boundaries for DRC from GeoBoundaries (sf object)
# Correct shapefile URL (zip archive)
# url <- "https://www.geoboundaries.org/data/geoBoundaries-4_2_0/gbOpen/COD/ADM3/geoBoundaries-4_2_0_COD_ADM3.geojson"
# 
# adm3 <- st_read(url)
# 
# download.file(
#   "https://www.geoboundaries.org/data/geoBoundaries-4_2_0/gbOpen/COD/ADM3/geoBoundaries-4_2_0_COD_ADM3.geojson",
#   destfile = "COD_ADM3.geojson",
#   mode = "wb"
# ) #go here to find the data
adm3 <- st_read("COD_ADM3.geojson")

# 3️⃣ Merge GeoQuery data with shapefile by region name
merged <- adm3 %>%
  left_join(data, by = c("shapeName" = "shapeName"))

# 4️⃣ Visualize total aid by ADM3 region
tmap_mode("plot")
tm_shape(merged) +
  tm_polygons(
    fill = "drc.aims_geocodedresearchrelease_level1_v1_3_1.bb09d14.sum",
    style = "quantile",
    palette = "YlOrRd",
    alpha = 0.7,
    title = "Total Aid (USD)"
  ) +
  tm_basemap("OpenStreetMap") +
  tm_layout(
    main.title = "DRC Aid Distribution (GeoQuery / AMP data, as of 2013)",
    legend.outside = TRUE
  )

tm_basemap("Esri.WorldImagery")


