# Learn tmap package for generating static or interactive thematic maps
# code samples from CRAN vignette - tmap: get started!
# https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html

library(tmap)
library(sf)
library(dplyr)

data("World")

tm_shape(World) +
  tm_polygons("HPI") +
  tm_layout(bg.color = "skyblue", inner.margins = c(0, .02, .02, .02))

tmap_mode("view")

data(World, metro, rivers, land)

tmap_mode("plot")
## tmap mode set to plotting
tm_shape(land) +
  tm_raster("elevation", palette = terrain.colors(10)) +
  tm_shape(World) +
  tm_borders("white", lwd = .5) +
  tm_text("iso_a3", size = "AREA") +
  tm_shape(metro) +
  tm_symbols(col = "red", size = "pop2020", scale = .5) +
  tm_legend(show = FALSE)

tmap_options(bg.color = "black", legend.text.color = "white")

tm_shape(World) +
  tm_polygons("HPI", title = "Happy Planet Index")

tmap_style("col_blind")
## tmap style set to "classic"
## other available styles are: "white", "gray", "natural", "cobalt", "col_blind", "albatross", "beaver", "bw", "watercolor"

tm_shape(World) +
  tm_polygons("HPI", title = "Happy Planet Index")

tmap_options_diff()
tmap_options_reset()
?tmap_options

# export maps
tm <- tm_shape(World) +
  tm_polygons("HPI", legend.title = "Happy Planet Index")

## save an image ("plot" mode)
#tmap_save(tm, filename = "world_map.png")

## save as stand-alone HTML file ("view" mode)
#tmap_save(tm, filename = "world_map.html")

# quick thematic maps
qtm(World, fill = "HPI", fill.palette = "RdYlGn")

tmap_tip()

data(us_states, package = "spData")

db_states <- c("California", "Idaho", "Oregon", "Washington")

#state_abbrs <- c("CA", "ID", "OR", "WA")

pnw <- us_states |> dplyr::filter(NAME %in% db_states)

tm_shape(pnw)
