library(devtools)
remotes::install_github('christopherkenny/dots')

library(dots)
library(sf)
#> Linking to GEOS 3.11.2, GDAL 3.6.2, PROJ 9.2.0; sf_use_s2() is TRUE
library(ggplot2)
data('suffolk')
dots::dots(suffolk, c(pop_black, pop_white), divisor = 1000, engine = engine_sf_random) + 
  scale_color_viridis_d() + 
  theme_void()
