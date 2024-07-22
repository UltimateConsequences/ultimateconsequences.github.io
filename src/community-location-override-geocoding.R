library(osmdata)
library(sf)
library(googlesheets4)

filename <- "Bolivia Deaths Database"
clo.filename <- "data/geocode/community-location-override.rds"
database_file <- drive_find(filename, n_max=1) # drive_find returns a dataframe of matching files

##
community_location_override_table.in <- range_read(ss = database_file,
            range = "CommunityLocationOverrideTable" )
update_versioned_archive(community_location_override_table.in, clo.filename)

##

community_location_override_table <- read_rds(here::here("data", "geocode", "community-location-override.rds"))

dat1 <- opq_osm_id (type = "node", id = community_location_override_table$openstreetmap_node) %>%
  opq_string () %>%
  osmdata_sf ()

dat1 <- dat1$osm_points %>% select(osm_id, geometry) %>% 
                    mutate(lon=st_coordinates(geometry)[,1]) %>% 
                    mutate(lat=st_coordinates(geometry)[,2]) %>%
                    st_drop_geometry() %>% 
                    mutate(osm_id = as.numeric(osm_id)) %>%
                    rename(openstreetmap_node=osm_id) 

community_location_override_table_geocoded <- left_join(select(community_location_override_table, -lat, -lon), 
                                                          dat1)

## Access the database file
library(tidyverse)
library(googledrive)
library(googlesheets4)

source(here::here("src", "update-versioned-archive.R"))

filename <- "Bolivia Deaths Database"
database_file <- drive_find(filename, n_max=1) # drive_find returns a dataframe of matching files

range_write(ss = database_file,
            data = community_location_override_table_geocoded,
            sheet = "LocationOverrideTables",
            range = cell_limits(c(3, 1), 
                                c(4+nrow(community_location_override_table_geocoded), 
                                  1+ncol(community_location_override_table_geocoded)))
            )


##
mlo.filename <- "data/geocode/municipality-location-override.rds"
municipality_location_override_table.in <- range_read(ss = database_file,
                                                   range = "MunicipalityLocationOverrideTable" )
update_versioned_archive(municipality_location_override_table.in, mlo.filename)
##
municipality_location_override_table <- read_rds(here_filename(mlo.filename))
