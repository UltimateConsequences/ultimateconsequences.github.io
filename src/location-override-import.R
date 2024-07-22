library(tidyverse)
library(googledrive)
library(googlesheets4)

source(here::here("src", "update-versioned-archive.R"))

# This file imports and manages "location override" tables, which assist with geocoding.
#
# Each table specifies locations for places referred to in the dataset.
# This may be done by referring to an OpenStreetMap node (geographic point) or a lon-lat pair.
#
# Like other import files, this file should be run infrequently, when there are changes to the
# original copies of the tables stored in the Google Sheet.
# By using update_versioned_archive(), it stores past versions in the data/ folder.

filename <- "Bolivia Deaths Database"
database_file <- drive_find(filename, n_max=1) # drive_find returns a dataframe of matching files
# By setting n_max=1, we limit to one file
# The files are sorted newest first, so this gives the newest version in the drive

# this inactive code looks at metadata for the file
gs4_get(database_file)
# Import the community location override table
community_location_override_table <- range_read(ss=database_file, col_names = TRUE,
                        range = "CommunityLocationOverrideTable")

clot.filename <- "data/geocode/community-location-override.rds"
update_versioned_archive(community_location_override_table, clot.filename)

# community_location_override_table <- read_rds(here::here("data", "geocode", "community-location-override.rds"))

address_location_override_table <- range_read(ss=database_file, col_names = TRUE,
                                                range = "AddressLocationOverrideTable")

alot.filename <- "data/geocode/address-location-override.rds"
update_versioned_archive(address_location_override_table, alot.filename)

# use this code to read the file where needed:
# address_location_override_table <- read_rds(here::here("data", "geocode", "address-location-override.rds"))
