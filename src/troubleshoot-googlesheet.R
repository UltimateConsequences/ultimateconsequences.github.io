# Database Import Troubleshooting Script
# 
# This is minimum code to reproduce the error line-by-line

# Necessary(?) libraries
library(googlesheets4)
library(googledrive)
library(tidyr)
library(readr)
library(dplyr)
library(stringr)

# FYI, this is the code that is drawing the error, but we aren't going to run it.
#
# source("src/import-deaths-database-spec1.R")
# de <- import_deaths_database()

# Search for the file in Google Drive

filename <- "Bolivia Deaths Database"

database_file <- drive_find(filename, n_max=1)

# database_file2 <- drive_get("1qm8cs-R2N5gB2G4O3tlutVdwy36ZwrhS68NUqIEA4MA")

# Import the single corner cell that contains the "specification" 
# (a code telling us the version of the database we're using)
#
sheet_name="Entries"

specification <- range_read(ss=database_file, 
                            sheet = sheet_name, 
                            range = "A1:A1",
                            col_names = c("spec")) %>% pull(1)

