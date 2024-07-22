## 01-import.R
#
#  This is the default importer for RMarkdown scripts in this project
#  
#  If live.import is TRUE, it imports the Deaths and Presidents Table from Google Sheets
#  If live.import is FALSE, it loads these according to de.filename and pt.filename
#
#  If live.importe.es is TRUE, it imports the Event Status Table from Google Sheets
#  If live.import.es is FALSE, it loads this according to es.filename
library(here)
library(googledrive)
library(googlesheets4)

source(here::here("src","import-deaths-database-spec1.R"))
source(here::here("src", "update-versioned-archive.R"))

if (live.import){
  # Import the Entries
  de <- import_deaths_database(incl_inactive=FALSE, incl_narrative=FALSE,
                               parse_parenthical=TRUE)
  
  #
  # Save current version of the file to data/
  de.filename.dated <- update_versioned_archive(de, de.filename)
  
  # Import the presidents table & save current version of the file
  pt <- import_presidents_table()
  pt.filename.dated <- update_versioned_archive(pt, pt.filename)
  
}else
{
  de.path <- dirname(de.filename)
  de.filename_base <- basename(de.filename)
  de.filename_full <- here(de.path, de.filename_base)
  
  pt.path <- dirname(pt.filename)
  pt.filename_base <- basename(pt.filename)
  pt.filename_full <- here(pt.path, pt.filename_base)
  
  de <- read_rds(de.filename_full) 
  pt <- read_rds(pt.filename_full)
  
  if(use.current.archive){ # if the filenames are undated, 
    # use the most recent archived file as
    # the file name in question
    # 
    # (potential future upgrade: check that the 
    # undated file contains the same data as this file)
    filelist <-file.info(list.files(path=here(de.path), pattern="deaths-entries-2.*"), # The "2" is necessary to distinguish from "-temp"

                         extra_cols = FALSE)
    filelist <- filelist[with(filelist, order(as.POSIXct(mtime))), ]
    files <- rownames(filelist)
    de.filename.dated <- paste(de.path, tail(files, 1), sep="/")
    
    filelist <-file.info(list.files(path=here(pt.path), pattern="presidents-table-.*"), 
                         extra_cols = FALSE)
    filelist <- filelist[with(filelist, order(as.POSIXct(mtime))), ]
    files <- rownames(filelist)
    pt.filename.dated <- paste(pt.path, tail(files, 1), sep="/")
  }else
  {
    de.filename.dated <- de.filename
    pt.filename.dated <- pt.filename
  }
}

if (live.import.es){
  # Import the Event Status table
  event.status <- import_events_table(include_totals = FALSE)
  #
  # Save current version of the file to data/
  #
  es.filename.dated <- update_versioned_archive(event.status, es.filename)
}else
{

  es.path <- dirname(es.filename)
  es.filename_base <- basename(es.filename)
  es.filename_full <- here(es.path, es.filename_base)
  
  event.status <- read_rds(es.filename_full) 
  
  if(use.current.es.archive){ # if the filenames are undated, 
    # use the most recent archived file as
    # the file name in question
    # 
    # (potential future upgrade: check that the 
    # undated file contains the same data as this file)
    filelist <-file.info(list.files(path=here(es.path), pattern="event-status-.*"), extra_cols = FALSE)
    filelist <- filelist[with(filelist, order(as.POSIXct(mtime))), ]
    files <- rownames(filelist)
    es.filename.dated <- paste(es.path, tail(files, 1), sep="/")
  }else
  {
    es.filename.dated <- es.filename
  }
}

datafiles_footer <- function(de=TRUE, pt=FALSE, es=FALSE){
  datafile_text <- ""
  if(de){
    datafile_text <- paste(datafile_text, "**Entries:**", de.filename.dated, sep=" ")
  }
  
  if(pt){
    datafile_text <- paste(datafile_text, "**Presidents:**", pt.filename.dated, sep=" ")
  }
  
  if(es){
    datafile_text <- paste(datafile_text, "**Event Status:**", es.filename.dated, sep=" ")
  }
  
  str_trim(datafile_text)
}