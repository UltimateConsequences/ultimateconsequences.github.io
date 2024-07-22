## 01-import.R
#
#  This is an importer for RMarkdown scripts in this project which imports
#  de and pt, but not es
#  
#  If live.import is TRUE, it imports the Deaths and Presidents Table from Google Sheets
#  If live.import is FALSE, it loads these according to de.filename and pt.filename
#
#  It does not handle the Event Status sheet from Google Sheets
library(here)

source(here::here("src", "import-deaths-database-spec1.R"))
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

    filelist <-file.info(list.files(path=here(de.path), pattern="deaths-entries-2.*"), 
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

datafiles_footer <- function(de=TRUE, pt=FALSE, es=FALSE){
  datafile_text <- ""
  if(de){
    datafile_text <- paste(datafile_text, "**Entries:**", de.filename.dated, sep=" ")
  }
  
  if(pt){
    datafile_text <- paste(datafile_text, "**Presidents:**", pt.filename.dated, sep=" ")
  }

  str_trim(datafile_text)
}