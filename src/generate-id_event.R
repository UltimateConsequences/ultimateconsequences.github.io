event.yearcount <- event.status %>% count(year)
event.years <- left_join(select(event.status, event_title, year), event.yearcount )
event.years <- event.years %>% mutate(year_id=year-1980)
event.years$year_id <- replace_na(event.years$year_id, 1)

#
event.years <- event.years %>% 
  group_by(year) %>%
  mutate(event_num = row_number()) %>% 
  ungroup()

# Produces an event ID with an initial "e",
# followed by a two-digit year code (year minus 1980)
# followed by a two-digit event count
event.years <- event.years %>%
  mutate(id_event = str_c("e", 
                          sprintf("%02d", year_id), 
                          sprintf("%02d", event_num), 
                          sep = ""))
event.id <- select(event.years, event_title, year, id_event)

# Notes:

# The code above doesn't currently save the event id directly to the variable.
# instead I copied and added it by hand.
#

# Adding id_event to event.status (through direct assignment)
# in old files

event.status <- read_rds(here_filename(es.filename)) 

if ("id_event" %in% names(event.status)){
  event.status.key <- select(event.status, event_title, year, id_event) # current event status
}

date_string <- "2022-04-05"
es.filename.dated <- str_c("data/event-status-", date_string, ".rds")
event.status.temp <- read_rds(here_filename(es.filename.dated)) 

if (!("id_event" %in% names(event.status.temp))){
  event.status.processed <- left_join(event.status.temp, event.status.key, by=join_by(event_title, year)) %>% 
    relocate(event_title, year, id_event)
  filter(event.status.processed, is.na(year))
  
  es.unmatched.fix <- event_title.update.table.combined %>% select(event_title_old, year, id_event) %>%
                      rename(event_title = event_title_old)
  
# first manually apply the prior fix here!
  event.status.processed <- rows_update(event.status.processed, select(es.unmatched.fix, event_title, id_event), 
                                        unmatched = "ignore") 
   
  event.status.processed <- event.status.processed %>%                         
                mutate(event_title=str_replace(event_title, "Asusta", "Asunta"))

  es.unmatched <- filter(event.status.processed, is.na(id_event))
  es.unmatched
} 
## Manual fix
unmatched.filename <- str_c("data/es.unmatched-" , date_string, ".csv")
write_csv(select(es.unmatched, event_title, year, id_event), unmatched.filename)
#
# do the manual fix in text editor now
#
es.unmatched.fix <- read_csv(unmatched.filename)

event.status.processed <- rows_update(event.status.processed, select(es.unmatched.fix, event_title, id_event), 
                              unmatched = "ignore")
event.status.processed[event.status.processed$id_event=="e2903",2] <- 2009 # fixed TIPNIS date
event.status.processed[event.status.processed$id_event=="e2005",2] <- 2000

#e Verify
event.status.processed %>% filter(year==2009) # change year to a year in es.unmatched
event.status.processed %>% filter(is.na(id_event)) # should have zero rows

event_title.update.table <- left_join(event.status.key, 
                                      rename(es.unmatched.fix, event_title_old = event_title)) %>% 
                            filter(!is.na(event_title_old)) %>%
                            filter(event_title != event_title_old)
if(nrow(event_title.update.table)>0){
  event_title.update.table.combined <- bind_rows(event_title.update.table.combined, 
                                                 event_title.update.table)
  event_title.update.table.ru <- event_title.update.table %>%  
    filter(!is.na(event_title_old)) %>% 
    select(id_event, event_title)
  
  event.status.processed <- rows_update(event.status.processed, event_title.update.table.ru, 
                                        by = c("id_event"))
}

#e Verify
event.status.processed %>% filter(year==2018) 

es.filename.updated <- str_replace(es.filename.dated, ".rds", "u.rds")
write_rds(event.status.processed, es.filename.updated)

write_rds(event_title.update.table.combined, "data/event-title-update-table.rds")

# for edits
# install.packages("DataEditR")
library(DataEditR)
data_edit(es.unmatched)

# Adding id_event to event.status (through direct assignment)
#.  This is the procedure we would do automaticallly.
#   But we SHOULD NOT do this to old files, since we want 
#.  the same ID to match to the same event over time, even as 
#.  other things shift.
#
# event.status.id <- left_join(event.status, select(event.id, -year)) %>% 
#  relocate(event_title, year, id_event)
#
# Verifying that this worked…
#
# event.status.updated <- import_events_table()
# all.equal(select(event.status.updated, event_title, year, id_event), 
#           select(event.status.id, event_title, year, id_event))
#
# Returns TRUE, so success!
#
