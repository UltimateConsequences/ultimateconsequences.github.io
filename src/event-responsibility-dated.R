# create an event count by title that also counts the number of state perpetrator 
#   and state victim deaths
#
library(lubridate)
library(stringr)
library(dplyr)
library(incase)

estimated_date <- function(year, month, day){
  if (is.na(year)) {
    return(NA)
    }
  if (is.na(day) &
      is.na(month) & !is.na(year)) {
    date_string <- str_glue("{year}-06-30")
  } else if (is.na(day) & !is.na(month) & !is.na(year)) {
    date_string <- str_glue("{year}-{month}-15")
  } else {
    date_string <- paste(year, month, day, sep = "-")
  }
  date <- date_string %>% ymd() %>% as.Date()
  return(date)
}

estimated_date_string <- function(year, month, day){
  date_string <- in_case(
    (is.na(year)) ~ NA,
    (is.na(day) & is.na(month) & !is.na(year)) ~ str_glue("{year}-06-30"),
    (is.na(day) & !is.na(month) & !is.na(year)) ~ str_glue("{year}-{month}-15"),
    TRUE ~ paste(year, month, day, sep = "-")
  ) 
  
  return(date_string)
}

event_responsibilty_summary_table <- function(dataframe) {
  de <- dataframe
  
  event.responsibility.dateless <- de %>% group_by(event_title) %>%
    dplyr::summarize(
      n = n(),
      n_state_perp = sum(state_responsibility=="Perpetrator", na.rm = TRUE),
      n_state_victim = sum(state_responsibility=="Victim", na.rm = TRUE),
      n_state_separate = sum(str_detect(state_responsibility, "Separate"), na.rm = TRUE),
      .groups='drop'
    )
  
  de.first_per_event <- de %>% distinct(event_title, .keep_all=TRUE)
  
  de.first_per_event <- de.first_per_event %>% 
                        mutate(date_text = estimated_date_string(year, month, day)) %>%
                        mutate(date = as.Date(ymd(date_text)) )
  
  # This test code will show the corrected dates.
  # de.first_per_event %>% filter(is.na(day)) %>% select(event_title, date, year, month, day)
  
  # merge the event table with the corresponding year(s)
  event.responsibility.dated <- event.responsibility.dateless %>% 
    left_join(unique(select(de.first_per_event, event_title, date, year, protest_domain, pres_admin))) %>%
    select(event_title, date, year, everything()) %>% 
    arrange(date)
  
  event.responsibility <- event.responsibility.dated # use this result as the event responsibility table
  
  return(event.responsibility)
}

er.numerical.columns <- c("n","n_state_perp","n_state_victim", 
                          "n_state_separate")
