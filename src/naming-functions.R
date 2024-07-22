source(here::here("src","render-age.R"))
library(stringr)
library(dplyr)

add_nameline <- function(dataframe){
  dataframe <- dataframe %>% mutate(name_line = in_case(
    is.na(dec_age) ~ paste("<b>Name:</b> ", dec_firstname, " ", dec_surnames, "\n", sep=""),
    TRUE ~ paste("<b>Name:</b> ", dec_firstname, " ", dec_surnames, " (", 
                 render_age(dec_age), ")\n", sep="")))
  return(dataframe)
}

add_dateline <- function(dataframe){
  dataframe <- dataframe %>% mutate(month_name = month.name[month],
                                    date_text = case_when(
                                      (is.na(day) &
                                         is.na(month) & !is.na(year)) ~ str_glue("in {year}"),
                                      (is.na(day) &
                                         !is.na(month) &
                                         !is.na(year)) ~ str_glue("in {month.name[month]} {year}"),
                                      (is.na(year)) ~  "on unknown date",
                                      # Normal date are just pasted
                                      TRUE ~ (str_glue("on {day} {month_name} {year}"))
                                    ),
                                    incident_line = str_c("Fatal incident ", date_text)
  )
  return(dataframe) 
}
