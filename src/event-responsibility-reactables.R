library(reactable)
library(reactablefmtr)
library(lubridate)

## Color scales
# 
# future note: this likely won't work well in a package
red_pal <- function(x) rgb(colorRamp(c("#ffe0e0", "#ff3030", "#bb2020"))(x), maxColorValue = 255)
if (exists("state_resp.colors")){
  perp_pal <- function(x) rgb(colorRamp(c("#eeeeee", state_resp.colors[["Perpetrator"]]))(x), maxColorValue = 255)
  sv_pal <- function(x) rgb(colorRamp(c("#eeeeee", state_resp.colors[["Victim"]]))(x), maxColorValue = 255)
  sep_pal  <- function(x) rgb(colorRamp(c("#eeeeee", state_resp.colors[["Separate"]]))(x), maxColorValue = 255)
} else {
  perp_pal <- red_pal
  sv_pal <- red_pal
  sep_pal <- red_pal
}

# deaths columns formatting
deaths_column <- function(maxWidth = 60, class = NULL, maxValue = 100, chosen_palette = red_pal, ...) {
  colDef(
    maxWidth = maxWidth,
    defaultSortOrder = "desc",
    style = function(value) {
      # Lighter color for <1%
      if (value / maxValue < 0.01) {
        list(color = "#888", background="#fff")
      } else {
        list(color = case_when(sqrt(value/ maxValue) < .48 ~ "#111",
                               TRUE ~ "#eee"), 
             background = chosen_palette((value/ maxValue)^0.8),
             fontWeight = "bold")
      }
    },
    ...
  )
}

# text columns formatting
text_column <- function(static=FALSE, ...) {
  colDef(
    filterable=!static,
    defaultSortOrder = "asc",
    ...
  )
}
#
#
# column list
#
# Too many variables, so this can't be passed as a function, but can be sourced!
#
# er_column_list <- list (date = colDef(name="Year", maxWidth=60,
#                 cell = function(value) strftime(value, "%Y"),
#                 defaultSortOrder = "asc",
#                 filterable=TRUE, sortNALast = TRUE,
#                 style = list(background = "#ffffff")),
#   year = colDef (show = FALSE), 
#   event_title = text_column(static=static, name="Event", maxWidth=250,
#                         style = list(fontWeight = "bold")),
#   n = deaths_column(maxValue = max_deaths, name="Confirmed"),
#   n_state_perp = deaths_column(maxValue = max_deaths, name="State Perp",
#                                chosen_palette = perp_pal),
#   n_state_victim = deaths_column(maxValue = max(max_deaths/2, max_sv_deaths),  
#                                  # sets the maximum intensity 
#                                  name="State Victim",
#                                  chosen_palette = sv_pal),
#   n_state_separate = deaths_column(maxValue = max(max_deaths/2, max_sep_deaths),  
#                                    # sets the maximum intensity 
#                                    name="Sep from State",
#                                    chosen_palette = sep_pal),
#   protest_domain = text_column(static=static, name="Protest Domain",
#                            maxWidth=150),
#   pres_admin = text_column(static=static, name="President",
#                            maxWidth=300)
# )


# event responsibility table
# 
# event | date (as year) | 4 numerical columns by responsiblity | domain | presidency
standard_event_responsibility_reactable <- function(event.responsibility, static=FALSE, max_larger=0,
                                                short_table=FALSE) {
  
  # set limits for shading gradient
  max_deaths <- max(select(event.responsibility, n:n_state_victim),  na.rm=TRUE)
  max_sv_deaths <- max(select(event.responsibility, n_state_victim), na.rm=TRUE)
  max_sep_deaths <- max(select(event.responsibility, n_state_separate), na.rm=TRUE)
  max_deaths <- max(max_deaths, max_larger) # pass through a maximum value as specified in the function call
  
  # default group of numerical columns
  if(!exists("er.numerical.columns")){
    er.numerical.columns <- c("n","n_state_perp","n_state_victim", 
                              "n_state_separate")
  }
  
  if (short_table){
    defaultPageSize = 8
    pageSizeOptions = c(4, 8, 12, 15, 20)  
  } else {
    defaultPageSize = 20
    pageSizeOptions =  c(10, 15, 20, 25, 40, 50, 75)   
  }
  
  source(here::here("src","er-column-list.R"), local = TRUE)

  if (static){
    tbl <- reactable(event.responsibility,
                     filterable=FALSE,
                     theme = nytimes(),
                     defaultPageSize = defaultPageSize,
                     pageSizeOptions = pageSizeOptions,
                     showPageSizeOptions=TRUE,
                     defaultColDef = colDef(
                       filterable=FALSE,
                       defaultSortOrder = "desc",
                       minWidth = 30, maxWidth=50),
                     columnGroups = list(colGroup(name = "deaths", columns =
                                                    er.numerical.columns)
                     ),
                     columns = er_column_list_static 
    )
  }else{
    tbl <- reactable(event.responsibility,
                     filterable=TRUE,
                     theme = nytimes(),
                     defaultPageSize = defaultPageSize,
                     pageSizeOptions = pageSizeOptions,
                     showPageSizeOptions=TRUE,
                     defaultColDef = colDef(
                       filterable=FALSE,
                       defaultSortOrder = "desc",
                       minWidth = 30, maxWidth=50),
                     columnGroups = list(colGroup(name = "deaths", columns =
                                                    er.numerical.columns)
                     ),
                     columns = er_column_list_dynamic 
    )
  }
  
  tbl
}

single_color_event_responsibility_table <- function(event.responsibility, static=FALSE, max_larger=0) {
  
  # set limits for shading gradient
  max_deaths <- max(select(event.responsibility, n:n_state_victim),  na.rm=TRUE)
  max_sv_deaths <- max(select(event.responsibility, n_state_victim), na.rm=TRUE)
  max_sep_deaths <- max(select(event.responsibility, n_state_separate), na.rm=TRUE)
  max_deaths <- max(max_deaths, max_larger) # pass through a maximum value as specified in the function call
  
  # default group of numerical columns
  if(!exists("er.numerical.columns")){
    er.numerical.columns <- c("n","n_state_perp","n_state_victim", 
                              "n_state_separate")
  }
  
  tbl <- reactable(event.responsibility,
                   filterable=!static,
                   theme = nytimes(),
                   defaultPageSize=20,
                   pageSizeOptions = c(10, 15, 20, 25, 40, 50, 75),
                   showPageSizeOptions=TRUE,
                   defaultColDef = colDef(
                     filterable=FALSE,
                     defaultSortOrder = "desc",
                     minWidth = 30, maxWidth=50),
                   columnGroups = list(colGroup(name = "deaths", columns =
                                                  er.numerical.columns)
                   ),
                   columns = list (
                     date = colDef(name="Year", maxWidth=60,
                                   cell = function(value) strftime(value, "%Y"),
                                   defaultSortOrder = "asc",
                                   filterable=!static, sortNALast = TRUE,
                                   style = list(background = "#ffffff")),
                     year = colDef (show = FALSE), 
                     event_title = text_column(name="Event", maxWidth=250,
                                               style = list(fontWeight = "bold")),
                     n = deaths_column(maxValue = max_deaths, name="Confirmed"),
                     n_state_perp = deaths_column(maxValue = max_deaths, name="State Perp",
                                                  chosen_palette = perp_pal),
                     n_state_victim = deaths_column(maxValue = max(max_deaths/2, max_sv_deaths),  
                                                    # sets the maximum intensity 
                                                    name="State Victim",
                                                    chosen_palette = sv_pal),
                     n_state_separate = deaths_column(maxValue = max(max_deaths/2, max_sep_deaths),  
                                                      # sets the maximum intensity 
                                                      name="Sep from State",
                                                      chosen_palette = sep_pal),
                     protest_domain = text_column(name="Protest Domain",
                                                  maxWidth=150),
                     pres_admin = text_column(name="President",
                                              maxWidth=300)
                   )
  )
  tbl
}

### Individuals Table
#
# (for nesting inside the main table)

individual_directory_reactable <- function(dataframe, static = FALSE,
                                             striped = TRUE, show_event_title = FALSE, ...){
  reactable(data       = dataframe,
            compact    = TRUE, 
            filterable = !static,
            bordered   = TRUE, 
            striped    = striped, # banded rows
            resizable  = FALSE,
            defaultPageSize=20,
            pageSizeOptions = c(12, 20, 30, 40, 100),
            columns    = list (
              event_title = colDef(show = show_event_title, name="Event"),
              date = colDef(cell = function(value) strftime(value, "%d %b %Y"),
                            defaultSortOrder = "asc",name="Event Date",
                            maxWidth = 100),
              dec_firstname = colDef (name="First Name"),
              dec_surnames = colDef (name="Surnames"),
              age = colDef (name="Age", maxWidth=40),
              dec_affiliation = colDef (name="Affiliation"),
              intentionality = colDef (name="Intentionality"),
              cause_death = colDef (name="Cause of Death"),
              state_responsibility = colDef(name = "State Responsibility",
                                            defaultSortOrder = "asc",
                                            style = function(value, index) {
                                              if(is.na(value)){
                                                list(color = "black")
                                                # } else if (value == "Separate"){
                                                #   list(color = state_resp.colors[["Separate"]])
                                                # } else if (value == "Perpetrator"){
                                                #   list(color = state_resp.colors[["Perpetrator"]])
                                                # } else if (value == "Victim"){
                                                #   list(color = state_resp.colors[["Victim"]])
                                              }else if(value %in%
                                                       c("Separate", "Perpetrator",
                                                         "Victim")){
                                                list(color = state_resp.colors[[value]])
                                              }else {
                                                list(color = "black")
                                              }
                                            }),
              perp_affiliation = colDef(name = "Perp Affiliation",
                                        defaultSortOrder = "asc"
              )
            ),
            ...
  )
}

## Two level event-responsibility reactable with directory inside
#
#
nested_event_responsibility_reactable <- function(event.responsibility, dataframe,
                                                  static=FALSE, max_larger=0,
                                                  short_table=FALSE) {
  top_level <- event.responsibility 
  # set limits for shading gradient
  max_deaths <- max(select(event.responsibility, n:n_state_victim),  na.rm=TRUE)
  max_sv_deaths <- max(select(event.responsibility, n_state_victim), na.rm=TRUE)
  max_sep_deaths <- max(select(event.responsibility, n_state_separate), na.rm=TRUE)
  max_deaths <- max(max_deaths, max_larger) # pass through a maximum value as specified in the function call

  second_level <- dataframe %>% combine_dates() %>% 
    select(event_title, date, dec_firstname, dec_surnames, age,
           dec_affiliation, cause_death, intentionality, 
           state_responsibility, perp_affiliation)
 
  source(here::here("src","er-column-list.R"), local = TRUE)
  
  reactable(
    data       = top_level,
    compact    = TRUE, # for minimum row height
    filterable = TRUE, # for individual column filters
    striped    = FALSE, # banded rows
    resizable  = FALSE, # for resizable column widths
    theme = nytimes(),
    defaultPageSize=30,
    pageSizeOptions = c(12, 20, 30, 40, 100),
    showPageSizeOptions=TRUE,
    defaultColDef = colDef(
      filterable=FALSE,
      defaultSortOrder = "desc",
      minWidth = 30, maxWidth=50),
    columnGroups = list(colGroup(name = "deaths", columns =
                                   er.numerical.columns)),
    columns = er_column_list_dynamic,
    details = function(index) { # index is the row number of current row.
      # sub-table of only those students for current row.
      sec_lvl = second_level[second_level$event_title == top_level$event_title[index], ] 
      individual_directory_reactable(sec_lvl, show_event_title = FALSE)
    }
  )
}
