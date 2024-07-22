library(reactable)
library(reactablefmtr)

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

  tbl <- reactable(event.responsibility,
                   filterable=!static,
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


