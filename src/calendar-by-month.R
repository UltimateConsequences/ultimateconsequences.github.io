
# First create a year/month table

monthly_calendar <- function(dataframe, 
                             initial_month = 10, initial_year=1982,
                             final_month=0, final_year=0) {
  data.valid_dates <- dataframe %>%
    filter(!is.na(year) & !is.na(month))
  
  dataframe <- data.valid_dates
  
  # calculate the last month and year in the dataset
  max_year <- max(dataframe$year)
  data.final_year <- filter(dataframe, year==max_year)
  max_month <- max(data.final_year$month)
  
  # if no final month given in the function call, take the last month 
  # in the dataset as the upper limit for the calendar
  if (final_month==0) {
    final_month <- max_month
  }
  if (final_year==0) {
    final_year <- max_year
  }
  
  calendar_tibble <- dataframe %>%
    group_by(year, month) %>%
    summarize(N = n()) %>%
    ungroup() %>%
    complete(year = 1982:final_year, month = 1:12, 
             fill = list(N = 0)) %>%
    spread(month, N)
  
  # clear months before study period
  final_prestudy_month <- initial_month - 1
  final_prestudy_month_column <- final_prestudy_month + 1
  initial_year <- 1982
  calendar_tibble[calendar_tibble$year==initial_year, (1+1):(initial_month)] <- NA
  
  # clear months after the last one
  if (final_month<12) {
    final_month_column <- final_month+1
    calendar_tibble[calendar_tibble$year==final_year, (final_month_column+1):13] <- NA 
  }

  names(calendar_tibble) <- c("Year", month.abb) # Add 3-char month names (in English)
  
  calendar_tibble
}

monthly_calendar_plus_totals <- function (dataframe, 
                              initial_month = 10, initial_year=1982,
                              final_month=0, final_year=0) {
  calendar_tibble <- monthly_calendar(dataframe, initial_month, initial_year,
                                      final_month, final_year)
  
  calendar_tibble <- calendar_tibble %>% mutate(total = rowSums(.[2:13], na.rm=TRUE))
  
  names(calendar_tibble) <- c("Year", month.abb, "Total") # Add 3-char month names (in English)
  
  calendar_tibble
}

shaded_monthly_calendar <- function (calendar_tibble) {
  calendar_tibble %>%
    reactable(
      theme = nytimes(),
      defaultPageSize=25,
      pageSizeOptions = c(10, 25, 40),
      showPageSizeOptions=TRUE,
      defaultColDef = colDef(
        filterable=FALSE,
        defaultSortOrder = "desc",
        style = color_scales(calendar_tibble, span=2:13,
                             colors = c("#ffffff", "#ff3030")),
        minWidth = 40, maxWidth=60),
    )
}
