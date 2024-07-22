entries_to_sf <- function(dataframe, jitter=0.001, add_label=TRUE){
  data_as_sf <- dataframe %>%
    filter(!is.na(lat) & !is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_jitter(factor = jitter)  # shakes out overlapping points
  if(add_label){
    data_as_sf <- data_as_sf %>% 
      mutate(label = str_c(event_title, name, sep=":\n"))
  }
  return(data_as_sf)
}

date_stratified_mapview <- function(dataset_sf) {
  mapview(dataset_sf,
          label = "label",
          zcol = "year",
          at = c(1982, 1985, 1999, 2005, 2019, 2024),
          labFormat = leaflet::labelFormat(big.mark = ""), # no commas in years
          popup = popupTable(dataset_sf,
                             zcol = c("event_title", "year", "name", "id_indiv", "location")))
}