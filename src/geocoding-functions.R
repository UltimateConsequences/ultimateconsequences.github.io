mutate_geocode_bolivia <- function(dataframe, ...){
  mutate_geocode(dataframe,
                 ..., 
                 region="bo", 
                 bounds="-69.7,-22.9|-57.4,-9.6")
}
