here_filename <- function(filename="directory/name.ext"){
  path <- dirname(filename)
  filename_base <- basename(filename)
  filename_full <- here::here(path, filename_base)
  
  filename_full
}
