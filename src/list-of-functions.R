# install.packages("NCmisc")

knitr::purl("LARR-Data-Pages.Rmd")

library("NCmisc")
list.functions.in.file("LARR-Data-Pages.R")
