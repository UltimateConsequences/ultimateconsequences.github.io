cp -R ../ultimate-consequences/data data

mv ultimate-consequences/WLRF-Tables.Rmd ultimate-consequences/WLRF-Tables-old.Rmd
cp ../ultimate-consequences/WLRF-Tables-color-id.Rmd ultimate-consequences/WLRF-Tables.Rmd 

quarto render

cp ../ultimate-consequences/WLRF-Tables-archive-id.Rmd ultimate-consequences/WLRF-Tables-bw.Rmd

quarto render ultimate-consequences/WLRF-Tables-bw.Rmd
