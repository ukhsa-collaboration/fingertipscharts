# pre-compute the vignette
# https://ropensci.org/technotes/2019/12/08/precompute-vignettes/
knitr::knit("vignettes/quick_charts.Rmd.orig", output = "vignettes/quick_charts.Rmd")
images_to_move <- list.files(pattern = ".png$",
                             full.names = TRUE)

# Must manually move image files from fingertipscharts/ to fingertipscharts/vignettes/ after knit
for (i in images_to_move) file.rename(from = i, to = gsub("\\./", "./vignettes/", i))
