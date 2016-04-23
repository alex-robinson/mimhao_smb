## Script for load the data from .nc with ncdf
## Function get.nc made by Alex Robinson
source("functions_ncdf4.r")

#dir_data<-"ice_data/Antarctica/ANT-40KM_clim"

files <- list.files("ice_data/Antarctica/ANT-40KM_clim",full.names = TRUE)
whatisdir <- file.info(files)$isdir
files <- files[!whatisdir]

files_names <- list.files("ice_data/Antarctica/ANT-40KM_clim")
files_names <- files_names[!whatisdir]

# install.packages("stringr")
library(stringr)
delstr <- c("ANT-40KM_", ".nc")
for (i in 1:length(delstr)){
    files_names <- str_replace_all(files_names, delstr[i], "")
    files_names <- str_replace_all(files_names, "-", "_")
}
for (i in 1:length(files_names)) {
    assign(files_names[i], get.nc4(files[i]))
}
rm(list = c("i","whatisdir","delstr"))
