## Script for load the data from .nc with ncdf
## Function get.nc made by Alex Robinson
source("functions_ncdf.r")

#dir_data<-"ice_data/Antarctica/ANT-40KM_clim"

patch <- c("ice_data/Antarctica/ANT-40KM_clim","mapping")
files <- list.files(patch, full.names = TRUE)
## files <- list.files("ice_data/Antarctica/ANT-40KM_clim", full.names = TRUE)
whatisdir <- file.info(files)$isdir
files <- files[!whatisdir]
files <- files[grep("nc$",files)]

## files_names <- list.files("ice_data/Antarctica/ANT-40KM_clim")
## files_names <- files_names[!whatisdir]
## # install.packages("stringr")
## require(stringr)
## delstr <- c("ANT-40KM_", ".nc")
## for (i in 1:length(delstr)){
##     files_names <- str_replace_all(files_names, delstr[i], "")
##     files_names <- str_replace_all(files_names, "-", "_")
## }
files_names <- files
delstr <- c(patch, "ANT_40KM_", ".nc","/")
require(stringr)
for (i in 1:length(delstr)){    
    files_names <- str_replace_all(files_names, delstr[i], "")
    files_names <- str_replace_all(files_names, "-", "_")
}

for (i in 1:length(files_names)) {
    assign(files_names[i], get.nc(files[i]))
}
rm(list = c("i","whatisdir","delstr","files_names","files","patch","get.nc"))
