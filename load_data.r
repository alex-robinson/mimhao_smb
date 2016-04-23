## Script to load the data from .nc with RNetCDF library
library(RNetCDF)
library(stringr)

data_fldr = "data/ANT-40KM"

# Determine filenames of all data files
files = list.files(data_fldr)


# Extract dataset names from filenames
delstr    = c("ANT-40KM_", ".nc")
dat_names = files
for (i in 1:length(delstr)){
    dat_names = str_replace_all(dat_names, delstr[i], "")
    dat_names = str_replace_all(dat_names, "-", "_")
}
for (i in 1:length(dat_names)) {
    nc = open.nc(file.path(data_fldr,files[i]))
    assign(dat_names[i], read.nc(nc))
    close.nc(nc)
}
