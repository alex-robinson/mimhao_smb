## Script to load the data from .nc with RNetCDF library
library(RNetCDF)

# Define the input data folder 
data_fldr = "data/ANT-40KM"

# Determine filenames of all data files
files = list.files(data_fldr)

# Extract dataset names from filenames
dat_names = files
dat_names = gsub("ANT-40KM_","",dat_names)
dat_names = gsub(".nc","",dat_names)
dat_names = gsub("-","_",dat_names)

# Load the datasets and store them with the appropriate names
for (i in 1:length(dat_names)) {
    nc = open.nc(file.path(data_fldr,files[i]))
    assign(dat_names[i], read.nc(nc))
    cat("Loaded dataset: ",dat_names[i],"\n")
    close.nc(nc)
}

# Make some additional global helper variables
# common to all datasets 
Xc    = BASINS_nasa$xc
Yc    = BASINS_nasa$yc
area  = BASINS_nasa$area
lon2D = BASINS_nasa$lon2D
lat2D = BASINS_nasa$lat2D
cat("Global dimensions: Xc, Yc, area, lon2D, lat2D","\n")

data_loaded = TRUE 
