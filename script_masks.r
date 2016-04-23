# Load data and get masks and region masks =====================================

source('load_data.r')
Xc = BASINS_nasa$xc
Yc = BASINS_nasa$yc
area = BASINS_nasa$area

## Regions of NASA extended
nasa_basin = BASINS_nasa$basin
## Antartic' Mask from NASA (ice+land)
nasa_basin_mask = BASINS_nasa$basin_mask

## Ice and land mask from TOPO BEDMAP2
mask_ice = TOPO_BEDMAP2$mask_ice

mask_ice_land <- apply(mask_ice, c(1, 2), function(x) if (x==2) 1 else 0)
mask_ice_ice <- apply(mask_ice, c(1, 2), function(x) if (x==3) 1 else 0)
mask_ice_all <- apply(mask_ice, c(1, 2), function(x) if (x!=0) 1 else 0)

## Ice and land mask from RACMO23
mask_racmo <- RACMO23_ERA_INTERIM_monthly_1981_2010$mask
mask_racmo_land <- RACMO23_ERA_INTERIM_monthly_1981_2010$mask_grounded
mask_racmo_ice <- mask_racmo - mask_racmo_land


source('function_regions.r')
region_mask=region.mask(land=mask_ice_land, ice=mask_ice_ice)
region_mask2=region.mask(land=mask_racmo_land, ice=mask_racmo_ice)
