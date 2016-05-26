## Function to extract the region's mask of land and ice -----------------------

region.mask <- function(data=nasa_basin, mask=nasa_basin_mask, land, ice, 
                        Area=area, r=NULL)
{
    # Arguments #
    # 
    # data : data where is the mask of diferent regions
    # mask : mask of border from BASIN_nasa
    # ice : mask of ice shelf 
    # land : mask of ice sheet
    # Area : grid with area data
    # r : number of region. Default NULL that give all regions
    # 
    # Return #
    # 
    # Return the list mask_list that contain the mask of ice, mask of land, area (km^2) of ice,
    # and the mask of region from mask
    
    if (is.null(r)) {
        r <- seq(length.out=range(data)[2])
    }    
    
    
    mask_list <- list()
    list_temp <- list()
    for (i in r) {
        data_mask <- array(0,dim(data))
        data_mask[which(data==i)] <- 1 #we give the value 1 to the data we'll use in the current iteration
        nm <- paste("region",i,sep="")
        #data_mask2 <- data_mask * mask
        mask_all <- data_mask * ice
        data_mask <- data_mask * land
        area <- sum(mask_all*Area) / 1e6 #units are km²  
        #         ice_old <- (mask - mask * data_mask2)*ice
        #         are_old <- sum(ice_old*Area) / 1e6
        #         mask_list[[nm]] <- list(land=data_mask, ice=mask_all, area=area, 
        #                                 mask_old=data_mask2, ice_old=ice_old, area_old=are_old)
        mask_list[[nm]] <- list(land=data_mask, ice=mask_all, area=area)#, mask_old =data_mask2)        
    }    
    return(mask_list) #region.mask returns 3 matrix in each region: land, ice, area
}


## Function to extract the mask of Antartic, icefront, ...======================
region_any <- function(mask, vi=1, vo=0)
{
    # vi : value of ice shelf to compare 
    # vo : value of ocean to compare 
    mask_any <- array(0,dim(mask))
    for (i in 2:dim(mask)[1]){
        for (j in 2:dim(mask)[2]){
            if (mask[i,j] == vi){
                if (mask[i+1,j]==vo | mask[i-1,j]==vo | 
                        mask[i,j+1]==vo | mask[i,j-1]==vo){
                    mask_any[i,j] <- 1
                }
            }
        }
    }
    return(mask_any)
}


## Function to extract the vectors 'u' and 'v' #################################
regions_uv <- function(mask) #the value of the sea is 0 in mask_ice_land and mask_ice_ice
{
    ## Return the mask of velocity u and v with the sign of dS
    mask_u <- array(0,dim(mask))
    mask_v <- array(0,dim(mask))
    for (i in 2:(dim(mask)[1]-1)){
        for (j in 2:(dim(mask)[2]-1)){
          if(mask[i,j]==1)
          {
            if(mask[i,j+1]==0)
            {
              mask_v[i,j] <- mask[i,j+1]-mask[i,j] # -1 (Sea in the region j+1: up)
            }
            else if(mask[i,j-1]==0)
            {
              mask_v[i,j] <- mask[i,j]-mask[i,j-1] # +1 (Sea in the region j-1: down)
            }
            else if(mask[i+1,j]==0)
            {
              mask_u[i,j] <- mask[i+1,j]-mask[i,j] # -1 (Sea in the region i+1: right)
            }
            else if(mask[i-1,j]==0)
            {
              mask_u[i,j] <- mask[i,j]-mask[i-1,j] # +1 (Sea in the region i-1: left)
            }
          }
        }
    }
    mask_uv <- list(u=mask_u, v=mask_v)
    return(mask_uv)
}
