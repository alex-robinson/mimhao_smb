
# First, make sure to load the data using:
source('load_data.r')

# Load dependencies 
source('function_regions.r')
library(gridExtra)
library(fields)

# Define the output folder 
outfldr = "output" 

# Get diferents masks ============================================

## Regions of NASA extended
nasa_basin      = BASINS_nasa$basin
## Antarctica Mask from NASA (ice+land)
nasa_basin_mask = BASINS_nasa$basin_mask

## Ice and land mask from TOPO BEDMAP2
mask_ice = TOPO_BEDMAP2$mask_ice

mask_ice_land = apply(mask_ice, c(1, 2), function(x) if (x==2) 1 else 0)
mask_ice_ice  = apply(mask_ice, c(1, 2), function(x) if (x==3) 1 else 0)
mask_ice_all  = apply(mask_ice, c(1, 2), function(x) if (x!=0) 1 else 0)

region_mask=region.mask(land=mask_ice_land, ice=mask_ice_ice)

# Basal melt rate, actual present day
bmelt = BMELT_R13$bm_actual
# Basal melt in the equilibrium
bss = BMELT_R13$bm_equil

# Units   ######################################################################
# magnitude : from ncdf to table
# Area  : m^2  ->  1e-6 km^2
# SMB   : kg*m^-2*d^-1  ->  1e-12 Gt/year
#         RACMO23 : mean; sum(*area)/1e12*365
#         RACMO2_ANT3K55_HadCM3_A1B_monthly : sum; sum(*area)/1e12
# BMelt : m*a^-1  ->  Gt/year
#         BMELT_13 : mean((bmelt*regions)[regions==1])


# Creating data frame with areas  ##############################################
index = paste("Region",seq(length.out=range(nasa_basin)[2]),sep="")
df = data.frame(row.names=index)

#df[,"Area_bedmap"] = sapply(region_mask, function(x) x$area) #1st COLUMN OF THE df TABLE

#Written in a easiest (but longer) way:
for(i in 1:length(index)){
df[i,"Area_bedmap"] = region_mask[[i]]$area #1st COLUMN OF THE df TABLE
}



# Calculations #################################################################

## ajr: test calcs 

# smb_ann_era0 = apply(RACMO23_ERA_INTERIM_monthly_1981_2010$smb,c(1,2),mean)*365*1e-3
# 
# smb_ann_had0 = apply(RACMO2_ANT3K55_HadCM3_A1B_monthly_2000_2010$smb,c(1,2),sum)*1e-3
# smb_ann_had1 = apply(RACMO2_ANT3K55_HadCM3_A1B_monthly_2001_2030$smb,c(1,2),sum)*1e-3
# smb_ann_had2 = apply(RACMO2_ANT3K55_HadCM3_A1B_monthly_2071_2100$smb,c(1,2),sum)*1e-3
# dsmb_ann_had1 = smb_ann_had1-smb_ann_had0
# dsmb_ann_had2 = smb_ann_had2-smb_ann_had0

# par(mfrow=c(1,3))
# image.plot(Xc,Yc,smb_ann_era0)
# contour(Xc,Yc,nasa_basin_mask,add=TRUE,col=1)
# contour(Xc,Yc,mask_ice_ice,add=TRUE,col=2,lwd=2)

# image.plot(Xc,Yc,dsmb_ann_had1)
# contour(Xc,Yc,nasa_basin_mask,add=TRUE,col=1)
# contour(Xc,Yc,mask_ice_ice,add=TRUE,col=2,lwd=2)

# image.plot(Xc,Yc,dsmb_ann_had2)
# contour(Xc,Yc,nasa_basin_mask,add=TRUE,col=1)
# contour(Xc,Yc,mask_ice_ice,add=TRUE,col=2,lwd=2)



##################


## SMB #
#   RACMO23_ERA_INTERIM have mean monthly
#   RACMO2_ANT3K55_HadCM3_A1B_monthly have accumulative monthly

smb_racmo.month = RACMO23_ERA_INTERIM_monthly_1981_2010$smb
smb_racmo.year = apply(smb_racmo.month,c(1,2),mean) #as RACMO23 is accumulative monthly, racmo.year is the annual mean value of smb in a "typical" (representative) month

SMB.month = list(RACMO2_ANT3K55_HadCM3_A1B_monthly_2000_2010$smb,
            RACMO2_ANT3K55_HadCM3_A1B_monthly_2001_2030$smb, 
            RACMO2_ANT3K55_HadCM3_A1B_monthly_2071_2100$smb)

SMB.year = lapply(SMB.month, function(x) apply(x,c(1,2),mean))

#SMB.year = lapply(SMB.month,sum)
#SMB.year = apply(RACMO2_ANT3K55_HadCM3_A1B_monthly_2000_2010$smb,c(1,2), sum)



for (i in 1:length(index)){
    smb_racmo_bm = sum(smb_racmo.year*region_mask[[i]]$ice*area)/1e12*365
    df[i,"SMB-1981-2010"] = smb_racmo_bm #2nd COLUMN OF THE df TABLE (real data from reanalysis) #NOT USEFUL
    smb = sapply(smb_racmo_bm, function(x) sum(x*region_mask[[i]]$ice*area)/1e12 * 365) #As it comes from SMB.month, smb has 3 matrix inside too
    #smb = sapply(SMB.year, function(x) sum(region_mask[[i]]$ice*area)/10**6) 
    #df[i,"SMB-A1B-2000-2010"] = smb[1] #3rd COLUMN OF THE df TABLE (it is the reference for the A1B scenario simulation)
    #df[i,"SMB-A1B-2000-2010"] = smb#[1] #3rd COLUMN OF THE df TABLE (it is the reference for the A1B scenario simulation)
    # df[i,"dSMB-A1B-2001-2030"] = (smb[2] - smb[1]) #4th COLUMN OF THE df TABLE #NOT USEFUL
    # df[i,"dSMB-A1B-2071-2100"] = (smb[3] - smb[1]) #5th COLUMN OF THE df TABLE #NOT USEFUL
}

pdf(file.path(outfldr,"df_smb.pdf"), height=8, width=11.5)
grid.table(round(df, digits = 2)) #THE PDF FILE CONTAINS THE df TABLE
dev.off()

## BMELT_13 #

df_2 = data.frame(row.names=index)

#Basal melt in m/year
for (i in 1:length(index)){
    df_2[i,"bmelt(m/year)"] = sum((bmelt*region_mask[[i]]$ice)) #before it was 'mean' instead of 'sum'
    df_2[i,"bss(m/year)"]   = sum((bss*region_mask[[i]]$ice))
}

#Basal melt in Gt/year
for (i in 1:length(index)){
  df_2[i,"bmelt(Gt/year)"] = sum((bmelt*region_mask[[i]]$ice*area*916.8/1e12))
  df_2[i,"bss(Gt/year)"]   = sum((bss*region_mask[[i]]$ice*area*916.8/1e12))
  # df_2[i,"bmelt(Gt/year)"] = df_2[i,"bmelt(m/year)"] * df[i,"Area_bedmap"] *916.8*1e6/1e12
  # df_2[i,"bss(Gt/year)"] = df_2[i,"bss(m/year)"] * df[i,"Area_bedmap"] *916.8*1e6/1e12
}

pdf(file.path(outfldr,"df_melt.pdf"), height=8, width=11.5)
grid.table(round(df_2, digits = 2))
dev.off()


## Plots ##################################################################
#image(Xc, Yc, nasa_basin,breaks=c(1:27), col=rainbow(26))
# color=sample(colours(), 27)
colors = c('chocolate4', 'orange', 'lightblue4', 'lightskyblue', 'blue', 
            'mediumspringgreen', 'firebrick4', 'gold', 'darkolivegreen', 
            'sandybrown', 'aquamarine4', 'darkgoldenrod', 'gray80', 'seagreen', 
            'pink3', 'khaki1', 'darkred', 'green', 'magenta', 'lightsteelblue', 
            'mediumblue', 'lightsalmon', 'aquamarine', 'yellow2', 'cadetblue1', 
            'darkorange', 'darkgreen')

## Contour from TOPO BEDMAP2
mask_plot2 = nasa_basin * mask_ice_all
mask_plot2[mask_plot2==0] = NA

## Plot of the topography of the Antarctica
pdf(file.path(outfldr,"TOPO_BEDMAP2.pdf"))
image(Xc, Yc, mask_plot2,col=NA) #first plot
for (i in 1:27){
    #image(Xc, Yc, region_mask[[i]]$ice, add=TRUE, col=c(NA,colors[i]))
    image(Xc, Yc, region_mask[[i]]$land+region_mask[[i]]$ice, add=TRUE, col=c(NA,colors[i])) #plot of each region (27)
    for (ii in seq(length=length(Xc))){
        for (ij in seq(length=length(Yc))){
            if (region_mask[[i]]$ice[ii,ij] != 0) {
                points(Xc[ii], Yc[ij], pch = ".", cex = .1) #ice shelves are points
            }
        }
    }#The following code in this loop is written to put the number of the regions in the plot
#current_basin = which(region_mask[[i]]$land+region_mask[[i]]$ice!=0, arr.ind=TRUE)
#basin_coordinates[i] = current_basin[8] #We take the first coincidence
#text(Xc[basin_coordinates[i]], Yc[basin_coordinates[i]] ,label = i)  
}

contour(Xc, Yc, mask_ice_land, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="red1") #grounding line contour
contour(Xc, Yc, mask_ice_all, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="black") #total contour
title("Antarctica (TOPO BEDMAP2)")
dev.off()


# Creation of ice front mask ###################################################

i1 = which(mask_ice_all == 1, arr.ind=T)

iceland = region_any(mask_ice_all) #iceland is the contour of any region with ice (floating or supported in the land)
jpeg(file.path(outfldr,"Antarctica.jpeg"), width = 720, height = 720, quality=100)
image(Xc, Yc, iceland)
title("Contour Antarctica")
dev.off()
jpeg(file.path(outfldr,"land.jpeg"), width = 720, height = 720, quality=100)
landcontour = region_any(mask_ice_land) #landcontour is the contour of any region supported in land (so it's the region delimited by the grounding line)
image(Xc, Yc, landcontour)
title("Contour grounding land")
dev.off()
jpeg(file.path(outfldr,"ice.jpeg"), width = 720, height = 720, quality=100)
icecontour = region_any(mask_ice_ice) #icecontour is the contour of any region with floating ice (so it's delimited by the ice selves)
image(Xc, Yc, icecontour)
title("Contour ice shelf")
dev.off()
jpeg(file.path(outfldr,"icefront.jpeg"), width = 720, height = 720, quality=100)
icefront = region_any(mask_ice, 3) #icefront is the contour of the coast where there is ice on it
image(Xc, Yc, icefront)
title("Contour icefront")
dev.off()

#I HAVE CHANGE THIS TO DO IT WITH THE VELOCITIES
#df_if = data.frame(row.names=index) #CHANGED
#for (i in 1:length(index)){ #CHANGED
    #smb_racmo_bm[i] = sum(icefront*region_mask[[i]]$ice*area)/1e12*365 #CHANGED
    #df_if[i,"icefront"] =  smb_racmo_bm[i] #CHANGED
    #smb = sapply(SMB.year, function(x) sum(x*region_mask[[i]]$ice*area)/1e12)

#} #CHANGED

## Plot the velocity u and v of the grounding line with the sign of dS
groundline = regions_uv(mask_ice_land)
jpeg(file.path(outfldr,"mask_u.jpeg"), width = 720, height = 720, quality=100)
image(Xc,Yc,groundline$u, col=c('blue', NA, 'red'))
legend("bottomleft", c("-1","1"), lty=1, col=c("blue", "red"), cex=.70, bty='n')
title("u with sign dS")
dev.off()
jpeg(file.path(outfldr,"mask_v.jpeg"), width = 720, height = 720, quality=100)
image(Xc,Yc,groundline$v, col=c('blue', NA, 'red'))
legend("bottomleft", c("-1","1"), lty=1, col=c("blue", "red"), cex=.70, bty='n')
title("v with sign dS")
dev.off()

#We define the thickness h
h = TOPO_BEDMAP2$H

#NEW : GL FLUX (REGION BY REGION)
gl_flux <- array(0,c(dim(h), length(index)))
gl_flux_u <- array(0,c(dim(h), length(index)))
gl_flux_v <- array(0,c(dim(h), length(index)))
gl_total_flux <- array(0,c(dim(h), length(index)))

#Loop to get the grounding line flux 
for (i in 1:length(index)){
#Ice density at 0ºC is (aproximately) ---------------------------------------------> 916,8 kg/m³
gl_flux_u[,,i] <- -groundline$u * landcontour * region_mask[[i]]$land * dxdy *VEL_R11$u * h * 916.8 /1e12 #Gt/year
gl_flux_v[,,i] <- -groundline$v * landcontour * region_mask[[i]]$land * dxdy *VEL_R11$v * h * 916.8 /1e12 #Gt/year
#I have to fix this part of the loop...
#gl_flux_u_plot[,,i] = apply(gl_flux_u[,,i], MARGIN=c(1,2,i) , FUN=function(x) if (x==0) NA else x)
#gl_flux_v_plot[,,i] = apply(gl_flux_v[,,i], MARGIN=c(1,2,i) ,FUN=function(x) if (x==0) NA else x)
#gl_flux_u = (gl_flux[1:141,1:141,i]$u * dxdy * VEL_R11$u * h /1e6)
#gl_flux_v = (gl_flux[1:141,1:141,i]$u * dxdy * VEL_R11$u * h /1e6)
gl_total_flux[,,i] <- gl_flux_u[,,i]+gl_flux_v[,,i]
}


df_gl = data.frame(row.names=index)
for (i in 1:length(index)){
df_gl[i,"groundline-bm"]  = sum(gl_total_flux[,,i])
#gl_total_u_flux = sum(gl_flux_u[,,i])
#gl_total_v_flux = sum(gl_flux_u[,,i])
#df_gl[i,"groundline-bm"]= gl_total_u_flux + gl_total_v_flux
  }

##flux_gl_u = -groundline$u * icelon * VEL_R11$u
flux_gl_u = (-groundline$u * VEL_R11$u * dxdy * h *0.916/1e9) #CHANGED
flux_gl_v = (-groundline$v * VEL_R11$v * dxdy * h *0.916/1e9) #CHANGED
#flux_gl_v = ((-groundline$v * dxdy * h + -groundline$v * dxdy * h) /1e6)
flux_gl_u_plot = apply(flux_gl_u, c(1,2), function(x) if (x==0) NA else x) #CHANGED
flux_gl_v_plot = apply(flux_gl_v, c(1,2), function(x) if (x==0) NA else x) #CHANGED
image.plot(Xc,Yc, flux_gl_v_plot)

flux_gl = sum(flux_gl_u + flux_gl_v)   
print(flux_gl)
#df_gl = data.frame(row.names=index)
for (i in 1:length(index)){                                          #CHANGED
  df_gl[i,"groundline-bm"]    = sum((flux_gl_v+flux_gl_u)*region_mask[[i]]$land)  #CHANGED
}                                                                    #CHANGED

gl_tot = apply(gl_total_flux,c(1,2),sum)
gl_tot[gl_tot==0] = NA 
gl_tot1 = flux_gl_u + flux_gl_v
gl_tot1[gl_tot1==0] = NA 


# ## Plot the calving velocity uc and vc
calving = regions_uv(mask_ice_ice)
jpeg(file.path(outfldr,"mask_uc.jpeg"), width = 720, height = 720, quality=100)
image(Xc,Yc,calving$u, col=c('blue', NA, 'red'))
legend("bottomleft", c("-1","1"), lty=1, col=c("blue", "red"), cex=.70, bty='n')
title("calving u with sign dS")
dev.off()
jpeg(file.path(outfldr,"mask_vc.jpeg"), width = 720, height = 720, quality=100)
image(Xc,Yc,calving$v, col=c('blue', NA, 'red'))
legend("bottomleft", c("-1","1"), lty=1, col=c("blue", "red"), cex=.70, bty='n')
title("calving v with sign dS")
dev.off()

#NEW: IF FLUX (REGION BY REGION)
if_flux <- array(0,c(dim(h), length(index)))
if_flux_u <- array(0,c(dim(h), length(index)))
if_flux_v <- array(0,c(dim(h), length(index)))
if_total_flux <- array(0,c(dim(h), length(index)))


#Loop to get the grounding line flux 
for (i in 1:length(index)){
#Ice density at 0ºC is (aproximately) ---------------------------------------------> 916,8 kg/m³
if_flux_u[,,i] <- calving$u * icecontour * region_mask[[i]]$ice * dxdy *VEL_R11$u * h * 916.8 /1e12 #Gt/year
if_flux_v[,,i] <- calving$v * icecontour * region_mask[[i]]$ice * dxdy *VEL_R11$v * h * 916.8 /1e12 #Gt/year
#I have to fix this part of the loop...
#if_flux_u_plot[,,i] = apply(gl_flux_u[,,i], MARGIN=c(1,2,i) , FUN=function(x) if (x==0) NA else x)
#if_flux_v_plot[,,i] = apply(gl_flux_v[,,i], MARGIN=c(1,2,i) ,FUN=function(x) if (x==0) NA else x)
#if_flux_u = (if_flux[1:141,1:141,i]$u * dxdy * VEL_R11$u * h /1e6)
#if_flux_v = (if_flux[1:141,1:141,i]$u * dxdy * VEL_R11$u * h /1e6)
if_total_flux[,,i] <- if_flux_u[,,i]+if_flux_v[,,i] #Total ice-front flux is the total lost in u and v directions
}


df_if = data.frame(row.names=index)
for (i in 1:length(index)){
  df_if[i,"icefront"]  = sum(if_total_flux[,,i])
}

### NEW TABLE ###
### Rignot's-like table ###

df_3 = data.frame(row.names=index)
df_3[,"Area km2"] = df[,"Area_bedmap"] #1st COLUMN
df_3[, "GL Gt/year"] = df_gl["groundline-bm"] #2nd COLUMN
df_3[,"SMB Gt/year"] = df[,"SMB-1981-2010"] #3rd COLUMN: 3rd COLUMN of the df TABLE (it is the reference for the A1B scenario simulation)
df_3[,"Ice front Gt/year"] = df_if[,"icefront"] #4th COLUMN
df_3[,"dH/dt Gt/year"] = df_gl["groundline-bm"]+df[,"SMB-1981-2010"]-df_if[,"icefront"]-df_2[,"bmelt(Gt/year)"] #5th COLUMN #CHANGED
df_3[,"Basal melt Gt/year"] = df_2[,"bmelt(Gt/year)"] #6th COLUMN
df_3[,"Bss Gt/year"] = df_2[,"bss(Gt/year)"] #7th COLUMN #direct value
df_3[,"Bss(est) Gt/year"] = -(df_3[, "GL Gt/year"] + df_3[,"SMB Gt/year"] - df_3[,"Ice front Gt/year"]) #estimated value

###################################################
#We eliminate the NaN characters in the table
for(i in 1:length(index)){
  if(is.nan(df_3[i,"Basal melt Gt/year"])==TRUE){
    df_3[i,"Basal melt Gt/year"] = 0
  }
}

for(i in 1:length(index)){
  if(is.nan(df_3[i,"dH/dt Gt/year"])==TRUE){
    df_3[i,"dH/dt Gt/year"] = 0
  }
}

for(i in 1:length(index)){
if(is.nan(df_3[i,"Bss Gt/year"])==TRUE){
df_3[i,"Bss Gt/year"] = 0
  }
}
###################################################

#Total sum
df_3[length(index)+1,]=0 #The sum will be initially zero
for (i in 1:length(index))

{
  
  df_3[length(index)+1,"Area km2"] = df_3[length(index)+1,"Area km2"] + df_3[i,"Area km2"]
  df_3[length(index)+1,"GL Gt/year"] = df_3[length(index)+1,"GL Gt/year"] + df_3[i,"GL Gt/year"]
  df_3[length(index)+1,"SMB Gt/year"] = df_3[length(index)+1,"SMB Gt/year"] + df_3[i,"SMB Gt/year"]
  df_3[length(index)+1,"Ice front Gt/year"] = df_3[length(index)+1,"Ice front Gt/year"] + df_3[i,"Ice front Gt/year"]
  df_3[length(index)+1,"dH/dt Gt/year"] = df_3[length(index)+1,"dH/dt Gt/year"] + df_3[i,"dH/dt Gt/year"]
  df_3[length(index)+1,"Basal melt Gt/year"] = df_3[length(index)+1,"Basal melt Gt/year"] + df_3[i,"Basal melt Gt/year"]
  df_3[length(index)+1,"Bss Gt/year"] = df_3[length(index)+1,"Bss Gt/year"] + df_3[i,"Bss Gt/year"] #direct value
  df_3[length(index)+1,"Bss(est) Gt/year"] =  df_3[length(index)+1,"Bss(est) Gt/year"] + df_3[i,"Bss(est) Gt/year"] #estimated value

}


###################################################

pdf(file.path(outfldr,"main_table.pdf"), height=8, width=11.5)
grid.table(round(df_3, digits = 2))
dev.off()

###################################################


# # Plot Checking ###################################################
comparation=FALSE
if (comparation==TRUE){
    source('contour_check.r')
}
