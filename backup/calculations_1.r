
# First, make sure to load the data using:
# `source('load_data.r')`

# Load dependencies 
source('function_regions.r')
library(gridExtra)
library(fields)

# Define the output folder 
outfldr = "output" 

# Get diferents masks ============================================

## Regions of NASA extended
nasa_basin = BASINS_nasa$basin
## Antarctica Mask from NASA (ice+land)
nasa_basin_mask = BASINS_nasa$basin_mask

## Ice and land mask from TOPO BEDMAP2
mask_ice = TOPO_BEDMAP2$mask_ice

mask_ice_land = apply(mask_ice, c(1, 2), function(x) if (x==2) 1 else 0)
mask_ice_ice  = apply(mask_ice, c(1, 2), function(x) if (x==3) 1 else 0)
mask_ice_all  = apply(mask_ice, c(1, 2), function(x) if (x!=0) 1 else 0)

## Ice and land mask from RACMO23
mask_racmo      = RACMO23_ERA_INTERIM_monthly_1981_2010$mask
mask_racmo_land = RACMO23_ERA_INTERIM_monthly_1981_2010$mask_grounded
mask_racmo_ice  = mask_racmo - mask_racmo_land

region_mask=region.mask(land=mask_ice_land, ice=mask_ice_ice)
#region_mask2=region.mask(land=mask_racmo_land, ice=mask_racmo_ice)

# Basal melt rate, actual present day
bmelt = BMELT_R13$bm_actual
bss = BMELT_R13$bm_equil

# Units   ######################################################################
# magnitude : from ncdf to table
# Area  : m^2  ->  1e-9 km^2
# SMB   : kg*m^-2*d^-1  ->  1e-12 Gt/year
#         RACMO23 : mean; sum(*area)/1e12*365
#         RACMO2_ANT3K55_HadCM3_A1B_monthly : sum; sum(*area)/1e12
# BMelt : m*a^-1  ->  Gt/year
#         BMELT_13 : mean((bmelt*regions)[regions==1])


# Creating data frame with areas  ##############################################
index = paste("region",seq(length.out=range(nasa_basin)[2]),sep="")
df = data.frame(row.names=index)

df[,"Area_bedmap"] = sapply(region_mask, function(x) x$area) #1st COLUMN OF THE df TABLE
#df[,"Area_racmo"] = sapply(region_mask2, function(x) x$area)


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
smb_racmo.year = apply(smb_racmo.month,c(1,2),mean)

SMB.month = list(RACMO2_ANT3K55_HadCM3_A1B_monthly_2000_2010$smb,
            RACMO2_ANT3K55_HadCM3_A1B_monthly_2001_2030$smb, 
            RACMO2_ANT3K55_HadCM3_A1B_monthly_2071_2100$smb)

SMB.year = lapply(SMB.month, function(x) apply(x,c(1,2),sum))

for (i in 1:length(index)){
    #smb_racmo = sum(smb_racmo.year*region_mask2[[i]]$ice*area)/1e12*365
    smb_racmo_bm = sum(smb_racmo.year*region_mask[[i]]$ice*area)/1e12*365
    #df[i,"SMB-RACMO23-racmo"] = smb_racmo #2nd COLUMN OF THE df TABLE
    df[i,"SMB-1981-2010"] = smb_racmo_bm #3rd COLUMN OF THE df TABLE (real data from reanalysis)
    smb = sapply(SMB.year, function(x) sum(x*region_mask[[i]]$ice*area)/1e12) #As it comes from SMB.month, smb has 3 matrix inside too
    df[i,"SMB-A1B-2000-2010"] = smb[1] #4rd COLUMN OF THE df TABLE (it is the reference for the A1B scenario simulation)
    df[i,"dSMB-A1B-2001-2030"] = (smb[2] - smb[1]) #5th COLUMN OF THE df TABLE
    df[i,"dSMB-A1B-2071-2100"] = (smb[3] - smb[1]) #6th COLUMN OF THE df TABLE
}

pdf(file.path(outfldr,"df_smb.pdf"), height=8, width=11.5)
grid.table(round(df, digits = 2)) #THE PDF FILE CONTAINS THE df TABLE
dev.off()

## BMELT_13 #

df_2 = data.frame(row.names=index)

for (i in 1:length(index)){
    ii = which(region_mask[[i]]$ice == 1)
    #df_2[i,"bmelt(Gt/year)"] = mean(region_mask2[[i]]$ice[ii]*bmelt[ii])
    df_2[i,"bmelt(Gt/year)"] = mean((bmelt*region_mask[[i]]$ice)[ii])
    #df_2[i,"bm_equil(Gt/year)"] = mean((BMELT_R13$bm_equil*region_mask2[[i]]$ice)[ii])
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
    }
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


# df_if = data.frame(row.names=index)
# for (i in 1:length(index)){
#     smb_racmo = sum(icefront*region_mask2[[i]]$ice*area)/1e12*365
#     smb_racmo_bm = sum(icefront*region_mask[[i]]$ice*area)/1e12*365
#     df_if[i,"icefront"] = 
#     smb = sapply(SMB.year, function(x) sum(x*region_mask[[i]]$ice*area)/1e12)
# 
# }

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

# icelon = mask_ice_ice*lon2D
# icelat = mask_ice_ ice*lat2D

h = TOPO_BEDMAP2$H
##flux_gl_u = -groundline$u * icelon * VEL_R11$u
flux_gl_u = (-groundline$u * 40e3 * VEL_R11$u * h /1e6)
flux_gl_v = (-groundline$v * 40e3 * VEL_R11$v * h /1e6)
# flux_gl_v = ((-groundline$v * 40e3 * h + -groundline$v * 40e3 * h) /1e6)
flux_gl_v_plot = apply(flux_gl_v, c(1,2), function(x) if (x==0) NA else x)
#image.plot(Xc,Yc, flux_gl_v_plot)

flux_gl = flux_gl_u + flux_gl_v   

df_gl = data.frame(row.names=index)
for (i in 1:length(index)){
    #df_gl[i,"groundline-racmo"] = sum(flux_gl_v*region_mask2[[i]]$ice)
    df_gl[i,"groundline-bm"]    = sum(flux_gl_v*region_mask[[i]]$ice)
}

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

### NEW TABLE ###
### Rignot's-like table ###

df_3 = data.frame(row.names=index) #1st COLUMN
df_3[,"Area km2"] = df[,"Area_bedmap"] #2nd COLUMN
df_3[,"SMB Gt/year"] = df[,"SMB-A1B-2000-2010"] #4rd COLUMN OF THE df TABLE (it is the reference for the A1B scenario simulation)





# # Plot Checking ###################################################
comparation=FALSE
if (comparation==TRUE){
    source('contour_check.r')
}
