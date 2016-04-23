
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
region_mask2=region.mask(land=mask_racmo_land, ice=mask_racmo_ice)

# Basal melt rate, actual present day
bmelt = BMELT_R13$bm_actual

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

df[,"Area_bedmap"] = sapply(region_mask, function(x) x$area) 
#df[,"Area_racmo"] = sapply(region_mask2, function(x) x$area)


# Calculations #################################################################

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
    smb_racmo = sum(smb_racmo.year*region_mask2[[i]]$ice*area)/1e12*365
    smb_racmo_bm = sum(smb_racmo.year*region_mask[[i]]$ice*area)/1e12*365
    df[i,"SMB-RACMO23-racmo"] = smb_racmo
    df[i,"SMB-RACMO23-bedmap"] = smb_racmo_bm
    smb = sapply(SMB.year, function(x) sum(x*region_mask[[i]]$ice*area)/1e12)
    df[i,"SMB-2000-2010"] = smb[1]
    df[i,"dSMB-2001-2030"] = (smb[1] - smb[2])
    df[i,"dSMB-2071-2100"] = (smb[1] - smb[3])
}

pdf(file.path(outfldr,"df_smb.pdf"), height=8, width=11.5); grid.table(round(df, digits = 2))
dev.off()

## BMELT_13 #
## BMELT_13_conservative #

df_2 = data.frame(row.names=index)

for (i in 1:length(index)){
    ii = which(region_mask2[[i]]$ice == 1)
    #df_2[i,"bmelt(Gt/year)"] = mean(region_mask2[[i]]$ice[ii]*bmelt[ii])
    df_2[i,"bmelt(Gt/year)"] = mean((bmelt*region_mask2[[i]]$ice)[ii])
    #df_2[i,"bm_equil(Gt/year)"] = mean((BMELT_R13$bm_equil*region_mask2[[i]]$ice)[ii])
}

pdf(file.path(outfldr,"df_melt.pdf"), height=8, width=11.5); grid.table(round(df_2, digits = 2))
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

# image.plot(Xc, Yc, mask_plot, nlevel=27, col=rainbow(27), main="Antarctica (NASA)")
# contour(Xc, Yc, mask_ice_land, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=3, col="black")
# for (i in 1:27){
#     contour(Xc, Yc, region_mask[[i]]$ice, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=1, col="white")
#     contour(Xc, Yc, region_mask[[i]]$land, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=1, col="gray20")
# }

# ## Obtencion de dos vectores para puntear las zonas de hielo
# xy=list()
# for (i in 1:27){
#     xx=logical(0)
#     yy=logical(0)
#     for (ij in seq(length=length(Yc))){
#         for (ii in seq(length=length(Xc))){
#             if (region_mask[[i]]$ice[ii,ij] != 0) {
#                 xx = append(xx, Xc[ii])
#                 yy = append(yy, Yc[ij])
#             }
#         }
#         xy[[paste("region",i,sep="")]] = list(x=xx,y=yy)
#     }
# }
# ## Añadir points(xy[[i]]$x, xy[[i]]$y, pch = ".", cex = .1) al bucle donde pinta cada region

## Contour from NASA
# mask_plot =  nasa_basin * nasa_basin_mask
# mask_plot[mask_plot==0] = NA

# image(Xc, Yc, mask_plot,col=NA)
# for (i in 1:27){
#     image(Xc, Yc, region_mask[[i]]$ice, add=TRUE, col=c(NA,colors[i]))
#     image(Xc, Yc, region_mask[[i]]$land, add=TRUE, col=c(NA,colors[i]))
#     for (ii in seq(length=length(Xc))){
#         for (ij in seq(length=length(Yc))){
#             if (region_mask[[i]]$ice[ii,ij] != 0) {
#                 points(Xc[ii], region_mask[[i]]$ice[ii,ij]*Yc[ij], pch = ".", cex = .1)
#             }
#         }
#     }
# }
# contour(Xc, Yc, nasa_basin_mask, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="black")
# title("Antarctica (NASA)")


## Contour from TOPO BEDMAP2
mask_plot2 = nasa_basin * mask_ice_all
mask_plot2_unit = apply(mask_plot2, c(1, 2), function(x) if (x!=0) x/x else x)
mask_plot2[mask_plot2==0] = NA

pdf(file.path(outfldr,"TOPO_BEDMAP2.pdf"))
image(Xc, Yc, mask_plot2,col=NA)
for (i in 1:27){
    #image(Xc, Yc, region_mask[[i]]$ice, add=TRUE, col=c(NA,colors[i]))
    image(Xc, Yc, region_mask[[i]]$land+region_mask[[i]]$ice, add=TRUE, col=c(NA,colors[i]))
    for (ii in seq(length=length(Xc))){
        for (ij in seq(length=length(Yc))){
            if (region_mask[[i]]$ice[ii,ij] != 0) {
                points(Xc[ii], region_mask[[i]]$ice[ii,ij]*Yc[ij], pch = ".", cex = .1)
            }
        }
    }
}
contour(Xc, Yc, mask_ice_land, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="red1")
contour(Xc, Yc, mask_ice_all, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="black")
title("Antarctica (TOPO BEDMAP2)")
dev.off()


## Contour from RACMO23
# mask_plot3 = nasa_basin * mask_racmo
# mask_plot3[mask_plot3==0] = NA

# pdf("RACMO23.pdf")
# image(Xc, Yc, mask_racmo,col=NA)
# for (i in 1:27){
#     image(Xc, Yc, region_mask[[i]]$ice, add=TRUE, col=c(NA,colors[i]))
#     image(Xc, Yc, region_mask[[i]]$land, add=TRUE, col=c(NA,colors[i]))
#     for (ii in seq(length=length(Xc))){
#         for (ij in seq(length=length(Yc))){
#             if (region_mask[[i]]$ice[ii,ij] != 0) {
#                 points(Xc[ii], region_mask[[i]]$ice[ii,ij]*Yc[ij], pch = ".", cex = .1)
#             }
#         }
#     }
# #    points(xy[[i]]$x, xy[[i]]$y, pch = ".", cex = .1)
# }
# contour(Xc, Yc, mask_racmo_land, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="red")
# contour(Xc, Yc, mask_racmo, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="black")
# title("Antarctica (RACMO23)")
# dev.off()


# image.plot(Xc, Yc, SMB.year[[1]],nlevel=1, col=rainbow(27))
# contour(Xc, Yc, SMB.year[[1]], add=TRUE, drawlabels=FALSE,lwd=0.05, col="white")

# filled.contour(Xc, Yc, SMB.year[[1]], color.palette=rainbow, plot.axes = {axis(1); axis(2) ; contour(Xc, Yc, mask_ice_land, levels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="brown"); contour(Xc, Yc, mask_ice_ice, levels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="white"); contour(Xc, Yc, mask_smb, add=TRUE, drawlabels=FALSE,lwd=0.5, col="darkgrey")})


# Creation of ice front mask ###################################################

i1 = which(mask_ice_all == 1, arr.ind=T)

iceland = region_any(mask_ice_all)
jpeg(file.path(outfldr,"Antarctica.jpeg"), width = 720, height = 720, quality=100)
image(Xc, Yc, iceland); title("Contour Antarctica")
dev.off()
jpeg(file.path(outfldr,"land.jpeg"), width = 720, height = 720, quality=100)
landcontour = region_any(mask_ice_land)
image(Xc, Yc, landcontour); title("Contour grounding land")
dev.off()
jpeg(file.path(outfldr,"ice.jpeg"), width = 720, height = 720, quality=100)
icecontour = region_any(mask_ice_ice)
image(Xc, Yc, icecontour); title("Contour ice shelf")
dev.off()
jpeg(file.path(outfldr,"icefront.jpeg"), width = 720, height = 720, quality=100)
icefront = region_any(mask_ice, 3)
image(Xc, Yc, icefront); title("Contour icefront")
dev.off()


# df_if = data.frame(row.names=index)
# for (i in 1:length(index)){
#     smb_racmo = sum(icefront*region_mask2[[i]]$ice*area)/1e12*365
#     smb_racmo_bm = sum(icefront*region_mask[[i]]$ice*area)/1e12*365
#     df_if[i,"icefront"] = 
#     smb = sapply(SMB.year, function(x) sum(x*region_mask[[i]]$ice*area)/1e12)
# 
# }


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
flux_gl_v = (-groundline$u * 40e3 * VEL_R11$v * h /1e6)
flux_gl_v_plot = apply(flux_gl_v, c(1,2), function(x) if (x==0) NA else x)
#image.plot(Xc,Yc, flux_gl_v_plot)


df_gl = data.frame(row.names=index)
for (i in 1:length(index)){
    df_gl[i,"groundline-racmo"] = sum(flux_gl_v*region_mask2[[i]]$ice)
    df_gl[i,"groundline-bm"] = sum(flux_gl_v*region_mask[[i]]$ice)
}


# # Plot Checking ###################################################
comparation=FALSE
if (comparation==TRUE){
    source('contour_check.r')
}
