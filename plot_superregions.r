
# First run `calculations.r` to load the data and perform calculations 

# Load plotting functions
source("functions_plotting.r")

# Map variables by basin 


# Plot variables 
# image(Xc,Yc,mask_ice,col=c("grey95",NA,"white","grey80"))
# image(Xc,Yc,map_gl,add=TRUE)
# # contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2),drawlabels=FALSE,lwd=4)



zlim = range(map_gl, na.rm=TRUE)
z1 = zlim[1]
z2 = zlim[2]
zint = 10
brks = seq(z1,z2,length.out=zint)
# col    = c("red","blue")
# col = rainbow(length(zlim)-1)
col=colorRampPalette(c("darkmagenta","darkblue","blue","khaki1","red","red4"))(length(brks)-1)
# plot_antarctica(Xc,Yc,map_gl,mask_ice,mask_super,breaks=brks,col=col)

image.plot(Xc,Yc,map_gl,breaks=brks,col=col)
contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")


###GL PLOT###
png(file.path(outfldr,"map_gl"))
zlim = c(range(map_gl, na.rm=TRUE))
z1 = zlim[1]
z2 = zlim[2]
zint = 10
brks = seq(z1,z2,length.out=zint)
col=colorRampPalette(c("darkmagenta","darkblue","blue","khaki1","red","red4"))(length(brks)-1)
image.plot(Xc,Yc,map_gl,breaks=brks,col=col)
title("Grounding line flux")
contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")
dev.off()

###IF PLOT###
png(file.path(outfldr,"map_if"))
zlim = c(range(map_if, na.rm=TRUE))
z1 = zlim[1]
z2 = zlim[2]
zint = 10
brks = seq(z1,z2,length.out=zint)
col=colorRampPalette(c("darkmagenta","darkblue","blue", "khaki1","red","red4"))(length(brks)-1)
image.plot(Xc,Yc,map_if,breaks=brks,col=col)
title("Ice front flux")
contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")
dev.off()

###BM PLOT###
png(file.path(outfldr,"map_bm"))
zlim = c(range(map_bm, na.rm=TRUE))
z1 = zlim[1]
z2 = zlim[2]
zint = 10
brks = seq(z1,z2,length.out=zint)
col=colorRampPalette(c("darkmagenta","darkblue","blue","khaki1","red","red4"))(length(brks)-1)
image.plot(Xc,Yc,map_bm,breaks=brks,col=col)
title("Basal melt")
contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")
dev.off()

###RIGNOT'S SMB PLOT###
png(file.path(outfldr,"map_smb_rignot"))
zlim = c(range(map_smb, na.rm=TRUE))
z1 = zlim[1]
z2 = zlim[2]
zint = 10
brks = seq(z1,z2,length.out=zint)
col=colorRampPalette(c("darkmagenta","darkblue","blue","khaki1","red","red4"))(length(brks)-1)
image.plot(Xc,Yc,map_smb,breaks=brks,col=col)
title("Surface mass balance (Rignot's values)")
contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")
dev.off()

###SMB 1981-2010 (REANALYSIS)###
png(file.path(outfldr,"map_smb_81_10"))
zlim = c(range(map_smb_81_10, na.rm=TRUE))
z1 = zlim[1]
z2 = zlim[2]
zint = 10
brks = seq(z1,z2,length.out=zint)
col=colorRampPalette(c("darkmagenta","darkblue","blue","khaki1","red","red4"))(length(brks)-1)
image.plot(Xc,Yc,map_smb_81_10,breaks=brks,col=col)
title("Surface mass balance (1981-2010)")
contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")
dev.off()

###SMB A1B_2000-2010###
png(file.path(outfldr,"map_smb_00_10"))
zlim = c(range(map_smb_00_10, na.rm=TRUE))
z1 = zlim[1]
z2 = zlim[2]
zint = 10
brks = seq(z1,z2,length.out=zint)
col=colorRampPalette(c("darkmagenta","darkblue","blue","khaki1","red","red4"))(length(brks)-1)
image.plot(Xc,Yc,map_smb_00_10,breaks=brks,col=col)
title("Surface mass balance reference (A1B 2000-2010)")
contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")
dev.off()

##dSMB A1B_2001-2030###
png(file.path(outfldr,"map_dsmb_001_030"))
zlim = range(map_dsmb_001_030, na.rm=TRUE)
z1 = zlim[1]
z2 = zlim[2]
zint = 10
brks = seq(z1,z2,length.out=zint)
col=colorRampPalette(c("darkmagenta","darkblue","blue","khaki1","red","red4"))(length(brks)-1)
image.plot(Xc,Yc,map_dsmb_001_030,breaks=brks,col=col)
title("Surface mass balance change (A1B 2001-2030)")
contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")
dev.off()

##dSMB A1B_2071-2100###
png(file.path(outfldr,"map_dsmb_071_100"))
zlim = c(range(map_dsmb_071_100, na.rm=TRUE))
z1 = zlim[1]
z2 = zlim[2]
zint = 10
brks = seq(z1,z2,length.out=zint)
col=colorRampPalette(c("darkmagenta","darkblue","blue","khaki1","red","red4"))(length(brks)-1)
image.plot(Xc,Yc,map_dsmb_071_100,breaks=brks,col=col)
title("Surface mass balance change (A1B 2071-2100)")
contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")
dev.off()

### dh/dt (Rignot) ##
png(file.path(outfldr,"map_dhdt_rig"))
zlim = c(range(map_dhdt, na.rm=TRUE))
z1 = zlim[1]
z2 = zlim[2]
zint = 10
brks = seq(z1,z2,length.out=zint)
col=colorRampPalette(c("darkmagenta","darkblue","blue","khaki1","red","red4"))(length(brks)-1)
image.plot(Xc,Yc,map_dhdt,breaks=brks,col=col)
title("Surface mass balance change (A1B 2071-2100)")
contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")
dev.off()











#..........MAPS WITH THE RESULTS..........#

# supercolors = c('chocolate4', 'lightblue4', 'lightskyblue', 'blue', 
#            'mediumspringgreen', 'firebrick4', 'gold', 
#            'sandybrown', 'darkgoldenrod', 'gray80', 'seagreen', 
#            'pink3', 'khaki1', 'darkred', 'magenta', 'lightsteelblue', 
#            'mediumblue', 'lightsalmon', 'aquamarine', 'yellow2', 'cadetblue1', 
#            'darkorange')


# ### PLOT OF THE SUPER-REGIONS ###
# 
# pdf(file.path(outfldr,"NEW_REGIONS.pdf"))
# image(Xc, Yc, mask_plot2,col=NA) #first plot
# for (i in 1:22){
# image(Xc, Yc, super_region_mask[[i]]$ice + super_region_mask[[i]]$land, add=TRUE, col=c(NA,supercolors[i]))
# for (ii in seq(length=length(Xc))){
# for (ij in seq(length=length(Yc))){
# if (super_region_mask[[i]]$ice[ii,ij] != 0) {
# points(Xc[ii], Yc[ij], pch = ".", cex = .1) #ice shelves are points
#        }
#      }
#    } 
#  }
#  
# contour(Xc, Yc, mask_ice_land, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="red1") #grounding line contour
# contour(Xc, Yc, mask_ice_all, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="black") #total contour
# title("Antarctica NEW regions")
# dev.off()


#MAPS OF SMB (ALL ANTARCTICA)
# image.plot(smb_racmo.year) #Reanalysis data
# image.plot(SMB.year_2000_2010)
# image.plot(SMB.year_2001_2030 - SMB.year_2000_2010)
# image.plot(SMB.year_2071_2100 - SMB.year_2000_2010)

# 
# #MAPS OF SMB (BY REGIONS)
# 
# def[[16]][1:22] #This are the different values to plot in the map
#                 #I don't take all the column [[16]] because the 23th value is the TOTAL
# 
# #SMB_1981-2010 (reanalysis)
# pdf(file.path(outfldr,"SMB_1981-2010.pdf"))
# image(Xc, Yc, mask_plot2,col=NA) #first plot
# for (i in 1:22){
#   image(Xc, Yc, super_region_mask[[i]]$ice + super_region_mask[[i]]$land, add=TRUE, col=NA)
#   #image.plot() #Plot here the values of smb
#   contour(Xc, Yc, super_region_mask[[i]]$ice, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="black") #grounding line contour
# }
# 
# contour(Xc, Yc, mask_ice_all, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="black") #total contour
# title("Antarctica NEW regions")
# dev.off()

