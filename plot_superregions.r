
# First run `calculations.r` to load the data and perform calculations 

# Load plotting functions
source("functions_plotting.r")

# zlim = range(map_gl, na.rm=TRUE)
# z1 = zlim[1]
# z2 = zlim[2]
# zint = 10
# brks = seq(z1,z2,length.out=zint)
# # col    = c("red","blue")
# # col = rainbow(length(zlim)-1)
# col=colorRampPalette(c("darkmagenta","darkblue","blue","khaki1","red","red4"))(length(brks)-1)

# par(mfrow=c(1,3))
# asp=1.2,pointsize=12
# asp=2.4,pointsize=7 


# Map variables by basin 
palette1 = c("darkblue","blue4","blue2","khaki1","red","red4")
palette2 = c("darkblue","blue4","blue2","white","orange","red","red4")
palette3 = c("darkblue","blue4","blue2","cyan","white") #to plot precipitation (always negative)
palette4 = c("white", "khaki1","red","red4") #to plot projected dH/dt and oceanic temperature anomalies (always positives)
palette5 = c("red4", "red", "khaki1", "white") #to plot unstable areas

zlim = range(map_gl,map_if,map_bm,map_smb,map_dhdt,na.rm=TRUE)
# brks = pretty(zlim,11)
brks   = seq(-110,190,by=10)
col    = colorRampPalette(palette2,bias=1.5)(length(brks)-1)

# brks1  = seq(-20,20,length.out=11)
# col1   = colorRampPalette(palette2,bias=1)(length(brks1)-1)

myfigure(outfldr,"map_gl",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_gl,mask_ice,mask_super,breaks=brks,col=col,title="GL [Gt/yr]")
graphics.off()

myfigure(outfldr,"map_if",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_if,mask_ice,mask_super,breaks=brks,col=col,title="IF [Gt/yr]")
graphics.off()

myfigure(outfldr,"map_bm",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_bm,mask_ice,mask_super,breaks=brks,col=col,title="BM [Gt/yr]")
graphics.off()

myfigure(outfldr,"map_smb",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_smb,mask_ice,mask_super,breaks=brks,col=col,title="SMB [Gt/yr] (Rignot)")
graphics.off()

myfigure(outfldr,"map_dhdt_rignot",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_dhdt,mask_ice,mask_super,breaks=brks,col=col,title="dH/dt [Gt/yr] (Rignot et al)")
graphics.off()

myfigure(outfldr,"map_dhdt_model",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_dhdt_model,mask_ice,mask_super,breaks=brks,col=col,title="dH/dt [Gt/yr] (RACMO 2.3)")
graphics.off()

myfigure(outfldr,"map_smb_00_10",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_smb_00_10,mask_ice,mask_super,breaks=brks,col=col,title="SMB (2000-2010) [Gt/yr]")
graphics.off()

myfigure(outfldr,"map_smb_001_030",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_smb_001_030,mask_ice,mask_super,breaks=brks,col=col,title="SMB (2001-2030) [Gt/yr]")
graphics.off()

myfigure(outfldr,"map_smb_071_100",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_smb_071_100,mask_ice,mask_super,breaks=brks,col=col,title="SMB (2071-2100) [Gt/yr]")
graphics.off()

### PRECIPITATION PLOTS ###

zlim = range(map_prec_001_030,map_prec_071_100,na.rm=TRUE)
# brks = pretty(zlim,11)
brks   = seq(-50000,-15,by=2000)
col    = colorRampPalette(palette3)(length(brks)-1)


myfigure(outfldr,"map_prec_001_030",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_prec_001_030,mask_ice,mask_super,breaks=brks,col=col,title="Snow precipitation offset (2001-2030) [mm/yr]")
graphics.off()

myfigure(outfldr,"map_prec_071_100",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_prec_071_100,mask_ice,mask_super,breaks=brks,col=col,title="Snow precipitation offset (2071-2100) [mm/yr]")
graphics.off()

#Maybe is a good idea to plot with more precision the regions where snow precipitation offset is smaller

brks   = seq(-1000,-15,by=10)
col    = colorRampPalette(palette3)(length(brks)-1)

myfigure(outfldr,"map_prec_current_fine_rignot",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_prec_current_rignot,mask_ice,mask_super,breaks=brks,col=col,title="Snow precipitation offset (current) [mm/yr]")
graphics.off()

myfigure(outfldr,"map_prec_current_fine_model",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_prec_current_model,mask_ice,mask_super,breaks=brks,col=col,title="Snow precipitation offset (current) [mm/yr]")
graphics.off()

myfigure(outfldr,"map_prec_001_030_fine",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_prec_001_030,mask_ice,mask_super,breaks=brks,col=col,title="Snow precipitation offset (2001-2030) [mm/yr]")
graphics.off()

myfigure(outfldr,"map_prec_071_100_fine",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_prec_071_100,mask_ice,mask_super,breaks=brks,col=col,title="Snow precipitation offset (2071-2100) [mm/yr]")
graphics.off()

# image.plot(Xc,Yc,map_gl,breaks=brks,col=col)
# contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")



### CURRENT OFFSET PRECIPITATION MAP ###

zlim = range(map_prec_current,na.rm=TRUE)
brks   = seq(-720,5400,by=200)
col    = colorRampPalette(palette2,bias=3.5)(length(brks)-1)

myfigure(outfldr,"map_prec_current_rignot",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_prec_current_rignot ,mask_ice,mask_super,breaks=brks,col=col,title="Snow precipitation offset (current) [mm/yr]")
graphics.off() #This is the precipitation necesary to not to lose mass ice

myfigure(outfldr,"map_prec_current_model",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_prec_current_model ,mask_ice,mask_super,breaks=brks,col=col,title="Snow precipitation offset (current) [mm/yr]")
graphics.off() #This is the precipitation necesary to not to lose mass ice


brks   = seq(-150,3150,by=100)
col    = colorRampPalette(palette2, bias=5)(length(brks)-1)

myfigure(outfldr,"map_prec_current_perc",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_prec_current_percent,mask_ice,mask_super,breaks=brks,col=col,title="Snow precipitation offset (current) [%]")
graphics.off() #I HAVE TO CREATE A NEW "myfigure" with percentages in the legend


### NET MASS BALANCE PROJECTIONS (2071-2100) ###

zlim = range(map_dHdT_071_100,na.rm=TRUE)
brks   = pretty(zlim,11)
col    = colorRampPalette(palette4)(length(brks)-1)

myfigure(outfldr,"map_dHdT_071_100",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_dHdT_071_100,mask_ice,mask_super,breaks=brks,col=col,title="Net mass balance (2071-2100) [m/yr]")
graphics.off() #This is the projected net mass balance for the period 2071-2100

#Maybe is a good idea to plot with more precision the regions where net mass balance is smaller

brks   = seq(0,5,by=0.1)
col    = colorRampPalette(palette4)(length(brks)-1)

myfigure(outfldr,"map_dHdT_071_100_uns_fine",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_dHdT_071_100,mask_ice,mask_super,breaks=brks,col=col,title="Net mass balance (2071-2100) [m/yr]")
graphics.off()


### OCEANIC TEMPERATURE ANOMALIES PLOTS ###

zlim = range(map_dT_uns,na.rm=TRUE)
brks   = pretty(zlim,11)
col    = colorRampPalette(palette4)(length(brks)-1)

myfigure(outfldr,"map_dT_uns",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_dT_uns,mask_ice,mask_super,breaks=brks,col=col,title="Oceanic subsurface-temperature anomalies (2071-2100) [m/yr]")
graphics.off()

#Maybe is a good idea to plot with more precision the regions where temperature anomalies are smaller

brks   = seq(0,5,by=0.1)
col    = colorRampPalette(palette4)(length(brks)-1)

myfigure(outfldr,"map_dT_uns_fine",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_dT_uns,mask_ice,mask_super,breaks=brks,col=col,title="Oceanic subsurface-temperature anomalies (2071-2100) [deg]")
graphics.off()

### PLOTS OF THE AREAS IN DANGER ###

brks   = seq(0,5,by=0.1)
col    = colorRampPalette(palette5)(length(brks)-1)

myfigure(outfldr,"map_instability",type="png",asp=1.2,pointsize=12)
plot_antarctica(Xc,Yc,map_dT_uns,mask_ice,mask_super,breaks=brks,col=col,title="Unstable areas (2071-2100) [deg]")
graphics.off()


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
image.plot(Xc,Yc,map_smb,breaks=brks,col=col)
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

