
# First run `calculations.r` to load the data and perform calculations 


# Map variables by basin 


# Plot variables 
# image(Xc,Yc,mask_ice,col=c("grey95",NA,"white","grey80"))
# image(Xc,Yc,map_gl,add=TRUE)
# # contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2),drawlabels=FALSE,lwd=4)

breaks = range(map_gl)
# col    = c("red","blue")
col    = rainbow(length(breaks)-1)

image.plot(Xc,Yc,map_gl,breaks=breaks,col=col)
contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey50")







#..........MAPS WITH THE RESULTS..........#

supercolors = c('chocolate4', 'lightblue4', 'lightskyblue', 'blue', 
           'mediumspringgreen', 'firebrick4', 'gold', 
           'sandybrown', 'darkgoldenrod', 'gray80', 'seagreen', 
           'pink3', 'khaki1', 'darkred', 'magenta', 'lightsteelblue', 
           'mediumblue', 'lightsalmon', 'aquamarine', 'yellow2', 'cadetblue1', 
           'darkorange')







### OLD ###

pdf(file.path(outfldr,"NEW_REGIONS.pdf"))
image(Xc, Yc, mask_plot2,col=NA) #first plot
  for (i in 1:22){
  image(Xc, Yc, super_region_mask[[i]]$ice + super_region_mask[[i]]$land, add=TRUE, col=c(NA,supercolors[i]))
  for (ii in seq(length=length(Xc))){
    for (ij in seq(length=length(Yc))){
      if (super_region_mask[[i]]$ice[ii,ij] != 0) {
        points(Xc[ii], Yc[ij], pch = ".", cex = .1) #ice shelves are points
      }
    }
  } 
}

contour(Xc, Yc, mask_ice_land, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="red1") #grounding line contour
contour(Xc, Yc, mask_ice_all, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="black") #total contour
title("Antarctica NEW regions")
dev.off()


#MAPS OF SMB (ALL ANTARCTICA)
image.plot(smb_racmo.year) #Reanalysis data
image.plot(SMB.year_2000_2010)
image.plot(SMB.year_2001_2030 - SMB.year_2000_2010)
image.plot(SMB.year_2071_2100 - SMB.year_2000_2010)


#MAPS OF SMB (BY REGIONS)

def[[16]][1:22] #This are the different values to plot in the map
                #I don't take all the column [[16]] because the 23th value is the TOTAL

#SMB_1981-2010 (reanalysis)
pdf(file.path(outfldr,"SMB_1981-2010.pdf"))
image(Xc, Yc, mask_plot2,col=NA) #first plot
for (i in 1:22){
  image(Xc, Yc, super_region_mask[[i]]$ice + super_region_mask[[i]]$land, add=TRUE, col=NA)
  #image.plot() #Plot here the values of smb
  contour(Xc, Yc, super_region_mask[[i]]$ice, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="black") #grounding line contour
}

contour(Xc, Yc, mask_ice_all, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="black") #total contour
title("Antarctica NEW regions")
dev.off()

