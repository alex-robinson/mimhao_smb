

myfigure <- function(fldr=".",file="Rplot",date=TRUE,type="pdf",engine="cairo",
                     width=NULL,height=NULL,units="mm",asp=1,pointsize=12,res=300,
                     cex=1,cex.lab=1,cex.axis=1,bg="white",onefile=TRUE)
{
    # Some system settings
    host  = system("hostname",intern=TRUE)
    os    = system("uname",intern=TRUE)
    today = format(Sys.time(),"%Y-%m-%d")

    # Make filename
    file = paste(file,".",type,sep="")
    if (date == TRUE) file = paste(today,"_",file,sep="")
    file = file.path(fldr,file)

    # If running on a mac, make sure engine is quartz!
    if ( os == "Darwin" ) engine = "quartz"

    # Determine width/heights in inches
    if ( is.null(width) & is.null(height) ) {  # Use default height, determine win via asp
        width  = 189  # Default width for pointsize 12
        height = width/asp
    } else if ( is.null(height) ) {  # only width specified, determine height
        height = width/asp
    } else if ( is.null(width) ) {  # only height specified, determine width
        width = asp*height
    } else {                    # height and width specified, determine asp
        asp = width/height
    }

    # Convert quantities if input was not inches
    cat(type,":",file,"\n")
    cat("width=",width,", height=",height," (",units,".) \n",sep="")
    conv = 1.0
    if ( units == "mm" ) conv = 0.0393700787
    if ( units == "cm" ) conv = 0.393700787
    hin = height*conv
    win = width*conv 
    #cat("width =",win,", height =",hin," (in.)\n")

    if (FALSE & os %in% c("Darwin") & type %in% c("png","jpg","tiff","pdf","ps")) {
        
        cat("Quartz plotting","\n")
        quartz(file=file,type=type,width=win,height=hin,pointsize=pointsize,dpi=res)

    } else if ( type == "png" ) {

        cat("engine = ",engine,"\n")
        png(file,width=win,height=hin,units="in",pointsize=pointsize,res=res,type=engine)

    } else if ( type == "jpg" ) {

        jpeg(file,width=win,height=hin,units="in",pointsize=pointsize,res=res,type=engine)

    } else if ( type == "tiff" ) {

        tiff(file,width=win,height=hin,units="in",pointsize=pointsize,res=res,type=engine)

    } else if ( type == "pdf" ) {

        if (engine %in% c("cairo","cairo1")) {
            cat("**cairo_pdf","\n")
            cairo_pdf(file,width=win,height=hin,pointsize=pointsize,onefile=onefile)
        } else {
            pdf(file,width=win,height=hin,pointsize=pointsize,onefile=onefile)
        }

    } else if ( type == "svg" ) {

        svg(file,width=win,height=hin,pointsize=pointsize)

    } else if ( type == "fig" ) {

        xfig(file,width=win,height=hin,pointsize=pointsize)

    } else {

        cairo_ps(file,width=win,height=hin,pointsize=pointsize)

    }

    par(bg=bg,cex=cex,cex.axis=cex.axis,cex.lab=cex.lab,tcl=0.2,mgp=c(2.5,0.3,0),las=1)

    return(win)
}

mylegend <- function(breaks,col,units="mm",x=c(0,1),y=c(0,1),at=NULL,labels=NULL,
                     xlab="",ylab="",xlim=NULL,ylim=NULL,zlim=range(breaks),
                     cex=1,cex.lab=1,new=TRUE,vertical=TRUE,line=1.8,
                     asp=1,mgp=c(3,0.5,0),col.axis="grey10",...)
{
    n      = length(breaks)    
    ynorm  = (breaks - min(breaks))
    ynorm  = ynorm / max(ynorm)
    y00    = ynorm[1:(n-1)]
    y11    = ynorm[2:n]
    x00    = rep(0,n)
    x11    = rep(1,n) 

    if ( vertical ) {
      x0   = x00
      x1   = x11 
      y0   = y00
      y1   = y11 
      xlim = c(0,1)
      ylim = zlim 
      ax   = 4
    } else {
      x0   = y00
      x1   = y11
      y0   = x00
      y1   = x11
      xlim = zlim
      ylim = c(0,1)
      ax   = 1
    }

    xlim0 = range(x0,x1)
    ylim0 = range(y0,y1)

    par(new=new,xpd=NA,xaxs="i",yaxs="i",...)
    plot( xlim0,ylim0, type="n",axes=F,ann=F,cex=cex)
    rect(x0,y0,x1,y1,col=col,border=col,lwd=1)

    par(new=TRUE,xpd=NA,xaxs="i",yaxs="i",...)
    plot(xlim,ylim,type="n",axes=F,ann=F,cex=cex)
    axis(ax,at=at,labels=labels,mgp=mgp,tcl=-0.1,col=col.axis,col.axis=col.axis,cex.axis=cex)
    box(col="grey10")

    mtext(side=1,line=line,xlab,cex=cex.lab)
    mtext(side=2,line=line,ylab,cex=cex.lab)

    par(xpd=FALSE)
}




plot_antarctica = function(xc,yc,var,mask,mask_reg,breaks,col,title,cex.lab=1)
{
    colax = "grey30" 

    # xlim = range(xc)
    # ylim = range(yc) 
    xlim = c(-3000,3000)
    ylim = c(-3000,3000)

    var1 = var 
    var1[var<min(breaks)] = min(breaks)
    var1[var>max(breaks)] = max(breaks)
    
    # # Also interpolate to higher resolution to avoid overlapping with contours 
    # xhi = seq(xlim[1],xlim[2],by=10)
    # yhi = seq(ylim[1],ylim[2],by=10)

    # loc    = make.surface.grid(list(xhi,yhi))
    # tmp    = interp.surface(list(x=xc,y=yc,z=var1), loc)
    # var1hi = as.surface( loc, tmp)$z
    ## NOTE: The above doesn't work, because nearest neighbor interpolation is needed
    ##       for basin values 

    # Plot of complete Antarctic domain
    par(plt=c(0.14,0.85,0.12,0.9),xaxs="i",yaxs="i",col.lab=colax,col.axis=colax)

    plot(xlim,ylim,type="n",ann=FALSE,axes=FALSE)
    mtext(side=3,line=0.5,las=0,title,cex=cex.lab*1.2)

    grid()
    axis(1,col=colax,col.lab=colax,col.axis=colax)
    axis(2,col=colax,col.lab=colax,col.axis=colax)

    mtext(side=1,line=1.2,las=0,"x [km]",col=colax,cex=cex.lab)
    mtext(side=2,line=2.2,las=0,"y [km]",col=colax,cex=cex.lab)

    image(xc,yc,mask_ice,add=TRUE,breaks=c(0.5,2),col="grey90")
    contour(Xc,Yc,mask_ice,add=TRUE,levels=c(0,2,3),drawlabels=FALSE,lwd=3,col="grey40")
    image(xc,yc,var1,add=TRUE,breaks=breaks,col=col)
    
    # Add point for south pole 
    text(0,0,"South Pole",cex=0.7,col="grey20")

    # Add text and lines for region names 
    text(x=340,y=-380,cex=0.8,col="black","1")
    segments(x0=150,y0=-670,x1=280,y1=-420,col=colax,lwd=1)









    # # Zoom lower-left 
    # # regions: 14, 15, 16, 17, 18, 19
    # var1_zoom = var1 
    # var1_zoom[! mask_reg %in% c(15,16,17,18)] = NA 
    # mask_zoom = mask 

    # xlim = c(-2000,600)
    # ylim = c(-1300,600)

    # par(plt=c(0.05,0.8,0.05,0.65),new=TRUE)
    # plot(xlim,ylim,type="n",ann=FALSE,axes=FALSE)
    # image(xc,yc,var1_zoom,add=TRUE,breaks=breaks,col=col)


    # Add the legend 
    par(plt=c(0.90,0.92,0.3,0.7),new=TRUE)
    mylegend(breaks=brks,col=col)

}


