
function (col, percent = 50) 
{
    c <- rep("#000000", length(col))
    for (i in 1:length(col)) {
        cc <- col2rgb(col[i])/255
        c[i] <- rgb(t(cc), alpha = percent/100)
    }
    return(c)
}

calc_flux = function(mask,u,v,SA,mval0=3,mval1=c(0,1,2))
{

    flux_u = mask*0
    flux_v = mask*0
    for (i in 2:(dim(mask)[1]-1)) {
    for (j in 2:(dim(mask)[2]-1)) {

        if (mask[i,j]==mval0) {    # If point of interest 

            # u_now  = mean(u[(i-1):(i+1),(j-1):(j+1)])
            # v_now  = mean(v[(i-1):(i+1),(j-1):(j+1)])
            # SA_now = mean(SA[(i-1):(i+1),(j-1):(j+1)])
            
            # # Check all neighbor cases 
            # # (with both u- and v-contributions allowed simultaneously!)
            # if (mask[i-1,j]!=mval0 && SA[i-1,j] > 0) flux_u[i,j] = flux_u[i,j] +  1*u_now*SA_now
            # if (mask[i+1,j]!=mval0 && SA[i+1,j] > 0) flux_u[i,j] = flux_u[i,j] + -1*u_now*SA_now

            # if (mask[i,j-1]!=mval0 && SA[i,j-1] > 0) flux_v[i,j] = flux_v[i,j] +  1*v_now*SA_now
            # if (mask[i,j+1]!=mval0 && SA[i,j+1] > 0) flux_v[i,j] = flux_v[i,j] + -1*v_now*SA_now
            
            # Check all neighbor cases 
            # (with both u- and v-contributions allowed simultaneously!)
            if (mask[i-1,j]!=mval0 && SA[i-1,j] > 0) flux_u[i,j] = flux_u[i,j] + ( 1*(u[i-1,j]+u[i,j])/2)*(SA[i-1,j]+SA[i,j])/2
            if (mask[i+1,j]!=mval0 && SA[i+1,j] > 0) flux_u[i,j] = flux_u[i,j] + (-1*(u[i+1,j]+u[i,j])/2)*(SA[i+1,j]+SA[i,j])/2

            if (mask[i,j-1]!=mval0 && SA[i,j-1] > 0) flux_v[i,j] = flux_v[i,j] + ( 1*(v[i,j-1]+v[i,j])/2)*(SA[i,j-1]+SA[i,j])/2
            if (mask[i,j+1]!=mval0 && SA[i,j+1] > 0) flux_v[i,j] = flux_v[i,j] + (-1*(v[i,j+1]+v[i,j])/2)*(SA[i,j+1]+SA[i,j])/2
            
        }

    }
    }

    flux = (flux_u + flux_v) * 916.8 /1e12 #Gt/year 
    # flux = (flux_u + flux_v) * (SA) * 916.8 /1e12 #Gt/year 
    flux[flux<0] = 0 

    return(flux)
}

# After calling `load_data.r` 

#### User options ###
outfldr = "output" 


# Get ocean-land-ice-floating mask
mask = TOPO_BEDMAP2$mask_ice

# Get topography 
H = TOPO_BEDMAP2$H

# Get clean initial basin mask 
basins = BASINS_nasa$basin
basins[BASINS_nasa$basin_mask==0] = NA 



# Basal melt rate, actual present day
bm_actual = BMELT_R13$bm_actual
# Basal melt in the equilibrium
bm_equil  = BMELT_R13$bm_equil


# Calculate fluxes
flux_gl = calc_flux(mask,u=VEL_R11$u,v=VEL_R11$v,SA=dxdy*H,mval0=3,mval1=c(0,1,2))
flux_gl[flux_gl==0]=NA



# Zoom in on an ice shelf 
xlim=c(-620,220)
ylim=c(-1500,-350)


image(Xc,Yc,mask,col=c("white","grey60","grey80")) #,xlim=xlim,ylim=ylim)
image(Xc,Yc,flux_gl,add=TRUE,col=c("blue","red"),levels=c(-1e5,0,1e5))

H0 = H 
H0[H!=0] = NA 
image(Xc,Yc,H0,col="green",add=TRUE)

# image(Xc,Yc,+abs(groundline$u)+abs(groundline$v),col=c(NA,1),add=TRUE)

# uv = VEL_R11$uv
# image(Xc,Yc,VEL_R11$uv,col=alpha(jet.colors,50),add=TRUE)



