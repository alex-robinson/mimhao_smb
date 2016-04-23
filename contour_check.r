# # Plot Checking #########################################################
contour(Xc, Yc, nasa_basin_mask, nlevels=1, drawlabels=FALSE,lwd=2, col="blue")
contour(Xc, Yc, mask_ice_all, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="red")
legend("bottomleft", c("NASA","TOPO BEDMAP2"), lty=1, col=c("blue", "red"), cex=.75, bty='n')
title("All Antartic")

contour(Xc, Yc, mask_racmo, nlevels=1, drawlabels=FALSE,lwd=2, col="blue")
contour(Xc, Yc, mask_ice_all, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="red")
legend("bottomleft", c("RACMO23","TOPO BEDMAP2"), lty=1, col=c("blue", "red"), cex=.75, bty='n')
title("All Antartic")

contour(Xc, Yc, mask_racmo_land, nlevels=1, drawlabels=FALSE,lwd=2, col="blue")
contour(Xc, Yc, mask_ice_land, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="red")
legend("bottomleft", c("RACMO23","TOPO BEDMAP2"), lty=1, col=c("blue", "red"), cex=.75, bty='n')
title("Antartic's ground line")

contour(Xc, Yc, mask_racmo, nlevels=1, drawlabels=FALSE,lwd=2, col="blue")
contour(Xc, Yc, nasa_basin_mask, nlevels=1, add=TRUE, drawlabels=FALSE,lwd=2, col="red")
legend("bottomleft", c("RACMO23","NASA"), lty=1, col=c("blue", "red"), cex=.75, bty='n')
title("All Antartic")


print("Diferencia del area total de la entre NASA y RACMO23, en km^2")
print(sum(mask_ice_all*area)/1e6-sum(mask_racmo*area)/1e6)
