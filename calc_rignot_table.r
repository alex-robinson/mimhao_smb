

library(openxlsx)




# Load original Rignot data 
rig0 = read.xlsx("data/1235798tableS1.xlsx",rows=c(1,3:72),cols=c(1:14))


# Generate a new table, with values instead of strings in the right columns 
rig = data.frame(name=rig0$Ice.Shelf.Name,area=rig0$Actual.area,GL=NA,SMB=NA,IF=NA,dHdt=NA,BM=NA,
                 GL.err=NA,SMB.err=NA,IF.err=NA,dHdt.err=NA,BM.err=NA)


# Convert the strings to numbers in each column of interest
nrow = dim(rig)[1]
for (q in 1:nrow) {

    tmp = strsplit(rig0$Grounding.line.flux[q],"±")[[1]]
    tmp1 = strsplit(tmp[2]," ")[[1]][1]
    rig$GL[q]     = tmp[1]
    rig$GL.err[q] = tmp1

}



# Write the clean table back to excel 
#?write.xlsx()