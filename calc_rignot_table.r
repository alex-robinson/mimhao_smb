

library(openxlsx)




# Load original Rignot data 
tmp = read.xlsx("data/1235798tableS1.xlsx",rows=c(1,3:72),cols=c(1:14))


# Generate a new table, with values instead of strings in the right columns 
rig = data.frame(name=tmp$Ice.Shelf.Name,area=tmp$Actual.area,GL=NA,SMB=NA,IF=NA,dHdt=NA,BM=NA)


# Convert the strings to numbers in each column of interest
nrow = dim(rig)[1]
for (q in 1:nrow) {

    rig$GL[q] = as.numeric(strsplit(tmp$Grounding.line.flux[q],"±")[[1]][1])

}



# Write the clean table back to excel 
#?write.xlsx()