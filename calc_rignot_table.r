

library(openxlsx)




# Load original Rignot data 
rig0 = read.xlsx("data/1235798tableS1.xlsx",rows=c(1,3:72),cols=c(1:14))


# Generate a new table, with values instead of strings in the right columns 
rig = data.frame(Name=rig0$Ice.Shelf.Name,Area=rig0$Actual.area,GL=NA,IF=NA,SMB=NA,dHdt=NA,BSS=NA,BM=NA,
                 GL.err=NA,IF.err=NA,SMB.err=NA,dHdt.err=NA,BSS.err=NA,BM.err=NA)


# Convert the strings to numbers in each column of interest
nrow = dim(rig)[1]
for (q in 1:nrow) {

    #GL - Grounding line flux column
    grl = strsplit(rig0$Grounding.line.flux[q],"±")[[1]]
    grl1 = strsplit(grl[2]," ")[[1]][1]
    rig$GL[q]     = grl[1] #Grounding line flux
    rig$GL.err[q] = grl1 #Grounding line flux error

    #IF - Ice front flux column
    ift = strsplit(rig0$'Ice-front.flux'[q],"±")[[1]]
    ift1 = strsplit(ift[2]," ")[[1]][1]
    rig$IF[q]     = ift[1] #Ice-front flux
    rig$IF.err[q] = ift1 #Ice-front flux error
    #I write 'ice-front.flux' with '' because if not the program doesn't read the column correctly
    
    #SMB - Surface mass balance column
    smb = strsplit(rig0$SMB[q],"±")[[1]]
    smb1 = strsplit(smb[2]," ")[[1]][1]
    rig$SMB[q]     = smb[1] #SMB
    rig$SMB.err[q] = smb1 #SMB error
    
    #∂H/∂t
    dhdt = strsplit(rig0$'∂H/∂t'[q],"±")[[1]]
    dhdt1 = strsplit(dhdt[2]," ")[[1]][1]
    rig$dHdt[q]     = dhdt[1] #∂H/∂t
    rig$dHdt.err[q] = dhdt1 #∂H/∂t error
    #I've modified the original file because the [5] didn't have separation from the rest of the terms
    #I write '∂H/∂t' with '' because if not the program doesn't read the column correctly
    
    #BSS - Basal melt in steady state (Gt/year)
    bml = strsplit(rig0$B[q],"±")[[1]] #The first column called 'B' is in Gt/year
    bml1 = strsplit(bml[2]," ")[[1]][1]
    rig$BM[q]     = bml[1] #Basal melt flux
    rig$BM.err[q] = bml1 #Basal melt flux error
    
    #BM - Basal melt (Gt/year)
    bss = strsplit(rig0$Bss[q],"±")[[1]] #The first column called 'Bss' is in Gt/year
    bss1 = strsplit(bss[2]," ")[[1]][1]
    rig$BSS[q]     = bss[1] #Basal melt in steady state
    rig$BSS.err[q] = bss1 #Basal melt in steady state error
    
    }


# Write the clean table back to excel 
write.xlsx(rig,"data/rignot.xlsx")
