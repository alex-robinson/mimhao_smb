

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

#RIGNOT'S TABLE WITH NEW REGIONS

# Load modified data from xslx
new0 = read.xlsx("data/new_regions.xlsx",sheet = 1, rows=c(1:24),cols=c(1:5))
#(I did this xlsx by compairing the NASA basins with Rignot's regions)

# Generate a new table, with values instead of strings in the right columns 
new = data.frame(Name=new0$Name,Area.Rignot=new0$`Area.(Rignot)`,GL=NA,IF=NA,SMB=NA,dHdt=NA,BSS=NA,BM=NA,
                 GL.err=NA,IF.err=NA,SMB.err=NA,dHdt.err=NA,BSS.err=NA,BM.err=NA)

for (r in 3:14)  #r gives the different columns of the data frame
  
{
    #New column for SMB
    new[[r]][1]=as.numeric(rig[[r]][which(rig=="Ronne")])+as.numeric(rig[[r]][which(rig=="Larsen G")])
    new[[r]][2]=rig[[r]][which(rig=="Filchner")]
    new[[r]][3]=as.numeric(rig[[r]][which(rig=="Quar")])+as.numeric(rig[[r]][which(rig=="Riiser-Larsen")])+as.numeric(rig[[r]][which(rig=="Brunt/Stancomb")])
    new[[r]][4]=as.numeric(rig[[r]][which(rig=="Jelbart")])+as.numeric(rig[[r]][which(rig=="Atka [8]")])+as.numeric(rig[[r]][which(rig=="Ekstrom")])
    new[[r]][5]=as.numeric(rig[[r]][which(rig=="Baudouin [9]")])+as.numeric(rig[[r]][which(rig=="Borchgrevink")])+as.numeric(rig[[r]][which(rig=="Lazarev")])+as.numeric(rig[[r]][which(rig=="Nivl")])+as.numeric(rig[[r]][which(rig=="Vigrid")])+as.numeric(rig[[r]][which(rig=="Fimbul")])
    new[[r]][6]=as.numeric(rig[[r]][which(rig=="Rayner/Thyer")])+as.numeric(rig[[r]][which(rig=="Shirase")])+as.numeric(rig[[r]][which(rig=="Prince Harald")])
    new[[r]][7]=as.numeric(rig[[r]][which(rig=="Wilma/Robert/Downer")])+as.numeric(rig[[r]][which(rig=="Edward VIII")])
    new[[r]][8]=as.numeric(rig[[r]][which(rig=="Amery")])
    new[[r]][9]=as.numeric(rig[[r]][which(rig=="Conger/Glenzer")])+as.numeric(rig[[r]][which(rig=="Tracy/Tremenchus")])+as.numeric(rig[[r]][which(rig=="Shackleton")])+as.numeric(rig[[r]][which(rig=="West")])+as.numeric(rig[[r]][which(rig=="Publications")])
    new[[r]][10]=as.numeric(rig[[r]][which(rig=="Holmes")])+as.numeric(rig[[r]][which(rig=="Moscow University")])+as.numeric(rig[[r]][which(rig=="Totten")])+as.numeric(rig[[r]][which(rig=="Vincennes [8]")])
    new[[r]][11]=as.numeric(rig[[r]][which(rig=="Cook")])+as.numeric(rig[[r]][which(rig=="Ninnis")])+as.numeric(rig[[r]][which(rig=="Mertz")])+as.numeric(rig[[r]][which(rig=="Dibble")])
    new[[r]][12]=as.numeric(rig[[r]][which(rig=="Aviator")])+as.numeric(rig[[r]][which(rig=="Mariner")])+as.numeric(rig[[r]][which(rig=="Lillie")])+as.numeric(rig[[r]][which(rig=="Rennick")])
    new[[r]][13]=as.numeric(rig[[r]][which(rig=="Drygalski")])+as.numeric(rig[[r]][which(rig=="Nansen")])
    new[[r]][14]=as.numeric(rig[[r]][which(rig=="Ross East [7]")])
    new[[r]][15]=as.numeric(rig[[r]][which(rig=="Ross West")])
    new[[r]][16]=as.numeric(rig[[r]][which(rig=="Getz")])+as.numeric(rig[[r]][which(rig=="Land")])+as.numeric(rig[[r]][which(rig=="Nickerson")])+as.numeric(rig[[r]][which(rig=="Sulzberger")])+as.numeric(rig[[r]][which(rig=="Swinburne")])+as.numeric(rig[[r]][which(rig=="Withrow")])
    new[[r]][17]=as.numeric(rig[[r]][which(rig=="Thwaites")])+as.numeric(rig[[r]][which(rig=="Crosson")])+as.numeric(rig[[r]][which(rig=="Dotson")])
    new[[r]][18]=as.numeric(rig[[r]][which(rig=="Pine Island")])
    new[[r]][19]=as.numeric(rig[[r]][which(rig=="Ferrigno")])+as.numeric(rig[[r]][which(rig=="Venable")])+as.numeric(rig[[r]][which(rig=="Abbot")])+as.numeric(rig[[r]][which(rig=="Cosgrove")])
    new[[r]][20]=as.numeric(rig[[r]][which(rig=="Wilkins")])+as.numeric(rig[[r]][which(rig=="Bach")])+as.numeric(rig[[r]][which(rig=="George VI")])+as.numeric(rig[[r]][which(rig=="Stange")])
    new[[r]][21]=as.numeric(rig[[r]][which(rig=="Wordie")])
    new[[r]][22]=as.numeric(rig[[r]][which(rig=="Larsen B")])+as.numeric(rig[[r]][which(rig=="Larsen C")])+as.numeric(rig[[r]][which(rig=="Larsen D")])+as.numeric(rig[[r]][which(rig=="Larsen E")])+as.numeric(rig[[r]][which(rig=="Larsen F")])
    new[[r]][23]=sum(as.numeric(rig[[r]]))
}    

# Write the clean table back to excel 
write.xlsx(new,"data/rignot_new.xlsx")
