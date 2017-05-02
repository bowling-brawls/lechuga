library(dplyr)
library(lattice)
library(ggplot2)

if(R.version$os=="linux-gnu"){ 
  load("~/Documents/lechuga/datoscompletos.RData")
} else if (R.version$os=="darwin15.6.0"){
  load("~/Documents/Biologia/Tesis/dataAnalysis/datoscompletos.RData")
}
HojasRawFinal$AFE <- HojasRawFinal$a.fol/HojasRawFinal$ms.hoja.tot
HojasRawFinal$IAF <- HojasRawFinal$a.fol/HojasRawFinal$asup
HojasRawFinal$mstotal <- HojasRawFinal$ms.hoja.tot+HojasRawFinal$ms.raiz+HojasRawFinal$ms.tallo

calcRGRdf <- select(HojasRawFinal, DDS, mstotal, IDmata)

tmp1 <- filter(calcRGRdf, DDS==30) %>% rename(DDS1 = DDS, mstotal1 = mstotal)
tmp2 <- filter(calcRGRdf, DDS==45) %>% rename(DDS2 = DDS, mstotal2 = mstotal)
tmp3 <- filter(calcRGRdf, DDS==60) %>% rename(DDS3 = DDS, mstotal3 = mstotal)
tmp4 <- filter(calcRGRdf, DDS==75) %>% rename(DDS4 = DDS, mstotal4 = mstotal)

RGRdf <- merge(tmp1,tmp2, by = "IDmata") 
RGRdf <- merge(RGRdf,tmp3, by = "IDmata") 
RGRdf <- merge(RGRdf,tmp4, by = "IDmata") 

# tiempos <- paste0("DDS",1:4)
# weights <- paste0("mstotal",1:4)
# rgrs <- paste0("RGR",1:4)

RGRdf <- mutate(RGRdf, rgr1 = round((log(mstotal1) - log(mstotal2))/(DDS1-DDS2),3))
RGRdf <- mutate(RGRdf, rgr2 = round((log(mstotal2) - log(mstotal3))/(DDS2-DDS3),3))
RGRdf <- mutate(RGRdf, rgr3 = round((log(mstotal3) - log(mstotal4))/(DDS3-DDS4),3))
RGRdf <- mutate(RGRdf, rgrtot = round((log(mstotal1) - log(mstotal4))/(DDS1-DDS4),3))

RGRdf <- merge(RGRdf, HojasRawFinal[,c("IDmata", "T")], by="IDmata")
RGRdf <- RGRdf[!duplicated(RGRdf),]

HojasConRGR <- merge(HojasRawFinal, RGRdf[,c("IDmata", "rgrtot")], by="IDmata")

save(HojasConRGR, file= "~/Documents/Biologia/Tesis/dataAnalysis/noRawdfs.RData") 
