library(xlsx)
library(dplyr)
setwd("/Users/macintoshhd/Documents/Biologia/Tesis/dataAnalysis/")
# Read Data ---------------------------------------------------------------

hoja1 <- read.xlsx("/Users/macintoshhd/Downloads/HOJA DATOS TESIS.xlsx", 
                   sheetIndex = 1, header = TRUE, 
                   rowIndex =1:37,
                   stringsAsFactors=FALSE)
hoja3 <- read.xlsx("/Users/macintoshhd/Downloads/HOJA DATOS TESIS.xlsx",
                   sheetIndex = 3, header = TRUE,
                   rowIndex =1:37,
                   stringsAsFactors=FALSE)
hoja3$DDS <- 45
hoja5 <- read.xlsx("/Users/macintoshhd/Downloads/HOJA DATOS TESIS.xlsx", 
                   sheetIndex = 5, header = TRUE,
                   rowIndex =1:37,
                   stringsAsFactors=FALSE)
hoja7 <- read.xlsx("/Users/macintoshhd/Downloads/HOJA DATOS TESIS.xlsx", 
                   sheetIndex = 7, header = TRUE,
                   rowIndex =1:37,
                   stringsAsFactors=FALSE)

# hojas pares
colqsi <- c("DDS", "T", "R","P","mf.hoj","a.fol","PFcra","PTcra","PScra","ms.raiz",
"ms.tallo","ms.hoja.pcl","ms.hoja.tot","CRA")

hoja2 <- read.xlsx("/Users/macintoshhd/Downloads/HOJA DATOS TESIS.xlsx", 
                   sheetIndex = 2, header = TRUE)
hoja2 <- hoja2[,colqsi]

hoja4 <- read.xlsx("/Users/macintoshhd/Downloads/HOJA DATOS TESIS.xlsx",
                   sheetIndex = 4, header = TRUE)
names(hoja4)[names(hoja4) == "mf..hoj"] <- "mf.hoj"
hoja4 <- hoja4[,colqsi]
hoja6 <- read.xlsx("/Users/macintoshhd/Downloads/HOJA DATOS TESIS.xlsx",
                   sheetIndex = 6, header = TRUE)
names(hoja6)[names(hoja6) == "mf..hoj"] <- "mf.hoj"
hoja6 <- hoja6[,colqsi]
hoja8 <- read.xlsx("/Users/macintoshhd/Downloads/HOJA DATOS TESIS.xlsx",
                   sheetIndex = 8, header = TRUE)
names(hoja8)[names(hoja8) == "mf..hoj"] <- "mf.hoj"
hoja8 <- hoja8[,colqsi]

nomsdfs <- paste0("hoja", 1:8)

for(i in 1:8){
tempdf <- get(nomsdfs[i]) 
tempdfID <- transform(tempdf[,c("DDS","T","R","P")], IDmata=paste0(T,R,P))  
tempdf$IDmata <- as.numeric(as.character(tempdfID$IDmata))
assign(nomsdfs[i], tempdf)
rm(tempdf,tempdfID)
}

hojasFluo <- rbind(hoja1,hoja3,hoja5,hoja7) 
hojasMue <-  rbind(hoja2,hoja4,hoja6,hoja8)

HojasRawFinal<- merge(hojasFluo,hojasMue)

#crear columna area superficial
names(HojasRawFinal)[names(HojasRawFinal) == "a.sup"] <- "diam"
HojasRawFinal$asup <- ((HojasRawFinal$diam/2)^2)*pi



save(HojasRawFinal, file="datoscompletos.RData")
write.xlsx(HojasRawFinal,file = "datoscompletos.xlsx")
