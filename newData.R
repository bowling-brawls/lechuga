library(xlsx)

if(R.version$os=="linux-gnu"){ 
  load("~/Documents/Ignacio/lechuga/noRawdfs.RData")
} else if (R.version$os=="darwin15.6.0"){
  load("~/Documents/Biologia/Tesis/dataAnalysis/noRawdfs.RData")
}

PE <- read.xlsx("/Users/macintoshhd/Documents/Biologia/Tesis/dataAnalysis/DatosTesis2.xlsx", 
                   sheetIndex = 1, header = TRUE, 
                  # rowIndex =1:37,
                   stringsAsFactors=FALSE)

verdosidad <- read.xlsx("/Users/macintoshhd/Documents/Biologia/Tesis/dataAnalysis/DatosTesis2.xlsx", 
                sheetIndex = 3, header = TRUE, 
                # rowIndex =1:37,
                stringsAsFactors=FALSE)

clima <- read.xlsx("/Users/macintoshhd/Documents/Biologia/Tesis/dataAnalysis/DatosTesis2.xlsx", 
                sheetIndex = 4, header = TRUE, 
                # rowIndex =1:37,
                stringsAsFactors=FALSE)

save(HojasConRGR, PE, verdosidad, clima,
     file= "~/Documents/Biologia/Tesis/dataAnalysis/noRawdfs.RData") 
