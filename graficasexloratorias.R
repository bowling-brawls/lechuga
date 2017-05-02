library(ggplot2)
if(R.version$os=="linux-gnu"){ 
  load("~/Documents/lechuga/noRawdfs.RData")
} else if (R.version$os=="darwin15.6.0"){
  load("~/Documents/Biologia/Tesis/dataAnalysis/noRawdfs.RData")
}

print(qplot(as.factor(as.character(T)), rgrtot, data = HojasConRGR, 
            geom="boxplot",color=T))
