
if(R.version$os=="linux-gnu"){ 
  load("~/Documents/Ignacio/lechuga/noRawdfs.RData")
} else if (R.version$os=="darwin15.6.0"){
  load("~/Documents/Biologia/Tesis/dataAnalysis/noRawdfs.RData")
}


qqnorm(HojasConRGR$fv.fm)
#plot(results$fitted,results$res,xlab="Fitted",ylab="Residuals")

HojasConRGR$T <- as.factor(HojasConRGR$T)
HojasConRGR$DDS <- as.factor(HojasConRGR$DDS)
fvfmresults = lm(fv.fm ~ DDS + T + DDS*T, data=HojasConRGR)

# la funcion aov en este caso son intercambiambles
obj <- aov(fv.fm ~ DDS, data=HojasConRGR)

anova(fvfmresults)
anova(onewayfvfm)
anova(obj)

print(obj)

TukeyHSD(obj)
