
if(R.version$os=="linux-gnu"){ 
  load("~/Documents/Ignacio/lechuga/noRawdfs.RData")
} else if (R.version$os=="darwin15.6.0"){
  load("~/Documents/Biologia/Tesis/dataAnalysis/noRawdfs.RData")
}
HojasConRGR$T <- as.factor(HojasConRGR$T)
HojasConRGR$DDS <- as.factor(HojasConRGR$DDS)

# ANOVA FvfM --------------------------------------------------------------
qqnorm(HojasConRGR$fv.fm)
#plot(results$fitted,results$res,xlab="Fitted",ylab="Residuals")

fvfmresults = lm(fv.fm ~ DDS + T + DDS*T, data=HojasConRGR)

# la funcion aov en este caso son intercambiambles
obj <- aov(fv.fm ~ DDS, data=HojasConRGR)

anova(fvfmresults)

anova(obj)

print(obj)

TukeyHSD(obj)

# ANOVA YII ---------------------------------------------------------------
qqnorm(HojasConRGR$y.II.)
YIIresults.aov <-  lm(y.II. ~ DDS + T + DDS*T, data=HojasConRGR)
anova(YIIresults.aov)
TukeyHSD(YIIresults.aov)

YIIresults <- aov(y.II. ~ DDS + T + DDS*T, data=HojasConRGR)
anova(YIIresults)
TukeyHSD(YIIresults)

# ANOVA NPQ ---------------------------------------------------------------
qqnorm(HojasConRGR$NPQ)
NPQresults<- aov(NPQ ~ DDS + T + DDS*T, data=HojasConRGR)
anova(NPQresults)
TukeyHSD(NPQresults)


# ANOVA qP ----------------------------------------------------------------
qqnorm(HojasConRGR$qP)
qPresults<- aov(qP ~ DDS + T + DDS:T, data=HojasConRGR)
anova(qPresults)
TukeyHSD(qPresults)


# ANOVA Área foliar -------------------------------------------------------
qqnorm(HojasConRGR$a.fol)
qqline(HojasConRGR$a.fol)
shapiro.test(HojasConRGR$a.fol)
kruskal.test(a.fol ~ T, data=HojasConRGR)
kruskalmc(a.fol ~ T, data=HojasConRGR)
afoliarresults<- aov(a.fol ~ DDS + T + DDS:T, data=HojasConRGR)
anova(afoliarresults)
TukeyHSD(afoliarresults)



