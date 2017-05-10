if(R.version$os=="linux-gnu"){ 
  load("~/Documents/Ignacio/lechuga/noRawdfs.RData")
} else if (R.version$os=="darwin15.6.0"){
  load("~/Documents/Biologia/Tesis/dataAnalysis/noRawdfs.RData")
}
 
library(pgirmess)

HojasConRGR$T <- as.factor(HojasConRGR$T)
dds30<- subset(HojasConRGR, DDS<31)
dds45<- subset(HojasConRGR, DDS>31 & DDS<46)
dds60<- subset(HojasConRGR, DDS>46 & DDS<61)
dds75<- subset(HojasConRGR, DDS>61)


#AFOLIAR 30 DIAS-----
qqnorm(dds30$a.fol)
qqline(dds30$a.fol)
shapiro.test(dds30$a.fol)
hist(dds30$a.fol)
kruskal.test(a.fol ~ T, data=dds30)
kruskalmc(a.fol ~ T, data=dds30)
# afolresults <- aov(a.fol ~ T, data=dds30)
# print(afolresults)
# anova(afolresults)
# TukeyHSD(afolresults)

#AFOLIAR 45 DIAS----
qqnorm(dds45$a.fol)
qqline(dds45$a.fol)
shapiro.test(dds45$a.fol)
kruskal.test(a.fol ~ T, data=dds45)
kruskalmc(a.fol ~ T, data=dds45)
# afolresults <- aov(a.fol ~ T, data=dds45)
# anova(afolresults)
# TukeyHSD(afolresults)

#AFOLIAR 60 DIAS----
qqnorm(dds60$a.fol)
qqline(dds60$a.fol)
hist(dds60$a.fol)
shapiro.test(dds60$a.fol)
kruskal.test(a.fol ~ T, data=dds60)
kruskalmc(a.fol ~ T, data=dds60)
afolresults <- aov(a.fol ~ T, data=dds60)
# print(afolresults) 
# anova(afolresults)
#  TukeyHSD(afolresults)

#AFOLIAR 75 DIAS----
qqnorm(dds75$a.fol)
qqline(dds75$a.fol)
shapiro.test(dds75$a.fol)
hist(dds75$a.fol)
kruskal.test(a.fol ~ T, data=dds75)
kruskalmc(a.fol ~ T, data=dds75)
# TukeyHSD(afolresults)
# afolresults <- aov(a.fol ~ T, data=dds75)
# anova(afolresults)
# TukeyHSD(afolresults)

#AFE 30 DIAS----
qqnorm(dds30$AFE )
qqline(dds30$AFE)
hist(dds30$AFE)
shapiro.test(dds30$AFE)
tempresults <- aov(AFE ~ T, data=dds30)
anova(tempresults)
plot(tempresults$residuals)
TukeyHSD(tempresults)
# kruskal.test(a.fol ~ T, data=dds75)
# kruskalmc(a.fol ~ T, data=dds75)

#AFE 45 DIAS----
qqnorm(dds45$AFE)
qqline(dds45$AFE)
hist(dds45$AFE)
shapiro.test(dds45$AFE)
kruskal.test(AFE ~ T, data=dds45)
kruskalmc(AFE ~ T, data=dds45)

#AFE 60 DIAS----
qqnorm(dds60$AFE )
qqline(dds60$AFE)
hist(dds60$AFE)
shapiro.test(dds60$AFE)
tempresults <- aov(AFE ~ T, data=dds60)
anova(tempresults)
plot(tempresults$residuals)
TukeyHSD(tempresults)

#AFE 75 DIAS----
qqnorm(dds75$AFE)
qqline(dds75$AFE)
hist(dds75$AFE)
shapiro.test(dds75$AFE)
tempresults <- aov(AFE ~ T, data=dds75)
anova(tempresults)
plot(tempresults$residuals)
TukeyHSD(tempresults)

#IAF 30 DIAS------
qqnorm(dds30$IAF)
qqline(dds30$IAF)
hist(dds30$IAF)
shapiro.test(dds30$IAF)
tempresults <- aov(IAF ~ T, data=dds30)
anova(tempresults)
plot(tempresults$residuals)
TukeyHSD(tempresults)

#IAF 45 DIAS----
qqnorm(dds45$IAF)
qqline(dds45$IAF)
hist(dds45$IAF)
shapiro.test(dds45$IAF)
tempresults <- aov(IAF ~ T, data=dds45)
anova(tempresults)
plot(tempresults$residuals)
TukeyHSD(tempresults)

#IAF 60 DIAS----
qqnorm(dds60$IAF)
qqline(dds60$IAF)
hist(dds60$IAF)
shapiro.test(dds60$IAF)
kruskal.test(IAF ~ T, data=dds60)
kruskalmc(IAF ~ T, data=dds60)
# tempresults <- aov(IAF ~ T, data=dds60)
# anova(tempresults)
# plot(tempresults$residuals)
# TukeyHSD(tempresults)

#IAF 75 DIAS----
qqnorm(dds75$IAF)
qqline(dds75$IAF)
hist(dds75$IAF)
shapiro.test(dds75$IAF)
tempresults <- aov(IAF ~ T, data=dds75)
anova(tempresults)
plot(tempresults$residuals)
TukeyHSD(tempresults)

#FVFM 30 DIAS-----
qqnorm(dds30$fv.fm)
qqline(dds30$fv.fm)
hist(dds30$fv.fm)
shapiro.test(dds30$fv.fm)
kruskal.test(fv.fm~ T, data=dds30)
kruskalmc(fv.fm~ T, data=dds30)
# fvfmresults <- aov(fv.fm ~ T, data=dds30)
# anova(fvfmresults)
# TukeyHSD(fvfmresults)

#FVFM 45 DIAS-----
qqnorm(dds45$fv.fm)
qqline(dds45$fv.fm)
hist(dds45$fv.fm)
shapiro.test(dds45$fv.fm)
fvfmresults <- aov(fv.fm ~ T, data=dds45)
anova(fvfmresults)
TukeyHSD(fvfmresults)

#FVFM 60 DIAS-----
qqnorm(dds60$fv.fm)
qqline(dds60$fv.fm)
shapiro.test(dds60$fv.fm)
hist(dds60$fv.fm)
kruskal.test(fv.fm~ T, data=dds60)
kruskalmc(fv.fm~ T, data=dds60)
# fvfmresults <- aov(fv.fm ~ T, data=dds60)
# anova(fvfmresults)
# TukeyHSD(fvfmresults)

#FVFM 75 DIAS-----
qqnorm(dds75$fv.fm)
qqline(dds75$fv.fm)
shapiro.test(dds75$fv.fm)
kruskal.test(fv.fm~ T, data=dds75)
kruskalmc(fv.fm~ T, data=dds75)
# fvfmresults <- aov(fv.fm ~ T, data=dds75)
# anova(fvfmresults)
# TukeyHSD(fvfmresults)

#YII 30 DIAS-----
qqnorm(dds30$y.II.)
qqline(dds30$y.II.)
shapiro.test(dds30$y.II.)
# kruskal.test(y.II.~ T, data=dds30)
# kruskalmc(y.II.~ T, data=dds30)
tempresults <- aov(y.II. ~ T, data=dds30)
anova(tempresults)
TukeyHSD(tempresults)

#YII 45 DIAS-----
qqnorm(dds45$y.II.)
qqline(dds45$y.II.)
shapiro.test(dds45$y.II.)
kruskal.test(y.II.~ T, data=dds45)
kruskalmc(y.II.~ T, data=dds45)
# tempresults <- aov(y.II. ~ T, data=dds45)
# anova(tempresults)
# TukeyHSD(tempresults)

#YII 60 DIAS-----
qqnorm(dds60$y.II.)
qqline(dds60$y.II.)
shapiro.test(dds60$y.II.)
tempresults <- aov(y.II. ~ T, data=dds60)
anova(tempresults)
TukeyHSD(tempresults)

#YII 75 DIAS-----
qqnorm(dds75$y.II.)
qqline(dds75$y.II.)
shapiro.test(dds75$y.II.)
tempresults <- aov(y.II. ~ T, data=dds75)
anova(tempresults)
TukeyHSD(tempresults)

#NPQ 30 DIAS-----
qqnorm(dds30$NPQ)
qqline(dds30$NPQ)
shapiro.test(dds30$NPQ)
kruskal.test(NPQ~ T, data=dds30)
kruskalmc(NPQ~ T, data=dds30)
# tempresults <- aov(NPQ ~ T, data=dds30)
# anova(tempresults)
# TukeyHSD(tempresults)

#NPQ 45 DIAS-----
qqnorm(dds45$NPQ)
qqline(dds45$NPQ)
shapiro.test(dds45$NPQ)
tempresults <- aov(NPQ ~ T, data=dds45)
anova(tempresults)
TukeyHSD(tempresults)

#NPQ 60 DIAS-----
qqnorm(dds60$NPQ)
qqline(dds60$NPQ)
shapiro.test(dds60$NPQ)
tempresults <- aov(NPQ ~ T, data=dds60)
anova(tempresults)
TukeyHSD(tempresults)

#NPQ 75 DIAS-----
qqnorm(dds75$NPQ)
qqline(dds75$NPQ)
shapiro.test(dds75$NPQ)
tempresults <- aov(NPQ ~ T, data=dds75)
anova(tempresults)
TukeyHSD(tempresults)

#qP 30 DIAS-----
qqnorm(dds30$qP)
qqline(dds30$qP)
shapiro.test(dds30$qP)
kruskal.test(qP ~ T, data=dds30)
kruskal.test(qP ~ T, data=dds30)
kruskalmc(qP ~ T, data=dds30)
# tempresults <- aov(qP ~ T, data=dds30)
# anova(tempresults)
# TukeyHSD(tempresults)

#qP 45 DIAS-----
qqnorm(dds45$qP)
qqline(dds45$qP)
shapiro.test(dds45$qP)
kruskal.test(qP ~ T, data=dds45)
kruskalmc(qP ~ T, data=dds45)

#qP 60 DIAS-----
qqnorm(dds60$qP)
qqline(dds60$qP)
shapiro.test(dds60$qP)
kruskal.test(qP ~ T, data=dds60)
kruskalmc(qP ~ T, data=dds60)
# tempresults <- aov(qP ~ T, data=dds60)
# anova(tempresults)
# TukeyHSD(tempresults)

#qP 75 DIAS-----
qqnorm(dds75$qP)
qqline(dds75$qP)
shapiro.test(dds75$qP)
tempresults <- aov(qP ~ T, data=dds75)
anova(tempresults)
TukeyHSD(tempresults)

#TTE 30 DIAS-----
qqnorm(dds30$etr)
qqline(dds30$etr)
shapiro.test(dds30$etr)
tempresults <- aov(etr ~ T, data=dds30)
anova(tempresults)
plot(tempresults$residuals)
TukeyHSD(tempresults)

#TTE 45 DIAS-----
qqnorm(dds45$etr)
qqline(dds45$etr)
shapiro.test(dds45$etr)
kruskal.test(etr ~ T, data=dds45)
kruskalmc(etr ~ T, data=dds45)
# tempresults <- aov(etr ~ T, data=dds45)
# anova(tempresults)
# plot(tempresults$residuals)
# TukeyHSD(tempresults)

#TTE 60 DIAS-----
qqnorm(dds60$etr)
qqline(dds60$etr)
shapiro.test(dds60$etr)
tempresults <- aov(etr ~ T, data=dds60)
anova(tempresults)
plot(tempresults$residuals)
TukeyHSD(tempresults)

#TTE 75 DIAS-----
qqnorm(dds75$etr)
qqline(dds75$etr)
shapiro.test(dds75$etr)
tempresults <- aov(etr ~ T, data=dds75)
anova(tempresults)
plot(tempresults$residuals)
TukeyHSD(tempresults)

#CRA 30 DIAS ------------------------------------------------------------

qqnorm(dds30$CRA)
qqline(dds30$CRA)
hist(dds30$CRA)
shapiro.test(dds30$CRA)
kruskal.test(CRA ~ T, data=dds30)
kruskalmc(CRA ~ T, data=dds30)
# fvfmresults <- aov(fv.fm ~ T, data=dds30)
# anova(fvfmresults)
# TukeyHSD(fvfmresults)

# #CRA 45 DIAS ------------------------------------------------------------

qqnorm(dds45$CRA)
qqline(dds45$CRA)
hist(dds45$CRA)
shapiro.test(dds45$CRA)
CRAmresults <- aov(CRA ~ T, data=dds45)
anova(CRAmresults)
plot(CRAmresults$residuals)
# #CRA 60 DIAS ------------------------------------------------------------
qqnorm(dds60$CRA)
qqline(dds60$CRA)
hist(dds60$CRA)
shapiro.test(dds60$CRA)
CRAmresults <- aov(CRA ~ T, data=dds60)
anova(CRAmresults)
plot(CRAmresults$residuals)
# #CRA 75 DIAS ------------------------------------------------------------

qqnorm(dds75$CRA)
qqline(dds75$CRA)
hist(dds75$CRA)
shapiro.test(dds75$CRA)
CRAmresults <- aov(CRA ~ T, data=dds75)
anova(CRAmresults)
plot(tempresults$residuals)

# MASA SECA 30 DIAS ----
qqnorm(dds30$mstotal)
qqline(dds30$mstotal)
hist(dds30$mstotal)
shapiro.test(dds30$mstotal)
kruskal.test(mstotal ~ T, data=dds30)
kruskalmc(mstotal ~ T, data=dds30)

# MASA SECA 45 DIAS ----
qqnorm(dds45$mstotal)
qqline(dds45$mstotal)
hist(dds45$mstotal)
shapiro.test(dds45$mstotal)
tempresults <- aov(mstotal ~ T, data=dds45)
anova(tempresults)
TukeyHSD(tempresults)
plot(tempresults$residuals)

# MASA SECA 60 DIAS ----
qqnorm(dds60$mstotal)
qqline(dds60$mstotal)
hist(dds60$mstotal)
shapiro.test(dds60$mstotal)
tempresults <- aov(mstotal ~ T, data=dds60)
anova(tempresults)
TukeyHSD(tempresults)
plot(tempresults$residuals)

# MASA SECA 75 DIAS ----
qqnorm(dds75$mstotal)
qqline(dds75$mstotal)
hist(dds75$mstotal)
shapiro.test(dds75$mstotal)
tempresults <- aov(mstotal ~ T, data=dds75)
anova(tempresults)
TukeyHSD(tempresults)
plot(tempresults$residuals)

# MASA fresca 30 DIAS ----
qqnorm(dds30$mf.hoj)
qqline(dds30$mf.hoj)
hist(dds30$mf.hoj)
shapiro.test(dds30$mf.hoj)
kruskal.test(mf.hoj ~ T, data=dds30)
kruskalmc(mf.hoj ~ T, data=dds30)

# MASA fresca 45 DIAS ----
qqnorm(dds45$mf.hoj)
qqline(dds45$mf.hoj)
hist(dds45$mf.hoj)
shapiro.test(dds45$mf.hoj)
kruskal.test(mf.hoj ~ T, data=dds45)
kruskalmc(mf.hoj ~ T, data=dds45)

# MASA fresca 60 DIAS ----
qqnorm(dds60$mf.hoj)
qqline(dds60$mf.hoj)
hist(dds60$mf.hoj)
shapiro.test(dds60$mf.hoj)
kruskal.test(mf.hoj ~ T, data=dds60)
kruskalmc(mf.hoj ~ T, data=dds60)

# MASA fresca 75 DIAS ----
qqnorm(dds75$mf.hoj)
qqline(dds75$mf.hoj)
hist(dds75$mf.hoj)
shapiro.test(dds75$mf.hoj)
kruskal.test(mf.hoj ~ T, data=dds75)
kruskalmc(mf.hoj ~ T, data=dds75)


# PIGMENTOS ---------------------------------------------------------------
verdosidad$T <- as.factor(verdosidad$T)
qqnorm(verdosidad$cla)
qqline(verdosidad$cla)
hist(verdosidad$cla)
shapiro.test(verdosidad$cla)
claresults <- aov(cla ~ T, data=verdosidad)
anova(claresults)
TukeyHSD(claresults)
plot(claresults$residuals)

qqnorm(verdosidad$clb)
qqline(verdosidad$clb)
hist(verdosidad$clb)
shapiro.test(verdosidad$clb)
tempresults <- aov(clb ~ T, data=verdosidad)
anova(tempresults)
plot(tempresults$residuals)

qqnorm(verdosidad$clt)
qqline(verdosidad$clt)
hist(verdosidad$clt)
shapiro.test(verdosidad$clt)
tempresults <- aov(clt ~ T, data=verdosidad)
anova(tempresults)
plot(tempresults$residuals)

qqnorm(verdosidad$car)
qqline(verdosidad$car)
hist(verdosidad$car)
shapiro.test(verdosidad$car)
tempresults <- aov(car ~ T, data=verdosidad)
anova(tempresults)
plot(claresults$residuals)

#PE----
PE$T <- as.factor(PE$T)
qqnorm(PE$pe)
qqline(PE$pe)
hist(PE$pe)
shapiro.test(PE$pe)
tempresults <- aov(pe ~ T, data=PE)
anova(tempresults)
TukeyHSD(tempresults)

#RGR----
qqnorm(HojasConRGR$rgrtot)
qqline(HojasConRGR$rgrtot)
hist(HojasConRGR$rgrtot)
shapiro.test(HojasConRGR$rgrtot)
kruskal.test(rgrtot ~ T, data=HojasConRGR)
kruskalmc(rgrtot ~ T, data=HojasConRGR)

#productividad m2 ----
qqnorm(dds75$prodm2)
qqline(dds75$prodm2)
hist(dds75$prodm2)
shapiro.test(dds75$prodm2)
kruskal.test(prodm2~ T, data=dds75)
kruskalmc(prodm2~ T, data=dds75)
