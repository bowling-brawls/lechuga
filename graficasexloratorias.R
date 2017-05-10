library(ggplot2)
library(reshape2)
library(dplyr)

if(R.version$os=="linux-gnu"){ 
  load("~/Documents/Ignacio/lechuga/noRawdfs.RData")
  load("~/Documents/Ignacio/lechuga/promedios.RData")
} else if (R.version$os=="darwin15.6.0"){
  load("~/Documents/Biologia/Tesis/dataAnalysis/noRawdfs.RData")
  load("~/Documents/Biologia/Tesis/dataAnalysis/promedios.RData")
}



# Boxplots ----------------------------------------------------------------


print(qplot(as.factor(as.character(T)), rgrtot, data = HojasConRGR, 
            geom="boxplot",color=T))

print(qplot(as.factor(as.character(T)), IAF, data = HojasConRGR, 
            geom="boxplot",color=T))

# Lineas ------------------------------------------------------------------


# preparar los datos para el plot de lineas
promediosmelt <- melt(promedios, id.vars = c("T", "DDS"))
prodmelt <- melt(productionmean, id.vars = c("T"))

#graficar con ggplot2
df <- filter(promediosmelt, variable=="meanafol")


afoliarplot <- ggplot(data = df, aes(x=DDS, y=value, colour=as.factor(as.character(T)))) + geom_line() + 
  ylab("Área foliar (cm^2)") + xlab("Días después de siembra") + 
  scale_color_discrete(name="Tratamiento", breaks=c("1", "2", "3"),
                       labels=c("PolUV", "Control", "Polcom"))

print(afoliarplot)

#histogramas-----
# pigmentos bars -------------------------------------------------------------
df <- melt(promediopigmentos, id.vars = c("T"))
pigmentosbars <- ggplot(data=df, aes(x=T, y=value, fill=variable)) +  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(pigmentosbars)

#fvfm bars ----

df <- filter(promediosmelt, variable=="meanFvFm")

# df <- summarySE(HojasConRGR, measurevar="fv.fm", groupvars=c("T", "DDS"))
# 
# FvFmbars <- ggplot(data=df, aes(x=as.factor(T), y=fv.fm, fill=as.factor(DDS))) +
#   geom_bar(stat="identity", position=position_dodge(), colour="black") + 
#   coord_cartesian(ylim=c(0.75, 0.85)) + ylab("Fv/Fm") +  
#   scale_x_discrete(name="Tratamiento", 
#                    breaks=as.character(1:3), 
#                    labels=c("PolUV", "Control","PolCom"))+
#   scale_fill_discrete(name="DDS")+
#   geom_errorbar(aes(ymin=fv.fm-se, ymax=fv.fm+se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))
# 
# print(FvFmbars)


#YII bars
df <- filter(promediosmelt, variable=="meanYII")

YIIbars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") 
# + 
  # coord_cartesian(ylim=c(0, 0.4))
print(YIIbars)

# qP bars----
df <- filter(promediosmelt, variable=="meanqP")

qPbars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  coord_cartesian(ylim=c(0, 0.6))
print(qPbars)

#NPQ bars ----

df <- filter(promediosmelt, variable=="meanNPQ")

NPQbars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  coord_cartesian(ylim=c(0, 1.45))
print(NPQbars)

#masa seca bars----
df <- filter(promediosmelt, variable=="meanmseca")

msecabars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(msecabars)

#AFEbars----
df <- filter(promediosmelt, variable=="meanAFE")

AFEbars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(AFEbars)

#IAF bars-----
df <- filter(promediosmelt, variable=="meanIAF")

IAFbars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(IAFbars)

#productividad bars----
df <- filter(prodmelt, variable=="meanprod")

productivitybars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(T))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(productivitybars)

df <- filter(prodmelt, variable=="meanprodm2")

productivitybars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(T))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(productivitybars)

#

# df <- filter(prod, variable=="prodm2")
# 
# productivitybars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
#   geom_bar(stat="identity", position=position_dodge(), colour="black")
# print(productivitybars)


